{-# LANGUAGE ScopedTypeVariables #-}
module FWPL.GHC
  ( load
  , run
  , Interpreter
  ) where

import Control.Exception (SomeException)
import Data.Data (Data)
import qualified Data.Dynamic as Dynamic
import Data.Maybe (catMaybes, mapMaybe)
import Data.Typeable (Typeable)
import qualified Digraph as Digraph
import Exception (gtry, gcatch)

import qualified Annotations as Ann
import qualified ErrUtils as ErrUtils
import qualified HscTypes as HSC
import qualified IfaceSyn as IfaceSyn
import GHC (Ghc)
import qualified GHC as GHC
import qualified GHC.Paths as Paths
import qualified GhcPlugins as Plugins
import qualified OccName as OccName
import qualified Outputable as Out
import qualified RdrName as RdrName
import qualified Var as Var

import FWPL.Model (Model, Module(..), Value(..))

{- Type alias to make switching between Hint and GHC more convenient -}
type Interpreter = Ghc

run :: Ghc a -> IO (Either String a)
run action = do
  result <- gtry (GHC.runGhc (Just Paths.libdir) action)

  pure $
    case result of
      Right a -> Right a
      Left err -> Left (show (err :: SomeException))

load :: [FilePath] -> Ghc Model
load filePaths =
  handleUncaughtSourceError $ do
    dynFlags <- GHC.getSessionDynFlags
    GHC.setSessionDynFlags (configureInterpretedMode dynFlags)

    -- Cleary any previous targets to force a reload
    GHC.setTargets []
    GHC.load GHC.LoadAllTargets

    GHC.setTargets (map fileTarget filePaths)
    graph <- GHC.depanal [] True

    let topOrder = GHC.topSortModuleGraph False graph Nothing
        compileOrder = Digraph.flattenSCCs topOrder
        loadAndCatch mod = loadModule mod `gcatch` handleModuleError dynFlags mod

    mapM loadAndCatch compileOrder

handleUncaughtSourceError :: Ghc a -> Ghc (Either String a)
handleUncaughtSourceError action = do
  result <- gtry action

  pure $
    case result of
      Right a -> Right a
      Left err -> Left (show (err :: HSC.SourceError))

configureInterpretedMode :: GHC.DynFlags -> GHC.DynFlags
configureInterpretedMode dynFlags =
  dynFlags
    { GHC.ghcLink = GHC.LinkInMemory
    , GHC.hscTarget = GHC.HscInterpreted
    , GHC.ghcMode = GHC.CompManager
    , GHC.verbosity = 0
    }

loadModule :: GHC.ModSummary -> Ghc Module
loadModule summary = do
  parsed <- GHC.parseModule summary
  checked <- GHC.typecheckModule parsed
  loaded <- GHC.loadModule checked
  introspections <- moduleIntrospections summary

  let moduleImport = GHC.moduleName (GHC.ms_mod summary)

  GHC.setContext [GHC.IIDecl prelude, GHC.IIModule moduleImport]
  values <- mapM loadValue introspections
  GHC.setContext []

  pure $
    Module
      { moduleName = moduleNameString summary
      , moduleValues = values
      , moduleErrors = []
      }

handleModuleError :: GHC.DynFlags -> GHC.ModSummary -> HSC.SourceError -> Ghc Module
handleModuleError dynFlags summary err =
  pure $
    Module
      { moduleName = moduleNameString summary
      , moduleValues = []
      , moduleErrors = sourceErrorMessages dynFlags err
      }

sourceErrorMessages :: GHC.DynFlags -> HSC.SourceError -> [String]
sourceErrorMessages dynFlags =
  map (Out.showSDocDump dynFlags) . ErrUtils.pprErrMsgBagWithLoc . HSC.srcErrorMessages


moduleNameString :: GHC.ModSummary -> String
moduleNameString =
  GHC.moduleNameString . GHC.moduleName . GHC.ms_mod

data Introspection =
  Show OccName.OccName


moduleIntrospections :: GHC.ModSummary -> Ghc [Introspection]
moduleIntrospections summary = do
  info <- GHC.getModuleInfo (GHC.ms_mod summary)

  let crash thing = error ("Failed to load " ++ thing ++ " for " ++ moduleNameString summary)
      iface = maybe (crash "ModuleInfo") GHC.modInfoIface info
      anns = maybe (crash "ModIFace") GHC.mi_anns iface

  pure (mapMaybe introspectionForAnnotation anns)

introspectionForAnnotation :: IfaceSyn.IfaceAnnotation -> Maybe Introspection
introspectionForAnnotation ann =
  case IfaceSyn.ifAnnotatedTarget ann of
    Ann.ModuleTarget _ ->
      Nothing

    Ann.NamedTarget occName ->
      case deserializeAnnotationPayload (IfaceSyn.ifAnnotatedValue ann) of
        Just "fwpl:show" -> Just (Show occName)
        _ -> Nothing

deserializeAnnotationPayload :: (Data a, Typeable a) => Plugins.Serialized -> Maybe a
deserializeAnnotationPayload = Plugins.fromSerialized Plugins.deserializeWithData

moduleAnnotations :: GHC.ModSummary -> Ghc Int
moduleAnnotations summary = do
  info <- GHC.getModuleInfo (GHC.ms_mod summary)

  let crash thing = error ("Failed to load " ++ thing ++ " for " ++ moduleNameString summary)
      iface = maybe (crash "ModuleInfo") GHC.modInfoIface info
      anns = maybe (crash "ModIFace") GHC.mi_anns iface

  pure (length anns)


loadValue :: Introspection -> Ghc Value
loadValue (Show name) = do
  dynFlags <- GHC.getSessionDynFlags

  let nameString = Out.showPpr dynFlags name

  typResult <- gtry (GHC.exprType GHC.TM_Inst nameString)

  case typResult of
    Left (err :: HSC.SourceError) ->
      pure $ Value
        { valueType = "??"
        , valueName = nameString
        , valueEval = show err
        }

    Right typ -> do
      -- It would be nice to use HsSyn (or similar) to construct this expression
      -- in a way guaranteed to parse, but for now we take this shortcut.
      result <- gtry (GHC.dynCompileExpr ("FWPL_QXJIT_PRELUDE.show " ++ nameString))

      pure $
        case result of
          Left (err :: HSC.SourceError) ->
            Value
              { valueType = Out.showPpr dynFlags typ
              , valueName = nameString
              , valueEval = show err
              }

          Right dyn ->
            Value
              { valueType = Out.showPpr dynFlags typ
              , valueName = nameString
              , valueEval = Dynamic.fromDyn dyn "<FWPL INTERNAL ERROR: Show result was not a String>"
              }

fileTarget :: FilePath -> GHC.Target
fileTarget filePath =
  GHC.Target
    { GHC.targetId = GHC.TargetFile filePath Nothing
    , GHC.targetAllowObjCode = False
    , GHC.targetContents = Nothing
    }

prelude :: GHC.ImportDecl GHC.RdrName
prelude =
  (GHC.simpleImportDecl (GHC.mkModuleName "Prelude"))
    { GHC.ideclQualified = True
    , GHC.ideclAs = Just (GHC.noLoc (GHC.mkModuleName "FWPL_QXJIT_PRELUDE"))
    }
