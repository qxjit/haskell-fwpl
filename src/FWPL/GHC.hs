{-# LANGUAGE ScopedTypeVariables #-}
module FWPL.GHC
  ( load
  , run
  , Interpreter
  ) where

import Control.Exception (SomeException)
import qualified Data.Dynamic as Dynamic
import Data.Maybe (catMaybes, mapMaybe)
import qualified Digraph as Digraph
import Exception (gtry, gcatch)
import qualified ErrUtils as ErrUtils
import qualified HscTypes as HSC
import GHC (Ghc)
import qualified GHC as GHC
import qualified GHC.Paths as Paths
import qualified Outputable as Out
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

  let ids = moduleIds loaded
      moduleImport = GHC.moduleName (GHC.ms_mod summary)

  GHC.setContext [GHC.IIDecl prelude, GHC.IIModule moduleImport]
  values <- mapM loadValue ids
  GHC.setContext []

  pure $
    Module
      { moduleName = moduleNameString summary
      , moduleValues = catMaybes values
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


moduleIds :: GHC.TypecheckedModule -> [GHC.Id]
moduleIds =
  mapMaybe justId . GHC.modInfoTyThings . GHC.moduleInfo
    where
      justId (GHC.AnId id) = Just id
      justId _ = Nothing

loadValue :: GHC.Id -> Ghc (Maybe Value)
loadValue valueId = do
  dynFlags <- GHC.getSessionDynFlags

  let nameString = Out.showPpr dynFlags valueId

  -- It would be nice to use HsSyn (or similar) to construct this expression
  -- in a way guaranteed to parse, but for now we take this shortcut.
  result <- gtry (GHC.dynCompileExpr ("FWPL_QXJIT_PRELUDE.show " ++ nameString))

  pure $
    case result of
      Left (err :: HSC.SourceError) ->
        Nothing

      Right dyn ->
        Just $ Value
          { valueType = Out.showPpr dynFlags (GHC.idType valueId)
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
