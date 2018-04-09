module FWPL.VtyView
  ( render
  ) where

import qualified Graphics.Vty as Vty

import FWPL.Model (Model, Module(..), Value(..))

keywordAttr :: Vty.Attr
keywordAttr =
  Vty.defAttr `Vty.withForeColor` Vty.blue

typeNameAttr :: Vty.Attr
typeNameAttr =
  Vty.defAttr `Vty.withForeColor` Vty.yellow

moduleNameAttr :: Vty.Attr
moduleNameAttr =
  typeNameAttr

funNameTypeSigAttr :: Vty.Attr
funNameTypeSigAttr =
  Vty.defAttr `Vty.withForeColor` Vty.magenta

errorAttr :: Vty.Attr
errorAttr =
  Vty.defAttr `Vty.withForeColor` Vty.red

render :: Model -> Vty.Picture
render model =
  Vty.picForImage $
    case model of
      Left err ->
        Vty.string Vty.defAttr (show err)

      Right modules ->
        -- The modules are given to us in compile order. We want to display
        -- the most recent one on top. In particular, we would like errors
        -- to be on top ;)
        foldMap renderModule (reverse modules)

renderModule :: Module -> Vty.Image
renderModule module_ =
  Vty.vertCat
    [ renderModuleName module_
    , Vty.string Vty.defAttr ""
    , foldMap renderValue (moduleValues module_)
    , foldMap renderError (moduleErrors module_)
    ]

renderModuleName :: Module -> Vty.Image
renderModuleName module_ =
  Vty.horizCat
    [ Vty.string keywordAttr "module "
    , Vty.string moduleNameAttr (moduleName module_)
    , Vty.string keywordAttr " where"
    ]

renderValue :: Value -> Vty.Image
renderValue value =
  padBottom $
    Vty.vertCat
      [ renderType value
      , renderEval value
      ]

renderError :: String -> Vty.Image
renderError string =
  padBottom $
    Vty.vertJoin
      (Vty.vertCat
        (map (Vty.string errorAttr) (lines string)))
      (Vty.string Vty.defAttr "")

renderType :: Value -> Vty.Image
renderType value =
  Vty.horizCat
    [ Vty.string funNameTypeSigAttr (valueName value)
    , Vty.string keywordAttr " :: "
    , Vty.string typeNameAttr (valueType value)
    ]

renderEval :: Value -> Vty.Image
renderEval value =
  Vty.horizCat
    [ Vty.string Vty.defAttr (valueName value)
    , Vty.string keywordAttr " = "
    , Vty.string Vty.defAttr (valueEval value)
    ]

padBottom :: Vty.Image -> Vty.Image
padBottom = Vty.pad 0 0 0 1
