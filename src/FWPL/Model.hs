module FWPL.Model
  ( Model
  , Module(..)
  , Value(..)
  ) where

type Model = Either String [Module]

data Module =
  Module
    { moduleName :: String
    , moduleValues :: [Value]
    , moduleErrors :: [String]
    }

data Value =
  Value
    { valueType :: String
    , valueName :: String
    , valueEval :: String
    }



