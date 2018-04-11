module FWPL.Model
  ( Model
  , Module(..)
  , Value(..)
  , Eval(..)
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
    , valueEval :: Either String Eval
    }

data Eval =
  Eval
    { evalResult :: String
    , evalOutput :: String
    }



