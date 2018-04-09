module Foo
  ( fooAAA
  , yves
  ) where

import Bar

data Foo = Bar Int | Baz
  deriving Show

data Multiplier = Multiplier Int
  deriving Show

mult :: Multiplier -> Int -> Int
mult (Multiplier a) b = a * b

yves :: String
yves = "Yves"

times30 :: Multiplier
times30 = Multiplier 30

aaa :: Int
aaa = mult times30 bbb

fooAAA :: Foo
fooAAA = Bar aaa

