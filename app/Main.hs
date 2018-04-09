module Main where

import qualified FWPL
import qualified System.Environment as Env

main :: IO ()
main = do
  mainPaths <- Env.getArgs
  FWPL.main mainPaths
