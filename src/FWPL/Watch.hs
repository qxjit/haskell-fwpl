module FWPL.Watch
  ( forChanges
  ) where

import qualified Control.Concurrent as Concurrent
import Control.Monad (forever, when)
import System.FilePath (FilePath)
import qualified System.FilePath as FilePath
import qualified System.FSNotify as FSN

forChanges :: (FilePath -> IO()) -> IO ()
forChanges onChange =
  FSN.withManagerConf watchConfig $ \manager -> do
    putStrLn "Watching"
    FSN.watchTree manager "." isCompileTrigger (onChange . FSN.eventPath)
    forever (Concurrent.threadDelay 1000000)

watchConfig :: FSN.WatchConfig
watchConfig =
  FSN.defaultConfig
    { FSN.confDebounce = FSN.Debounce (50 / 1000)
    }

isCompileTrigger :: FSN.Event -> Bool
isCompileTrigger event =
  case event of
    FSN.Removed _ _ -> False
    _ -> isHaskellFile (FSN.eventPath event)

isHaskellFile :: FilePath -> Bool
isHaskellFile path =
  FilePath.takeExtension path == ".hs"
