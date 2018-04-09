module FWPL
    ( main
    ) where

import qualified Control.Concurrent as Concurrent
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Monad.IO.Class (liftIO)
import qualified Data.List as List
import Graphics.Vty (Vty)
import qualified Graphics.Vty as Vty
import System.FilePath (FilePath)
import qualified System.FilePath as FilePath
import qualified System.FSNotify as FSN

import FWPL.Model (Model, Module(..), Value(..))
import qualified FWPL.GHC as GHC
import qualified FWPL.VtyView as VtyView
import qualified FWPL.Watch as Watch

data Event
  = FileChanged FilePath
  | VtyEvent Vty.Event

main :: [FilePath] -> IO ()
main mainPaths = do
  eventChan <- newChan

  Concurrent.forkIO (Watch.forChanges (writeChan eventChan . FileChanged))

  vtyConfig <- Vty.standardIOConfig
  vty <- Vty.mkVty vtyConfig

  Concurrent.forkIO (writeVtyEvents vty eventChan)

  result <- GHC.run (fwpl mainPaths eventChan vty)

  Vty.shutdown vty

  case result of
    Right _ -> putStrLn "Exited without error"
    Left err -> print err

fwpl :: [FilePath] -> Chan Event -> Vty -> GHC.Interpreter ()
fwpl mainPaths eventChan vty = do
  event <- liftIO (readChan eventChan)

  case event of
    FileChanged filePath -> do
      let loadPaths = case mainPaths of
                        [] -> [filePath]
                        _ -> mainPaths

      model <- GHC.load mainPaths
      liftIO $ do
        Vty.update vty (VtyView.render model)
        -- Ensure that the screen is entirely up to date
        Vty.refresh vty

      fwpl mainPaths eventChan vty

    VtyEvent vtyEvent -> do
      case vtyEvent of
        Vty.EvKey (Vty.KChar 'q') _ ->
          pure ()

        _ ->
          fwpl mainPaths eventChan vty

writeVtyEvents :: Vty -> Chan Event -> IO ()
writeVtyEvents vty eventChan = do
  event <- Vty.nextEvent vty
  writeChan eventChan (VtyEvent event)
  writeVtyEvents vty eventChan

