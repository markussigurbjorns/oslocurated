{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Segmenter
  ( startRecording,
    stopRecording,
  )
where

import Control.Concurrent.MVar
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import System.IO (hFlush, stdout)
import System.Process (ProcessHandle, createProcess, proc, terminateProcess)

-- Function to start recording
startRecording :: MVar (Map String ProcessHandle) -> String -> IO ()
startRecording processHandlesVar name = do
  currentTime <- getCurrentTime
  let timestamp = formatTime defaultTimeLocale "%Y-%m-%d_%H-%M-%S" currentTime
      filename = name ++ "_" ++ timestamp ++ ".mp3"
      tmpname = name ++ ".mp3"
  (_, _, _, ph) <- createProcess (proc "ffmpeg" ["-i", "https://oslocurated.com/stream.mp3", "-c", "copy", tmpname])
  modifyMVar_ processHandlesVar $ \ph_map -> return $ Map.insert name ph ph_map
  putStrLn $ "Recording started: " ++ filename
  hFlush stdout

-- Function to stop recording
stopRecording :: MVar (Map String ProcessHandle) -> String -> IO ()
stopRecording processHandlesVar name = do
  maybePh <- modifyMVar processHandlesVar $ \ph_map ->
    case Map.lookup name ph_map of
      Just ph -> return (Map.delete name ph_map, Just ph)
      Nothing -> return (ph_map, Nothing)
  case maybePh of
    Just ph -> do
      terminateProcess ph
      putStrLn "Recording stopped"
    Nothing -> putStrLn $ "No recording found with name: " ++ name
  hFlush stdout
