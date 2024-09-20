module Segmenter
  ( startRecording,
    stopRecording,
  )
where

import Control.Concurrent.MVar
import Data.Map.Strict (Map)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import System.IO (hFlush, stdout)
import System.Process (ProcessHandle, createProcess, proc, terminateProcess)

-- Function to start recording
startRecording :: MVar (Map String ProcessHandle) -> String -> IO ()
startRecording processHandlesVar name = do
  currentTime <- getCurrentTime
  let timestamp = formatTime defaultTimeLocale "%Y-%m-%d_%H-%M-%S" currentTime
      filename = name ++ "_" ++ timestamp ++ ".mp3"

  -- Start the ffmpeg process
  (_, _, _, ph) <- createProcess (proc "ffmpeg" ["-i", "https://oslocurated.com/stream.mp3", "-c", "copy", filename])
  putStrLn $ "Recording started: " ++ filename
  hFlush stdout

-- Function to stop recording
stopRecording :: ProcessHandle -> IO ()
stopRecording processHandle = do
  terminateProcess processHandle
  putStrLn "Recording stopped"
  hFlush stdout
