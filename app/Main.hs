{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent.MVar
import Control.Exception (IOException, catch)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (unpack)
import Network.HTTP.Types (hContentLength, hContentType, status200, status206, status401, status404, status500)
import Network.HTTP.Types.Header (hAuthorization, hContentRange, hRange, hWWWAuthenticate)
import Network.Wai
import Network.Wai.Application.Static (defaultFileServerSettings, staticApp)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Segmenter (startRecording, stopRecording)
import System.Process (ProcessHandle)

-- Returns a tuple of Int64 (start, end) representing the byte range
parseRange :: BSC.ByteString -> Int64 -> (Int64, Int64)
parseRange rangeHeader fileSize =
  case BSC.splitAt 6 rangeHeader of
    ("bytes=", byteRange) -> parseByteRange (BSC.drop 6 rangeHeader) fileSize
    _ -> (0, fileSize - 1) -- Default case if format is incorrect or no range specified

-- Helper function to parse byte ranges from ByteString
parseByteRange :: BSC.ByteString -> Int64 -> (Int64, Int64)
parseByteRange byteRange fileSize =
  let (startStr, endStr) = BSC.break (== '-') byteRange
      start = fromMaybe 0 (readMaybeInt (BSC.unpack startStr))
      end = fromMaybe (fileSize - 1) (readMaybeInt (BSC.unpack (BSC.drop 1 endStr)))
   in (start, end)

-- Safe reading of Int64 from String, returning Maybe Int64
readMaybeInt :: String -> Maybe Int64
readMaybeInt str = case reads str of
  [(val, "")] -> Just val
  _ -> Nothing

-- Serve audio file from the given file path
serveAudioFile :: FilePath -> Application
serveAudioFile filePath req respond = do
  fileResult <- catch (Just <$> BLC.readFile filePath) handleReadError
  case fileResult of
    Just fileContents -> do
      let filesize = BL.length fileContents
      case lookup hRange (requestHeaders req) of
        -- Handle Range requests (e.g., "Range: bytes=100-200")
        Just rangeHeader -> do
          let (start, end) = parseRange rangeHeader filesize
          let rangeData = BL.take (end - start + 1) $ BL.drop start fileContents
          let contentRangeHeader = BSC.pack ("bytes " <> show start <> "-" <> show end <> "/" <> show filesize)
          respond $
            responseLBS
              status206
              [(hContentRange, contentRangeHeader), (hContentLength, BSC.pack (show (end - start + 1))), (hContentType, "audio/mpeg")]
              rangeData
        Nothing -> do
          let headers = [(hContentType, "audio/mpeg"), (hContentLength, BSC.pack (show filesize))]
          respond $ responseLBS status200 headers fileContents
    Nothing -> respond $ responseLBS status404 [(hContentType, "text/plain")] "File not found"
  where
    handleReadError :: IOException -> IO (Maybe BL.ByteString)
    handleReadError _ = return Nothing

-- Serve the admin.html file
serveAdmin :: Application
serveAdmin _ respond =
  respond $
    responseFile status200 [(hContentType, "text/html")] "static/admin.html" Nothing

-- Middleware for basic auth
checkBasicAuth :: BSC.ByteString -> Bool
checkBasicAuth _ = True

reqiureAuth :: Application
reqiureAuth _ respond =
  respond $
    responseLBS
      status401
      [(hContentType, "text/plain"), (hWWWAuthenticate, "Basic realm=\"admin\"")]
      "Need Auth"

withBasicAuth :: Middleware
withBasicAuth successHandler req respond =
  case lookup hAuthorization (requestHeaders req) of
    Just authHeader ->
      if checkBasicAuth authHeader
        then successHandler req respond
        else reqiureAuth req respond
    Nothing -> reqiureAuth req respond

handleSegment :: MVar (Map String ProcessHandle) -> Application
handleSegment processHandlesVar req respond =
  getRequestBody req >>= \body ->
    case body of
      "record" -> do
        startRecording processHandlesVar "test"
        respond $ responseLBS status200 [(hContentType, "text/plain")] ("start recording " <> body)
      "stop" -> do
        stopRecording processHandlesVar "test"
        respond $ responseLBS status200 [(hContentType, "text/plain")] ("stop recording " <> body)
      _ ->
        respond $ responseLBS status200 [(hContentType, "text/plain")] body

getRequestBody :: Request -> IO BLC.ByteString
getRequestBody req = loop mempty
  where
    loop acc = do
      chunk <- getRequestBodyChunk req
      if BSC.null chunk
        then return acc
        else loop (acc <> BLC.fromStrict chunk)

-- Main application
app :: MVar (Map String ProcessHandle) -> Application
app processHandlesVar req respond =
  case requestMethod req of
    "GET" ->
      case pathInfo req of
        ["audio", fileName] -> serveAudioFile ("audio/" <> unpack fileName) req respond
        ["admin"] -> withBasicAuth serveAdmin req respond
        _ -> staticApp (defaultFileServerSettings "static") req respond
    "POST" ->
      case pathInfo req of
        ["segment"] -> handleSegment processHandlesVar req respond
        _ -> respond $ responseLBS status500 [(hContentType, "text/plain")] "Bad Request"
    _ ->
      respond $ responseLBS status500 [(hContentType, "text/plain")] "Bad Request"

main :: IO ()
main = do
  putStrLn "Starting server on port 3000"
  processHandlesVar <- newMVar Map.empty
  run 3000 $ logStdoutDev (app processHandlesVar)
