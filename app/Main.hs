{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString.Lazy.Char8 (pack)
import Network.HTTP.Types (status200, status404)
import Network.Wai
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger
-- A simple application that responds with "Hello, world!" for the "/hello" route
helloApp :: Application
helloApp req respond =
  case pathInfo req of
    ["hello"] -> respond $ responseLBS status200 [("Content-Type", "text/plain")] "Hello, world!"
    _         -> respond $ responseLBS status200 [("Content-Type", "text/plain")] "Not Found"

-- Combine the static file server and the custom application
app :: Application
app req respond =
  if null (pathInfo req)
    then staticApp myStaticSettings req respond
    else helloApp req respond

-- Define StaticSettings with the correct directory path
myStaticSettings :: StaticSettings
myStaticSettings = defaultFileServerSettings "static"

main :: IO ()
main = do
  putStrLn "Starting server on port 3000"
  run 3000 $ logStdoutDev app

{-
main :: IO ()
main =
  run 3000 $
    logStdout $
      staticApp $
        defaultFileServerSettings "static"
-}
{-
main :: IO ()
main = run 3000 $ \_req send ->
  send $
    responseBuilder
      status200
      [("Content-Type", "text/plain; charset=utf-8")]
      "Hello, World!"
-}

{-
main :: IO ()
main = do
  putStrLn "Starting server on port 3000..."
  run 3000 app

-- Application definition
app :: Application
app req send = do
  response <-
    case pathInfo req of
      [] ->
        case requestMethod req of
          "GET" -> responseBuilder status200 [("Content-Type", "text/plain")] ("Hello Heimur")
  pure send response
-}
{-
  let method = requestMethod req
      path = pathInfo req

  case (method, path) of
    ("GET", []) -> staticApp (defaultFileServerSettings "static") req respond
    ("GET", ["hello", name]) -> respond $ responseLBS status200 [("Content-Type", "text/plain")] (pack $ "Hello, " ++ show name ++ "!")
    _ -> respond $ responseLBS status404 [("Content-Type", "text/plain")] (pack "404 not found")
-}
