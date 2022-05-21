{-# LANGUAGE OverloadedStrings #-}

module FrontEnd.Telegram
  ( Handle (..),
    Config (..),
    Message (..),
    run,
  )
where

import qualified Data.Text as T
import qualified EchoBot as EB
import Network.HTTP.Simple (Request, setRequestPath, QueryItem, addToRequestQueryString, httpBS, getResponseBody)
import qualified Data.ByteString.Char8 as B
-- import Data.Aeson.Lens (key, _String)

data Handle = Handle
  { hBotHandle :: EB.Handle IO Message,
    hConfig :: Config
  }

data Config = Config
  { confRequest :: Request,
    confTokenPath :: B.ByteString 
  }

data Message = Text T.Text | Sticker StickerId

type StickerId = T.Text

makeQueryItem :: (B.ByteString, B.ByteString) -> QueryItem
makeQueryItem (a, b) = (a, Just b)

makeGetUpdatesRequest :: Request -> B.ByteString -> Integer -> Request
makeGetUpdatesRequest telegramRequest token beginOffset = let
  methodGetUpdates = "getUpdates"
  timeout = makeQueryItem ("timeout", "60")
  offset = makeQueryItem ("offset", B.pack $ show $ beginOffset + 1)
  -- allowedUpdates = makeQueryItem ("allowed_updates", "[\"update_id\", \"message\"]")
  telegramRequestUpdates = setRequestPath (token <> "/" <> methodGetUpdates) telegramRequest
  query = 
    [ timeout,
      offset
      -- allowedUpdates
    ]
  in addToRequestQueryString query telegramRequestUpdates


-- getUpdates :: Request -> Integer -> IO [Message]
getUpdates :: Request -> B.ByteString -> Integer -> IO B.ByteString
getUpdates telegramRequest token offset = do
  response <- httpBS $ makeGetUpdatesRequest telegramRequest token offset
  let jsonBody = getResponseBody response
  return jsonBody


readAndSendMessage :: offset -> Handle -> IO ()
readAndSendMessage offset handle = do
  -- let botHandle = hBotHandle handle
  let conf = hConfig handle
      telegramRequest = confRequest conf
      tokenPath = confTokenPath conf
  print telegramRequest
  b <- getUpdates telegramRequest tokenPath offset
  B.putStrLn b

run :: Handle -> IO ()
run = readAndSendMessage 0
