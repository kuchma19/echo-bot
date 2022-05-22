{-# LANGUAGE OverloadedStrings #-}

module FrontEnd.Telegram
  ( Handle (..),
    Config (..),
    Message (..),
    User (..),
    StateM,
    run,
  )
where

import Control.Lens.Fold ((^?))
import Control.Monad.State (StateT (..), evalStateT, MonadState (get), modify)
import Control.Monad.Trans (liftIO)
import Data.Aeson (Object, Value)
import qualified Data.Aeson as DA (Value (Number, String))
import Data.Aeson.Lens (AsValue, key, nth)
import qualified Data.ByteString.Char8 as B
import qualified EchoBot as EB
import Data.Either.Combinators (rightToMaybe)
import Data.Map (Map, empty, notMember, insert, (!))
import Data.Maybe (isJust, mapMaybe)
import Data.Scientific (floatingOrInteger)
import qualified Data.Text as T
import qualified EchoBot as EB
import Network.HTTP.Simple (QueryItem, Request, addToRequestQueryString, getResponseBody, httpBS, httpNoBody, setRequestPath, Response)
import Control.Monad (when)
import EchoBot (Response(MessageResponse))

type UserId = Integer

type UpdateId = Integer

type ChatId = Integer

type StickerId = T.Text

type StateM = StateT (Map UserId User) IO

newtype Handle = Handle
  { hUser :: User
  }

data User = User
  { id :: UserId,
    handle :: IO (EB.Handle IO Message)
  }

data MessageInformation = MessageInformation {
  userId :: Integer,
  updateId :: UpdateId,
  chatId :: ChatId,
  message :: Message
}

instance Show User where
  show (User id _) = "[UserId: " <> show id <> "]"

instance Eq User where
  (User id1 _) == (User id2 _) = id1 == id2

instance Ord User where
  (User id1 _) <= (User id2 _) = id1 <= id2

data Config = Config
  { confRequest :: Request,
    confTokenPath :: B.ByteString
  }

data Message = Text T.Text | Sticker StickerId
  deriving (Show)

makeQueryItem :: (B.ByteString, B.ByteString) -> QueryItem
makeQueryItem (a, b) = (a, Just b)

makeGetUpdatesRequest :: Request -> B.ByteString -> Integer -> Request
makeGetUpdatesRequest telegramRequest token beginOffset =
  let methodGetUpdates = "getUpdates"
      timeout = ("timeout", "60")
      offset = ("offset", B.pack $ show $ beginOffset + 1)
      allowedUpdates = ("allowed_updates", "[\"update_id\", \"message\"]")
      telegramRequestUpdates = setRequestPath (token <> "/" <> methodGetUpdates) telegramRequest
      query = map makeQueryItem [timeout, offset, allowedUpdates]
   in addToRequestQueryString query telegramRequestUpdates

checkPosition :: AsValue s => (Int, s) -> Bool
checkPosition (pos, jsonBody) = isJust $ jsonBody ^? key "result" . nth pos

getResultInPos :: (AsValue t, Applicative f) => Int -> (Value -> f Value) -> t -> f t
getResultInPos pos = key "result" . nth pos

getUserId :: (AsValue t, Applicative f) => Int -> (Value -> f Value) -> t -> f t
getUserId pos = getResultInPos pos . key "message" . key "from" . key "id"

getChatId :: (AsValue t, Applicative f) => Int -> (Value -> f Value) -> t -> f t
getChatId pos = getResultInPos pos . key "message" . key "chat" . key "id"

getText :: (AsValue t, Applicative f) => Int -> (Value -> f Value) -> t -> f t
getText pos = getResultInPos pos . key "message" . key "text"

getSticker :: (AsValue t, Applicative f) => Int -> (Value -> f Value) -> t -> f t
getSticker pos = getResultInPos pos . key "message" . key "sticker" . key "file_id"

getUpdateId :: (AsValue t, Applicative f) => Int -> (Value -> f Value) -> t -> f t
getUpdateId pos = getResultInPos pos . key "update_id"

valuesToMessage :: AsValue s => s -> Int -> Maybe MessageInformation
valuesToMessage jsonBody pos = do
  DA.Number userIdS <- jsonBody ^? getUserId pos
  DA.Number updateIdS <- jsonBody ^? getUpdateId pos
  DA.Number chatIdS <- jsonBody ^? getChatId pos
  let sToInteger = rightToMaybe . floatingOrInteger
  userId <- sToInteger userIdS
  updateId <- sToInteger updateIdS
  chatId <- sToInteger chatIdS
  message <- case jsonBody ^? getText pos of
    Just (DA.String text) -> Just $ Text text
    _ -> case jsonBody ^? getSticker pos of
      Just (DA.String stickerId) -> Just $ Sticker stickerId
      _ -> Nothing
  return $ MessageInformation userId updateId chatId message

getUpdates :: (UserId -> Handle) -> Request -> B.ByteString -> Integer -> IO [MessageInformation]
-- getUpdates :: (UserId -> Handle) -> Request -> B.ByteString -> Integer -> IO String
getUpdates getNewUserByUserId telegramRequest token offset = do
  response <- httpBS $ makeGetUpdatesRequest telegramRequest token offset
  let jsonBody = getResponseBody response
      goodPositions = takeWhile checkPosition $ zip [0..] (repeat jsonBody)
      messages = mapMaybe (\(pos, js) -> valuesToMessage js pos) goodPositions
  B.putStrLn jsonBody
  return messages

makeSendRequest :: Monad m => Request -> B.ByteString -> ChatId -> Message -> m Request
makeSendRequest telegramRequest token chatId (Text text) =
  let methodSendMessage = "sendMessage"
      chatIdQ = ("chat_id", B.pack $ show chatId)
      textQ = ("text", B.pack $ T.unpack text)
      telegramRequestSend = setRequestPath (token <> "/" <> methodSendMessage) telegramRequest
      query = map makeQueryItem [chatIdQ, textQ]
  in return $ addToRequestQueryString query telegramRequestSend
makeSendRequest telegramRequest token chatId (Sticker stickerId) =
  let methodSendSticker = "sendSticker"
      chatIdQ = ("chat_id", B.pack $ show chatId)
      stickerIdQ = ("sticker", B.pack $ T.unpack stickerId)
      telegramRequestSend = setRequestPath (token <> "/" <> methodSendSticker) telegramRequest
      query = map makeQueryItem [chatIdQ, stickerIdQ]
  in return $ addToRequestQueryString query telegramRequestSend


handleResponse :: Request -> B.ByteString -> ChatId -> EB.Response Message -> StateM ()
handleResponse telegramRequest token chatId (MessageResponse message) = do
  req <- makeSendRequest telegramRequest token chatId message
  _ <- httpNoBody req
  return ()


sendMessage :: Request -> B.ByteString -> (UserId -> Handle) -> MessageInformation -> StateM ()
sendMessage telegramRequest token getNewUserByUserId (MessageInformation userId updateId chatId message) = do
  mapaUsersNotModify <- get
  when (notMember userId mapaUsersNotModify) $ do
    modify $ insert userId (hUser $ getNewUserByUserId userId)
  mapaUsers <- get
  botHandle <- liftIO $ handle $ mapaUsers ! userId
  responses <- liftIO $ EB.respond botHandle (EB.MessageEvent message)
  mapM_ (handleResponse telegramRequest token chatId) responses

getMaxUpdateId :: UpdateId -> [MessageInformation] -> UpdateId
getMaxUpdateId def [] = def
getMaxUpdateId def (MessageInformation {updateId = updateId} : xs) = max updateId (getMaxUpdateId def xs)

readAndSendMessage :: UpdateId -> Config -> (UserId -> Handle) -> StateM ()
readAndSendMessage offset conf getNewUserByUserId = do
  let telegramRequest = confRequest conf
      tokenPath = confTokenPath conf
  listUpdates <- liftIO $ getUpdates getNewUserByUserId telegramRequest tokenPath offset
  mapM_ (sendMessage telegramRequest tokenPath getNewUserByUserId) listUpdates
  let newOffset = getMaxUpdateId offset listUpdates
  readAndSendMessage newOffset conf getNewUserByUserId

run :: Config -> (UserId -> Handle) -> IO ()
run conf getNewUserByUserId = evalStateT (readAndSendMessage 0 conf getNewUserByUserId) empty
