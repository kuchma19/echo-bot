{-# LANGUAGE OverloadedStrings #-}

module FrontEnd.Telegram
  ( Handle (..),
    Config (..),
    Message (..),
    User (..),
    StateM,
    TypeNextMessage (..),
    run,
  )
where

import Control.Lens ((.~))
import Control.Lens.Fold ((^?))
import qualified Control.Lens.Getter
import Control.Lens.Lens ((&))
import Control.Monad (when)
import qualified Control.Monad.State as ST (MonadState (get), StateT (..), evalStateT, modify)
import Control.Monad.Trans (liftIO)
import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON), Value, encode, object)
import qualified Data.Aeson as DA (Value (Number, String))
import Data.Aeson.Lens (AsValue, key, nth)
import qualified Data.ByteString.Lazy as B
import Data.Either.Combinators (rightToMaybe)
import Data.Map (Map, adjust, empty, insert, notMember, (!))
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import qualified Data.Monoid
import qualified Data.Scientific as DS (Scientific, floatingOrInteger)
import qualified Data.Text as T
import EchoBot (Response (..))
import qualified EchoBot as EB
import Network.Wreq (FormParam ((:=)))
import qualified Network.Wreq as NW
import Text.Read (readMaybe)

type UserId = Integer

type UpdateId = Integer

type ChatId = Integer

type StickerId = T.Text

type StateM = ST.StateT (Map UserId User) IO

newtype Handle = Handle
  { hUser :: User
  }

data TypeNextMessage = RepeatedMessage | RepetitionCount [(EB.RepetitionCount, EB.Event Message)]

data User = User
  { id_ :: UserId,
    typeNextMessage :: TypeNextMessage,
    handle :: IO (EB.Handle IO Message)
  }

data MessageInformation = MessageInformation
  { userId :: Integer,
    updateId :: UpdateId,
    chatId :: ChatId,
    message :: Message
  }

instance Show User where
  show User {id_ = id__} = "[UserId: " <> show id__ <> "]"

newtype Config = Config
  { confRequest :: String
  }

data Message = Text T.Text | Sticker StickerId
  deriving (Show)

data Keyboard = Keyboard
  { bottoms :: [[Bottom]],
    resize_keyboard :: Bool,
    one_time_keyboard :: Bool
  }

instance ToJSON Keyboard where
  toJSON (Keyboard bottoms_ resize_keyboard_ one_time_keyboard_) =
    object
      [ "keyboard" .= bottoms_,
        "resize_keyboard" .= resize_keyboard_,
        "one_time_keyboard" .= one_time_keyboard_
      ]

newtype Bottom = Bottom T.Text

instance ToJSON Bottom where
  toJSON (Bottom text_) = object ["text" .= text_]

makeGetUpdatesRequest :: (Show a, Num a) => [Char] -> a -> IO (Maybe B.ByteString)
makeGetUpdatesRequest telegramRequest beginOffset =
  let methodGetUpdates = "getUpdates"
      params =
        NW.defaults
          & NW.param "timeout" .~ ["10"]
          & NW.param "offset" .~ [T.pack $ show $ beginOffset + 1]
          & NW.param "allowed_updates" .~ ["update_id", "message"]
      telegramRequestUpdates = telegramRequest <> methodGetUpdates
   in do
        result <- NW.getWith params telegramRequestUpdates
        return $ result ^? NW.responseBody

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

sToInteger :: DS.Scientific -> Maybe Integer
sToInteger = rightToMaybe . (DS.floatingOrInteger :: DS.Scientific -> Either Double Integer)

getWith :: s -> (Int -> Control.Lens.Getter.Getting (Data.Monoid.First Value) s Value) -> Int -> Maybe Integer
getWith jsonBody getter pos = do
  DA.Number updateIdS <- jsonBody ^? getter pos
  sToInteger updateIdS

valuesToMessage :: AsValue s => s -> Int -> Maybe MessageInformation
valuesToMessage jsonBody pos = do
  userId_ <- getWith jsonBody getUserId pos
  updateId_ <- getWith jsonBody getUpdateId pos
  chatId_ <- getWith jsonBody getChatId pos
  message_ <- case jsonBody ^? getText pos of
    Just (DA.String text_) -> Just $ Text text_
    _ -> case jsonBody ^? getSticker pos of
      Just (DA.String stickerId) -> Just $ Sticker stickerId
      _ -> Nothing
  return $
    MessageInformation
      { userId = userId_,
        updateId = updateId_,
        chatId = chatId_,
        message = message_
      }

getMaxUpdateId :: AsValue s => Integer -> [(Int, s)] -> Integer
getMaxUpdateId def [] = def
getMaxUpdateId def ((pos, jsonBody) : xs) = max (fromMaybe def (getWith jsonBody getUpdateId pos)) (getMaxUpdateId def xs)

getUpdates :: String -> UpdateId -> IO (UpdateId, [MessageInformation])
getUpdates telegramRequest offset = do
  Just jsonBody <- makeGetUpdatesRequest telegramRequest offset
  let goodPositions = takeWhile checkPosition $ zip [0 ..] (repeat jsonBody)
      newOffset = getMaxUpdateId offset goodPositions
      messages = mapMaybe (\(pos, js) -> valuesToMessage js pos) goodPositions
  return (newOffset, messages)

makeSendRequest :: String -> ChatId -> Message -> Bool -> IO (Maybe B.ByteString)
makeSendRequest telegramRequest chatId_ (Text text_) withKeyBoard =
  let methodSendMessage = "sendMessage"
      telegramRequestSend = telegramRequest <> methodSendMessage
      chatIdQ = "chat_id" := show chatId_
      textQ = "text" := text_
      keybrQ = "reply_markup" := encode keybr
      keybr =
        Keyboard
          { bottoms = [map (Bottom . T.pack . show) [1 :: Int .. 5]],
            resize_keyboard = True,
            one_time_keyboard = True
          }
      maybeKeybrQ = [keybrQ | withKeyBoard]
   in do
        result <- NW.post telegramRequestSend $ chatIdQ : textQ : maybeKeybrQ
        return $ result ^? NW.responseBody
makeSendRequest telegramRequest chatId_ (Sticker stickerId) _ =
  let methodSendSticker = "sendSticker"
      telegramRequestSend = telegramRequest <> methodSendSticker
      chatIdQ = "chat_id" := show chatId_
      stickerIdQ = "sticker" := stickerId
   in do
        result <- NW.post telegramRequestSend [chatIdQ, stickerIdQ]
        return $ result ^? NW.responseBody

changeNextMessageType :: [(EB.RepetitionCount, EB.Event Message)] -> User -> User
changeNextMessageType events user@User {typeNextMessage = RepeatedMessage} = user {typeNextMessage = RepetitionCount events}
changeNextMessageType _ user@User {typeNextMessage = RepetitionCount _} = user {typeNextMessage = RepeatedMessage}

setNewHandle :: EB.Handle IO Message -> User -> User
setNewHandle newHandle user = user {handle = return newHandle}

getUser :: UserId -> StateM User
getUser userId_ = do
  mapa <- ST.get
  return $ mapa ! userId_

goodNumberMessage :: Message -> StateM (Maybe Int)
goodNumberMessage (Text num) = return $ do
  number <- readMaybe (T.unpack num)
  if 0 <= number && number <= 5
    then return number
    else Nothing
goodNumberMessage _ = return Nothing

handleResponse :: String -> ChatId -> UserId -> EB.Response Message -> StateM ()
handleResponse telegramRequest chatId_ _ (MessageResponse message_) = do
  _ <- liftIO $ makeSendRequest telegramRequest chatId_ message_ False
  return ()
handleResponse telegramRequest chatId_ userId_ (MenuResponse title events) = do
  _ <- liftIO $ makeSendRequest telegramRequest chatId_ (Text title) True
  ST.modify $ adjust (changeNextMessageType events) userId_
  return ()

handleResponses :: Foldable t => String -> ChatId -> UserId -> t (Response Message) -> ST.StateT (Map UserId User) IO ()
handleResponses telegramRequest chatId_ userId_ = mapM_ (handleResponse telegramRequest chatId_ userId_)

getResponsesFromUser :: User -> Message -> StateM [Response Message]
getResponsesFromUser User {typeNextMessage = RepeatedMessage, handle = handle_} message_ = liftIO $ do
  botHandle <- handle_
  EB.respond botHandle (EB.MessageEvent message_)
getResponsesFromUser User {typeNextMessage = RepetitionCount events, handle = handle_, id_ = userId_} message_ = do
  maybeNum <- goodNumberMessage message_
  botHandle <- liftIO handle_
  case maybeNum of
    Just num -> do
      let event = EB.findEvent num botHandle events
      ST.modify $ adjust (changeNextMessageType [] . setNewHandle botHandle) userId_
      liftIO $ EB.respond botHandle event
    Nothing -> return [EB.MessageResponse $ Text "You wrote wrong number."]

takeAnswerFromBotAndSendMessage :: String -> (UserId -> Handle) -> MessageInformation -> StateM ()
takeAnswerFromBotAndSendMessage telegramRequest getNewUserByUserId (MessageInformation userId_ _ chatId_ message_) = do
  mapaUsersNotModify <- ST.get
  when (notMember userId_ mapaUsersNotModify) $ do
    ST.modify $ insert userId_ (hUser $ getNewUserByUserId userId_)
  currentUser <- getUser userId_
  responses <- getResponsesFromUser currentUser message_
  handleResponses telegramRequest chatId_ userId_ responses

readAndSendMessage :: UpdateId -> Config -> (UserId -> Handle) -> StateM ()
readAndSendMessage offset conf getNewUserByUserId = do
  let telegramRequest = confRequest conf
  (newOffset, listUpdates) <- liftIO $ getUpdates telegramRequest offset
  mapM_ (takeAnswerFromBotAndSendMessage telegramRequest getNewUserByUserId) listUpdates
  readAndSendMessage newOffset conf getNewUserByUserId

run :: Config -> (UserId -> Handle) -> IO ()
run conf getNewUserByUserId = ST.evalStateT (readAndSendMessage 0 conf getNewUserByUserId) empty
