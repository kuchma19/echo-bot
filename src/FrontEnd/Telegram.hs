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

import Control.Lens.Fold ((^?))
import Control.Monad (when)
import qualified Control.Monad.State as ST (MonadState (get), StateT (..), evalStateT, modify)
import Control.Monad.Trans (liftIO)
import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON), Value, encode, object)
import qualified Data.Aeson as DA (Value (Number, String))
import Data.Aeson.Lens (AsValue, key, nth)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy (toStrict)
import Data.Either.Combinators (rightToMaybe)
import Data.Map (Map, adjust, empty, insert, notMember, (!))
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Scientific as DS (Scientific, floatingOrInteger)
import qualified Data.Text as T
import EchoBot (Response (..))
import qualified EchoBot as EB
import Network.HTTP.Simple (QueryItem, Request, addToRequestQueryString, getResponseBody, httpBS, httpNoBody, setRequestPath)
import Text.Read (readMaybe)

type UserId = Integer

type UpdateId = Integer

type ChatId = Integer

type StickerId = T.Text

type StateM = ST.StateT (Map UserId User) IO

newtype Handle = Handle
  { hUser :: User
  }

data TypeNextMessage = RepeatedMessage | RepetitionCount
  deriving (Eq)

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

data Config = Config
  { confRequest :: Request,
    confTokenPath :: B.ByteString
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

makeQueryItem :: (B.ByteString, B.ByteString) -> QueryItem
makeQueryItem (a, b) = (a, Just b)

makeGetUpdatesRequest :: Request -> B.ByteString -> Integer -> Request
makeGetUpdatesRequest telegramRequest token beginOffset =
  let methodGetUpdates = "getUpdates"
      timeout = ("timeout", "10")
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
  let sToInteger = rightToMaybe . (DS.floatingOrInteger :: DS.Scientific -> Either Double Integer)
  userId_ <- sToInteger userIdS
  updateId_ <- sToInteger updateIdS
  chatId_ <- sToInteger chatIdS
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

getUpdates :: Request -> B.ByteString -> Integer -> IO [MessageInformation]
getUpdates telegramRequest token offset = do
  let requestUpdates = makeGetUpdatesRequest telegramRequest token offset
  response <- httpBS requestUpdates
  let jsonBody = getResponseBody response
      goodPositions = takeWhile checkPosition $ zip [0 ..] (repeat jsonBody)
      messages = mapMaybe (\(pos, js) -> valuesToMessage js pos) goodPositions
  B.putStrLn jsonBody
  return messages

makeSendRequest :: Request -> B.ByteString -> ChatId -> Message -> Bool -> StateM Request
makeSendRequest telegramRequest token chatId_ (Text text_) withKeyBoard =
  let methodSendMessage = "sendMessage"
      chatIdQ = ("chat_id", B.pack $ show chatId_)
      textQ = ("text", B.pack $ T.unpack text_)
      telegramRequestSend = setRequestPath (token <> "/" <> methodSendMessage) telegramRequest
      keybrQ =
        ("reply_markup", toStrict $ encode keybr)
      keybr =
        Keyboard
          { bottoms = [map (Bottom . T.pack . show) [1 :: Int .. 5]],
            resize_keyboard = True,
            one_time_keyboard = True
          }
      queryWitoutKeybr = [chatIdQ, textQ]
      query =
        map makeQueryItem $
          if withKeyBoard
            then keybrQ : queryWitoutKeybr
            else queryWitoutKeybr
   in return $ addToRequestQueryString query telegramRequestSend
makeSendRequest telegramRequest token chatId_ (Sticker stickerId) _ =
  let methodSendSticker = "sendSticker"
      chatIdQ = ("chat_id", B.pack $ show chatId_)
      stickerIdQ = ("sticker", B.pack $ T.unpack stickerId)
      telegramRequestSend = setRequestPath (token <> "/" <> methodSendSticker) telegramRequest
      query = map makeQueryItem [chatIdQ, stickerIdQ]
   in return $ addToRequestQueryString query telegramRequestSend

changeNextMessageType :: User -> User
changeNextMessageType user@User {typeNextMessage = RepeatedMessage} = user {typeNextMessage = RepetitionCount}
changeNextMessageType user@User {typeNextMessage = RepetitionCount} = user {typeNextMessage = RepeatedMessage}

setNewHandle :: EB.Handle IO Message -> User -> User
setNewHandle newHandle user = user {handle = return newHandle}

getUser :: UserId -> StateM User
getUser userId_ = do
  mapa <- ST.get
  return $ mapa ! userId_

getTypeMesssageUser :: UserId -> StateM TypeNextMessage
getTypeMesssageUser userId_ = do
  user <- getUser userId_
  return $ typeNextMessage user

isRepeatedMessage :: UserId -> StateM Bool
isRepeatedMessage userId_ = do
  typeMessage <- getTypeMesssageUser userId_
  return $ typeMessage == RepeatedMessage

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p t f = p >>= (\p' -> if p' then t else f)

goodNumberMessage :: Message -> StateM (Maybe Int)
goodNumberMessage (Text num) = return $ do
  number <- readMaybe (T.unpack num)
  if 0 <= number && number <= 5
    then return number
    else Nothing
goodNumberMessage _ = return Nothing

handleResponse :: Request -> B.ByteString -> ChatId -> UserId -> EB.Response Message -> StateM ()
handleResponse telegramRequest token chatId_ userId_ (MessageResponse message_) =
  ifM
    (isRepeatedMessage userId_)
    ( do
        req <- makeSendRequest telegramRequest token chatId_ message_ False
        _ <- httpNoBody req
        return ()
    )
    ( do
        maybeInt <- goodNumberMessage message_
        liftIO $ print maybeInt
        case maybeInt of
          Just num -> do
            user <- getUser userId_
            userHandle <- liftIO $ handle user
            st1 <- liftIO $ EB.hGetState userHandle
            liftIO $ print st1
            responses <- liftIO $ EB.respond userHandle (EB.SetRepetitionCountEvent num)
            st2 <- liftIO $ EB.hGetState userHandle
            liftIO $ print st2
            ST.modify $ adjust (changeNextMessageType . setNewHandle userHandle) userId_
            handleResponses telegramRequest token chatId_ userId_ responses
          Nothing -> return ()
    )
handleResponse telegramRequest token chatId_ userId_ (MenuResponse title _) = do
  req <- makeSendRequest telegramRequest token chatId_ (Text title) True
  ST.modify $ adjust changeNextMessageType userId_
  _ <- httpNoBody req
  return ()

handleResponses ::
  Foldable t =>
  Request ->
  B.ByteString ->
  ChatId ->
  UserId ->
  t (Response Message) ->
  ST.StateT (Map UserId User) IO ()
handleResponses telegramRequest token chatId_ userId_ = mapM_ (handleResponse telegramRequest token chatId_ userId_)

sendMessage :: Request -> B.ByteString -> (UserId -> Handle) -> MessageInformation -> StateM ()
sendMessage telegramRequest token getNewUserByUserId (MessageInformation userId_ _ chatId_ message_) = do
  mapaUsersNotModify <- ST.get
  when (notMember userId_ mapaUsersNotModify) $ do
    ST.modify $ insert userId_ (hUser $ getNewUserByUserId userId_)
  mapaUsers <- ST.get
  botHandle <- liftIO $ handle $ mapaUsers ! userId_
  responses <- liftIO $ EB.respond botHandle (EB.MessageEvent message_)
  handleResponses telegramRequest token chatId_ userId_ responses

getMaxUpdateId :: UpdateId -> [MessageInformation] -> UpdateId
getMaxUpdateId def [] = def
getMaxUpdateId def (MessageInformation {updateId = updateId_} : xs) = max updateId_ (getMaxUpdateId def xs)

readAndSendMessage :: UpdateId -> Config -> (UserId -> Handle) -> StateM ()
readAndSendMessage offset conf getNewUserByUserId = do
  let telegramRequest = confRequest conf
      tokenPath = confTokenPath conf
  listUpdates <- liftIO $ getUpdates telegramRequest tokenPath offset
  mapM_ (sendMessage telegramRequest tokenPath getNewUserByUserId) listUpdates
  let newOffset = getMaxUpdateId offset listUpdates
  readAndSendMessage newOffset conf getNewUserByUserId

run :: Config -> (UserId -> Handle) -> IO ()
run conf getNewUserByUserId = ST.evalStateT (readAndSendMessage 0 conf getNewUserByUserId) empty
