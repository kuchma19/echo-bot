{-# LANGUAGE LambdaCase #-}

module Main
  ( main,
  )
where

import qualified Config
import qualified ConfigurationTypes
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Text as T
import qualified EchoBot
import qualified FrontEnd.Console
import qualified FrontEnd.Telegram as FET
import qualified Logger
import qualified Logger.Impl
import System.Exit (die)

main :: IO ()
main = do
  withLogHandle $ \logHandle -> do
    frontEnd <- Config.getFrontEndType
    case frontEnd of
      ConfigurationTypes.TelegramFrontEnd -> do
        configTelegram <- Config.getTelegramConfig
        runTelegramFrontEnd logHandle configTelegram
      ConfigurationTypes.ConsoleFrontEnd -> do
        botHandle <- makeBotHandleForPlainText logHandle
        runConsoleFrontEnd botHandle

runConsoleFrontEnd :: EchoBot.Handle IO T.Text -> IO ()
runConsoleFrontEnd botHandle =
  FrontEnd.Console.run
    FrontEnd.Console.Handle {FrontEnd.Console.hBotHandle = botHandle}

runTelegramFrontEnd :: Logger.Handle IO -> FET.Config -> IO ()
runTelegramFrontEnd logHandle conf =
  FET.run conf $ \userId ->
    FET.Handle
      { FET.hUser =
          FET.User
            { FET.id_ = userId,
              FET.typeNextMessage = FET.RepeatedMessage,
              FET.handle = makeBotHandleForTextAndStickers logHandle
            }
      }

withLogHandle :: (Logger.Handle IO -> IO ()) -> IO ()
withLogHandle f = do
  config <- Config.getLoggerConfig
  Logger.Impl.withHandle config f

-- | Creates a bot handle. Please note:
--
-- * a handle holds a reference to an 'IORef' with a 'EchoBot.State',
--   so that it can only keep state of a single user. In order to
--   support multiple users in a chat, you should create a new handle
--   for each user and probably keep them in a 'Data.Map' keyed by a
--   user id.
--
-- * 'EchoBot.Handle' is parameterized with the 'Text' type, so that
--   it supports only plain text messages suitable for the console.
--   When implementing Telegram or another multimedia chat support,
--   you should create a similar function, but parameterized with
--   another message type which can represent either text or
--   multimedia messages. You will need to specify different functions
--   @hMessageFromText@ and @hTextFromMessage@.
makeBotHandleForPlainText :: Logger.Handle IO -> IO (EchoBot.Handle IO T.Text)
makeBotHandleForPlainText logHandle = do
  botConfig <- Config.getBotConfig
  initialState <- either (die . T.unpack) pure $ EchoBot.makeState botConfig
  stateRef <- newIORef initialState
  pure
    EchoBot.Handle
      { EchoBot.hGetState = readIORef stateRef,
        EchoBot.hModifyState' = modifyIORef' stateRef,
        EchoBot.hLogHandle = logHandle,
        EchoBot.hConfig = botConfig,
        EchoBot.hTextFromMessage = Just,
        EchoBot.hMessageFromText = id
      }

makeBotHandleForTextAndStickers :: Logger.Handle IO -> IO (EchoBot.Handle IO FET.Message)
makeBotHandleForTextAndStickers logHandle = do
  botConfig <- Config.getBotConfig
  initialState <- either (die . T.unpack) pure $ EchoBot.makeState botConfig
  stateRef <- newIORef initialState
  pure
    EchoBot.Handle
      { EchoBot.hGetState = readIORef stateRef,
        EchoBot.hModifyState' = modifyIORef' stateRef,
        EchoBot.hLogHandle = logHandle,
        EchoBot.hConfig = botConfig,
        EchoBot.hTextFromMessage = \case
          FET.Text text -> Just text
          FET.Sticker _ -> Nothing,
        EchoBot.hMessageFromText = FET.Text
      }