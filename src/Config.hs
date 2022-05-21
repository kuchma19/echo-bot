{-# LANGUAGE OverloadedStrings #-}

-- | A module to provide a configuration reader for other modules.
module Config
  ( getBotConfig,
    getLoggerConfig,
    getFrontEndType,
    getTelegramConfig,
  )
where

import qualified ConfigurationTypes
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import Data.HashMap.Strict ((!))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified EchoBot
import qualified FrontEnd.Telegram
import qualified Logger (Level (..))
import qualified Logger.Impl
import Network.HTTP.Simple (Request, parseRequestThrow)
import System.Directory (createDirectoryIfMissing)
import System.Exit (die)
import System.FilePath ((</>))
import qualified System.IO (Handle, IOMode (..), openFile)
import qualified Data.ByteString.Char8 as B

validateRepetitionCount :: Int -> IO EchoBot.RepetitionCount
validateRepetitionCount repetitionCount =
  if 1 <= repetitionCount && repetitionCount <= 5
    then return repetitionCount
    else do
      TIO.putStrLn "Invalid bot_messages.repetition_count in configurations.cfg. Must be number from 1 to 5."
      return 1

openFileHandle :: FilePath -> FilePath -> IO System.IO.Handle
openFileHandle pathDirectory fileName = do
  createDirectoryIfMissing True pathDirectory
  System.IO.openFile (pathDirectory </> fileName) System.IO.AppendMode

validateLogLevel :: T.Text -> IO Logger.Level
validateLogLevel levelText = do
  case levelText of
    "error" -> return Logger.Error
    "warning" -> return Logger.Warning
    "info" -> return Logger.Info
    "debug" -> return Logger.Debug
    _ -> do
      TIO.putStrLn "Invalid log.min_level in configurations.cfg. Must be 'error', 'warning', 'info' or 'debug'."
      return Logger.Error

getConfig :: IO CT.Config
getConfig = C.load [C.Required "configurations.cfg"]

-- | Gets the bot config. In any case it can provide reasonable
-- default values.
getBotConfig :: IO EchoBot.Config
getBotConfig = do
  conf <- getConfig
  mp <- C.getMap conf
  let repCount = fromMaybe 1 (CT.convert $ mp ! "bot_messages.repetition_count") :: Int
  repetitionCount <- validateRepetitionCount repCount
  return $
    EchoBot.Config
      { EchoBot.confHelpReply =
          fromMaybe "You can use command /repeat for change repetition count." (CT.convert $ mp ! "bot_messages.help"),
        EchoBot.confRepeatReply = fromMaybe "repetition count is {count}." (CT.convert $ mp ! "bot_messages.repeat"),
        EchoBot.confRepetitionCount = repetitionCount
      }

getLoggerConfig :: IO Logger.Impl.Config
getLoggerConfig = do
  conf <- getConfig
  mp <- C.getMap conf
  let pathDirectory = fromMaybe "." (CT.convert $ mp ! "log.directory")
      fileName = fromMaybe "configurations.cfg" (CT.convert $ mp ! "log.file_name")
      levelText = fromMaybe "error" (CT.convert $ mp ! "log.min_level")
  level <- validateLogLevel levelText
  return $
    Logger.Impl.Config
      { Logger.Impl.confFileHandle = openFileHandle (T.unpack pathDirectory) (T.unpack fileName),
        Logger.Impl.confMinLevel = level
      }

doWhenError :: IO ConfigurationTypes.FrontEndType
doWhenError = do
  TIO.putStrLn "Invalid bot_type.type in configurations.cfg. Must be 'console' or 'telegram'"
  return ConfigurationTypes.ConsoleFrontEnd

getFrontEndType :: IO ConfigurationTypes.FrontEndType
getFrontEndType = do
  conf <- getConfig
  mp <- C.getMap conf
  let botType = (CT.convert $ mp ! "bot_type.type" :: Maybe T.Text)
  case botType of
    Just text -> case text of
      "console" -> return ConfigurationTypes.ConsoleFrontEnd
      "telegram" -> return ConfigurationTypes.TelegramFrontEnd
      _ -> doWhenError
    Nothing -> doWhenError

getTelegramConfig :: IO FrontEnd.Telegram.Config
getTelegramConfig = do
  tokenConf <- C.load [C.Required "token.cfg"]
  mp <- C.getMap tokenConf
  let tokenMaybe = CT.convert $ mp ! "token" :: Maybe T.Text
  case tokenMaybe of
    Just token -> case token of
      "<token>" -> die "You have to write token in file 'token.cfg'."
      _ -> do
        telegramUri <- makeTelegramURI
        return $ FrontEnd.Telegram.Config telegramUri (B.pack $ T.unpack $ "bot" <> token)
    Nothing -> die "File token.cfg must contain a token in format 'token = <your token>'."

makeTelegramURI :: IO Request
makeTelegramURI = do
  let requestTelegram = "https://api.telegram.org/"
      maybeRequest = parseRequestThrow (T.unpack requestTelegram) :: Maybe Request
  case maybeRequest of
    Just request -> return request
    Nothing -> die "Wrong token in token.cfg"
