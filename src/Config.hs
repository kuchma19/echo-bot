{-# LANGUAGE OverloadedStrings #-}

-- | A module to provide a configuration reader for other modules.
module Config
  ( getBotConfig,
    getLoggerConfig,
    getFrontEndType,
  )
where

import qualified ConfigurationTypes
import qualified EchoBot
import qualified Logger.Impl
import qualified Logger ( Level (..) )
import qualified System.IO (stderr)

-- | Gets the bot config. In any case it can provide reasonable
-- default values.
getBotConfig :: IO EchoBot.Config
getBotConfig = return $ EchoBot.Config {
  EchoBot.confHelpReply = "I'm echo-bot. I will repeat your messages.\nYou can write command /repeat to change the number of repetition.",
  EchoBot.confRepeatReply = "Current repetition count is {count}.",
  EchoBot.confRepetitionCount = 1
}

getLoggerConfig :: IO Logger.Impl.Config
getLoggerConfig = return $ Logger.Impl.Config {
  Logger.Impl.confFileHandle = System.IO.stderr,
  Logger.Impl.confMinLevel = Logger.Info
}

getFrontEndType :: IO ConfigurationTypes.FrontEndType
getFrontEndType = return ConfigurationTypes.ConsoleFrontEnd
