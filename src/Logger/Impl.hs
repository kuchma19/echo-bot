{-# LANGUAGE OverloadedStrings #-}

-- | The default implementation of the Logger interface.
module Logger.Impl
  ( withHandle,
    Config (..),
  )
where

import Control.Monad (when)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (getCurrentTime)
import Logger ((.<))
import qualified Logger
import qualified System.IO

data Config = Config
  { -- | A file handle to output formatted log messages to with
    -- 'System.IO.hPutStrLn' or 'Data.Text.IO.hPutStrLn'. For example,
    -- it might be 'System.IO.stderr' or a handle of a regular open
    -- file.
    confFileHandle :: IO System.IO.Handle,
    -- | The minimum level of a printable log message. Messages with
    -- lower levels should not be printed.
    confMinLevel :: Logger.Level
  }

withHandle :: Config -> (Logger.Handle IO -> IO ()) -> IO ()
withHandle config f = f Logger.Handle {Logger.hLowLevelLog = logWith config}

makeLogMessage :: Logger.Level -> T.Text -> IO T.Text
makeLogMessage level message = do
  currentTime <- getCurrentTime
  return $ "[ " .< level <> " " .< currentTime <> " ] " <> message

logWith :: Config -> Logger.Level -> T.Text -> IO ()
logWith config level message = do
  let minLevel = confMinLevel config
  when (level <= minLevel) $ do
    logMessage <- makeLogMessage level message
    outputHandle <- confFileHandle config
    TIO.hPutStrLn outputHandle logMessage
