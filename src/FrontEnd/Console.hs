{-# LANGUAGE OverloadedStrings #-}

-- | The console front-end is responsible for console I/O and
-- appropriate handling of other high-level bot interactions (menu
-- output etc).
module FrontEnd.Console
  ( run,
    runImp,
    readAndSendMessage,
    Handle (..),
  )
where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified EchoBot as EB
import Text.Read (readMaybe)

newtype Handle m = Handle
  { hBotHandle :: EB.Handle m T.Text
  }

betweenNum :: (Ord a, Num a) => a -> a -> a -> Bool
betweenNum l r a = l <= a && a <= r

doSomethingBasedOnResponce :: Monad m => m T.Text -> (T.Text -> m ()) -> EB.Handle m T.Text -> EB.Response T.Text -> m ()
doSomethingBasedOnResponce _ displayOutput _ (EB.MessageResponse message) = displayOutput message
doSomethingBasedOnResponce getInput displayOutput botHandle (EB.MenuResponse title events) = doWhileBadInput
  where
    badInput = do
      displayOutput "You wrote incorrect number. You can try again."
      doWhileBadInput
    doWhileBadInput = do
      displayOutput title
      displayOutput "Write number from 1 to 5: "
      textNumber <- getInput
      let maybeNumber = readMaybe (T.unpack textNumber) :: (Maybe Int)
      case maybeNumber of
        Just number ->
          if betweenNum 1 5 number
            then do
              let event = EB.findEvent number botHandle events
              responces <- EB.respond botHandle event
              doSomethingWithResponces getInput displayOutput botHandle responces
            else badInput
        Nothing -> badInput

doSomethingWithResponces :: Monad m => m T.Text -> (T.Text -> m ()) -> EB.Handle m T.Text -> [EB.Response T.Text] -> m ()
doSomethingWithResponces getInput displayOutput botHandle = mapM_ (doSomethingBasedOnResponce getInput displayOutput botHandle)

readAndSendMessage :: Monad m => m T.Text -> (T.Text -> m ()) -> Handle m -> m ()
readAndSendMessage getInput displayOutput handle = do
  let botHandle = hBotHandle handle
  line <- getInput
  responces <- EB.respond botHandle (EB.MessageEvent line)
  doSomethingWithResponces getInput displayOutput botHandle responces

runImp :: Monad m => m T.Text -> (T.Text -> m ()) -> Handle m -> m ()
runImp getInput displayOutput handle = do
  displayOutput "Welcome to the echo-bot!"
  -- 1. Read a line from the console.
  -- 2. Send it to the bot, get its response and output it.
  -- 3. Go to 1.
  mapM_ (readAndSendMessage getInput displayOutput) (repeat handle)

run :: Handle IO -> IO ()
run = runImp TIO.getLine TIO.putStrLn
