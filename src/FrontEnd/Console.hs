{-# LANGUAGE OverloadedStrings #-}

-- | The console front-end is responsible for console I/O and
-- appropriate handling of other high-level bot interactions (menu
-- output etc).
module FrontEnd.Console
  ( run,
    Handle (..),
  )
where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified EchoBot as EB
import Text.Read (readMaybe)


type BotHandle = EB.Handle IO T.Text

newtype Handle = Handle
  { hBotHandle :: BotHandle
  }

betweenNum :: (Ord a, Num a) => a -> a -> a -> Bool
betweenNum l r a = l <= a && a <= r

findEvent :: EB.RepetitionCount -> [(EB.RepetitionCount, EB.Event T.Text)] -> EB.Event T.Text
findEvent _ [] = EB.MessageEvent "You entered the correct number, but something went wrong."
findEvent repetitionCount ((repetitionCountCurrentEvent, currentEvent) : otherEvents) =
  if repetitionCount == repetitionCountCurrentEvent
  then currentEvent
  else findEvent repetitionCount otherEvents

doSomethingBasedOnResponce :: BotHandle -> EB.Response T.Text -> IO ()
doSomethingBasedOnResponce _ (EB.MessageResponse message) = TIO.putStrLn message
doSomethingBasedOnResponce botHandle (EB.MenuResponse title events) = doWhileBadInput
  where 
    badInput = do
      TIO.putStrLn "You wrote incorrect number. You can try again."
      doWhileBadInput
    doWhileBadInput = do
      TIO.putStrLn title
      TIO.putStrLn "Write number from 1 to 5: "
      textNumber <- getLine
      let maybeNumber = (readMaybe textNumber) :: (Maybe Int)
      case maybeNumber of
        Just number -> 
          if betweenNum 1 5 number
          then do
            let event = findEvent number events
            responces <- EB.respond botHandle event
            doSomethingWithResponces botHandle responces
          else badInput
        Nothing -> badInput

doSomethingWithResponces :: BotHandle -> [EB.Response T.Text] -> IO ()
doSomethingWithResponces botHandle = mapM_ (doSomethingBasedOnResponce botHandle)

readAndSendMessage :: Handle -> IO ()
readAndSendMessage handle = do
  let botHandle = hBotHandle handle
  line <- TIO.getLine
  responces <- EB.respond botHandle (EB.MessageEvent line)
  doSomethingWithResponces botHandle responces

run :: Handle -> IO ()
run handle = do
  TIO.putStrLn "Welcome to the echo-bot!"
  -- 1. Read a line from the console.
  -- 2. Send it to the bot, get its response and output it.
  -- 3. Go to 1.
  mapM_ readAndSendMessage (repeat handle)
