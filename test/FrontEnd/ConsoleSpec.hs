{-# LANGUAGE OverloadedStrings #-}

module FrontEnd.ConsoleSpec
  ( spec,
  )
where

import Control.Monad (when)
import Control.Monad.State (State, get, put, execState)
import Test.Hspec (Spec, describe, it, shouldBe)
import qualified Data.Text as T
import qualified FrontEnd.Console
import qualified Logger
import qualified EchoBot
import Logger ((.<))

spec :: Spec
spec = 
  describe "Console" $ do
    it "by default one repetition" $ do
      let console = consoleByInput ["one", "two"]
      let finishConsole = evalOutputWithConsole console
      getInputConsole finishConsole `shouldBe` []
      getOutputConsole finishConsole `shouldBe` 
        [ "[Info] Echoing user input: \"one\"", 
          "one",
          "[Info] Echoing user input: \"two\"",
          "two"
        ]
    it "help command" $ do
      let console = consoleByInput ["help", "/help"]
      let finishConsole = evalOutputWithConsole console
      getInputConsole finishConsole `shouldBe` []
      getOutputConsole finishConsole `shouldBe`
        [ "[Info] Echoing user input: \"help\""
        , "help"
        , "[Info] Got the help command"
        , "help message"
        ]
    it "repeat command" $ do
      let console = consoleByInput ["one", "/repeat", "3", "three", "/repeat", "two", "2", "two"]
      let finishConsole = evalOutputWithConsole console
      getInputConsole finishConsole `shouldBe` []
      getOutputConsole finishConsole `shouldBe`
        [ "[Info] Echoing user input: \"one\""
        , "one"
        , "[Info] Got the repeat command"
        , "repeat message 1"
        , "Write number from 1 to 5: "
        , "[Info] The user has set the repetition count to 3"
        , "You changed how many times I will repeat your massages."
        , "[Info] Echoing user input: \"three\""
        , "three"
        , "three"
        , "three"
        , "[Info] Got the repeat command"
        , "repeat message 3"
        , "Write number from 1 to 5: "
        , "You wrote incorrect number. You can try again."
        , "repeat message 3"
        , "Write number from 1 to 5: "
        , "[Info] The user has set the repetition count to 2"
        , "You changed how many times I will repeat your massages."
        , "[Info] Echoing user input: \"two\""
        , "two"
        , "two"
        ]


type ConsoleState = State (EchoBot.State, Console)


evalOutputWithConsole :: Console -> Console
evalOutputWithConsole console = finishConsole
  where
    conf = stubConfig
    state = stateWith conf
    consoleEmulator = testRunWhileNonEmptyInput $ consoleHandle (botHandle conf)
    (_, finishConsole) = execState consoleEmulator (state, console)

consoleByInput :: [T.Text] -> Console
consoleByInput input = Console {
  outputs = [],
  inputs = input
}

testRunWhileNonEmptyInput :: FrontEnd.Console.Handle ConsoleState -> ConsoleState ()
testRunWhileNonEmptyInput handle = do
  (_, console) <- get
  let allInput = inputs console
  when (not (null allInput)) $ do
    testRun handle
    testRunWhileNonEmptyInput handle

testRun :: FrontEnd.Console.Handle ConsoleState -> ConsoleState ()
testRun = FrontEnd.Console.readAndSendMessage getInput displayOutput

getInput :: ConsoleState T.Text
getInput = do
  (state, console) <- get
  let allInput = inputs console
      input = head allInput
  put $ (state, console { inputs = tail allInput })
  return input

displayOutput :: T.Text -> ConsoleState ()
displayOutput output = do
  (state, console) <- get
  put $ (state, console { outputs = output : outputs console })

data Console = Console {
  outputs :: [T.Text],
  inputs :: [T.Text]
} deriving (Eq, Show)

logHandle :: Logger.Handle ConsoleState
logHandle = Logger.Handle {
  Logger.hLowLevelLog = 
    \level text -> do
      (state, console) <- get
      put $ (state, console {outputs = ("[" .< level <> "] " <> text) : outputs console})
}

botHandle :: EchoBot.Config -> EchoBot.Handle ConsoleState T.Text
botHandle config = EchoBot.Handle {
  EchoBot.hLogHandle = logHandle,
  EchoBot.hConfig = config,
  EchoBot.hGetState = do
    (state, _) <- get
    return state,
  EchoBot.hModifyState' = \f -> do
    (state, console) <- get
    put $ (f state, console),
  EchoBot.hTextFromMessage = Just,
  EchoBot.hMessageFromText = id
}

consoleHandle :: EchoBot.Handle ConsoleState T.Text -> FrontEnd.Console.Handle ConsoleState
consoleHandle botH = FrontEnd.Console.Handle { FrontEnd.Console.hBotHandle = botH }

getOutputConsole :: Console -> [T.Text]
getOutputConsole (Console {outputs = out}) = reverse out

getInputConsole :: Console -> [T.Text]
getInputConsole (Console {inputs = input}) = reverse input

stubConfig :: EchoBot.Config
stubConfig =
  EchoBot.Config
    { EchoBot.confRepeatReply = "repeat message {count}",
      EchoBot.confHelpReply = "help message",
      EchoBot.confRepetitionCount = 1
    }

stateWith :: EchoBot.Config -> EchoBot.State
stateWith = either (error . T.unpack) id . EchoBot.makeState
