module FrontEnd.Telegram 

where

import qualified EchoBot as EB
import qualified Data.Text as T

newtype Handle = Handle {
    hBotHandle :: EB.Handle IO TelegramMessages
}

data TelegramMessages = Text T.Text | Sticker StickerId

type StickeId = T.Text




run :: Handle -> IO ()
run = _r 
