# echo-bot


`echo-bot` is a programme which repeated your messages. 

# Feachers

* Command `/help` will show you a help message.
* Command `/repeat` will allow you to change how many times `echo-bot` will repeat your messages.  

# How to run `echo-bot`?

1. Clone this repository.
2. You have to get your telegram bot token. You can get it in [BotFather](https://t.me/BotFather) Telegram bot.
3. You have to inform your token to `echo-bot`. For this, you need to create file `token.cfg` and take advantage of `token-template.cfg`. You have to replace substring `<token>` with your token.
4. To run `echo-bot` you need to use [Stack](https://docs.haskellstack.org/en/stable/README/). Use command
    
        stack run

for run `echo-bot`.

# Configurations

Start configurations bot is in `configurations.cfg`.

1. **bot_type** contains when the bot will work. The value of field `type` must be `"telegram"` or `"console"`. If the value is `"telegram"` then the bot will run in your telegram bot. If the value is `"console"` then the bot will run in the console.

2. **bot_messages** contains default messages for `/help` and `/repeat` command as well as a default value for the repetition count.

3. **log** contains a minimal level log. The value of field `min_level` must be  `'error'`, `'warning'`, `'info'` or `'debug'`.