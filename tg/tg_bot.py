import telegram
import json
import asyncio,os
from telegram import error


async def test_telegram_connection(bot):
    """Тестовая отправка сообщения."""
    token, chat_id = load_telegram_config()
    if token is None or chat_id is None:
        print("Не удалось загрузить конфигурацию Telegram.")
        return
    bot = tg()
    print("Запускаю функцию")
    await bot.send_telegram_message("Тестовое сообщение")
    print("Тестовое сообщение отправлено")


async def edit_or_send_msg(update, context, msg_txt,markup=None):
    if update.message:
        try:
            if 'bot_message_id' in context.user_data:
                await update.message.edit_text(text=msg_txt)
                await update.message.edit_reply_markup(reply_markup=markup)
            else:
                sent_message = await update.message.reply_text(msg_txt, reply_markup=markup)
                context.user_data['bot_message_id'] = sent_message.message_id
        except error.BadRequest:
            sent_message = await update.message.reply_text(msg_txt, reply_markup=markup)
            context.user_data['bot_message_id'] = sent_message.message_id
    elif update.callback_query:
        sent_message = await update.callback_query.edit_message_text(msg_txt, reply_markup=markup)
        context.user_data['bot_message_id'] = sent_message.message_id


# Функция для получения токена и ID чата из конфигурации
def load_telegram_config(config_path="tg/config.json"):
    """Загружает конфигурацию из файла config.json"""
    try:
        with open(config_path, "r") as config_file:
            config = json.load(config_file)
        return config.get("telegram_token"), config.get("telegram_chat_id")
    except FileNotFoundError:
        print(f"Ошибка: файл конфигурации '{config_path}' не найден.")
        return None, None
    except json.JSONDecodeError:
        print(f"Ошибка при разборе конфигурационного файла '{config_path}'.")
        return None, None

class tg:
    def __init__(self):
        self.token, self.chat_id = load_telegram_config()
        self.bot = telegram.Bot(token=self.token)
    
    def get_token(self):
        return self.token, self.chat_id
    

    def notify(self, message="Рендеринг завершен."):
        """Отправить сообщение в Telegram после завершения рендеринга.""" 
        asyncio.run(self.send_telegram_message(message))
        
    def video(self, name, path="media/videos/1080p60/"):
        current_dir = os.path.dirname(os.path.abspath(__file__))
        abs_path = os.path.join(current_dir,path+name+".mp4")
        asyncio.run(self.send_telegram_video(abs_path))

        #Async block

    async def send_telegram_message(self, message):
        """Отправка сообщения в Telegram."""
        try:
            await self.bot.send_message(chat_id=self.chat_id, text=message)
        except telegram.error.TelegramError as e:
            print(f"Ошибка при отправке сообщения: {e}")

    async def notify_async(self, message="Рендеринг завершен."):
        """Отправить сообщение в Telegram после завершения рендеринга.""" 
        await self.send_telegram_message(message)
        
    async def video_async(self, name, path="media/videos/1080p60/"):
        current_dir = os.path.dirname(os.path.abspath(__file__))
        abs_path = os.path.join(current_dir,path+name+".mp4")
        await self.send_telegram_video(abs_path)

    async def send_telegram_video(self, video_path):
        try:
            with open(video_path, 'rb') as video_file:
                await self.bot.send_video(chat_id=self.chat_id, video=video_file)
        except telegram.error.TelegramError as e:
            print(f"Ошибка при отправке видео: {e}")
        except FileNotFoundError:
            print(f"Видео не найдено по пути: {video_path}")
 


# Вызывайте тестовую функцию через asyncio.run
if __name__ == "__main__":
    bot = tg()
    asyncio.run(test_telegram_connection(bot))
