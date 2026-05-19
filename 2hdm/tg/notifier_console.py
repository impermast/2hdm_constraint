import sys
from tg_bot import tg

def send_msg(message):
    try:
        bot = tg()
        bot.notify(message)
    except Exception as e:
        print(f"Ошибка при отправке сообщения: {e}")

if __name__ == "__main__":
    try:
        if len(sys.argv) > 1:
            message = " ".join(sys.argv[1:])
            send_msg(message)
            print("Message sent.")
        else:
            print("No message to send.")
    except Exception as error:
        print(f"Send message: {error}")
