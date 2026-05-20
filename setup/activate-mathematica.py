import argparse
import sys

def get_password(math_id):
    """
    Генерирует пароль для Mathematica 13.1
    
    Args:
        math_id: Machine ID (строка)
    
    Returns:
        password: Сгенерированный пароль
    """
    # Фиксированные значения для версии 13.1
    DEFAULT_KEY = '3893-9258-K6XJLE'
    DEFAULT_TYPE = '803000'
    DEFAULT_CLASS = 'CC'
    
    # Используем первую соль из списка для mathematica:13.0-14.1
    SALT = 0x5417  # Первый элемент из SALT_MAP['mathematica:13.0-14.1']
    
    def build_key_string(math_id, act_key):
        """Строит строку ключа"""
        return f"{math_id}&{act_key}"
    
    def process_salt(n, byte, c):
        """Обработка соли через битовые операции"""
        for i in range(8):
            bit = (byte >> i) & 1
            if bit + ((n - bit) & ~1) == n:
                n = (n - bit) >> 1
            else:
                n = ((c - bit) ^ n) >> 1
        return n
    
    def gen_password(key_str, salt):
        """Генерирует пароль из строки ключа и соли"""
        chars = [ord(c) for c in key_str]
        
        # Обработка первой соли
        salt1 = salt
        for i in range(len(chars) - 1, -1, -1):
            salt1 = process_salt(salt1, chars[i], 0x105C3)
        
        salt1 = salt1 ^ 0xCEDF
        salt1 = int(((salt1 + 0x72FA) & 0xFFFF) * 99999 / 0xFFFF)
        
        offset1 = f"{salt1:05d}"
        
        # Обработка второй соли
        salt2_parts = offset1[0:2] + offset1[3:5] + offset1[2:3]
        salt2 = int(salt2_parts)
        salt2 = int((salt2 / 99999.0) * 0xFFFF) + 1
        salt2 = process_salt(process_salt(0, salt2 & 0xFF, 0x1064B), salt2 >> 8, 0x1064B)
        
        for i in range(len(chars) - 1, -1, -1):
            salt2 = process_salt(salt2, chars[i], 0x1064B)
        
        salt2 = salt2 ^ 0xEF22
        salt2 = int((salt2 & 0xFFFF) * 99999 / 0xFFFF)
        
        offset2 = f"{salt2:05d}"
        
        # Формирование пароля
        password = [
            offset2[3],
            offset1[3],
            offset1[1],
            offset1[0],
            '-',
            offset2[4],
            offset1[2],
            offset2[0],
            '-',
            offset2[2],
            offset1[4],
            offset2[1]
        ]
        
        return ''.join(password)
    
    # Построение строки ключа
    key_str = build_key_string(math_id, DEFAULT_KEY)
    
    # Генерация пароля
    password = gen_password(key_str, SALT)
    
    return password




def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--mid",
        required=True,
        help="Machine ID in format xxxx-xxxxx-xxxxx"
    )

    args = parser.parse_args()

    password = get_password(args.mid)

    print(password)
    return 0


if __name__ == "__main__":
    sys.exit(main())
