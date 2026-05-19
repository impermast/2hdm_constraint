# 2HDM Constraint — Setup

## Требования
- Windows 10/11 с WSL2 (Ubuntu 24.04)
- WSL установлен на D:\WSL\Ubuntu-24.04

## Первый запуск (один раз)
Открой WSL и выполни:
```bash
cd /mnt/d/progs/2hdm_constraint/setup
source activate.sh
```

## Каждый раз перед работой
```bash
source /mnt/d/progs/2hdm_constraint/setup/activate.sh
```

## Установленные пакеты
- lhapdf >= 6.5.4 (собран из исходников)
- numpy, pandas, matplotlib
- python-telegram-bot

## Структура
- venv/                  — виртуальное Python окружение
- lhapdf_install/        — собранная библиотека LHAPDF
- LHAPDF-6.5.4/          — исходники LHAPDF (можно удалить после сборки)
