# 2hdm_constraint

## Description

This project focuses on studying constraints on the **Two-Higgs-Doublet Model (2HDM)** parameters using **anomalous vertex functions**. The methodology is based on analyzing the **anomalous coupling coefficients** (ZZZ) and (ZWW), derived from LHC experimental data.

Calculations are performed in **Wolfram Mathematica** using several key packages:
- **FeynArts** – for generating Feynman diagrams.
- **FeynCalc** – for computing process amplitudes.
- **LoopTools** – for Passarino-Veltman reduction.
- **LHAPDF** – for working with Parton Distribution Functions (PDFs).

Additionally, a test process **pp → H → gg** is used to verify cross-section calculations with PDFs.

---

## **Project Structure**

### **1. `modules/` – Core modules**
- `FunctionalModules.wl` – Core utility functions for timing, progress tracking, and Telegram notifications.
- `ModelParams.wl` – 2HDM parameter definitions and analytic replacements.
- `SaveToCSVmodule.wl` – CSV export functionality for data analysis.
- `setup.m` – Module initialization and configuration.

### **2. `ZZZ_ZWW/` – Main calculations for vertex functions**
Main scripts for calculating amplitudes and interaction coefficients:
- `f4calc_ZWW.wl` – Amplitude and coefficient extraction for ZWW process.
- `f4calc_ZZZ.wl` – Amplitude and coefficient extraction for ZZZ process.
- `f6calc_ZWW.wl` – f6 coefficient calculations for ZWW.
- `SCRIPTqqZZZ.wls` – Executable script for qq → Z → ZZ process.
- `SCRIPTcross.wls` – Cross-section calculation and result saving.
- `SCRIPT_f4meanvalue.wls` – Mean value calculations for f4 coefficients.
- `CSV_generator.wl` – CSV data generation for analysis.
- `GRAPHs_Metod1.wl`, `GRAPHs_Metod2.wl`, `GRAPHs_Metod3.wl` – Graph generation methods.
- `run_f4mean_scan.sh` – Bash script for f4 mean value scanning.
- `TESTqqZZZ.wl` – Test file for developing new computational methods.
- `TestPVphysicalError.wl` – Test file for checking physical errors in PV process.

Subdirectories:
- `buffer/` – Serialized intermediate calculations (.mx), CSV data, and numerical results.
- `subgraphs/` – Additional subgraphs and plotting scripts.

### **3. `pdfcode/` – Working with Parton Distribution Functions (PDFs)**
- `PDF_csv.py` – Python script for extracting specific PDF values.
- `xPDF.py` – Extended PDF extraction and processing utilities.
- `pdf_mathematica_test.wl` – Example of how to call PDFs in Mathematica.

### **4. `ppHgg/` – Cross-section testing**
Auxiliary process **pp → H → gg** for validating cross-section computation with PDFs:
- `ppHgg_HCmodel.wl` – HC model implementation.
- `qAqg_check.wl`, `qeqe_check.wl`, `qqee_check.wl` – Process validation.
- `testingHC.wl` – HC model testing.

### **5. `setup/` – Installation files**
- `looptools_install.sh` – LoopTools installation script for Linux.
- `LoopTools-Cygwin.exe` – LoopTools executable for Windows.
- `cygwin1.dll`, `ml64i4.dll`, `wstp64i4.dll` – Required DLLs.
- `THDM.nb` – Mathematica notebook for THDM setup.
- `THDMCPV.mod`, `THDMCPV.pars` – FeynArts model files for CP-violating THDM.

### **6. `tg/` – Telegram bot for computation notifications**
- `notifier_console.py` – Console-based Telegram notifier.
- `tg_bot.py` – Telegram bot implementation.

The bot interacts with Mathematica through the `PrintTG[string]` function, which sends a message to a specified Telegram chat.

---

## **Installation & Setup**

### **Prerequisites**
1. **Wolfram Mathematica** with `FeynArts`, `FeynCalc`, and `LoopTools` installed.
2. **Python 3** for working with PDFs and Telegram bot.
3. **LHAPDF** for accessing and managing parton distribution functions.

### **LoopTools Installation**

#### **Windows**
1. Put everything into:
   `%APPDATA%\Mathematica\Applications\LoopTools\Windows-x86-64`
2. Required files:
   - `LoopTools.exe` (unpacked from `LoopTools-Cygwin.exe.gz`)
   - `ml64i4.dll`, `wstp64i4.dll` (from Mathematica)
   - `cygwin1.dll` (+ any other missing `cyg*.dll`)

3. Test in Mathematica:
```wl
exe  = FileNameJoin[{$UserBaseDirectory,"Applications","LoopTools","Windows-x86-64","LoopTools.exe"}];
link = Install[exe];
N@B0[1000,50,80]
Uninstall[link];
```

> Do **not** run `LoopTools.exe` manually. Always use `Install[...]`.

#### **Linux**
1. Unpack and enter sources:
```bash
tar -xzf LoopTools-2.16.tar.gz
cd LoopTools-2.16
```

2. Add WSTP DeveloperKit to `PATH`:
```bash
export PATH="$(math -noprompt -run 'Print[FileNameJoin[{$InstallationDirectory,"SystemFiles","Links","WSTP","DeveloperKit",$SystemID,"CompilerAdditions"}]]; Quit[]' | tr -d '\r'):$PATH"
```

3. Configure / build / install:
```bash
PREFIX="$(math -noprompt -run 'Print[FileNameJoin[{$UserBaseDirectory,"Applications","LoopTools"}]]; Quit[]' | tr -d '\r')"
./configure --prefix="$PREFIX"
make -j"$(nproc)"
make install
```

4. Test in Mathematica:
```wl
link = Install[FileNameJoin[{$UserBaseDirectory,"Applications","LoopTools","bin","LoopTools"}]];
N@B0[1000,50,80]
Uninstall[link];
```

### **Python Dependencies**
```bash
pip install numpy pandas matplotlib
```

### **Telegram Bot Setup**
1. Create a bot with @BotFather on Telegram.
2. Get the bot token.
3. Update `tg_bot.py` with your token.
4. Start the bot to receive notifications.

---

## **Contact**

For any questions, feel free to contact:

- **Dmitry Kalashnikov**: <dskalashnikov@mephi.ru>

---

# Описание

Этот проект посвящён исследованию ограничений на параметры **Двуххиггсовской модели (2HDM)** с использованием **аномальных вершинных функций**. Методология основана на анализе **коэффициентов аномальных взаимодействий** (ZZZ) и (ZWW), полученных из экспериментальных данных LHC.

Расчёты выполняются в **Wolfram Mathematica** с использованием нескольких ключевых пакетов:
- **FeynArts** – для генерации диаграмм Фейнмана.
- **FeynCalc** – для вычисления амплитуд процессов.
- **LoopTools** – для редукции интегралов Пасарино-Вельтмана.
- **LHAPDF** – для работы с функциями плотности распределения частиц (PDF).

Дополнительно для проверки расчётов сечения с использованием PDF анализируется тестовый процесс **pp → H → gg**.

---

## **Структура проекта**

### **1. `modules/` – Основные модули**
- `FunctionalModules.wl` – Утилитные функции для тайминга, отслеживания прогресса и уведомлений Telegram.
- `ModelParams.wl` – Определения параметров 2HDM и аналитические замены.
- `SaveToCSVmodule.wl` – Экспорт данных в CSV для анализа.
- `setup.m` – Инициализация модулей и конфигурация.

### **2. `ZZZ_ZWW/` – Основные вычисления вершинных функций**
Основные скрипты для расчёта амплитуд и коэффициентов взаимодействий:
- `f4calc_ZWW.wl` – Расчёт амплитуд и коэффициентов для процесса ZWW.
- `f4calc_ZZZ.wl` – Расчёт амплитуд и коэффициентов для процесса ZZZ.
- `f6calc_ZWW.wl` – Расчёт коэффициентов f6 для ZWW.
- `SCRIPTqqZZZ.wls` – Исполняемый скрипт для процесса qq → Z → ZZ.
- `SCRIPTcross.wls` – Расчёт сечения и сохранение результатов.
- `SCRIPT_f4meanvalue.wls` – Расчёт средних значений коэффициентов f4.
- `CSV_generator.wl` – Генерация CSV данных для анализа.
- `GRAPHs_Metod1.wl`, `GRAPHs_Metod2.wl`, `GRAPHs_Metod3.wl` – Методы генерации графиков.
- `run_f4mean_scan.sh` – Bash скрипт для сканирования средних значений f4.
- `TESTqqZZZ.wl` – Тестовый файл для разработки новых методов.
- `TestPVphysicalError.wl` – Проверка физических ошибок в процессах PV.

Поддиректории:
- `buffer/` – Сериализованные промежуточные расчёты (.mx), CSV данные и численные результаты.
- `subgraphs/` – Дополнительные подграфы и скрипты для построения графиков.

### **3. `pdfcode/` – Работа с функциями плотности распределения (PDF)**
- `PDF_csv.py` – Python скрипт для извлечения значений PDF.
- `xPDF.py` – Расширенные утилиты для обработки PDF.
- `pdf_mathematica_test.wl` – Пример вызова PDF в Mathematica.

### **4. `ppHgg/` – Тестирование сечения**
Вспомогательный процесс **pp → H → gg** для валидации расчёта сечения с PDF:
- `ppHgg_HCmodel.wl` – Реализация HC модели.
- `qAqg_check.wl`, `qeqe_check.wl`, `qqee_check.wl` – Валидация процессов.
- `testingHC.wl` – Тестирование HC модели.

### **5. `setup/` – Файлы установки**
- `looptools_install.sh` – Скрипт установки LoopTools для Linux.
- `LoopTools-Cygwin.exe` – Исполняемый файл LoopTools для Windows.
- `cygwin1.dll`, `ml64i4.dll`, `wstp64i4.dll` – Необходимые библиотеки.
- `THDM.nb` – Mathematica ноутбук для настройки THDM.
- `THDMCPV.mod`, `THDMCPV.pars` – Файлы модели FeynArts для CP-нарушающей THDM.

### **6. `tg/` – Telegram бот для уведомлений о вычислениях**
- `notifier_console.py` – Консольный уведомитель Telegram.
- `tg_bot.py` – Реализация Telegram бота.

Бот взаимодействует с Mathematica через функцию `PrintTG[string]`, которая отправляет сообщение в указанный чат Telegram.

---

## **Установка и настройка**

### **Предварительные требования**
1. **Wolfram Mathematica** с установленными пакетами: `FeynArts`, `FeynCalc`, `LoopTools`.
2. **Python 3** для работы с PDF и Telegram ботом.
3. **LHAPDF** для доступа к функциям плотности распределения частиц.

### **Установка LoopTools**

#### **Windows**
1. Поместите все файлы в:
   `%APPDATA%\Mathematica\Applications\LoopTools\Windows-x86-64`
2. Необходимые файлы:
   - `LoopTools.exe` (распакованный из `LoopTools-Cygwin.exe.gz`)
   - `ml64i4.dll`, `wstp64i4.dll` (из Mathematica)
   - `cygwin1.dll` (+ другие недостающие `cyg*.dll`)

3. Тестирование в Mathematica:
```wl
exe  = FileNameJoin[{$UserBaseDirectory,"Applications","LoopTools","Windows-x86-64","LoopTools.exe"}];
link = Install[exe];
N@B0[1000,50,80]
Uninstall[link];
```

> **Не** запускайте `LoopTools.exe` вручную. Всегда используйте `Install[...]`.

#### **Linux**
1. Распакуйте и перейдите в директорию:
```bash
tar -xzf LoopTools-2.16.tar.gz
cd LoopTools-2.16
```

2. Добавьте WSTP DeveloperKit в `PATH`:
```bash
export PATH="$(math -noprompt -run 'Print[FileNameJoin[{$InstallationDirectory,"SystemFiles","Links","WSTP","DeveloperKit",$SystemID,"CompilerAdditions"}]]; Quit[]' | tr -d '\r'):$PATH"
```

3. Конфигурация / сборка / установка:
```bash
PREFIX="$(math -noprompt -run 'Print[FileNameJoin[{$UserBaseDirectory,"Applications","LoopTools"}]]; Quit[]' | tr -d '\r')"
./configure --prefix="$PREFIX"
make -j"$(nproc)"
make install
```

4. Тестирование в Mathematica:
```wl
link = Install[FileNameJoin[{$UserBaseDirectory,"Applications","LoopTools","bin","LoopTools"}]];
N@B0[1000,50,80]
Uninstall[link];
```

### **Зависимости Python**
```bash
pip install numpy pandas matplotlib
```

### **Настройка Telegram бота**
1. Создайте бота через @BotFather в Telegram.
2. Получите токен бота.
3. Обновите `tg_bot.py` своим токеном.
4. Запустите бота для получения уведомлений.

---

## **Контакты**

По всем вопросам обращайтесь:

- **Дмитрий Калашников**: <dskalashnikov@mephi.ru>