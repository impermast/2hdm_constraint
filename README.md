
# 2hdm_constraint

## Description

This project focuses on studying constraints on the **Two-Higgs-Doublet Model (2HDM)** parameters using **anomalous vertex functions**. The methodology is based on analyzing the **anomalous coupling coefficients** \( ZZZ \) and \( ZWW \), derived from LHC experimental data.

Calculations are performed in **Wolfram Mathematica** using several key packages:
- **FeynArts** – for generating Feynman diagrams.
- **FeynCalc** – for computing process amplitudes.
- **LoopTools** – for Passarino-Veltman reduction.
- **LHAPDF** – for working with Parton Distribution Functions (PDFs).

Additionally, a test process **pp → H → gg** is used to verify cross-section calculations with PDFs.

---

## **Project Structure**

### **1. `ZZZ_ZWW/` – Main calculations for vertex functions**
This directory contains the main scripts for calculating amplitudes and interaction coefficients:
- `Vertex_graphs.wl` – generation of graphs and analysis of vertex functions.
- `f4calc_ZWW.wl` – calculation of amplitude and extraction of coefficients for the \( ZWW \) process.
- `f4calc_ZZZ.wl` – calculation of amplitude and extraction of coefficients for the \( ZZZ \) process.
- `SCRIPTqqZZZ.wls` – executable script for calculating the amplitude and square of the matrix element for the \( q\bar{q} \to Z \to ZZ \) process.
- `SCRIPTcross.wls` – calculates the **cross-section** of the process and saves the results to the buffer.
- `TESTqqZZZ.wl` – test file for developing new computational methods.
- `TestPVphysicalError.wl` – test file for checking physical errors in the \( PV \) process.

Additionally, this directory stores **backup copies of calculation results**:
- `.mx` files – serialized intermediate calculations.
- `.txt` files – saved numerical values of coefficients.

All buffer files, intermediate results, and additional graphs are stored in:
- `ZZZ_ZWW/buffer/` – temporary buffer files for storing intermediate data.
- `ZZZ_ZWW/graphs/` – various graphical representations of calculations.
- `ZZZ_ZWW/subgraphs/` – additional subgraphs related to the analysis.

---

### **2. `automation/` – Automated cross-section scanning**
This directory contains scripts for **automated calculations of \( \sigma(mh_2) \)**.

- `run_mh2_scan.sh` – **Bash script** that loops over different values of \( mh_2 \) and launches `SCRIPTcross.wls`.
- `plot_cs_data.wls` – **WolframScript** that reads stored cross-section values and plots \( \sigma(mh_2) \) with uncertainties.

Results are stored in:
- `buffer/cs_uuZZZ.wl` – Cross-section results.
- `subgraphs/cross_sectionMh2.png` – Generated plot.

---

### **3. `pdfcode/` – Working with Parton Distribution Functions (PDFs)**
This directory contains a set of **precomputed CSV files** with PDFs for various particles and energies.

Key files:
- `PDF_csv.py` – Python script for extracting specific PDF values.
- `pdf_mathematica_test.wl` – Example of how to call PDFs in Mathematica.

---

### **4. `ppHgg/` – Cross-section testing**
This directory focuses on the auxiliary process **pp → H → gg**, used to validate the cross-section computation method with PDFs.

---

### **5. `tg/` – Telegram bot for computation notifications**
This directory contains files related to the Telegram bot, which sends notifications about the status of computations in **Wolfram Mathematica**.  

The bot interacts with Mathematica through the `PrintTG[string]` function, which sends a message with the provided `string` parameter to a specified Telegram chat.

---

## **Installation & Setup**

To work with the project, you need:
1. **Wolfram Mathematica** with `FeynArts`, `FeynCalc`, and `LoopTools` installed.
2. **Python 3** for working with PDFs (`pdfcode/`).
3. **LHAPDF** for accessing and managing parton distribution functions.

# LoopTools install
# Windows (use prebuilt `LoopTools-Cygwin.exe`)
1) Put everything into:
`%APPDATA%\Mathematica\Applications\LoopTools\Windows-x86-64\`
Required files in that folder:
- `LoopTools.exe` (unpacked from `LoopTools-Cygwin.exe.gz`)
- `ml64i4.dll`, `wstp64i4.dll` (from Mathematica:
  `...\Mathematica\<ver>\SystemFiles\Libraries\Windows-x86-64\`)
- `cygwin1.dll` (+ any other missing `cyg*.dll` if Windows asks for them)

2) Test in Mathematica:
```wl
exe  = FileNameJoin[{$UserBaseDirectory,"Applications","LoopTools","Windows-x86-64","LoopTools.exe"}];
link = Install[exe];
N@B0[1000,50,80]
Uninstall[link];
```
> Do **not** run `LoopTools.exe` manually. Always use `Install[...]`.

# Linux (build from source)

1. Unpack and enter sources:
```bash
tar -xzf LoopTools-2.16.tar.gz
cd LoopTools-2.16
```
2. Add WSTP DeveloperKit to `PATH`:

```bash
export PATH="$(math -noprompt -run 'Print[FileNameJoin[{$InstallationDirectory,"SystemFiles","Links","WSTP","DeveloperKit",$SystemID,"CompilerAdditions"}]]; Quit[]' | tr -d '\r'):$PATH"
```
3. Configure / build / install into Mathematica Applications:
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
---

## **Contact**
For any questions, feel free to contact:
- **Dmitry Kalashnikov**: [dskalashnikov@mephi.ru](mailto:dskalashnikov@mephi.ru)



## Описание

Этот проект посвящён исследованию ограничений на параметры **Двуххиггсовской модели (2HDM)** с использованием **аномальных вершинных функций**. Методология основана на анализе **коэффициентов аномальных взаимодействий** \( ZZZ \) и \( ZWW \), полученных из экспериментальных данных LHC.

Расчёты выполняются в **Wolfram Mathematica** с использованием нескольких ключевых пакетов:
- **FeynArts** – для генерации диаграмм Фейнмана.
- **FeynCalc** – для вычисления амплитуд процессов.
- **LoopTools** – для редукции интегралов Пасарино-Вельтмана.
- **LHAPDF** – для работы с функциями плотности распределения партонов (PDF).

Дополнительно для проверки расчётов сечения с использованием PDF анализируется тестовый процесс **pp → H → gg**.

---

## **Структура проекта**

### **1. `ZZZ_ZWW/` – Основные вычисления для вершинных функций**
Этот каталог содержит основные скрипты для вычисления амплитуд и коэффициентов взаимодействий:
- `Vertex_graphs.wl` – генерация графиков и анализ вершинных функций.
- `f4calc_ZWW.wl` – расчёт амплитуды и извлечение коэффициентов для процесса \( ZWW \).
- `f4calc_ZZZ.wl` – расчёт амплитуды и извлечение коэффициентов для процесса \( ZZZ \).
- `SCRIPTqqZZZ.wls` – исполняемый скрипт для вычисления амплитуды и квадрата матричного элемента процесса \( q\bar{q} \to Z \to ZZ \).
- `SCRIPTcross.wls` – вычисляет **сечение процесса** и сохраняет результаты в буфер.
- `SCRIPT_f4meanvalue.wls` – исполняемый скрипт для вычисления среднего значения f4.
- `TESTqqZZZ.wl` – тестовый файл для разработки новых вычислительных методов.
- `TestPVphysicalError.wl` – тестовый файл для проверки физических ошибок в процессе \( PV \).

Дополнительно здесь хранятся **резервные копии результатов вычислений**:
- `.mx` файлы – сериализованные промежуточные вычисления.
- `.txt` файлы – сохранённые числовые значения коэффициентов.

Все буферные файлы, промежуточные результаты и дополнительные графики хранятся в:
- `ZZZ_ZWW/buffer/` – временные буферные файлы для хранения промежуточных данных.
- `ZZZ_ZWW/graphs/` – различные графические представления вычислений.
- `ZZZ_ZWW/subgraphs/` – дополнительные подграфики, относящиеся к анализу.

---

### **2. `automation/` – Автоматизированный перебор параметров и расчёт сечения**
Этот каталог содержит скрипты для **автоматизированных расчётов сечения \( \sigma(mh_2) \)**.

- `run_mh2_scan.sh` – **Bash-скрипт**, который перебирает различные значения \( mh_2 \) и запускает `SCRIPTcross.wls`.
- `plot_cs_data.wls` – **WolframScript**, который загружает сохранённые значения сечений и строит график \( \sigma(mh_2) \) с учётом погрешностей.

Результаты сохраняются в файлы:
- `buffer/cs_uuZZZ.wl` – рассчитанные значения сечения.
- `subgraphs/cross_sectionMh2.png` – сгенерированный график.

---

### **3. `pdfcode/` – Работа с функциями плотности распределения частиц (PDF)**
Этот каталог содержит набор **предварительно вычисленных CSV-файлов** с PDF-функциями для различных частиц и энергий.

Основные файлы:
- `PDF_csv.py` – Python-скрипт для извлечения конкретных значений PDF.
- `pdf_mathematica_test.wl` – пример вызова PDF-функций в Mathematica.

---

### **4. `ppHgg/` – Тестирование расчёта сечения**
Этот каталог посвящён вспомогательному процессу **pp → H → gg**, который используется для проверки метода расчёта сечения с PDF.

---

### **5. `tg/` – Telegram-бот для уведомлений о вычислениях**
Этот каталог содержит файлы, связанные с Telegram-ботом, который отправляет уведомления о ходе вычислений в **Wolfram Mathematica**.  

Бот взаимодействует с Mathematica через функцию `PrintTG[string]`, которая отправляет сообщение с переданной строкой `string` в указанный Telegram-чат.

---

## **Установка и настройка**

Для работы с проектом необходимо:
1. **Wolfram Mathematica** с установленными `FeynArts`, `FeynCalc`, `LoopTools`.
2. **Python 3** для работы с PDF-функциями (`pdfcode/`).
3. **LHAPDF** для загрузки и работы с функциями плотности распределения частиц.

---

## **Контакты**
По всем вопросам обращайтесь:
- **Дмитрий Калашников**: [dskalashnikov@mephi.ru](mailto:dskalashnikov@mephi.ru)