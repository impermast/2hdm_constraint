
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

### **1. `ZZZ_ZWW/` – Core calculations for vertex functions**
This directory contains essential scripts for computing amplitudes and interaction coefficients:
- `Vertex_graphs.wl` – Graph generation and analysis for vertex functions.
- `ZWW_full.wl` – Amplitude calculation and coefficient extraction for the \( ZWW \) process.
- `ZZZ_amp.wl` – Amplitude calculation and coefficient extraction for the \( ZZZ \) process.
- `SCRIPTqqZZZ.wls` – Executable script for computing the amplitude and matrix element squared for \( q\bar{q} \to Z \to ZZ \).
- `SCRIPTcross.wls` – Computes the **cross-section** and saves results in a buffer.
- `TESTqqZZZ.wl` – Test file for developing new computational methods.

Additionally, **backup calculation results** are stored here:
- `.mx` files – Serialized intermediate computations.
- `.txt` files – Stored numerical values of coefficients.

All buffer files, intermediate results, and additional plots are stored in:
- `ZZZ_ZWW/buffer/` – Temporary buffer files for storing intermediate data.
- `ZZZ_ZWW/graphs/` – Various graphical representations of calculations.
- `ZZZ_ZWW/subgraphs/` – Additional subgraphs related to the analysis.

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
- `ZWW_full.wl` – расчёт амплитуды и извлечение коэффициентов для процесса \( ZWW \).
- `ZZZ_amp.wl` – расчёт амплитуды и извлечение коэффициентов для процесса \( ZZZ \).
- `SCRIPTqqZZZ.wls` – исполняемый скрипт для вычисления амплитуды и квадрата матричного элемента процесса \( q\bar{q} \to Z \to ZZ \).
- `SCRIPTcross.wls` – вычисляет **сечение процесса** и сохраняет результаты в буфер.
- `TESTqqZZZ.wl` – тестовый файл для разработки новых вычислительных методов.

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