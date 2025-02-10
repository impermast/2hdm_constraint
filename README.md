
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
- `TESTqqZZZ.wl` – Test file for developing new computational methods.

Additionally, **backup calculation results** are stored here:
- `.mx` files – Serialized intermediate computations.
- `.txt` files – Stored numerical values of coefficients.

---

### **2. `pdfcode/` – Working with Parton Distribution Functions (PDFs)**
This directory contains a set of **precomputed CSV files** with PDFs for various particles and energies.

Key files:
- `PDF_csv.py` – Python script for extracting specific PDF values.
- `pdf_mathematica_test.wl` – Example of how to call PDFs in Mathematica.

---

### **3. `ppHgg/` – Cross-section testing**
This directory focuses on the auxiliary process **pp → H → gg**, used to validate the cross-section computation method with PDFs.

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

Этот проект посвящен исследованию ограничений на параметры **2HDM (Двуххиггсовской модели)** с использованием **анизотропных вершинных функций**. Методология основана на анализе **коэффициентов аномальных взаимодействий** \( ZZZ \) и \( ZWW \), полученных из экспериментальных данных LHC.  

Расчеты выполняются в среде **Wolfram Mathematica** с использованием нескольких ключевых пакетов:  
- **FeynArts** – построение диаграмм Фейнмана.  
- **FeynCalc** – вычисление амплитуд процессов.  
- **LoopTools** – редукция интегралов Пасарино-Вельтмана.  
- **LHAPDF** – работа с функциями плотности распределения частиц (PDF).  

Для проверки работы метода также рассматривается вспомогательный процесс **pp → H → gg**, который тестирует вычисление сечения с учетом PDF.

---

## **Структура проекта**

### **1. `ZZZ_ZWW/` – Основные расчеты для вершинных функций**
Здесь находятся основные скрипты для вычисления амплитуд и коэффициентов взаимодействий:
- `Vertex_graphs.wl` – построение и анализ графиков вершинных функций.
- `ZWW_full.wl` – расчет амплитуды и коэффициента \( f_4^Z \) для процесса \( ZWW \).
- `ZZZ_amp.wl` – расчет амплитуды и коэффициента \( f_4^Z \) для процесса \( ZZZ \).
- `SCRIPTqqZZZ.wls` – исполняемый скрипт для вычисления амплитуды и квадрата матричного элемента процесса \( q\bar{q} \to Z \to ZZ \).
- `TESTqqZZZ.wl` – тестовый файл для отработки новых методов вычислений.

Дополнительно здесь хранятся **бэкапы вычислений**:
- `.mx` файлы – сериализованные промежуточные вычисления.
- `.txt` файлы – сохраненные численные значения коэффициентов.

---

### **2. `pdfcode/` – Работа с PDF-функциями**
Архив CSV-файлов с предвычисленными **функциями плотности распределения частиц (PDF)** для различных энергий и типов частиц.  

Ключевые файлы:
- `PDF_csv.py` – Python-скрипт для получения нужного PDF.  
- `pdf_mathematica_test.wl` – пример вызова PDF в коде Mathematica.  

---

### **3. `ppHgg/` – Тестирование расчета сечения**
Раздел, посвященный вспомогательному процессу **pp → H → gg**, который используется для тестирования расчета сечения процесса через PDF.

---

## **Запуск и установка**

Для работы с проектом необходимо:
1. **Wolfram Mathematica** с установленными `FeynArts`, `FeynCalc`, `LoopTools`.
2. **Python 3** для работы с PDF-функциями (`pdfcode/`).
3. **LHAPDF** для работы с PDF-файлами.

### **Запуск основного кода**
Выполнить расчет амплитуды и квадрата матричного элемента:
```sh
wolframscript -file ZZZ_ZWW/SCRIPTqqZZZ.wls 1
```


### **Контакты**
По всем вопросам можно обращаться:

Дмитрий Калашников: [dskalashnikov@mephi.ru](mailto:dskalashnikov@mephi.ru)