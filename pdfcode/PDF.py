
import numpy as np
import matplotlib.pyplot as plt
import lhapdf
import csv

import os
import os
import csv

def pdf_csv(id, q, output_folder="/root/generated_pdf"):
    # Создаем папку, если ее нет
    global df_set
    if not os.path.exists(output_folder):
        os.makedirs(output_folder)
    pdf = lhapdf.mkPDF(df_set, 0)
    x_values = np.logspace(-6, 0, 1000)  # Диапазон от очень малых значений x до 1
    pdf_values = [pdf.xfxQ(id, x, q) for x in x_values]  # 21 - код глюона в LHAPDF 23 Z 
    
    # Создаем название файла без указания пути
    filename = f"pdf_{id}_{q}.csv"
    
    # Создаем полный путь к файлу
    filepath = os.path.join(output_folder, filename)
    
    # Запись данных в .csv файл
    with open(filepath, 'w', newline='') as csvfile:
        writer = csv.writer(csvfile)
        writer.writerow(['x', 'PDF'])  # Записываем заголовок
        for x, pdf_val in zip(x_values, pdf_values):
            writer.writerow([x, pdf_val])

    return filepath

def get_pdf_table(id_list, x_values, q):
    global df_set
    pdf = lhapdf.mkPDF(df_set, 0)

    pdf_table = [['x'] + [f'PDF_{id}' for id in id_list]] 
    for x in x_values:
        row = [x] + [pdf.xfxQ(id, x, q) for id in id_list]  # Значения PDF для текущего x и всех id
        pdf_table.append(row)
    return pdf_table

def write_pdf_table_to_csv(filename, pdf_table):
    with open(filename, 'w', newline='') as csvfile:
        writer = csv.writer(csvfile)
        # Записываем заголовок таблицы
        writer.writerow(pdf_table[0])
        # Записываем данные таблицы, начиная со второй строки
        for row in pdf_table[1:]:
            writer.writerow(row)


def testgraph():
    # Загрузка нужного набора данных PDF (например, NNPDF)
    global df_set

    # Инициализация набора данных PDF
    pdf = lhapdf.mkPDF(df_set, 0)

    # Определение характеристик процесса (например, масса Z-бозона и фактор Q^2)
    mz = 91.1876  # масса Z-бозона в GeV
    mh = 125  # масса Z-бозона в GeV
    q=mh
    # Определение диапазона значений x для построения графика
    x_values = np.logspace(-5, -0.2, 1000)  # Диапазон от очень малых значений x до 1

    # Получение значений PDF для заданного диапазона значений x и фактора Q^2
    pdf_values = [pdf.xfxQ(5, x, q) for x in x_values] 
    pdf_values2= [pdf.xfxQ(5, x, 100*q) for x in x_values]
    diff = [abs(pdf_values[i]-pdf_values2[i]) for i in range(len(pdf_values2))]
    # Построение графика
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(12, 6))

    # Построение графиков
    ax1.plot(x_values, pdf_values, 'b.-', label='PDF 1')  # Синяя линия с точками
    ax1.plot(x_values, pdf_values2, 'r-', label='PDF 2')  # Красная линия
    ax1.set_xscale('log')  # Установка логарифмической шкалы для оси X
    ax1.set_yscale('log')  # Установка логарифмической шкалы для оси Y
    ax1.grid(True, which='both', linestyle='--', linewidth=0.7)  # Сетка для обеих шкал
    ax1.legend()  # Легенда для первого графика

    ax2.plot(x_values, diff, 'g-', label='difference')  # Зеленая линия
    ax2.set_xscale('log')  # Установка логарифмической шкалы для оси X
    ax2.set_yscale('log')  # Установка логарифмической шкалы для оси Y
    ax2.grid(True, which='both', linestyle='--', linewidth=0.7)  # Сетка для обеих шкал
    ax2.legend()  # Легенда для второго графика

    # Настройка макета
    plt.tight_layout()

    plt.show()

    plt.close()  # Закрытие окна графика


df_set = "NNPDF30_nnlo_as_0118"
q = 91
id_list = [-5,-4,-3,-2,-1,1,2,3,4,5]
x_values = np.logspace(-6, 0, 1000)  # Диапазон от очень малых значений x до 1

# print("\n\n q = ", q," PDF = ", df_set,"\n\n")

# pdf_table = get_pdf_table(id_list,x_values,q)
# filename = f"/root/output/output_{q}.csv"
# write_pdf_table_to_csv(filename, pdf_table)
testgraph()