#include <iostream>
#include <iomanip> 
#include <fstream>

#include "CPPProcess.h"
#include "rambo.h"


double result(double energy){
  // Create a process object
  CPPProcess process;

  // Read param_card and set parameters
  process.initProc("../../Cards/param_card.dat");
  double weight;

  // Get phase space point
  vector<double*> p = get_momenta(process.ninitial, energy, 
				 process.getMasses(), weight);
  // Set momenta for this event
  process.setMomenta(p);

  // Evaluate matrix element
  process.sigmaKin();

  const double* matrix_elements = process.getMatrixElements();
//   cout << "Momenta:" << endl;
//   for(int i=0;i < process.nexternal; i++)
//     cout << setw(4) << i+1 
// 	 << setiosflags(ios::scientific) << setw(14) << p[i][0]
// 	 << setiosflags(ios::scientific) << setw(14) << p[i][1]
// 	 << setiosflags(ios::scientific) << setw(14) << p[i][2]
// 	 << setiosflags(ios::scientific) << setw(14) << p[i][3] << endl;
//   cout << " -----------------------------------------------------------------------------" << endl;

    return matrix_elements[0];
// Display matrix elements

//   return 0;
}

int main(int argc, char** argv){
    std::vector<double> energies;
    std::vector<double> results;
    double minEnergy = 10;
    double maxEnergy = 13000;
    int numPoints = 100;
    // Проверяем, были ли переданы аргументы командной строки
    if (argc >= 2) {
        // Если переданы, считываем значения из аргументов
        numPoints = std::atoi(argv[1]); // Преобразуем строку в целое число
        } else {
        // Если аргументы не переданы, сообщаем об ошибке
        std::cout << "Стандартное значение numPoints = 100 использовано.\n";
    }


    // Вычисляем логарифмический шаг между точками
    double logStep = std::log10(maxEnergy / minEnergy) / (numPoints - 1);
    // Заполняем векторы значениями энергии и результатов
    for (int i = 0; i < numPoints; ++i) {
        double en = minEnergy * std::pow(10, i * logStep);
        double res = result(en);
        energies.push_back(en);
        results.push_back(res);
    }
    

    // Открываем файл для записи таблицы
    std::ofstream outputFile("qqh.txt");

    // Проверяем, удалось ли открыть файл
    if (outputFile.is_open()) {
        // Записываем значения энергии и результатов в файл
        for (int i = 0; i < numPoints; ++i) {
            outputFile << energies[i] << " " << results[i] << std::endl;
        }

        // Закрываем файл
        outputFile.close();

        std::cout << "Таблица с результатами записана в файл results_table.txt" << std::endl;
    } else {
        // Если не удалось открыть файл, выводим сообщение об ошибке
        std::cerr << "Ошибка при открытии файла для записи" << std::endl;
        return 1; // Возвращаем ненулевой код ошибки
    }
    return 0;
}
