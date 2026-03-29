#!/bin/bash
# Активация окружения для 2hdm_constraint
source /mnt/d/progs/2hdm_constraint/setup/venv/bin/activate
export LD_LIBRARY_PATH=/mnt/d/progs/2hdm_constraint/setup/lhapdf_install/lib:$LD_LIBRARY_PATH
export LHAPDF_DATA_PATH=/mnt/d/progs/2hdm_constraint/setup/lhapdf_install/share/LHAPDF
echo "Окружение активировано. Python: $(which python3)"
export PATH=/mnt/d/progs/2hdm_constraint/setup/lhapdf_install/bin:$PATH
