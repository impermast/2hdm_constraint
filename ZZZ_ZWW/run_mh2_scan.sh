#!/bin/bash
echo -e "\033[1;32m"
echo "==============================================="
echo "     ███████╗ ██████╗ ██████╗  ██████╗        "
echo "     ██╔════╝██╔═══██╗██╔══██╗██╔═══██╗       "
echo "     █████╗  ██║   ██║██████╔╝██║   ██║       "
echo "     ██╔══╝  ██║   ██║██╔══██╗██║   ██║       "
echo "     ██║     ╚██████╔╝██║  ██║╚██████╔╝       "
echo "     ╚═╝      ╚═════╝ ╚═╝  ╚═╝ ╚═════╝        "
echo "   ZZZ One-loop Cross-section Calculator      "
echo "==============================================="
echo -e "\033[0m"


for mh2 in $(seq 100 20 1000); do
    echo "Calculating for mh2 = $mh2 GeV..."
    wolframscript -file SCRIPT_f4meanvalue.wls 0 $mh2 False
done

echo "Calculation completed. Results saved in csFile"
