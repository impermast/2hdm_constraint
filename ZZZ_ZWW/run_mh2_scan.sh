#!/bin/bash



# Function to display the banner
function display_banner() {
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
}

# Function to calculate mean f4 for a given mh2 value
function calculate_f4mean() {
    local mh2=$1
    echo "Calculating for mh2 = $mh2 GeV..."
    wolframscript -file SCRIPT_f4meanvalue.wls 0 $mh2 False
    if [ $? -ne 0 ]; then
        echo "Error calculating for mh2 = $mh2 GeV"
        exit 1
    fi
}

# Function to calculate crosssection for a given mh2 value
function calculate_crosssection() {
    local mh2=$1
    echo "Calculating for mh2 = $mh2 GeV..."
    wolframscript -file SCRIPTcross.wls 0 $mh2 False
    if [ $? -ne 0 ]; then
        echo "Error calculating for mh2 = $mh2 GeV"
        exit 1
    fi
}



# Main script execution
function main() {
    display_banner
    for mh2 in $(seq 126 2 150); do
        calculate_crosssection $mh2
        calculate_f4mean $mh2
    done
    for mh2 in $(seq 160 10 200); do
        calculate_crosssection $mh2
        calculate_f4mean $mh2
    done
    for mh2 in $(seq 210 10 300); do
        calculate_crosssection $mh2
        calculate_f4mean $mh2
    done
    echo "Calculation completed. Results saved in csFile"
}

# Execute the main function
main
