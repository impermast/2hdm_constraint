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

# Function to calculate cross-section for a given mh2 value
function calculate_cross_section() {
    local mh2=$1
    echo "Calculating for mh2 = $mh2 GeV..."
    wolframscript -file SCRIPT_f4meanvalue.wls 0 $mh2 False
    if [ $? -ne 0 ]; then
        echo "Error calculating for mh2 = $mh2 GeV"
        exit 1
    fi
}

# Main script execution
function main() {
    display_banner
    for mh2 in $(seq 100 20 1000); do
        calculate_cross_section $mh2
    done
    echo "Calculation completed. Results saved in csFile"
}

# Execute the main function
main