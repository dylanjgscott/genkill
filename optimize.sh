#!/bin/sh
#COMP3109 Assignment 3
#Launcher for optimizer
#Group Members:
#   
#
#

numArgs="$#"

#If no args passed print manual
if [ $numArgs -eq 0 ]; then
    echo "\n"
    echo "**************************************"
    echo "Welcome to our COMP3109 Assignment 3" 
    echo "Intermediate Language Optimizer tool"
    echo "**************************************"
    echo "usage: ./optimize.sh [input-file] [output-file] -[optional-args]"
    echo "Available optional commands: "
    echo "    u   Remove unreachable code."
    echo "    d   Eliminate Dead code."
    echo "    l   Eliminate redundant loads"
    echo "Note: No optional arguments will result in all optimizations being run."
    echo "Example: "
    echo "\n"
    echo "    ./optimize.sh myFile outputFile -ud"
    echo "\n"
    echo "Optimizer will remove unreachable and dead code from \"myFile\" and output result to \"outputFile\""
    echo "\n"
else
    if [ $numArgs -lt 2 ] && [ $numArgs -gt 0 ]; then
        echo "Only one argument passed to optimizer. Please supply at least 2: input and output files, or none to see help message."
    else
        if [ $numArgs -ge 2 ] && [ $numArgs -lt 3 ]; then
            if [ -x optimizer ]; then
                #add some processing of command here. Only pass valid options to main file
                  ./optimizer $1 $3 > $2
            else
                echo "No optimizer executable found. Please run make to compile and consult the providecd README file."
            fi
        else
            echo "WHOA, too many arguments. Please consult the README file or run this script without arguments to see help message."
        fi
    fi
fi

