#!/bin/bash

DIRS="bin"

files=$(find $DIRS -type f \( -name "*.c" -o -name "*.h" \))

clang-format --Werror --verbose -i $files 

echo "Files formatted successfully."