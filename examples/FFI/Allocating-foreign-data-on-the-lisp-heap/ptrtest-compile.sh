#!/bin/sh
cd $1
echo In directory: `pwd`
gcc -dynamiclib -Wall -o libptrtest.dylib ptrtest.c -install_name ./libptrtest.dylib