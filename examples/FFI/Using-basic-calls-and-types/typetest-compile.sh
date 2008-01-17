#!/bin/sh
cd $1
echo In directory: `pwd`
gcc -dynamiclib -Wall -o libtypetest.dylib typetest.c -install_name ./libtypetest.dylib