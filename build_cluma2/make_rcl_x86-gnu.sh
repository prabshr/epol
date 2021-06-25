#!/bin/bash


if [ ${1}X = "-hX" ]; then
    echo "
This script calls the makefile for build_rcl. You can pass options for make, e.g.,

     $0 clean
     $0 -j8

"
    exit
fi

if [ $# -gt 0 ]; then
    options="$*"
fi

hier=$(pwd)

#CLUMA2 SPECIFIC
#export PATH=
export ECCODES_DEFINITION_PATH=/daten/daten01/z4/local/definitions.edzw-2.20.0-1:/usr/local/gcc94_suite/share/eccodes/definitions
export LD_LIBRARY_PATH=/usr/local/gcc94_suite/lib:/usr/local/gcc94_suite/lib64

make -f Makefile.rcl.x86-gnu.MPI.makedepf90 $options

err=$?

if [ $err -ne 0 ]; then
    echo "
    Error during rcl mpinfort (gfortran) build!
"
    exit $err
else
    echo "
    ACTION WAS SUCCESSFUL!
"
fi
