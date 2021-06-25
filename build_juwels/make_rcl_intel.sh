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

#CPS module purge
#CPS module load apps x86
#CPS module load intel/2020 netcdf4/4.7.3-x86-intel hdf5/1.10.5-x86-intel aec/1.0.3-x86-intel eccodes/2.19.0-x86-intel aocl/2.1-x86-gnu
#CPS module switch x86 sx
#CPS module load mpi/2.9.0

#CPS export NMPI_FC_H=ifort
module use $OTHERSTAGES
ml Stages/Devel-2020
ml Intel/2020.2.254-GCC-9.3.0
ml ecCodes/2.21.0

make -f Makefile.rcl.intel.MPI.makedepf90 $options

err=$?

if [ $err -ne 0 ]; then
    echo "
Error during rcl mpiifort build!
"
    exit $err
else
    echo "
    ACTION WAS SUCCESSFUL!
"
fi
