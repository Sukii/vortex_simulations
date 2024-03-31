#!/bin/bash
mkdir out/
rm out/*.*
for n in {0..250};
do
    v=$(printf "%05d" $n)
    val="out/vortices_$v.dat"
    touch $val
done
gfortran -Ofast -O3 -o free_vortices2d free_vortices2d_mc.f90 -fopenmp
./free_vortices2d
