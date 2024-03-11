#!/bin/bash
mkdir out/
rm out/*.*
for n in {0..250};
do
    v=$(printf "%05d" $n)
    val="out/vortices_$v.dat"
    touch $val
done
gfortran -o free_vortices2d free_vortices2d.f90 -fopenmp
./free_vortices2d
