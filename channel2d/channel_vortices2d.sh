#!/bin/bash
mkdir outc/
rm ./channel_vortices2d
rm outc/*.*
for n in {0..500};
do
    v=$(printf "%05d" $n)
    val="outc/vortices_$v.dat"
    touch $val
done
gfortran -Ofast -O3 -o channel_vortices2d channel_vortices2d.f90 -fopenmp
time ./channel_vortices2d
