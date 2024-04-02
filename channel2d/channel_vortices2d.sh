#!/bin/bash
n=$1
mkdir outc/
rm ./channel_vortices2d
if [ $n == "0" ]; then
    rm outc/*.*
fi
for k in {0..500};
do
    v=$(printf "%05d" $k)
    val="outc/vortices_$v.dat"
    touch $val
done
gfortran -Ofast -O3 -o channel_vortices2d channel_vortices2d.f90 -fopenmp
time ./channel_vortices2d $n
