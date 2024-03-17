# Vortex Simulations

This work was part of my Ph.D. thesis work in Indian Statistical Institute, Calcutta (Kolkatta). At that time it ran on [VAX](https://en.wikipedia.org/wiki/VAX) systems with 512MB RAM! [Dr.U.N.Sinha](https://www.facebook.com/DrUnSinha/), developed his `FloSolver` in 1986, a powerful super computer for such flow simulations. Now I am able to run this on my laptop with Intel 7 and 12 CPU cores using OpenMP support!

We use FORTRAN for generating flow data and Lua-Love2D for displaying results as a graphical simulation. Rewriting the FORTRAN code in [Mojo](https://github.com/modularml/mojo) to see how fast it runs on my laptop. Also hoping to do some Deep Learning simulations of vorticity flows with some application to ocean and atmospheric flows.

## Running FORTRAN code (Linux UBUNTU users):

For boundaryless free flow:

```
cd free2d/
./free_vortices2d.sh
0
```

Please supply '0' as input as it waits to receive it.
If you have alreday run till 25 iterations then you can restart from 25.
The output is written to `free2d/out/` folder.


For 2D channel flow:

```
cd channel2d/
./channel_vortices2d.sh
0
```

Please supply '0' as input as it waits to receive it.
If you have alreday run till 25 iterations then you can restart from 25.
The output is written to `channel2d/outc/` folder.


## Display simulation using **Lua** code

Install [Lua](https://lua.org) and [Love2d](https://love2d.org/)

For boundaryless free flow:
```
love free2d/ free2d/out/
```

Sample 1: [free vortices video](https://vimeo.com/922070885)


For 2D channel flow:
```
love channel2d/ channel2d/out/
```

You should here sound of water flowing!

Sample 2D channel flow video:

Sample 2: [2D channel flow video](https://vimeo.com/922081441)


If you are Windows user you should be able to modify the shell script to a batch script and run it on Windows. Lua-Love2d also should run fine on Windows!
Please do tell me if you have difficulties running on Windows!