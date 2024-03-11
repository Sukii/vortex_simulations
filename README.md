# Vortex Simulations

We use FORTRAN for generating flow data and Lua-Love2D for displaying results as a graphical simulation. 

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

For boundaryless free flow:
```
love free2d/ free2d/out/
```

(sample video: ![free2d-vortices](https://vimeo.com/922070885?share=copy))

For 2D channel flow:
```
love channel2d/ channel2d/out/
```

You should here sound of water flowing!

(sample video: ![channel-flow](https://vimeo.com/922081441?share=copy))

If you are Windows user you should be able to modify the shell script to a batch script and run it on Windows. Lua-Love2d also should run fine on Windows!
Please do tell me if you have difficulties running on Windows!