# Benchmarking OpenMP and FORTRAN code

## `free_vortices2d.f90` on different CPU configurations

Running this benchmark code [modified OpenMP code](https://fortran-lang.discourse.group/uploads/short-url/oJ4C7T6AmsDWZh6p5SZyMiEOYdz.f90)

1. Intel(R) Core(TM) i7-6700 CPU @ 3.40GHz (PC)

```
Number of physical cores: 4
Number of logical cores: 8
Number of performance cores: 4

Total run in seconds:
1 thread 16.3750
2 threads 8.3750
4 threads 4.4375
8 threads 3.8750
```

2. 12th Gen Intel(R) Core(TM) i5-1235U (Laptop)
```
Number of physical cores: 10
Number of logical cores: 12
Number of performance cores: 10

Total run in seconds:
1 thread 50.7344
2 threads 25.9453
4 threads 28.3242
8 threads 29.4375
12 threads 27.9766

```

## Running [llama2.mojo(https://github.com/tairov/llama2.mojo) on different CPU configurations

```
mojo llama2.mojo tl-chat.bin -z tok_tl-chat.bin -n 256 -t 0 -s 100 -i "<|im_start|>user\nGive me a python function to generate Fibonacci sequence<|im_end|>\n<|im_start|>assistant\n"
```

1. Intel(R) Core(TM) i7-6700 CPU @ 3.40GHz (PC)

```
Number of physical cores: 4
Number of logical cores: 8
Number of performance cores: 4

Total run in seconds:

real	1m16.292s
user	2m23.454s
sys	0m10.896s

```

2. 12th Gen Intel(R) Core(TM) i5-1235U (Laptop)
```
Number of physical cores: 10
Number of logical cores: 12
Number of performance cores: 10

Total run in seconds:

real	0m50.704s
user	7m48.871s
sys	0m3.072s

```
