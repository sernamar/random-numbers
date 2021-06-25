# random-numbers

Generate random numbers from the standard normal distribution (mean 0 and standard deviation 1) using different programming languages.

## Benchmarks

### System info
```
Intel(R) Core(TM) i3-9100F CPU @ 3.60GHz (4 cores)
RAM memory: 8103MB
OS: Debian GNU/Linux 10 (buster)
Kernel: Linux 4.19.0-16-amd64 (x86_64)
gcc version: 8.3.0
sbcl version: 2.1.5
Python version: 3.7.3
```

### Simulations

#### 1 billion numbers, sequentially

##### C (gcc, default optimization level)

```shell
time ./normal_numbers 1000000000
Done.

real    1m19,716s
user    1m17,244s
sys     0m2,053s
```

##### C (gcc, O3 optimization level)

```shell
time ./normal_numbers_O3 1000000000
Done.

real    1m22,199s
user    1m17,696s
sys     0m1,920s
```

##### Common Lisp (SBCL)

```shell
time ./normal-numbers 1000000000
Heap exhausted during garbage collection: 0 bytes available, 16 requested.
Gen  Boxed   Code    Raw  LgBox LgCode  LgRaw  Pin       Alloc     Waste        Trig      WP GCs Mem-age
 1    5479      0   5468      0      0      0    3   358529280    182016   225815690   10947   1  1.3998
 2   10350      0  10191      0      0      0    9   672972992    114496     2000000   20541   0  0.4743
 3       0      0      0      0      0      0    0           0         0     2000000       0   0  0.0000
 4       0      0      0      0      0      0    0           0         0     2000000       0   0  0.0000
 5       0      0      0      0      0      0    0           0         0     2000000       0   0  0.0000
 6     828      5    333     94      0     20    0    40768320   1174720     2000000    1280   0  0.0000
           Total bytes allocated    =    1072270592
           Dynamic-space-size bytes =    1073741824
GC control variables:
   *GC-INHIBIT* = true
   *GC-PENDING* = true
   *STOP-FOR-GC-PENDING* = false
fatal error encountered in SBCL pid 14826 tid 14826:
Heap exhausted, game over.

Welcome to LDB, a low-level debugger for the Lisp runtime environment.
ldb> 
```

Unfortunately, it seems that the Common Lisp program cannot deal with a list of 1 billion numbers... An option would be to use an array created with the `static-arrays` library (using the native `make-array` function doesn't work either for so many elements). We'll see that aproach in the parallel version of this program.

#### 1 billion numbers, in parallel

##### C (gcc, default optimization level)

```shell
time ./p_normal_numbers 1000000000 4


real    0m29,002s
user    1m47,867s
sys     0m2,259s
```

##### C (gcc, O3 optimization level)

```shell
time ./p_normal_numbers_O3 1000000000 4

real    0m28,418s
user    1m44,276s
sys     0m2,962s
```

##### C (OpenMP)

```shell
time ./openmp_normal_numbers 1000000000

real    0m53,234s
user    3m22,833s
sys     0m2,309s
```

##### Common Lisp

```shell
time ./p-normal-numbers 1000000000 4

real    1m17,709s
user    1m15,465s
sys     0m1,278s
```

##### Python (numpy runs the `standard_normal` function in parallel)

```shell
time python3 normal_numbers.py 1000000000

real    0m19,954s
user    0m13,570s
sys     0m4,412s
```

## How to run the programs

For example, to generate 100 random numbers from the standard normal distribution, use:

### Sequential programs

- C (not optimized): `./normal_numbers 100`
- C (optimized): `./normal_numbers_O3 100`
- Common Lisp: `./normal-numbers 100`

### Parallel programs

- C (not optimized): `./p_normal_numbers 100 4`
- C (optimized): `./p_normal_numbers_O3 100 4`
- C (OpenMP): `./openmp_normal_numbers 100`
- Common Lisp: `./p-normal-numbers 100 4`
- Python: `python3 normal_numbers.py 100`

