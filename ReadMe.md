Basis_OOP

**Basis_OOP** is a Fortran program to deal with basis sets denfined as a tree:

- Each branch can be of **DP** (direct product) type/class or **SBG** (Smolyak Sparse Basis/Grid).
- Each leaf are primitive basis set.

- The primitive basis sets are:
  - Harmonic Oscilator, **HO**
  - Particule-in-a-box, **BoxAB**
  - Others are in prevision (**Fourier** ...)


Right now, the library is doing only the reading and the writing.


date: 23/08/2024

  Copyright 2024 David Lauvergnat [1]


## 1) Data structure
The basis set tree is read from an input file (std input) and an example is given in the **dat_test** file.


## 2) Installation

### a) With fpm:

```bash
fpm build
```

### b) with a makefile:

```bash
make all
```

It will make the library, the executable tests and example.
You can change the compiler, Lapack flag, the OpenMP flag, the compiler optimization flag and the default integer either in the makefile or when calling make:

```bash
make all FC=ifort OMP=0 OPT=0 LAPACK=0 INT=4
  # FC=ifort to change the compiller to ifort
  # OMP=0/1 to turn off/on the OpenMP fortran flag.
  # OPT=0/1 to turn off/on the fortran optimization.
  # LAPACK=0/1 to turn off/on the lapack use
  # INT=4/8 to change the default integer
```

## 3) run the example

With **fpm**

```bash
    fpm run Exe --< dat_test
```

or in the Tests directory, run the scripts

```bash
    ./Test_QDBa.x < dat_test
```
