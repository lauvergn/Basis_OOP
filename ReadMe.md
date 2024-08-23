Basis_OOP

**Basis_OOP** is a Fortran program to deal with basis sets denfined as a tree:

- Each branch can be of **DP** (direct product) type/class or **SBG** (Smolyak Sparse Basis/Grid).
- Each leaf are primitive basis set.

- The primitive basis sets are:
  - Harmonic Oscilator, **HO**
  - Others are in prevision (sine or **BoxAB**, **Fourier** ...)


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

The library, **libAD_dnSVM_XXX_oppY_ompZ_lapackW_intV.a** is created in the main directory and the **libAD_dnSVM.a** library is linked to it.
Remarks : 
- XXX is the compiller (gfortran, ifort ...)
- Y is 0 or 1 (opt0 / opt1: compiler optimization)
- Z is 0 or 1 (omp0 / omp1: whitout/with OpenMP)
- W is 0 or 1 (lapack0 / lapack1: whitout/with lapack)
- V is 4 or 8 (int4 / int8)

If needed, the .mod files are in the **OBJ/obj_XXX_oppY_ompZ_lapackW_intV** directory.

## 3) run the example

With **fpm**

```bash
    fpm run Exe --< dat_test
```

or in the Tests directory, run the scripts

```bash
    ./Test_QDBa.x < dat_test
```
