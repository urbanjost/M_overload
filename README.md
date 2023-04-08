You never change something by fighting the existing reality. To
change something, build a new model that makes the existing model
obsolete — Buckminster Fuller 

## Name
   M_overload(3f) - a module of intrinsic and operator overloads
   (LICENSE:MIT)
## Synopsis
```text
       use M_overload
```
## Description

   A WIP (Work In Progress)

   Common examples of intrinsic and operator overloads available
   as an fpm(1) package.

## Building the Module using make(1) ![gmake](docs/images/gnu.gif)
     git clone https://github.com/urbanjost/M_overload.git
     cd M_overload/src
     # change Makefile if not using one of the listed compilers
     
     # for gfortran
     make clean
     make F90=gfortran gfortran
     
     # for ifort
     make clean
     make F90=ifort ifort

     # for nvfortran
     make clean
     make F90=nvfortran nvfortran

This will compile the Fortran module and basic example
program that exercise the routine.

## Build and Test with FPM ![-](docs/images/fpm_logo.gif)

   Alternatively, download the github repository and build it with
   fpm ( as described at [Fortran Package Manager](https://github.com/fortran-lang/fpm) )

   ```bash
        git clone https://github.com/urbanjost/M_overload.git
        cd M_overload
        fpm run "*"
        fpm run --example "*"
        fpm test
   ```

   or just list it as a dependency in your fpm.toml project file.

```toml
        [dependencies]
        M_overload        = { git = "https://github.com/urbanjost/M_overload.git" }
```

## Documentation   ![docs](docs/images/docs.gif)

### User
![man-pages](docs/images/manpages.gif)
   - There are man-pages in the repository download in the docs/ directory
     that may be installed on ULS (Unix-Like Systems).

   + [manpages.zip](https://urbanjost.github.io/M_overload/manpages.zip)
   + [manpages.tgz](https://urbanjost.github.io/M_overload/manpages.tgz)

   - a simple index to the man-pages in HTML form for the
   [routines](https://urbanjost.github.io/M_overload/man3.html) 
   and [programs](https://urbanjost.github.io/M_overload/man1.html) 

   - A single page that uses javascript to combine all the HTML
     descriptions of the man-pages is at 
     [BOOK_M_overload](https://urbanjost.github.io/M_overload/BOOK_M_overload.html).

   - [CHANGELOG](docs/CHANGELOG.md) provides a history of significant changes

### Developer
   - [ford(1) output](https://urbanjost.github.io/M_overload/fpm-ford/index.html).
<!--
   - [doxygen(1) output](https://urbanjost.github.io/M_overload/doxygen_out/html/index.html).
-->
   - [github action status](docs/STATUS.md) 
---
