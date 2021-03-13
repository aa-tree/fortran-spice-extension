# Introduction

This project adds some routines to exten JPL's SPICE (https://naif.jpl.nasa.gov/naif/).



## Dependencies

This project requires:

- gfortran [64bit] (Tested with 5.4.0 20160609)


# Installation

Note: These steps will be automated in upcoming version of the install script.


1. Download this repository. Let's say you store it in */path-to-repo/fortran-spice-extension/*

1. Download JPL's SPICELIB (FORTRAN) from here: [http://naif.jpl.nasa.gov/pub/naif/toolkit//FORTRAN/PC_Linux_gfortran_64bit/packages/toolkit.tar.Z](http://naif.jpl.nasa.gov/pub/naif/toolkit//FORTRAN/PC_Linux_gfortran_64bit/packages/toolkit.tar.Z)

    - Unzip the downloaded file. Let's say you get a folder like */path-to-SPICE-ZIP/toolkit*

    - Run 'makeall.csh'.

    - Copy the file */path-to-SPICE-ZIP/toolkit/lib/spicelib.a* to */path-to-repo/Ffortran-spice-extension/core/toolkit/lib/*


1. Download the following Binary kernels from JPL’s website. These are data files that store the ephemerides. Use FTP, to make this process simple.

    - [https://naif.jpl.nasa.gov/pub/naif/generic_kernels/spk/planets/de435.bsp](https://naif.jpl.nasa.gov/pub/naif/generic_kernels/spk/planets/de435.bsp)

    - All files in the folder https://naif.jpl.nasa.gov/pub/naif/generic_kernels/spk/satellites/

    - Skip the folder a_old_version.


1. Move all these downloaded files to */path-to-repo/fortran-spice-extension/core/eph/*


1. Download all the files in the folder: [https://naif.jpl.nasa.gov/pub/naif/generic_kernels/lsk/](https://naif.jpl.nasa.gov/pub/naif/generic_kernels/lsk/)

1. Move these to */path-to-repo/fortran-spice-extension/core/gkernels*

# Examples

A few example programs can be found in example folder.

# Documentation

Coming soon.
