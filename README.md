fSAX
====

Fortran implementation of the Simple API for XML (SAX)

Description
-----------
fSAX is a library to read XML files. It is fully developed in Fortran and the
API follows the approach of the Simple API for XML (SAX).
SAX is recommended for very large XML files, since it avoids the
extra memory usage of other approaches like DOM.
The purpose of fSAX is to provide a library fully written in Fortran to avoid
cross compilation with other languages like C, prioritizing I/O speed.
Although fSAX does not provide full compatibility with SAX, particularly with
SAX2, it is a lot faster than other libraries.

Installation
------------
In order to install fSAX you need to have CMake installed.
Use install.sh for a basic installation, which will generate the module (mod)
files and libfsax.a to compile and link with your application.
One may also want to generate the documentation and testing. In such case a
manual installation is required.

Basic installation:

Example 1:  
$ ./install.sh FC=gfortran TARGET_PREFIX=/usr  
will install fSAX using gfortran. The mod files will be under /usr/include and
libfsax.a will be under /usr/lib.  
Example 2:  
$ ./install.sh  
will install fSAX using the default Fortran compiler (gfortran). The mod files
will be under ./fsax/include and libfsax.a will be under ./fsax/lib.

Manual installation:

Follow the following steps as it's usually done using CMake:  
1) Create a build directory and go there, e.g.:  
$ mkdir fsax  
$ cd fsax  
2) Run cmake pointing to fSAX's src directory and choosing the desired
compiler, e.g.:  
$ cmake ../src -DCMAKE_Fortran_COMPILER=gfortran  
3) Configure your installation as desired, e.g.:  
$ ccmake ../src  
Here you'll be able to enable the building of tests and documentation, among other options.  
4) Once the configuration is finished, you are ready to compile:  
$ make install

