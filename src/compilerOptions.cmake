###############################################################################
# Compiler options
###############################################################################
# FFLAGS depend on the compiler
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

if (Fortran_COMPILER_NAME STREQUAL "gfortran")
  # gfortran
  set (CMAKE_Fortran_FLAGS_RELEASE "-O2 -mtune=native -march=native -msse4.1")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-g -W -fbacktrace -O0")
elseif (Fortran_COMPILER_NAME STREQUAL "ifort")
  # ifort (untested)
  set (CMAKE_Fortran_FLAGS_RELEASE "-O2 -xHost")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-g -traceback -check uninit -check bound -check pointer -check output_conversion -fp-stack-check -fpe0 -stand f03 -O0")
elseif (Fortran_COMPILER_NAME STREQUAL "g77")
  # g77
  set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O3 -m32")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g -m32")
elseif (Fortran_COMPILER_NAME STREQUAL "xlf95")
  # xlf95
  set (CMAKE_Fortran_FLAGS_RELEASE "-O3 -qstrict -qhot -qarch=auto -qtune=auto -qcache=auto")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-g -C -O0")
elseif (Fortran_COMPILER_NAME STREQUAL "ftn")
  # ftn
  set (CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-g -O0")
else (Fortran_COMPILER_NAME STREQUAL "gfortran")
  message ("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
  message ("Fortran compiler: " ${Fortran_COMPILER_NAME})
  message ("No optimized Fortran compiler flags are known, we just try -O2...")
  set (CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g")
endif (Fortran_COMPILER_NAME STREQUAL "gfortran")

