#
# For GNU make
#
ROOTDIR := $(dir $(lastword $(MAKEFILE_LIST)))

#
# preset
#   F_COMPILER = {gnu|intel}
#   MODE       = {debug|release}
#
F_COMPILER = gnu
MODE = debug

#
# External libraries (e.g., BLAS and LAPACK) should be installed
# in the directory $(LIBDIR). The library should be static (.a).
# All the internal library files will be in src/.
#
LIBDIR := $(ROOTDIR)lib
SRCDIR := $(ROOTDIR)src

#
# Fortran compiler
#
ifeq ($(F_COMPILER),intel)
  FC = ifort
  FCFLAGS = -O3 -fpp -qopenmp -I$(SRCDIR) -I$(LIBDIR)
  LDFLAGS = -I$(SRCDIR) -I$(LIBDIR) -L$(SRCDIR) -L$(LIBDIR) -lfsparse -lamd
else
  FC = gfortran
  FCFLAGS = -O3 -cpp -fopenmp -I$(SRCDIR) -I$(LIBDIR)
  LDFLAGS = -I$(SRCDIR) -I$(LIBDIR) -L$(SRCDIR) -L$(LIBDIR) -lfsparse -lamd
endif

#
# options
#
ifdef USE_MTMETIS
  FCFLAGS += -D USE_MTMETIS
  LDFLAGS += -lmtmetis -lwildriver
endif
ifdef USE_METIS
  FCFLAGS += -D USE_METIS
  LDFLAGS += -lmetis
endif

#
# LAPACK and BLAS
#
ifeq ($(F_COMPILER),intel)
  LDFLAG += -mkl
else
  LDFLAG += -llapak -lrefblas
endif
