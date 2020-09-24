COMP ?= gnu

OMP_NUM_THREADS ?= 1
NTHREADS = $(OMP_NUM_THREADS)
NTHREADS ?= 1
# Used only for the "explicit" vec subroutines
VEC_LENGTH ?= 8
# change this to \Sum 2^n to run only specific tests
WHICH_TESTS ?= 2147483647
export NTHREADS VEC_LENGTH

ifeq ($(COMP),gnu)
 FC = gfortran
 CXX = g++
 LDFLAGS = -fopenmp
 FFLAGS_INFO = -fopt-info -ftree-vectorizer-verbose=2
 ## default ARCH_FLAGS are for Ryzen 2500U. For whatever reason, generic ~2x faster than znver1 or native
 FFLAGS_ARCH = -march=znver1 -mtune=generic
 FFLAGS_OPTIM = -O3 -fopenmp
 FFLAGS_AUTO_PARALLEL = -ftree-parallelize-loops=$(NTHREADS)
endif
ifeq ($(COMP),intel)
 FC = ifort
 CXX = icpc
 LDFLAGS = -qopenmp -lstdc++
 FFLAGS_INFO = -qopt-report=5
 FFLAGS_ARCH = -march=native -mtune=generic
 FFLAGS_OPTIM = -O3 -fopenmp
 FFLAGS_AUTO_PARALLEL = -parallel
endif
FFLAGS = $(FFLAGS_ARCH) $(FFLAGS_OPTIM)
ifeq ($(LISTING),TRUE)
 FFLAGS := $(FFLAGS) $(FFLAGS_INFO)
endif
ifeq ($(AUTOPAR),TRUE)
 FFLAGS := $(FFLAGS) $(FFLAGS_AUTO_PARALLEL)
endif
DFLAGS = -DSET_TO_NEGATIVE_ONE -DVEC_LENGTH=$(VEC_LENGTH) -DWHICH_TESTS32=$(WHICH_TESTS)
FFLAGS := $(FFLAGS) $(DFLAGS) $(EXTRA_FFLAGS)

LD = $(FC)

SOURCE_F90 = test_do_2.F90 kernel_f.F90
SOURCE_CXX = kernel_c.cpp

OBJ_F90 = $(SOURCE_F90:.F90=.o)
OBJ_CXX = $(SOURCE_CXX:.cpp=.o)

EXEC = test_do
.DEFAULT_GOAL = all
.PHONY: all clean
all: $(EXEC)

$(EXEC): $(OBJ_F90) $(OBJ_CXX)
	$(LD) -o $@ $(LDFLAGS) $^

%.o: %.F90
	$(FC) -c $(FFLAGS) -o $@ $^

%.o: %.cpp
	$(CXX) -c $(CXXFLAGS) $(SOURCE_CXX)

clean:
	-rm $(OBJ_F90) $(OBJ_CXX)
