#=================================================================================
#=================================================================================
# Compiler?
#Possible values: (Empty: gfortran)
#                ifort (version: 19. linux)
#                gfortran (version: 9. linux and osx)
#                nagfor (version 7.0, osx)
 FC = gfortran
#FC = ifort
#FC = nagfor
#
# Optimize? Empty: default Optimization; 0: No Optimization; 1 Optimzation
OPT = 1
## OpenMP? Empty: default with OpenMP; 0: No OpenMP; 1 with OpenMP
OMP = 1
## Lapack/blas/mkl? Empty: default with Lapack; 0: without Lapack; 1 with Lapack
LAPACK = 1
## force the default integer (without kind) during the compillation.
## default 4: , INT=8 (for kind=8)
INT = 4
## how to get external libraries;  "loc" (default): from local zip file, Empty or something else, from github
EXTLIB_TYPE = loc
#=================================================================================
ifeq ($(FC),)
  FFC      := gfortran
else
  FFC      := $(FC)
endif
ifeq ($(OPT),)
  OOPT      := 1
else
  OOPT      := $(OPT)
endif
ifeq ($(OMP),)
  OOMP      := 1
else
  OOMP      := $(OMP)
endif
ifeq ($(LAPACK),)
  LLAPACK      := 1
else
  LLAPACK      := $(LAPACK)
endif
#=================================================================================
# Operating system, OS? automatic using uname:
#=================================================================================
OS:=$(shell uname)

#=================================================================================
# extansion for the library (.a), objects and modules directory
#=================================================================================
ext_obj=_$(FFC)_opt$(OOPT)_omp$(OOMP)_lapack$(LLAPACK)_int$(INT)

#=================================================================================
# Directories
#=================================================================================
MAIN_path :=$(shell pwd)

OBJ_DIR    := OBJ/obj$(ext_obj)
$(shell [ -d $(OBJ_DIR) ] || mkdir -p $(OBJ_DIR))
MOD_DIR    := $(OBJ_DIR)

SRC_DIR=SRC
MAIN_DIR=APP
TESTS_DIR=Tests

#=================================================================================
# External Libraries directory
ifeq ($(ExtLibDIR),)
  ExtLibDIR := Ext_Lib
endif

QD_DIR=$(ExtLibDIR)/QDUtilLib
QDMOD_DIR=$(QD_DIR)/OBJ/obj$(ext_obj)
QDLIBA=$(QD_DIR)/libQD$(ext_obj).a
#===============================================================================

#=================================================================================
#=================================================================================
# gfortran (osx and linux)
#=================================================================================
ifeq ($(FFC),gfortran)

  ifeq ($(OOPT),1)
    FFLAGS = -O5 -g -fbacktrace -funroll-loops -ftree-vectorize -falign-loops=16
  else
    FFLAGS = -Og -g -fbacktrace -fcheck=all -fwhole-file -fcheck=pointer -Wuninitialized -finit-real=nan -finit-integer=nan
    #FFLAGS = -O0 -fbounds-check -Wuninitialized
  endif

  # integer kind management
  ifeq ($(INT),8)
    FFLAGS += -fdefault-integer-8
  endif

  # where to store .mod files
  FFLAGS +=-J$(MOD_DIR)

  # omp management
  ifeq ($(OOMP),1)
    FFLAGS += -fopenmp
  endif

  # lapack management with cpreprocessing
  FFLAGS += -cpp -D__LAPACK="$(LLAPACK)"

  # where to look .mod files
  FFLAGS += -I$(QDMOD_DIR)


   FLIB   = $(QDLIBA)
  # OS management
  ifeq ($(LLAPACK),1)
    ifeq ($(OS),Darwin)    # OSX
      # OSX libs (included lapack+blas)
      FLIB += -framework Accelerate
    else                   # Linux
      # linux libs
      FLIB += -llapack -lblas
      #
      # linux libs with mkl and with openmp
      #FLIB = -L$(MKLROOT)/lib/intel64 -lmkl_intel_lp64 -lmkl_core -lmkl_gnu_thread
      # linux libs with mkl and without openmp
      #FLIB = -L$(MKLROOT)/lib/intel64 -lmkl_intel_lp64 -lmkl_core -lmkl_sequential
    endif
  endif


   FC_VER = $(shell $(FFC) --version | head -1 )

endif
#=================================================================================
#=================================================================================
$(info ***********************************************************************)
$(info ***********OS:           $(OS))
$(info ***********COMPILER:     $(FFC))
$(info ***********COMPILER_VER: $(FC_VER))
$(info ***********OPTIMIZATION: $(OOPT))
$(info ***********OpenMP:       $(OOMP))
$(info ***********INT:          $(INT))
$(info ***********FFLAGS:       $(FFLAGS))
$(info ***********FLIB:         $(FLIB))
$(info ***********ext_obj:      $(ext_obj))
$(info ***********QD_DIR:       $(QD_DIR))
$(info ***********************************************************************)


VPATH = $(MAIN_DIR):$(TESTS_DIR):$(SRC_DIR)

SRCFILES=BasisInput_m.f90 Basis_DP_m.f90 Basis_HO_m.f90 Basis_base_m.f90 Basis_m.f90

OBJ0=${SRCFILES:.f90=.o}
OBJ=$(addprefix $(OBJ_DIR)/, $(OBJ0))


#
TEST_QDBaEXE   = Test_QDBa.x

EXA_QDBaEXE    = Exa_QDBa.x
LIBQDBashort   = libQDBa
LIBQDBa        = libQDBa$(ext_obj)
#===============================================
#============= Main programs: tests + example ==
#
.PHONY: all
all: Exa_QDBa

# Example Exa_QDBa
.PHONY: exa Exa Exa_QDBa
exa Exa Exa_QDBa:$(EXA_QDBaEXE)
	echo "Exa_QDBa compilation: OK"
$(EXA_QDBaEXE): $(OBJ_DIR)/Exa_QDBa.o $(LIBQDBa).a
	$(FFC) $(FFLAGS)   -o $(TEST_QDBaEXE) $(OBJ_DIR)/Exa_QDBa.o $(LIBQDBa).a $(FLIB)
	echo "Exa_QDBa compilation: OK"
#
#===============================================
#================ unitary tests ================
.PHONY: ut UT
ut UT: $(TEST_dnPolyEXE) $(TEST_dnSEXE) $(TEST_dnVecEXE)
	@./$(TEST_dnSEXE) > Test.log
	@./$(TEST_dnPolyEXE) >> Test.log
	@./$(TEST_dnVecEXE) >> Test.log
	@grep -F TESTING Test.log| grep -F Number
	@echo "  done with the unitary tests"
#===============================================
#===============================================
#
#===============================================
#============= compilation dnSVM ===============
#===============================================
$(OBJ_DIR)/%.o: %.f90
	$(FFC) $(FFLAGS) -o $@ -c $<
#
#===============================================
#============= Library: libAD_dnSVM.a  =========
#===============================================
.PHONY: lib libba libBa
lib libba libBa: $(LIBQDBa).a
$(LIBQDBa).a: $(OBJ)
	ar -cr $(LIBQDBa).a $(OBJ)
	rm -f $(LIBQDBashort).a
	ln -s  $(LIBQDBa).a $(LIBQDBashort).a
	@echo "  done Library: "$(LIBQDBa).a
#===============================================
#===============================================
#
#===============================================
#================ cleaning =====================
.PHONY: clean cleanall
clean:
	rm -f $(EXA_QDBaEXE) $(LIBQDBa).a $(LIBQDBashort).a
	rm -f comp.log QDBa.log  Test.log
	rm -fr *.dSYM
	rm -fr build
	cd $(OBJ_DIR) ; rm -f *.o *.mod *.MOD
	@cd Tests && ./clean
	@echo "  done cleaning"
cleanall: clean
	rm -f *.a
	rm -rf OBJ
	cd $(MAIN_path)/Ext_Lib ; ./cleanlib
	@echo "  done remove *.a libraries and OBJ directory"
#===============================================
#================ zip and copy the directory ===
ExtLibSAVEDIR := /Users/lauvergn/git/Ext_Lib
BaseName := QDBa
.PHONY: zip
zip: cleanall
	test -d $(ExtLibSAVEDIR) || (echo $(ExtLibDIR) "does not exist" ; exit 1)
	$(ExtLibSAVEDIR)/makezip.sh $(BaseName)
	cd $(ExtLibSAVEDIR) ; ./cp_QDBa.sh
	@echo "  done zip"
#===============================================
#===============================================
#== external libraries
#
$(QDLIBA):
	@test -d $(ExtLibDIR) || (echo $(ExtLibDIR) "does not exist" ; exit 1)
	@test -d $(QD_DIR) || (cd $(ExtLibDIR) ; ./get_QDUtilLib.sh $(EXTLIB_TYPE))
	@test -d $(QD_DIR) || (echo $(QD_DIR) "does not exist" ; exit 1)
	cd $(QD_DIR) ; make lib FC=$(FFC) OPT=$(OOPT) OMP=$(OOMP) LAPACK=$(LLAPACK) INT=$(INT) ExtLibDIR=$(ExtLibDIR)
	@echo "  done " $(QDLIBA) " in "$(BaseName)
#
#===============================================
#
#===============================================
### dependencies
#
$(OBJ_DIR)/Exa_QDBa.o:      $(LIBQDBa).a
$(LIBQDBa).a:               $(OBJ_lib)
#
$(OBJ_DIR)/Basis_m.o:       $(OBJ_DIR)/Basis_DP_m.o
$(OBJ_DIR)/Basis_DP_m.o:    $(OBJ_DIR)/Basis_HO_m.o $(OBJ_DIR)/Basis_base_m.o $(OBJ_DIR)/BasisInput_m.o $(QDLIBA)
$(OBJ_DIR)/Basis_HO_m.o:    $(OBJ_DIR)/Basis_base_m.o $(QDLIBA)
$(OBJ_DIR)/Basis_base_m.o:  $(OBJ_DIR)/BasisInput_m.o $(QDLIBA)
#
$(OBJ_DIR)/BasisInput_m.o:  $(QDLIBA)
#
#===============================================




#=================================================================================
#=================================================================================
# ifort compillation v17 v18 with mkl
#=================================================================================
ifeq ($(FFC),ifort)

  # opt management
  ifeq ($(OOPT),1)
      #F90FLAGS = -O -parallel -g -traceback
      FFLAGS = -O  -g -traceback -heap-arrays
  else
      FFLAGS = -O0 -check all -g -traceback
  endif

# integer kind management
  ifeq ($(INT),8)
    FFLAGS += -i8
  endif

  # where to store the modules
  FFLAGS +=-module $(MOD_DIR)

  # omp management
  ifeq ($(OOMP),1)
    FFLAGS += -qopenmp
  endif

  # lapack management with cpreprocessing
  FFLAGS += -cpp -D__LAPACK="$(LLAPACK)"

  # where to look .mod files
  FFLAGS += -I$(QDMOD_DIR)

  FLIB    = $(QDLIBA)

  ifeq ($(LLAPACK),1)
    #FLIB += -mkl -lpthread
    FLIB += -qmkl -lpthread
    #FLIB +=  ${MKLROOT}/lib/libmkl_blas95_ilp64.a ${MKLROOT}/lib/libmkl_lapack95_ilp64.a ${MKLROOT}/lib/libmkl_intel_ilp64.a \
    #         ${MKLROOT}/lib/libmkl_intel_thread.a ${MKLROOT}/lib/libmkl_core.a -liomp5 -lpthread -lm -ldl
  else
    FLIB += -lpthread
  endif

  FC_VER = $(shell $(F90) --version | head -1 )

endif
#=================================================================================
#=================================================================================
#=================================================================================
#=================================================================================
#===============================================================================
# nag compillation (nagfor)
#===============================================================================
ifeq ($(FFC),nagfor)

  # opt management
  ifeq ($(OOPT),1)
      FFLAGS = -O4 -o -compatible -kind=byte -Ounroll=4 -s
  else
    ifeq ($(OOMP),0)
      ifeq ($(LLAPACK),0)
          FFLAGS = -O0 -g -gline -kind=byte -C -C=alias -C=intovf -C=undefined
      else
          FFLAGS = -O0 -g -gline -kind=byte -C -C=alias -C=intovf
      endif
    else
          FFLAGS = -O0 -g        -kind=byte -C -C=alias -C=intovf
    endif
  endif

  # integer kind management
  ifeq ($(INT),8)
    FFLAGS += -i8
  endif

 # where to store the .mod files
  FFLAGS +=-mdir $(MOD_DIR)

# where to look the .mod files
  FFLAGS +=-I $(MOD_DIR)

  # omp management
  ifeq ($(OOMP),1)
    FFLAGS += -openmp
  endif

  # lapack management with cpreprocessing
  FFLAGS += -fpp -D__LAPACK="$(LLAPACK)"

  # where to look .mod files
  FFLAGS += -I$(QDMOD_DIR)

  FLIB    = $(QDLIBA)

  # lapact management (default with openmp), with cpreprocessing
  ifeq ($(LLAPACK),1)
    ifeq ($(OS),Darwin)    # OSX
      # OSX libs (included lapack+blas)
      FLIB += -framework Accelerate
    else                   # Linux
      # linux libs
      FLIB += -llapack -lblas
    endif
  endif

  FC_VER = $(shell $(FFC) -V 3>&1 1>&2 2>&3 | head -1 )

endif