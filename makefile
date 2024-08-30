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
OPT = 0
## OpenMP? Empty: default with OpenMP; 0: No OpenMP; 1 with OpenMP
OMP = 0
## Lapack/blas/mkl? Empty: default with Lapack; 0: without Lapack; 1 with Lapack
LAPACK = 0
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
MAIN_path :=$(shell pwd)
#=================================================================================
# extansion for the library (.a), objects and modules directory
#=================================================================================
ext_obj:=_$(FFC)_opt$(OOPT)_omp$(OOMP)_lapack$(LLAPACK)_int$(INT)
#
#=================================================================================
# To deal with external compilers.mk file
CompilersDIR = $(MAIN_path)
ifeq ($(CompilersDIR),)
  include compilers.mk
else
  include $(CompilersDIR)/compilers.mk
endif
#=================================================================================
# Directories
#=================================================================================
OBJ_DIR    := OBJ/obj$(ext_obj)
$(shell [ -d $(OBJ_DIR) ] || mkdir -p $(OBJ_DIR))
MOD_DIR    := $(OBJ_DIR)

SRC_DIR=SRC
MAIN_DIR=APP
TESTS_DIR=Tests

#=================================================================================
# External Libraries directory
ifeq ($(ExtLibDIR),)
  ExtLibDIR := $(MAIN_path)/Ext_Lib
endif
$(shell [ -d $(ExtLibDIR) ] || (echo $(ExtLibDIR) "does not exist" ; exit 1))

QD_DIR            = $(ExtLibDIR)/QDUtilLib
QDMOD_DIR         = $(QD_DIR)/OBJ/obj$(ext_obj)
QDLIBA            = $(QD_DIR)/libQD$(ext_obj).a

AD_DIR    = $(ExtLibDIR)/AD_dnSVM
ADMOD_DIR = $(AD_DIR)/OBJ/obj$(ext_obj)
ADLIBA    = $(AD_DIR)/libAD_dnSVM$(ext_obj).a
#===============================================================================
EXTLib     = $(ADLIBA) $(QDLIBA)
EXTMod     = -I$(QDMOD_DIR) -I$(ADMOD_DIR)
#===============================================================================
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

SRCFILES=BasisInput_m.f90 Basis_SBG_m.f90 Basis_DP_m.f90 Basis_BoxAB_m.f90 Basis_HO_m.f90 Basis_base_m.f90 Basis_m.f90

OBJ0=${SRCFILES:.f90=.o}
OBJ=$(addprefix $(OBJ_DIR)/, $(OBJ0))


#
TEST_QDBaEXE   = Test_QDBa.x

EXA_QDBaEXE    = Exa_QDBa.x
LIBA           = libQDBa$(extlibwi_obj).a
$(info ***********EXA_QDBaEXE:       $(EXA_QDBaEXE))
$(info ***********LIBA:              $(LIBA))

#===============================================
#============= Main programs: tests + example ==
#
.PHONY: all
all: exe

# Example Exa_QDBa
.PHONY: exe
exe :$(EXA_QDBaEXE)
	@echo "Exa_QDBa compilation: OK"
$(EXA_QDBaEXE): $(OBJ_DIR)/Exa_QDBa.o $(LIBA)
	$(FFC) $(FFLAGS)   -o $(TEST_QDBaEXE) $(OBJ_DIR)/Exa_QDBa.o $(LIBA) $(EXTLib) $(FLIB)
	@echo "Exa_QDBa compilation: OK"
	./$(TEST_QDBaEXE) < dat_DP
	@echo "execution"
#
#===============================================
#============= compilation =====================
#===============================================
$(OBJ_DIR)/%.o: %.f90
	$(FFC) $(FFLAGS) -o $@ -c $<
#
#===============================================
#============= Library: libAD_dnSVM.a  =========
#===============================================
.PHONY: lib
lib : $(LIBA)
$(LIBA): $(OBJ)
	ar -cr $(LIBA) $(OBJ)
	@echo "  done Library: "$(LIBA)
#===============================================
#===============================================
#
#===============================================
#================ cleaning =====================
.PHONY: clean cleanall
clean:
	rm -f $(EXA_QDBaEXE) $(LIBA)
	rm -f comp.log QDBa.log  Test.log
	rm -fr *.dSYM
	rm -fr build
	cd $(OBJ_DIR) ; rm -f *.o *.mod *.MOD
	@cd Tests && ./clean
	@echo "  done cleaning"
cleanall: clean
	rm -f *.a res* Test_QDBa.x
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
.PHONY: getlib
getlib:
	cd $(ExtLibDIR) ; ./get_Lib.sh QDUtilLib
	cd $(ExtLibDIR) ; ./get_Lib.sh AD_dnSVM
#
$(QDLIBA):
	cd $(ExtLibDIR) ; ./get_Lib.sh QDUtilLib
	cd $(ExtLibDIR)/QDUtilLib ; make lib FC=$(FFC) OPT=$(OOPT) OMP=$(OOMP) LAPACK=$(LLAPACK) INT=$(INT) ExtLibDIR=$(ExtLibDIR) CompilersDIR=$(CompilersDIR)
	@test -f $(QDLIBA) || (echo $(QDLIBA) "does not exist" ; exit 1)
	@echo "  done " $(QDLIBA)
$(ADLIBA):
	cd $(ExtLibDIR) ; ./get_Lib.sh AD_dnSVM
	cd $(ExtLibDIR)/AD_dnSVM ; make lib FC=$(FFC) OPT=$(OOPT) OMP=$(OOMP) LAPACK=$(LLAPACK) INT=$(INT) ExtLibDIR=$(ExtLibDIR) CompilersDIR=$(CompilersDIR)
	@test -f $(ADLIBA) || (echo $(ADLIBA) "does not exist" ; exit 1)
	@echo "  done " $(ADLIBA)
#
#===============================================
#
#===============================================
### dependencies
#
$(OBJ_DIR)/Exa_QDBa.o:   $(LIBA)
$(LIBA).a:               $(OBJ_lib)
#
$(OBJ_DIR)/Basis_m.o:       $(OBJ_DIR)/Basis_SBG_m.o
$(OBJ_DIR)/Basis_SBG_m.o:   $(OBJ_DIR)/Basis_DP_m.o
$(OBJ_DIR)/Basis_DP_m.o:    $(OBJ_DIR)/Basis_HO_m.o $(OBJ_DIR)/Basis_BoxAB_m.o \
                            $(OBJ_DIR)/Basis_base_m.o $(OBJ_DIR)/BasisInput_m.o | $(EXTLib)
$(OBJ_DIR)/Basis_HO_m.o:    $(OBJ_DIR)/BasisInput_m.o $(OBJ_DIR)/Basis_base_m.o | $(EXTLib)
$(OBJ_DIR)/Basis_BoxAB_m.o: $(OBJ_DIR)/BasisInput_m.o $(OBJ_DIR)/Basis_base_m.o | $(EXTLib)
$(OBJ_DIR)/Basis_base_m.o:  $(OBJ_DIR)/BasisInput_m.o | $(EXTLib)
#
$(OBJ_DIR)/BasisInput_m.o:  | $(EXTLib)
#
#===============================================