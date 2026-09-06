!===============================================================================
!===============================================================================
!This file is part of QDBa library.
!
!===============================================================================
! MIT License
!
! Copyright (c) 2022 David Lauvergnat
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!===============================================================================
!===============================================================================
MODULE Basis_m
  USE Basis_base_m
  USE Basis_HO_m
  USE Basis_BoxAB_m
  USE Basis_DP_m
  USE Basis_SBG_m
  IMPLICIT NONE

  PUBLIC
  PRIVATE :: make_tab_layer

CONTAINS
  RECURSIVE SUBROUTINE ReadInit_Basis(basis,layer,LG_in)
    USE QDUtil_m
    USE BasisInput_m
    USE Basis_base_m
    USE Basis_HO_m
    USE Basis_BoxAB_m
    CLASS (Basis_t), intent(inout), allocatable :: basis
    integer,         intent(in)                 :: layer
    integer,         intent(in),    optional    :: LG_in

    TYPE (BasisInput_t) :: BasisIn
    integer :: ib

    !-----------------------------------------------------------------------
    !logical, parameter :: debug=.TRUE.
    logical, parameter :: debug=.FALSE.
    character(len=*), parameter :: name_sub='ReadInit_Basis@Basis_m'
    !-----------------------------------------------------------------------
    IF (debug) THEN
      write(out_unit,*) 'BEGINNING ',name_sub
      write(out_unit,*) 'layer ',layer
      IF (present(LG_in)) THEN
        write(out_unit,*) 'LG_in ',LG_in
      ELSE
        write(out_unit,*) 'LG_in: not present'
      END IF
      flush(out_unit)
      IF (allocated(basis)) THEN 
        CALL basis%write()
      ELSE
        write(out_unit,*) 'basis: not allocated'
      END IF
      flush(out_unit)
    END IF
    !-----------------------------------------------------------------------
    IF (present(LG_in)) THEN
      CALL BasisIn%Read(LG_in)
    ELSE
      CALL BasisIn%Read()
    END IF
    IF (print_level > 0 .OR. debug) write(out_unit,*) 'basis namelist read: done ',BasisIn%name


    SELECT CASE (BasisIn%name)
    CASE ('ho')
      !write(out_unit,*) 'HO basis'
      ! its means only one primitive basis
      allocate(Basis_HO_t :: basis)
      basis = init_Basis_HO(BasisIn)
      CALL basis%Set_tab_n_OF_l(BasisIn%LG_in)

    CASE ('boxab')
      !write(out_unit,*) 'BoxAB basis'
      ! its means only one primitive basis
      allocate(Basis_BoxAB_t :: basis)
      basis = init_Basis_BoxAB(BasisIn)
      CALL basis%Set_tab_n_OF_l(BasisIn%LG_in)

    CASE ('dp')
      !write(out_unit,*) 'DP basis'
      allocate(Basis_DP_t :: basis)
      basis = init_Basis_DP(BasisIn)
    CASE ('sbg','sgb')
      !write(out_unit,*) 'SBG basis'
      allocate(Basis_SBG_t :: basis)
      basis = init_Basis_SBG(BasisIn)
    CASE default
      write(out_unit,*) 'ERROR in ',name_sub
      write(out_unit,*) ' basis name is incorrect, BasisIn%name: ',BasisIn%name
      write(out_unit,*) ' Possible values: BoxAB, HO, DP, (SBG or SGB)'

      STOP 'ERROR in ReadInit_Basis@Basis_m: basis name is incorrect'
    END SELECT
    IF (print_level > 0 .OR. debug) write(out_unit,*) 'OOP basis init: done ',basis%name
    IF (debug) CALL basis%write()

    basis%layer     = layer + 1
    basis%tab_layer = make_tab_layer(layer)

    IF (basis%primitive) THEN
      CALL basis%build()
    END IF
    IF (print_level > 0 .OR. debug) write(out_unit,*) 'OOP basis build: done ',basis%name
    IF (debug) CALL basis%write()
    
    SELECT TYPE (basis)
    TYPE IS(Basis_DP_t)
      DO ib=1,BasisIn%nb_basis
        CALL ReadInit_Basis(basis%tab_Pbasis(ib)%PBasis,basis%layer)
      END DO
      CALL basis%Set_ndim()
      CALL basis%Set_tab_n_OF_l(-1)
      
    TYPE IS(Basis_SBG_t)
      DO ib=1,BasisIn%nb_basis
        CALL ReadInit_Basis(basis%tab_Pbasis(ib)%PBasis,basis%layer,LG_in=basis%LG)
      END DO
      CALL basis%Set_ndim()
      CALL basis%Set_tab_n_OF_l(-1)
    END SELECT
    IF (print_level > 0 .OR. debug) write(out_unit,*) 'OOP basis recursive (DP SBG): done ',basis%name
    IF (debug) CALL basis%write()

    CALL BasisIn%dealloc()
    !-----------------------------------------------------------------------
    IF (debug) THEN
      IF (allocated(basis)) THEN 
        CALL basis%write()
      ELSE
        write(out_unit,*) 'basis is not allocated'
      END IF
      write(out_unit,*) 'END ',name_sub
      flush(out_unit)
    END IF
    !-----------------------------------------------------------------------
  END SUBROUTINE ReadInit_Basis

  FUNCTION make_tab_layer(layer) RESULT(tab_layer)
  USE QDUtil_m
  character (len=:), allocatable :: tab_layer

  integer,         intent(in)                 :: layer

  integer :: i

  tab_layer = ''
  DO i=1,layer*4
    tab_layer = tab_layer // ' '
  END DO

END FUNCTION make_tab_layer
END MODULE Basis_m