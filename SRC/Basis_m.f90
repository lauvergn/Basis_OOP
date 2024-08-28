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
  RECURSIVE SUBROUTINE Read_Basis(basis,layer)
    USE QDUtil_m
    USE BasisInput_m
    USE Basis_base_m
    USE Basis_HO_m
    USE Basis_BoxAB_m
    CLASS (Basis_t), intent(inout), allocatable :: basis
    integer,         intent(in)                 :: layer

    TYPE (BasisInput_t) :: BasisIn
    integer :: ib

    CALL BasisIn%Read()
    !CALL BasisIn%Write()

    SELECT CASE (BasisIn%name)
    CASE ('ho')
      !write(out_unit,*) 'HO basis'
      ! its means only one primitive basis
      allocate(Basis_HO_t :: basis)
      basis = init_Basis_HO(BasisIn)
      CALL basis%Set_tab_n_OF_l(BasisIn%LB_in,BasisIn%LG_in)

    CASE ('boxab')
      !write(out_unit,*) 'BoxAB basis'
      ! its means only one primitive basis
      allocate(Basis_BoxAB_t :: basis)
      basis = init_Basis_BoxAB(BasisIn)

      CALL basis%Set_tab_n_OF_l(BasisIn%LB_in,BasisIn%LG_in)
      CALL basis%Set_Grid()
      CALL basis%Set_GB()
      CALL basis%Scale()
      CALL basis%Set_BGW()
      CALL basis%Set_BB()
      CALL basis%Set_GG()
      CALL basis%CheckOrtho()
    CASE ('dp')
      !write(out_unit,*) 'DP basis'
      allocate(Basis_DP_t :: basis)
      basis = init_Basis_DP(BasisIn)
    CASE ('sbg')
      !write(out_unit,*) 'SBG basis'
      allocate(Basis_SBG_t :: basis)
      basis = init_Basis_SBG(BasisIn)
    CASE default
      STOP 'no default'
    END SELECT
    basis%layer     = layer + 1
    basis%tab_layer = make_tab_layer(layer)
    CALL BasisIn%dealloc()


    SELECT TYPE (basis)
    TYPE IS(Basis_DP_t)
      DO ib=1,BasisIn%nb_basis
        CALL Read_Basis(basis%tab_Pbasis(ib)%PBasis,basis%layer)
      END DO
      CALL basis%Set_ndim()
      CALL basis%Set_tab_n_OF_l(-1,-1)
      
    TYPE IS(Basis_SBG_t)
      DO ib=1,BasisIn%nb_basis
        CALL Read_Basis(basis%tab_Pbasis(ib)%PBasis,basis%layer)
      END DO
      CALL basis%Set_ndim()
      CALL basis%Set_tab_n_OF_l(basis%LB,basis%LG)
    END SELECT

  END SUBROUTINE Read_Basis

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