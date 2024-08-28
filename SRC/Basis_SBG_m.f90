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
MODULE Basis_SBG_m
  USE Basis_DP_m
  IMPLICIT NONE
  PRIVATE

  TYPE, EXTENDS (Basis_DP_t) :: Basis_SBG_t
    integer :: LB = -1
    integer :: LG = -1
  CONTAINS
    PROCEDURE :: Write          => Write_Basis_SBG
    PROCEDURE :: Set_tab_n_OF_l => Set_tab_n_OF_l_Basis_SBG
  END TYPE Basis_SBG_t

  PUBLIC :: Basis_SBG_t,init_Basis_SBG

  CONTAINS
  FUNCTION init_Basis_SBG(basisIn) RESULT (basis)
    USE QDUtil_m
    USE BasisInput_m

    TYPE (Basis_SBG_t)              :: basis
    TYPE (BasisInput_t), intent(in) :: basisIn


    !write(out_unit,*) 'Beginning init_Basis_SBG'

    basis%name = 'SBG'
    IF (basisIn%nb_basis < 1) STOP ' ERROR in init_Basis_SBG: nb_basis < 1'
    allocate(basis%tab_Pbasis(basisIn%nb_basis))

  END FUNCTION init_Basis_SBG

  RECURSIVE SUBROUTINE Write_Basis_SBG(basis)
    USE QDUtil_m

    CLASS (Basis_SBG_t), intent(in) :: basis

    write(out_unit,*) basis%tab_layer,'---- SBG ----------------------------'
    CALL basis%Basis_DP_t%write()

    write(out_unit,*) basis%tab_layer,'SBG: nb_basis',size(basis%tab_Pbasis)
    write(out_unit,*) basis%tab_layer,'LB=',basis%LB
    write(out_unit,*) basis%tab_layer,'LG=',basis%LG
    write(out_unit,*) basis%tab_layer,'---- END SBG -------------------------'

  END SUBROUTINE Write_Basis_SBG

  SUBROUTINE Set_tab_n_OF_l_Basis_SBG(basis,LB_in,LG_in)
    USE QDUtil_m, ONLY : Rkind, out_unit

    CLASS (Basis_SBG_t), intent(inout) :: basis
    integer,            intent(in)    :: LB_in,LG_in

    integer :: ib,l

    IF (LB_in > -1 .AND. LG_in > -1) THEN
      STOP 'STOP in Set_tab_n_OF_l_Basis_SBG: not yet with LB_in,LG_in'

    ELSE
      STOP 'STOP in Set_tab_n_OF_l_Basis_SBG: not yet' 
      allocate(basis%tab_nb(0:basis%LB))
      allocate(basis%tab_nq(0:basis%LG))

      basis%tab_nb(0) = 1
      basis%tab_nq(0) = 1

      DO ib=1,size(basis%tab_Pbasis)

        basis%tab_nb(0) =  basis%tab_nb(0) * basis%tab_Pbasis(ib)%Pbasis%nb
        basis%tab_nq(0) =  basis%tab_nq(0) * basis%tab_Pbasis(ib)%Pbasis%nq
      END DO

    END IF

    !write(*,*) 'basis%tab_nb',basis%tab_nb
    !write(*,*) 'basis%tab_nq',basis%tab_nq
  END SUBROUTINE Set_tab_n_OF_l_Basis_SBG
END MODULE Basis_SBG_m