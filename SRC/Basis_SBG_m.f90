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
    TYPE (Pbasis_t), allocatable :: tab_SBG_Pbasis(:,:)
  CONTAINS
    PROCEDURE :: Write => Write_Basis_SBG
  END TYPE Basis_SBG_t

  PUBLIC :: Basis_SBG_t,init_Basis_SBG

  CONTAINS
  FUNCTION init_Basis_SBG(nb_basis) RESULT (basis)
    USE QDUtil_m
    TYPE (Basis_SBG_t)   :: basis
    integer, intent(in) :: nb_basis


    integer :: i,nb,nq,ndim

    !write(out_unit,*) 'Beginning init_Basis_SBG'

    basis%name = 'SBG'
    IF (nb_basis < 1) STOP ' ERROR in init_Basis_SBG: nb_basis < 1'
    allocate(basis%tab_Pbasis(nb_basis))

  END FUNCTION init_Basis_SBG

  SUBROUTINE Write_Basis_SBG(basis)
    USE QDUtil_m

    CLASS (Basis_SBG_t), intent(in) :: basis

    write(out_unit,*) basis%tab_layer,'---- SBG ----------------------------'
    CALL basis%Basis_DP_t%write()

    write(out_unit,*) basis%tab_layer,'SBG: nb_basis',size(basis%tab_Pbasis)
    write(out_unit,*) basis%tab_layer,'LB=',basis%LB
    write(out_unit,*) basis%tab_layer,'LG=',basis%LG
    write(out_unit,*) basis%tab_layer,'---- END SBG -------------------------'

  END SUBROUTINE Write_Basis_SBG
END MODULE Basis_SBG_m