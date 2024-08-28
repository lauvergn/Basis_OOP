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
MODULE Basis_DP_m
  USE Basis_base_m
  USE Basis_HO_m
  USE Basis_BoxAB_m
  IMPLICIT NONE
  PRIVATE

  TYPE :: Pbasis_t
    CLASS (Basis_t), allocatable :: Pbasis
  END TYPE Pbasis_t

  TYPE, EXTENDS (Basis_t) :: Basis_DP_t
    TYPE (Pbasis_t), allocatable :: tab_Pbasis(:)
  CONTAINS
    PROCEDURE :: Write          => Write_Basis_DP
    PROCEDURE :: Set_tab_n_OF_l => Set_tab_n_OF_l_Basis_DP
    PROCEDURE :: Set_ndim       => Set_ndim_Basis_DP
  END TYPE Basis_DP_t

  PUBLIC :: Pbasis_t,Basis_DP_t,init_Basis_DP

  CONTAINS
  FUNCTION init_Basis_DP(basisIn) RESULT (basis)
    USE QDUtil_m
    USE BasisInput_m

    TYPE (Basis_DP_t)               :: basis
    TYPE (BasisInput_t), intent(in) :: basisIn

    !write(out_unit,*) 'Beginning init_Basis_DP'

    basis%name = 'DP'

    IF (basisIn%nb_basis < 1) STOP ' ERROR in init_Basis_DP: nb_basis < 1'
    allocate(basis%tab_Pbasis(basisIn%nb_basis))

  END FUNCTION init_Basis_DP

  RECURSIVE SUBROUTINE Write_Basis_DP(basis)
    USE QDUtil_m

    CLASS (Basis_DP_t), intent(in) :: basis

    integer :: ib

    write(out_unit,*) basis%tab_layer,'-------------------------------------'
    CALL basis%Basis_t%write()
    write(out_unit,*) basis%tab_layer,'DP: nb_basis',size(basis%tab_Pbasis)

    IF (allocated(basis%tab_Pbasis)) THEN

      DO ib=1,size(basis%tab_Pbasis)
        write(out_unit,*) basis%tab_layer,'ib: ',ib

        IF (allocated(basis%tab_Pbasis(ib)%Pbasis)) THEN
          CALL basis%tab_Pbasis(ib)%Pbasis%write()
        ELSE
          write(out_unit,*) basis%tab_layer,'DP: Pbasis is not allocated'
        END IF
      END DO
    ELSE
      write(out_unit,*) basis%tab_layer,'DP: tab_Pbasis is not allocated'
    END IF

    write(out_unit,*) basis%tab_layer,'-------------------------------------'
  END SUBROUTINE Write_Basis_DP

  SUBROUTINE Set_ndim_Basis_DP(basis)
    USE QDUtil_m, ONLY : Rkind, out_unit

    CLASS (Basis_DP_t), intent(inout) :: basis

    integer :: ib

    basis%ndim = 0
    DO ib=1,size(basis%tab_Pbasis)
      basis%ndim = basis%ndim + basis%tab_Pbasis(ib)%Pbasis%ndim
    END DO

  END SUBROUTINE Set_ndim_Basis_DP

  SUBROUTINE Set_tab_n_OF_l_Basis_DP(basis,LB_in,LG_in)
    USE QDUtil_m, ONLY : Rkind, out_unit

    CLASS (Basis_DP_t), intent(inout) :: basis
    integer,            intent(in)    :: LB_in,LG_in

    integer :: ib,l

    IF (LB_in > -1 .AND. LG_in > -1) THEN
      STOP 'STOP in Set_tab_n_OF_l_Basis_DP: not yet with LB_in,LG_in'
      allocate(basis%tab_nb(0:LB_in))
      !basis%tab_nb(0:LB_in) = [((l+1),l=0,LB_in)]

      allocate(basis%tab_nq(0:LG_in))
      !basis%tab_nq(0:LG_in) = [((l+1),l=0,LG_in)]
    ELSE
      allocate(basis%tab_nb(0:0))
      allocate(basis%tab_nq(0:0))

      basis%tab_nb(0) = 1
      basis%tab_nq(0) = 1

      DO ib=1,size(basis%tab_Pbasis)
        basis%tab_nb(0) =  basis%tab_nb(0) * basis%tab_Pbasis(ib)%Pbasis%nb
        basis%tab_nq(0) =  basis%tab_nq(0) * basis%tab_Pbasis(ib)%Pbasis%nq
      END DO

      basis%nb = basis%tab_nb(0)
      basis%nq = basis%tab_nq(0)

    END IF

    !write(*,*) 'basis%tab_nb',basis%tab_nb
    !write(*,*) 'basis%tab_nq',basis%tab_nq
  END SUBROUTINE Set_tab_n_OF_l_Basis_DP
END MODULE Basis_DP_m