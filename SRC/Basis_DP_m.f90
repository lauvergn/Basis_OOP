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
  FUNCTION init_Basis_DP(basisIn) RESULT (this)
    USE QDUtil_m
    USE BasisInput_m

    TYPE (Basis_DP_t)               :: this
    TYPE (BasisInput_t), intent(in) :: basisIn

    !write(out_unit,*) 'Beginning init_Basis_DP'

    this%name = 'DP'

    IF (basisIn%nb_basis < 1) STOP ' ERROR in init_Basis_DP: nb_basis < 1'
    allocate(this%tab_Pbasis(basisIn%nb_basis))

  END FUNCTION init_Basis_DP

  RECURSIVE SUBROUTINE Write_Basis_DP(this)
    USE QDUtil_m

    CLASS (Basis_DP_t), intent(in) :: this

    integer :: ib

    write(out_unit,*) this%tab_layer,'-------------------------------------'
    CALL this%Basis_t%write()
    write(out_unit,*) this%tab_layer,'DP: nb_basis',size(this%tab_Pbasis)

    IF (allocated(this%tab_Pbasis)) THEN

      DO ib=1,size(this%tab_Pbasis)
        write(out_unit,*) this%tab_layer,'ib: ',ib

        IF (allocated(this%tab_Pbasis(ib)%Pbasis)) THEN
          CALL this%tab_Pbasis(ib)%Pbasis%write()
        ELSE
          write(out_unit,*) this%tab_layer,'DP: Pbasis is not allocated'
        END IF
      END DO
    ELSE
      write(out_unit,*) this%tab_layer,'DP: tab_Pbasis is not allocated'
    END IF

    write(out_unit,*) this%tab_layer,'-------------------------------------'
  END SUBROUTINE Write_Basis_DP

  SUBROUTINE Set_ndim_Basis_DP(this)
    USE QDUtil_m, ONLY : Rkind, out_unit

    CLASS (Basis_DP_t), intent(inout) :: this

    integer :: ib

    this%ndim = 0
    DO ib=1,size(this%tab_Pbasis)
      this%ndim = this%ndim + this%tab_Pbasis(ib)%Pbasis%ndim
    END DO

  END SUBROUTINE Set_ndim_Basis_DP

  SUBROUTINE Set_tab_n_OF_l_Basis_DP(this,LG_in)
    USE QDUtil_m, ONLY : Rkind, out_unit

    CLASS (Basis_DP_t), intent(inout) :: this
    integer,            intent(in)    :: LG_in

    integer :: ib,l

    IF (LG_in > -1) THEN
      STOP 'STOP in Set_tab_n_OF_l_Basis_DP: not yet with LB_in,LG_in'
      allocate(this%tab_nb(0:LG_in))
      !this%tab_nb(0:LG_in) = [((l+1),l=0,LG_in)]

      allocate(this%tab_nq(0:LG_in))
      !this%tab_nq(0:LG_in) = [((l+1),l=0,LG_in)]
    ELSE
      allocate(this%tab_nb(0:0))
      allocate(this%tab_nq(0:0))

      this%tab_nb(0) = 1
      this%tab_nq(0) = 1

      DO ib=1,size(this%tab_Pbasis)
        this%tab_nb(0) =  this%tab_nb(0) * this%tab_Pbasis(ib)%Pbasis%nb
        this%tab_nq(0) =  this%tab_nq(0) * this%tab_Pbasis(ib)%Pbasis%nq
      END DO

      this%nb = this%tab_nb(0)
      this%nq = this%tab_nq(0)

    END IF

    !write(*,*) 'this%tab_nb',this%tab_nb
    !write(*,*) 'this%tab_nq',this%tab_nq
  END SUBROUTINE Set_tab_n_OF_l_Basis_DP
END MODULE Basis_DP_m