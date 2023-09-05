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
  USE BasisInput_m
  USE Basis_base_m
  USE Basis_HO_m
  IMPLICIT NONE
  PRIVATE

  TYPE :: Pbasis_t
    CLASS (Basis_t), allocatable :: Pbasis
  END TYPE Pbasis_t

  TYPE, EXTENDS (Basis_t) :: Basis_DP_t
    TYPE (Pbasis_t), allocatable :: tab_Pbasis(:)
  CONTAINS
    PROCEDURE :: Write => Write_Basis_DP
  END TYPE Basis_DP_t

  PUBLIC :: Basis_DP_t,init_Basis_DP

  CONTAINS
  FUNCTION init_Basis_DP(nb_basis) RESULT (basis)
    USE QDUtil_m
    TYPE (Basis_DP_t)   :: basis
    integer, intent(in) :: nb_basis


    TYPE (BasisInput_t) :: BasisIn

    integer :: i,nb,nq,ndim

    basis%name = 'DP'
    nb   = 1
    nq   = 1
    ndim = 0
    IF (nb_basis < 1) STOP ' ERROR in init_Basis_DP: nb_basis < 1'
    allocate(basis%tab_Pbasis(nb_basis))

    DO i=1,nb_basis
      CALL BasisIn%Read()
      !CALL BasisIn%Write()

      SELECT CASE (BasisIn%name)
      CASE ('ho')
        ! its means only one primitive basis
        basis%tab_Pbasis(i)%Pbasis = init_Basis_HO(nb=BasisIn%nb,nq=BasisIn%nq,Q0=BasisIn%Q0,ScQ=BasisIn%ScQ)
      CASE ('dp')
        basis%tab_Pbasis(i)%Pbasis = init_Basis_DP(nb_basis=BasisIn%nb_basis)
      CASE default
        STOP 'no default'
      END SELECT

      CALL BasisIn%dealloc()

      nb   = nb   * basis%tab_Pbasis(i)%Pbasis%nb
      nq   = nq   * basis%tab_Pbasis(i)%Pbasis%nq
      ndim = ndim + basis%tab_Pbasis(i)%Pbasis%ndim
    END DO
    basis%nb   = nb
    basis%nq   = nq
    basis%ndim = ndim

  END FUNCTION init_Basis_DP

  SUBROUTINE Write_Basis_DP(basis)
    USE QDUtil_m

    CLASS (Basis_DP_t), intent(in) :: basis

    integer :: i

    write(out_unit,*) '-------------------------------------'
    CALL basis%Basis_t%write()
    write(out_unit,*) 'DP: nb_basis',size(basis%tab_Pbasis)
    IF (allocated(basis%tab_Pbasis)) THEN
      DO i=1,size(basis%tab_Pbasis)
        IF (allocated(basis%tab_Pbasis(i)%Pbasis)) THEN
          CALL basis%tab_Pbasis(i)%Pbasis%write()
        END IF
      END DO
    END IF
    write(out_unit,*) '-------------------------------------'
  END SUBROUTINE Write_Basis_DP
END MODULE Basis_DP_m