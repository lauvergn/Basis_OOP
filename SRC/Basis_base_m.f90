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
MODULE Basis_base_m
  USE QDUtil_m, ONLY : Rkind
  IMPLICIT NONE
  PRIVATE

  TYPE Basis_t
    integer :: nb
    integer :: nq
    integer :: ndim
    character (len=:), allocatable :: name
  CONTAINS
    PROCEDURE :: Write  => Write_Basis_base
  END TYPE Basis_t

  PUBLIC :: Basis_t,Init_Basis

  !interface assignment (=)
  !  module procedure basis2_TO_basis1 
  !end interface assignment (=)

  CONTAINS
  FUNCTION Init_Basis(nb,nq,ndim,name) RESULT(basis)
    USE QDUtil_m, ONLY : Rkind
    CLASS (Basis_t), allocatable :: basis

    integer,           intent(in) :: nb
    integer,           intent(in) :: nq
    integer,           intent(in) :: ndim
    character (len=*), intent(in) :: name

    allocate(Basis_t :: basis)
  
    basis%nb   = nb
    basis%nq   = nq
    basis%ndim = ndim
    basis%name = name

  END FUNCTION init_Basis
  SUBROUTINE basis2_TO_basis1(basis1,basis2)
    CLASS(Basis_t), allocatable , intent(out) :: basis1
    CLASS(Basis_t),               intent(in)  :: basis2

    allocate(basis1, source=basis2)

  END  SUBROUTINE basis2_TO_basis1
  SUBROUTINE Write_Basis_base(basis)
    USE QDUtil_m, ONLY : Rkind, out_unit
    CLASS (Basis_t), intent(in) :: basis

    write(out_unit,*) '-------------------------------------'
    IF (allocated(basis%name)) THEN
      write(out_unit,*) 'name: ',basis%name
    ELSE
      write(out_unit,*) 'name: not initialized!'
    END IF
    write(out_unit,*) 'ndim=',basis%ndim
    write(out_unit,*) 'nb=  ',basis%nb
    write(out_unit,*) 'nq=  ',basis%nq
    write(out_unit,*) '-------------------------------------'
  END SUBROUTINE Write_Basis_base
END MODULE Basis_base_m