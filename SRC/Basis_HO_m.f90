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
MODULE Basis_HO_m
  USE QDUtil_m, ONLY : Rkind
  USE Basis_base_m
  IMPLICIT NONE
  PRIVATE

  TYPE, EXTENDS (Basis_t) :: Basis_HO_t

  CONTAINS
    PROCEDURE :: Write  => Write_Basis_HO
  END TYPE Basis_HO_t

  PUBLIC :: Basis_HO_t,init_Basis_HO

  CONTAINS
  FUNCTION init_Basis_HO(basisIn) RESULT(this)
    USE QDUtil_m
    USE BasisInput_m

    TYPE (Basis_HO_t)               :: this
    TYPE (BasisInput_t), intent(in) :: basisIn

    !write(out_unit,*) 'Beginning init_Basis_HO'

    this%basis_t    = Init_Basis(basisIn)

    this%ndim       = 1
    this%primitive  = .TRUE.

    this%Q0         = basisIn%Q0(1:1)
    this%ScQ        = basisIn%ScQ(1:1)

  END FUNCTION init_Basis_HO
  SUBROUTINE Write_Basis_HO(this)
    USE QDUtil_m, ONLY : Rkind, out_unit

    CLASS (Basis_HO_t), intent(in) :: this

    write(out_unit,*) this%tab_layer,'-------------------------------------'
    CALL this%Basis_t%write()
    write(out_unit,*) this%tab_layer,'-------------------------------------'

  END SUBROUTINE Write_Basis_HO
END MODULE Basis_HO_m