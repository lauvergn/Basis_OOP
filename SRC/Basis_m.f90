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
  USE BasisInput_m
  USE Basis_base_m
  USE Basis_HO_m
  USE Basis_DP_m
  IMPLICIT NONE


  CONTAINS
  SUBROUTINE Read_Basis(basis)
    USE QDUtil_m

    TYPE (Basis_t), intent(inout) :: basis

    TYPE (BasisInput_t) :: BasisIn

    SELECT CASE (BasisIn%name)
    CASE ('ho')
      ! its means only one primitive basis
      basis = init_Basis_HO(nb=BasisIn%nb,nq=BasisIn%nq,Q0=BasisIn%Q0,ScQ=BasisIn%ScQ)
    CASE ('dp')
      basis = init_Basis_DP(nb_basis=BasisIn%nb_basis)
    CASE default
      STOP 'no default'
    END SELECT

  END SUBROUTINE Read_Basis

END MODULE Basis_m