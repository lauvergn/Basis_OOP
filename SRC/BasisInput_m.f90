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
MODULE BasisInput_m
  USE QDUtil_m
  IMPLICIT NONE
  PRIVATE
  TYPE BasisInput_t
    integer                        :: nb = 0
    integer                        :: nq = 0
    integer                        :: nb_basis = 0
    character (len=:), allocatable :: name
    real (kind=Rkind)              :: Q0  = ZERO
    real (kind=Rkind)              :: ScQ = ONE

  CONTAINS
    PROCEDURE :: Read    => Read_BasisInput
    PROCEDURE :: Write   => Write_BasisInput
    PROCEDURE :: dealloc => dealloc_BasisInput
  END TYPE BasisInput_t

  PUBLIC :: BasisInput_t

  CONTAINS
  SUBROUTINE Read_BasisInput(BasisInput)
    USE QDUtil_m

    CLASS (BasisInput_t), intent(inout) :: BasisInput

    integer            :: nb,nq,nb_basis
    character (len=50) :: name
    real (kind=Rkind)  :: Q0,ScQ

    namelist / basis / nb,nq,name,Q0,ScQ,nb_basis

    nb       = 0
    nq       = 0
    nb_basis = 0
    name     = '0'
    Q0       = ZERO
    ScQ      = ONE

    read(*,basis)
    !write(*,basis)

    BasisInput%nb       = nb
    BasisInput%nq       = nq
    BasisInput%nb_basis = nb_basis
    BasisInput%name     = TO_lowercase(trim(adjustl(name)))
    BasisInput%Q0       = Q0
    BasisInput%ScQ      = ScQ

  END SUBROUTINE Read_BasisInput
  SUBROUTINE Write_BasisInput(BasisInput)
    USE QDUtil_m

    CLASS (BasisInput_t), intent(in) :: BasisInput

    write(out_unit,*) '-------------------------------------'
    write(out_unit,*) '--- BasisInput ----------------------'
    write(out_unit,*) '-------------------------------------'
    IF (allocated(BasisInput%name)) THEN
      write(out_unit,*) 'name: ',BasisInput%name
    ELSE
      write(out_unit,*) 'name: not initialized!'
    END IF
    write(out_unit,*) 'nb =',BasisInput%nb
    write(out_unit,*) 'nq =',BasisInput%nq
    write(out_unit,*) 'Q0 =',BasisInput%Q0
    write(out_unit,*) 'ScQ=',BasisInput%ScQ
    write(out_unit,*) '-------------------------------------'
    write(out_unit,*) '--- END BasisInput ------------------'
    write(out_unit,*) '-------------------------------------'
  END SUBROUTINE Write_BasisInput

  SUBROUTINE dealloc_BasisInput(BasisInput)
    CLASS (BasisInput_t), intent(inout) :: BasisInput

    IF (allocated(BasisInput%name)) deallocate(BasisInput%name) 

  END SUBROUTINE dealloc_BasisInput
END MODULE BasisInput_m