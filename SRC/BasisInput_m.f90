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

    real (kind=Rkind)              :: A = ZERO
    real (kind=Rkind)              :: B = ONE

    integer                        :: LB = -1
    integer                        :: LG = -1

    integer                        :: LB_in = -1
    integer                        :: LG_in = -1
  CONTAINS
    PROCEDURE :: Read    => Read_BasisInput
    PROCEDURE :: Write   => Write_BasisInput
    PROCEDURE :: dealloc => dealloc_BasisInput
  END TYPE BasisInput_t

  PUBLIC :: BasisInput_t

  CONTAINS
  SUBROUTINE Read_BasisInput(BasisInput,LB_in,LG_in)
    USE QDUtil_m

    CLASS (BasisInput_t), intent(inout)        :: BasisInput
    integer,              intent(in), optional :: LB_in,LG_in


    integer            :: nb,nq,nb_basis,LB,LG
    character (len=50) :: name
    real (kind=Rkind)  :: Q0,ScQ
    real (kind=Rkind)  :: A,B

    namelist / basis / nb,nq,name,Q0,ScQ,A,B,nb_basis,LB,LG

    nb       = 0
    nq       = 0
    LB       = -1
    LG       = -1

    nb_basis = 0

    name     = '0'
    Q0       = ZERO
    ScQ      = ONE
    A        = ZERO
    B        = ONE

    read(*,basis)
    !write(*,basis)

    BasisInput%nb_basis = nb_basis

    BasisInput%name     = TO_lowercase(trim(adjustl(name)))
    BasisInput%Q0       = Q0
    BasisInput%ScQ      = ScQ
    BasisInput%A        = A
    BasisInput%B        = B

    BasisInput%LB_in    = -1
    BasisInput%LG_in    = -1

    IF (present(LB_in) .OR. present(LG_in)) THEN
      BasisInput%nb       = 0
      BasisInput%nq       = 0
      BasisInput%LB       = -1
      BasisInput%LG       = -1
      IF (present(LB_in)) THEN 
        BasisInput%LB       = LB
      ELSE
        BasisInput%LB       = LG
      END IF
      IF (present(LG_in)) THEN 
        BasisInput%LG       = LG
      ELSE
        BasisInput%LG       = LB
      END IF
    ELSE
      BasisInput%nb       = nb
      IF (nq < 1) nq = nb
      BasisInput%nq       = nq
      BasisInput%LB       = LB
      BasisInput%LG       = LG
    END IF

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
    write(out_unit,*) 'nb    =',BasisInput%nb
    write(out_unit,*) 'nq    =',BasisInput%nq

    write(out_unit,*) 'Q0    =',BasisInput%Q0
    write(out_unit,*) 'ScQ   =',BasisInput%ScQ

    write(out_unit,*) 'A     =',BasisInput%A
    write(out_unit,*) 'B     =',BasisInput%B

    write(out_unit,*) 'LB    =',BasisInput%LB
    write(out_unit,*) 'LG    =',BasisInput%LG
    write(out_unit,*) 'LB_in =',BasisInput%LB_in
    write(out_unit,*) 'LG_in =',BasisInput%LG_in
    write(out_unit,*) '-------------------------------------'
    write(out_unit,*) '--- END BasisInput ------------------'
    write(out_unit,*) '-------------------------------------'
  END SUBROUTINE Write_BasisInput

  SUBROUTINE dealloc_BasisInput(BasisInput)
    CLASS (BasisInput_t), intent(inout) :: BasisInput

    IF (allocated(BasisInput%name)) deallocate(BasisInput%name) 

  END SUBROUTINE dealloc_BasisInput
END MODULE BasisInput_m