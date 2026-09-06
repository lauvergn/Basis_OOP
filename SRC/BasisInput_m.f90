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

    real (kind=Rkind), allocatable :: Q0(:)
    real (kind=Rkind), allocatable :: ScQ(:)

    real (kind=Rkind), allocatable :: A(:)
    real (kind=Rkind), allocatable :: B(:)

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
  SUBROUTINE Read_BasisInput(BasisInput,LG_in)
    USE QDUtil_m

    CLASS (BasisInput_t), intent(inout)        :: BasisInput
    integer,              intent(in), optional :: LG_in

    integer, parameter :: max_ndim = 10

    integer            :: nb,nq,nb_basis,LB,LG
    character (len=50) :: name
    real (kind=Rkind), allocatable  :: Q0(:),ScQ(:)
    real (kind=Rkind), allocatable  :: A(:),B(:)

    namelist / basis / nb,nq,name,Q0,ScQ,A,B,nb_basis,LB,LG

    !-----------------------------------------------------------------------
    !logical, parameter :: debug=.TRUE.
    logical, parameter :: debug=.FALSE.
    character(len=*), parameter :: name_sub='Read_BasisInput@BasisInput_m'
    !-----------------------------------------------------------------------
    IF (debug) THEN
      write(out_unit,*) 'BEGINNING ',name_sub
      write(out_unit,*) 'max_ndim ',max_ndim
      IF (present(LG_in)) THEN
        write(out_unit,*) 'LG_in ',LG_in
      ELSE
        write(out_unit,*) 'LG_in: not present'
      END IF
      flush(out_unit)
      CALL BasisInput%write()
      flush(out_unit)
    END IF
    !-----------------------------------------------------------------------

    allocate(Q0(max_ndim))
    allocate(ScQ(max_ndim))
    allocate(A(max_ndim))
    allocate(B(max_ndim))


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
    IF (debug) THEN 
      write(*,basis)
      flush(out_unit)
    END IF


    BasisInput%nb_basis = nb_basis

    BasisInput%name     = TO_lowercase(trim(adjustl(name)))
    BasisInput%Q0       = Q0
    BasisInput%ScQ      = ScQ
    BasisInput%A        = A
    BasisInput%B        = B

    BasisInput%LB_in    = -1
    BasisInput%LG_in    = -1

    IF (present(LG_in)) THEN
      BasisInput%nb       = 0
      BasisInput%nq       = 0
      BasisInput%LB       = LG_in
      BasisInput%LG       = LG_in
      BasisInput%LB_in    = LG_in
      BasisInput%LG_in    = LG_in
    ELSE
      BasisInput%nb       = nb
      IF (nq < 1) nq = nb
      BasisInput%nq       = nq
      BasisInput%LB       = LB
      BasisInput%LG       = LG
    END IF

    !-----------------------------------------------------------------------
    IF (debug) THEN
      CALL BasisInput%write()
      write(out_unit,*) 'END ',name_sub
      flush(out_unit)
    END IF
    !-----------------------------------------------------------------------
  END SUBROUTINE Read_BasisInput
  SUBROUTINE Write_BasisInput(BasisInput,nio,info)
    USE QDUtil_m

    CLASS (BasisInput_t), intent(in)           :: BasisInput
    integer,              intent(in), optional :: nio
    character (len=*),    intent(in), optional :: info

    integer :: nio_loc


    IF (present(nio)) THEN
      nio_loc =nio
    ELSE
      nio_loc = out_unit
    END IF

    write(nio_loc,*) '-------------------------------------'
    IF (present(info)) THEN
      write(nio_loc,*) '--- BasisInput: ',info
    ELSE
      write(nio_loc,*) '--- BasisInput ----------------------'
    END IF
    write(nio_loc,*) '-------------------------------------'
    IF (allocated(BasisInput%name)) THEN
      write(nio_loc,*) 'name: ',BasisInput%name
    ELSE
      write(nio_loc,*) 'name: not initialized!'
    END IF
    write(nio_loc,*) 'nb    =',BasisInput%nb
    write(nio_loc,*) 'nq    =',BasisInput%nq

    write(nio_loc,*) 'Q0    =',BasisInput%Q0
    write(nio_loc,*) 'ScQ   =',BasisInput%ScQ

    write(nio_loc,*) 'A     =',BasisInput%A
    write(nio_loc,*) 'B     =',BasisInput%B

    write(nio_loc,*) 'LB    =',BasisInput%LB
    write(nio_loc,*) 'LG    =',BasisInput%LG
    write(nio_loc,*) 'LB_in =',BasisInput%LB_in
    write(nio_loc,*) 'LG_in =',BasisInput%LG_in
    write(nio_loc,*) '-------------------------------------'
    IF (present(info)) THEN
      write(nio_loc,*) '--- BasisInput: ',info
      write(nio_loc,*) '--- END BasisInput: ',info
    ELSE
    write(nio_loc,*) '--- END BasisInput ------------------'
    END IF
    write(nio_loc,*) '-------------------------------------'
  END SUBROUTINE Write_BasisInput

  SUBROUTINE dealloc_BasisInput(BasisInput)
    CLASS (BasisInput_t), intent(inout) :: BasisInput

    IF (allocated(BasisInput%name)) deallocate(BasisInput%name) 

  END SUBROUTINE dealloc_BasisInput
END MODULE BasisInput_m