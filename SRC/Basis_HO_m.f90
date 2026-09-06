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
    PROCEDURE :: Write     => Write_Basis_HO
    PROCEDURE :: Set_Grid  => Set_Grid_Basis_HO
    PROCEDURE :: Set_GB    => Set_GB_Basis_HO
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
  SUBROUTINE Write_Basis_HO(this,nio,info)
    USE QDUtil_m, ONLY : Rkind, out_unit

    CLASS (Basis_HO_t),   intent(in)           :: this
    integer,              intent(in), optional :: nio
    character (len=*),    intent(in), optional :: info

    integer :: nio_loc

    IF (present(nio)) THEN
      nio_loc = nio
    ELSE
      nio_loc = out_unit
    END IF

    write(nio_loc,*) this%tab_layer,'-------------------------------------'
    CALL this%Basis_t%write(nio=nio_loc)
    write(nio_loc,*) this%tab_layer,'-------------------------------------'

  END SUBROUTINE Write_Basis_HO

  SUBROUTINE Set_Grid_Basis_HO(this)
    USE QDUtil_m, ONLY : Rkind, out_unit, Quadrature_t, Init_Quadrature_HP, dealloc_Quadrature

    CLASS (Basis_HO_t), intent(inout) :: this

    integer :: l,LG,nql

    TYPE (Quadrature_t) :: xw
    integer :: err

    IF (allocated(this%tab_nq)) THEN

      LG = size(this%tab_nq)-1
      allocate(this%X(0:LG))
      allocate(this%W(0:LG))

      DO l=0,LG
        nql = this%get_nq(l)
        CALL Init_Quadrature_HP(xw,nql,name='HO',err=err)
        IF (err /=0) THEN
          write(out_unit,*) 'ERROR in Set_Grid_Basis_HO'
          write(out_unit,*) 'Problem with the "Init_Quadrature_HP" subroutine (QDUtil module)'
          STOP 'ERROR in Set_Grid_Basis_HO: Problem with the "Init_Quadrature_HP" subroutine (QDUtil module)'
        END IF
        this%X(l) = xw%x
        this%W(l) = xw%w
      END DO
      CALL dealloc_Quadrature(xw)
    ELSE
      write(out_unit,*) 'ERROR in Set_Grid_Basis_HO'
      write(out_unit,*) 'tab_nq is not allocated'
      STOP 'ERROR in Set_Grid_Basis_HO: tab_nq is not allocated'
    END IF

  END SUBROUTINE Set_Grid_Basis_HO

  SUBROUTINE Set_GB_Basis_HO(this)
    USE QDUtil_m, ONLY : Rkind, ZERO, HALF, PI, out_unit
    USE ADdnSVM_m

    CLASS (Basis_HO_t), intent(inout) :: this

    integer           :: l,LG,nql,nbl,iq,ib
    TYPE (dnS_t)      :: dnx,dnB
    real (kind=Rkind) :: xiq

    IF (allocated(this%tab_nq) .AND. allocated(this%tab_nb)) THEN

      LG = size(this%tab_nq)-1
      allocate(this%GB(0:LG))

      DO l=0,LG
        nql = this%get_nq(l)
        nbl = this%get_nb(l)

        CALL alloc_dnMat(this%GB(l),sizeL=nql, sizeC=nbl, nVar=1, nderiv=2)

        DO iq=1,nql
          xiq = this%X(l)%d0(1,iq)

          dnx = Variable(xiq, nvar=1, nderiv=2)

          DO ib=1,nbl
            dnB = dnExpHermite(dnX,ib-1,ReNorm=.TRUE.)
            CALL dnS_TO_dnMat(dnB,this%GB(l),i=iq,j=ib)
          END DO

        END DO
      END DO
    ELSE
      write(out_unit,*) 'ERROR in Set_GB_Basis_HO'
      write(out_unit,*) 'tab_nq or tab_nb are not allocated'
      STOP 'ERROR in Set_GB_Basis_HO: tab_nq or tab_nb are not allocated'
    END IF

  END SUBROUTINE Set_GB_Basis_HO
END MODULE Basis_HO_m