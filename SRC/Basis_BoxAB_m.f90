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
! THE SOFTWARE IS PROVIDED "AS IS", WITBoxABUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTBoxABRS OR COPYRIGHT BoxABLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!===============================================================================
!===============================================================================
MODULE Basis_BoxAB_m
  USE QDUtil_m, ONLY : Rkind
  USE Basis_base_m
  IMPLICIT NONE
  PRIVATE

  TYPE, EXTENDS (Basis_t) :: Basis_BoxAB_t
    real (kind=Rkind) :: A
    real (kind=Rkind) :: B
  CONTAINS
    PROCEDURE :: Write     => Write_Basis_BoxAB
    PROCEDURE :: Set_Grid  => Set_Grid_Basis_BoxAB
    PROCEDURE :: Set_GB    => Set_GB_Basis_BoxAB
  END TYPE Basis_BoxAB_t

  PUBLIC :: Basis_BoxAB_t,init_Basis_BoxAB

  CONTAINS
  FUNCTION init_Basis_BoxAB(basisIn) RESULT(this)
    USE QDUtil_m
    USE BasisInput_m

    TYPE (Basis_BoxAB_t)            :: this
    TYPE (BasisInput_t), intent(in) :: basisIn

    !write(out_unit,*) 'Beginning init_Basis_BoxAB'

    this%basis_t = Init_Basis(basisIn)

    this%ndim   = 1
    this%A      = basisIn%A
    this%B      = basisIn%B

    this%Q0     = this%A
    this%ScQ    = PI/(this%B-this%A)

  END FUNCTION init_Basis_BoxAB
  SUBROUTINE Write_Basis_BoxAB(this)
    USE QDUtil_m, ONLY : Rkind, out_unit

    CLASS (Basis_BoxAB_t), intent(in) :: this

    write(out_unit,*) this%tab_layer,'-------------------------------------'
    CALL this%Basis_t%write()
    write(out_unit,*) this%tab_layer,'A= ',this%A
    write(out_unit,*) this%tab_layer,'B= ',this%B
    write(out_unit,*) this%tab_layer,'-------------------------------------'

  END SUBROUTINE Write_Basis_BoxAB

  SUBROUTINE Set_Grid_Basis_BoxAB(this)
    USE QDUtil_m, ONLY : Rkind, HALF, PI, out_unit

    CLASS (Basis_BoxAB_t), intent(inout) :: this

    real (kind=Rkind) :: dx
    integer :: l,i,LG,nql

    IF (allocated(this%tab_nq)) THEN

      LG = size(this%tab_nq)-1
      allocate(this%X(0:LG))
      allocate(this%W(0:LG))

      DO l=0,LG
        nql = this%get_nq(l)
        dx  = PI/nql
        this%X(l) = reshape([(dx*(-HALF+i),i=1,nql)],shape=[1,nql])
        this%W(l) = [(dx,i=1,nql)]
      END DO
    ELSE
      write(out_unit,*) 'ERROR in Set_Grid_Basis_BoxAB'
      write(out_unit,*) 'tab_nq is not allocated'
      STOP 'ERROR in Set_Grid_Basis_BoxAB: tab_nq is not allocated'
    END IF

  END SUBROUTINE Set_Grid_Basis_BoxAB

  SUBROUTINE Set_GB_Basis_BoxAB(this)
    USE QDUtil_m, ONLY : Rkind, ZERO, HALF, PI, out_unit
    USE ADdnSVM_m

    CLASS (Basis_BoxAB_t), intent(inout) :: this

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
            IF (ib == nbl .AND. nbl == nql) THEN
              dnB = dnBox(dnX, ib) * sqrt(HALF)
            ELSE
              dnB = dnBox(dnX, ib)
            END IF
            CALL dnS_TO_dnMat(dnB,this%GB(l),i=iq,j=ib)
          END DO

        END DO
      END DO
    ELSE
      write(out_unit,*) 'ERROR in Set_GB_Basis_BoxAB'
      write(out_unit,*) 'tab_nq or tab_nb are not allocated'
      STOP 'ERROR in Set_GB_Basis_BoxAB: tab_nq or tab_nb are not allocated'
    END IF

  END SUBROUTINE Set_GB_Basis_BoxAB
END MODULE Basis_BoxAB_m