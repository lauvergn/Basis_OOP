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
  USE ADdnSVM_m
  IMPLICIT NONE
  PRIVATE

  TYPE Basis_t
    integer :: nb   = 0
    integer :: nq   = 0
    integer :: ndim = 0

    logical :: primitive = .FALSE.

    real (kind=Rkind), allocatable :: Q0(:)
    real (kind=Rkind), allocatable :: ScQ(:)

    character (len=:), allocatable :: name

    integer :: layer = -1
    character (len=:), allocatable :: tab_layer

    integer, allocatable :: tab_nb(:)
    integer, allocatable :: tab_nq(:)

    TYPE(dnMat_t), allocatable :: X(:)
    TYPE(dnVec_t), allocatable :: W(:)
    TYPE(dnMat_t), allocatable :: GB(:)
    TYPE(dnMat_t), allocatable :: BGW(:)
    TYPE(dnMat_t), allocatable :: GG(:)
    TYPE(dnMat_t), allocatable :: BB(:)

  CONTAINS
    PROCEDURE :: Write          => Write_Basis_base

    PROCEDURE :: Set_tab_n_OF_l => Set_tab_n_OF_l_Basis_base
    PROCEDURE :: Get_nb         => Get_nb_Basis_base
    PROCEDURE :: Get_nq         => Get_nq_Basis_base

    PROCEDURE :: Set_Grid       => Set_Grid_Basis_base

    PROCEDURE :: Set_GB         => Set_GB_Basis_base
    PROCEDURE :: Get_GB         => Get_GB_Basis_base
    PROCEDURE :: Set_BGW        => Set_BGW_Basis_base
    PROCEDURE :: Get_BGW        => Get_BGW_Basis_base

    PROCEDURE :: Set_BB         => Set_BB_Basis_base
    PROCEDURE :: Get_BB         => Get_BB_Basis_base
    PROCEDURE :: Set_GG         => Set_GG_Basis_base
    PROCEDURE :: Get_GG         => Get_GG_Basis_base

    PROCEDURE :: CheckOrtho     => CheckOrtho_Basis_base
    PROCEDURE :: Scale          => Scale_Basis_base

    PROCEDURE :: Build          => Build_Basis_base
  END TYPE Basis_t

  PUBLIC :: Basis_t,Init_Basis

  CONTAINS
  FUNCTION Init_Basis(basisIn) RESULT(this)
    USE QDUtil_m
    USE BasisInput_m
    TYPE (Basis_t)                  :: this
    TYPE (BasisInput_t), intent(in) :: basisIn

    this%nb   = basisIn%nb
    this%nq   = basisIn%nq
    this%ndim = 0
    this%name = basisIn%name

  END FUNCTION init_Basis

  SUBROUTINE Write_Basis_base(this)
    USE QDUtil_m, ONLY : Rkind, out_unit, TO_string
    USE ADdnSVM_m
    CLASS (Basis_t), intent(in) :: this

    character (len=4), parameter :: tab='    '
    integer :: i,l

    write(out_unit,*) this%tab_layer,'-------------------------------------'
    IF (allocated(this%name)) THEN
      write(out_unit,*) this%tab_layer,'name: ',this%name
    ELSE
      write(out_unit,*) this%tab_layer,'name: not initialized!'
    END IF
    write(out_unit,*) this%tab_layer,'ndim=  ',this%ndim
    write(out_unit,*) this%tab_layer,'primitive: ',this%primitive

    IF (allocated(this%tab_nb)) THEN
      write(out_unit,*) this%tab_layer,'tab_nb:    ',this%tab_nb
    ELSE
      write(out_unit,*) this%tab_layer,'tab_nb:    not allocated'
    END IF
    IF (allocated(this%tab_nq)) THEN
      write(out_unit,*) this%tab_layer,'tab_nq:    ',this%tab_nq
    ELSE
      write(out_unit,*) this%tab_layer,'tab_nq:    not allocated'
    END IF
    write(out_unit,*) this%tab_layer,'layer= ',this%layer

    IF (allocated(this%Q0) .AND. allocated(this%ScQ)) THEN
      write(out_unit,*) this%tab_layer,'Q0=  ',this%Q0
      write(out_unit,*) this%tab_layer,'ScQ= ',this%ScQ
    ELSE
      write(out_unit,*) this%tab_layer,' Q0 or ScQ are not allocated'
    END IF


    write(out_unit,*)
    IF (allocated(this%X)) THEN
      DO l=0,size(this%X)-1
        CALL Write_dnMat(this%X(l), nio=out_unit, info='X(' // TO_string(l) // ')')
      END DO
    END IF
    write(out_unit,*)
    IF (allocated(this%W)) THEN
      DO l=0,size(this%W)-1
        CALL Write_dnVec(this%W(l), nio=out_unit, info='W(' // TO_string(l) // ')')
      END DO
    END IF

    write(out_unit,*)
    IF (allocated(this%GB)) THEN
      DO l=0,size(this%GB)-1
        CALL Write_dnMat(this%GB(l), nio=out_unit, info='GB(' // TO_string(l) // ')')
      END DO
    END IF

    write(out_unit,*)
    IF (allocated(this%BGW)) THEN
      DO l=0,size(this%BGW)-1
        CALL Write_dnMat(this%BGW(l), nio=out_unit, info='BGW(' // TO_string(l) // ')')
      END DO
    END IF

    write(out_unit,*)
    IF (allocated(this%BB)) THEN
      DO l=0,size(this%BB)-1
        CALL Write_dnMat(this%BB(l), nio=out_unit, info='BB(' // TO_string(l) // ')')
      END DO
    END IF

    write(out_unit,*)
    IF (allocated(this%GG)) THEN
      DO l=0,size(this%GG)-1
        CALL Write_dnMat(this%GG(l), nio=out_unit, info='GG(' // TO_string(l) // ')')
      END DO
    END IF

    write(out_unit,*) this%tab_layer,'-------------------------------------'

  END SUBROUTINE Write_Basis_base

  SUBROUTINE Set_tab_n_OF_l_Basis_base(this,LG_in)
    !USE QDUtil_m, ONLY : out_unit

    CLASS (Basis_t), intent(inout) :: this
    integer,         intent(in)    :: LG_in

    integer :: l

    IF (LG_in > -1) THEN

      allocate(this%tab_nb(0:LG_in))
      this%tab_nb(0:LG_in) = [((l+1),l=0,LG_in)]

      allocate(this%tab_nq(0:LG_in))
      this%tab_nq(0:LG_in) = [((l+1),l=0,LG_in)]
    ELSE
      allocate(this%tab_nb(0:0))
      this%tab_nb(0) =  this%nb

      allocate(this%tab_nq(0:0))
      this%tab_nq(0) =  this%nq
    END IF

  END SUBROUTINE Set_tab_n_OF_l_Basis_base

  FUNCTION Get_nb_Basis_base(this,l) RESULT(nb)
    integer :: nb

    CLASS (Basis_t), intent(in)           :: this
    integer,         intent(in), optional :: l

    IF (allocated(this%tab_nb)) THEN
      IF (present(l)) THEN
        nb = this%tab_nb(l)
      ELSE
        nb = this%tab_nb(0)
      END IF
    ELSE
      nb = -1
    END IF

  END FUNCTION Get_nb_Basis_base

  FUNCTION Get_nq_Basis_base(this,l) RESULT(nq)
    integer :: nq

    CLASS (Basis_t), intent(inout)        :: this
    integer,         intent(in), optional :: l

    IF (allocated(this%tab_nq)) THEN
      IF (present(l)) THEN
        nq = this%tab_nq(l)
      ELSE
        nq = this%tab_nq(0)
      END IF
    ELSE
      nq = -1
    END IF

  END FUNCTION Get_nq_Basis_base

  SUBROUTINE Set_Grid_Basis_base(this)
    USE QDUtil_m, ONLY : out_unit

    CLASS (Basis_t), intent(inout) :: this

    write(out_unit,*) 'ERROR in Set_Grid_Basis_base'
    write(out_unit,*) 'grid cannot be set with Basis_base'
    STOP 'ERROR in Set_Grid_Basis_base: grid cannot be set'

  END SUBROUTINE Set_Grid_Basis_base

  SUBROUTINE Set_GB_Basis_base(this)
    USE QDUtil_m, ONLY : out_unit
    USE ADdnSVM_m

    CLASS (Basis_t), intent(inout) :: this

    write(out_unit,*) 'ERROR in Set_Grid_Basis_base'
    write(out_unit,*) 'GB cannot be set with Basis_base'
    STOP 'ERROR in Set_GB_Basis_base: GB cannot be set'

  END SUBROUTINE Set_GB_Basis_base

  FUNCTION Get_GB_Basis_base(this,ider,l) RESULT(gb)
    real(kind=Rkind), pointer :: gb(:,:)

    CLASS (Basis_t), intent(in), target   :: this
    integer,         intent(in), optional :: ider(2)
    integer,         intent(in), optional :: l

    integer :: l_loc

    l_loc = 0
    IF (present(l)) l_loc = l

    IF (allocated(this%GB)) THEN
      IF (present(ider)) THEN
        IF (ider(1) == 0 .AND. ider(2) == 0) THEN
          gb => this%GB(l_loc)%d0
        ELSE IF (ider(1) == 0 .AND. ider(2) /= 0) THEN
          gb => this%GB(l_loc)%d1(:,:,ider(2))
        ELSE IF (ider(1) /= 0 .AND. ider(2) == 0) THEN
          gb => this%GB(l_loc)%d1(:,:,ider(1))
        ELSE
          gb => this%GB(l_loc)%d2(:,:,ider(1),ider(2))
        END IF
      ELSE
        gb => this%GB(l_loc)%d0
      END IF
    ELSE
      nullify(gb)
    ENDIF

  END FUNCTION Get_GB_Basis_base

  SUBROUTINE Set_BGW_Basis_base(this)
    USE QDUtil_m, ONLY : out_unit
    USE ADdnSVM_m

    CLASS (Basis_t), intent(inout) :: this

    integer :: l,ib,LG

    IF (allocated(this%W) .AND. allocated(this%GB)) THEN
      LG = size(this%W) - 1

      allocate(this%BGW(0:LG))
      DO l=0,LG
        this%BGW(l)%d0 = transpose(this%GB(l)%d0)

        DO ib=1,size(this%GB(l)%d0,dim=2)
          this%BGW(l)%d0(ib,:) = this%BGW(l)%d0(ib,:) * this%w(l)%d0
        END DO
      END DO
    ELSE
      write(out_unit,*) 'ERROR in Set_BGW_Basis_base'
      write(out_unit,*) 'GB or W are not allocated'
      STOP 'ERROR in Set_BGW_Basis_base: GB or W are not allocated'  
    END IF

  END SUBROUTINE Set_BGW_Basis_base
  FUNCTION Get_BGW_Basis_base(this,l) RESULT(bgw)
    real(kind=Rkind), pointer :: bgw(:,:)

    CLASS (Basis_t), intent(in), target   :: this
    integer,         intent(in), optional :: l

    integer :: l_loc

    l_loc = 0
    IF (present(l)) l_loc = l

    IF (allocated(this%BGW)) THEN
      bgw => this%BGW(l_loc)%d0
    ELSE
      nullify(bgw)
    ENDIF

  END FUNCTION Get_BGW_Basis_base

  SUBROUTINE Set_BB_Basis_base(this)
    USE QDUtil_m, ONLY : Rkind, out_unit
    USE ADdnSVM_m

    CLASS (Basis_t), intent(inout) :: this

    integer           :: l,LG

    IF (allocated(this%GB) .AND. allocated(this%BGW)) THEN

      LG = size(this%GB)-1
      allocate(this%BB(0:LG))

      DO l=0,LG
        this%BB(l) = matmul(this%Get_BGW(l),this%GB(l))
      END DO
    ELSE
      write(out_unit,*) 'ERROR in Set_BB_Basis_base'
      write(out_unit,*) 'GB or BGW are not allocated'
      STOP 'ERROR in Set_BB_Basis_base: GB or BGW are not allocated'
    END IF

  END SUBROUTINE Set_BB_Basis_base
  FUNCTION Get_BB_Basis_base(this,ider,l) RESULT(bb)
    real(kind=Rkind), pointer :: bb(:,:)

    CLASS (Basis_t), intent(in), target   :: this
    integer,         intent(in), optional :: ider(2)
    integer,         intent(in), optional :: l

    integer :: l_loc

    l_loc = 0
    IF (present(l)) l_loc = l

    IF (allocated(this%BB)) THEN
      IF (present(ider)) THEN
        IF (ider(1) == 0 .AND. ider(2) == 0) THEN
          bb => this%BB(l_loc)%d0
        ELSE IF (ider(1) == 0 .AND. ider(2) /= 0) THEN
          bb => this%BB(l_loc)%d1(:,:,ider(2))
        ELSE IF (ider(1) /= 0 .AND. ider(2) == 0) THEN
          bb => this%BB(l_loc)%d1(:,:,ider(1))
        ELSE
          bb => this%BB(l_loc)%d2(:,:,ider(1),ider(2))
        END IF
      ELSE
        bb => this%BB(l_loc)%d0
      END IF
    ELSE
      nullify(bb)
    ENDIF

  END FUNCTION Get_BB_Basis_base

  SUBROUTINE Set_GG_Basis_base(this)
    USE QDUtil_m, ONLY : Rkind, out_unit
    USE ADdnSVM_m

    CLASS (Basis_t), intent(inout) :: this

    integer           :: l,LG

    IF (allocated(this%GB) .AND. allocated(this%BGW)) THEN

      LG = size(this%GB)-1
      allocate(this%GG(0:LG))

      DO l=0,LG
        this%GG(l) = matmul(this%GB(l),this%Get_BGW(l))
      END DO
    ELSE
      write(out_unit,*) 'ERROR in Set_BB_Basis_base'
      write(out_unit,*) 'GB or BGW are not allocated'
      STOP 'ERROR in Set_BB_Basis_base: GB or BGW are not allocated'
    END IF

  END SUBROUTINE Set_GG_Basis_base
  FUNCTION Get_GG_Basis_base(this,ider,l) RESULT(gg)
    real(kind=Rkind), pointer :: gg(:,:)

    CLASS (Basis_t), intent(in), target   :: this
    integer,         intent(in), optional :: ider(2)
    integer,         intent(in), optional :: l

    integer :: l_loc

    l_loc = 0
    IF (present(l)) l_loc = l

    IF (allocated(this%GG)) THEN
      IF (present(ider)) THEN
        IF (ider(1) == 0 .AND. ider(2) == 0) THEN
          gg => this%GG(l_loc)%d0
        ELSE IF (ider(1) == 0 .AND. ider(2) /= 0) THEN
          gg => this%GG(l_loc)%d1(:,:,ider(2))
        ELSE IF (ider(1) /= 0 .AND. ider(2) == 0) THEN
          gg => this%GG(l_loc)%d1(:,:,ider(1))
        ELSE
          gg => this%GG(l_loc)%d2(:,:,ider(1),ider(2))
        END IF
      ELSE
        gg => this%GG(l_loc)%d0
      END IF
    ELSE
      nullify(gg)
    ENDIF

  END FUNCTION Get_GG_Basis_base
  SUBROUTINE CheckOrtho_Basis_base(this)
    USE QDUtil_m, ONLY : Rkind, ONETENTH, out_unit, Write_Mat, TO_string, Identity_Mat
    USE ADdnSVM_m

    CLASS (Basis_t), intent(in) :: this


    real (kind=Rkind), allocatable :: SmId(:,:)
    real (kind=Rkind) :: max_err


    integer :: l,LG

    IF (allocated(this%BB)) THEN
      LG = size(this%GB) - 1

      DO l=0,LG
        SmId = this%get_BB(l=l) - Identity_Mat(this%tab_nb(l))
        max_err = maxval(abs(SmId))
        write(out_unit,*) 'At l=',TO_string(l),' Max error of S-Id',max_err
        IF (max_err > ONETENTH**6) THEN
          write(out_unit,*) 'At l=',TO_string(l),', S:'
          CALL Write_Mat(this%get_BB(l=l),nio=out_unit,nbcol=5)
        END IF
        flush(out_unit)
      END DO
    ELSE
      write(out_unit,*) 'ERROR in CheckOrtho_Basis_base'
      write(out_unit,*) 'GB or BGW are not allocated'
      STOP 'ERROR in CheckOrtho_Basis_base: GB or BGW are not allocated'  
    END IF

  END SUBROUTINE CheckOrtho_Basis_base


  SUBROUTINE Scale_Basis_base(this)
    USE QDUtil_m, ONLY : ZERO, ONE, Rkind, out_unit
    USE ADdnSVM_m

    CLASS (Basis_t), intent(inout) :: this

    real (kind=Rkind) :: ScQ
    integer           :: i,j,l,LG
    logical           :: not_scaled

    not_scaled = .TRUE.
    DO i=1,this%ndim
      not_scaled  =  not_scaled .AND. (this%Q0(i)  == ZERO .AND. this%ScQ(i) == ONE)
    END DO
    IF (not_scaled) RETURN

    ScQ = product(this%ScQ)


    IF (allocated(this%GB) .AND. allocated(this%X) .AND. allocated(this%W)) THEN

      LG = size(this%GB)-1

      DO l=0,LG
        DO i=1,this%ndim
          this%X(l)%d0(i,:) = this%Q0(i) + this%X(l)%d0(i,:) / this%ScQ(i)
        END DO
        this%W(l) =           this%W(l) / ScQ

        this%GB(l)%d0(:,:) = this%GB(l)%d0(:,:) * sqrt(ScQ)
        DO i=1,this%ndim
          this%GB(l)%d1(:,:,i) = this%GB(l)%d1(:,:,i) * sqrt(ScQ)*this%ScQ(i)
        END DO

        DO j=1,this%ndim
        DO i=1,this%ndim
          this%GB(l)%d2(:,:,i,j) = this%GB(l)%d2(:,:,i,j) * sqrt(ScQ)*this%ScQ(i)*this%ScQ(j)
        END DO
        END DO

      END DO
    ELSE
      write(out_unit,*) 'ERROR in Scale_Basis_base'
      write(out_unit,*) 'GB or X or W are not allocated'
      STOP 'ERROR in Scale_Basis_base: GB or X or W are not allocated'
    END IF

  END SUBROUTINE Scale_Basis_base
  SUBROUTINE Build_Basis_base(this)
    USE QDUtil_m, ONLY : ZERO, ONE, out_unit
    USE ADdnSVM_m

    CLASS (Basis_t), intent(inout) :: this

    integer           :: l,LG

    !CALL this%Set_tab_n_OF_l(BasisIn%LB_in,BasisIn%LG_in)
    CALL this%Set_Grid()
    CALL this%Set_GB()
    CALL this%Scale()
    CALL this%Set_BGW()
    CALL this%Set_BB()
    CALL this%Set_GG()
    CALL this%CheckOrtho()

  END SUBROUTINE Build_Basis_base
END MODULE Basis_base_m