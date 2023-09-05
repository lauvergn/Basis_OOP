PROGRAM test_OO
  USE QDUtil_m
  USE BasisInput_m
  USE Basis_base_m
  USE Basis_HO_m
  USE Basis_DP_m
  IMPLICIT NONE

  TYPE (BasisInput_t) :: BaIn

  TYPE (Basis_t) :: b1
  CLASS (Basis_t), allocatable :: b2,b3
  TYPE (Basis_DP_t), allocatable :: basis_nD

  !b1 = init_Basis(nb=5,nq=6,ndim=1,name='default')
  !CALL b1%write()

  !allocate(Basis_HO_t :: b2)
  !b2 = init_Basis_HO(nb=5,nq=6,Q0=ZERO,ScQ=ONE)
  !CALL b2%write()

  !allocate(Basis_DP_t :: b3)
  !b3 = init_Basis_DP(nb_basis=2)
  !CALL b3%write()

  CALL BaIn%Read()
  CALL BaIn%Write()
  IF (BaIn%name /= 'dp') STOP 'the first basis layer MUST be a DP basis set'
  basis_nD = init_Basis_DP(nb_basis=BaIn%nb_basis)
  CALL basis_nD%write()


END PROGRAM test_OO