PROGRAM test_OO
  USE QDUtil_m
  USE Basis_m
  IMPLICIT NONE

  CLASS (Basis_t),  allocatable :: basis_nD


  CALL Read_Basis(basis_nD,0)
  CALL basis_nD%write()


END PROGRAM test_OO