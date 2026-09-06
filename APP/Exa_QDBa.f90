PROGRAM test_OO
  USE QDUtil_m
  USE Basis_m
  IMPLICIT NONE

  CLASS (Basis_t),  allocatable :: basis_nD

  CLASS (Basis_t),  allocatable :: basis2_nD

  CALL version_QDUtil(.TRUE.)
  CALL set_print_level(1)

  write(out_unit,*) "= Read Basis ==================================="
  CALL ReadInit_Basis(basis_nD,0)
  !CALL basis_nD%write()
  write(out_unit,*) "= END Read Basis ==============================="

  write(out_unit,*) "= basis2_nD=basis_nD ==========================="
  basis2_nD = basis_nD
  CALL basis2_nD%write()
  write(out_unit,*) "= END basis2_nD=basis_nD ======================="

  deallocate(basis_nD)
  IF (allocated(basis_nD)) CALL basis_nD%write()

END PROGRAM test_OO