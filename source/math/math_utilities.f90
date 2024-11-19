module math_utilities
  implicit none
  contains
SUBROUTINE compress_lower_triangular(full_matrix, packed_matrix, M)
  IMPLICIT NONE

  ! Input/Output arguments
  INTEGER(kind=8), intent(in) :: M
  DOUBLE PRECISION, DIMENSION(M, M), INTENT(IN) :: full_matrix  ! Full symmetric matrix
  DOUBLE PRECISION, DIMENSION((M * (M + 1)) / 2), INTENT(OUT) :: packed_matrix  ! Packed output array

  ! Local variables
  INTEGER :: I, J, IJ

  ! Initialize packed index
  IJ = 0

  ! Loop over lower triangular part of full_matrix
  DO J = 1, M
     DO I = 1, J
        IJ = IJ + 1
        packed_matrix(IJ) = full_matrix(I, J)
     END DO
  END DO

END SUBROUTINE compress_lower_triangular

SUBROUTINE decompress_lower_triangular(packed_matrix, full_matrix, M)
  IMPLICIT NONE
  INTEGER(kind=8), intent(in) :: M
  integer :: I, J, IJ
  DOUBLE PRECISION, DIMENSION((M * (M + 1)) / 2) :: packed_matrix
  DOUBLE PRECISION, DIMENSION(M, M) :: full_matrix

  IJ = 0
  DO J = 1, M
     DO I = 1, J
        IJ = IJ + 1
        full_matrix(I, J) = packed_matrix(IJ)
        full_matrix(J, I) = packed_matrix(IJ)  ! Symmetric assignment
     END DO
  END DO

END SUBROUTINE decompress_lower_triangular

SUBROUTINE initialize_symmetric_matrix(A, N)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: N
  DOUBLE PRECISION, INTENT(INOUT) :: A(N,N)
  INTEGER :: i, j

  CALL random_seed()

  ! Initialize lower triangle and mirror it to the upper triangle
  DO j = 1, N
    DO i = j, N
      CALL random_number(A(i, j))
      IF (i /= j) THEN
        A(j, i) = A(i, j)
      END IF
    END DO
  END DO


END SUBROUTINE initialize_symmetric_matrix
end module math_utilities
