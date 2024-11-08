SUBROUTINE TFTRI_BUT_GOOD(H, F, T, WRK, M, N, LDT)
  USE omp_lib
  use matrix_printer 
  IMPLICIT NONE

  ! Input/output arguments
  integer, intent(in) :: M, N, LDT
  DOUBLE PRECISION, DIMENSION(*), INTENT(OUT) :: H   ! Packed lower triangular output matrix
  DOUBLE PRECISION, DIMENSION(N), INTENT(IN) :: F  ! Symmetric matrix (lower triangular part)
  DOUBLE PRECISION, DIMENSION(LDT, M), INTENT(IN) :: T  ! Transform matrix
  DOUBLE PRECISION, DIMENSION(N, M), INTENT(INOUT) :: WRK  ! Workspace array

  ! Parameters
  DOUBLE PRECISION, PARAMETER :: ZERO = 0.0D+00
  DOUBLE PRECISION, PARAMETER :: ONE = 1.0D+00

  ! Local variables
  INTEGER :: I, J, IJ
  DOUBLE PRECISION, ALLOCATABLE :: FULL_H(:,:), full_f(:,:)

  ! Allocate a full symmetric matrix to hold the intermediate result
  ALLOCATE(FULL_H(M, M), full_f(n,n))

  call decompress_lower_triangular(F,full_F,M)

  ! Step 1: Compute G = F * T using DSYMM
  CALL DSYMM('L', 'L', N, M, ONE, full_F, N, T, LDT, ZERO, WRK, N)


  ! Step 2: Compute FULL_H = T^T * G using DGEMM
  CALL DGEMM('T', 'N', M, M, N, ONE, T, LDT, WRK, N, ZERO, FULL_H, M)

  ! Step 3: Compress FULL_H into packed lower triangular format in H
  CALL compress_lower_triangular(FULL_H, H, M)


  ! Free the allocated memory
  DEALLOCATE(FULL_H)

  RETURN
END SUBROUTINE TFTRI_BUT_GOOD

SUBROUTINE TFTRI_DGEMM(H, F, T, WRK, M, N, LDT)
  use matrix_printer 
  USE omp_lib
  IMPLICIT NONE

  ! Input/output arguments
  integer, intent(in) :: M, N, LDT
  DOUBLE PRECISION, DIMENSION(*), INTENT(OUT) :: H   ! Packed lower triangular output matrix
  DOUBLE PRECISION, DIMENSION(N), INTENT(IN) :: F  ! Symmetric matrix (lower triangular part)
  DOUBLE PRECISION, DIMENSION(LDT, M), INTENT(IN) :: T  ! Transform matrix
  DOUBLE PRECISION, DIMENSION(N, M), INTENT(INOUT) :: WRK  ! Workspace array
  DOUBLE PRECISION, ALLOCATABLE :: full_F(:,:), G(:,:)

  ! Parameters
  DOUBLE PRECISION, PARAMETER :: ZERO = 0.0D+00
  DOUBLE PRECISION, PARAMETER :: ONE = 1.0D+00

  ! Local variables
  INTEGER :: I, J, IJ
  DOUBLE PRECISION, ALLOCATABLE :: FULL_H(:,:)  ! Temporary full symmetric matrix to hold H

  ! Allocate a full symmetric matrix to hold the intermediate result
  ALLOCATE(FULL_H(M, M))
  ALLOCATE(full_F(M, M))
  ALLOCATE(G(LDT, M))

  call decompress_lower_triangular(F,full_F,M)

  ! Step 1: Compute G = F * T using DSYMM
  CALL DGEMM('N', 'N', M, N, M, 1.0D0, full_F, M, T, LDT, 0.0D0, G, M)

  ! Step 2: Compute FULL_H = T^T * G using DGEMM
  CALL DGEMM('T', 'N', M, M, N, 1.0D0, T, LDT, G, M, 0.0D0, full_H, M)

  ! Step 3: Compress FULL_H into packed lower triangular format in H
  CALL compress_lower_triangular(FULL_H, H, M)

  ! Free the allocated memory
  DEALLOCATE(FULL_H)
  deallocate(G)
  deallocate(full_F)

  RETURN
END SUBROUTINE TFTRI_DGEMM

SUBROUTINE compress_lower_triangular(full_matrix, packed_matrix, M)
  IMPLICIT NONE

  ! Input/Output arguments
  INTEGER, INTENT(IN) :: M                             ! Dimension of the matrix (M x M)
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
  INTEGER :: M, I, J, IJ
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

  write(*,*) A(1,1)

END SUBROUTINE initialize_symmetric_matrix

