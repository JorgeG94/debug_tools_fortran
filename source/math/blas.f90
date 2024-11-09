SUBROUTINE TFTRI_BUT_GOOD(H, F, T, WRK, M, N, LDT)
  USE omp_lib
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


SUBROUTINE TFTRI(H,F,T,WRK,M,N,LDT)
      use omp_lib
      double precision :: H(*),F(*),T(LDT,M),WRK(N)
      double precision, PARAMETER :: ZERO=0.0D+00
      double precision, PARAMETER :: ONE=1.0D+00
      double precision, PARAMETER :: SMALL=1.0D-11
      integer :: M2 
      integer :: M, N, LDT

      M2 = (M*M+M)/2


!        THE COMPUTATION HERE IS H = T-DAGGER * (F * T),
!        WITH THE -DSPMV- FIRST PRODUCING ONE COLUMN OF F*T,
!        THEN THE -DGEMV- GENERATES AN ENTIRE ROW -J- OF -H-.

      DO J = 1,M
         IJ = (J*J-J)/2
         CALL DSPMV('U',N,ONE,F,T(1,J),1,ZERO,WRK,1)
         CALL DGEMV('T',N,J,ONE,T,LDT,WRK,1,ZERO,H(IJ+1),1)
         DO I=1,J
            IF (ABS(H(IJ+I)).LT.SMALL) H(IJ+I)=ZERO
         ENDDO
      end do

RETURN
END
