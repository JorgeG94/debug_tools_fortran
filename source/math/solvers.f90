SUBROUTINE sym_solve(A, X, IPVT, N, LDA, JOB, IERR)
   USE omp_lib
   IMPLICIT NONE
   external dlansy
   double precision dlansy
   DOUBLE PRECISION, INTENT(INOUT) :: A(LDA, N), X(N)
   INTEGER, INTENT(OUT) :: IPVT(N)
   INTEGER, INTENT(IN) :: N, LDA, JOB
   double precision, dimension(:), allocatable :: work
   integer,dimension(:), allocatable ::iwork
   integer :: lwork
   INTEGER :: IERR
   DOUBLE PRECISION, PARAMETER :: tol = 1.0d-14
   DOUBLE PRECISION :: rcond, anorm
   INTEGER :: INFO

   IERR = 0
   INFO = 0

   lwork = -1
   allocate(work(1))
   call dsytrf('L', N, A, LDA, IPVT, work, lwork, INFO)
   lwork = int(work(1))
   deallocate(work)
   allocate(work(lwork))
   anorm = dlansy('1', 'U', N, A, LDA, work)
!      Perform the symmetric factorization of A
   CALL DSYTRF('U', N, A, LDA, IPVT, work, lwork, INFO)
   IF (INFO .NE. 0) THEN
      IERR = INFO
      PRINT *, "ERROR: DSYTRF failed with INFO =", INFO
      RETURN
   END IF
   allocate(iwork(N))
   call dsycon('U', N, A, LDA, IPVT, ANORM, rcond, work,iwork, INFO)
   IF (INFO .NE. 0) THEN
      PRINT *, "ERROR: DSYCON failed with INFO =", INFO
      IERR = INFO
      RETURN
   END IF
   deallocate(iwork)
   IF (rcond < tol) THEN
      PRINT *, "WARNING: Matrix may be ill-conditioned, rcond =",rcond
   END IF
   CALL DSYTRS('U', N, 1, A, LDA, IPVT, X, LDA, INFO)
   IF (INFO .NE. 0) THEN
      IERR = INFO
      PRINT *, "ERROR: DSYTRS failed with INFO =", INFO
      RETURN
   END IF
   RETURN
END SUBROUTINE sym_solve

SUBROUTINE gen_solve(A, X, IPVT, N, LDA, JOB, IERR)
   USE omp_lib
   IMPLICIT NONE
   DOUBLE PRECISION, INTENT(INOUT) :: A(LDA, N), X(N)
   INTEGER, INTENT(OUT) :: IPVT(N)
   INTEGER, INTENT(IN) :: N, LDA, JOB
   INTEGER :: INFO, ierr

   INFO = 0
   call DGETRF(N, N, A, LDA, IPVT, INFO)
   call DGETRS('N', N, 1, A, LDA, IPVT, X, LDA, INFO)

   RETURN
END SUBROUTINE gen_solve
