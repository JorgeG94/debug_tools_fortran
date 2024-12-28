PROGRAM compare_tftri
   use pic_matrix_printer
   use pic_blas_provider
   use pic_message_printer
   IMPLICIT NONE

!  INTEGER, PARAMETER :: M = 500, N = 500, LDT = M
   integer(kind=int64) :: m, n, ldt, n_loops
   CHARACTER(LEN=10) :: arg1, arg2, arg3
   !integer :: n_loops = 100
   INTEGER(kind=int64) :: start_clock, end_clock, clock_rate
   DOUBLE PRECISION :: time_original, time_optimized, time_dgemm
   DOUBLE PRECISION, ALLOCATABLE :: H_original(:), H_optimized(:), H_dgemm(:)
   DOUBLE PRECISION, ALLOCATABLE :: F(:), T(:,:), WRK(:,:)

   INTEGER(kind=int64) :: i, packed_size, ierror

   ! Get M from the command line argument 1
    CALL get_command_argument(1, arg1)
    READ(arg1, *, IOSTAT=ierror) M
    IF (ierror /= 0) THEN
       PRINT *, "Error: First argument (M) must be an integer."
       STOP
    END IF
   ! Get N from the command line argument 2
   CALL get_command_argument(2, arg2)
   READ(arg2, *, IOSTAT=ierror) N
   IF (ierror /= 0) THEN
      PRINT *, "Error: Second argument (N) must be an integer."
      STOP
   END IF
   ! Set LDT based on M
   LDT = M
   call get_command_argument(3,arg3)
   read(arg3, *, iostat=ierror) n_loops
   IF (ierror /= 0) THEN
      PRINT *, "Error: Third argument (N) must be an integer."
      STOP
   END IF
   ! Determine the size of the packed lower triangular array
   packed_size = (M * (M + 1)) / 2
   ! Allocate matrices
   ALLOCATE(H_original(packed_size), H_optimized(packed_size), H_dgemm(packed_size))
   allocate(F(packed_size))
   ALLOCATE(T(LDT, M), WRK(N, M))
   call print_message("Doing TFTRI with matrices of M = " // trim(to_string(M)) // " N = " // trim(to_string(N)) &
      // " and doing it " // trim(to_string(n_loops)) // " times!" )
   ! Initialize matrices F and T with random values
   CALL random_seed()
   CALL random_number(F)
   CALL random_number(T)
   ! Get the clock rate
   CALL SYSTEM_CLOCK(COUNT_RATE = clock_rate)
   ! Time the original TFTRI
   CALL SYSTEM_CLOCK(COUNT = start_clock)
   do i = 1, n_loops
      CALL by_column_transform(H_original, F, T, WRK(:,1), M, N, LDT)  ! WRK is 1D here
   end do
   CALL SYSTEM_CLOCK(COUNT = end_clock)
   time_original = REAL(end_clock - start_clock) / REAL(clock_rate)
   ! Time the optimized TFTRI
   CALL SYSTEM_CLOCK(COUNT = start_clock)
   do i = 1, n_loops
      CALL dsym_transform(H_optimized, F, T, WRK, M, N, LDT)  ! WRK is 2D here
   end do
   CALL SYSTEM_CLOCK(COUNT = end_clock)
   time_optimized = REAL(end_clock - start_clock) / REAL(clock_rate)
   CALL SYSTEM_CLOCK(COUNT = start_clock)
   do i = 1, n_loops
      CALL dgemm_transform(H_dgemm, F, T, M, N, LDT)  ! WRK is 2D here
   end do
   CALL SYSTEM_CLOCK(COUNT = end_clock)
   time_dgemm = REAL(end_clock - start_clock) / REAL(clock_rate)
   ! Print timing results
   PRINT *, "Time taken by original TFTRI: ", time_original, " seconds"
   PRINT *, "Time taken by dsymm/dgemm TFTRI: ", time_optimized, " seconds"
   PRINT *, "Time taken by dgemm/dgemm TFTRI: ", time_dgemm, " seconds"
   ! Verify that both implementations yield similar results
   PRINT *, "Checking results for consistency..."
   DO i = 1, packed_size
      IF (ABS(H_original(i) - H_optimized(i)) > 1.0E-6) THEN
         PRINT *, "DSYM esults differ at index ", i
         EXIT
      END IF
   END DO
   PRINT *, "DSYM/DGEMM consistency check passed."
   DO i = 1, packed_size
      IF (ABS(H_original(i) - H_dgemm(i)) > 1.0E-6) THEN
         PRINT *, "DGEMM results differ at index ", i
         EXIT
      END IF
   END DO
   PRINT *, "DGEMM consistency check passed."

   ! Deallocate arrays
   DEALLOCATE(H_original, H_optimized, F, T, WRK)

END PROGRAM compare_tftri
