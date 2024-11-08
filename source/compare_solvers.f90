PROGRAM compare_solvers
  USE omp_lib
  USE matrix_printer  ! Assuming you have a module to print matrices, similar to your original code.
  USE message_printer  ! Assuming you have a module to print messages.
  IMPLICIT NONE

  INTEGER, PARAMETER :: dp = KIND(1.0d0)
  INTEGER :: M, N, LDA, n_loops, INFO, i
  CHARACTER(LEN=10) :: arg1, arg2, arg3
  INTEGER :: start_clock, end_clock, clock_rate
  DOUBLE PRECISION :: time_sym_solve, time_gen_solve
  DOUBLE PRECISION, ALLOCATABLE :: A_sym(:,:), A_gen(:,:), X_sym(:), X_gen(:)
  DOUBLE PRECISION, ALLOCATABLE :: A_sym_0(:,:), X_sym_0(:)
  INTEGER, ALLOCATABLE :: IPVT_sym(:), IPVT_gen(:)

  ! Get command line arguments
  CALL get_command_argument(1, arg1)
  READ(arg1, *, IOSTAT=INFO) M
  IF (INFO /= 0) THEN
    PRINT *, "Error: First argument (M) must be an integer."
    STOP
  END IF

  CALL get_command_argument(2, arg2)
  READ(arg2, *, IOSTAT=INFO) N
  IF (INFO /= 0) THEN
    PRINT *, "Error: Second argument (N) must be an integer."
    STOP
  END IF

  CALL get_command_argument(3, arg3)
  READ(arg3, *, IOSTAT=INFO) n_loops
  IF (INFO /= 0) THEN
    PRINT *, "Error: Third argument (n_loops) must be an integer."
    STOP
  END IF

  LDA = M

  ! Allocate matrices
  ALLOCATE(A_sym(LDA, N), A_gen(LDA, N))
  ALLOCATE(X_sym(N), X_gen(N))
  ALLOCATE(A_sym_0(LDA, N))
  ALLOCATE(X_sym_0(N))
  ALLOCATE(IPVT_sym(N), IPVT_gen(N))

  ! Initialize matrices A and vectors X with random values

  call initialize_symmetric_matrix(A_sym,N)
  !call initialize_symmetric_matrix(X_sym,N)
  CALL random_number(X_sym)

  A_gen = A_sym 
  X_gen = X_sym
  A_sym_0 = A_sym
  X_sym_0 = X_sym

  ! Get the clock rate
  CALL SYSTEM_CLOCK(COUNT_RATE = clock_rate)

  ! Time the symmetric solver
  CALL SYSTEM_CLOCK(COUNT = start_clock)
  DO i = 1, n_loops
    CALL sym_solve(A_sym, X_sym, IPVT_sym, N, LDA, 0, INFO)
    A_sym = A_sym_0
    X_sym = X_sym_0
    IF (INFO /= 0) THEN
      PRINT *, "ERROR: sym_solve failed with INFO =", INFO
      STOP
    END IF
  END DO
  CALL SYSTEM_CLOCK(COUNT = end_clock)
  time_sym_solve = REAL(end_clock - start_clock) / REAL(clock_rate)

  ! Time the general solver
  CALL SYSTEM_CLOCK(COUNT = start_clock)
  DO i = 1, n_loops
    CALL gen_solve(A_gen, X_gen, IPVT_gen, N, LDA, 0, INFO)
    A_gen = A_sym_0
    X_gen = X_sym_0
    IF (INFO /= 0) THEN
      PRINT *, "ERROR: gen_solve failed with INFO =", INFO
      STOP
    END IF
  END DO
  CALL SYSTEM_CLOCK(COUNT = end_clock)
  time_gen_solve = REAL(end_clock - start_clock) / REAL(clock_rate)

  ! Print timing results
  PRINT *, "Time taken by sym_solve (DSYTRF/DSYTRS): ", time_sym_solve, " seconds"
  PRINT *, "Time taken by gen_solve (DGETRF/DGETRS): ", time_gen_solve, " seconds"

  ! Verify that both implementations yield similar results
  PRINT *, "Checking results for consistency..."
  DO i = 1, N
    IF (ABS(X_sym(i) - X_gen(i)) > 1.0E-6_dp) THEN
      PRINT *, "Results differ at index ", i, ": X_sym =", X_sym(i), " X_gen =", X_gen(i)
      STOP
    END IF
  END DO
  PRINT *, "Consistency check passed. Results are similar."

  ! Deallocate matrices
  DEALLOCATE(A_sym, A_gen, X_sym, X_gen, IPVT_sym, IPVT_gen)

END PROGRAM compare_solvers

