module test_math_utilities
  use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed
  use, intrinsic :: iso_fortran_env, only: error_unit
  use solver_provider
  use math_utilities
  use types_module
  implicit none
  private

  public :: collect_math_utilities_test
  real(dp), parameter :: tol = 1.0d-14


  contains

  subroutine collect_math_utilities_test(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    integer, parameter :: ntests = 3

    allocate(testsuite(ntests))
    testsuite(1) = new_unittest("compress_lower_triangular", test_compress_lower_triangular)
    testsuite(2) = new_unittest("decompress_lower_triangular", test_decompress_lower_triangular)
    testsuite(3) = new_unittest("initialize_symmetric_matrix", test_initialize_symmetric_matrix)

  end subroutine collect_math_utilities_test

  subroutine test_compress_lower_triangular(error)
    type(error_type), allocatable, intent(out) :: error
    integer, parameter :: M = 3
    double precision, dimension(M, M) :: full_matrix
    double precision, dimension((M * (M + 1)) / 2) :: packed_matrix
    integer :: I, J, IJ

    ! Initialize full_matrix
    full_matrix = 0.0d0
    full_matrix(1, 1) = 1.0d0
    full_matrix(2, 1) = 2.0d0
    full_matrix(2, 2) = 3.0d0
    full_matrix(3, 1) = 4.0d0
    full_matrix(3, 2) = 5.0d0
    full_matrix(3, 3) = 6.0d0

    ! Call compress_lower_triangular
    call compress_lower_triangular(full_matrix, packed_matrix, M)

    ! Check packed_matrix
    IJ = 0
    do J = 1, M
      do I = 1, J
        IJ = IJ + 1
        call check(error, abs(packed_matrix(IJ) - full_matrix(I, J)) .lt. tol)
        if (allocated(error)) return
      end do
    end do

  end subroutine test_compress_lower_triangular

  subroutine test_decompress_lower_triangular(error)
    type(error_type), allocatable, intent(out) :: error
    integer, parameter :: M = 3
    double precision, dimension(M, M) :: full_matrix
    double precision, dimension((M * (M + 1)) / 2) :: packed_matrix
    integer :: I, J, IJ

    ! Initialize packed_matrix
    packed_matrix = 0.0d0
    packed_matrix(1) = 1.0d0
    packed_matrix(2) = 2.0d0
    packed_matrix(3) = 3.0d0
    packed_matrix(4) = 4.0d0
    packed_matrix(5) = 5.0d0
    packed_matrix(6) = 6.0d0

    ! Call decompress_lower_triangular
    call decompress_lower_triangular(packed_matrix, full_matrix, M)

    ! Check full_matrix
    IJ = 0
    do J = 1, M
      do I = 1, J
        IJ = IJ + 1
        call check(error, abs(full_matrix(I, J) - packed_matrix(IJ)) .lt. tol)
        if (allocated(error)) return
        call check(error, abs(full_matrix(J, I) - packed_matrix(IJ)) .lt. tol)
        if (allocated(error)) return
      end do
    end do

  end subroutine test_decompress_lower_triangular

  subroutine test_initialize_symmetric_matrix(error)
    type(error_type), allocatable, intent(out) :: error
    integer, parameter :: N = 3
    double precision, dimension(N, N) :: A
    integer :: i, j

    ! Call initialize_symmetric_matrix
    call initialize_symmetric_matrix(A, N)

    ! Check A
    do j = 1, N
      do i = j, N
        call check(error, A(i, j) >= 0.0d0 .and. A(i, j) <= 1.0d0)
        if (allocated(error)) return
        if (i /= j) then
          call check(error, abs(A(j, i) - A(i, j)) .lt. tol)
          if (allocated(error)) return
        end if
      end do
    end do

  end subroutine test_initialize_symmetric_matrix

end module test_math_utilities
