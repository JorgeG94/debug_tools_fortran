module test_integrator_suite
  use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed
  use integrator, only: simpson_integral
  use pic_types, only: dp
  use, intrinsic :: iso_fortran_env, only: error_unit
  implicit none
  private

  public :: collect_integrator_tests

contains

  !> Collect all tests for the integrator module
  subroutine collect_integrator_tests(testsuite3)
    type(unittest_type), allocatable, intent(out) :: testsuite3(:)
    integer, parameter :: ntests = 4
    allocate(testsuite3(ntests))
    testsuite3(1) = new_unittest("test_integrate_sin", test_integrate_sin)
    testsuite3(2) = new_unittest("test_integrate_constant", test_integrate_constant)
    testsuite3(3) = new_unittest("test_sin_function", test_sin_function)
    testsuite3(4) = new_unittest("test_constant_function", test_constant_function)

  end subroutine collect_integrator_tests

  !> Test: Integrate sin(x) from 0 to pi, expected result 2
  subroutine test_integrate_sin(error)
    type(error_type), allocatable, intent(out) :: error
    real(dp), parameter :: tol = 1.0E-8
    real(dp) :: result

    result = simpson_integral(sin_function, 0.0_dp, 3.141592653589793_dp, tol)

    call check(error, abs(result - 2.0_dp) < tol)
    if (allocated(error)) return
  end subroutine test_integrate_sin

  !> Test: Integrate constant 1 from 0 to 1, expected result 1
  subroutine test_integrate_constant(error)
    type(error_type), allocatable, intent(out) :: error
    real(dp), parameter :: tol = 1.0E-8
    real(dp) :: result

    result = simpson_integral(constant_function, 0.0_dp, 1.0_dp, tol)

    call check(error, abs(result - 1.0_dp) < tol)
    if (allocated(error)) return
  end subroutine test_integrate_constant

  !> Test: sin_function(x) at specific points
  subroutine test_sin_function(error)
    type(error_type), allocatable, intent(out) :: error
    real(dp), parameter :: tol = 1.0E-8

    call check(error, abs(sin_function(0.0_dp) - 0.0_dp) < tol)
    if (allocated(error)) return
    call check(error, abs(sin_function(3.141592653589793_dp / 2.0_dp) - 1.0_dp) < tol)
    if (allocated(error)) return
  end subroutine test_sin_function

  !> Test: constant_function(x) at specific points
  subroutine test_constant_function(error)
    type(error_type), allocatable, intent(out) :: error
    real(dp), parameter :: tol = 1.0E-8

    call check(error, abs(constant_function(0.0_dp) - 1.0_dp) < tol)
    if (allocated(error)) return
    call check(error, abs(constant_function(10.0_dp) - 1.0_dp) < tol)
    if (allocated(error)) return
  end subroutine test_constant_function

  !> Function to compute sin(x)
  function sin_function(x) result(f)
    real(dp), intent(in) :: x
    real(dp) :: f
    f = sin(x)
  end function sin_function

  !> Function to compute constant value 1
  function constant_function(x) result(f)
    real(dp), intent(in) :: x
    real(dp) :: f
    f = 1.0_dp + 0.0_dp * x
  end function constant_function

end module test_integrator_suite
