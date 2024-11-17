module test_integrator_suite
  use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed
  use integrator, only: simpson_integral
  use, intrinsic :: iso_fortran_env, only: error_unit
  implicit none
  private

  public :: collect_integrator_tests

contains

  !> Collect all tests for the integrator module
  subroutine collect_integrator_tests(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    testsuite = [ &
        new_unittest("test_integrate_sin", test_integrate_sin), &
        new_unittest("test_integrate_constant", test_integrate_constant), &
        new_unittest("test_sin_function", test_sin_function), &
        new_unittest("test_constant_function", test_constant_function) &
        ]
  end subroutine collect_integrator_tests

  !> Test: Integrate sin(x) from 0 to pi, expected result 2
  subroutine test_integrate_sin(error)
    type(error_type), allocatable, intent(out) :: error
    real(8), parameter :: tol = 1.0E-8_8
    real(8) :: result

    result = simpson_integral(sin_function, 0.0_8, 3.141592653589793_8, tol)

    call check(error, abs(result - 2.0_8) < tol)
    if (allocated(error)) return
  end subroutine test_integrate_sin

  !> Test: Integrate constant 1 from 0 to 1, expected result 1
  subroutine test_integrate_constant(error)
    type(error_type), allocatable, intent(out) :: error
    real(8), parameter :: tol = 1.0E-8_8
    real(8) :: result

    result = simpson_integral(constant_function, 0.0_8, 1.0_8, tol)

    call check(error, abs(result - 1.0_8) < tol)
    if (allocated(error)) return
  end subroutine test_integrate_constant

  !> Test: sin_function(x) at specific points
  subroutine test_sin_function(error)
    type(error_type), allocatable, intent(out) :: error

    call check(error, abs(sin_function(0.0_8) - 0.0_8) < 1.0E-8_8)
    if (allocated(error)) return
    call check(error, abs(sin_function(3.141592653589793_8 / 2.0_8) - 1.0_8) < 1.0E-8_8)
    if (allocated(error)) return
  end subroutine test_sin_function

  !> Test: constant_function(x) at specific points
  subroutine test_constant_function(error)
    type(error_type), allocatable, intent(out) :: error

    call check(error, abs(constant_function(0.0_8) - 1.0_8) < 1.0E-8_8)
    if (allocated(error)) return
    call check(error, abs(constant_function(10.0_8) - 1.0_8) < 1.0E-8_8)
    if (allocated(error)) return
  end subroutine test_constant_function

  !> Function to compute sin(x)
  function sin_function(x) result(f)
    real(8), intent(in) :: x
    real(8) :: f
    f = sin(x)
  end function sin_function

  !> Function to compute constant value 1
  function constant_function(x) result(f)
    real(8), intent(in) :: x
    real(8) :: f
    f = 1.0_8
  end function constant_function

end module test_integrator_suite

