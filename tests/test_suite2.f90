module test_pic_string_utils
  use testdrive, only: new_unittest, unittest_type, error_type, check
  use pic_string_utils, only: to_string, operator(+)
  use pic_types, only: int32, int64, dp
  implicit none
  private

  public :: collect_pic_string_utils_tests

contains

  !> Collect all tests for pic_string_utils module
  subroutine collect_pic_string_utils_tests(testsuite2)
    type(unittest_type), allocatable, intent(out) :: testsuite2(:)
    integer, parameter :: ntests = 6
    allocate(testsuite2(ntests))
    testsuite2(1) = new_unittest("test_to_string_int32", test_to_string_int32)
    testsuite2(2) = new_unittest("test_to_string_int64", test_to_string_int64)
    testsuite2(3) = new_unittest("test_to_string_dp", test_to_string_dp)
    testsuite2(4) = new_unittest("test_to_string_char", test_to_string_char)
    testsuite2(5) = new_unittest("test_to_string_logical", test_to_string_logical)
    testsuite2(6) = new_unittest("test_concatenate_strings", test_concatenate_strings)

  end subroutine collect_pic_string_utils_tests

  !> Test: to_string for int32
  subroutine test_to_string_int32(error)
    type(error_type), allocatable, intent(out) :: error
    character(len=50) :: result

    result = to_string(int(123, kind=int32))
    call check(error, result == "123")
  end subroutine test_to_string_int32

  !> Test: to_string for int64
  subroutine test_to_string_int64(error)
    type(error_type), allocatable, intent(out) :: error
    character(len=50) :: result

    result = to_string(int(123456789012345, kind=int64))
    call check(error, result == "123456789012345")
  end subroutine test_to_string_int64

  !> Test: to_string for double precision
  subroutine test_to_string_dp(error)
    type(error_type), allocatable, intent(out) :: error
    character(len=50) :: result

    result = to_string(123.456_dp)
    call check(error, result == "123.456000000000")
  end subroutine test_to_string_dp

  !> Test: to_string for character
  subroutine test_to_string_char(error)
    type(error_type), allocatable, intent(out) :: error
    character(len=500) :: result

    result = to_string("Hello, world!")
    call check(error, result == "Hello, world!")
  end subroutine test_to_string_char

  !> Test: to_string for logical
  subroutine test_to_string_logical(error)
    type(error_type), allocatable, intent(out) :: error
    character(len=5) :: result

    result = to_string(.true.)
    call check(error, result == "TRUE")
    if (allocated(error)) return

    result = to_string(.false.)
    call check(error, result == "FALSE")
  end subroutine test_to_string_logical

  !> Test: concatenate_strings operator(+)
  subroutine test_concatenate_strings(error)
    type(error_type), allocatable, intent(out) :: error
    character(len=100) :: result

    result = "Hello, " + "world!"
    call check(error, result == "Hello, world!")
  end subroutine test_concatenate_strings

end module test_pic_string_utils
