program tester
  use, intrinsic :: iso_fortran_env, only: error_unit
  use testdrive, only: run_testsuite, new_testsuite, testsuite_type
  use test_suite1, only: collect_suite1
  use test_suite2, only: collect_suite2
  use test_integrator_suite, only: collect_integrator_tests
  implicit none
  integer :: stat, is
  integer, parameter :: ntest_suites = 2
  type(testsuite_type), allocatable :: testsuites(:)
  character(len=*), parameter :: fmt = '("#", *(1x, a))'

  stat = 0
  allocate (testsuites(ntest_suites))
  testsuites = [ &
               new_testsuite("suite1", collect_suite1), &
               new_testsuite("suite2", collect_suite2), &
               new_testsuite("integrator", collect_integrator_tests) &
               ]

  do is = 1, size(testsuites)
    write (error_unit, fmt) "Testing:", testsuites(is)%name
    call run_testsuite(testsuites(is)%collect, error_unit, stat)
  end do

  if (stat > 0) then
    write (error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
    error stop
  end if

end program tester
