module string_utilities
   implicit none
   ! Generic interface for to_string to handle different types
   interface to_string
      module procedure to_string_int
      module procedure to_string_dp
      module procedure to_string_char
   end interface
contains

   ! Overloaded to_string function for integer
   function to_string_int(i) result(str)
      integer, intent(in) :: i
      character(len=50) :: str
      write (str, '(I0)') i  ! Convert integer to string without leading spaces
   end function to_string_int

   ! Overloaded to_string function for real
   function to_string_dp(r) result(str)
      double precision, intent(in) :: r
      character(len=50) :: str
      write (str, '(F0.12)') r  ! Convert real to string with 3 decimal places
   end function to_string_dp

   ! Overloaded to_string function for character
   function to_string_char(c) result(str)
      character(len=*), intent(in) :: c
      character(len=500) :: str
      str = c
   end function to_string_char

end module string_utilities
