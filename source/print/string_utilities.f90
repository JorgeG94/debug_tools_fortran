module string_utilities
   use types_module
   implicit none
   ! Generic interface for to_string to handle different types
   interface to_string
      module procedure to_string_int32
      module procedure to_string_int64
      module procedure to_string_dp
      module procedure to_string_char
      module procedure to_string_logical
   end interface

   interface operator(+)
      module procedure concatenate_strings
   end interface

contains

   function concatenate_strings(str1, str2) result(concatenated)
      character(len=*), intent(in) :: str1, str2
      character(len=len(str1) + len(str2)) :: concatenated
      concatenated = str1 // str2
      concatenated = trim(concatenated)
   end function concatenate_strings

   ! Overloaded to_string function for integer
   function to_string_int32(i) result(str)
      integer(kind=int32), intent(in) :: i
      character(len=50) :: str
      write(str, '(I0)') i  ! Convert integer to string without leading spaces
      str = trim(str)
   end function to_string_int32
   function to_string_int64(i) result(str)
      integer(kind=int64), intent(in) :: i
      character(len=50) :: str
      write(str, '(I0)') i  ! Convert integer to string without leading spaces
      str = trim(str)
   end function to_string_int64

   ! Overloaded to_string function for real
   function to_string_dp(r) result(str)
      double precision, intent(in) :: r
      character(len=50) :: str
      write(str, '(F0.12)') r  ! Convert real to string with 3 decimal places
      str = trim(str)
   end function to_string_dp

   ! Overloaded to_string function for character
   function to_string_char(c) result(str)
      character(len=*), intent(in) :: c
      character(len=500) :: str
      str = c
      str = trim(str)
   end function to_string_char

   function to_string_logical(l) result(str)
      logical, intent(in) :: l
      character(len=5) :: str
      if (l) then
         str = 'TRUE'
      else
         str = 'FALSE'
      end if
      str = trim(str)
   end function to_string_logical


end module string_utilities
