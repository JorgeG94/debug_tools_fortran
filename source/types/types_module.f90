module pic_types
   !! main module for defining types for integer and double precision
   use iso_fortran_env
   implicit none

   ! Define kinds for different data types
   ! int32 and int64 are defined in the iso_fortran_env, if you need to change things please do so here
   !integer, parameter :: int64_custom = SELECTED_INT_KIND(18)
   !integer, parameter :: int32_custom = SELECTED_INT_KIND(9)
   integer, parameter :: sp = SELECTED_REAL_KIND(6, 37)
   integer, parameter :: dp = SELECTED_REAL_KIND(15, 307)
   integer, parameter :: qp = SELECTED_REAL_KIND(33, 4931)

   ! Define default types
   integer, parameter :: default_int = int32
    !! default integer kind, be careful if you are using fdefault-size=8
   integer, parameter :: default_real = dp
     !! naturally, our default real is double precision
   integer, parameter :: default_complex = dp
     !! default complex is double precision

end module pic_types
