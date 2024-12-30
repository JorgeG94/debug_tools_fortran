MODULE pic_types
  IMPLICIT NONE

  ! Define kinds for different data types
  INTEGER, PARAMETER :: int64 = SELECTED_INT_KIND(18)
  INTEGER, PARAMETER :: int32 = SELECTED_INT_KIND(9)
  INTEGER, PARAMETER :: sp = SELECTED_REAL_KIND(6, 37)
  INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(15, 307)
  INTEGER, PARAMETER :: qp = SELECTED_REAL_KIND(33, 4931)

  ! Define default types
  INTEGER, PARAMETER :: default_int = int32
  INTEGER, PARAMETER :: default_real = dp
  INTEGER, PARAMETER :: default_complex = dp

  TYPE, PUBLIC :: DPType
     REAL(KIND=default_real) :: value
  END TYPE DPType

  TYPE, PUBLIC :: Int8Type
     INTEGER(KIND=default_int) :: value
  END TYPE Int8Type

  TYPE, PUBLIC :: LogicalType
     LOGICAL :: value
  END TYPE LogicalType

  TYPE, PUBLIC :: ComplexDPType
     COMPLEX(KIND=default_complex) :: value
  END TYPE ComplexDPType

END MODULE pic_types
