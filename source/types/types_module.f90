MODULE types_module
  IMPLICIT NONE

  ! Define kinds for different data types
  INTEGER, PARAMETER :: int64 = SELECTED_INT_KIND(18)          ! 64-bit integer
  INTEGER, PARAMETER :: int32 = SELECTED_INT_KIND(9)           ! 32-bit integer
  INTEGER, PARAMETER :: sp = SELECTED_REAL_KIND(6, 37)         ! Single precision real (about 6 decimal places)
  INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(15, 307)       ! Double precision real (about 15 decimal places)
  INTEGER, PARAMETER :: qp = SELECTED_REAL_KIND(33, 4931)      ! Quad precision real (about 33 decimal places)

  ! Define default types
  INTEGER, PARAMETER :: default_int = int64                    ! Default integer type
  INTEGER, PARAMETER :: default_real = dp                      ! Default real type

  ! Define type aliases for convenience
  TYPE, PUBLIC :: RealType
     REAL(KIND=default_real) :: value
  END TYPE RealType

  TYPE, PUBLIC :: IntType
     INTEGER(KIND=default_int) :: value
  END TYPE IntType

  TYPE, PUBLIC :: LogicalType
     LOGICAL :: value
  END TYPE LogicalType

END MODULE types_module
