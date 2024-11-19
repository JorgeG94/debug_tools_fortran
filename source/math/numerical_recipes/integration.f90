MODULE integrator
  USE types_module, ONLY: default_real, default_int
  IMPLICIT NONE

CONTAINS

  FUNCTION simpson_integral(func, a, b, tol) RESULT(integral)
    IMPLICIT NONE
    INTERFACE
      FUNCTION func(x) RESULT(f)
        USE types_module, ONLY: default_real
        REAL(KIND=default_real), INTENT(IN) :: x
        REAL(KIND=default_real) :: f
      END FUNCTION func
    END INTERFACE

    REAL(KIND=default_real), INTENT(IN) :: a, b, tol
    REAL(KIND=default_real) :: integral
    REAL(KIND=default_real) :: c, h, s1, s2, s3

    c = (a + b) / 2.0_default_real
    h = (b - a) / 6.0_default_real

    s1 = func(a)
    s2 = func(c)
    s3 = func(b)

    integral = adaptive_simpson(func, a, b, tol, s1, s2, s3)
  END FUNCTION simpson_integral

  RECURSIVE FUNCTION adaptive_simpson(func, a, b, tol, fa, fc, fb) RESULT(integral)
    IMPLICIT NONE
    INTERFACE
      FUNCTION func(x) RESULT(f)
        USE types_module, ONLY: default_real
        REAL(KIND=default_real), INTENT(IN) :: x
        REAL(KIND=default_real) :: f
      END FUNCTION func
    END INTERFACE

    REAL(KIND=default_real), INTENT(IN) :: a, b, tol, fa, fc, fb
    REAL(KIND=default_real) :: integral
    REAL(KIND=default_real) :: c, h, s1, s2, left, right, whole, error

    c = (a + b) / 2.0_default_real
    h = (b - a) / 6.0_default_real
    s1 = func((a + c) / 2.0_default_real)
    s2 = func((c + b) / 2.0_default_real)
    whole = h * (fa + 4.0_default_real * fc + fb)
    left = h * (fa + 4.0_default_real * s1 + fc) / 2.0_default_real
    right = h * (fc + 4.0_default_real * s2 + fb) / 2.0_default_real
    error = ABS(left + right - whole)

    IF (error < 15.0_default_real * tol) THEN
      integral = left + right + (left + right - whole) / 15.0_default_real
    ELSE
      integral = adaptive_simpson(func, a, c, tol / 2.0_default_real, fa, s1, fc) + &
                 adaptive_simpson(func, c, b, tol / 2.0_default_real, fc, s2, fb)
    END IF
  END FUNCTION adaptive_simpson

END MODULE integrator
