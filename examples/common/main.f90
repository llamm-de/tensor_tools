! A short program to demonstrate the module libtt_common
program common_example
    
    ! Include packages needed for this example
    use libtt_precision, only: dp               ! Compiler independent double precision
    use libtt_print,     only: print            ! Pretty printer for tensors
    use libtt_common                            ! Common tensor operations
    
    implicit none

    ! Define some variables for this example
    real(kind=dp)                 :: scalar
    real(kind=dp), dimension(3,3) :: a
    real(kind=dp), dimension(3,3) :: b

    ! Set some random values for tensors
    call random_number(a)

    ! Kronecker delta function
    ! Returns 1 if index i equals index j and 0 else.
    scalar = kronecker(1,1)
    scalar = kronecker(2,4)

    ! Trace of 2nd order tensor
    scalar = trace(a)

    ! Determinant of 2nd order tensor
    scalar = det(a)

    ! Symmetric part of 2nd order tensor
    b = symmetric(a)

    ! Skew symmetric part of 2nd order tensor
    b = skew(a)

    ! Inverse of 2nd order tensor
    b = inverse(a)

    
end program common_example
