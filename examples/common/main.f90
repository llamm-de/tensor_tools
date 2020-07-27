! A short program to demonstrate the module libtt_common
program common_example
    
    ! Include packages needed for this example
    use libtt_precision, only: dp               ! Compiler independent double precision
    use libtt_print,     only: print            ! Pretty printer for tensors
    use libtt_common                            ! Common tensor operations
    
    implicit none

    ! Define some variables for this example
    integer                       :: number
    real(kind=dp)                 :: scalar
    real(kind=dp), dimension(3)   :: vector
    real(kind=dp), dimension(3,3) :: a
    real(kind=dp), dimension(3,3) :: b

    ! Set some random values for tensors and print it to screen
    call random_number(a)
    write(*,'(/,A)') "Examples for libtt_common!"
    write(*,'(A)') "The following operations are performed on the tensor:"
    call print(a)
    
    ! Kronecker delta function
    ! Returns 1 if index i equals index j and 0 else.
    write(*,'(/,A,/)') "1. Example: Kronecker delta function"
    number = kronecker(1,1)
    write(*,'(A, I1)') "Kronecker delta function of i=1 and j=1: ", number 
    number = kronecker(2,4)
    write(*,'(A, I1)') "Kronecker delta function of i=2 and j=4: ", number 

    ! Identity tensor
    write(*,'(/,A,/)') "2. Example: Identity tensor"
    call print(eye())

    ! Trace of 2nd order tensor
    write(*,'(/,A,/)') "3. Example: Trace of a tensor"
    scalar = trace(a)
    write(*,'(A, F8.6)') "Trace of A: ", scalar 

    ! Determinant of 2nd order tensor
    write(*,'(/,A,/)') "4. Example: Determinant of a tensor"
    scalar = det(a)
    write(*,'(A, F8.6)') "Determinant of A: ", scalar 

    ! Symmetric part of 2nd order tensor
    write(*,'(/,A)') "5. Example: Symmetric part of a tensor"
    b = symmetric(a)
    call print(b)

    ! Skew symmetric part of 2nd order tensor
    write(*,'(/,A)') "6. Example: Skew-symmetric part of a tensor"
    b = skew(a)
    call print(b)
    
    ! Inverse of 2nd order tensor
    write(*,'(/,A)') "7. Example: Inverse of a 2nd order tensor"
    b = inverse(a)
    call print(b)

    ! Deviatoric part of a 2nd order tensor
    write(*,'(/,A)') "8. Example: Deviatoric part of a 2nd order tensor"
    b = dev(a)
    call print(b)

    ! Diag function
    write(*,'(/,A)') "9. Example: Diag function applied to 2nd order tensor"
    call print(diag(A))
    
    ! Diag function
    write(*,'(/,A)') "10. Example: Diag function applied to 1st order tensor"
    vector = (/1,2,3/)
    b = diag(vector)
    call print(b)

    write(*,'(/,A, /)') "Thats it."
end program common_example
