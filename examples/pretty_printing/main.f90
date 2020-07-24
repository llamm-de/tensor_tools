program pretty_print
    
    ! Include packages needed for this example
    use libtt_precision, only: dp               ! Compiler independent double precision
    use libtt_print,     only: print            ! Pretty printer for tensors
    
    implicit none

    ! Define some array / tensors
    real(kind=dp), dimension(3)       :: a
    real(kind=dp), dimension(3,3)     :: B
    real(kind=dp), dimension(3,3,3)   :: C
    real(kind=dp), dimension(3,3,3,3) :: D

    ! Fill tensors with random numbers
    call random_number(a)
    call random_number(B)
    call random_number(C)
    call random_number(D)

    ! Print tensors to screen
    call print(a)
    call print(B)
    call print(C)
    call print(D)
    
end program pretty_print