!> Module of procedures to solve eigenvalue /-vector problems
module libtt_solvers_eigenproblem
    
    use libtt_precision, only: dp

    implicit none
    private
    
    public :: powerMethod
    
contains

    !> Power method to solve for largest eigenvalue-eigenvector pair
    !!
    !! @param A      NxN Matrix
    !! @param x      Startvector / Eigenvector 
    !! @param eigVal Dominant eigenvalue of A
    subroutine powerMethod(A, x, eigVal)
        real(kind=dp), dimension(:,:)            :: A
        real(kind=dp), dimension(:)              :: x
        real(kind=dp)                            :: eigVal
        real(kind=dp)                            :: tolerance
        integer                                  :: maxIter
        integer                                  :: iter
        real(kind=dp)                            :: delta
        real(kind=dp), dimension(:), allocatable :: y
        real(kind=dp)                            :: eigVal_old
        logical                                  :: flag

        tolerance = 1.0d-12
        maxIter   = 10**2

        ! Initialization
        allocate(y(size(x,1)))
        x          = x/norm2(x)
        iter       = 0
        delta      = 2*tolerance
        eigVal_old = 0.0d0

        ! Main iteration
        do while (iter <= maxIter .AND. delta >= tolerance)
            
            y = matmul(A,x)

            ! Eigenvector
            x = y/norm2(y)

            ! Eigenvalue
            eigVal = dot_product(x, matmul(A,x))

            if (iter > 0) then
                delta = abs(eigVal - eigVal_old)
            end if

            iter       = iter + 1
            eigVal_old = eigVal

        end do

        ! Error handling
        if (iter == maxIter .AND. delta >= tolerance) then
            error stop "ERROR: PowerMethod did not converge!"
        end if

        deallocate(y)

    end subroutine
    
end module libtt_solvers_eigenproblem