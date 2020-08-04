!> Module of procedures to solve eigenvalue /-vector problems
module libtt_solvers_eigenproblem
    
    use libtt_precision, only: dp

    implicit none
    private
    
    public :: powerMethod
    public :: jacobi
    
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
            error stop "ERROR in POWERMETHOD: Solution did not converge!"
        end if

        deallocate(y)

    end subroutine

    !> Jacobi method to compute eigenvalues/-vectors for symmetric tensors/matrices
    !! Results are not sorted!
    !!
    !! @param A      Symmetric NxN Matrix
    !! @param eigvec Array of eigenvectors of A
    !! @param eigval Array of eigenvalues of A
    subroutine jacobi(A, eigvec, eigval)

        use libtt_common,    only: eye, diag
        use libtt_logical,   only: isSquare, isSymmetric, isDiagonal
        use libtt_constants, only: pi

        real(kind=dp), dimension(:,:), intent(in) :: A
        real(kind=dp), dimension(:,:)             :: eigvec
        real(kind=dp), dimension(:)               :: eigval

        real(kind=dp), dimension(:,:), allocatable :: B
        integer                                    :: n
        integer                                    :: i
        real(kind=dp)                              :: pivot
        real(kind=dp)                              :: max
        real(kind=dp)                              :: theta
        real(kind=dp)                              :: factor
        real(kind=dp), dimension(:,:), allocatable :: Q
        integer,       dimension(2)                :: idx
        
        if ((.NOT.isSquare(A)) .OR. (.NOT.isSymmetric(A))) then
            error stop "ERROR in JACOBI EIGENVALUE: Matrix is not square or symmetric!"
        end if

        n = size(A, 1)
        
        allocate(Q(n,n))
        allocate(B(n,n))

        B      = A
        eigvec = eye(n)
                
        do while (.NOT. isDiagonal(B))
            
            pivot = 0.0
            idx   = 0

            ! Get maximum pivot element
            do i = 2,n,1
                max = maxval(abs(B(i, 1:(i-1))))
                if (max > pivot) then 
                    pivot  = maxval(abs(B(i, 1:(i-1))))
                    idx(2) = maxloc(abs(B(i, 1:(i-1))), 1)
                    idx(1) = i
                end if 
            end do

            ! Construct rotational matrix Q
            theta = 0.0
            if (B(idx(1), idx(1)) /= B(idx(2), idx(2))) then
                factor = 2 * B(idx(1), idx(2)) / (B(idx(2), idx(2)) - B(idx(1), idx(1)))
                theta  = 0.5 * atan(factor)
            else 
                theta  = 0.25 * pi
            end if
            Q = eye(n) 
            Q(idx(1), idx(1)) = cos(theta)
            Q(idx(1), idx(2)) = sin(theta)
            Q(idx(2), idx(2)) = cos(theta)
            Q(idx(2), idx(1)) = -sin(theta)

            ! Perform linearization step
            eigvec = matmul(eigvec, Q)
            B      = matmul(transpose(Q), matmul(B, Q))
        end do

        do i =1,n,1
            eigval(i) = B(i,i)
        end do

        ! Clean up
        deallocate(Q)
        deallocate(B)

    end subroutine jacobi

    
end module libtt_solvers_eigenproblem
