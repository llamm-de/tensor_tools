!> Solver for linear system of equations using Gauss elimination
module libtt_solvers_gauss
    
    use libtt_precision, only: dp

    implicit none
    private 

    public :: gauss 

    !> Interface for Gaussian elimination algorithm with partial pivoting
    interface gauss
        module procedure gauss_linSys
        module procedure gauss_matrix
    end interface gauss

    !> Interface to forward elimination in gaussian algorithm
    interface eliminateForward
        module procedure eliminateForward_linSys
        module procedure eliminateForward_matrix
    end interface eliminateForward

    !> Interface to row switching function for vectors and matrices
    interface switchRow
        module procedure switchRow_rank1
        module procedure switchRow_rank2
    end interface switchRow

contains

    !> Gaussian elimination to solve nxn linear system of equations A*x=b
    !! This routine return an upper triangular matrix as a and the solution
    !! vector as b.
    !!
    !! @param a System matrix
    !! @param b Right hand side vector
    subroutine gauss_linSys(a,b)
        real(kind=dp), dimension(:,:), intent(inout) :: a   ! System matrix
        real(kind=dp), dimension(:),   intent(inout) :: b   ! Right hand side vector

        call eliminateForward_linSys(a,b)
        call substituteBackward(a,b)

    end subroutine gauss_linSys

    !> Gaussian elimination for nxn matrix
    !! This routine return an upper triangular matrix as a.
    !!
    !! @param a System matrix
    subroutine gauss_matrix(a)
        real(kind=dp), dimension(:,:), intent(inout) :: a   ! System matrix

        call eliminateForward_matrix(a)

    end subroutine gauss_matrix
    
    !> Backward substitution to solve system of equations
    !!
    !! @param a System matrix
    !! @param b Right-hand side
    subroutine substituteBackward(a, b)
        real(kind=dp), dimension(:,:), intent(inout) :: a   ! System matrix
        real(kind=dp), dimension(:),   intent(inout) :: b   ! Right hand side vector
        integer                                      :: n   ! Number of rows/columns
        integer                                      :: i   ! Row iterator

        n = size(a,1)

        b(n) = b(n)/a(n,n)
        do i = (n-1),1,-1
            b(i) = (b(i) - dot_product(a(i,(i+1):n),b((i+1):n))) / a(i,i)
        end do

    end subroutine substituteBackward

    !> Forward elimination for linear system using scaled partial pivoting
    !!
    !! @param a System matrix
    !! @param b Right-hand side
    subroutine eliminateForward_linSys(a, b)
        real(kind=dp), dimension(:,:), intent(inout) :: a      ! System matrix
        real(kind=dp), dimension(:),   intent(inout) :: b      ! Right hand side vector
        integer                                      :: n      ! Number of rows/columns
        integer                                      :: i      ! Row iterator
        integer                                      :: j      ! Column iterator
        integer                                      :: index  ! Column iterator
        real(kind=dp)                                :: fac    ! Multiplication factor for elimination
        real(kind=dp), dimension(:), allocatable     :: scales ! Scaling factors

        n = size(a,1)
        allocate(scales(n))
        call getScalingFactors(a, scales)

        do j = 1,(n-1),1

            ! Search for pivot and switch rows
            index = maxloc(abs(a(j:n,j)/scales(j:n)),1)
            index = index + j - 1
            if (index /= j) then 
                call switchRow(a, (/j, index/))
                call switchRow(b, (/j, index/))
                call switchRow(scales, (/j, index/))
            end if

            call check_zero_pivot(a, j)

            do i = (j+1),n,1
                fac    = a(i,j)/a(j,j)
                a(i,:) = a(i,:) - a(j,:)* fac
                b(i)   = b(i)   - b(j)  * fac
            end do
        end do  

        deallocate(scales)

    end subroutine eliminateForward_linSys

    !> Forward elimination for a single martrix using scaled partial pivoting
    !!
    !! @param a System matrix
    !! @param b Right-hand side
    subroutine eliminateForward_matrix(a)
        real(kind=dp), dimension(:,:), intent(inout) :: a      ! System matrix      
        integer                                      :: n      ! Number of rows/columns
        integer                                      :: i      ! Row iterator
        integer                                      :: j      ! Column iterator
        integer                                      :: index  ! Column iterator
        real(kind=dp)                                :: fac    ! Multiplication factor for elimination
        real(kind=dp), dimension(:), allocatable     :: scales ! Scaling factors

        n = size(a,1)
        allocate(scales(n))
        call getScalingFactors(a, scales)

        do j = 1,(n-1),1
            
            ! Search for pivot and switch rows
            index = maxloc(abs(a(j:n,j)/scales(j:n)),1)
            index = index + j - 1
            if (index /= j) then 
                call switchRow(a, (/j, index/))
            end if

            call check_zero_pivot(a, j)

            do i = (j+1),n,1
                fac    = a(i,j)/a(j,j)
                a(i,:) = a(i,:) - a(j,:)* fac
            end do
        end do  

        deallocate(scales)

    end subroutine eliminateForward_matrix

    !> Zero pivoting error function
    subroutine check_zero_pivot(a, j)
        real(kind=8), dimension(:,:), intent(in) :: a
        integer, intent(in)                      :: j

        if (a(j,j) == 0) then
            write(*,'(/, 3X, A, I3, A, I3, A)') "ERROR in GAUSS(): Zero pivot element at (", j, ",", j, ")!"
            write(*,'(3X, A)') "Matrix might be singular!"
            write(*,'(3X, A, /)') "Process terminated!"
            error stop
        end if

    end subroutine check_zero_pivot

    !> Scaling factors for scaled partial pivoting
    !!
    !! @param a   System matrix
    !! @param res Array of scaling factors
    subroutine getScalingFactors(a, res)
        real(kind=dp), dimension(:,:), intent(in) :: a
        real(kind=dp), dimension(:), intent(out)  :: res
        integer                                   :: i

        do i = 1,size(a,1),1
            res(i) = maxval(abs(a(i,:)),1)
        end do

    end subroutine getScalingFactors

    !> Switching of rows in vector
    !!
    !! @param a    Vector
    !! @param rows Array containing indices of rows to switch
    subroutine switchRow_rank1(a, rows)
        real(kind=dp), dimension(:), intent(inout) :: a
        integer, dimension(2), intent(in)          :: rows
        real(kind=dp)                              :: tmp

        tmp        = a(rows(1))
        a(rows(1)) = a(rows(2))
        a(rows(2)) = tmp

    end subroutine switchRow_rank1

    !> Switching of rows in matrix
    !!
    !! @param a    System matrix
    !! @param rows Array containing indices of rows to switch
    subroutine switchRow_rank2(a, rows)
        real(kind=dp), dimension(:,:), intent(inout) :: a
        integer, dimension(2), intent(in)            :: rows
        real(kind=dp), dimension(:), allocatable     :: tmp

        allocate(tmp(size(a,2)))

        tmp          = a(rows(1),:)
        a(rows(1),:) = a(rows(2),:)
        a(rows(2),:) = tmp

        deallocate(tmp)

    end subroutine switchRow_rank2

end module libtt_solvers_gauss