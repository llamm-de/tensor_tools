!> Collection of common functions and/or subroutines for the tensortools
!! library.
module libtt_common

    use libtt_precision

    implicit none
    private

    public :: kronecker
    public :: trace
    public :: det
    public :: eye
    public :: symmetric
    public :: skew
    public :: inverse
    public :: diag
    public :: dev
    public :: invariants

    interface diag
        module procedure getDiagElementsFromTensor
        module procedure makeDiagonalTensor
    end interface diag

    interface eye
        module procedure eye_2
        module procedure eye_4
    end interface eye

    interface invariants
        module procedure principal_invariants
        module procedure mixed_invariants
    end interface invariants

    
contains

    !> Kronecker delta function
    !! Evaluates the Kronecker delta function \f$\delta_{ij}\f$,
    !! i.e. returns 1 if input parameters i and j are equal.
    !! Returns 0 else.
    !!
    !! @param  i    Integer 
    !! @param  j    Integer 
    !! @return res  Result (1 or 0)
    pure function kronecker(i,j) result(res)
        integer, intent(in) :: i,j
        integer             :: res

        if (i==j) then
            res = 1
        else
            res = 0
        endif
    end function kronecker

    !> Trace of a tensor / matrix
    !!
    !! @param  a   Matrix tensor (rank n)
    !! @return res Trace of the matrix / tensor
    pure function trace(a) result(res)
        real(kind=dp), dimension (:,:), intent(in) :: a
        real(kind=dp)                              :: res
        integer                                    :: i

        res = 0.0d0

        do i = 1,size(a,1)
            res = res + a(i,i)
        end do

    end function trace

    !> Determinant of 2nd order tensor
    !!
    !! @param  a   Matrix tensor (rank n)
    !! @return res Trace of the matrix / tensor
    pure function det(a) result(res)
        real(kind=dp), dimension(3,3), intent(in) :: a
        real(kind=dp)                             :: res

        res = a(1,1)*a(2,2)*a(3,3) - &
              a(1,1)*a(2,3)*a(3,2) - &
              a(2,1)*a(1,2)*a(3,3) + &
              a(2,1)*a(1,3)*a(3,2) + &
              a(3,1)*a(1,2)*a(2,3) - &
              a(3,1)*a(1,3)*a(2,2)
    end function det

    !> Identity tensor of 2nd order
    !! 
    !! @return res Identity tensor of 2nd order
    function eye_2() result(res)
        real(kind=dp), dimension(3,3) :: res

        res      = 0.0d0
        res(1,1) = 1.0d0
        res(2,2) = 1.0d0
        res(3,3) = 1.0d0

    end function eye_2

    !> Identity tensor of higher rank than 4
    !! 
    !! @return res Identity tensor of higher rank
    function eye_4(rank) result(res)
        integer, intent(in)               :: rank
        real(kind=dp), dimension(3,3,3,3) :: res
        integer                           :: i
        integer                           :: j
        integer                           :: k
        integer                           :: l

        if (rank == 4) then
            do i =1,3,1
                do j = 1,3,1
                    do k = 1,3,1
                        do l = 1,3,1
                            res(i,j,k,l) = res(i,j,k,l) + &
                                           0.5*(kronecker(i,k)*kronecker(j,l) + kronecker(i,l)*kronecker(j,k))
                        end do
                    end do
                end do
            end do
        else
            write(*,*) "ERROR in function Eye(): Input of rank ", rank, " not supported yet!" 
            error stop
        end if

    end function eye_4

    !> Symmetric part of 2nd order tensor
    !! 
    !! @param  A   Tensor of second order
    !! @return res Symmetric part of A
    pure function symmetric(A) result(res)
        real(kind=dp), dimension(3,3), intent(in) :: A 
        real(kind=dp), dimension(3,3)             :: res

        res = 0.5d0 * (A + transpose(A))

    end function symmetric

    !> Scew symmetric part of 2nd order tensor
    !! 
    !! @param  A   Tensor of second order
    !! @return res Scew symmetric part of A
    pure function skew(A) result(res)
        real(kind=dp), dimension(3,3), intent(in) :: A
        real(kind=dp), dimension(3,3)             :: res

        res = 0.5d0 * (A - transpose(A))

    end function skew

    !> Inverse of 2nd order tensor
    !! 
    !! @param  A   Tensor of second order
    !! @return res Inverse of A
    pure function inverse(A) result(res)
        real(kind=dp), dimension(3,3), intent(in) :: A
        real(kind=dp), dimension(3,3)             :: res

        res(1,1) = A(2,2)*A(3,3) - A(2,3)*A(3,2)
        res(2,1) = A(2,3)*A(3,1) - A(3,3)*A(2,1)
        res(3,1) = A(2,1)*A(3,2) - A(3,1)*A(2,2)
        res(1,2) = A(3,2)*A(1,3) - A(1,2)*A(3,3)
        res(2,2) = A(3,3)*A(1,1) - A(1,3)*A(3,1)
        res(3,2) = A(3,1)*A(1,2) - A(1,1)*A(3,2)
        res(1,3) = A(1,2)*A(2,3) - A(2,2)*A(1,3)
        res(2,3) = A(1,3)*A(2,1) - A(2,3)*A(1,1)
        res(3,3) = A(1,1)*A(2,2) - A(2,1)*A(1,2)
        res      = 1/det(A) * res
    end function inverse

    !> Get diagonal element from tensor 2nd order
    !!
    !! @param  A   Tensor of second order
    !! @return res Array of diagonal elements
    pure function getDiagElementsFromTensor(A) result(res)
        real(kind=dp), dimension(3,3), intent(in) :: A
        real(kind=dp), dimension(3)               :: res

        res(1) = A(1,1)
        res(2) = A(2,2)
        res(3) = A(3,3)
    end function getDiagElementsFromTensor

    !> Make diagonal tensor from vector entries
    !!
    !! @param  vec Array containing diagonal entries
    !! @return res Diagonal tensor
    pure function makeDiagonalTensor(vec) result(res)
        real(kind=dp), dimension(3), intent(in) :: vec
        real(kind=dp), dimension(3,3)           :: res

        res = 0.0d0
        res(1,1) = vec(1)
        res(2,2) = vec(2)
        res(3,3) = vec(3)
    end function makeDiagonalTensor

    !> Get deviatoric part of 2nd order tensor
    !!
    !! @param  A   2nd order tensor
    !! @return res Deviatoric part of A
    function dev(A) result(res)
        real(kind=dp), dimension(3,3), intent(in)  :: A
        real(kind=dp), dimension(3,3)              :: res

        res = A - trace(A)/3 * eye()

    end function dev

    !> Get principal invariants of 2nd order tensor
    !!
    !! @param  A   Second order tensor
    !! @return res Array of invariants
    pure function principal_invariants(A) result(res)
        real(kind=dp), dimension(3,3), intent(in) :: A
        real(kind=dp), dimension(3)               :: res

        res(1) = trace(A)
        res(2) = 0.5 * (trace(A)**2 - trace(matmul(A,A)))
        res(3) = det(A)

    end function principal_invariants

    !> Get mixed invariants of two 2nd order tensor
    !!
    !! @param  A   Second order tensor
    !! @param  B   Second order tensor
    !! @return res Array of mixed invariants
    pure function mixed_invariants(A,B) result(res)

        use libtt_products, only: doubleContract

        real(kind=dp), dimension(3,3), intent(in) :: A
        real(kind=dp), dimension(3,3), intent(in) :: B
        real(kind=dp), dimension(5)               :: res

        res(1:3) = principal_invariants(A)
        res(4)   = doubleContract(B, A)
        res(5)   = doubleContract(B, matmul(A,A))

    end function mixed_invariants

end module libtt_common

