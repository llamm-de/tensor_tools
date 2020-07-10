!> Module for common tensors
module libtt_tensors_common
    
    use libtt_common, only: det

    implicit none
    private

    public :: eye
    public :: symmetric
    public :: scew
    public :: inverse

contains

    !> Identity tensor of 2nd order
    !! 
    !! @return res Identity tensor of 2nd order
    pure function eye() result(res)
        real(kind=8), dimension(3,3) :: res

        res      = 0.0d0
        res(1,1) = 1.0d0
        res(2,2) = 1.0d0
        res(3,3) = 1.0d0

    end function eye

    !> Symmetric part of 2nd order tensor
    !! 
    !! @param  A   Tensor of second order
    !! @return res Symmetric part of A
    pure function symmetric(A) result(res)
        real(kind=8), dimension(3,3), intent(in) :: A 
        real(kind=8), dimension(3,3)             :: res

        res = 0.5d0 * (A + transpose(A))

    end function symmetric

    !> Scew symmetric part of 2nd order tensor
    !! 
    !! @param  A   Tensor of second order
    !! @return res Scew symmetric part of A
    pure function scew(A) result(res)
        real(kind=8), dimension(3,3), intent(in) :: A
        real(kind=8), dimension(3,3)             :: res

        res = 0.5d0 * (A - transpose(A))

    end function scew

    !> Inverse of 2nd order tensor
    !! 
    !! @param  A   Tensor of second order
    !! @return res Inverse of A
    pure function inverse(A) result(res)
        real(kind=8), dimension(3,3), intent(in) :: A
        real(kind=8), dimension(3,3)             :: res

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

end module libtt_tensors_common