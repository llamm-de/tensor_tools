!> Module for common tensors
module libtt_tensors_common

    implicit none
    private

    public :: eye
    public :: symmetric
    public :: scew

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

end module libtt_tensors_common