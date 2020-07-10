!> Module for common tensors
module libtt_tensors_common

    implicit none
    private

    public :: eye

contains

    pure function eye() result(res)
        real(kind=8), dimension(3,3) :: res

        res      = 0.0d0
        res(1,1) = 1.0d0
        res(2,2) = 1.0d0
        res(3,3) = 1.0d0

    end function eye

end module libtt_tensors_common