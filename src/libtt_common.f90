!> Collection of common functions and/or subroutines for the tensortools
!! library.
module libtt_common

    implicit none

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
    !! @param a   Matrix tensor (rank n)
    !! @param res Trace of the matrix / tensor
    pure function trace(a) result(res)
        real(kind=8), dimension (:,:), intent(in) :: a
        real(kind=8)                              :: res

        integer :: i

        res = 0.0d0

        do i = 1,size(a,1)
            res = res + a(i,i)
        end do

    end function trace

end module libtt_common