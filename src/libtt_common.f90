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
    !! @param[in]  i    Integer 
    !! @param[in]  j    Integer 
    !! @return     res  Result (1 or 0)
    pure function kronecker(i,j) result(res)
        integer, intent(in) :: i,j
        integer             :: res

        if (i==j) then
            res = 1
        else
            res = 0
        endif
    end function kronecker

end module libtt_common