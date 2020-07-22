!> Collection of common functions and/or subroutines for the tensortools
!! library.
module libtt_common

    use libtt_precision
    use libtt_exception, only: throw_exception

    implicit none
    private

    public :: kronecker
    public :: trace
    public :: det
    
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

end module libtt_common