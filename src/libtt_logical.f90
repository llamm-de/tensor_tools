!> Colloction of logical functions/subroutines
module libtt_logical

    use libtt_precision, only: dp

    implicit none
    private

    public :: isDiagonal
    
contains

    !> Check if matrix/tensor is diagonal
    !!
    !! @param A   Square matrix / tensor of 2nd order
    !! @þaram res Logical (True if A is diagonal)
    pure function isDiagonal(A) result(res)
        real(kind=dp), dimension(:,:), intent(in) :: A
        logical                                   :: res
        integer                                   :: i
        integer                                   :: j
        real(kind=dp), parameter                  :: tol = 1.0d-12

        res = .TRUE.
        do i = 2,size(A,1),1
            do j = 1,(i-1),1
                if (abs(A(i,j)) >= tol .OR. abs(A(j,i)) >= tol) then
                    res = .FALSE.
                    return
                end if
            end do
        end do

    end function isDiagonal

    
end module libtt_logical