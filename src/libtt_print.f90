module libtt_print
    
    use libtt_precision, only: dp

    implicit none
    private
    
    public :: print

    interface print
        module procedure print0th
        module procedure print1st
        module procedure print2nd
        module procedure print3rd
        module procedure print4th
    end interface print

contains

    subroutine print0th(a)
        real(kind=dp), intent(in) :: a

        call print_tensorInfo(0)
        write(*,'(6X, F16.8)') a

    end subroutine print0th

    subroutine print1st(a)
        real(kind=dp), dimension(3), intent(in) :: a

        call print_tensorInfo(1)
        call print_vector(a)

    end subroutine print1st

    subroutine print2nd(a)
        real(kind=dp), dimension(3,3), intent(in) :: a

        call print_tensorInfo(2)
        call print_matrix(a)

    end subroutine print2nd

    subroutine print3rd(a)
        real(kind=dp), dimension(3,3,3), intent(in) :: a
        integer                                     :: i

        call print_tensorInfo(3)

        do i = 1,3,1
            call print_subsetinfo((/i/), 3)
            call print_matrix(a(i,:,:))
        end do

    end subroutine print3rd

    subroutine print4th(a)
        real(kind=dp), dimension(3,3,3,3), intent(in) :: a
        integer                                       :: i
        integer                                       :: j

        call print_tensorInfo(4)

        do i = 1,3,1
            do j = 1,3,1
                call print_subsetinfo((/i, j/), 4)
                call print_matrix(a(i,j,:,:))
            end do
        end do

    end subroutine print4th

    subroutine print_matrix(a)
        real(kind=dp), dimension(:,:) :: a
        integer                       :: i
        integer                       :: j
        do i = 1,size(a,1),1
            do j = 1,size(a,2),1
                if (j == 1) then
                    write(*,'(6X, F16.8)', advance="no") a(i,j)
                else if (j == size(a,2)) then 
                    write(*,'(F16.8)') a(i,j)
                else
                    write(*,'(F16.8)', advance="no") a(i,j)
                end if
            end do
        end do

    end subroutine print_matrix

    subroutine print_vector(a)
        real(kind=dp), dimension(:), intent(in) :: a
        integer                                 :: i

        do i = 1,size(a,1),1
            write(*, '(6X, F16.8)') a(i)
        end do

    end subroutine print_vector

    subroutine print_tensorInfo(rank)
        integer, intent(in) :: rank

        write(*,'(/, A, I1, A)') "Tensor of rank ", rank, ":"

    end subroutine print_tensorInfo

    subroutine print_subsetinfo(subset,rank)
        integer, dimension(:), intent(in) :: subset
        integer, intent(in)               :: rank

        select case(rank)
            case (3)
                write(*,'(/, 3X, A, I1, A)') "Dimension (", subset(1), ",:,:):"
            case (4)
                write(*,'(/, 3X, A, I1, A, I1, A)') "Dimension (", subset(1), ",", subset(2), ",:,:):"
        end select

    end subroutine print_subsetinfo

end module libtt_print