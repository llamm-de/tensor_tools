!> Collection of tensor products for tensors of different ranks.
module libtt_products

    implicit none
    
    !> Double contracting (inner) product A:B for tensors of various
    !! ranks. 
    interface double_contract
        module procedure double_contract_ranks22
        module procedure double_contract_ranks24    
        module procedure double_contract_ranks42 
    end interface double_contract

contains

    !> Double contracting product of two tensors of rank 2
    !! @param[in]  a      First tensor of product A:B
    !! @param[in]  b      Second tensor of product A:B
    !! @param[out] result Scalar result of product A:B
    subroutine double_contract_ranks22(result, a, b)
        real(kind=8)                              :: result        
        real(kind=8), dimension (3,3), intent(in) :: a, b
        
        integer :: i,j
    
        result = 0
        do i = 1,3,1
          do j = 1,3,1
            result = result + a(i,j)*b(i,j)
          end do
        end do
    end subroutine double_contract_ranks22

    !> Double contracting product for tensors of rank 2 and rank 4
    !! @param[in]  a      First tensor (rank 2) of product A:B
    !! @param[in]  b      Second tensor (rank 4) of product A:B
    !! @param[out] result Rank 2 tensor result of product A:B
    subroutine double_contract_ranks24(result, a, b)
        real(kind=8), dimension(3,3,3,3), intent(in) :: b
        real(kind=8), dimension(3,3), intent(in)     :: a
        real(kind=8), dimension(3,3)                 ::result
        
        integer :: i, j, m, n

        do m = 1,3,1
            do n = 1,3,1
                do i = 1,3,1
                    do j = 1,3,1
                        result(m,n) = result(m,n) + a(i,j)*b(i,j,m,n)
                    end do
                end do
            end do
        end do
    end subroutine double_contract_ranks24

    !> Double contracting product for tensors of rank 4 and rank 2
    !! @param[in]  a      First tensor (rank 4) of product A:B
    !! @param[in]  b      Second tensor (rank 2) of product A:B
    !! @param[out] result Rank 2 tensor result of product A:B
    subroutine double_contract_ranks42(result, a, b)
        real(kind=8), dimension(3,3,3,3), intent(in) :: a
        real(kind=8), dimension(3,3), intent(in)     :: b
        real(kind=8), dimension(3,3)                 ::result
        
        integer :: i, j, k, l

        do i = 1,3,1
            do j = 1,3,1
                do k = 1,3,1
                    do l = 1,3,1
                        result(i,j) = result(i,j) + a(i,j,k,l)*b(k,l)
                    end do
                end do
            end do
        end do
    end subroutine double_contract_ranks42

end module libtt_products