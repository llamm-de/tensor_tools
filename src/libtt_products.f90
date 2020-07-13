!> Collection of tensor products for tensors of different ranks.
module libtt_products

    implicit none
    private

    public :: doubleContract
    public :: operator(.ddot.)
    
    !> Double contracting (inner) product A:B for tensors of various
    !! ranks. 
    interface operator(.ddot.)
        module procedure double_contract_ranks22
        module procedure double_contract_ranks24    
        module procedure double_contract_ranks42 
    end interface
    
    !> Double contracting (inner) product A:B for tensors of various
    !! ranks. 
    interface doubleContract
        module procedure double_contract_ranks22
        module procedure double_contract_ranks24    
        module procedure double_contract_ranks42 
    end interface doubleContract

contains

    !> Double contracting product of two tensors of rank 2
    !! @param[in]  a      First tensor of product A:B
    !! @param[in]  b      Second tensor of product A:B
    !! @param[out] result Scalar result of product A:B
    pure function double_contract_ranks22(a, b) result(res)
        real(kind=8)                              :: res        
        real(kind=8), dimension (3,3), intent(in) :: a, b
        
        integer :: i,j
    
        res = 0
        do i = 1,3,1
          do j = 1,3,1
            res = res + a(i,j)*b(i,j)
          end do
        end do
    end function double_contract_ranks22

    !> Double contracting product for tensors of rank 2 and rank 4
    !! @param[in]  a      First tensor (rank 2) of product A:B
    !! @param[in]  b      Second tensor (rank 4) of product A:B
    !! @param[out] result Rank 2 tensor result of product A:B
    pure function double_contract_ranks24(a, b) result(res)
        real(kind=8), dimension(3,3,3,3), intent(in) :: b
        real(kind=8), dimension(3,3), intent(in)     :: a
        real(kind=8), dimension(3,3)                 ::res
        
        integer :: i, j, m, n

        do m = 1,3,1
            do n = 1,3,1
                do i = 1,3,1
                    do j = 1,3,1
                        res(m,n) = res(m,n) + a(i,j)*b(i,j,m,n)
                    end do
                end do
            end do
        end do
    end function double_contract_ranks24

    !> Double contracting product for tensors of rank 4 and rank 2
    !! @param[in]  a      First tensor (rank 4) of product A:B
    !! @param[in]  b      Second tensor (rank 2) of product A:B
    !! @param[out] result Rank 2 tensor result of product A:B
    pure function double_contract_ranks42(a, b) result(res)
        real(kind=8), dimension(3,3,3,3), intent(in) :: a
        real(kind=8), dimension(3,3), intent(in)     :: b
        real(kind=8), dimension(3,3)                 :: res
        
        integer :: i, j, k, l

        do i = 1,3,1
            do j = 1,3,1
                do k = 1,3,1
                    do l = 1,3,1
                        res(i,j) = res(i,j) + a(i,j,k,l)*b(k,l)
                    end do
                end do
            end do
        end do
    end function double_contract_ranks42

end module libtt_products