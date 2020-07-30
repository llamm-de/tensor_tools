!> Collection of tensor products for tensors of different ranks.
module libtt_products

    use libtt_precision
    
    implicit none
    private

    public :: doubleContract
    public :: operator(.ddot.)
    public :: dyad
    public :: operator(.dyad.)
    
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

    !> Dyadic product of tensors A and B
    interface operator(.dyad.)
        module procedure dyadic_ranks11
        module procedure dyadic_ranks11_self
        module procedure dyadic_ranks12
        module procedure dyadic_ranks21
        module procedure dyadic_ranks22
        module procedure dyadic_ranks22_self
    end interface 
    
    !> Dyadic product of tensors A and B
    interface dyad
        module procedure dyadic_ranks11
        module procedure dyadic_ranks11_self
        module procedure dyadic_ranks12
        module procedure dyadic_ranks21
        module procedure dyadic_ranks22
        module procedure dyadic_ranks22_self
    end interface dyad

contains

    !> Double contracting product of two tensors of rank 2
    !! @param[in]  a      First tensor of product A:B
    !! @param[in]  b      Second tensor of product A:B
    !! @param[out] result Scalar result of product A:B
    pure function double_contract_ranks22(a, b) result(res)
        real(kind=dp)                              :: res        
        real(kind=dp), dimension (3,3), intent(in) :: a, b
        
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
        real(kind=dp), dimension(3,3,3,3), intent(in) :: b
        real(kind=dp), dimension(3,3), intent(in)     :: a
        real(kind=dp), dimension(3,3)                 ::res
        
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
        real(kind=dp), dimension(3,3,3,3), intent(in) :: a
        real(kind=dp), dimension(3,3), intent(in)     :: b
        real(kind=dp), dimension(3,3)                 :: res
        
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

    !> Dyadic product of tensors of rank 1
    !! @param a    Rank 1 tensor
    !! @param b    Rank 1 tensor
    !! @return res Rank 2 tensor
    pure function dyadic_ranks11(a, b) result(res)
        real(kind=dp), dimension(3), intent(in) :: a
        real(kind=dp), dimension(3), intent(in) :: b
        real(kind=dp), dimension(3,3)           :: res
        integer                                 :: i
        integer                                 :: j

        res = 0.0d0

        do i = 1,3,1
            do j = 1,3,1
                res(i,j) = res(i,j) + a(i)*b(j)
            end do
        end do

    end function dyadic_ranks11

    !> Dyadic product of tensor of rank 1 with itself
    !! @param a    Rank 1 tensor
    !! @return res Rank 2 tensor
    pure function dyadic_ranks11_self(a) result(res)
        real(kind=dp), dimension(3), intent(in) :: a
        real(kind=dp), dimension(3,3)           :: res
        
        res = dyadic_ranks11(a,a)

    end function dyadic_ranks11_self

    !> Dyadic product of tensors of rank 1 and 2
    !! @param a    Rank 1 tensor
    !! @param b    Rank 2 tensor
    !! @return res Rank 3 tensor
    pure function dyadic_ranks12(a, b) result(res)
        real(kind=dp), dimension(3), intent(in)   :: a
        real(kind=dp), dimension(3,3), intent(in) :: b
        real(kind=dp), dimension(3,3,3)           :: res
        integer                                   :: i
        integer                                   :: j
        integer                                   :: k

        res = 0.0d0

        do i = 1,3,1
            do j = 1,3,1
                do k = 1,3,1
                    res(i,j,k) = res(i,j,k) + a(i)*b(j,k)
                end do
            end do
        end do

    end function dyadic_ranks12

    !> Dyadic product of tensors of rank 2 and 1
    !! @param a    Rank 2 tensor
    !! @param b    Rank 1 tensor
    !! @return res Rank 3 tensor
    pure function dyadic_ranks21(a, b) result(res)
        real(kind=dp), dimension(3,3), intent(in) :: a
        real(kind=dp), dimension(3), intent(in)   :: b
        real(kind=dp), dimension(3,3,3)           :: res
        integer                                   :: i
        integer                                   :: j
        integer                                   :: k

        res = 0.0d0

        do i = 1,3,1
            do j = 1,3,1
                do k = 1,3,1
                    res(i,j,k) = res(i,j,k) + a(i,j)*b(k)
                end do
            end do
        end do

    end function dyadic_ranks21

    !> Dyadic product of tensors of rank 2
    !! @param a    Rank 2 tensor
    !! @param b    Rank 2 tensor
    !! @return res Rank 4 tensor
    pure function dyadic_ranks22(a, b) result(res)
        real(kind=dp), dimension(3,3), intent(in) :: a
        real(kind=dp), dimension(3,3), intent(in) :: b
        real(kind=dp), dimension(3,3,3,3)         :: res
        integer                                   :: i
        integer                                   :: j
        integer                                   :: k
        integer                                   :: l

        res = 0.0d0

        do i = 1,3,1
            do j = 1,3,1
                do k = 1,3,1
                    do l = 1,3,1
                        res(i,j,k,l) = res(i,j,k,l) + a(i,j)*b(k,l)
                    end do
                end do
            end do
        end do

    end function dyadic_ranks22

    !> Dyadic product of tensor of rank 2 with itself
    !! @param a    Rank 2 tensor
    !! @return res Rank 4 tensor
    pure function dyadic_ranks22_self(a) result(res)
        real(kind=dp), dimension(3,3), intent(in) :: a
        real(kind=dp), dimension(3,3,3,3)         :: res

        res = dyadic_ranks22(a,a)

    end function dyadic_ranks22_self

end module libtt_products