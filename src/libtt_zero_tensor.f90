!> Module defining zero tensors up to rank 4.
module libtt_zero_tensor

    implicit none

    !> Zero tensor interface
    !! Defines tensors with all entries zeros
    !!
    !! @param[in,out] tensor The zero tensor
    interface zero_tensor       
        module procedure zero_tensor_rank1
        module procedure zero_tensor_rank2
        module procedure zero_tensor_rank3
        module procedure zero_tensor_rank4
    end interface zero_tensor

contains

    !> Zero_tensor of rank 1
    module subroutine zero_tensor_rank1(tensor)
        real(kind=8), dimension(3) :: tensor

        tensor = (/0, 0, 0/)

    end subroutine zero_tensor_rank1
    
    !> Zero_tensor of rank 2
    module subroutine zero_tensor_rank2(tensor)
        real(kind=8), dimension(3,3) :: tensor

        tensor = reshape((/0, 0, 0, &
                        0, 0 ,0, &
                        0, 0, 0/), &
                        (/3, 3/))

    end subroutine zero_tensor_rank2

    !> Zero_tensor of rank 3
    module subroutine zero_tensor_rank3(tensor)
        real(kind=8), dimension(3,3,3) :: tensor

        tensor = reshape((/0, 0 ,0, 0, 0, 0, 0, 0, 0, &
                        0, 0 ,0, 0, 0, 0, 0, 0, 0, &
                        0, 0 ,0, 0, 0, 0, 0, 0, 0 /), &
                        (/3, 3, 3/))

    end subroutine zero_tensor_rank3

    !> Zero_tensor of rank 4
    module subroutine zero_tensor_rank4(tensor)
        real(kind=8), dimension(3,3,3,3) :: tensor

        tensor = reshape((/0, 0 ,0, 0, 0, 0, 0, 0, 0, &
                        0, 0 ,0, 0, 0, 0, 0, 0, 0, &
                        0, 0 ,0, 0, 0, 0, 0, 0, 0, &
                        0, 0 ,0, 0, 0, 0, 0, 0, 0, &
                        0, 0 ,0, 0, 0, 0, 0, 0, 0, &
                        0, 0 ,0, 0, 0, 0, 0, 0, 0, &
                        0, 0 ,0, 0, 0, 0, 0, 0, 0, &
                        0, 0 ,0, 0, 0, 0, 0, 0, 0, &
                        0, 0 ,0, 0, 0, 0, 0, 0, 0 /), &
                        (/3, 3, 3, 3/))
                        
    end subroutine zero_tensor_rank4

end module libtt_zero_tensor