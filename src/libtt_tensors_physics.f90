!> Module for special tensors used in pyhsical simulations (e.g. solid mechanics)
module libtt_tensors_physics

    implicit none
    private

    public :: getRightCauchyGreen
    public :: getLeftCauchyGreen
    
contains

    !> Right Cauchy Green tensor
    !!
    !! @param  defGrad Deformation gradient tensor
    !! @return res     Right Cauchy Green tensor 
    pure function getRightCauchyGreen(defGrad) result(res)

        real(kind=8), dimension(3,3), intent(in) :: defGrad
        real(kind=8), dimension(3,3) :: res

        res = matmul(transpose(defGrad), defGrad)

    end function getRightCauchyGreen

    !> Left Cauchy Green tensor
    !!
    !! @param  defGrad Deformation gradient tensor
    !! @return res     Left Cauchy Green tensor 
    pure function getLeftCauchyGreen(defGrad) result(res)

        real(kind=8), dimension(3,3), intent(in) :: defGrad
        real(kind=8), dimension(3,3) :: res

        res = matmul(defGrad, transpose(defGrad))

    end function getLeftCauchyGreen

end module libtt_tensors_physics
