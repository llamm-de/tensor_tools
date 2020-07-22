!> Module for special tensors used in pyhsical simulations (e.g. solid mechanics)
module libtt_physics_elasticity

    use libtt_precision
    use libtt_common,   only: eye
    
    implicit none
    private

    public :: getRightCauchyGreen
    public :: getLeftCauchyGreen
    public :: getGreenLagrange
    
contains

    !> Right Cauchy Green tensor
    !!
    !! @param  defGrad Deformation gradient tensor
    !! @return res     Right Cauchy Green tensor 
    pure function getRightCauchyGreen(defGrad) result(res)

        real(kind=dp), dimension(3,3), intent(in) :: defGrad
        real(kind=dp), dimension(3,3) :: res

        res = matmul(transpose(defGrad), defGrad)

    end function getRightCauchyGreen

    !> Left Cauchy Green tensor
    !!
    !! @param  defGrad Deformation gradient tensor
    !! @return res     Left Cauchy Green tensor 
    pure function getLeftCauchyGreen(defGrad) result(res)

        real(kind=dp), dimension(3,3), intent(in) :: defGrad
        real(kind=dp), dimension(3,3) :: res

        res = matmul(defGrad, transpose(defGrad))

    end function getLeftCauchyGreen

    !> Green Lagrange strain tensor
    !!
    !! @param  defGrad Deformation gradient tensor
    !! @return res     Green Lagrange strain tensor
    pure function getGreenLagrange(defGrad) result(res)

        real(kind=dp), dimension(3,3), intent(in) :: defGrad
        real(kind=dp), dimension(3,3)             :: res

        res = 0.5d0*(getRightCauchyGreen(defGrad) - eye())

    end function getGreenLagrange

end module libtt_physics_elasticity
