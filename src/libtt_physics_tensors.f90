module libtt_physics_tensors

    implicit none
    private

    public :: getRightCauchyGreen
    public :: getLeftCauchyGreen
    
contains

    pure function getRightCauchyGreen(defGrad) result(res)

        real(kind=8), dimension(3,3), intent(in) :: defGrad
        real(kind=8), dimension(3,3) :: res

        res = matmul(transpose(defGrad), defGrad)

    end function getRightCauchyGreen


    pure function getLeftCauchyGreen(defGrad) result(res)

        real(kind=8), dimension(3,3), intent(in) :: defGrad
        real(kind=8), dimension(3,3) :: res

        res = matmul(defGrad, transpose(defGrad))

    end function getLeftCauchyGreen

end module libtt_physics_tensors