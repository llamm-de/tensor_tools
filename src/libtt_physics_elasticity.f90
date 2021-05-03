!> Module for special tensors used in pyhsical simulations (e.g. solid mechanics)
module libtt_physics_elasticity

    use libtt_precision
    use libtt_common,   only: eye, eye_4, inverse, det, trace
    use libtt_products, only: dyad
    
    implicit none
    private

    real(kind=dp), dimension(5), parameter :: ab_coefficients = [1/2.d0, 1/20.d0, 11/1050.d0, 19/7000.d0, 519/673750.d0]

    public :: getRightCauchyGreen
    public :: getLeftCauchyGreen
    public :: getGreenLagrange
    public :: getNeoHooke_stress
    public :: getNeoHooke_tangent
    public :: getArrudaBoyce_stress
    public :: getArrudaBoyce_energy
    public :: getDerivativeInvRCG
    public :: getStVenant_stress
    public :: getStVenant_tangent
    public :: getAnisotropicReese_stress
    public :: getAnisotropicReese_tangent
    
    
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
    !! @param  rightCauchyGreen Right Cauchy Green strain tensor
    !! @return res              Green Lagrange strain tensor
    function getGreenLagrange(rightCauchyGreen) result(res)

        real(kind=dp), dimension(3,3), intent(in) :: rightCauchyGreen   
        real(kind=dp), dimension(3,3)             :: res

        res = 0.5d0*(RightCauchyGreen - eye())

    end function getGreenLagrange


    !> Neo Hookean material model (stress response)
    !! Based on the formulation of the strain energy function as:
    !! psi = mu/2 * (trace(C) - 3) - mu * ln(J) + lambda/4 *(J^2 - 1 - 2*ln(J))
    !!
    !! @param  rightCauchyGreen Right Cauchy Green strain tensor
    !! @param  mu               Material Parameter (Lame mu)
    !! @param  lambda           Material Parameter (Lame lambda)
    !! @return res              2nd Piola Kirchhoff stress tensor
    function getNeoHooke_stress(rightCauchyGreen, mu, lambda) result(res)
        real(kind=dp), dimension(3,3), intent(in) :: rightCauchyGreen
        real(kind=dp)                , intent(in) :: mu
        real(kind=dp)                , intent(in) :: lambda
        real(kind=dp), dimension(3,3)             :: res
        real(kind=dp), dimension(3,3)             :: invRCG
        real(kind=dp)                             :: J

        invRCG = inverse(rightCauchyGreen)
        J      = sqrt(det(rightCauchyGreen))

        res = mu * (eye() - invRCG) + lambda/2.0d0 * (J**2 - 1)*invRCG

    end function getNeoHooke_stress

    !> Neo Hookean material model (material tangent modulus)
    !! Based on the formulation of the strain energy function as:
    !! psi = mu/2 * (trace(C) - 3) - mu * ln(J) + lambda/4 *(J^2 - 1 - 2*ln(J))
    !!
    !! @param  rightCauchyGreen Right Cauchy Green strain tensor
    !! @param  mu               Material Parameter (Lame mu)
    !! @param  lambda           Material Parameter (Lame lambda)
    !! @return res              4th order material tangent modulus (ref. config.)
    pure function getNeoHooke_tangent(rightCauchyGreen, mu, lambda) result(res)
        real(kind=dp), dimension(3,3), intent(in) :: rightCauchyGreen
        real(kind=dp)                , intent(in) :: mu
        real(kind=dp)                , intent(in) :: lambda
        real(kind=dp), dimension(3,3,3,3)         :: res
        real(kind=dp), dimension(3,3,3,3)         :: derivativeInvRCG
        real(kind=dp), dimension(3,3)             :: invRCG
        real(kind=dp)                             :: J

        invRCG           = inverse(rightCauchyGreen)
        J                = sqrt(det(rightCauchyGreen))
        derivativeInvRCG = getDerivativeInvRCG(invRCG)

        res = 2.0d0 * (lambda/2.0d0  * (J**2 - 1) - mu) * derivativeInvRCG + &
              lambda * J**2 * dyad(invRCG)

    end function getNeoHooke_tangent


    !> Arruda Boyce material model (material tangent modulus)
    !! Based on the formulation of the strain energy function as:
    !! psi = mu * SUM_k ( C_k / N^(k-1) * (trace(C)^k - 3^k)) - mu * ln(J) + lambda/4 *(J^2 - 1 - 2*ln(J))
    !!
    !! @param  rightCauchyGreen Right Cauchy Green strain tensor
    !! @param  mu               Material Parameter (Lame mu)
    !! @param  lambda           Material Parameter (Lame lambda)
    !! @param  N                Material Parameter (Chain length)
    !! @return res              Energy
    function getArrudaBoyce_energy(rightCauchyGreen, mu, lambda, N) result(res) 
        real(kind=dp), dimension(3,3), intent(in) :: rightCauchyGreen
        real(kind=dp)                , intent(in) :: mu
        real(kind=dp)                , intent(in) :: N
        real(kind=dp)                , intent(in) :: lambda
        real(kind=dp)                             :: res
        real(kind=dp)                             :: J
        real(kind=dp)                             :: ab_sum
        integer                                   :: i

        J      = sqrt(det(rightCauchyGreen))
        
        ab_sum = 0 
        do i = 1,5,1
            ab_sum = ab_sum + ab_coefficients(i)/N**(i-1) * ( trace(rightCauchyGreen)**i - 3**i ) 
        end do 
        
        res    = mu * (ab_sum - log(J))  + lambda/4.0d0 * (J**2 - 1 - 2*log(J))

    end function getArrudaBoyce_energy


    !> Arruda Boyce material model (material tangent modulus)
    !! Based on the formulation of the strain energy function as:
    !! psi = mu * SUM_k ( C_k / N^(k-1) * (trace(C)^k - 3^k)) - mu * ln(J) + lambda/4 *(J^2 - 1 - 2*ln(J))
    !!
    !! @param  rightCauchyGreen Right Cauchy Green strain tensor
    !! @param  mu               Material Parameter (Lame mu)
    !! @param  lambda           Material Parameter (Lame lambda)
    !! @param  N                Material Parameter (Chain length)
    !! @return res              2nd Piola Kirchhoff stress tensor
    function getArrudaBoyce_stress(rightCauchyGreen, mu, lambda, N) result(res) 
        real(kind=dp), dimension(3,3), intent(in) :: rightCauchyGreen
        real(kind=dp)                , intent(in) :: mu
        real(kind=dp)                , intent(in) :: N
        real(kind=dp)                , intent(in) :: lambda
        real(kind=dp), dimension(3,3)             :: res
        real(kind=dp), dimension(3,3)             :: invRCG
        real(kind=dp)                             :: J
        real(kind=dp)                             :: ab_sum
        integer                                   :: i

        invRCG = inverse(rightCauchyGreen)
        J      = sqrt(det(rightCauchyGreen))
        
        ab_sum = 0 
        do i = 1,5,1
            ab_sum = ab_sum + ab_coefficients(i)/N**(i-1) * i * trace(rightCauchyGreen)**(i-1)
        end do 
        
        res    = 2 * (mu * (ab_sum * eye() - 0.5d0 * invRCG) + lambda/4.0d0 * (J**2 - 1) * invRCG)

    end function getArrudaBoyce_stress


    !> Derivative of inverse of right Cauchy Green wrt. the right Cauchy Green tensor
    !!
    !! @param  invRightCauchyGreen Inverse of right Cauchy Green strain tensor
    !! @return res                 4th order tensor
    pure function getDerivativeInvRCG(invRightCauchyGreen) result(res)
        real(kind=dp), dimension(3,3), intent(in) :: invRightCauchyGreen
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
                        res(i,j,k,l) = res(i,j,k,l) + &
                                       0.5d0 * (invRightCauchyGreen(i,k) * invRightCauchyGreen(l,j) + &
                                       invRightCauchyGreen(i,l) * invRightCauchyGreen(k,j))
                    end do
                end do
            end do
        end do

        res = -1.0d0 * res

    end function getDerivativeInvRCG

    !> St.Venant Kirchhoff material model (stress response)
    !!
    !! @param  rightCauchyGreen Right Cauchy Green strain tensor
    !! @param  mu               Material Parameter (Lame mu)
    !! @param  lambda           Material Parameter (Lame lambda)
    !! @return res              2nd Piola Kirchhoff stress tensor
    function getStVenant_stress(rightCauchyGreen, mu, lambda) result(res)
        real(kind=dp), dimension(3,3), intent(in) :: rightCauchyGreen
        real(kind=dp)                , intent(in) :: mu
        real(kind=dp)                , intent(in) :: lambda
        real(kind=dp), dimension(3,3)             :: res
        real(kind=dp), dimension(3,3)             :: greenStrain

        greenStrain = getGreenLagrange(rightCauchyGreen)

        res = lambda * trace(greenStrain) * eye() + 2 * mu * greenStrain

    end function getStVenant_stress

    !> St.Venant Kirchhoff material model (stress response)
    !!
    !! @param  rightCauchyGreen Right Cauchy Green strain tensor
    !! @param  mu               Material Parameter (Lame mu)
    !! @param  lambda           Material Parameter (Lame lambda)
    !! @return res              4th order material tangent modulus (ref. config.)
    function getStVenant_tangent(mu, lambda) result(res)
        
        use libtt_print
        real(kind=dp)                , intent(in) :: mu
        real(kind=dp)                , intent(in) :: lambda
        real(kind=dp), dimension(3,3,3,3)         :: res

        res = eye_4()
        res = 2 * mu * res
        res = res + lambda * dyad(eye())

    end function getStVenant_tangent

    !> Anisotropic material model after Reese at al. 2000 (stress response)
    !!
    !! @param  rightCauchyGreen Right Cauchy Green strain tensor
    !! @param  fibreDir         Fibre direction vector
    !! @param  k                Material Parameter (stiffness)
    !! @param  alpha            Material Parameter (shape)
    !! @return res              2nd Piola Kirchhoff stress tensor
    function getAnisotropicReese_stress(rightCauchyGreen, fibreDir, k, alpha) result(res)

        use libtt_common  , only: invariants
        use libtt_products, only: dyad, doubleContract

        real(kind=dp), dimension(3,3), intent(in) :: rightCauchyGreen
        real(kind=dp), dimension(3)  , intent(in) :: fibreDir
        real(kind=dp)                , intent(in) :: k
        integer                      , intent(in) :: alpha
        real(kind=dp), dimension(3,3)             :: res
        real(kind=dp), dimension(3,3)             :: structuralTensor
        real(kind=dp), dimension(5)               :: invars
       
        structuralTensor = dyad(fibreDir)
        invars           = invariants(rightCauchyGreen, structuralTensor)

        res = 2 * alpha * k * (invars(4) - 1)**(alpha-1) * structuralTensor

    end function getAnisotropicReese_stress

    !> Anisotropic material model after Reese at al. 2000 (tangent modulus)
    !!
    !! @param  rightCauchyGreen Right Cauchy Green strain tensor
    !! @param  fibreDir         Fibre direction vector
    !! @param  k                Material Parameter (stiffness)
    !! @param  alpha            Material Parameter (shape)
    !! @return res              4th order material tangent modulus (ref. config.)
    function getAnisotropicReese_tangent(rightCauchyGreen, fibreDir, k, alpha) result(res)

        use libtt_common,   only: invariants
        use libtt_products, only: dyad

        real(kind=dp), dimension(3,3), intent(in) :: rightCauchyGreen
        real(kind=dp), dimension(3)  , intent(in) :: fibreDir
        real(kind=dp)                , intent(in) :: k
        integer                      , intent(in) :: alpha
        real(kind=dp), dimension(3,3,3,3)         :: res
        real(kind=dp), dimension(3,3)             :: structuralTensor
        real(kind=dp), dimension(5)               :: invars

        structuralTensor = dyad(fibreDir)
        invars           = invariants(rightCauchyGreen, structuralTensor)

        res = 4 * alpha**2 * k * (invars(4) - 1)**(alpha - 2) * dyad(structuralTensor, structuralTensor)

    end function getAnisotropicReese_tangent

end module libtt_physics_elasticity
