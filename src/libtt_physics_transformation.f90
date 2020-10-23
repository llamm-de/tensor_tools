module libtt_physics_transformation

    use libtt_precision, only: dp
    
    implicit none
    private

    public :: pushForward
    
    interface pushForward
        module procedure push_stress
        module procedure push_tangent
        !module procedure push_stress_voigt
        !module procedure push_tangent_voigt
    end interface pushForward
    
contains

    !> Push forward stresses from one to the other configuration
    !!
    !! Input:
    !!          stress       : Stress tensor to be pushed forward
    !!          def_grad     : Deformation gradient tensor
    !!          quantity_in  : Character(3), defining the input quantity
    !!                         Options are:
    !!                                      'pk2' - Second Piola-Kirchhoff stress
    !!                                      'pk1' - First Piola-Kirchhoff stress
    !!                                      'sig' - Cauchy stress
    !!                                      'tau' - Kirchhoff stress
    !!          quantity_out : Character(3), defining the output quantity
    !!                         Options are:
    !!                                      'pk1' - First Piola-Kirchhoff stress
    !!                                      'sig' - Cauchy stress
    !!                                      'tau' - Kirchhoff stress
    !!
    !! Output:
    !!          stress  : Stress tensor in new configuration
    function push_stress(stress, def_grad, quantity_in, quantity_out) result(res)

        use libtt_common, only: det

        real(kind=dp),    dimension(3,3), intent(in) :: def_grad      ! Deformation gradient
        character(len=3),                 intent(in) :: quantity_in   ! Name of input stress
        character(len=3),                 intent(in) :: quantity_out  ! Name of output stress
        real(kind=dp),    dimension(3,3), intent(in) :: stress
        real(kind=dp),    dimension(3,3)             :: res
        real(kind=dp)                                :: det_f

        if (quantity_in == 'pk2' .AND. quantity_out == 'pk1') then
            ! Push from second Piola-Kirchhoff to first Piola-Kirchhoff
            res = matmul(def_grad, stress)

        else if (quantity_in == 'pk2' .AND. quantity_out == 'sig') then
            ! Push from second Piola-Kirchhoff to Cauchy
            det_f = det(def_grad)
            res   = (1/det_f) * matmul(def_grad, matmul(stress, transpose(def_grad)))

        else if (quantity_in == 'pk1' .AND. quantity_out == 'sig') then
            ! Push from first Piola-Kirchhoff to Cauchy
            det_f = det(def_grad)
            res   = (1/det_f) * matmul(stress, transpose(def_grad))

        else if (quantity_in == 'pk2' .AND. quantity_out == 'tau') then
            ! Push from second Piola-Kirchhoff to Kirchhoff
            res = matmul(def_grad, matmul(stress, transpose(def_grad)))

        else if (quantity_in == 'pk1' .AND. quantity_out == 'tau') then
            ! Push from first Piola-Kirchhoff to Kirchhoff
            res = matmul(stress, transpose(def_grad))

        else if (quantity_in == 'sig' .AND. quantity_out == 'tau') then
            ! Push from Cauchy to Kirchhoff
            det_f = det(def_grad)
            res   = det_f * stress

        else if (quantity_in == 'tau' .AND. quantity_out == 'sig') then
            ! Push from Kirchhoff to Cauchy
            det_f = det(def_grad)
            res   = (1/det_f) * stress

        else
            ! Raise undefined error!
            write(*,*) 'ERROR: Undefined push-forward sequence!'
            error stop

        end if

    end function push_stress

    !> Wrapper for push forward operation on Voigt notation stress tensors
    !!
    !! Input:
    !!          stress       : Stress tensor to be pushed forward
    !!          def_grad     : Deformation gradient tensor
    !!          quantity_in  : Character(3), defining the input quantity
    !!                         Options are:
    !!                                      'pk2' - Second Piola-Kirchhoff stress
    !!                                      'pk1' - First Piola-Kirchhoff stress
    !!                                      'sig' - Cauchy stress
    !!                                      'tau' - Kirchhoff stress
    !!          quantity_out : Character(3), defining the output quantity
    !!                         Options are:
    !!                                      'pk1' - First Piola-Kirchhoff stress
    !!                                      'sig' - Cauchy stress
    !!                                      'tau' - Kirchhoff stress
    !!
    !! Output:
    !!          stress  : Stress tensor in new configuration
    ! subroutine push_stress_voigt(stress, def_grad, quantity_in, quantity_out)

    !     use libtt_voigt, only: toTensor, toVoigt

    !     real(kind=dp),    dimension(3,3), intent(in)    :: def_grad      ! Deformation gradient
    !     character(len=3),                 intent(in)    :: quantity_in   ! Name of input stress
    !     character(len=3),                 intent(in)    :: quantity_out  ! Name of output stress
    !     real(kind=dp),    dimension(:),   intent(inout) :: stress
    !     logical                                         :: symmetry
    !     real(kind=dp),    dimension(3,3)                :: stress_tens

    !     ! Convert voigt to tensor notation
    !     stress_tens = toTensor(stress)

    !     ! Push stress
    !     call push_stress(stress_tens, def_grad, quantity_in, quantity_out)

    !     ! Convert back to voigt notation
    !     stress = toVoigt(stress_tens)

    ! end subroutine push_stress_voigt

    !> Push forward operation of tangent operator
    !!
    !! Input:
    !!          tangent  : Tangent modulus to be pushed
    !!          def_grad : Deformation gradient tensor 
    !!
    !! Output:
    !!          tangent  : Tangent modulus in new configuration
    !!
    function push_tangent(tangent, def_grad) result(res)

        use libtt_common, only: det

        real(kind=dp), dimension(3,3),     intent(in) :: def_grad
        real(kind=dp), dimension(3,3,3,3), intent(in) :: tangent
        real(kind=dp), dimension(3,3,3,3)             :: res
        integer                                       :: a, b, c, d, i, j, k, l
        real(kind=dp)                                 :: det_f
        real(kind=dp), dimension(3,3,3,3)             :: tmp_tang

        ! Get qunatities neede for operation
        det_f    = det(def_grad)
        tmp_tang = 0.0d0

        ! Performe push operation
        do a = 1,3,1
            do b = 1,3,1
                do c = 1,3,1
                    do d = 1,3,1
                        do i = 1,3,1
                            do j = 1,3,1
                                do k = 1,3,1
                                    do l = 1,3,1
                                        tmp_tang(a,b,c,d) = tmp_tang(a,b,c,d) + &
                                                            def_grad(a,i) * &
                                                            def_grad(b,j) * &
                                                            def_grad(c,k) * &
                                                            def_grad(d,l) * &
                                                            tangent(i,j,k,l)
                                    end do
                                end do
                            end do
                        end do
                    end do
                end do
            end do
        end do

        res = (1/det_f) * tmp_tang

    end function push_tangent

    !> Wrapper for push forward operation on tangents given in voigt notation
    !!
    !!
    !! Input:
    !!          tangent  : Tangent modulus to be pushed
    !!          def_grad : Deformation gradient tensor 
    !!
    !! Output:
    !!          tangent : Tangent modulus in new configuration
    !!
    ! subroutine push_tangent_voigt(tangent, def_grad)

    !     real(kind=dp), dimension(3,3),    intent(in)    :: def_grad
    !     real(kind=dp), dimension(6,6),    intent(inout) :: tangent
    !     real(kind=dp), dimension(3,3,3,3)               :: tangent_tens

    !     ! Convert to tensor notation
    !     call toTensor(tangent_tens, tangent, .TRUE.)

    !     ! Performe operation
    !     call push_tangent(tangent_tens, def_grad)

    !     ! Convert back to voigt notation
    !     call toVoigt(tangent, tangent_tens, .TRUE.)

    ! end subroutine push_tangent_voigt
    
end module libtt_physics_transformation