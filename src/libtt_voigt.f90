module libtt_voigt
    
    use libtt_precision, only: dp

    implicit none
    private

    public :: toVoigt
    public :: toVoigtNonSym
    public :: toTensor
    public :: toTensorNonSym

    interface toVoigt
        module procedure toVoigt2nd
    end interface toVoigt

    interface toVoigtNonSym
        module procedure toVoigt2nd_nonSym
    end interface toVoigtNonSym

    interface toTensor
        module procedure toTensor2nd
    end interface toTensor

    interface toTensorNonSym
        module procedure toTensor2nd_nonSym
    end interface toTensorNonSym
contains

    pure function toVoigt2nd_nonSym(a) result(res)
        real(kind=dp), dimension(3,3), intent(in) :: a
        real(kind=dp), dimension(9)               :: res

        res(1) = a(1,1)
        res(2) = a(1,2)
        res(3) = a(1,3)
        res(4) = a(2,1)
        res(5) = a(2,2)
        res(6) = a(2,3)
        res(7) = a(3,1)
        res(8) = a(3,2)
        res(9) = a(3,3)
        
    end function toVoigt2nd_nonSym

    pure function toVoigt2nd(a) result(res)
        real(kind=dp), dimension(3,3), intent(in) :: a
        real(kind=dp), dimension(6)               :: res

        res(1) = a(1,1)
        res(2) = a(2,2)
        res(3) = a(3,3)
        res(4) = a(1,2)
        res(5) = a(2,3)
        res(6) = a(1,3)
                
    end function toVoigt2nd

    pure function toTensor2nd_nonSym(a) result(res)
        real(kind=dp), dimension(9), intent(in) :: a
        real(kind=dp), dimension(3,3)           :: res

        res(1,1) = a(1)
        res(1,2) = a(2)
        res(1,3) = a(3)
        res(2,1) = a(4)
        res(2,2) = a(5)
        res(2,3) = a(6)
        res(3,1) = a(7)
        res(3,2) = a(8)
        res(3,3) = a(9)

    end function toTensor2nd_nonSym

    pure function toTensor2nd(a) result(res)
        real(kind=dp), dimension(6), intent(in) :: a
        real(kind=dp), dimension(3,3)           :: res

        res(1,1) = a(1)
        res(2,2) = a(2)
        res(3,3) = a(3)
        res(1,2) = a(4)
        res(2,3) = a(5)
        res(1,3) = a(6)
        res(2,1) = res(1,2)        
        res(3,2) = res(2,3)        
        res(3,1) = res(1,3)        

    end function toTensor2nd

end module libtt_voigt
    