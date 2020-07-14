module libtt_voigt
    
    use libtt_precision, only: dp

    implicit none
    private

    public :: toVoigt
    public :: toVoigtNonSym

    interface toVoigt
        module procedure toVoigt2nd
    end interface toVoigt

    interface toVoigtNonSym
        module procedure toVoigt2nd_nonSym
    end interface toVoigtNonSym

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

end module libtt_voigt
    