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
        module procedure toVoigt4th_nonSym
    end interface toVoigtNonSym

    interface toTensor
        module procedure toTensor2nd
        module procedure toTensor4th
    end interface toTensor

    interface toTensorNonSym
        module procedure toTensor2nd_nonSym
        module procedure toTensor4th_nonSym
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

    pure function toVoigt4th_nonSym(a) result(res)
        real(kind=dp), dimension(3,3,3,3) , intent(in) :: a
        real(kind=dp), dimension(9,9)                  :: res

        res(1,1) = a(1,1,1,1)
        res(1,2) = a(1,1,1,2)
        res(1,3) = a(1,1,1,3)
        res(1,4) = a(1,1,2,1)
        res(1,5) = a(1,1,2,2)
        res(1,6) = a(1,1,2,3)
        res(1,7) = a(1,1,3,1)
        res(1,8) = a(1,1,3,2)
        res(1,9) = a(1,1,3,3)

        res(2,1) = a(1,2,1,1)
        res(2,2) = a(1,2,1,2)
        res(2,3) = a(1,2,1,3)
        res(2,4) = a(1,2,2,1)
        res(2,5) = a(1,2,2,2)
        res(2,6) = a(1,2,2,3)
        res(2,7) = a(1,2,3,1)
        res(2,8) = a(1,2,3,2)
        res(2,9) = a(1,2,3,3)

        res(3,1) = a(1,3,1,1)
        res(3,2) = a(1,3,1,2)
        res(3,3) = a(1,3,1,3)
        res(3,4) = a(1,3,2,1)
        res(3,5) = a(1,3,2,2)
        res(3,6) = a(1,3,2,3)
        res(3,7) = a(1,3,3,1)
        res(3,8) = a(1,3,3,2)
        res(3,9) = a(1,3,3,3)

        res(4,1) = a(2,1,1,1)
        res(4,2) = a(2,1,1,2)
        res(4,3) = a(2,1,1,3)
        res(4,4) = a(2,1,2,1)
        res(4,5) = a(2,1,2,2)
        res(4,6) = a(2,1,2,3)
        res(4,7) = a(2,1,3,1)
        res(4,8) = a(2,1,3,2)
        res(4,9) = a(2,1,3,3)

        res(5,1) = a(2,2,1,1)
        res(5,2) = a(2,2,1,2)
        res(5,3) = a(2,2,1,3)
        res(5,4) = a(2,2,2,1)
        res(5,5) = a(2,2,2,2)
        res(5,6) = a(2,2,2,3)
        res(5,7) = a(2,2,3,1)
        res(5,8) = a(2,2,3,2)
        res(5,9) = a(2,2,3,3)

        res(6,1) = a(2,3,1,1)
        res(6,2) = a(2,3,1,2)
        res(6,3) = a(2,3,1,3)
        res(6,4) = a(2,3,2,1)
        res(6,5) = a(2,3,2,2)
        res(6,6) = a(2,3,2,3)
        res(6,7) = a(2,3,3,1)
        res(6,8) = a(2,3,3,2)
        res(6,9) = a(2,3,3,3)

        res(7,1) = a(3,1,1,1)
        res(7,2) = a(3,1,1,2)
        res(7,3) = a(3,1,1,3)
        res(7,4) = a(3,1,2,1)
        res(7,5) = a(3,1,2,2)
        res(7,6) = a(3,1,2,3)
        res(7,7) = a(3,1,3,1)
        res(7,8) = a(3,1,3,2)
        res(7,9) = a(3,1,3,3)

        res(8,1) = a(3,2,1,1)
        res(8,2) = a(3,2,1,2)
        res(8,3) = a(3,2,1,3)
        res(8,4) = a(3,2,2,1)
        res(8,5) = a(3,2,2,2)
        res(8,6) = a(3,2,2,3)
        res(8,7) = a(3,2,3,1)
        res(8,8) = a(3,2,3,2)
        res(8,9) = a(3,2,3,3)

        res(9,1) = a(3,3,1,1)
        res(9,2) = a(3,3,1,2)
        res(9,3) = a(3,3,1,3)
        res(9,4) = a(3,3,2,1)
        res(9,5) = a(3,3,2,2)
        res(9,6) = a(3,3,2,3)
        res(9,7) = a(3,3,3,1)
        res(9,8) = a(3,3,3,2)
        res(9,9) = a(3,3,3,3)

    end function toVoigt4th_nonSym

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

    pure function toTensor4th_nonSym(a) result(res)
        real(kind=dp), dimension(9,9),   intent(in) :: a
        real(kind=dp), dimension(3,3,3,3)           :: res

        res(1,1,1,1) = a(1,1)
        res(1,1,1,2) = a(1,2)
        res(1,1,1,3) = a(1,3)
        res(1,1,2,1) = a(1,4)
        res(1,1,2,2) = a(1,5)
        res(1,1,2,3) = a(1,6)
        res(1,1,3,1) = a(1,7)
        res(1,1,3,2) = a(1,8)
        res(1,1,3,3) = a(1,9)

        res(1,2,1,1) = a(2,1)
        res(1,2,1,2) = a(2,2)
        res(1,2,1,3) = a(2,3)
        res(1,2,2,1) = a(2,4)
        res(1,2,2,2) = a(2,5)
        res(1,2,2,3) = a(2,6)
        res(1,2,3,1) = a(2,7)
        res(1,2,3,2) = a(2,8)
        res(1,2,3,3) = a(2,9)

        res(1,3,1,1) = a(3,1)
        res(1,3,1,2) = a(3,2)
        res(1,3,1,3) = a(3,3)
        res(1,3,2,1) = a(3,4)
        res(1,3,2,2) = a(3,5)
        res(1,3,2,3) = a(3,6)
        res(1,3,3,1) = a(3,7)
        res(1,3,3,2) = a(3,8)
        res(1,3,3,3) = a(3,9)

        res(2,1,1,1) = a(4,1)
        res(2,1,1,2) = a(4,2)
        res(2,1,1,3) = a(4,3)
        res(2,1,2,1) = a(4,4)
        res(2,1,2,2) = a(4,5)
        res(2,1,2,3) = a(4,6)
        res(2,1,3,1) = a(4,7)
        res(2,1,3,2) = a(4,8)
        res(2,1,3,3) = a(4,9)

        res(2,2,1,1) = a(5,1)
        res(2,2,1,2) = a(5,2)
        res(2,2,1,3) = a(5,3)
        res(2,2,2,1) = a(5,4)
        res(2,2,2,2) = a(5,5)
        res(2,2,2,3) = a(5,6)
        res(2,2,3,1) = a(5,7)
        res(2,2,3,2) = a(5,8)
        res(2,2,3,3) = a(5,9)

        res(2,3,1,1) = a(6,1)
        res(2,3,1,2) = a(6,2)
        res(2,3,1,3) = a(6,3)
        res(2,3,2,1) = a(6,4)
        res(2,3,2,2) = a(6,5)
        res(2,3,2,3) = a(6,6)
        res(2,3,3,1) = a(6,7)
        res(2,3,3,2) = a(6,8)
        res(2,3,3,3) = a(6,9)

        res(3,1,1,1) = a(7,1)
        res(3,1,1,2) = a(7,2)
        res(3,1,1,3) = a(7,3)
        res(3,1,2,1) = a(7,4)
        res(3,1,2,2) = a(7,5)
        res(3,1,2,3) = a(7,6)
        res(3,1,3,1) = a(7,7)
        res(3,1,3,2) = a(7,8)
        res(3,1,3,3) = a(7,9)

        res(3,2,1,1) = a(8,1)
        res(3,2,1,2) = a(8,2)
        res(3,2,1,3) = a(8,3)
        res(3,2,2,1) = a(8,4)
        res(3,2,2,2) = a(8,5)
        res(3,2,2,3) = a(8,6)
        res(3,2,3,1) = a(8,7)
        res(3,2,3,2) = a(8,8)
        res(3,2,3,3) = a(8,9)
        
        res(3,3,1,1) = a(9,1)
        res(3,3,1,2) = a(9,2)
        res(3,3,1,3) = a(9,3)
        res(3,3,2,1) = a(9,4)
        res(3,3,2,2) = a(9,5)
        res(3,3,2,3) = a(9,6)
        res(3,3,3,1) = a(9,7)
        res(3,3,3,2) = a(9,8)
        res(3,3,3,3) = a(9,9)

    end function toTensor4th_nonSym

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

    pure function toTensor4th(voigt) result(tensor)

        real(kind=dp), dimension(6,6), intent(in) :: voigt
        real(kind=dp), dimension(3,3,3,3)         :: tensor

        integer :: a,b,c,d

        tensor(1,1,1,1) = voigt(1,1) 
        tensor(1,1,2,2) = voigt(1,2) 
        tensor(1,1,3,3) = voigt(1,3) 
        tensor(1,1,1,2) = voigt(1,4) 
        tensor(1,1,1,3) = voigt(1,5) 
        tensor(1,1,2,3) = voigt(1,6) 

        tensor(2,2,1,1) = voigt(2,1) 
        tensor(2,2,2,2) = voigt(2,2)
        tensor(2,2,3,3) = voigt(2,3) 
        tensor(2,2,1,2) = voigt(2,4) 
        tensor(2,2,1,3) = voigt(2,5) 
        tensor(2,2,2,3) = voigt(2,6) 

        tensor(3,3,1,1) = voigt(3,1) 
        tensor(3,3,2,2) = voigt(3,2) 
        tensor(3,3,3,3) = voigt(3,3) 
        tensor(3,3,1,2) = voigt(3,4) 
        tensor(3,3,1,3) = voigt(3,5) 
        tensor(3,3,2,3) = voigt(3,6) 

        tensor(1,2,1,1) = voigt(4,1) 
        tensor(1,2,2,2) = voigt(4,2) 
        tensor(1,2,3,3) = voigt(4,3) 
        tensor(1,2,1,2) = voigt(4,4) 
        tensor(1,2,1,3) = voigt(4,5) 
        tensor(1,2,2,3) = voigt(4,6) 

        tensor(1,3,1,1) = voigt(5,1) 
        tensor(1,3,2,2) = voigt(5,2) 
        tensor(1,3,3,3) = voigt(5,3) 
        tensor(1,3,1,2) = voigt(5,4) 
        tensor(1,3,1,3) = voigt(5,5) 
        tensor(1,3,2,3) = voigt(5,6) 

        tensor(2,3,1,1) = voigt(6,1) 
        tensor(2,3,2,2) = voigt(6,2) 
        tensor(2,3,3,3) = voigt(6,3) 
        tensor(2,3,1,2) = voigt(6,4) 
        tensor(2,3,1,3) = voigt(6,5) 
        tensor(2,3,2,3) = voigt(6,6) 

        ! Exploit minor symmetries
        do a = 1,3,1
            do b = 1,3,1
                do c = 1,3,1
                    do d = 1,3,1
                        tensor(b,a,c,d) = tensor(a,b,c,d)
                        tensor(a,b,d,c) = tensor(a,b,c,d)
                    end do
                end do
            end do
        end do

    end function toTensor4th

end module libtt_voigt
    