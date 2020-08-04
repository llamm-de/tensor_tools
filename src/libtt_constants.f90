!> Constants (e.g. pi)
module libtt_constants
    
    use libtt_precision, only: dp

    implicit none
    save

    real(kind=dp), parameter :: pi = 4 * atan(1.0_8)
    real(kind=dp), parameter :: e  = 2.71828182845904523536

end module libtt_constants