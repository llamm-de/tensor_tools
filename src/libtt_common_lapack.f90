!> Collection of common functions and/or subroutines for the tensortools
!! library.
module libtt_common_lapack

    use libtt_precision

    implicit none
    private

    public :: inverse
    
contains

    !> Inverse of NxN Matrix
    !! Computed using LAPACK routines
    !!
    !! @param  A   NxN Matrix
    !! @return res Inverse of A
    function inverse(A) result(res)
        real(kind=dp), dimension(:,:)                  :: A
        real(kind=dp), dimension(size(A,1), size(A,2)) :: res

        real(kind=dp), dimension(size(A,1)) :: work
        integer, dimension(size(A,1))       :: ipiv
        integer                             :: n, info

        res = A
        n   = size(A,1)
        ! SGETRF computes an LU factorization of a general M-by-N matrix A
        ! using partial pivoting with row interchanges.
        call SGETRF(n,n,res,n,ipiv,info)

        if (info.ne.0) stop 'Matrix is numerically singular!'
        
        ! SGETRI computes the inverse of a matrix using the LU factorization
        ! computed by SGETRF.
        call SGETRI(n,res,n,ipiv,work,n,info)
        if (info.ne.0) stop 'Matrix inversion failed!'

    end function inverse

end module libtt_common_lapack

