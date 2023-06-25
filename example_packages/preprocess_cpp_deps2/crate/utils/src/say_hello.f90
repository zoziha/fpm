module utils

#ifdef REAL64
    use, intrinsic :: iso_fortran_env, only: rk => real64
#else
    use, intrinsic :: iso_fortran_env, only: rk => real32
#endif
    implicit none

contains

    subroutine say_hello(x)
        real(rk), intent(in) :: x

        print '(a,1x,i0)', "Hello, X =", X

    end subroutine say_hello

end module utils
