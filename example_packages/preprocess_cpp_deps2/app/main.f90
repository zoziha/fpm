program hello_fpm

#ifdef REAL64
    use, intrinsic :: iso_fortran_env, only: rk => real64
#else
    use, intrinsic :: iso_fortran_env, only: rk => real32
#endif
    use utils, only: say_hello
    implicit none
    real(rk) :: x = 1.0_rk

    call say_hello(x)

end program hello_fpm
