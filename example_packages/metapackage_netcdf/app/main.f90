program main

    use netcdf, only: nf90_open, nf90_close, nf90_strerror
    implicit none
    integer :: status, fida

    real, dimension(:, :, :), allocatable :: t2m, u10
    real, dimension(:, :), allocatable :: lat, lon
    character(len=100) :: file_in

    file_in = 'test.nc'

    !- open netcdf file :
    status = nf90_open(trim(file_in), 0, fida)
    call erreur(status, .false., "read")

    !- close netcdf file :
    status = nf90_close(fida)
    call erreur(status, .false., "close")

contains

    subroutine erreur(iret, lstop, chaine)
        integer, intent(in) :: iret
        logical, intent(in) :: lstop
        character(len=*), intent(in) :: chaine
        character(len=80) :: message

        if (iret /= 0) then
            write (*, *) 'Routine: ', trim(chaine)
            write (*, *) 'Error: ', iret
            message = nf90_strerror(iret)
            write (*, *) 'Which means: ', trim(message)
            if (lstop) stop
        end if

    end subroutine erreur

end program main
