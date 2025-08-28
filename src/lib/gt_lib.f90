module gt_lib
    use datetime_ext_mod, only: &
    &   RelativeDelta, strptime
    use funit_lib, only: &
    &   LOG_UNIT
    implicit none

contains

! ===================================================================================================
subroutine open_gt( &
&   path, unit)
    character(len=256), intent(in) :: path
    integer           , intent(in) :: unit
    integer ios
    open(unit=unit, file=path, &
    &    access='sequential', form='unformatted', convert='big_endian', iostat=ios)
    if (ios /= 0) then
        write(LOG_UNIT, *) 'open_gt ERROR: ', trim(path), ios
        stop
    endif
end subroutine open_gt

! ===================================================================================================
! Geo-coordinate
! ===================================================================================================
subroutine get_lon_coord( &
&   coord_name, &
&   left, right)
    character(len=*), intent(in) :: &
    &   coord_name
    real(8), intent(out) :: &
    &   left, right
    select case (trim(coord_name))
        case ('GLON360M', 'GLON720M')
            left = 0.d0
            right = 360.d0
        case ('GLON1500M')
            left = 123.d0
            right = 148.d0
        case default
            write(LOG_UNIT, '(2a)') '[gt_lib/get_lon_coord InValidValueError] ', trim(coord_name)
            stop
    end select
end subroutine get_lon_coord


subroutine get_lat_coord( &
&   coord_name, &
&   top, bottom)
    character(len=*), intent(in) :: &
    &   coord_name
    real(8), intent(out) :: &
    &   top, bottom
    select case (trim(coord_name))
        case ('GLAT180IM', 'GLAT360IM')
            top = -90.d0
            bottom = 90.d0
        case ('GLAT180M', 'GLAT360M')
            top =  90.d0
            bottom = -90.d0
        case ('GLAT1320IM')
            top = 24.d0
            bottom = 46.d0
        case default
            write(LOG_UNIT, '(2a)') '[gt_lib/get_lon_coord InValidValueError] ', trim(coord_name)
            stop
    end select
end subroutine get_lat_coord

! ===================================================================================================
! header2*
! ===================================================================================================
subroutine header2shape( &
&   header, &
&   nx, ny, nz)
    character(len=16), intent(in) :: &
    &   header(:)
    integer, intent(out) :: &
    &   nx, ny, nz
    read(header(31), *) nx
    read(header(34), *) ny
    read(header(37), *) nz
end subroutine header2shape


subroutine header2domain( &
&   header, &
&   left, right, top, bottom)
    character(len=16), intent(in) :: &
    &   header(:)
    real(8), intent(out) :: &
    &   left, right, top, bottom
    call get_lon_coord( &
    &   header(29), &
    &   left, right)
    call get_lat_coord( &
    &   header(32), &
    &   top, bottom)
end subroutine header2domain


subroutine header2dt( &
&   header1, header2, &
&   dt, dtunit)
    character(len=16), intent(in) :: &
    &   header1(:), header2(:)
    integer, intent(out) :: &
    &   dt
    character(len=16), intent(out) :: &
    &   dtunit
    integer :: &
    &   time1, time2
    read(header1(25), *) time1
    read(header2(25), *) time2
    dt = time2 - time1
    if (dt == 9) dt = 6
    if ( dt == 0 ) dt = 1
    read(header1(26), *) dtunit
    dtunit = trim(dtunit)
end subroutine header2dt

! ---------------------------------------------------------------------------------------------------
subroutine read_headers( &
&   path, unit, &
&   header1, header2)
    character(len=256), intent(in) :: &
    &   path
    integer, intent(in) :: &
    &   unit
    character(len=16), intent(out) :: &
    &   header1(64), header2(64)
    real, allocatable :: &
    &   tmpmap(:,:,:)
    integer :: &
    &   nx, ny, nz
    call open_gt(path, unit)
    read(unit) header1
    call header2shape( &
    &   header1(:), &
    &   nx, ny, nz)

    allocate(tmpmap(nx, ny, nz), source=0.0)
    read(unit) tmpmap(:,:,:)
    read(unit) header2
    close(unit)
end subroutine read_headers

! ===================================================================================================
subroutine read_gt( &
&   inputMap, file_is_end, &
&   funit)
    real, allocatable, intent(out) :: &
    &   inputMap(:,:,:)
    logical, intent(out) :: &
    &   file_is_end
    integer, intent(in) :: &
    &   funit
    character(len=16) :: &
    &   header(64)
    integer :: &
    &   nx, ny, nz, &
    &   ios
    read(funit, iostat=ios) header(:)
    if (ios == 0) then
        call header2shape( &
        &   header(:), &
        &   nx, ny, nz)
        allocate(inputMap(nx,ny,nz), source=0.0)
        file_is_end = .FALSE.
        read(funit) inputMap(:,:,:)
    elseif (ios == -1) then ! end of file
        file_is_end = .TRUE.
    else
        write(LOG_UNIT, *) 'iostat, unit =', ios, funit
        stop 'read_2D_input_gt : ERROR'
    endif
end subroutine read_gt

end module gt_lib
