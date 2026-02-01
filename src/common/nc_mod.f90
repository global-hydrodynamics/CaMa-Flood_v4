module nc_mod
#ifdef UseCDF_CMF
    use funit_mod, only: &
    &   LOG_UNIT
    use netcdf
    implicit none

    type NCConfig
        integer :: &
        &   ncid, varid, ndims
        integer, allocatable :: &
        &   shape(:)
    end type NCConfig



    interface read_nc
        module procedure :: read_nc_r4_2d
        module procedure :: read_nc_r4_3d
    end interface read_nc

contains

subroutine handle_error(status)
    integer, intent(in) :: status
    if (status /= nf90_noerr .and. status /= -43) then
        ! status = -43: no attribute
        write(LOG_UNIT, *) status, trim(nf90_strerror(status))
        stop
    endif
end subroutine handle_error


integer function dimid2varid(ncid, dimid)
    integer, intent(in) :: &
    &   ncid, dimid
    character(len=64) :: &
    &   name
    integer :: &
    &   status
    status = nf90_inquire_dimension(ncid, dimid, name=name)
    call handle_error( &
    &   nf90_inq_varid(ncid, name, dimid2varid))
end function dimid2varid

! ===================================================================================================
type(NCConfig) function init_ncconfig(path, varname)
    character(len=*), intent(in) :: &
    &   path, varname
    integer :: &
    &   status, idim
    integer, allocatable :: &
    &   dimids(:)

    call handle_error( &
    &   nf90_open(path, nf90_nowrite, init_ncconfig%ncid))
    call handle_error( &
    &   nf90_inq_varid(init_ncconfig%ncid, trim(varname), init_ncconfig%varid))

    status = nf90_inquire_variable( &
    &   init_ncconfig%ncid, init_ncconfig%varid, ndims=init_ncconfig%ndims)
    allocate(dimids(init_ncconfig%ndims), source=0)
    status = nf90_inquire_variable( &
    &   init_ncconfig%ncid, init_ncconfig%varid, dimids=dimids)
    allocate(init_ncconfig%shape(init_ncconfig%ndims), source=0)
    do idim = 1, init_ncconfig%ndims  ! last dim is time
        call handle_error( &
        &   nf90_inquire_dimension(init_ncconfig%ncid, dimids(idim), len=init_ncconfig%shape(idim)))
    enddo
end function init_ncconfig

! ===================================================================================================
subroutine get_nc_domain( &
&   ncconf, &
&   left, right, top, bottom)
    type(NCConfig), intent(in) :: &
    &   ncconf
    real(8), intent(out) :: &
    &   left, right, top, bottom
    real(8), allocatable :: &
    &   var(:)
    real(8) :: &
    &   res
    integer :: &
    &   status
    integer, allocatable :: &
    &   dimids(:)

    allocate(dimids(ncconf%ndims), source=0)
    status = nf90_inquire_variable( &
    &   ncconf%ncid, ncconf%varid, dimids=dimids)

    allocate(var(ncconf%shape(1)), source=-9999.d0)
    call handle_error( &
    &   nf90_get_var(ncconf%ncid, dimid2varid(ncconf%ncid, dimids(1)), var(:)))
    res = abs(var(1) - var(2))
    if (var(1) < var(2)) then
        left = dble(nint(var(1) - res * 0.5d0))
        right = dble(nint(var(ncconf%shape(1)) + res * 0.5d0))
    else
        left = dble(nint(var(1) + res * 0.5d0))
        right = dble(nint(var(ncconf%shape(1)) - res * 0.5d0))
    endif
    deallocate(var)

    allocate(var(ncconf%shape(2)), source=-9999.d0)
    call handle_error( &
    &   nf90_get_var(ncconf%ncid, dimid2varid(ncconf%ncid, dimids(2)), var(:)))
    if (var(1) > var(2)) then
        top = dble(nint(var(1) + res * 0.5d0))
        bottom = dble(nint(var(ncconf%shape(2)) - res * 0.5d0))
    else
        top = dble(nint(var(1) - res * 0.5d0))
        bottom = dble(nint(var(ncconf%shape(2)) + res * 0.5d0))
    endif
    deallocate(var)
end subroutine get_nc_domain


integer function get_nc_dt(ncconf)
    type(NCConfig), intent(in) :: &
    &   ncconf
    integer :: &
    &   status, time_len
    real(4), allocatable :: &
    &   time(:)
    integer, allocatable :: &
    &   dimids(:)

    allocate(dimids(ncconf%ndims), source=0)
    status = nf90_inquire_variable( &
    &   ncconf%ncid, ncconf%varid, dimids=dimids)
    call handle_error( &
    &   nf90_inquire_dimension(ncconf%ncid, dimids(ncconf%ndims), len=time_len))
    allocate(time(time_len), source=0.0)
    call handle_error( &
    &   nf90_get_var(ncconf%ncid, dimid2varid(ncconf%ncid, dimids(ncconf%ndims)), time))
    get_nc_dt = int(time(2) - time(1))
    deallocate(time)
end function get_nc_dt


!subroutine get_nc_scale_offset(unit, var_id, scale, offset)
!    integer, intent(in)  :: unit, var_id
!    double precision, intent(out) :: scale, offset
!    call handle_error(nf90_get_att(unit, var_id, 'scale_factor', scale ))
!    call handle_error(nf90_get_att(unit, var_id, 'add_offset'  , offset))
!    if (scale == 0.d0) scale = 1.d0
!end subroutine get_nc_scale_offset

! ===================================================================================================
subroutine check_get_var_error( &
&   status, &
&   file_is_end)
    integer, intent(in) :: &
    &   status
    logical, intent(out) :: &
    &   file_is_end
    if (status == nf90_noerr) then
        file_is_end = .FALSE.
    elseif (status == nf90_einvalcoords) then
        file_is_end = .TRUE.
    else
        call handle_error(status)
    endif
end subroutine check_get_var_error


subroutine read_nc_r4_2d( &
&   arr, file_is_end, &
&   ncconf, recnum)
    real(4), intent(out) :: &
    &   arr(:,:)
    logical, intent(out) :: &
    &   file_is_end
    type(NCConfig), intent(in) :: &
    &   ncconf
    integer, intent(in) :: &
    &   recnum
    integer :: &
    &   status
    status = nf90_get_var( &
    &   ncconf%ncid, ncconf%varid, arr, &
    &   start=[1,1,recnum], count=[ncconf%shape(1),ncconf%shape(2),1])
    call check_get_var_error( &
    &   status, &
    &   file_is_end)
end subroutine read_nc_r4_2d


subroutine read_nc_r4_3d( &
&   arr, file_is_end, &
&   ncconf, recnum)
    real(4), intent(out) :: &
    &   arr(:,:,:)
    logical, intent(out) :: &
    &   file_is_end
    type(NCConfig), intent(in) :: &
    &   ncconf
    integer, intent(in) :: &
    &   recnum
    integer :: &
    &   status
    status = nf90_get_var( &
    &   ncconf%ncid, ncconf%varid, arr, &
    &   start=[1,1,1,recnum], count=[ncconf%shape(1),ncconf%shape(2),ncconf%shape(3),1])
    call check_get_var_error( &
    &   status, &
    &   file_is_end)
end subroutine read_nc_r4_3d
#endif
end module nc_mod
