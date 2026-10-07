module nc_mod
#ifdef UseCDF_CMF
    use PARKIND1, only: &
    &   JPIM, JPIB, JPRM, JPRB, JPRD
    use YOS_CMF_INPUT, only: &
    &   LOGNAM, LLEAPYR
    use datetime_mod, only: &
    &   DateTime
    use cmf_cf_time_mod, only: &
    &   cf_time_axis, &
    &   cf_read_time_axis, &
    &   cf_find_time_record, &
    &   cf_calendar_matches_lleapyr
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    use netcdf
    implicit none

    type NCConfig
        integer :: &
        &   ncid, varid, ndims, time_dimid, time_varid, time_len
        integer, allocatable :: &
        &   shape(:), dimids(:)
        character(len=nf90_max_name), allocatable :: dimnames(:)
        character(len=1024) :: path = ''
        character(len=nf90_max_name) :: varname = ''
        integer :: horizontal_dimpos(2) = [1,2]
        integer :: slice_dimpos = 0, slice_dimid = 0, slice_count = 1, slice_index_resolved = 1
        character(len=64) :: &
        &   time_name = ''
        type(cf_time_axis) :: &
        &   time_axis
    end type NCConfig



    interface read_nc
        module procedure :: read_nc_r4_2d
    end interface read_nc

contains

subroutine handle_error(status)
    integer, intent(in) :: status
    if (status /= nf90_noerr .and. status /= -43) then
        ! status = -43: no attribute
        write(LOGNAM, *) status, trim(nf90_strerror(status))
        stop 9
    endif
end subroutine handle_error


integer function dimid2varid(ncid, dimid)
    integer, intent(in) :: &
    &   ncid, dimid
    character(len=64) :: &
    &   name
    call handle_error(nf90_inquire_dimension(ncid, dimid, name=name))
    call handle_error( &
    &   nf90_inq_varid(ncid, name, dimid2varid))
end function dimid2varid

! ===================================================================================================
type(NCConfig) function init_ncconfig(path, varname, slice_dimname, slice_index) result(obj)
    character(len=*), intent(in) :: &
    &   path, varname
    character(len=*), optional, intent(in) :: slice_dimname
    integer(kind=JPIM), optional, intent(in) :: slice_index
    integer :: &
    &   idim
    integer(kind=JPIM) :: &
    &   ierr
    character(len=256) :: &
    &   message
    logical :: &
    &   calendar_ok

    obj%path = path
    obj%varname = varname
    write(LOGNAM, '(2a)') '    file: ', trim(path)
    write(LOGNAM, '(2a)') '    variable: ', trim(varname)
    call handle_error(nf90_open(path, nf90_nowrite, obj%ncid))
    call handle_error(nf90_inq_varid(obj%ncid, trim(varname), obj%varid))
    call handle_error(nf90_inquire_variable(obj%ncid, obj%varid, ndims=obj%ndims))
    allocate(obj%dimids(obj%ndims), obj%dimnames(obj%ndims), obj%shape(obj%ndims))
    call handle_error(nf90_inquire_variable(obj%ncid, obj%varid, dimids=obj%dimids))
    do idim = 1, obj%ndims
        call handle_error(nf90_inquire_dimension(obj%ncid, obj%dimids(idim), &
        &   name=obj%dimnames(idim), len=obj%shape(idim)))
        write(LOGNAM, '(a,i0,3a,i0,a,i0)') '    dimension (Fortran) ', idim, ': ', &
        &   trim(obj%dimnames(idim)), ', length=', obj%shape(idim), ', id=', obj%dimids(idim)
    enddo
    call configure_slice(obj, slice_dimname, slice_index)
    obj%time_dimid = obj%dimids(obj%ndims)
    call handle_error(nf90_inquire_dimension( &
    &   obj%ncid, obj%time_dimid, &
    &   name=obj%time_name, len=obj%time_len))
    call handle_error(nf90_inq_varid( &
    &   obj%ncid, trim(obj%time_name), obj%time_varid))
    call cf_read_time_axis( &
    &   obj%ncid, trim(obj%time_name), &
    &   obj%time_axis, ierr, message)
    if (ierr /= 0_JPIM) then
        write(LOGNAM, '(2a)') '[nc_mod/init_ncconfig ERROR] ', trim(message)
        stop 9
    endif
    if (obj%time_axis%ntime /= obj%time_len) then
        write(LOGNAM, '(a,i0,a,i0)') &
        &   '[nc_mod/init_ncconfig ERROR] time coordinate length=', &
        &   obj%time_axis%ntime, ', data time dimension length=', obj%time_len
        stop 9
    endif
    calendar_ok = cf_calendar_matches_lleapyr( &
    &   obj%time_axis%calendar, LLEAPYR, ierr, message)
    if (ierr /= 0_JPIM .or. .not. calendar_ok) then
        write(LOGNAM, '(3a,l1)') '[nc_mod/init_ncconfig ERROR] calendar=', &
        &   trim(obj%time_axis%calendar), ', LLEAPYR=', LLEAPYR
        if (ierr /= 0_JPIM) write(LOGNAM, '(a)') trim(message)
        stop 9
    endif
end function init_ncconfig


subroutine input_structure_error(obj, reason)
    type(NCConfig), intent(in) :: obj
    character(len=*), intent(in) :: reason
    write(LOGNAM, '(2a)') '[nc_mod ERROR] ', trim(reason)
    write(LOGNAM, '(2a)') '    file: ', trim(obj%path)
    write(LOGNAM, '(2a)') '    variable: ', trim(obj%varname)
    write(LOGNAM, '(a,i0)') '    slice_count: ', obj%slice_count
    stop 9
end subroutine input_structure_error

logical function coordinate_on_dimension(obj, varid, dimid) result(matches)
    type(NCConfig), intent(in) :: obj
    integer, intent(in) :: varid, dimid
    integer :: status, rank, xtype, ids(1)
    matches = .FALSE.
    status = nf90_inquire_variable(obj%ncid, varid, ndims=rank, xtype=xtype)
    if (status /= nf90_noerr .or. rank /= 1) return
    if (xtype == nf90_char .or. xtype == nf90_string) return
    status = nf90_inquire_variable(obj%ncid, varid, dimids=ids)
    if (status == nf90_noerr) matches = ids(1) == dimid
end function coordinate_on_dimension

integer function horizontal_axis(obj, pos) result(axis_number)
    type(NCConfig), intent(in) :: obj
    integer, intent(in) :: pos
    integer :: status, varid
    character(len=64) :: axis, standard_name, units
    axis_number = 0
    status = nf90_inq_varid(obj%ncid, trim(obj%dimnames(pos)), varid)
    if (status /= nf90_noerr) return
    if (.not. coordinate_on_dimension(obj, varid, obj%dimids(pos))) return
    axis = ''
    standard_name = ''
    units = ''
    status = nf90_get_att(obj%ncid, varid, 'axis', axis)
    if (status /= nf90_noerr) axis = ''
    status = nf90_get_att(obj%ncid, varid, 'standard_name', standard_name)
    if (status /= nf90_noerr) standard_name = ''
    status = nf90_get_att(obj%ncid, varid, 'units', units)
    if (status /= nf90_noerr) units = ''
    if (axis == 'X' .or. standard_name == 'longitude' .or. units == 'degrees_east' .or. &
    &   obj%dimnames(pos) == 'lon' .or. obj%dimnames(pos) == 'longitude') axis_number = 1
    if (axis == 'Y' .or. standard_name == 'latitude' .or. units == 'degrees_north' .or. &
    &   obj%dimnames(pos) == 'lat' .or. obj%dimnames(pos) == 'latitude') then
        if (axis_number /= 0) call input_structure_error(obj, 'conflicting horizontal coordinate metadata')
        axis_number = 2
    endif
end function horizontal_axis

subroutine configure_slice(obj, slice_dimname, slice_index)
    type(NCConfig), intent(inout) :: obj
    character(len=*), optional, intent(in) :: slice_dimname
    integer(kind=JPIM), optional, intent(in) :: slice_index
    character(len=nf90_max_name) :: requested_name
    integer :: requested_index, pos, axis_number, extra_count, named_pos, status, varid
    character(len=256) :: units
    integer :: expected_horizontal(2)

    requested_name = ''
    requested_index = 1
    if (present(slice_dimname)) requested_name = slice_dimname
    if (present(slice_index)) requested_index = slice_index
    write(LOGNAM, '(2a)') '    requested slice_dimname: ', trim(requested_name)
    write(LOGNAM, '(a,i0)') '    slice_index: ', requested_index
    obj%slice_count = 0
    if (obj%ndims < 3 .or. obj%ndims > 4) &
    &   call input_structure_error(obj, 'require time, two horizontal dimensions and at most one extra dimension')
    obj%horizontal_dimpos = 0
    named_pos = 0
    do pos = 1, obj%ndims
        status = nf90_inq_varid(obj%ncid, trim(obj%dimnames(pos)), varid)
        if (status == nf90_noerr) then
            if (coordinate_on_dimension(obj, varid, obj%dimids(pos))) then
                units = ''
                status = nf90_get_att(obj%ncid, varid, 'units', units)
                if (status /= nf90_noerr) units = ''
                if (status == nf90_noerr .and. pos /= obj%ndims) then
                    if (index(units, ' since ') > 0) &
                    &   call input_structure_error(obj, 'unsupported order: require time last in Fortran order')
                endif
            endif
        endif
        if (obj%dimnames(pos) == requested_name) named_pos = pos
        axis_number = horizontal_axis(obj, pos)
        if (axis_number == 0) cycle
        if (obj%horizontal_dimpos(axis_number) /= 0) &
        &   call input_structure_error(obj, 'ambiguous horizontal dimensions; unsupported structure')
        obj%horizontal_dimpos(axis_number) = pos
    enddo
    if (any(obj%horizontal_dimpos == 0)) then
        ! Preserve the positional convention only for legacy 3D input, or
        ! after an explicit extra dimension has made the remaining axes unique.
        if (obj%ndims == 3) then
            expected_horizontal = [1,2]
        else if (named_pos > 0 .and. named_pos < obj%ndims) then
            axis_number = 0
            do pos = 1, obj%ndims-1
                if (pos == named_pos) cycle
                axis_number = axis_number + 1
                expected_horizontal(axis_number) = pos
            enddo
        else
            call input_structure_error(obj, 'cannot identify horizontal axes uniquely; specify slice_dimname')
        endif
        if (any(obj%horizontal_dimpos /= 0 .and. obj%horizontal_dimpos /= expected_horizontal)) &
        &   call input_structure_error(obj, 'horizontal metadata conflicts with the supported X,Y order')
        obj%horizontal_dimpos = expected_horizontal
    endif
    if (obj%horizontal_dimpos(1) >= obj%horizontal_dimpos(2) .or. &
    &   any(obj%horizontal_dimpos >= obj%ndims)) &
    &   call input_structure_error(obj, 'unsupported order: require X before Y and time last in Fortran order')
    do pos = 1, 2
        axis_number = obj%horizontal_dimpos(pos)
        if (obj%shape(axis_number) < 2) &
        &   call input_structure_error(obj, 'horizontal dimensions must have at least two elements')
        call validate_dimension_coordinate(obj, axis_number)
    enddo
    extra_count = 0
    obj%slice_dimpos = 0
    do pos = 1, obj%ndims-1
        if (any(obj%horizontal_dimpos == pos)) cycle
        extra_count = extra_count + 1
        obj%slice_dimpos = pos
    enddo
    if (extra_count > 1) call input_structure_error(obj, 'multiple extra dimensions are unsupported')
    obj%slice_count = 1
    if (obj%slice_dimpos > 0) obj%slice_count = obj%shape(obj%slice_dimpos)
    if (len_trim(requested_name) > 0) then
        if (named_pos == 0) call input_structure_error(obj, 'slice_dimname is not a dimension of this variable')
        if (named_pos /= obj%slice_dimpos) &
        &   call input_structure_error(obj, 'slice_dimname selects time or a horizontal dimension')
    endif
    if (obj%slice_dimpos == 0 .and. requested_index /= 1) &
    &   call input_structure_error(obj, 'slice_index requires an extra dimension')
    if (requested_index == 0 .or. requested_index < -1 .or. requested_index > obj%slice_count) &
    &   call input_structure_error(obj, 'slice_index is out of range (use 1..slice_count or -1)')
    if (obj%slice_count < 1) call input_structure_error(obj, 'empty slice dimension')
    obj%slice_index_resolved = requested_index
    if (requested_index == -1) obj%slice_index_resolved = obj%slice_count
    if (obj%slice_dimpos > 0) then
        obj%slice_dimid = obj%dimids(obj%slice_dimpos)
        write(LOGNAM, '(2a)') '    slice_dimname: ', trim(obj%dimnames(obj%slice_dimpos))
    else
        write(LOGNAM, '(a)') '    slice_dimname: (none)'
    endif
    write(LOGNAM, '(a,i0)') '    slice_dimpos (Fortran): ', obj%slice_dimpos
    write(LOGNAM, '(a,i0)') '    slice_dimid: ', obj%slice_dimid
    write(LOGNAM, '(a,i0)') '    slice_count: ', obj%slice_count
    write(LOGNAM, '(a,i0)') '    slice_index_resolved: ', obj%slice_index_resolved
    if (obj%slice_dimpos > 0) call log_slice_coordinate(obj)
end subroutine configure_slice

subroutine validate_dimension_coordinate(obj, pos)
    type(NCConfig), intent(in) :: obj
    integer, intent(in) :: pos
    integer :: status, varid
    status = nf90_inq_varid(obj%ncid, trim(obj%dimnames(pos)), varid)
    if (status /= nf90_noerr) call input_structure_error(obj, 'missing horizontal dimension coordinate')
    if (.not. coordinate_on_dimension(obj, varid, obj%dimids(pos))) &
    &   call input_structure_error(obj, 'require numeric 1D horizontal dimension coordinates')
end subroutine validate_dimension_coordinate

subroutine log_slice_coordinate(obj)
    type(NCConfig), intent(in) :: obj
    integer :: status, varid, candidate, nvars, matches, pos
    character(len=1024) :: coordinates
    character(len=nf90_max_name) :: name, units
    real(kind=JPRD) :: value(1), fill, scale, offset

    status = nf90_inq_varid(obj%ncid, trim(obj%dimnames(obj%slice_dimpos)), varid)
    candidate = 0
    if (status == nf90_noerr) then
        if (coordinate_on_dimension(obj, varid, obj%slice_dimid)) candidate = varid
    endif
    if (candidate == 0) then
        ! Only use an unambiguous 1D auxiliary coordinate explicitly associated
        ! with this variable; do not assume coordinate and dimension names match.
        coordinates = ''
        status = nf90_get_att(obj%ncid, obj%varid, 'coordinates', coordinates)
        if (status /= nf90_noerr) return
        status = nf90_inquire(obj%ncid, nVariables=nvars)
        if (status /= nf90_noerr) return
        matches = 0
        do varid = 1, nvars
            if (.not. coordinate_on_dimension(obj, varid, obj%slice_dimid)) cycle
            status = nf90_inquire_variable(obj%ncid, varid, name=name)
            if (index(' '//trim(coordinates)//' ', ' '//trim(name)//' ') == 0) cycle
            matches = matches + 1
            candidate = varid
        enddo
        if (matches /= 1) return
    endif
    status = nf90_get_var(obj%ncid, candidate, value, start=[obj%slice_index_resolved], count=[1])
    if (status /= nf90_noerr .or. .not. ieee_is_finite(value(1))) return
    status = nf90_get_att(obj%ncid, candidate, '_FillValue', fill)
    if (status == nf90_noerr) then
        if (value(1) == fill) return
    endif
    status = nf90_get_att(obj%ncid, candidate, 'missing_value', fill)
    if (status == nf90_noerr) then
        if (value(1) == fill) return
    endif
    ! This decoding is for the optional coordinate log only.
    scale = 1.0_JPRD
    offset = 0.0_JPRD
    status = nf90_get_att(obj%ncid, candidate, 'scale_factor', scale)
    if (status /= nf90_noerr) scale = 1.0_JPRD
    status = nf90_get_att(obj%ncid, candidate, 'add_offset', offset)
    if (status /= nf90_noerr) offset = 0.0_JPRD
    value = value * scale + offset
    if (.not. ieee_is_finite(value(1))) return
    units = ''
    status = nf90_get_att(obj%ncid, candidate, 'units', units)
    if (status /= nf90_noerr) units = ''
    pos = index(units, achar(0))
    if (pos > 0) units(pos:) = ''
    status = nf90_inquire_variable(obj%ncid, candidate, name=name)
    write(LOGNAM, '(3a,es24.16,2a)') '    slice coordinate ', trim(name), ': ', value(1), ' ', trim(units)
end subroutine log_slice_coordinate

! ===================================================================================================
subroutine get_nc_domain( &
&   ncconf, &
&   left, right, top, bottom)
    type(NCConfig), intent(in) :: &
    &   ncconf
    real(kind=JPRB), intent(out) :: &
    &   left, right, top, bottom
    real(kind=JPRB), allocatable :: &
    &   var(:)
    real(kind=JPRB) :: &
    &   res
    integer :: &
    &   status
    integer, allocatable :: &
    &   dimids(:)

    allocate(dimids(ncconf%ndims), source=0)
    status = nf90_inquire_variable( &
    &   ncconf%ncid, ncconf%varid, dimids=dimids)

    allocate(var(ncconf%shape(ncconf%horizontal_dimpos(1))), source=-9999._JPRB)
    call handle_error( &
    &   nf90_get_var(ncconf%ncid, dimid2varid(ncconf%ncid, dimids(ncconf%horizontal_dimpos(1))), var(:)))
    res = abs(var(1) - var(2))
    if (var(1) < var(2)) then
        left = real(nint(var(1) - res * 0.5_JPRB), kind=JPRB)
        right = real(nint(var(ncconf%shape(ncconf%horizontal_dimpos(1))) + res * 0.5_JPRB), kind=JPRB)
    else
        left = real(nint(var(1) + res * 0.5_JPRB), kind=JPRB)
        right = real(nint(var(ncconf%shape(ncconf%horizontal_dimpos(1))) - res * 0.5_JPRB), kind=JPRB)
    endif
    deallocate(var)

    allocate(var(ncconf%shape(ncconf%horizontal_dimpos(2))), source=-9999._JPRB)
    call handle_error( &
    &   nf90_get_var(ncconf%ncid, dimid2varid(ncconf%ncid, dimids(ncconf%horizontal_dimpos(2))), var(:)))
    if (var(1) > var(2)) then
        top = real(nint(var(1) + res * 0.5_JPRB), kind=JPRB)
        bottom = real(nint(var(ncconf%shape(ncconf%horizontal_dimpos(2))) - res * 0.5_JPRB), kind=JPRB)
    else
        top = real(nint(var(1) - res * 0.5_JPRB), kind=JPRB)
        bottom = real(nint(var(ncconf%shape(ncconf%horizontal_dimpos(2))) + res * 0.5_JPRB), kind=JPRB)
    endif
    deallocate(var)
end subroutine get_nc_domain


logical function same_nc_horizontal_grid(a, b) result(matches)
    type(NCConfig), intent(in) :: a, b
    real(kind=JPRD), allocatable :: old_values(:), new_values(:)
    integer :: axis_number, old_pos, new_pos
    matches = .FALSE.
    if (any(a%shape(a%horizontal_dimpos) /= b%shape(b%horizontal_dimpos))) return
    do axis_number = 1, 2
        old_pos = a%horizontal_dimpos(axis_number)
        new_pos = b%horizontal_dimpos(axis_number)
        allocate(old_values(a%shape(old_pos)), new_values(b%shape(new_pos)))
        call handle_error(nf90_get_var(a%ncid, dimid2varid(a%ncid, a%dimids(old_pos)), old_values))
        call handle_error(nf90_get_var(b%ncid, dimid2varid(b%ncid, b%dimids(new_pos)), new_values))
        if (.not. all(ieee_is_finite(old_values)) .or. .not. all(ieee_is_finite(new_values))) return
        if (any(old_values /= new_values)) return
        deallocate(old_values, new_values)
    enddo
    matches = .TRUE.
end function same_nc_horizontal_grid

integer function get_nc_dt(ncconf)
    type(NCConfig), intent(in) :: &
    &   ncconf
    get_nc_dt = int(ncconf%time_axis%dt_minutes * 60_JPIB)
end function get_nc_dt


integer(kind=JPIM) function get_nc_start_record(ncconf, start_dt) result(record)
    type(NCConfig), intent(in) :: &
    &   ncconf
    type(DateTime), intent(in) :: &
    &   start_dt
    integer(kind=JPIM) :: &
    &   ierr
    character(len=256) :: &
    &   message

    call cf_find_time_record( &
    &   ncconf%time_axis, start_dt%yyyymmdd, start_dt%hour, 0_JPIM, &
    &   record, ierr, message, .TRUE.)
    if (ierr /= 0_JPIM) then
        write(LOGNAM, '(2a)') '[nc_mod/get_nc_start_record ERROR] ', trim(message)
        stop 9
    endif
end function get_nc_start_record


!subroutine get_nc_scale_offset(unit, var_id, scale, offset)
!    integer, intent(in)  :: unit, var_id
!    real(kind=JPRD), intent(out) :: scale, offset
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


subroutine read_nc_r4_2d(arr, file_is_end, ncconf, recnum)
    real(kind=JPRM), intent(out) :: arr(:,:)
    logical, intent(out) :: file_is_end
    type(NCConfig), intent(in) :: ncconf
    integer, intent(in) :: recnum
    integer :: status, start(ncconf%ndims), count(ncconf%ndims)
    real(kind=JPRM), allocatable :: buffer(:,:,:)

    if (any(shape(arr) /= ncconf%shape(ncconf%horizontal_dimpos))) &
    &   call input_structure_error(ncconf, 'horizontal buffer shape mismatch')
    start = 1
    count = ncconf%shape
    start(ncconf%ndims) = recnum
    count(ncconf%ndims) = 1
    if (ncconf%slice_dimpos > 0) then
        start(ncconf%slice_dimpos) = ncconf%slice_index_resolved
        count(ncconf%slice_dimpos) = 1
        ! Time remains last; removing the singleton slice preserves X,Y order.
        allocate(buffer(count(1),count(2),count(3)))
        status = nf90_get_var(ncconf%ncid, ncconf%varid, buffer, start=start, count=count)
        if (status == nf90_noerr) arr = reshape(buffer, shape(arr))
    else
        status = nf90_get_var(ncconf%ncid, ncconf%varid, arr, start=start, count=count)
    endif
    call check_get_var_error(status, file_is_end)
end subroutine read_nc_r4_2d
#endif
end module nc_mod
