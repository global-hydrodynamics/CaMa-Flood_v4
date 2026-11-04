module input_conf_class
    ! many items are input by namelist
    ! see input_namelist_mod > read_nml_input_item
    use PARKIND1, only: &
    &   JPIM, JPRB, JPRM
    use YOS_CMF_INPUT, only: &
    &   LOGNAM
    use datetime_mod, only: &
    &   DateTime, datetime2string, seconds_since_year_start

    use const_mod, only: &
    &   CLEN_ITEM, CLEN_PATH, CLEN_SHORT
    use glob_mod, only: &
    &   NML_PATH
    use funit_mod, only: &
    &   INQUIRE_FID
    use bin_mod, only: &
    &   open_bin, read_bin
    use text_mod, only: &
    &   to_lowercase
    use numeric_utils_mod, only: &
    &   nearly_equal
#ifdef UseCDF_CMF
    use nc_mod, only: &
    &   NCConfig, &
    &   init_ncconfig, get_nc_dt, get_nc_start_record, & !get_nc_scale_offset, &
    &   read_nc, get_nc_domain, handle_error, same_nc_horizontal_grid
    use netcdf, only: nf90_close
#endif
    use time_mod, only: &
    &   dt2sec
    use camaframe_mod, only: &
    &   CaMaFrame, init_CaMaFrame
    !use datetime_ext_mod, only: &
    !&   DateTime, RelativeDelta, operator(+), init_RelativeDelta
    !use intrp_time, only: &
    !&   LINTRP_TIME
    use dim_converter, only: &
    &   map2vec, get_inpmat_index
    use io_namelist_mod, only: &
    &   read_nml_input_item, read_nml_input_domain, read_nml_input_shape, read_nml_input_tres, read_nml_input_nc, &
    &   raise_item_not_found_error
    implicit none
    private
    public :: &
    &   InputConf, append_InputConf, init_InputConf

    type InputConf
        private
        ! fixed after initialization
        character(len=CLEN_ITEM) :: item ! variable identifier for matching namelist e.g. 'Tair, 'Roff'
        character(len=CLEN_SHORT) :: fmt ! 'bin', 'nc' ('gt' is deprecated)
        character(len=CLEN_PATH) :: path ! file path
        character(len=CLEN_PATH) :: diminfo_file, inpmat_file
        character(len=256) :: slice_dimname = '' ! requested extra dimension; empty means automatic
        type(CaMaFrame)    :: map
        integer(kind=JPIM) :: &
        &   inpmat_idx, &   ! for interpolation
        &   unit, &         ! file unit
        &   slice_count = 1, & ! binary file shape only; NetCDF count belongs to ncconf
        &   slice_index = 1, & ! requested index, including -1
        &   dt              ! temporal resolution [sec]
        character(len=CLEN_ITEM) :: div_item
        logical :: apply_scale, apply_offset
        real(kind=JPRM) :: scale, offset

#ifdef UseCDF_CMF
        type(NCConfig) :: &
        &   ncconf
#endif
        ! dynamically changed
        integer(kind=JPIM) :: &
        &   rec, &          ! record number (time step) to be read next
        &   now_t, nxt_t    ! current time, next time, counted from calculation start [sec]
        logical :: &
        &   is_updated ! flag to indicate whether the input is updated at current time step

        contains
        procedure :: get_item       => get_item
        procedure :: get_fmt        => get_fmt
        procedure :: get_path       => get_path
        procedure :: get_unit       => get_unit
        procedure :: get_slice_count => get_slice_count
        procedure :: get_slice_index => get_slice_index
        procedure :: get_slice_index_resolved => get_slice_index_resolved
#ifdef UseCDF_CMF
        procedure :: open_next_file => open_next_file
#endif
        procedure :: get_rec        => get_rec
        procedure :: get_map        => get_map
        procedure :: get_inpmat_idx => get_inpmat_idx
        procedure :: get_next_t     => get_next_t
        procedure :: get_dt         => get_dt
        procedure :: get_scale      => get_scale
        procedure :: get_offset     => get_offset
        procedure :: get_div_item   => get_div_item
        procedure :: get_file_shape => get_file_shape
        procedure :: update_needed  => update_needed
        procedure :: update_input   => update_input
        procedure :: set_next       => set_next
        procedure :: apply_scale_offset => apply_scale_offset
        procedure :: set_is_updated => set_is_updated
    end type InputConf

contains

! ===================================================================================================
! Constructor
! ===================================================================================================
function init_InputConf(item_name, nml_unit, start_dt) result(obj)
    type(InputConf) obj
    character(len=*), intent(in) :: item_name
    integer(kind=JPIM), intent(in) :: nml_unit
    type(DateTime), intent(in) :: start_dt
    character(len=CLEN_SHORT) :: &
    &   fmt, dt_unit
    character(len=CLEN_ITEM) :: &
    &   div_item
    character(len=CLEN_PATH) :: &
    &   path
    integer(kind=JPIM) :: &
    &   slice_index, nx, ny, slice_count, unit, rec, dt_val
#ifdef UseCDF_CMF
    integer(kind=JPIM) :: &
    &   nc_dt_sec
    character(len=CLEN_ITEM) :: &
    &   var_name
#endif
    real(kind=JPRB) :: &
    &   left, right, top, bottom
    real(kind=JPRM) :: &
    &   scale, offset
    logical :: &
    &   is_catm, is_fldstg, is_found, is_netcdf

    call read_nml_input_item(nml_unit, item_name, &
    &   is_found, fmt, path, slice_index, is_catm, is_fldstg, scale, offset, div_item, obj%diminfo_file, obj%inpmat_file)
    if (.not. is_found) call raise_item_not_found_error('read_nml_input_item', 'input_item', item_name)
    obj%fmt    = fmt
    obj%path   = path
    obj%slice_index   = slice_index
    obj%div_item = div_item
    obj%apply_scale  = .not. nearly_equal(scale,  1.0_JPRM)
    obj%apply_offset = .not. nearly_equal(offset, 0.0_JPRM)
    obj%scale  = scale
    obj%offset = offset
    rec = 1
    is_netcdf = .FALSE.

    select case (trim(to_lowercase(fmt)))
        case ('binary', 'bin')
            call read_nml_input_domain( &
            &   nml_unit, item_name, &
            &   is_found, left, right, top, bottom)
            if (.not. is_found) call raise_item_not_found_error('read_nml_input_domain', 'input_domain', item_name)
            call read_nml_input_shape( &
            &   nml_unit, item_name, &
            &   is_found, nx, ny, slice_count)
            if (.not. is_found) call raise_item_not_found_error('read_nml_input_shape', 'input_shape', item_name)
            call read_nml_input_tres( &
            &   nml_unit, item_name, &
            &   is_found, dt_val, dt_unit)
            if (.not. is_found) call raise_item_not_found_error('read_nml_input_tres', 'input_tres', item_name)
 
            obj%slice_count = slice_count
            if (nx < 1 .or. ny < 1 .or. slice_count < 1 .or. slice_index == 0 .or. &
            &   slice_index < -1 .or. slice_index > slice_count) then
                write(LOGNAM, '(2a)') '[init_InputConf ERROR] invalid binary shape or slice_index: ', trim(path)
                write(LOGNAM, '(a,i0,a,i0)') '    slice_index: ', slice_index, ', slice_count: ', slice_count
                stop 9
            endif

            unit = INQUIRE_FID()
            obj%unit = unit
            call open_bin(unit, path, 4 * nx * ny * slice_count)
#ifdef UseCDF_CMF
        case ('netcdf', 'nc')
            is_netcdf = .TRUE.
            call read_nml_input_nc( &
            &   nml_unit, item_name, &
            &   is_found, var_name, obj%slice_dimname)
            if (.not. is_found) call raise_item_not_found_error('read_nml_input_nc', 'input_nc', item_name)
            ! NetCDF shape is authoritative; also reject stale binary shape settings.
            call read_nml_input_shape(nml_unit, item_name, is_found, nx, ny, slice_count)
            if (is_found) then
                write(LOGNAM, '(a)') '[init_InputConf ERROR] input_shape is only for binary; NetCDF shape comes from the variable'
                stop 9
            endif
            obj%ncconf = init_ncconfig( &
            &   path, var_name, obj%slice_dimname, obj%slice_index)
            call get_nc_domain( &
            &   obj%ncconf, &
            &   left, right, top, bottom)
            call read_nml_input_tres( &
            &   nml_unit, item_name, &
            &   is_found, dt_val, dt_unit)
            if (is_found) then
                nc_dt_sec = get_nc_dt(obj%ncconf)
                if (dt2sec(dt_val, dt_unit) <= 0) then
                    write(LOGNAM, '(a)') &
                    &   '[input_conf_class/init_InputConf ERROR] input_tres must be positive'
                    stop 9
                endif
                if (nc_dt_sec > 0 .and. dt2sec(dt_val, dt_unit) /= nc_dt_sec) then
                    write(LOGNAM, '(a,i0,a,i0)') &
                    &   '[input_conf_class/init_InputConf ERROR] input_tres [s]=', &
                    &   dt2sec(dt_val, dt_unit), ', NetCDF time interval [s]=', nc_dt_sec
                    stop 9
                endif
                if (nc_dt_sec > 0) then
                    write(LOGNAM, '(a)') '    input_tres agrees with NetCDF CF time interval'
                else
                    write(LOGNAM, '(a)') '    single-record NetCDF interval is supplied by input_tres'
                endif
            else
                dt_val  = get_nc_dt(obj%ncconf)
                if (dt_val <= 0) then
                    write(LOGNAM, '(a)') &
                    &   '[input_conf_class/init_InputConf ERROR] input_tres is required for a single-record NetCDF without time bounds'
                    stop 9
                endif
                dt_unit = 'sec'
            endif
            rec = get_nc_start_record(obj%ncconf, start_dt)
            !call get_nc_scale_offset( &
            !&   unit, var_id, &
            !&   scale, offset)
            !rec = 2 ! rec=1 is sYear-1/12/31/21:00-24:00, skipped in update_input
            nx = obj%ncconf%shape(obj%ncconf%horizontal_dimpos(1))
            ny = obj%ncconf%shape(obj%ncconf%horizontal_dimpos(2))
#endif
        case default
            write(LOGNAM, '(a)') '[input_conf_class/init_InputConf InValidValueError]'
            write(LOGNAM, '(2a)') 'fmt = ', trim(fmt)
            stop
    end select
    !call read_area(west, east, south, north)
    obj%item = item_name
    obj%map = init_CaMaFrame( &
    &   left, right, top, bottom, nx, ny, is_catm, is_fldstg)
!write(LOGNAM, *) west, east, south, north
!write(LOGNAM, *) nx, ny, is_n2s, catm, fldstg
    if (obj%map%is_catm()) then
        if (len_trim(obj%diminfo_file) > 0) then
            write(LOGNAM, '(a)') '[init_InputConf ERROR] mapping files are not used for is_catm input'
            stop 1
        endif
        obj%inpmat_idx = 0
    else if (len_trim(obj%diminfo_file) > 0) then
        obj%inpmat_idx = get_inpmat_index(obj%diminfo_file, obj%inpmat_file, nx, ny)
    else
        write(LOGNAM, '(2a)') &
        &   '[init_InputConf ERROR] gridded input requires diminfo_file and inpmat_file: ', trim(item_name)
        stop 1
    endif
    obj%dt = dt2sec(dt_val, dt_unit)

    ! Binary annual inputs retain the legacy year-start convention. NetCDF
    ! inputs select their initial record from the file's CF time coordinate.
    if (.not. is_netcdf) rec = 1_JPIM + seconds_since_year_start(start_dt) / obj%dt
    obj%rec = rec
    obj%now_t = 0_JPIM
    obj%nxt_t = 0_JPIM
    obj%is_updated = .FALSE.

    write(LOGNAM, '(3a,i2,2a,i0)') '    shape: ', trim(obj%map%str())
    write(LOGNAM, '(a,i0,a)')      '    temporal resolution: ', dt_val, dt_unit
    write(LOGNAM, '(a,i0)')        '    inpmat_idx: ', obj%inpmat_idx
    write(LOGNAM, '(a,i0)')        '    slice_count = ', obj%get_slice_count()
    write(LOGNAM, '(2a)')          '    start datetime: ', datetime2string(start_dt)
    write(LOGNAM, '(a,i0)')        '    start record: ', obj%rec
#ifdef UseCDF_CMF
    if (is_netcdf) then
        write(LOGNAM, '(2a)')      '    CF time units: ', trim(obj%ncconf%time_axis%units)
        write(LOGNAM, '(2a)')      '    CF calendar: ', trim(obj%ncconf%time_axis%calendar)
    endif
#endif
    write(LOGNAM, '(2(a,L,a,e10.2))')  '    scale  = ', obj%apply_scale, ' ', obj%scale
    write(LOGNAM, '(2(a,L,a,e10.2))')  '    offset = ', obj%apply_offset, ' ', obj%offset
end function init_InputConf

! ===================================================================================================
! Getter/ Setter
! ===================================================================================================
character(len=CLEN_ITEM) function get_item(self) result(item)
    class(InputConf), intent(in) :: self
    item = self%item
end function get_item

character(len=CLEN_SHORT) function get_fmt(self) result(fmt)
    class(InputConf), intent(in) :: self
    fmt = self%fmt
end function get_fmt

character(len=CLEN_PATH) function get_path(self) result(path)
    class(InputConf), intent(in) :: self
    path = self%path
end function get_path

integer(kind=JPIM) function get_unit(self) result(unit)
    class(InputConf), intent(in) :: self
    unit = self%unit
end function get_unit

integer(kind=JPIM) function get_slice_count(self) result(slice_count)
    class(InputConf), intent(in) :: self
    slice_count = self%slice_count
#ifdef UseCDF_CMF
    if (trim(to_lowercase(self%fmt)) == 'nc' .or. trim(to_lowercase(self%fmt)) == 'netcdf') &
    &   slice_count = self%ncconf%slice_count
#endif
end function get_slice_count

integer(kind=JPIM) function get_slice_index(self) result(slice_index)
    class(InputConf), intent(in) :: self
    slice_index = self%slice_index
end function get_slice_index

integer(kind=JPIM) function get_slice_index_resolved(self) result(slice_index_resolved)
    class(InputConf), intent(in) :: self
    slice_index_resolved = self%slice_index
    if (self%slice_index == -1) slice_index_resolved = self%get_slice_count()
#ifdef UseCDF_CMF
    if (trim(to_lowercase(self%fmt)) == 'nc' .or. trim(to_lowercase(self%fmt)) == 'netcdf') &
    &   slice_index_resolved = self%ncconf%slice_index_resolved
#endif
end function get_slice_index_resolved

integer(kind=JPIM) function get_rec(self) result(rec)
    class(InputConf), intent(in) :: self
    rec = self%rec
end function get_rec

type(CaMaFrame) function get_map(self) result(map)
    class(InputConf), intent(in) :: self
    map = self%map
end function get_map

integer(kind=JPIM) function get_inpmat_idx(self) result(inpmat_idx)
    class(InputConf), intent(in) :: self
    inpmat_idx = self%inpmat_idx
end function get_inpmat_idx

integer(kind=JPIM) function get_dt(self) result(dt)
    class(InputConf), intent(in) :: self
    dt = self%dt
end function get_dt

integer(kind=JPIM) function get_next_t(self) result(nxt_t)
    class(InputConf), intent(in) :: self
    nxt_t = self%nxt_t
end function get_next_t

real(kind=JPRM) function get_scale(self) result(scale)
    class(InputConf), intent(in) :: self
    scale = self%scale
end function get_scale

real(kind=JPRM) function get_offset(self) result(offset)
    class(InputConf), intent(in) :: self
    offset = self%offset
end function get_offset

character(len=CLEN_ITEM) function get_div_item(self) result(res)
    class(InputConf), intent(in) :: self
    res = self%div_item
end function get_div_item

subroutine get_file_shape( &
&   self, &
&   nx, ny, slice_count)
    class(InputConf), intent(in) :: &
    &   self
    integer(kind=JPIM), intent(out) :: &
    &   nx, ny, slice_count
    call self%map%shape(nx, ny)
    slice_count = self%get_slice_count()
end subroutine get_file_shape

subroutine set_next(self)
    class(InputConf), intent(inout) :: self
    self%now_t = self%nxt_t
    self%nxt_t = self%nxt_t + self%dt
    self%rec   = self%rec + 1
end subroutine set_next

subroutine set_is_updated(self, is_updated)
    class(InputConf), intent(inout) :: self
    logical, intent(in) :: is_updated
    self%is_updated = is_updated
end subroutine set_is_updated

subroutine apply_scale_offset(self, data)
    class(InputConf), intent(in) :: self
    real(kind=JPRM), intent(inout) :: data(:,:,:)
    real(kind=JPRM), parameter :: fill_val = 1.e16_JPRM

    if (self%apply_scale) then
        where (.not. isnan(data(:,:,:)) .and. data(:,:,:) >= 0.0_JPRM .and. data(:,:,:) < fill_val)
            data(:,:,:) = data(:,:,:) * self%scale
        end where
    endif
    if (self%apply_offset) then
        where (.not. isnan(data(:,:,:)) .and. data(:,:,:) >= 0.0_JPRM .and. data(:,:,:) < fill_val)
            data(:,:,:) = data(:,:,:) + self%offset
        end where
    endif
end subroutine apply_scale_offset

! ===================================================================================================
logical function update_needed(self, now_t) result(is_needed)
    class(InputConf),  intent(in) :: self
    integer(kind=JPIM), intent(in) :: now_t
    is_needed = .FALSE.
    if (self%get_next_t() <= now_t) is_needed = .TRUE.
end function update_needed


subroutine update_input(self, arr)
    class(InputConf), intent(inout) :: self
    real(kind=JPRB), intent(out) :: &
    &   arr(:)
    integer(kind=JPIM) :: &
    &   nx, ny, slice_count, idx
    logical :: &
    &   is_end
    real(kind=JPRM), allocatable :: &
    &   arr_file(:,:,:)
    select case (trim(to_lowercase(self%get_fmt())))
        case ('binary', 'bin')
            call self%get_file_shape(nx, ny, slice_count)
            allocate(arr_file(nx,ny,slice_count), source=0.0_JPRM)
            call read_bin(arr_file(:,:,:), self%get_path(), self%get_rec())
            is_end = .FALSE.
            idx = self%get_slice_index_resolved()
#ifdef UseCDF_CMF
        case ('netcdf', 'nc')
            call self%get_file_shape(nx, ny, slice_count)
            allocate(arr_file(nx,ny,1), source=0.0_JPRM)
            idx = 1
            if (self%get_rec() > self%ncconf%time_len) then
                write(LOGNAM, '(2a,i0,a,i0)') &
                &   '[input_conf_class/update_input ERROR] ', trim(self%get_item()), &
                &   ': requested record ', self%get_rec(), ' exceeds NetCDF time length ', self%ncconf%time_len
                stop 9
            endif
            call read_nc(arr_file(:,:,1), is_end, self%ncconf, self%get_rec())
#endif
        case default
            write(LOGNAM, '(a)') '[input_conf_class/update_input InValidValueError]'
            write(LOGNAM, '(2a)') 'fmt = ', trim(self%get_fmt())
            stop
    end select
    if (is_end) then
        write(LOGNAM, '(2a,i0)') &
        &   '[input_conf_class/update_input ERROR] failed to read ', &
        &   trim(self%get_item()), self%get_rec()
        stop 9
    endif
    call self%apply_scale_offset(arr_file(:,:,:))
    write(LOGNAM, '(a10,i5)') trim(self%get_item()), self%get_rec()

    ! Input files are read into JPRM buffers. Invalid file-side values are
    ! left untouched, while valid cells are adjusted before map2vec.
    call map2vec(arr_file(:,:,idx), arr(:), self%get_map(), self%get_inpmat_idx())
    deallocate(arr_file)
    call self%set_next()
end subroutine update_input

#ifdef UseCDF_CMF
subroutine open_next_file(self, path, start_dt)
    class(InputConf), intent(inout) :: self
    character(len=*), intent(in) :: path
    type(DateTime), intent(in) :: start_dt
    type(NCConfig) :: next
    integer :: nx, ny, slice_count, rec, nc_dt

    if (trim(to_lowercase(self%fmt)) /= 'nc' .and. trim(to_lowercase(self%fmt)) /= 'netcdf') then
        write(LOGNAM, '(a)') '[open_next_file ERROR] only NetCDF input supports explicit reopening'
        stop 9
    endif
    next = init_ncconfig(path, trim(self%ncconf%varname), self%slice_dimname, self%slice_index)
    call self%get_file_shape(nx, ny, slice_count)
    if (any(next%shape(next%horizontal_dimpos) /= [nx,ny])) then
        write(LOGNAM, '(2a)') '[open_next_file ERROR] horizontal shape changed: ', trim(path)
        stop 9
    endif
    if (.not. same_nc_horizontal_grid(self%ncconf, next)) then
        write(LOGNAM, '(2a)') '[open_next_file ERROR] horizontal coordinates changed: ', trim(path)
        stop 9
    endif
    nc_dt = get_nc_dt(next)
    if (nc_dt > 0 .and. nc_dt /= self%dt) then
        write(LOGNAM, '(2a)') '[open_next_file ERROR] time interval changed: ', trim(path)
        stop 9
    endif
    rec = get_nc_start_record(next, start_dt)
    if (slice_count /= next%slice_count) then
        write(LOGNAM, '(a,i0,a,i0)') '    slice_count changed: ', slice_count, ' -> ', next%slice_count
    endif
    call handle_error(nf90_close(self%ncconf%ncid))
    self%ncconf = next
    self%path = path
    self%rec = rec
    ! The next read is still due at nxt_t; preserve the model's elapsed-time schedule.
    self%is_updated = .FALSE.
end subroutine open_next_file
#endif

! ===================================================================================================
! Array of InputConf
! ===================================================================================================
subroutine append_InputConf(array, obj)
    type(InputConf), allocatable, intent(inout) :: array(:)
    type(InputConf),              intent(in)    :: obj
    type(InputConf), allocatable :: tmp(:)
    integer(kind=JPIM) :: n
    if (.not. allocated(array)) then
        allocate(array(1)); array(1) = obj
        return
    endif
    n = size(array)
    allocate(tmp, source=array)
    deallocate(array)
    allocate(array(n + 1))
    array(1:n) = tmp(:)
    array(n+1) = obj
    deallocate(tmp)
end subroutine append_InputConf

end module input_conf_class
