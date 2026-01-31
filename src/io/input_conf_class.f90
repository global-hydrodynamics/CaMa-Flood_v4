module input_conf_class
    ! many items are input by namelist
    ! see input_namelist_mod > read_nml_input_item
    use PARKIND1, only: &
    &   JPIM, JPRB, JPRM
    use YOS_CMF_INPUT, only: &
    &   LOGNAM

    use glob_mod, only: &
    &   NML_PATH, CLEN_ITEM, CLEN_PATH, CLEN_SHORT
    use funit_lib, only: &
    &   INQUIRE_FID
    use bin_lib, only: &
    &   open_bin, read_bin
    use text_lib, only: &
    &   to_lowercase
    use numeric_utils_mod, only: &
    &   nearly_equal
#ifdef UseCDF_CMF
    use nc_lib, only: &
    &   NCConfig, &
    &   init_ncconfig, get_nc_dt, get_nc_scale_offset, &
    &   read_nc, get_nc_domain
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
    &   map2vec, find_inpmat
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
        type(CaMaFrame)    :: map
        integer(kind=JPIM) :: &
        &   inpmat_idx, &   ! for interpolation
        &   unit, &         ! file unit
        &   nz, z_in, &     ! vertical index in file
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

        contains
        procedure :: get_item       => get_item
        procedure :: get_fmt        => get_fmt
        procedure :: get_path       => get_path
        procedure :: get_unit       => get_unit
        procedure :: get_nz         => get_nz
        procedure :: get_z_in       => get_z_in
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
    end type InputConf

contains

! ===================================================================================================
! Constructor
! ===================================================================================================
function init_InputConf(item_name, nml_unit, t) result(obj)
    type(InputConf) obj
    character(len=*), intent(in) :: item_name
    integer(kind=JPIM), intent(in) :: nml_unit
    integer(kind=JPIM), intent(in) :: t ! [sec] time
    character(len=CLEN_SHORT) :: &
    &   fmt, dt_unit
    character(len=CLEN_ITEM) :: &
    &   div_item
    character(len=CLEN_PATH) :: &
    &   path
    integer(kind=JPIM) :: &
    &   z_in, nx, ny, nz, unit, rec, dt_val
#ifdef UseCDF_CMF
    character(len=CLEN_ITEM) :: &
    &   var_name
#endif
    real(kind=JPRB) :: &
    &   left, right, top, bottom
    real(kind=JPRM) :: &
    &   scale, offset
    logical :: &
    &   is_catm, is_fldstg, is_found

    call read_nml_input_item(nml_unit, item_name, &
    &   is_found, fmt, path, z_in, is_catm, is_fldstg, scale, offset, div_item)
    if (.not. is_found) call raise_item_not_found_error('read_nml_input_item', 'input_item', item_name)
    obj%fmt    = fmt
    obj%path   = path
    obj%z_in   = z_in
    obj%div_item = div_item
    obj%apply_scale  = .not. nearly_equal(scale,  1.0_JPRM)
    obj%apply_offset = .not. nearly_equal(offset, 0.0_JPRM)
    obj%scale  = scale
    obj%offset = offset
    rec = 1

    select case (trim(to_lowercase(fmt)))
        case ('binary', 'bin')
            call read_nml_input_domain( &
            &   nml_unit, item_name, &
            &   is_found, left, right, top, bottom)
            if (.not. is_found) call raise_item_not_found_error('read_nml_input_domain', 'input_domain', item_name)
            call read_nml_input_shape( &
            &   nml_unit, item_name, &
            &   is_found, nx, ny, nz)
            if (.not. is_found) call raise_item_not_found_error('read_nml_input_shape', 'input_shape', item_name)
            call read_nml_input_tres( &
            &   nml_unit, item_name, &
            &   is_found, dt_val, dt_unit)
            if (.not. is_found) call raise_item_not_found_error('read_nml_input_tres', 'input_tres', item_name)
 
            unit = INQUIRE_FID()
            obj%unit = unit
            call open_bin(unit, path, 4 * nx * ny * nz)
#ifdef UseCDF_CMF
        case ('netcdf', 'nc')
            call read_nml_input_nc( &
            &   nml_unit, item_name, &
            &   is_found, var_name)
            if (.not. is_found) call raise_item_not_found_error('read_nml_input_nc', 'input_nc', item_name)
            obj%ncconf = init_ncconfig( &
            &   path, var_name)
            call get_nc_domain( &
            &   obj%ncconf, &
            &   left, right, top, bottom)
            call read_nml_input_tres( &
            &   nml_unit, item_name, &
            &   is_found, dt_val, dt_unit)
            if (is_found) then
                write(LOGNAM, '(a)') '    CAUTION: temporal resolution is identified with namelist'
            else
                dt_val  = get_nc_dt( &
                &   obj%ncconf)
                dt_unit = 'hour'
            endif
            !call get_nc_scale_offset( &
            !&   unit, var_id, &
            !&   scale, offset)
            !rec = 2 ! rec=1 is sYear-1/12/31/21:00-24:00, skipped in update_input
            nx = obj%ncconf%shape(1)
            ny = obj%ncconf%shape(2)
            if (obj%ncconf%ndims >= 4) then
                nz = obj%ncconf%shape(3)
            else
                nz = 1
            endif
#endif
        case default
            write(LOGNAM, '(a)') '[input_conf_class/init_InputConf InValidValueError]'
            write(LOGNAM, '(2a)') 'fmt = ', trim(fmt)
            stop
    end select
    !call read_area(west, east, south, north)
    obj%item = item_name
    obj%rec = rec
    obj%nz = nz
    obj%map = init_CaMaFrame( &
    &   left, right, top, bottom, nx, ny, is_catm, is_fldstg)
!write(LOGNAM, *) west, east, south, north
!write(LOGNAM, *) nx, ny, is_n2s, catm, fldstg
    obj%inpmat_idx = find_inpmat(obj%map)
    obj%dt = dt2sec(dt_val, dt_unit)
    obj%now_t = t
    obj%nxt_t = t

    write(LOGNAM, '(3a,i2,2a,i0)') '    shape: ', trim(obj%map%str())
    write(LOGNAM, '(a,i0,a)')      '    temporal resolution: ', dt_val, dt_unit
    write(LOGNAM, '(a,i0)')        '    inpmat_idx: ', obj%inpmat_idx
    write(LOGNAM, '(a,i0)')        '    nz = ', nz
!    write(LOGNAM, '(2a)')      '    input datetime : ', t%strftime('%Y/%m/%d/%H:%M')
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

integer(kind=JPIM) function get_nz(self) result(nz)
    class(InputConf), intent(in) :: self
    nz = self%nz
end function get_nz

integer(kind=JPIM) function get_z_in(self) result(z_in)
    class(InputConf), intent(in) :: self
    z_in = self%z_in
end function get_z_in

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
&   nx, ny, nz)
    class(InputConf), intent(in) :: &
    &   self
    integer(kind=JPIM), intent(out) :: &
    &   nx, ny, nz
    call self%map%shape(nx, ny)
    nz = self%get_nz()
end subroutine get_file_shape

subroutine set_next(self)
    class(InputConf), intent(inout) :: self
    self%now_t = self%nxt_t
    self%nxt_t = self%nxt_t + self%dt
    self%rec   = self%rec + 1
end subroutine set_next

subroutine apply_scale_offset(self, data)
    class(InputConf), intent(in) :: self
    real(kind=JPRM), intent(inout) :: data(:,:,:)

    if (self%apply_scale) data(:,:,:) = data(:,:,:) * self%scale
    if (self%apply_offset) data(:,:,:) = data(:,:,:) + self%offset
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
    &   nx, ny, nz
    logical :: &
    &   is_end
    real(kind=JPRM), allocatable :: &
    &   arr_r4(:,:,:)
    select case (trim(to_lowercase(self%get_fmt())))
        case ('binary', 'bin')
            call self%get_file_shape(nx, ny, nz)
            allocate(arr_r4(nx,ny,nz), source=0.0)
            call read_bin(arr_r4(:,:,:), self%get_path(), self%get_rec())
            is_end = .FALSE.
#ifdef UseCDF_CMF
        case ('netcdf', 'nc')
            call self%get_file_shape(nx, ny, nz)
            allocate(arr_r4(nx,ny,nz), source=0.0)
            if (self%ncconf%ndims == 3) then ! (lon, lat, time)
                call read_nc( &
                &   arr_r4(:,:,1), is_end, self%ncconf, self%get_rec())
            else
                call read_nc( &
                &   arr_r4(:,:,:), is_end, self%ncconf, self%get_rec())
            endif
#endif
        case default
            write(LOGNAM, '(a)') '[input_conf_class/update_input InValidValueError]'
            write(LOGNAM, '(2a)') 'fmt = ', trim(self%get_fmt())
            stop
    end select
    call self%apply_scale_offset(arr_r4(:,:,:))

    if (is_end) then
        write(LOGNAM, *) trim(self%get_item()), ': read last step again'
        if (allocated(arr_r4)) deallocate(arr_r4)
        return
    endif
    write(LOGNAM, '(a10,i5)') trim(self%get_item()), self%get_rec()

    call map2vec(arr_r4(:,:,self%get_z_in()), arr(:), self%get_map(), self%get_inpmat_idx())
    deallocate(arr_r4)
    call self%set_next()
end subroutine update_input

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
