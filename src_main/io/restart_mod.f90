module restart_mod
    use PARKIND1, only: &
    &   JPIM, JPRM, JPRD
    use const_mod, only: &
    &   RMIS, CLEN_PATH, CLEN_ITEM
    use glob_mod, only: &
    &   NML_PATH
    use datetime_mod, only: &
    &   DateTime, datetime2string
    use bin_mod, only: &
    &   read_bin, write_bin
    use cmf_ctrl_restart_mod, only: &
    &   CRESTSTO, CRESTDIR, CVNREST, LRESTDBL, IFRQ_RST
    use YOS_CMF_INPUT, only: &
    &   NX, NY, LOGNAM
    use CMF_UTILS_MOD, only: &
    &   map2vec_precision => map2vec_catm, &
    &   vec2map_precision => vec2map_catm
#ifdef OPT_MPI
    use mpi_mod, only: &
    &   add_mpi_suffix
#endif
    implicit none
    private
    public :: &
    &   init_restart_mod, read_init, read_restart, write_restart

    logical, parameter :: &
    &   MAP_FORMAT_DEF = .true.
    character(len=CLEN_PATH), parameter :: &
    &   PATH_DEF = ''
    logical :: &
    &   initial_state_is_dumped = .false.

    ! JPRM and JPRD are always distinct kinds. JPRB resolves to one of them
    ! according to SinglePrec_CMF, so callers can keep passing JPRB arrays.
    interface read_init
        module procedure read_init_single_1d
        module procedure read_init_double_1d
        module procedure read_init_single_2d
        module procedure read_init_double_2d
    end interface read_init

    interface read_restart
        module procedure read_restart_single_1d
        module procedure read_restart_double_1d
        module procedure read_restart_single_2d
        module procedure read_restart_double_2d
    end interface read_restart

    interface write_restart
        module procedure write_restart_single_1d
        module procedure write_restart_double_1d
        module procedure write_restart_single_2d
        module procedure write_restart_double_2d
    end interface write_restart

    interface dump_initial_state
        module procedure dump_initial_state_single_1d
        module procedure dump_initial_state_double_1d
        module procedure dump_initial_state_single_2d
        module procedure dump_initial_state_double_2d
    end interface dump_initial_state

    ! Minimal usage example for additional physics state variables:
    !
    !   if (restart_run) then
    !       call read_restart('xxx_state', dt, is_found, xxx_state)
    !   else
    !       call read_init('xxx_state', dt, is_found, xxx_state)
    !   endif
    !   call write_restart('xxx_state', dt, xxx_state)
    !
    ! This helper does not modify the core CaMa-Flood restart sequence.
    ! It only reads/writes additional binary restart items.

contains

subroutine init_restart_mod
    integer(kind=JPIM) :: ios
    integer :: unit
    namelist /restart_default/ initial_state_is_dumped

    initial_state_is_dumped = .false.
    open(newunit=unit, file=trim(NML_PATH), status='old')
    read(unit, nml=restart_default, iostat=ios)
    close(unit)
    if (ios > 0_JPIM) then
        write(LOGNAM, '(a)') '[restart_mod/init_restart_mod] ERROR'
        write(LOGNAM, '(a)') '  failed to read namelist restart_default'
        stop
    endif

    write(LOGNAM, '(a)') '[restart_mod/init_restart_mod]'
    write(LOGNAM, '(a,a)') '  CRESTSTO = ', trim(CRESTSTO)
    write(LOGNAM, '(a,a)') '  CRESTDIR = ', trim(CRESTDIR)
    write(LOGNAM, '(a,a)') '  CVNREST  = ', trim(CVNREST)
    write(LOGNAM, '(a,l1)') '  LRESTDBL = ', LRESTDBL
    write(LOGNAM, '(a,i0)') '  IFRQ_RST = ', IFRQ_RST
    write(LOGNAM, '(a,l1)') '  initial_state_is_dumped = ', initial_state_is_dumped
end subroutine init_restart_mod

subroutine raise_config_error(proc_name, nml_name, item_name, path, recnum, msg)
    character(len=*), intent(in) :: proc_name, nml_name, item_name, path, msg
    integer(kind=JPIM), intent(in) :: recnum
    write(LOGNAM, '(a)') '[restart_mod/'//trim(proc_name)//'] ERROR'
    write(LOGNAM, '(a)') '  namelist = '//trim(nml_name)
    write(LOGNAM, '(a)') '  item     = '//trim(item_name)
    write(LOGNAM, '(a)') '  path     = '//trim(path)
    write(LOGNAM, '(a,i0)') '  recnum   = ', recnum
    write(LOGNAM, '(a)') '  message  = '//trim(msg)
    stop
end subroutine raise_config_error

subroutine warn_restart_precision_loss(proc_name, item, path, recnum)
    character(len=*), intent(in) :: proc_name, item, path
    integer(kind=JPIM), intent(in) :: recnum

    if (LRESTDBL) return
    write(LOGNAM, '(a)') '[restart_mod/'//trim(proc_name)//'] WARNING'
    write(LOGNAM, '(a)') '  item     = '//trim(item)
    write(LOGNAM, '(a)') '  path     = '//trim(path)
    write(LOGNAM, '(a,i0)') '  recnum   = ', recnum
    write(LOGNAM, '(a)') &
    &   '  message  = double-precision values are written to a single-precision restart file.'
end subroutine warn_restart_precision_loss

subroutine read_init_config(item_name, is_found, path, recnum, mapfmt, use_scalar, scalar)
    character(len=*), intent(in) :: item_name
    logical, intent(out) :: is_found
    character(len=CLEN_PATH), intent(out) :: path
    integer(kind=JPIM), intent(out) :: recnum
    logical, intent(out) :: mapfmt
    logical, intent(out) :: use_scalar
    real(kind=JPRD), intent(out) :: scalar
    character(len=CLEN_ITEM) :: item
    character(len=CLEN_PATH) :: file
    integer(kind=JPIM) :: ios
    integer :: unit
    logical :: lexist
    namelist /init_config/ item, file, recnum, mapfmt, use_scalar, scalar

    is_found = .false.
    open(newunit=unit, file=trim(NML_PATH), status='old')
    do
        item = ''
        file = PATH_DEF
        recnum = 0_JPIM
        mapfmt = MAP_FORMAT_DEF
        use_scalar = .false.
        scalar = 0._JPRD
        read(unit, nml=init_config, iostat=ios)
        if (ios < 0_JPIM) exit
        if (trim(item) == trim(item_name)) then
            is_found = .true.
            exit
        endif
    enddo
    close(unit)
    path = file
    if (.not. is_found) return

    if (path == PATH_DEF) then
        if (.not. use_scalar) then
            call raise_config_error( &
            &   'read_init_config', 'init_config', item_name, path, recnum, &
            &   'file is empty and use_scalar=.false.')
        endif
    else
        if (recnum <= 0_JPIM) then
            call raise_config_error( &
            &   'read_init_config', 'init_config', item_name, path, recnum, &
            &   'recnum must be positive when file is specified.')
        endif
        inquire(file=trim(path), exist=lexist)
        if (.not. lexist) then
            call raise_config_error( &
            &   'read_init_config', 'init_config', item_name, path, recnum, &
            &   'file does not exist.')
        endif
    endif
end subroutine read_init_config

function build_restart_path(component_suffix, dt) result(path)
    character(len=*), intent(in) :: component_suffix
    type(DateTime), intent(in) :: dt
    character(len=CLEN_PATH) :: path
    character(len=10) :: cdate

    cdate = datetime2string(dt)
    path = trim(CRESTDIR)//trim(CVNREST)//trim(cdate)//trim(component_suffix)
#ifdef OPT_MPI
    path = add_mpi_suffix(path)
#endif
end function build_restart_path

subroutine read_restart_config(item_name, dt, path, recnum, mapfmt, require_existing)
    character(len=*), intent(in) :: item_name
    type(DateTime), intent(in) :: dt
    character(len=CLEN_PATH), intent(out) :: path
    integer(kind=JPIM), intent(out) :: recnum
    logical, intent(out) :: mapfmt
    logical, intent(in) :: require_existing
    character(len=CLEN_ITEM) :: item
    character(len=CLEN_PATH) :: file
    integer(kind=JPIM) :: ios
    integer :: unit
    logical :: lexist
    namelist /restart_config/ item, file, recnum, mapfmt

    open(newunit=unit, file=trim(NML_PATH), status='old')
    do
        item = ''
        file = PATH_DEF
        recnum = 0_JPIM
        mapfmt = MAP_FORMAT_DEF
        read(unit, nml=restart_config, iostat=ios)
        if (ios < 0_JPIM) then
            call raise_config_error( &
            &   'read_restart_config', 'restart_config', item_name, PATH_DEF, 0_JPIM, &
            &   'item is not found.')
        endif
        if (trim(item) == trim(item_name)) exit
    enddo
    close(unit)

    if (file == PATH_DEF) then
        call raise_config_error( &
        &   'read_restart_config', 'restart_config', item_name, file, recnum, &
        &   'component suffix is empty.')
    endif
    if (recnum <= 0_JPIM) then
        call raise_config_error( &
        &   'read_restart_config', 'restart_config', item_name, file, recnum, &
        &   'recnum must be positive.')
    endif
    path = build_restart_path(file, dt)
    if (require_existing) then
        inquire(file=trim(path), exist=lexist)
        if (.not. lexist) then
            call raise_config_error( &
            &   'read_restart_config', 'restart_config', item_name, path, recnum, &
            &   'file does not exist.')
        endif
    endif
end subroutine read_restart_config

function time_to_log_string(dt) result(label)
    type(DateTime), intent(in) :: dt
    character(len=10) :: label

    label = datetime2string(dt)
end function time_to_log_string

function build_initial_state_dump_path(source, item, dt) result(path)
    character(len=*), intent(in) :: source, item
    type(DateTime), intent(in) :: dt
    character(len=CLEN_PATH) :: path

    path = 'dump_'//trim(source)//'_'// &
    &   trim(item)//'_'//trim(time_to_log_string(dt))//'.bin'
#ifdef OPT_MPI
    path = add_mpi_suffix(path)
#endif
end function build_initial_state_dump_path

subroutine write_log_single(proc_name, item, dt, path, recnum, mapfmt, vmin, vmax)
    character(len=*), intent(in) :: proc_name, item, path
    type(DateTime), intent(in) :: dt
    integer(kind=JPIM), intent(in) :: recnum
    logical, intent(in) :: mapfmt
    real(kind=JPRM), intent(in) :: vmin, vmax
    write(LOGNAM, '(a)') '[restart_mod/'//trim(proc_name)//']'
    write(LOGNAM, '(a)') '  item   = '//trim(item)
    write(LOGNAM, '(a)') '  time   = '//trim(time_to_log_string(dt))
    write(LOGNAM, '(a,a)') '  path   = ', trim(path)
    write(LOGNAM, '(a,i0)') '  recnum = ', recnum
    write(LOGNAM, '(a,l1)') '  mapfmt = ', mapfmt
    write(LOGNAM, '(a,l1)') '  LRESTDBL = ', LRESTDBL
    write(LOGNAM, '(a,2es15.6)') '  min/max = ', vmin, vmax
end subroutine write_log_single

subroutine write_log_double(proc_name, item, dt, path, recnum, mapfmt, vmin, vmax)
    character(len=*), intent(in) :: proc_name, item, path
    type(DateTime), intent(in) :: dt
    integer(kind=JPIM), intent(in) :: recnum
    logical, intent(in) :: mapfmt
    real(kind=JPRD), intent(in) :: vmin, vmax
    write(LOGNAM, '(a)') '[restart_mod/'//trim(proc_name)//']'
    write(LOGNAM, '(a)') '  item   = '//trim(item)
    write(LOGNAM, '(a)') '  time   = '//trim(time_to_log_string(dt))
    write(LOGNAM, '(a,a)') '  path   = ', trim(path)
    write(LOGNAM, '(a,i0)') '  recnum = ', recnum
    write(LOGNAM, '(a,l1)') '  mapfmt = ', mapfmt
    write(LOGNAM, '(a,l1)') '  LRESTDBL = ', LRESTDBL
    write(LOGNAM, '(a,2es24.15)') '  min/max = ', vmin, vmax
end subroutine write_log_double

! ===================================================================================================
subroutine read_init_single_1d(item, dt, is_found, arr)
    character(len=*), intent(in) :: item
    type(DateTime), intent(in) :: dt
    logical, intent(out) :: is_found
    real(kind=JPRM), intent(out) :: arr(:)
    character(len=CLEN_PATH) :: path
    integer(kind=JPIM) :: recnum
    logical :: mapfmt
    logical :: use_scalar
    real(kind=JPRD) :: scalar

    call read_init_config(item, is_found, path, recnum, mapfmt, use_scalar, scalar)
    if (.not. is_found) return

    if (path /= PATH_DEF) then
        call read_field_single_1d(path, recnum, mapfmt, arr)
        if (initial_state_is_dumped) then
            call dump_initial_state('init', item, dt, mapfmt, arr)
        endif
    else
        if (use_scalar) then
            arr(:) = real(scalar, kind=JPRM)
        else
            call raise_config_error( &
            &   'read_init', 'init_config', item, path, recnum, &
            &   'path is empty and use_scalar=.false.')
        endif
    endif
    call write_log_single('read_init', item, dt, path, recnum, mapfmt, minval(arr), maxval(arr))
end subroutine read_init_single_1d

subroutine read_init_double_1d(item, dt, is_found, arr)
    character(len=*), intent(in) :: item
    type(DateTime), intent(in) :: dt
    logical, intent(out) :: is_found
    real(kind=JPRD), intent(out) :: arr(:)
    character(len=CLEN_PATH) :: path
    integer(kind=JPIM) :: recnum
    logical :: mapfmt, use_scalar
    real(kind=JPRD) :: scalar

    call read_init_config(item, is_found, path, recnum, mapfmt, use_scalar, scalar)
    if (.not. is_found) return

    if (path /= PATH_DEF) then
        call read_field_double_1d(path, recnum, mapfmt, arr)
        if (initial_state_is_dumped) then
            call dump_initial_state('init', item, dt, mapfmt, arr)
        endif
    else
        if (use_scalar) then
            arr(:) = scalar
        else
            call raise_config_error( &
            &   'read_init', 'init_config', item, path, recnum, &
            &   'path is empty and use_scalar=.false.')
        endif
    endif
    call write_log_double('read_init', item, dt, path, recnum, mapfmt, minval(arr), maxval(arr))
end subroutine read_init_double_1d

subroutine read_init_single_2d(item, dt, is_found, arr)
    character(len=*), intent(in) :: item
    type(DateTime), intent(in) :: dt
    logical, intent(out) :: is_found
    real(kind=JPRM), intent(out) :: arr(:,:)
    character(len=CLEN_PATH) :: path
    integer(kind=JPIM) :: recnum
    logical :: mapfmt
    logical :: use_scalar
    real(kind=JPRD) :: scalar

    call read_init_config(item, is_found, path, recnum, mapfmt, use_scalar, scalar)
    if (.not. is_found) return

    if (path /= PATH_DEF) then
        call read_field_single_2d(path, recnum, mapfmt, arr)
        if (initial_state_is_dumped) then
            call dump_initial_state('init', item, dt, mapfmt, arr)
        endif
    else
        if (use_scalar) then
            arr(:,:) = real(scalar, kind=JPRM)
        else
            call raise_config_error( &
            &   'read_init', 'init_config', item, path, recnum, &
            &   'path is empty and use_scalar=.false.')
        endif
    endif
    call write_log_single('read_init', item, dt, path, recnum, mapfmt, minval(arr), maxval(arr))
end subroutine read_init_single_2d

subroutine read_init_double_2d(item, dt, is_found, arr)
    character(len=*), intent(in) :: item
    type(DateTime), intent(in) :: dt
    logical, intent(out) :: is_found
    real(kind=JPRD), intent(out) :: arr(:,:)
    character(len=CLEN_PATH) :: path
    integer(kind=JPIM) :: recnum
    logical :: mapfmt, use_scalar
    real(kind=JPRD) :: scalar

    call read_init_config(item, is_found, path, recnum, mapfmt, use_scalar, scalar)
    if (.not. is_found) return

    if (path /= PATH_DEF) then
        call read_field_double_2d(path, recnum, mapfmt, arr)
        if (initial_state_is_dumped) then
            call dump_initial_state('init', item, dt, mapfmt, arr)
        endif
    else
        if (use_scalar) then
            arr(:,:) = scalar
        else
            call raise_config_error( &
            &   'read_init', 'init_config', item, path, recnum, &
            &   'path is empty and use_scalar=.false.')
        endif
    endif
    call write_log_double('read_init', item, dt, path, recnum, mapfmt, minval(arr), maxval(arr))
end subroutine read_init_double_2d

! ===================================================================================================
subroutine read_restart_single_1d(item, dt, is_found, arr)
    character(len=*), intent(in) :: item
    type(DateTime), intent(in) :: dt
    logical, intent(out) :: is_found
    real(kind=JPRM), intent(out) :: arr(:)
    character(len=CLEN_PATH) :: path
    integer(kind=JPIM) :: recnum
    logical :: mapfmt

    call read_restart_config(item, dt, path, recnum, mapfmt, .true.)
    is_found = .true.
    call read_field_single_1d(path, recnum, mapfmt, arr)
    if (initial_state_is_dumped) then
        call dump_initial_state('restart', item, dt, mapfmt, arr)
    endif
    call write_log_single('read_restart', item, dt, path, recnum, mapfmt, minval(arr), maxval(arr))
end subroutine read_restart_single_1d

subroutine read_restart_double_1d(item, dt, is_found, arr)
    character(len=*), intent(in) :: item
    type(DateTime), intent(in) :: dt
    logical, intent(out) :: is_found
    real(kind=JPRD), intent(out) :: arr(:)
    character(len=CLEN_PATH) :: path
    integer(kind=JPIM) :: recnum
    logical :: mapfmt

    call read_restart_config(item, dt, path, recnum, mapfmt, .true.)
    is_found = .true.
    call read_field_double_1d(path, recnum, mapfmt, arr)
    if (initial_state_is_dumped) then
        call dump_initial_state('restart', item, dt, mapfmt, arr)
    endif
    call write_log_double('read_restart', item, dt, path, recnum, mapfmt, minval(arr), maxval(arr))
end subroutine read_restart_double_1d

subroutine read_restart_single_2d(item, dt, is_found, arr)
    character(len=*), intent(in) :: item
    type(DateTime), intent(in) :: dt
    logical, intent(out) :: is_found
    real(kind=JPRM), intent(out) :: arr(:,:)
    character(len=CLEN_PATH) :: path
    integer(kind=JPIM) :: recnum
    logical :: mapfmt

    call read_restart_config(item, dt, path, recnum, mapfmt, .true.)
    is_found = .true.
    call read_field_single_2d(path, recnum, mapfmt, arr)
    if (initial_state_is_dumped) then
        call dump_initial_state('restart', item, dt, mapfmt, arr)
    endif
    call write_log_single('read_restart', item, dt, path, recnum, mapfmt, minval(arr), maxval(arr))
end subroutine read_restart_single_2d

subroutine read_restart_double_2d(item, dt, is_found, arr)
    character(len=*), intent(in) :: item
    type(DateTime), intent(in) :: dt
    logical, intent(out) :: is_found
    real(kind=JPRD), intent(out) :: arr(:,:)
    character(len=CLEN_PATH) :: path
    integer(kind=JPIM) :: recnum
    logical :: mapfmt

    call read_restart_config(item, dt, path, recnum, mapfmt, .true.)
    is_found = .true.
    call read_field_double_2d(path, recnum, mapfmt, arr)
    if (initial_state_is_dumped) then
        call dump_initial_state('restart', item, dt, mapfmt, arr)
    endif
    call write_log_double('read_restart', item, dt, path, recnum, mapfmt, minval(arr), maxval(arr))
end subroutine read_restart_double_2d

! ===================================================================================================
subroutine dump_initial_state_single_1d(source, item, dt, mapfmt, arr)
    character(len=*), intent(in) :: source, item
    type(DateTime), intent(in) :: dt
    logical, intent(in) :: mapfmt
    real(kind=JPRM), intent(in) :: arr(:)
    character(len=CLEN_PATH) :: path

    path = build_initial_state_dump_path(source, item, dt)
    call write_field_single_1d(path, 1_JPIM, mapfmt, arr)
    call write_log_single('dump_initial_state', item, dt, path, 1_JPIM, mapfmt, &
    &   minval(arr), maxval(arr))
end subroutine dump_initial_state_single_1d

subroutine dump_initial_state_double_1d(source, item, dt, mapfmt, arr)
    character(len=*), intent(in) :: source, item
    type(DateTime), intent(in) :: dt
    logical, intent(in) :: mapfmt
    real(kind=JPRD), intent(in) :: arr(:)
    character(len=CLEN_PATH) :: path

    path = build_initial_state_dump_path(source, item, dt)
    call warn_restart_precision_loss('dump_initial_state', item, path, 1_JPIM)
    call write_field_double_1d(path, 1_JPIM, mapfmt, arr)
    call write_log_double('dump_initial_state', item, dt, path, 1_JPIM, mapfmt, &
    &   minval(arr), maxval(arr))
end subroutine dump_initial_state_double_1d

subroutine dump_initial_state_single_2d(source, item, dt, mapfmt, arr)
    character(len=*), intent(in) :: source, item
    type(DateTime), intent(in) :: dt
    logical, intent(in) :: mapfmt
    real(kind=JPRM), intent(in) :: arr(:,:)
    character(len=CLEN_PATH) :: path

    path = build_initial_state_dump_path(source, item, dt)
    call write_field_single_2d(path, 1_JPIM, mapfmt, arr)
    call write_log_single('dump_initial_state', item, dt, path, 1_JPIM, mapfmt, &
    &   minval(arr), maxval(arr))
end subroutine dump_initial_state_single_2d

subroutine dump_initial_state_double_2d(source, item, dt, mapfmt, arr)
    character(len=*), intent(in) :: source, item
    type(DateTime), intent(in) :: dt
    logical, intent(in) :: mapfmt
    real(kind=JPRD), intent(in) :: arr(:,:)
    character(len=CLEN_PATH) :: path

    path = build_initial_state_dump_path(source, item, dt)
    call warn_restart_precision_loss('dump_initial_state', item, path, 1_JPIM)
    call write_field_double_2d(path, 1_JPIM, mapfmt, arr)
    call write_log_double('dump_initial_state', item, dt, path, 1_JPIM, mapfmt, &
    &   minval(arr), maxval(arr))
end subroutine dump_initial_state_double_2d

! ===================================================================================================
subroutine write_restart_single_1d(item, dt, arr)
    character(len=*), intent(in) :: item
    type(DateTime), intent(in) :: dt
    real(kind=JPRM), intent(in) :: arr(:)
    character(len=CLEN_PATH) :: path
    integer(kind=JPIM) :: recnum
    logical :: mapfmt

    call read_restart_config(item, dt, path, recnum, mapfmt, .false.)
    call write_field_single_1d(path, recnum, mapfmt, arr)
    call write_log_single('write_restart', item, dt, path, recnum, mapfmt, minval(arr), maxval(arr))
end subroutine write_restart_single_1d

subroutine write_restart_double_1d(item, dt, arr)
    character(len=*), intent(in) :: item
    type(DateTime), intent(in) :: dt
    real(kind=JPRD), intent(in) :: arr(:)
    character(len=CLEN_PATH) :: path
    integer(kind=JPIM) :: recnum
    logical :: mapfmt

    call read_restart_config(item, dt, path, recnum, mapfmt, .false.)
    call warn_restart_precision_loss('write_restart', item, path, recnum)
    call write_field_double_1d(path, recnum, mapfmt, arr)
    call write_log_double('write_restart', item, dt, path, recnum, mapfmt, minval(arr), maxval(arr))
end subroutine write_restart_double_1d

subroutine write_restart_single_2d(item, dt, arr)
    character(len=*), intent(in) :: item
    type(DateTime), intent(in) :: dt
    real(kind=JPRM), intent(in) :: arr(:,:)
    character(len=CLEN_PATH) :: path
    integer(kind=JPIM) :: recnum
    logical :: mapfmt

    call read_restart_config(item, dt, path, recnum, mapfmt, .false.)
    call write_field_single_2d(path, recnum, mapfmt, arr)
    call write_log_single('write_restart', item, dt, path, recnum, mapfmt, minval(arr), maxval(arr))
end subroutine write_restart_single_2d

subroutine write_restart_double_2d(item, dt, arr)
    character(len=*), intent(in) :: item
    type(DateTime), intent(in) :: dt
    real(kind=JPRD), intent(in) :: arr(:,:)
    character(len=CLEN_PATH) :: path
    integer(kind=JPIM) :: recnum
    logical :: mapfmt

    call read_restart_config(item, dt, path, recnum, mapfmt, .false.)
    call warn_restart_precision_loss('write_restart', item, path, recnum)
    call write_field_double_2d(path, recnum, mapfmt, arr)
    call write_log_double('write_restart', item, dt, path, recnum, mapfmt, minval(arr), maxval(arr))
end subroutine write_restart_double_2d

! ===================================================================================================
subroutine read_field_single_1d(path, recnum, mapfmt, arr)
    character(len=*), intent(in) :: path
    integer(kind=JPIM), intent(in) :: recnum
    logical, intent(in) :: mapfmt
    real(kind=JPRM), intent(out) :: arr(:)
    real(kind=JPRM), allocatable :: r1d(:), r2d(:,:)
    real(kind=JPRD), allocatable :: d1d(:), d2d(:,:)

    if (mapfmt) then
        if (LRESTDBL) then
            allocate(d2d(NX, NY))
            call read_bin(d2d, path, recnum)
            call map2vec_precision(d2d, arr)
            deallocate(d2d)
        else
            allocate(r2d(NX, NY))
            r2d(:,:) = 0._JPRM
            call read_bin(r2d, path, recnum)
            call map2vec_precision(r2d, arr)
            deallocate(r2d)
        endif
    else
        if (LRESTDBL) then
            allocate(d1d(size(arr)))
            call read_bin(d1d, path, recnum)
            arr(:) = real(d1d(:), kind=JPRM)
            deallocate(d1d)
        else
            allocate(r1d(size(arr)))
            call read_bin(r1d, path, recnum)
            arr(:) = r1d(:)
            deallocate(r1d)
        endif
    endif
end subroutine read_field_single_1d

subroutine read_field_double_1d(path, recnum, mapfmt, arr)
    character(len=*), intent(in) :: path
    integer(kind=JPIM), intent(in) :: recnum
    logical, intent(in) :: mapfmt
    real(kind=JPRD), intent(out) :: arr(:)
    real(kind=JPRM), allocatable :: r1d(:), r2d(:,:)
    real(kind=JPRD), allocatable :: d2d(:,:)

    if (mapfmt) then
        if (LRESTDBL) then
            allocate(d2d(NX, NY))
            call read_bin(d2d, path, recnum)
            call map2vec_precision(d2d, arr)
            deallocate(d2d)
        else
            allocate(r2d(NX, NY))
            call read_bin(r2d, path, recnum)
            call map2vec_precision(r2d, arr)
            deallocate(r2d)
        endif
    else
        if (LRESTDBL) then
            call read_bin(arr, path, recnum)
        else
            allocate(r1d(size(arr)))
            call read_bin(r1d, path, recnum)
            arr(:) = real(r1d(:), kind=JPRD)
            deallocate(r1d)
        endif
    endif
end subroutine read_field_double_1d

subroutine read_field_single_2d(path, recnum, mapfmt, arr)
    character(len=*), intent(in) :: path
    integer(kind=JPIM), intent(in) :: recnum
    logical, intent(in) :: mapfmt
    real(kind=JPRM), intent(out) :: arr(:,:)
    real(kind=JPRM), allocatable :: r2d(:,:)
    real(kind=JPRD), allocatable :: d2d(:,:)

    ! 2D restart arrays are always treated as 2D binary fields.
    ! mapfmt is retained in the config and logs for future distinction.
    if (mapfmt) then
        continue
    endif

    if (LRESTDBL) then
        allocate(d2d(size(arr,1), size(arr,2)))
        call read_bin(d2d, path, recnum)
        arr(:,:) = real(d2d(:,:), kind=JPRM)
        deallocate(d2d)
    else
        allocate(r2d(size(arr,1), size(arr,2)))
        call read_bin(r2d, path, recnum)
        arr(:,:) = r2d(:,:)
        deallocate(r2d)
    endif
end subroutine read_field_single_2d

subroutine read_field_double_2d(path, recnum, mapfmt, arr)
    character(len=*), intent(in) :: path
    integer(kind=JPIM), intent(in) :: recnum
    logical, intent(in) :: mapfmt
    real(kind=JPRD), intent(out) :: arr(:,:)
    real(kind=JPRM), allocatable :: r2d(:,:)

    ! 2D restart arrays are always treated as 2D binary fields.
    if (mapfmt) continue

    if (LRESTDBL) then
        call read_bin(arr, path, recnum)
    else
        allocate(r2d(size(arr,1), size(arr,2)))
        call read_bin(r2d, path, recnum)
        arr(:,:) = real(r2d(:,:), kind=JPRD)
        deallocate(r2d)
    endif
end subroutine read_field_double_2d

! ===================================================================================================
subroutine write_field_single_1d(path, recnum, mapfmt, arr)
    character(len=*), intent(in) :: path
    integer(kind=JPIM), intent(in) :: recnum
    logical, intent(in) :: mapfmt
    real(kind=JPRM), intent(in) :: arr(:)
    real(kind=JPRM), allocatable :: r1d(:), r2d(:,:)
    real(kind=JPRD), allocatable :: d1d(:), d2d(:,:)

    if (mapfmt) then
        if (LRESTDBL) then
            allocate(d2d(NX, NY))
            call vec2map_precision(arr, d2d)
            call write_bin(d2d, path, recnum)
            deallocate(d2d)
        else
            allocate(r2d(NX, NY))
            r2d(:,:) = 0._JPRM
            call vec2map_precision(arr, r2d)
            call write_bin(r2d, path, recnum)
            deallocate(r2d)
        endif
    else
        if (LRESTDBL) then
            allocate(d1d(size(arr)))
            d1d(:) = real(arr(:), kind=JPRD)
            call write_bin(d1d, path, recnum)
            deallocate(d1d)
        else
            allocate(r1d(size(arr)))
            r1d(:) = real(arr(:), kind=JPRM)
            call write_bin(r1d, path, recnum)
            deallocate(r1d)
        endif
    endif
end subroutine write_field_single_1d

subroutine write_field_double_1d(path, recnum, mapfmt, arr)
    character(len=*), intent(in) :: path
    integer(kind=JPIM), intent(in) :: recnum
    logical, intent(in) :: mapfmt
    real(kind=JPRD), intent(in) :: arr(:)
    real(kind=JPRM), allocatable :: r1d(:), r2d(:,:)
    real(kind=JPRD), allocatable :: d2d(:,:)

    if (mapfmt) then
        if (LRESTDBL) then
            allocate(d2d(NX, NY))
            call vec2map_precision(arr, d2d)
            call write_bin(d2d, path, recnum)
            deallocate(d2d)
        else
            allocate(r2d(NX, NY))
            call vec2map_precision(arr, r2d)
            call write_bin(r2d, path, recnum)
            deallocate(r2d)
        endif
    else
        if (LRESTDBL) then
            call write_bin(arr, path, recnum)
        else
            allocate(r1d(size(arr)))
            r1d(:) = real(arr(:), kind=JPRM)
            call write_bin(r1d, path, recnum)
            deallocate(r1d)
        endif
    endif
end subroutine write_field_double_1d

subroutine write_field_single_2d(path, recnum, mapfmt, arr)
    character(len=*), intent(in) :: path
    integer(kind=JPIM), intent(in) :: recnum
    logical, intent(in) :: mapfmt
    real(kind=JPRM), intent(in) :: arr(:,:)
    real(kind=JPRM), allocatable :: r2d(:,:)
    real(kind=JPRD), allocatable :: d2d(:,:)

    ! 2D restart arrays are always written as 2D binary fields.
    ! mapfmt is retained in the config and logs for future distinction.
    if (mapfmt) then
        continue
    endif

    if (LRESTDBL) then
        allocate(d2d(size(arr,1), size(arr,2)))
        d2d(:,:) = real(arr(:,:), kind=JPRD)
        call write_bin(d2d, path, recnum)
        deallocate(d2d)
    else
        allocate(r2d(size(arr,1), size(arr,2)))
        r2d(:,:) = real(arr(:,:), kind=JPRM)
        call write_bin(r2d, path, recnum)
        deallocate(r2d)
    endif
end subroutine write_field_single_2d

subroutine write_field_double_2d(path, recnum, mapfmt, arr)
    character(len=*), intent(in) :: path
    integer(kind=JPIM), intent(in) :: recnum
    logical, intent(in) :: mapfmt
    real(kind=JPRD), intent(in) :: arr(:,:)
    real(kind=JPRM), allocatable :: r2d(:,:)

    ! 2D restart arrays are always written as 2D binary fields.
    if (mapfmt) continue

    if (LRESTDBL) then
        call write_bin(arr, path, recnum)
    else
        allocate(r2d(size(arr,1), size(arr,2)))
        r2d(:,:) = real(arr(:,:), kind=JPRM)
        call write_bin(r2d, path, recnum)
        deallocate(r2d)
    endif
end subroutine write_field_double_2d

end module restart_mod
