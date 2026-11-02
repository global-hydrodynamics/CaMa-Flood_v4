program test_input_mapping
    use, intrinsic :: iso_fortran_env, only: output_unit
    use PARKIND1, only: JPRM, JPRB
    use YOS_CMF_INPUT, only: LOGNAM, NX, NY, NLFP, LLEAPYR
    use YOS_CMF_MAP, only: NSEQMAX, I1SEQX, I1SEQY
    use inpmat_mod, only: Inpmat, load_inpmat_files, move_append_inpmat
    use dim_converter, only: init_dim_converter, get_inpmat_index, map2vec
    use input_conf_class, only: InputConf, init_InputConf
    use datetime_mod, only: date_hour2datetime
#ifdef UseCDF_CMF
    use netcdf
#endif
    implicit none
    type(Inpmat) :: mapping
    type(Inpmat), allocatable :: mappings(:)
    type(InputConf) :: conf
    real(kind=JPRM) :: source(2,2)
    real(kind=JPRB) :: vec(2)
    integer :: a, b, c, unit, nx_source
    character(len=64) :: mode
    call get_command_argument(1, mode)
    LOGNAM = output_unit
    NX = 2
    NY = 1
    NLFP = 1
    LLEAPYR = .TRUE.
    NSEQMAX = 2
    allocate(I1SEQX(2), I1SEQY(2))
    I1SEQX = [1,2]
    I1SEQY = [1,1]
    call init_dim_converter()
    source = reshape([10.0_JPRM,20.0_JPRM,30.0_JPRM,40.0_JPRM],[2,2])
    select case(trim(mode))
    case ('cache')
        a = get_inpmat_index('a.info', 'a.bin', 2, 2)
        b = get_inpmat_index('info-alias', './other/../alias.bin', 2, 2)
        call assert_true(a == b, 'alias cache reuse')
        b = get_inpmat_index('a.info', 'b.bin', 2, 2)
        call assert_true(a /= b, 'same metadata, different binary')
        c = get_inpmat_index('b.info', 'a.bin', 2, 2)
        call assert_true(a /= c, 'different metadata, same binary')
        c = get_inpmat_index('other/c.info', 'b.bin', 2, 2)
        call map2vec(source, vec, inpmat_idx=a)
        call check(vec, [17.5_JPRB,20.0_JPRB])
        call map2vec(source, vec, inpmat_idx=b)
        call check(vec, [10.0_JPRB,15.0_JPRB])
        call map2vec(source, vec, inpmat_idx=c)
        call check(vec, [10.0_JPRB,15.0_JPRB])
        call load_inpmat_files(mapping, 'a.info', 'a.bin')
        call move_append_inpmat(mappings, mapping)
        call assert_true(.not. allocated(mapping%inpx), 'moved ownership')
        call mappings(1)%map2vec_intrp(source, vec)
        call check(vec, [17.5_JPRB,20.0_JPRB])
        call init_dim_converter()
        a = get_inpmat_index('a.info', 'b.bin', 2, 2)
        call assert_true(a == 1, 'cache reset')
    case ('input', 'catm', 'netcdf')
#ifdef UseCDF_CMF
        if (trim(mode) == 'netcdf') call write_netcdf()
#endif
        open(newunit=unit, file='input.nml', status='old')
        conf = init_InputConf('TEST', unit, date_hour2datetime(20000101,0))
        close(unit)
        call conf%update_input(vec)
        if (trim(mode) == 'catm') then
            call check(vec, [10.0_JPRB,20.0_JPRB])
        else
            call check(vec, [17.5_JPRB,20.0_JPRB])
        endif
    case ('cache-shape')
        a = get_inpmat_index('a.info', 'a.bin', 2, 2)
        a = get_inpmat_index('a.info', 'a.bin', 3, 2)
    case default
        nx_source = 2
        if (trim(mode) == 'source-shape') nx_source = 3
        a = get_inpmat_index('a.info', 'a.bin', nx_source, 2)
    end select
    write(*,'(a)') 'INPUT_MAPPING_PASS'
contains
subroutine assert_true(value, message)
    logical, intent(in) :: value
    character(len=*), intent(in) :: message
    if (.not. value) then
        write(*,*) message
        error stop 1
    endif
end subroutine
subroutine check(actual, expected)
    real(kind=JPRB), intent(in) :: actual(2), expected(2)
    call assert_true(all(abs(actual-expected) < 1.e-5_JPRB), 'mapping result')
end subroutine
#ifdef UseCDF_CMF
subroutine nc_check(status)
    integer, intent(in) :: status
    if (status /= nf90_noerr) then
        write(*,*) nf90_strerror(status)
        error stop 1
    endif
end subroutine
subroutine write_netcdf()
    integer :: ncid, xdim, ydim, tdim, xv, yv, tv, v
    real(kind=JPRM) :: data(2,2,2)
    data(:,:,1) = source
    data(:,:,2) = source
    call nc_check(nf90_create('forcing.nc', nf90_clobber, ncid))
    call nc_check(nf90_def_dim(ncid, 'lon', 2, xdim))
    call nc_check(nf90_def_dim(ncid, 'lat', 2, ydim))
    call nc_check(nf90_def_dim(ncid, 'time', 2, tdim))
    call nc_check(nf90_def_var(ncid, 'lon', nf90_float, [xdim], xv))
    call nc_check(nf90_def_var(ncid, 'lat', nf90_float, [ydim], yv))
    call nc_check(nf90_def_var(ncid, 'time', nf90_float, [tdim], tv))
    call nc_check(nf90_put_att(ncid, tv, 'units', 'hours since 2000-01-01 00:00:00'))
    call nc_check(nf90_put_att(ncid, tv, 'calendar', 'standard'))
    call nc_check(nf90_def_var(ncid, 'forcing', nf90_float, [xdim,ydim,tdim], v))
    call nc_check(nf90_enddef(ncid))
    call nc_check(nf90_put_var(ncid, xv, [0.5_JPRM,1.5_JPRM]))
    call nc_check(nf90_put_var(ncid, yv, [1.5_JPRM,0.5_JPRM]))
    call nc_check(nf90_put_var(ncid, tv, [0.0_JPRM,1.0_JPRM]))
    call nc_check(nf90_put_var(ncid, v, data))
    call nc_check(nf90_close(ncid))
end subroutine
#endif
end program
