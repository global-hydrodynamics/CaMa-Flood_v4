program test_input_slice
    use, intrinsic :: iso_fortran_env, only: output_unit
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
    use PARKIND1, only: JPRM, JPRB
    use YOS_CMF_INPUT, only: LOGNAM, NX, NY, NLFP, LLEAPYR
    use YOS_CMF_MAP, only: NSEQMAX, I1SEQX, I1SEQY
    use input_conf_class, only: InputConf, init_InputConf
    use nc_mod, only: NCConfig, init_ncconfig, read_nc, handle_error
    use netcdf, only: nf90_close
    use datetime_mod, only: date_hour2datetime
    use dim_converter, only: init_dim_converter
    implicit none
    type(InputConf) :: conf
    type(NCConfig) :: ncconf
    real(kind=JPRB) :: vec(6)
    real(kind=JPRM) :: expected(6)
    real(kind=JPRM), allocatable :: field(:,:)
    integer :: unit, reference, settings, frames, frame, count, requested, resolved, index, record, leap
    logical :: file_is_end
    character(len=1024) :: mode, path, varname, dimname, arg

    LOGNAM = output_unit
    LLEAPYR = .TRUE.
    call get_command_argument(1, mode)
    if (mode == 'read') then
        call get_command_argument(2, path)
        call get_command_argument(3, varname)
        call get_command_argument(4, dimname)
        call get_command_argument(5, arg)
        read(arg,*) index
        call get_command_argument(6, arg)
        read(arg,*) record
        call get_command_argument(7, arg)
        read(arg,*) leap
        LLEAPYR = leap == 1
        ncconf = init_ncconfig(trim(path), trim(varname), trim(dimname), index)
        allocate(field(ncconf%shape(ncconf%horizontal_dimpos(1)), ncconf%shape(ncconf%horizontal_dimpos(2))))
        call read_nc(field, file_is_end, ncconf, record)
        if (file_is_end) error stop 'unexpected end of file'
        open(newunit=unit, file='actual.bin', access='stream', form='unformatted', status='replace')
        write(unit) field
        close(unit)
        call handle_error(nf90_close(ncconf%ncid))
    else
        NX = 3
        NY = 2
        NLFP = 1
        NSEQMAX = 6
        allocate(I1SEQX(6), I1SEQY(6))
        I1SEQX = [1,2,3,1,2,3]
        I1SEQY = [1,1,1,2,2,2]
        call init_dim_converter()
        open(newunit=unit, file='input.nml', status='old')
        conf = init_InputConf('TEST', unit, date_hour2datetime(20000101,0))
        close(unit)
        open(newunit=settings, file='expected.txt', status='old')
        read(settings,*) frames
        open(newunit=reference, file='expected.bin', access='stream', form='unformatted', status='old')
        do frame = 1, frames
            if (frame == 3) call conf%open_next_file('next.nc', date_hour2datetime(20000101,2))
            read(settings,*) count, requested, resolved
            if (conf%get_slice_count() /= count) error stop 'slice_count'
            if (conf%get_slice_index() /= requested) error stop 'slice_index was overwritten'
            if (conf%get_slice_index_resolved() /= resolved) error stop 'slice_index_resolved'
            if (conf%get_next_t() /= (frame-1)*3600) error stop 'time schedule'
            read(reference) expected
            call conf%update_input(vec)
            do index = 1, 6
                if (ieee_is_nan(expected(index))) then
                    if (.not. ieee_is_nan(vec(index))) error stop 'missing NaN changed'
                else
                    if (ieee_is_nan(vec(index))) error stop 'unexpected NaN'
                    if (abs(vec(index)-real(expected(index),JPRB)) > &
                    &   max(1.e-5_JPRB, abs(real(expected(index),JPRB))*1.e-6_JPRB)) then
                        write(*,*) 'mismatch', frame, index, vec(index), expected(index)
                        error stop 'input field mismatch'
                    endif
                endif
            enddo
        enddo
        close(reference)
        close(settings)
    endif
    write(*,'(a)') 'INPUT_SLICE_PASS'
end program test_input_slice
