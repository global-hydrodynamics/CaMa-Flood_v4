program test_noleap_calendar
    use parkind1
    use yos_cmf_input
    use yos_cmf_time
    use yos_cmf_map
    use yos_cmf_prog
    use cmf_ctrl_time_mod, only: CMF_TIME_NMLIST, CMF_TIME_INIT, CMF_TIME_NEXT, CMF_TIME_UPDATE
    use cmf_ctrl_forcing_mod, only: CMF_FORCING_NMLIST, CMF_FORCING_INIT, CMF_FORCING_GET, CMF_FORCING_END
    use cmf_ctrl_restart_mod, only: CMF_RESTART_INIT, CMF_RESTART_WRITE, &
    &   CRESTSTO, CRESTDIR, CVNREST, LRESTDBL, LRESTCDF, IFRQ_RST, restart_is_write_time
    use cmf_ctrl_output_mod, only: CMF_OUTPUT_NMLIST, CMF_OUTPUT_INIT, CMF_OUTPUT_WRITE, CMF_OUTPUT_END
    use datetime_mod, only: date_hour2datetime
    use input_conf_class, only: InputConf, init_InputConf
    use restart_mod, only: read_restart, write_restart
    implicit none
    type(InputConf) :: atm, annual
    real(kind=JPRB) :: forcing(2,2,2), air(4), binary(4)
    real(kind=JPRD) :: state(4), recovered(4), expected_state
    real(kind=JPRB) :: expected_value
    integer :: settings, expected, step, date, hour, rof_rec, atm_rec, bin_rec, n, last_date, last_hour
    logical :: resume, netcdf_restart, found
    character(len=1024) :: next_file

    open(newunit=settings,file='settings.txt',status='old')
    read(settings,*) LLEAPYR, DT, DTIN, resume, netcdf_restart
    close(settings)
    LOGNAM=6
    CSETFILE='input_cmf.nam'
    NX=2; NY=2; NLFP=1; NXIN=2; NYIN=2; NSEQMAX=4
    INPN=1; REGIONTHIS=1; NPTHOUT=0; NPTHLEV=0
    LROSPLIT=.false.; LWEVAP=.false.; LSTOONLY=.true.
    LPTHOUT=.false.; LDAMOUT=.false.; LLEVEE=.false.; LGDWDLY=.false.; LOUTINI=.false.
    RMIS=1.e20_JPRM; DMIS=1.e20_JPRD; CSUFBIN='.bin'; CSUFCDF='.nc'
    allocate(I1SEQX(4),I1SEQY(4),D1LON(2),D1LAT(2))
    I1SEQX=[1,2,1,2]; I1SEQY=[1,1,2,2]; D1LON=[0.5,1.5]; D1LAT=[1.5,0.5]
    allocate(P2RIVSTO(4,1),P2FLDSTO(4,1),D2COPY(4,1))
    allocate(D2RIVOUT(4,1),D2FLDOUT(4,1),D2RIVOUT_PRE(4,1),D2FLDOUT_PRE(4,1), &
    &   D2RIVDPH_PRE(4,1),D2FLDSTO_PRE(4,1))
    call CMF_TIME_NMLIST
    call CMF_FORCING_NMLIST
    call CMF_TIME_INIT
    call CMF_FORCING_INIT
    open(newunit=settings,file='input_cmf.nam',status='old')
    atm=init_InputConf('TAIR',settings,date_hour2datetime(IYYYYMMDD,IHOUR))
    annual=init_InputConf('ANNUAL',settings,date_hour2datetime(IYYYYMMDD,IHOUR))
    close(settings)
    CRESTDIR='./'; CVNREST='restart'; LRESTDBL=.true.; LRESTCDF=netcdf_restart; IFRQ_RST=0
    state=0._JPRD; P2RIVSTO=0._JPRD; P2FLDSTO=0._JPRD
    if (resume) then
        CRESTSTO='resume.core'
        call CMF_RESTART_INIT
        call read_restart('STATE',date_hour2datetime(IYYYYMMDD,IHOUR),found,state)
        if (.not.found .or. state(1)/=P2RIVSTO(1,1)) error stop 'restart states disagree'
    endif
    call CMF_OUTPUT_NMLIST
    call CMF_OUTPUT_INIT
    open(newunit=expected,file='expected.txt',status='old')
    read(expected,*) n
    if (n/=NSTEPS) error stop 'NSTEPS'
    do step=1,NSTEPS
        read(expected,*) date,hour,rof_rec,atm_rec,bin_rec,expected_value
        if (IYYYYMMDD/=date .or. IHOUR/=hour .or. IMIN/=0) error stop 'model datetime'
        if (step>1 .and. date/10000/=last_date/10000) then
            ! Legacy annual binaries have a fixed path per run. Reinitialize
            ! at a year boundary just as an annual run/restart script does.
            open(newunit=settings,file='input_cmf.nam',status='old')
            annual=init_InputConf('ANNUAL',settings,date_hour2datetime(date,hour))
            close(settings)
        endif
        if (step>1 .and. date/100/=last_date/100) then
            write(next_file,'(a,i6.6,a)') 'atm-',date/100,'.nc'
            inquire(file=trim(next_file),exist=found)
            if (found) call atm%open_next_file(trim(next_file),date_hour2datetime(date,hour))
        endif
        if (mod(step-1,int(DTIN/DT))==0) call CMF_FORCING_GET(forcing)
        if (atm%update_needed(int(DT)*(step-1))) call atm%update_input(air)
        if (annual%update_needed(int(DT)*(step-1))) then
            if (annual%get_rec()/=bin_rec) error stop 'annual binary record'
            call annual%update_input(binary)
        endif
        if (abs(forcing(1,1,1)-expected_value)>1.e-4_JPRB) error stop 'runoff value/date'
        if (abs(air(1)-expected_value)>1.e-4_JPRB) error stop 'atmosphere value/date'
        ! Binary fixture stores record numbers, exposing a one-day offset.
        if (binary(1)/=real(bin_rec,JPRB)) error stop 'binary value'
        if (atm%get_rec()-1/=atm_rec) error stop 'atmosphere record index'
        state=state+real(forcing(1,1,1),JPRD)+real(air(1),JPRD)
        P2RIVSTO(:,1)=state(:)
        call CMF_TIME_NEXT
        call CMF_OUTPUT_WRITE
        call CMF_TIME_UPDATE
        last_date=date; last_hour=hour
    enddo
    read(expected,*) last_date,last_hour,expected_state
    if (IYYYYMMDD/=last_date .or. IHOUR/=last_hour) error stop 'end datetime'
    if (abs(state(1)-expected_state)>1.e-4_JPRD) error stop 'accumulated state'
    if (.not.restart_is_write_time()) error stop 'restart schedule at final step'
    call CMF_RESTART_WRITE
    call write_restart('STATE',date_hour2datetime(IYYYYMMDD,IHOUR),state)
    call read_restart('STATE',date_hour2datetime(IYYYYMMDD,IHOUR),found,recovered)
    if (.not.found .or. any(recovered/=state)) error stop 'binary restart round trip'
    call CMF_OUTPUT_END
    call CMF_FORCING_END
    write(*,'(a)') 'NOLEAP_CALENDAR_PASS'
end program test_noleap_calendar
