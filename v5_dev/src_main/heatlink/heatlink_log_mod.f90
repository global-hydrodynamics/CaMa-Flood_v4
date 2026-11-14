module heatlink_log_mod
    use, intrinsic :: iso_fortran_env, only: output_unit
    use heatlink_config_mod, only: HEAT_LOG_FILE, LHEAT_DIAG
    implicit none
    private
    public :: HEAT_LOG_UNIT, init_heatlink_log, fin_heatlink_log, write_heatlink_time

    integer, protected, save :: HEAT_LOG_UNIT = output_unit ! [-] Heatlink log unit; stdout for standalone kernels.
    logical, save :: log_open = .FALSE. ! [-] Whether this module owns an open log file.
contains

subroutine init_heatlink_log(cama_log_unit)
    integer, intent(in) :: cama_log_unit ! [-] Existing CaMa log unit for file-opening errors.
    integer :: ios ! [-] I/O status from opening the heat log.
    logical :: already_open ! [-] Whether the requested file is already in use (including the CaMa log).
    character(len=512) :: message ! [-] Runtime I/O error description.

    if (log_open) call fin_heatlink_log()
    inquire(file=trim(HEAT_LOG_FILE), opened=already_open, iostat=ios)
    if (ios /= 0 .or. already_open .or. len_trim(HEAT_LOG_FILE) == 0) then
        write(cama_log_unit, '(a,1x,a)') 'ERROR: HEAT_LOG_FILE is empty, inaccessible or already open:', trim(HEAT_LOG_FILE)
        error stop 1
    endif
    open(newunit=HEAT_LOG_UNIT, file=trim(HEAT_LOG_FILE), status='replace', action='write', &
    &   iostat=ios, iomsg=message)
    if (ios /= 0) then
        write(cama_log_unit, '(a,1x,a,2a)') 'ERROR: cannot open HEAT_LOG_FILE:', trim(HEAT_LOG_FILE), ': ', trim(message)
        error stop 1
    endif
    log_open = .TRUE.
    write(cama_log_unit, '(a,1x,a)') 'HEAT-LINK log:', trim(HEAT_LOG_FILE)
    write(HEAT_LOG_UNIT, '(a,l1)') 'HEAT-LINK detailed monitoring: LHEAT_DIAG = ', LHEAT_DIAG
    write(HEAT_LOG_UNIT, '(a)') 'Calendar times follow the CaMa model clock. Interval end/duration values are seconds from run start.'
    if (.not. LHEAT_DIAG) write(HEAT_LOG_UNIT, '(a)') &
    &   'Detailed heat budgets are disabled; temperature checks and physical safeguards remain active.'
    write(HEAT_LOG_UNIT, '(a)') 'Water-temperature groups use end-of-update liquid-water volume [m3]:'
    write(HEAT_LOG_UNIT, '(a)') '  wet water temperature: volume > STO_IGNORE.'
    write(HEAT_LOG_UNIT, '(a)') '  dry water temperature: volume <= STO_IGNORE (dry or near-dry cells; retained temperature).'
end subroutine init_heatlink_log

subroutine write_heatlink_time(stage, step, date, hhmm)
    character(len=*), intent(in) :: stage ! [-] INIT, BEGIN, LOCAL_END or END marker.
    integer, intent(in) :: step ! [-] CaMa outer time-step counter at this marker.
    integer, intent(in) :: date ! [YYYYMMDD] Model calendar date from YOS_CMF_TIME.
    integer, intent(in) :: hhmm ! [HHMM] Model time from YOS_CMF_TIME; not wall-clock time.
    character(len=32) :: label ! [-] Human-readable position within the outer update.
    select case(stage)
    case('INIT')
        label = 'initialization'
    case('BEGIN')
        label = 'begin'
    case('LOCAL_END')
        label = ''
    case('END')
        label = 'end'
    case default
        label = stage
    end select
    write(HEAT_LOG_UNIT, '(i4.4,a,i2.2,a,i2.2,1x,i2.2,a,i2.2,a,i0,a)') &
    &   date/10000,'/',mod(date/100,100),'/',mod(date,100),hhmm/100,':',mod(hhmm,100), &
    &   '  step = ',step,trim('  '//trim(label))
end subroutine write_heatlink_time

subroutine fin_heatlink_log()
    if (log_open) close(HEAT_LOG_UNIT)
    log_open = .FALSE.
    HEAT_LOG_UNIT = output_unit
end subroutine fin_heatlink_log
end module heatlink_log_mod
