program test_heatlink_log
    use heatlink_config_mod, only: HEAT_LOG_FILE, LHEAT_DIAG
    use heatlink_log_mod, only: HEAT_LOG_UNIT, init_heatlink_log, fin_heatlink_log, write_heatlink_time
    implicit none
    integer :: unit, ios, found ! [-] Reader unit, I/O status and count of expected calendar markers.
    character(len=512) :: line ! [-] One diagnostic log record.
    character(len=16) :: scenario ! [-] Optional expected-error scenario.

    call get_command_argument(1, scenario)
    if (trim(scenario) == 'collision') then
        HEAT_LOG_FILE = 'test_heatlink_log_collision.tmp'
        open(newunit=unit, file=trim(HEAT_LOG_FILE), status='replace')
        write(unit, '(a)') 'CaMa log must be preserved'
        flush(unit)
        call init_heatlink_log(unit)
        error stop 'Collision was accepted'
    else if (trim(scenario) == 'bad-path') then
        HEAT_LOG_FILE = 'missing-heatlink-directory/test.log'
        call init_heatlink_log(6)
        error stop 'Inaccessible path was accepted'
    endif

    HEAT_LOG_FILE = 'test_heatlink_log.tmp'
    LHEAT_DIAG = .TRUE.
    call init_heatlink_log(6)
    call write_heatlink_time('BEGIN', 10, 20001231, 2330)
    call write_heatlink_time('END', 11, 20010101, 0)
    write(HEAT_LOG_UNIT, '(a)') 'sentinel heat diagnostic'
    call fin_heatlink_log()
    open(newunit=unit, file=trim(HEAT_LOG_FILE), status='old', action='read')
    found = 0
    do
        read(unit, '(a)', iostat=ios) line
        if (ios /= 0) exit
        if (trim(line) == '2000/12/31 23:30  step = 10  begin') found = found + 1
        if (trim(line) == '2001/01/01 00:00  step = 11  end') found = found + 1
        if (trim(line) == 'sentinel heat diagnostic') found = found + 1
    enddo
    close(unit, status='delete')
    if (found /= 3) error stop 'Separate log or calendar formatting is incorrect'
    ! Reopening must reset ownership, and the default minimal header must be explicit.
    LHEAT_DIAG = .FALSE.
    call init_heatlink_log(6)
    call fin_heatlink_log()
    open(newunit=unit, file=trim(HEAT_LOG_FILE), status='old', action='read')
    read(unit, '(a)') line
    if (trim(line) /= 'HEAT-LINK detailed monitoring: LHEAT_DIAG = F') error stop 'Minimal header is incorrect'
    close(unit, status='delete')
    write(*, '(a)') 'test_heatlink_log: PASS'
end program test_heatlink_log
