program test_heat_step_monitor
    use PARKIND1, only: JPRD
    !$ use omp_lib, only: omp_set_num_threads
    use heat_step_monitor_mod
    implicit none
    type(HeatStepState) :: state
    type(HeatStepLedger) :: ledger
    type(HeatStepStats) :: stats
    real(kind=JPRD) :: delta, scale, naive
    integer :: unit, i, threads, ios, undefined_count, heading_count
    integer, parameter :: n = 33003
    real(kind=JPRD) :: values(n), volume(n), theta(n), reference_scale
    character(len=512) :: line

    ! Cancellation crosses fixed-block boundaries; n also leaves a partial final block.
    volume = 1.0_JPRD
    theta = 0.0_JPRD
    do i = 1,n,3
        values(i:i+2) = [1.0e16_JPRD,1.0_JPRD,-1.0e16_JPRD]
    enddo
    call capture_heat_step(state,volume,theta)
    do threads = 1,8
        !$ call omp_set_num_threads(threads)
        call check(step_sum(values) == real(n/3,JPRD),'cross-block cancellation independent of threads')
        call measure_heat_step(state,volume,values,1.0_JPRD,1.0_JPRD,delta,scale,naive)
        call check(delta == real(n/3,JPRD),'parallel increment retains small terms')
        if (threads == 1) reference_scale = scale
        call check(scale == reference_scale,'deterministic parallel storage scale')
    enddo
    call check(step_sum(values(:0)) == 0.0_JPRD,'empty diagnostic domain')
    call check(step_sum([1.0e16_JPRD,1.0_JPRD,-1.0e16_JPRD]) == 1.0_JPRD,'compensated cancellation')
    call capture_heat_step(state,[1.0e16_JPRD,1.0_JPRD],[1.0_JPRD,0.0_JPRD])
    call measure_heat_step(state,[1.0e16_JPRD,1.0_JPRD],[1.0_JPRD,1.0_JPRD], &
    &   1.0_JPRD,1.0_JPRD,delta,scale,naive)
    call check(delta == 1.0_JPRD .and. naive == 0.0_JPRD,'small increment beside large unchanged storage')

    ! Heating and volume change: 4*5*2 - 4*3*1 = 28 J.
    call capture_heat_step(state,[3.0_JPRD],[1.0_JPRD])
    call measure_heat_step(state,[5.0_JPRD],[2.0_JPRD],4.0_JPRD,2.0_JPRD,delta,scale,naive)
    call check(delta == 28.0_JPRD .and. scale == 40.0_JPRD,'product increment')
    ! Sensible heat -4 J offsets latent heat +4 J during melting.
    call capture_heat_step(state,[1.0_JPRD],[1.0_JPRD],[2.0_JPRD],[3.0_JPRD])
    call measure_heat_step(state,[1.0_JPRD],[0.0_JPRD],4.0_JPRD,2.0_JPRD,delta,scale,naive, &
    &   [1.0_JPRD],[2.0_JPRD])
    call check(delta == 0.0_JPRD .and. scale == 14.0_JPRD,'latent and sensible cancellation')

    open(newunit=unit,status='scratch',action='readwrite')
    call add_step_heat(ledger,10.0_JPRD,10.0_JPRD,2.0_JPRD,2.0_JPRD)
    call monitor_heat_step(stats,unit,'test',1.0_JPRD,1.0_JPRD,9.0_JPRD,100.0_JPRD,9.0_JPRD,ledger)
    call check(stats%min_record(9,2) == 1.0_JPRD,'first positive minimum is not zero')
    ledger = HeatStepLedger()
    call add_step_heat(ledger,1.0_JPRD,1.0_JPRD,1.0_JPRD,1.0_JPRD)
    call monitor_heat_step(stats,unit,'test',2.0_JPRD,1.0_JPRD,0.5_JPRD,100.0_JPRD,0.5_JPRD,ledger)
    call check(stats%max_step(2) == 1 .and. stats%max_step(6) == 2,'independent error and ratio extrema')
    ledger = HeatStepLedger()
    call add_step_heat(ledger,-4.0_JPRD,4.0_JPRD,-2.0_JPRD,2.0_JPRD)
    call monitor_heat_step(stats,unit,'test',3.0_JPRD,1.0_JPRD,-3.0_JPRD,100.0_JPRD,-3.0_JPRD,ledger)
    call check(stats%min_record(9,2) == -1.0_JPRD .and. stats%min_step(2) == 3,'negative adjusted error')
    call check(stats%min_record(1,2) == 3.0_JPRD,'extreme metadata follows extreme')
    ledger = HeatStepLedger()
    call monitor_heat_step(stats,unit,'test',4.0_JPRD,1.0_JPRD,5.0_JPRD,0.0_JPRD,5.0_JPRD,ledger)
    call check(stats%no_exchange_ratio == 1 .and. stats%no_storage_ratio == 1,'undefined ratios counted')
    call check(stats%samples(6) == 3 .and. stats%max_step(2) == 4,'undefined ratio excludes only relative extrema')
    call write_heat_step_extrema(unit,'test',stats)
    ledger = HeatStepLedger()
    call add_step_heat(ledger,1.0e16_JPRD,1.0e16_JPRD,0.0_JPRD,0.0_JPRD)
    call add_step_heat(ledger,1.0_JPRD,1.0_JPRD,0.0_JPRD,0.0_JPRD)
    call add_step_heat(ledger,-1.0e16_JPRD,1.0e16_JPRD,0.0_JPRD,0.0_JPRD)
    call check(ledger%value(1)+ledger%correction(1) == 1.0_JPRD,'interval ledger compensation')
    ledger = HeatStepLedger()
    call add_step_heat(ledger,2.0_JPRD,2.0_JPRD,0.0_JPRD,0.0_JPRD)
    call check(ledger%value(1)+ledger%correction(1) == 2.0_JPRD,'interval reset')
    rewind(unit)
    undefined_count = 0
    heading_count = 0
    do
        read(unit,'(a)',iostat=ios) line
        if (ios /= 0) exit
        call check(index(line,'HEAT_') == 0,'no machine record tags')
        if (index(line,'undefined') > 0) undefined_count = undefined_count + 1
        if (trim(line) == '[test]') heading_count = heading_count + 1
    enddo
    call check(undefined_count >= 2 .and. heading_count == 5,'readable headings and undefined ratios')
    close(unit)
    print *, 'PASS test_heat_step_monitor'
contains
subroutine check(ok, label)
    logical, intent(in) :: ok
    character(len=*), intent(in) :: label
    if (.not. ok) then
        print *, 'FAIL: ',label
        stop 1
    endif
end subroutine
end program
