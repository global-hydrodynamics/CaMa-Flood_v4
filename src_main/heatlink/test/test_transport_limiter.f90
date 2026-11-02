program test_transport_limiter
    use PARKIND1, only: JPRB, JPRD
    use YOS_CMF_MAP, only: I1NEXT, NSEQALL, NSEQRIV, NPTHOUT, D2RIVWTH, D2RIVLEN
    use phys_const_mod, only: TMELT, RW, CW
    use river_water_advection_mod, only: advect_river_water_sensible_heat
    use river_ice_advection_mod, only: advect_river_surface_ice
    use heat_residual_mod, only: HeatConservationStats, record_heat_exchange, write_heat_conservation
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    implicit none
    real(kind=JPRB) :: temperature(2), flow(2), ice(2), fraction(2), runoff(2), inflow_t(2)
    real(kind=JPRD) :: volume(2), after(2), error(2), throughput(2), net, absolute, exported, capacity, expected
    type(HeatConservationStats) :: audit
    integer :: i, unit
    character(len=2048) :: line
    real(kind=JPRD) :: values(13)
    character(len=16) :: mode

    call get_command_argument(1, mode)
    NSEQALL = 2
    NSEQRIV = 1
    NPTHOUT = 0
    allocate(I1NEXT(2), D2RIVWTH(2,1), D2RIVLEN(2,1))
    I1NEXT = [2, -9]
    D2RIVWTH = 1.0_JPRB
    D2RIVLEN = 1.0_JPRB
    capacity = real(CW,JPRD)*real(RW,JPRD)
    volume = 1.0_JPRD
    ! Keep a positive subnormal request. The old quotient overflows in double precision.
    flow = [tiny(1.0_JPRB)*0.001_JPRB, 0.0_JPRB]
    call check(flow(1)>0.0_JPRB, 'test requires gradual underflow')
    if (mode == 'water' .or. mode == '') then
        temperature = [TMELT+10.0_JPRB, TMELT]
        call advect_river_water_sensible_heat(temperature,volume,volume,flow,1.0_JPRB, &
        &   heat_budget_error_j=error,heat_throughput_j=throughput,external_heat_j=net,external_heat_absolute_j=absolute)
        call check(all(ieee_is_finite(temperature)), 'finite tiny-flow water temperature')
        call check(temperature(1) == TMELT+10.0_JPRB.and.temperature(2) == TMELT, 'tiny exchange preserves resolved temperatures')
        call check(all(throughput>0.0_JPRD), 'positive tiny heat transport is retained')
        call check(net == 0.0_JPRD.and.absolute == 0.0_JPRD, 'internal heat flow is not an external input')
        call check(all(ieee_is_finite(error)), 'finite tiny-flow heat diagnostic')
    endif
    if (mode == 'ice' .or. mode == '') then
        ice = [1.0_JPRB,0.0_JPRB]
        fraction = 0.0_JPRB
        call advect_river_surface_ice(ice,fraction,volume,flow,1.0_JPRB,ice_budget_error_m3=error, &
        &   exported_ice_volume_m3=exported)
        call check(all(ieee_is_finite(ice)), 'finite tiny-flow ice volume')
        call check(ice(1) == 1.0_JPRB.and.ice(2) == flow(1), 'positive tiny ice transfer is retained')
        call check(exported == 0.0_JPRD, 'internal ice flow is not a boundary export')
        call check(all(ieee_is_finite(error)), 'finite tiny-flow ice diagnostic')
    endif

    ! Check the signs and units of actual boundary transfers, including limited ice export.
    do i = -1, 1, 2
        temperature = TMELT+10.0_JPRB
        flow = [0.0_JPRB,real(i,JPRB)*0.25_JPRB]
        after = [1.0_JPRD,1.0_JPRD-real(flow(2),JPRD)]
        call advect_river_water_sensible_heat(temperature,volume,after,flow,1.0_JPRB, &
        &   external_heat_j=net,external_heat_absolute_j=absolute)
        expected = -real(i,JPRD)*0.25_JPRD*capacity*10.0_JPRD
        call check(abs(net-expected) <= epsilon(1.0_JPRD)*abs(expected), 'mouth heat exchange sign and magnitude')
        call check(absolute == abs(net), 'mouth absolute exchange')
    enddo
    temperature = TMELT
    flow = 0.0_JPRB
    runoff = [0.25_JPRB,0.0_JPRB]
    inflow_t = TMELT+5.0_JPRB
    after = [1.25_JPRD,1.0_JPRD]
    call advect_river_water_sensible_heat(temperature,volume,after,flow,1.0_JPRB, &
    &   runoff_flow_m3s=runoff,inflow_temperature_k=inflow_t,external_heat_j=net,external_heat_absolute_j=absolute)
    call check(net == capacity*0.25_JPRD*5.0_JPRD.and.absolute == net, 'runoff heat is external input')
    do i = -1, 1, 2
        ice = [0.0_JPRB,1.0_JPRB]
        fraction = 0.0_JPRB
        flow = [0.0_JPRB,real(i,JPRB)*2.0_JPRB]
        call advect_river_surface_ice(ice,fraction,volume,flow,1.0_JPRB,exported_ice_volume_m3=exported)
        expected = real(max(i,0),JPRD)
        call check(exported == expected, 'ice boundary uses limited export and no ocean import')
        call check(ice(2) == 1.0_JPRB-real(expected,JPRB), 'exported ice matches lost source ice')
    enddo

    audit%initial_j = -10.0_JPRD
    call record_heat_exchange(audit,1,100.0_JPRD,100.0_JPRD)
    call record_heat_exchange(audit,2,10.0_JPRD,10.0_JPRD)
    call record_heat_exchange(audit,3,-20.0_JPRD,20.0_JPRD)
    open(newunit=unit,status='scratch',form='formatted')
    call write_heat_conservation(unit,audit,79.0_JPRD,1.0_JPRD)
    rewind(unit)
    do i = 1,6
        read(unit,'(a)') line
    enddo
    close(unit)
    read(line(index(line,'=')+1:index(line,';')-1),*) values(10)
    read(line(index(line,'adjusted = ')+11:),*) values(11)
    call check(values(10) == -1.0_JPRD.and.values(11) == 0.0_JPRD, 'raw deficit plus unapplied heat closes')
    audit = HeatConservationStats()
    call record_heat_exchange(audit,1,1.0e16_JPRD,1.0e16_JPRD)
    do i = 1, 100
        call record_heat_exchange(audit,1,1.0_JPRD,1.0_JPRD)
    enddo
    call record_heat_exchange(audit,1,-1.0e16_JPRD,1.0e16_JPRD)
    call check(audit%net_j(1) == 100.0_JPRD, 'compensated annual sum retains small increments')
    write(*,'(a)') '[ALL TESTS PASSED] test_transport_limiter'
contains
subroutine check(ok,label)
    logical, intent(in) :: ok
    character(len=*), intent(in) :: label
    if (ok) return
    write(*,'(a)') '[TEST FAILED] '//label
    error stop 1
end subroutine
end program
