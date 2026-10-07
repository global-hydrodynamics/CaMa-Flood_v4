program test_temperature_dry_state
    use PARKIND1, only: JPIM, JPRB, JPRD, JPIB
    use YOS_CMF_MAP, only: I1NEXT, NSEQALL, NSEQRIV, NPTHOUT
    use const_mod, only: STO_IGNORE
    use phys_const_mod, only: CW, RW, RI, HFUS, TMELT
    use river_water_advection_mod, only: advect_river_water_sensible_heat
    use river_ice_advection_mod, only: diagnose_surface_ice_transport_fraction
    use heat_budget_mod, only: update_local_water_ice_state, liquid_water_energy_j, &
    &   water_ice_energy_j, update_liquid_temperature_no_phase_change, apply_liquid_temperature_floor
    use heat_residual_mod, only: HeatResidualStats, record_heat_residual
    !$ use omp_lib, only: omp_set_num_threads
    implicit none
    real(kind=JPRD), parameter :: capacity = real(CW,JPRD)*real(RW,JPRD)
    real(kind=JPRB), parameter :: tol = 8.0_JPRB * epsilon(1.0_JPRB)

    NSEQALL = 2
    NSEQRIV = 1
    NPTHOUT = 0
    allocate(I1NEXT(2))
    I1NEXT = [2, -9]
    call observed_events()
    call near_complete_drainage()
    call dry_and_rewet()
    call phase_cycles()
    call local_heating_and_floor()
    call residual_ledger()
    call parallel_residual_ledger()
    call floor_without_diagnostics()
    write(*, '(a)') '[ALL TESTS PASSED] test_temperature_dry_state'
contains
subroutine check(ok, label)
    logical, intent(in) :: ok
    character(len=*), intent(in) :: label
    if (ok) return
    write(*, '(a)') '[TEST FAILED] '//label
    error stop 1
end subroutine

subroutine observed_events()
    real(kind=JPRB) :: t(2), q(2), dt, initial(2)
    real(kind=JPRD) :: before(2), after(2), u(2), err(2), volume(2), flow(2), old_t(2), tiny_volume(2)
    integer :: i
    ! Recorded states at the two MIROC6 internal steps that produced 552 K and 399 K.
    old_t = [278.3769003973869_JPRD, 284.60214717092794_JPRD]
    volume = [0.37984393587789056_JPRD, 0.3587431625008731_JPRD]
    tiny_volume = [1.1645828281248855e-18_JPRD, 1.2344025586910723e-18_JPRD]
    flow = [0.0011606342139261817_JPRD, 0.0010961596305290096_JPRD]
    dt = real(327.2727370262146_JPRD,JPRB)
    do i = 1, 2
        t = real(old_t(i),JPRB)
        initial = t
        q = [real(flow(i),JPRB), 0.0_JPRB]
        before = [volume(i), 100.0_JPRD]
        after = [tiny_volume(i), 100.0_JPRD+real(q(1),JPRD)*real(dt,JPRD)]
        call advect_river_water_sensible_heat(t,before,after,q,dt, &
        &   unapplied_sensible_heat_j=u,heat_budget_error_j=err)
        call check(t(1) == initial(1), 'observed dry event retains previous temperature exactly')
        call check(abs(t(2)-initial(2)) <= tol*initial(2), 'observed downstream temperature stays uniform')
        call check(maxval(abs(err)) < 1.0e-15_JPRD, 'observed event ledger closes')
        write(*,'(a,i0,3(1x,es24.16))') 'OBSERVED_EVENT_FIXED ',i,t(1),after(1),u(1)
    enddo
end subroutine

subroutine near_complete_drainage()
    real(kind=JPRB) :: t(2), q(2), dt
    real(kind=JPRD) :: before(2), after(2), u(2)
    integer :: i, tested, above_threshold
    dt = 360.0_JPRB
    tested = 0
    above_threshold = 0
    do i = 1, 1000
        t = TMELT + 5.0_JPRB
        before = [0.01_JPRD+real(i,JPRD)/13.0_JPRD, 100.0_JPRD]
        q = [nearest(real(before(1),JPRB)/dt,-1.0_JPRB),0.0_JPRB]
        ! Match differing JPRB flow-product and JPRD storage arithmetic.
        after = [before(1)-real(q(1)*dt,JPRD),before(2)+real(q(1)*dt,JPRD)]
        if (after(1) <= 0.0_JPRD) cycle
        tested = tested+1
        if (after(1)>STO_IGNORE) above_threshold = above_threshold+1
        call advect_river_water_sensible_heat(t,before,after,q,dt,unapplied_sensible_heat_j=u)
        call check(t(1) == TMELT+5.0_JPRB, 'near-complete drainage retains uniform source temperature')
        call check(abs(t(2)-(TMELT+5.0_JPRB)) <= tol*TMELT, 'near-complete drainage receiver stays uniform')
    enddo
    call check(tested>500, 'near-complete drainage covers enough representable cases')
    if (epsilon(1.0_JPRB)>1.0e-10_JPRB) call check(above_threshold>100, 'single precision wet roundoff cases exercised')
    write(*,'(a,2(1x,i0))') 'NEAR_DRAINAGE_TESTED total wet:', tested,above_threshold
end subroutine

subroutine dry_and_rewet()
    real(kind=JPRB) :: t(2), q(2), runoff(2), tin(2)
    real(kind=JPRD) :: before(2), after(2), u(2), expected, volumes(4)
    integer :: i
    volumes = [0.0_JPRD,0.5_JPRD*STO_IGNORE,STO_IGNORE,2.0_JPRD*STO_IGNORE]
    do i = 1, 4
        t = 290.0_JPRB
        tin = 280.0_JPRB
        before = 0.0_JPRD
        q = 0.0_JPRB
        runoff = [real(volumes(i),JPRB),0.0_JPRB]
        ! Use exact storage thresholds even with JPRB runoff rounding.
        after = [volumes(i),0.0_JPRD]
        call advect_river_water_sensible_heat(t,before,after,q,1.0_JPRB, &
        &   runoff_flow_m3s=runoff,inflow_temperature_k=tin,unapplied_sensible_heat_j=u)
        if (i <= 3) then
            call check(t(1) == 290.0_JPRB, 'zero/below/exact threshold retains warm memory')
            expected = capacity*real(runoff(1),JPRD)*real(tin(1)-TMELT,JPRD) &
            &   -capacity*after(1)*real(t(1)-TMELT,JPRD)
            call check(abs(u(1)-expected)<1.0e-14_JPRD, 'dry rewet signed residual uses actual represented heat')
            if (i>1) call check(u(1)<0.0_JPRD, 'cool water plus warm dry memory gives negative residual')
        else
            call check(abs(t(1)-280.0_JPRB) <= tol*280.0_JPRB, 'wet inflow does not inherit old dry temperature')
        endif
        before = after
        call advect_river_water_sensible_heat(t,before,after,q,1.0_JPRB,unapplied_sensible_heat_j=u)
        call check(all(u == 0.0_JPRD), 'unchanged dry state does not count prior residual again')
    enddo
    call check(diagnose_surface_ice_transport_fraction(STO_IGNORE,1.0_JPRD) == 0.0_JPRD, 'dry ice transport is zero')
    call check(diagnose_surface_ice_transport_fraction(0.5_JPRD*STO_IGNORE,1.0_JPRD) == 0.0_JPRD, 'tiny ice transport is zero')
    call check(diagnose_surface_ice_transport_fraction(2.0_JPRD*STO_IGNORE,STO_IGNORE) == 0.5_JPRD, 'wet ice transport remains active')
end subroutine

subroutine phase_step(v,t,ice,qw,qi,u,dry)
    real(kind=JPRB), intent(inout) :: v,t,ice
    real(kind=JPRB), intent(in) :: qw,qi
    real(kind=JPRB), intent(out) :: u,dry
    real(kind=JPRB) :: excess,frozen,melted,melted_excess,merr,eerr,negative,old_energy,scale
    logical :: valid,nonfinite
    excess = 0.0_JPRB
    old_energy = water_ice_energy_j(v,t,ice,TMELT)
    scale = max(abs(old_energy),abs(qw),abs(qi),tiny(1.0_JPRB))
    call update_local_water_ice_state(v,t,ice,excess,qw,qi,0.0_JPRB, &
    &   frozen,melted,melted_excess,u,merr,eerr,valid,nonfinite,negative,dry)
    call check(valid.and..not.nonfinite, 'phase step accepts finite positive state')
    call check(abs(eerr) <= max(1.0e-11_JPRB,64.0_JPRB*epsilon(1.0_JPRB))*scale, 'phase energy plus signed residual closes')
    call check(abs(merr) <= 32.0_JPRB*epsilon(1.0_JPRB)*max(RW*v+RI*ice,tiny(1.0_JPRB)), 'phase mass closes')
end subroutine

subroutine phase_cycles()
    real(kind=JPRB) :: v,t,ice,u,dry,qw,qi,old_v,expected_t
    v = 0.0_JPRB
    t = 290.0_JPRB
    ice = 0.0_JPRB
    call phase_step(v,t,ice,10.0_JPRB,20.0_JPRB,u,dry)
    call check(u == 30.0_JPRB.and.dry == 10.0_JPRB, 'empty liquid heating and absent-ice heating have separate reasons')
    ! Ice-only atmospheric melting remains possible with a warm dry memory.
    v = 0.0_JPRB
    t = 290.0_JPRB
    ice = real(0.5_JPRD*STO_IGNORE,JPRB)*RW/RI
    qi = RI*ice*HFUS
    call phase_step(v,t,ice,0.0_JPRB,qi,u,dry)
    call check(t == 290.0_JPRB.and.v>0.0_JPRB.and.ice == 0.0_JPRB, 'tiny melt keeps dry temperature without deleting mass')
    call check(dry<0.0_JPRB, 'warm memory on newly melted tiny water is accounted as negative heat')
    call phase_step(v,t,ice,0.0_JPRB,0.0_JPRB,u,dry)
    call check(u == 0.0_JPRB.and.dry == 0.0_JPRB, 'phase residual is not repeatedly charged')
    old_v = v
    ice = real(2.0_JPRD*STO_IGNORE,JPRB)*RW/RI
    qi = RI*ice*HFUS
    expected_t = TMELT+(290.0_JPRB-TMELT)*old_v/(old_v+RI*ice/RW)
    call phase_step(v,t,ice,0.0_JPRB,qi,u,dry)
    call check(abs(t-expected_t) <= tol*expected_t, 'wet melt mixes only pre-existing represented sensible energy')
    call check(dry == 0.0_JPRB, 'wet melt has no dry residual')

    v = real(0.5_JPRD*STO_IGNORE,JPRB)
    t = 290.0_JPRB
    ice = 0.0_JPRB
    qw = -1.1_JPRB*(liquid_water_energy_j(v,t)+RW*v*HFUS)
    call phase_step(v,t,ice,qw,0.0_JPRB,u,dry)
    call check(v == 0.0_JPRB.and.ice>0.0_JPRB.and.t == 290.0_JPRB, 'complete tiny freezing preserves warm dry memory and ice mass')
    qi = RI*ice*HFUS
    call phase_step(v,t,ice,0.0_JPRB,qi,u,dry)
    call check(v>0.0_JPRB.and.ice == 0.0_JPRB, 'tiny ice refreezing cycle conserves phase mass')

    v = real(STO_IGNORE,JPRB)
    t = 280.0_JPRB
    ice = 0.0_JPRB
    call phase_step(v,t,ice,1000.0_JPRB,0.0_JPRB,u,dry)
    call check(t == 280.0_JPRB.and.abs(u-1000.0_JPRB) <= tol*1000.0_JPRB, 'dry positive heating remains unapplied')
end subroutine

subroutine local_heating_and_floor()
    real(kind=JPRB) :: t,u,v
    t = 290.0_JPRB
    v = real(STO_IGNORE,JPRB)
    call update_liquid_temperature_no_phase_change(t,v,-100.0_JPRB,u)
    call check(t == 290.0_JPRB.and.u == -100.0_JPRB, 'liquid-only threshold uses <= and signed heat')
    t = TMELT-2.0_JPRB
    v = 1.0_JPRB
    call apply_liquid_temperature_floor(t,v,u)
    call check(t == TMELT.and.u == -2.0_JPRB*CW*RW, 'no-ice floor records unapplied cooling')
    t = 280.0_JPRB
    call apply_liquid_temperature_floor(t,0.0_JPRB,u)
    call check(t == 280.0_JPRB.and.u == 0.0_JPRB, 'dry memory has no no-ice floor heat')
end subroutine

subroutine floor_without_diagnostics()
    real(kind=JPRB) :: t(3), reference(3), v(3), u(3) ! [K,m3,J] Temperatures, volumes and rejected cooling.
    t = [260.0_JPRB, 290.0_JPRB, 260.0_JPRB]
    v = [1.0_JPRB, 1.0_JPRB, 0.0_JPRB]
    reference = t
    call apply_liquid_temperature_floor(reference, v, u)
    call apply_liquid_temperature_floor(t, v)
    call check(all(t == reference), 'omitting floor diagnostics preserves physical temperatures')
end subroutine floor_without_diagnostics

subroutine parallel_residual_ledger()
    integer, parameter :: n = 33003
    type(HeatResidualStats) :: stats
    real(kind=JPRD) :: residual(n),throughput(n)
    logical :: mask(n)
    integer :: i,threads
    do i = 1,n
        residual(i) = real(mod(i,3)-1,JPRD)
        mask(i) = mod(i,2) == 0
    enddo
    throughput = 2.0_JPRD
    do threads = 1,8
        !$ call omp_set_num_threads(threads)
        stats = HeatResidualStats()
        call record_heat_residual(stats,residual,throughput,mask)
        call record_heat_residual(stats,residual,throughput,mask)
        call check(stats%positive_j == 2.0_JPRD*sum(max(residual,0.0_JPRD),mask), 'parallel positive heat')
        call check(stats%negative_j == 2.0_JPRD*sum(min(residual,0.0_JPRD),mask), 'parallel negative heat')
        call check(stats%throughput_j == 4.0_JPRD*count(mask), 'parallel throughput')
        call check(stats%events == 2_JPIB*count(mask .and. residual /= 0.0_JPRD), 'repeated parallel events')
        call check(stats%affected_cells == count(mask .and. residual /= 0.0_JPRD), 'unique affected cells')
        call check(count(stats%affected) == stats%affected_cells, 'affected mask and count agree')
        call check(stats%maximum_cell == 2 .and. stats%maximum_absolute_j == 1.0_JPRD, 'first-cell tie handling')
    enddo
end subroutine parallel_residual_ledger

subroutine residual_ledger()
    type(HeatResidualStats) :: stats
    call record_heat_residual(stats,[2.0_JPRD,-3.0_JPRD],[10.0_JPRD,20.0_JPRD])
    call record_heat_residual(stats,[0.0_JPRD,0.0_JPRD],[0.0_JPRD,0.0_JPRD])
    call check(stats%positive_j == 2.0_JPRD.and.stats%negative_j == -3.0_JPRD, 'ledger retains both signs without cancellation')
    call check(stats%events == 2_JPIB.and.count(stats%affected) == 2, 'ledger distinguishes events and unique cells')
    call check(stats%maximum_absolute_j == 3.0_JPRD.and.stats%maximum_cell == 2, 'ledger records maximum and location')
    call check(stats%throughput_j == 30.0_JPRD, 'ledger records comparison energy scale')
end subroutine
end program test_temperature_dry_state
