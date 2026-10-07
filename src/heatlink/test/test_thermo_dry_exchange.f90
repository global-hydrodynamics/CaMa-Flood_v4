program test_thermo_dry_exchange
    use PARKIND1, only: JPRB
    use YOS_CMF_MAP, only: NSEQALL
    use YOS_CMF_INPUT, only: LOGNAM
    use const_mod, only: STO_IGNORE
    use phys_const_mod, only: RI, RW, HFUS, TMELT
    use thermo_mod, only: solve_water_ice_heat_budget, solve_heat_budget
    implicit none
    real(kind=JPRB) :: t(3), v(3), ice(3), excess(3), area(3), zero(3), atmospheric(3)
    real(kind=JPRB) :: u(3), mass_error(3), energy_error(3), dry(3), throughput(3), initial_v(3)
    NSEQALL = 3
    LOGNAM = 6
    t = 290.0_JPRB
    v = [0.0_JPRB, 0.5_JPRB*real(STO_IGNORE,JPRB), real(STO_IGNORE,JPRB)]
    initial_v = v
    ice = 1.0_JPRB
    excess = 0.0_JPRB
    area = 1.0_JPRB
    zero = 0.0_JPRB
    call solve_water_ice_heat_budget(t,v,ice,excess,area,area,zero,zero,zero,zero,zero,1.0_JPRB, &
    &   u,mass_error,energy_error,dry,throughput)
    if (any(t /= 290.0_JPRB).or.any(v /= initial_v).or.any(ice /= 1.0_JPRB)) &
    &   error stop 'Dry memory caused ghost water-to-ice heat transfer'
    if (any(u /= 0.0_JPRB).or.any(throughput /= 0.0_JPRB)) error stop 'No-flux dry state has residual heat'

    ! Atmospheric heat still melts ice when liquid water is absent or tiny.
    atmospheric = 0.1_JPRB*RI*HFUS
    call solve_water_ice_heat_budget(t,v,ice,excess,area,area,zero,zero,zero,atmospheric,zero,1.0_JPRB, &
    &   u,mass_error,energy_error,dry,throughput)
    if (any(v<0.09_JPRB).or.any(ice>0.91_JPRB)) error stop 'Dry-water guard incorrectly stopped atmospheric ice melting'
    if (any(abs(t-TMELT)>1.0e-5_JPRB)) error stop 'New meltwater inherited dry temperature'
    if (any(dry /= 0.0_JPRB)) error stop 'Wet meltwater incorrectly marked dry'
    if (maxval(abs(energy_error))>32.0_JPRB*epsilon(1.0_JPRB)*maxval(atmospheric)) &
    &   error stop 'Coupled ice melting energy budget failed'

    t = 290.0_JPRB
    v = initial_v
    atmospheric = [10.0_JPRB,-20.0_JPRB,30.0_JPRB]
    call solve_heat_budget(t,v,atmospheric,zero,area,1.0_JPRB,u,throughput)
    if (any(t /= 290.0_JPRB).or.any(u /= atmospheric)) error stop 'Coupled liquid-only dry update lost signed heat'
    write(*,'(a)') '[ALL TESTS PASSED] test_thermo_dry_exchange'
end program test_thermo_dry_exchange
