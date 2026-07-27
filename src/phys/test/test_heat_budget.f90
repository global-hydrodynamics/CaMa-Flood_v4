program test_heat_budget
    use, intrinsic :: ieee_arithmetic, only: &
    &   ieee_is_finite, ieee_value, ieee_quiet_nan, ieee_positive_inf
    use PARKIND1, only: &
    &   JPRB
    use const_mod, only: &
    &   STO_IGNORE
    use phys_const_mod, only: &
    &   CW, RW, CI, RI, HFUS, TMELT, SB, &
    &   iceSWref, ICE_LONGWAVE_EMISSIVITY, iceSWatten, Kice2air, KI
    use heat_flux_mod, only: &
    &   ICE_SURFACE_NEWTON_RESIDUAL_TOLERANCE_W_M2, &
    &   calc_ice_absorbed_longwave_flux, calc_ice_emitted_longwave_flux, &
    &   calc_ice_surface_heat_flux
    use heat_budget_mod, only: &
    &   NEGATIVE_VOLUME_TOLERANCE_M3, water_ice_mass_kg, water_ice_energy_j, &
    &   update_liquid_temperature_no_phase_change, update_local_water_ice_state, &
    &   equilibrate_water_ice
    implicit none

    call test_liquid_temperature_update()
    call test_partial_freezing()
    call test_complete_freezing_and_ice_cooling()
    call test_partial_melting()
    call test_complete_melting_and_water_warming()
    call test_empty_system_residual()
    call test_separate_water_budget_freezing()
    call test_separate_surface_ice_melting()
    call test_separate_surface_cooling_freezes_warm_water()
    call test_separate_excess_ice_only_melts()
    call test_separate_complete_melt_warms_water()
    call test_separate_complete_freeze_residual()
    call test_separate_zero_and_tiny_water()
    call test_separate_large_ice_volume()
    call test_separate_abrupt_phase_cycles()
    call test_separate_substep_invariance()
    call test_tiny_negative_state_normalization()
    call test_invalid_local_state_detection()
    call test_ice_longwave_fluxes()
    call test_ice_surface_heat_flux()
    call test_ice_surface_newton_convergence_range()

    write(*, '(a)') '[ALL TESTS PASSED] test_heat_budget'

contains

subroutine test_liquid_temperature_update()
    real(kind=JPRB) :: &
    &   liquid_water_temperature_k, & ! [K] Liquid-water temperature.
    &   liquid_water_volume_m3, &     ! [m3] Liquid-water volume.
    &   added_energy_j                ! [J] Energy added to liquid water.

    liquid_water_temperature_k = 280.0_JPRB
    liquid_water_volume_m3 = 2.0_JPRB
    added_energy_j = CW * RW * liquid_water_volume_m3 * 5.0_JPRB
    call update_liquid_temperature_no_phase_change( &
    &   liquid_water_temperature_k, liquid_water_volume_m3, added_energy_j)
    call assert_close(liquid_water_temperature_k, 285.0_JPRB, 1.0e-13_JPRB, &
    &   'liquid sensible heating [K]')

    liquid_water_temperature_k = 280.0_JPRB
    liquid_water_volume_m3 = 0.5_JPRB * real(STO_IGNORE, kind=JPRB)
    call update_liquid_temperature_no_phase_change( &
    &   liquid_water_temperature_k, liquid_water_volume_m3, added_energy_j)
    call assert_close(liquid_water_temperature_k, 280.0_JPRB, 0.0_JPRB, &
    &   'ignored liquid volume keeps temperature [K]')
end subroutine test_liquid_temperature_update


subroutine test_partial_freezing()
    real(kind=JPRB) :: &
    &   liquid_water_volume_m3, liquid_water_temperature_k, & ! [m3], [K] Liquid state.
    &   ice_volume_m3, ice_temperature_k, &                   ! [m3], [K] Ice state.
    &   added_energy_j, &                                     ! [J] Energy added to the system.
    &   ice_mass_change_kg, &                                 ! [kg] Positive mass frozen.
    &   residual_energy_j                                     ! [J] Unapplied energy.

    liquid_water_volume_m3 = 1.0_JPRB
    liquid_water_temperature_k = TMELT
    ice_volume_m3 = 0.0_JPRB
    ice_temperature_k = TMELT
    added_energy_j = -0.25_JPRB * RW * HFUS

    call check_equilibration( &
    &   liquid_water_volume_m3, liquid_water_temperature_k, &
    &   ice_volume_m3, ice_temperature_k, added_energy_j, &
    &   ice_mass_change_kg, residual_energy_j)

    call assert_close(liquid_water_volume_m3, 0.75_JPRB, 1.0e-13_JPRB, &
    &   'partial freezing liquid volume [m3]')
    call assert_close(ice_volume_m3, 0.25_JPRB * RW / RI, 1.0e-13_JPRB, &
    &   'partial freezing ice volume [m3]')
    call assert_close(liquid_water_temperature_k, TMELT, 0.0_JPRB, &
    &   'partial freezing liquid temperature [K]')
    call assert_close(ice_temperature_k, TMELT, 0.0_JPRB, &
    &   'partial freezing ice temperature [K]')
    call assert_close(ice_mass_change_kg, 0.25_JPRB * RW, 1.0e-13_JPRB, &
    &   'partial freezing ice-mass change [kg]')
end subroutine test_partial_freezing


subroutine test_complete_freezing_and_ice_cooling()
    real(kind=JPRB) :: &
    &   liquid_water_volume_m3, liquid_water_temperature_k, & ! [m3], [K] Liquid state.
    &   ice_volume_m3, ice_temperature_k, &                   ! [m3], [K] Ice state.
    &   added_energy_j, &                                     ! [J] Energy added to the system.
    &   ice_mass_change_kg, &                                 ! [kg] Positive mass frozen.
    &   residual_energy_j                                     ! [J] Unapplied energy.

    liquid_water_volume_m3 = 1.0_JPRB
    liquid_water_temperature_k = TMELT
    ice_volume_m3 = 0.0_JPRB
    ice_temperature_k = TMELT
    added_energy_j = -RW * (HFUS + CI * 5.0_JPRB)

    call check_equilibration( &
    &   liquid_water_volume_m3, liquid_water_temperature_k, &
    &   ice_volume_m3, ice_temperature_k, added_energy_j, &
    &   ice_mass_change_kg, residual_energy_j)

    call assert_close(liquid_water_volume_m3, 0.0_JPRB, 0.0_JPRB, &
    &   'complete freezing liquid volume [m3]')
    call assert_close(ice_volume_m3, RW / RI, 1.0e-13_JPRB, &
    &   'complete freezing ice volume [m3]')
    call assert_close(ice_temperature_k, TMELT - 5.0_JPRB, 1.0e-13_JPRB, &
    &   'complete freezing ice temperature [K]')
    call assert_close(ice_mass_change_kg, RW, 1.0e-13_JPRB, &
    &   'complete freezing ice-mass change [kg]')
end subroutine test_complete_freezing_and_ice_cooling


subroutine test_partial_melting()
    real(kind=JPRB) :: &
    &   liquid_water_volume_m3, liquid_water_temperature_k, & ! [m3], [K] Liquid state.
    &   ice_volume_m3, ice_temperature_k, &                   ! [m3], [K] Ice state.
    &   added_energy_j, &                                     ! [J] Energy added to the system.
    &   ice_mass_change_kg, &                                 ! [kg] Negative mass melted.
    &   residual_energy_j                                     ! [J] Unapplied energy.

    liquid_water_volume_m3 = 0.0_JPRB
    liquid_water_temperature_k = TMELT
    ice_volume_m3 = 1.0_JPRB
    ice_temperature_k = TMELT
    added_energy_j = 0.4_JPRB * RI * HFUS

    call check_equilibration( &
    &   liquid_water_volume_m3, liquid_water_temperature_k, &
    &   ice_volume_m3, ice_temperature_k, added_energy_j, &
    &   ice_mass_change_kg, residual_energy_j)

    call assert_close(liquid_water_volume_m3, 0.4_JPRB * RI / RW, 1.0e-13_JPRB, &
    &   'partial melting liquid volume [m3]')
    call assert_close(ice_volume_m3, 0.6_JPRB, 1.0e-13_JPRB, &
    &   'partial melting ice volume [m3]')
    call assert_close(ice_mass_change_kg, -0.4_JPRB * RI, 1.0e-13_JPRB, &
    &   'partial melting ice-mass change [kg]')
end subroutine test_partial_melting


subroutine test_complete_melting_and_water_warming()
    real(kind=JPRB) :: &
    &   liquid_water_volume_m3, liquid_water_temperature_k, & ! [m3], [K] Liquid state.
    &   ice_volume_m3, ice_temperature_k, &                   ! [m3], [K] Ice state.
    &   added_energy_j, &                                     ! [J] Energy added to the system.
    &   ice_mass_change_kg, &                                 ! [kg] Negative mass melted.
    &   residual_energy_j                                     ! [J] Unapplied energy.

    liquid_water_volume_m3 = 0.0_JPRB
    liquid_water_temperature_k = TMELT
    ice_volume_m3 = 1.0_JPRB
    ice_temperature_k = TMELT
    added_energy_j = RI * (HFUS + CW * 2.0_JPRB)

    call check_equilibration( &
    &   liquid_water_volume_m3, liquid_water_temperature_k, &
    &   ice_volume_m3, ice_temperature_k, added_energy_j, &
    &   ice_mass_change_kg, residual_energy_j)

    call assert_close(liquid_water_volume_m3, RI / RW, 1.0e-13_JPRB, &
    &   'complete melting liquid volume [m3]')
    call assert_close(liquid_water_temperature_k, TMELT + 2.0_JPRB, 1.0e-13_JPRB, &
    &   'complete melting liquid temperature [K]')
    call assert_close(ice_volume_m3, 0.0_JPRB, 0.0_JPRB, &
    &   'complete melting ice volume [m3]')
    call assert_close(ice_mass_change_kg, -RI, 1.0e-13_JPRB, &
    &   'complete melting ice-mass change [kg]')
end subroutine test_complete_melting_and_water_warming


subroutine test_empty_system_residual()
    real(kind=JPRB) :: &
    &   liquid_water_volume_m3, liquid_water_temperature_k, & ! [m3], [K] Liquid state.
    &   ice_volume_m3, ice_temperature_k, &                   ! [m3], [K] Ice state.
    &   added_energy_j, &                                     ! [J] Energy added to the system.
    &   ice_mass_change_kg, &                                 ! [kg] Ice-mass change.
    &   residual_energy_j                                     ! [J] Unapplied energy.

    liquid_water_volume_m3 = 0.0_JPRB
    liquid_water_temperature_k = TMELT
    ice_volume_m3 = 0.0_JPRB
    ice_temperature_k = TMELT
    added_energy_j = 123.0_JPRB

    call equilibrate_water_ice( &
    &   liquid_water_volume_m3, liquid_water_temperature_k, &
    &   ice_volume_m3, ice_temperature_k, added_energy_j, &
    &   ice_mass_change_kg, residual_energy_j)

    call assert_close(ice_mass_change_kg, 0.0_JPRB, 0.0_JPRB, &
    &   'empty system ice-mass change [kg]')
    call assert_close(residual_energy_j, added_energy_j, 0.0_JPRB, &
    &   'empty system residual energy [J]')
end subroutine test_empty_system_residual


subroutine test_separate_water_budget_freezing()
    real(kind=JPRB) :: &
    &   water_volume_m3, water_temperature_k, & ! [m3], [K] Liquid-water state.
    &   surface_ice_volume_m3, excess_ice_volume_m3, & ! [m3] Ice-pool states.
    &   frozen_mass_kg, surface_melted_mass_kg, excess_melted_mass_kg, & ! [kg] Phase changes.
    &   unapplied_energy_j                      ! [J] Energy not applied by the separate-budget kernel.

    water_volume_m3 = 1.0_JPRB
    water_temperature_k = TMELT + 1.0_JPRB
    surface_ice_volume_m3 = 0.0_JPRB
    excess_ice_volume_m3 = 0.0_JPRB
    call check_separate_update( &
    &   water_volume_m3, water_temperature_k, surface_ice_volume_m3, excess_ice_volume_m3, &
    &   -RW * (CW * 1.0_JPRB + 0.2_JPRB * HFUS), 0.0_JPRB, 0.0_JPRB, &
    &   frozen_mass_kg, surface_melted_mass_kg, excess_melted_mass_kg, unapplied_energy_j)

    call assert_close(water_volume_m3, 0.8_JPRB, 1.0e-13_JPRB, &
    &   'separate water cooling liquid volume [m3]')
    call assert_close(water_temperature_k, TMELT, 0.0_JPRB, &
    &   'separate water cooling liquid temperature [K]')
    call assert_close(surface_ice_volume_m3, 0.2_JPRB * RW / RI, 1.0e-13_JPRB, &
    &   'separate water cooling surface-ice volume [m3]')
    call assert_close(frozen_mass_kg, 0.2_JPRB * RW, 1.0e-13_JPRB, &
    &   'separate water cooling frozen mass [kg]')
    call assert_close(unapplied_energy_j, 0.0_JPRB, 0.0_JPRB, &
    &   'separate water cooling unapplied energy [J]')
end subroutine test_separate_water_budget_freezing


subroutine test_separate_surface_ice_melting()
    real(kind=JPRB) :: &
    &   water_volume_m3, water_temperature_k, & ! [m3], [K] Liquid-water state.
    &   surface_ice_volume_m3, excess_ice_volume_m3, & ! [m3] Ice-pool states.
    &   frozen_mass_kg, surface_melted_mass_kg, excess_melted_mass_kg, & ! [kg] Phase changes.
    &   unapplied_energy_j                      ! [J] Energy not applied by the separate-budget kernel.

    water_volume_m3 = 1.0_JPRB
    water_temperature_k = TMELT + 2.0_JPRB
    surface_ice_volume_m3 = 1.0_JPRB
    excess_ice_volume_m3 = 0.0_JPRB
    call check_separate_update( &
    &   water_volume_m3, water_temperature_k, surface_ice_volume_m3, excess_ice_volume_m3, &
    &   0.0_JPRB, 0.25_JPRB * RI * HFUS, 0.0_JPRB, &
    &   frozen_mass_kg, surface_melted_mass_kg, excess_melted_mass_kg, unapplied_energy_j)

    call assert_close(surface_ice_volume_m3, 0.75_JPRB, 1.0e-13_JPRB, &
    &   'separate surface melting ice volume [m3]')
    call assert_close(water_volume_m3, 1.0_JPRB + 0.25_JPRB * RI / RW, 1.0e-13_JPRB, &
    &   'separate surface melting liquid volume [m3]')
    call assert_close(surface_melted_mass_kg, 0.25_JPRB * RI, 1.0e-13_JPRB, &
    &   'separate surface melting mass [kg]')
    call assert_close(frozen_mass_kg, 0.0_JPRB, 0.0_JPRB, &
    &   'separate surface melting frozen mass [kg]')
end subroutine test_separate_surface_ice_melting


subroutine test_separate_surface_cooling_freezes_warm_water()
    real(kind=JPRB) :: &
    &   water_volume_m3, water_temperature_k, & ! [m3], [K] Liquid-water state.
    &   surface_ice_volume_m3, excess_ice_volume_m3, & ! [m3] Ice-pool states.
    &   frozen_mass_kg, surface_melted_mass_kg, excess_melted_mass_kg, & ! [kg] Phase changes.
    &   unapplied_energy_j, expected_frozen_mass_kg ! [J], [kg] Residual and expected phase change.

    water_volume_m3 = 1.0_JPRB
    water_temperature_k = TMELT + 4.0_JPRB
    surface_ice_volume_m3 = 0.5_JPRB
    excess_ice_volume_m3 = 0.0_JPRB
    expected_frozen_mass_kg = 100.0_JPRB * HFUS / (HFUS + CW * 4.0_JPRB)
    call check_separate_update( &
    &   water_volume_m3, water_temperature_k, surface_ice_volume_m3, excess_ice_volume_m3, &
    &   0.0_JPRB, -100.0_JPRB * HFUS, 0.0_JPRB, &
    &   frozen_mass_kg, surface_melted_mass_kg, excess_melted_mass_kg, unapplied_energy_j)

    call assert_close(frozen_mass_kg, expected_frozen_mass_kg, 1.0e-13_JPRB, &
    &   'separate surface cooling warm-water frozen mass [kg]')
    call assert_close(water_temperature_k, TMELT + 4.0_JPRB, 1.0e-13_JPRB, &
    &   'separate surface cooling remaining-water temperature [K]')
    call assert_close(surface_ice_volume_m3, &
    &   0.5_JPRB + expected_frozen_mass_kg / RI, 1.0e-13_JPRB, &
    &   'separate surface cooling surface-ice volume [m3]')
end subroutine test_separate_surface_cooling_freezes_warm_water


subroutine test_separate_excess_ice_only_melts()
    real(kind=JPRB) :: &
    &   water_volume_m3, water_temperature_k, & ! [m3], [K] Liquid-water state.
    &   surface_ice_volume_m3, excess_ice_volume_m3, & ! [m3] Ice-pool states.
    &   frozen_mass_kg, surface_melted_mass_kg, excess_melted_mass_kg, & ! [kg] Phase changes.
    &   unapplied_energy_j                      ! [J] Energy not applied by the separate-budget kernel.

    water_volume_m3 = 1.0_JPRB
    water_temperature_k = TMELT
    surface_ice_volume_m3 = 0.0_JPRB
    excess_ice_volume_m3 = 1.0_JPRB
    call check_separate_update( &
    &   water_volume_m3, water_temperature_k, surface_ice_volume_m3, excess_ice_volume_m3, &
    &   0.0_JPRB, 0.0_JPRB, -20.0_JPRB * HFUS, &
    &   frozen_mass_kg, surface_melted_mass_kg, excess_melted_mass_kg, unapplied_energy_j)

    call assert_close(excess_ice_volume_m3, 1.0_JPRB, 0.0_JPRB, &
    &   'separate excess cooling keeps excess-ice volume [m3]')
    call assert_close(surface_ice_volume_m3, 0.0_JPRB, 0.0_JPRB, &
    &   'separate excess cooling creates no surface ice [m3]')
    call assert_close(unapplied_energy_j, -20.0_JPRB * HFUS, 1.0e-13_JPRB, &
    &   'separate excess cooling unapplied energy [J]')

    call check_separate_update( &
    &   water_volume_m3, water_temperature_k, surface_ice_volume_m3, excess_ice_volume_m3, &
    &   0.0_JPRB, 0.0_JPRB, 0.3_JPRB * RI * HFUS, &
    &   frozen_mass_kg, surface_melted_mass_kg, excess_melted_mass_kg, unapplied_energy_j)
    call assert_close(excess_ice_volume_m3, 0.7_JPRB, 1.0e-13_JPRB, &
    &   'separate excess melting excess-ice volume [m3]')
    call assert_close(excess_melted_mass_kg, 0.3_JPRB * RI, 1.0e-13_JPRB, &
    &   'separate excess melting mass [kg]')
end subroutine test_separate_excess_ice_only_melts


subroutine test_separate_complete_melt_warms_water()
    real(kind=JPRB) :: &
    &   water_volume_m3, water_temperature_k, & ! [m3], [K] Liquid-water state.
    &   surface_ice_volume_m3, excess_ice_volume_m3, & ! [m3] Ice-pool states.
    &   frozen_mass_kg, surface_melted_mass_kg, excess_melted_mass_kg, & ! [kg] Phase changes.
    &   unapplied_energy_j, final_water_mass_kg ! [J], [kg] Residual and final liquid mass.

    water_volume_m3 = 0.0_JPRB
    water_temperature_k = TMELT
    surface_ice_volume_m3 = 1.0_JPRB
    excess_ice_volume_m3 = 0.0_JPRB
    call check_separate_update( &
    &   water_volume_m3, water_temperature_k, surface_ice_volume_m3, excess_ice_volume_m3, &
    &   0.0_JPRB, RI * (HFUS + CW * 3.0_JPRB), 0.0_JPRB, &
    &   frozen_mass_kg, surface_melted_mass_kg, excess_melted_mass_kg, unapplied_energy_j)

    final_water_mass_kg = RI
    call assert_close(surface_ice_volume_m3, 0.0_JPRB, 0.0_JPRB, &
    &   'separate complete melt surface-ice volume [m3]')
    call assert_close(water_volume_m3, final_water_mass_kg / RW, 1.0e-13_JPRB, &
    &   'separate complete melt liquid volume [m3]')
    call assert_close(water_temperature_k, TMELT + 3.0_JPRB, 1.0e-13_JPRB, &
    &   'separate complete melt liquid temperature [K]')
end subroutine test_separate_complete_melt_warms_water


subroutine test_separate_complete_freeze_residual()
    real(kind=JPRB) :: &
    &   water_volume_m3, water_temperature_k, & ! [m3], [K] Liquid-water state.
    &   surface_ice_volume_m3, excess_ice_volume_m3, & ! [m3] Ice-pool states.
    &   frozen_mass_kg, surface_melted_mass_kg, excess_melted_mass_kg, & ! [kg] Phase changes.
    &   unapplied_energy_j                      ! [J] Energy not applied by the separate-budget kernel.

    water_volume_m3 = 0.1_JPRB
    water_temperature_k = TMELT
    surface_ice_volume_m3 = 0.5_JPRB
    excess_ice_volume_m3 = 0.0_JPRB
    call check_separate_update( &
    &   water_volume_m3, water_temperature_k, surface_ice_volume_m3, excess_ice_volume_m3, &
    &   0.0_JPRB, -200.0_JPRB * HFUS, 0.0_JPRB, &
    &   frozen_mass_kg, surface_melted_mass_kg, excess_melted_mass_kg, unapplied_energy_j)

    call assert_close(water_volume_m3, 0.0_JPRB, 0.0_JPRB, &
    &   'separate complete freeze liquid volume [m3]')
    call assert_close(frozen_mass_kg, 0.1_JPRB * RW, 1.0e-13_JPRB, &
    &   'separate complete freeze frozen mass [kg]')
    call assert_close(unapplied_energy_j, -100.0_JPRB * HFUS, 1.0e-13_JPRB, &
    &   'separate complete freeze unapplied energy [J]')
end subroutine test_separate_complete_freeze_residual


subroutine test_separate_zero_and_tiny_water()
    real(kind=JPRB) :: &
    &   water_volume_m3, water_temperature_k, & ! [m3], [K] Liquid-water state.
    &   surface_ice_volume_m3, excess_ice_volume_m3, & ! [m3] Ice-pool states.
    &   frozen_mass_kg, surface_melted_mass_kg, excess_melted_mass_kg, & ! [kg] Phase changes.
    &   unapplied_energy_j                      ! [J] Energy not applied by the update.

    ! A positive water volume remains a physical state even when it is below
    ! the storage threshold used to skip atmospheric heat-flux calculations.
    water_volume_m3 = 1.0e-12_JPRB
    water_temperature_k = TMELT
    surface_ice_volume_m3 = 0.0_JPRB
    excess_ice_volume_m3 = 0.0_JPRB
    call check_separate_update( &
    &   water_volume_m3, water_temperature_k, surface_ice_volume_m3, excess_ice_volume_m3, &
    &   -RW * 1.0e-12_JPRB * HFUS, 0.0_JPRB, 0.0_JPRB, &
    &   frozen_mass_kg, surface_melted_mass_kg, excess_melted_mass_kg, unapplied_energy_j)
    call assert_close(water_volume_m3, 0.0_JPRB, 0.0_JPRB, &
    &   'tiny-water complete freeze liquid volume [m3]')
    call assert_close(surface_ice_volume_m3, RW * 1.0e-12_JPRB / RI, 1.0e-13_JPRB, &
    &   'tiny-water complete freeze surface-ice volume [m3]')
    call assert_finite(surface_ice_volume_m3, 'tiny-water surface-ice volume is finite')

    ! Energy presented to an empty cell cannot create mass and is reported as
    ! unapplied with its algebraic sign.
    water_volume_m3 = 0.0_JPRB
    water_temperature_k = TMELT
    surface_ice_volume_m3 = 0.0_JPRB
    excess_ice_volume_m3 = 0.0_JPRB
    call check_separate_update( &
    &   water_volume_m3, water_temperature_k, surface_ice_volume_m3, excess_ice_volume_m3, &
    &   10.0_JPRB, -20.0_JPRB, 30.0_JPRB, &
    &   frozen_mass_kg, surface_melted_mass_kg, excess_melted_mass_kg, unapplied_energy_j)
    call assert_close(water_volume_m3 + surface_ice_volume_m3 + excess_ice_volume_m3, &
    &   0.0_JPRB, 0.0_JPRB, 'empty update creates no water or ice [m3]')
    call assert_close(unapplied_energy_j, 20.0_JPRB, 0.0_JPRB, &
    &   'empty update signed unapplied energy [J]')
    call assert_finite(water_temperature_k, 'empty-update water temperature is finite')
end subroutine test_separate_zero_and_tiny_water


subroutine test_separate_large_ice_volume()
    real(kind=JPRB) :: &
    &   water_volume_m3, water_temperature_k, & ! [m3], [K] Liquid-water state.
    &   surface_ice_volume_m3, excess_ice_volume_m3, & ! [m3] Ice-pool states.
    &   initial_mass_kg, initial_energy_j, &     ! [kg], [J] Conservation scales before the update.
    &   excess_melt_energy_j, &                 ! [J] Energy used to melt immobile excess ice.
    &   frozen_mass_kg, surface_melted_mass_kg, excess_melted_mass_kg, & ! [kg] Phase changes.
    &   unapplied_energy_j, mass_budget_error_kg, energy_budget_error_j, & ! [J], [kg], [J] Diagnostics.
    &   maximum_negative_volume_m3              ! [m3] Largest negative input-volume magnitude.
    logical :: &
    &   state_is_valid, &                       ! [-] Local-state validation result.
    &   nonfinite_input_detected                ! [-] Nonfinite-input detection result.

    water_volume_m3 = 1.0e12_JPRB
    water_temperature_k = TMELT + 1.0_JPRB
    surface_ice_volume_m3 = 2.0e12_JPRB
    excess_ice_volume_m3 = 1.0e15_JPRB
    excess_melt_energy_j = RI * 1.0e9_JPRB * HFUS
    initial_mass_kg = water_ice_mass_kg( &
    &   water_volume_m3, surface_ice_volume_m3 + excess_ice_volume_m3)
    initial_energy_j = water_ice_energy_j( &
    &   water_volume_m3, water_temperature_k, &
    &   surface_ice_volume_m3 + excess_ice_volume_m3, TMELT)

    call update_local_water_ice_state( &
    &   liquid_water_volume_m3=water_volume_m3, &
    &   liquid_water_temperature_k=water_temperature_k, &
    &   surface_ice_volume_m3=surface_ice_volume_m3, &
    &   excess_ice_volume_m3=excess_ice_volume_m3, &
    &   liquid_water_added_energy_j=0.0_JPRB, &
    &   surface_ice_added_energy_j=0.0_JPRB, &
    &   excess_ice_added_energy_j=excess_melt_energy_j, &
    &   frozen_water_mass_kg=frozen_mass_kg, &
    &   surface_ice_melted_mass_kg=surface_melted_mass_kg, &
    &   excess_ice_melted_mass_kg=excess_melted_mass_kg, &
    &   unapplied_energy_j=unapplied_energy_j, &
    &   mass_budget_error_kg=mass_budget_error_kg, &
    &   energy_budget_error_j=energy_budget_error_j, &
    &   state_is_valid=state_is_valid, &
    &   nonfinite_input_detected=nonfinite_input_detected, &
    &   maximum_negative_volume_m3=maximum_negative_volume_m3)

    call assert_true(state_is_valid, 'large-volume local state is valid')
    call assert_true(.not. nonfinite_input_detected, 'large-volume inputs are finite')

    call assert_close(excess_ice_volume_m3, 1.0e15_JPRB - 1.0e9_JPRB, 1.0e-13_JPRB, &
    &   'large-volume excess ice after partial melt [m3]')
    call assert_close(excess_melted_mass_kg, RI * 1.0e9_JPRB, 1.0e-13_JPRB, &
    &   'large-volume excess melted mass [kg]')
    call assert_abs_le(mass_budget_error_kg, 1.0e-12_JPRB * initial_mass_kg, &
    &   'large-volume relative mass conservation [kg]')
    call assert_abs_le(energy_budget_error_j, &
    &   1.0e-12_JPRB * max(abs(initial_energy_j), excess_melt_energy_j), &
    &   'large-volume relative energy conservation [J]')
    call assert_finite(water_temperature_k, 'large-volume water temperature is finite')
    call assert_finite(excess_ice_volume_m3, 'large-volume excess ice is finite')
end subroutine test_separate_large_ice_volume


subroutine test_separate_abrupt_phase_cycles()
    integer, parameter :: &
    &   number_of_cycles = 100
    real(kind=JPRB), parameter :: &
    &   initial_water_volume_m3 = 2.0_JPRB ! [m3] Liquid water at the start of every cycle.
    real(kind=JPRB) :: &
    &   water_volume_m3, water_temperature_k, & ! [m3], [K] Liquid-water state.
    &   surface_ice_volume_m3, excess_ice_volume_m3, & ! [m3] Ice-pool states.
    &   frozen_mass_kg, surface_melted_mass_kg, excess_melted_mass_kg, & ! [kg] Phase changes.
    &   unapplied_energy_j                      ! [J] Energy not applied by the update.
    integer :: &
    &   cycle_index                              ! [-] Freeze-melt cycle index.

    water_volume_m3 = initial_water_volume_m3
    water_temperature_k = TMELT
    surface_ice_volume_m3 = 0.0_JPRB
    excess_ice_volume_m3 = 0.0_JPRB
    do cycle_index = 1, number_of_cycles
        call check_separate_update( &
        &   water_volume_m3, water_temperature_k, surface_ice_volume_m3, excess_ice_volume_m3, &
        &   -RW * water_volume_m3 * HFUS, 0.0_JPRB, 0.0_JPRB, &
        &   frozen_mass_kg, surface_melted_mass_kg, excess_melted_mass_kg, unapplied_energy_j)
        call assert_close(water_volume_m3, 0.0_JPRB, 0.0_JPRB, &
        &   'abrupt cycle complete freeze liquid volume [m3]')
        call assert_close(unapplied_energy_j, 0.0_JPRB, 1.0e-13_JPRB, &
        &   'abrupt cycle complete freeze unapplied energy [J]')

        call check_separate_update( &
        &   water_volume_m3, water_temperature_k, surface_ice_volume_m3, excess_ice_volume_m3, &
        &   0.0_JPRB, RI * surface_ice_volume_m3 * HFUS, 0.0_JPRB, &
        &   frozen_mass_kg, surface_melted_mass_kg, excess_melted_mass_kg, unapplied_energy_j)
        call assert_close(surface_ice_volume_m3, 0.0_JPRB, 0.0_JPRB, &
        &   'abrupt cycle complete melt surface-ice volume [m3]')
        call assert_close(unapplied_energy_j, 0.0_JPRB, 1.0e-13_JPRB, &
        &   'abrupt cycle complete melt unapplied energy [J]')
    enddo

    call assert_close(water_volume_m3, initial_water_volume_m3, 1.0e-12_JPRB, &
    &   'abrupt freeze-melt cycles conserve liquid volume [m3]')
    call assert_close(water_temperature_k, TMELT, 0.0_JPRB, &
    &   'abrupt freeze-melt cycles retain melting temperature [K]')
end subroutine test_separate_abrupt_phase_cycles


subroutine test_separate_substep_invariance()
    integer, parameter :: &
    &   number_of_substeps = 12
    real(kind=JPRB) :: &
    &   one_step_water_volume_m3, one_step_water_temperature_k, & ! [m3], [K] One-step liquid state.
    &   one_step_surface_ice_volume_m3, one_step_excess_ice_volume_m3, & ! [m3] One-step ice state.
    &   split_water_volume_m3, split_water_temperature_k, & ! [m3], [K] Substepped liquid state.
    &   split_surface_ice_volume_m3, split_excess_ice_volume_m3, & ! [m3] Substepped ice state.
    &   total_added_energy_j, &                    ! [J] Energy integrated over the full update.
    &   frozen_mass_kg, surface_melted_mass_kg, excess_melted_mass_kg, & ! [kg] Phase changes.
    &   unapplied_energy_j                         ! [J] Energy not applied by an update.
    integer :: &
    &   substep_index                               ! [-] Thermal substep index.

    ! Pure water cooling must give the same phase partition when the integrated
    ! energy is applied in one step or split into equal thermal substeps.
    total_added_energy_j = -RW * (CW * 1.0_JPRB + 0.4_JPRB * HFUS)
    one_step_water_volume_m3 = 1.0_JPRB
    one_step_water_temperature_k = TMELT + 1.0_JPRB
    one_step_surface_ice_volume_m3 = 0.0_JPRB
    one_step_excess_ice_volume_m3 = 0.0_JPRB
    split_water_volume_m3 = one_step_water_volume_m3
    split_water_temperature_k = one_step_water_temperature_k
    split_surface_ice_volume_m3 = one_step_surface_ice_volume_m3
    split_excess_ice_volume_m3 = one_step_excess_ice_volume_m3
    call check_separate_update( &
    &   one_step_water_volume_m3, one_step_water_temperature_k, &
    &   one_step_surface_ice_volume_m3, one_step_excess_ice_volume_m3, &
    &   total_added_energy_j, 0.0_JPRB, 0.0_JPRB, &
    &   frozen_mass_kg, surface_melted_mass_kg, excess_melted_mass_kg, unapplied_energy_j)
    do substep_index = 1, number_of_substeps
        call check_separate_update( &
        &   split_water_volume_m3, split_water_temperature_k, &
        &   split_surface_ice_volume_m3, split_excess_ice_volume_m3, &
        &   total_added_energy_j / real(number_of_substeps, kind=JPRB), &
        &   0.0_JPRB, 0.0_JPRB, &
        &   frozen_mass_kg, surface_melted_mass_kg, excess_melted_mass_kg, unapplied_energy_j)
    enddo
    call assert_close(split_water_volume_m3, one_step_water_volume_m3, 1.0e-12_JPRB, &
    &   'substep-invariant water-freezing liquid volume [m3]')
    call assert_close(split_surface_ice_volume_m3, one_step_surface_ice_volume_m3, 1.0e-12_JPRB, &
    &   'substep-invariant water-freezing ice volume [m3]')
    call assert_close(split_water_temperature_k, one_step_water_temperature_k, 1.0e-12_JPRB, &
    &   'substep-invariant water-freezing temperature [K]')

    ! The same invariance is required across complete melting, including the
    ! point at which surplus melt energy begins to warm the liquid water.
    total_added_energy_j = RI * (HFUS + CW * 2.0_JPRB)
    one_step_water_volume_m3 = 0.0_JPRB
    one_step_water_temperature_k = TMELT
    one_step_surface_ice_volume_m3 = 1.0_JPRB
    one_step_excess_ice_volume_m3 = 0.0_JPRB
    split_water_volume_m3 = one_step_water_volume_m3
    split_water_temperature_k = one_step_water_temperature_k
    split_surface_ice_volume_m3 = one_step_surface_ice_volume_m3
    split_excess_ice_volume_m3 = one_step_excess_ice_volume_m3
    call check_separate_update( &
    &   one_step_water_volume_m3, one_step_water_temperature_k, &
    &   one_step_surface_ice_volume_m3, one_step_excess_ice_volume_m3, &
    &   0.0_JPRB, total_added_energy_j, 0.0_JPRB, &
    &   frozen_mass_kg, surface_melted_mass_kg, excess_melted_mass_kg, unapplied_energy_j)
    do substep_index = 1, number_of_substeps
        call check_separate_update( &
        &   split_water_volume_m3, split_water_temperature_k, &
        &   split_surface_ice_volume_m3, split_excess_ice_volume_m3, &
        &   0.0_JPRB, total_added_energy_j / real(number_of_substeps, kind=JPRB), &
        &   0.0_JPRB, &
        &   frozen_mass_kg, surface_melted_mass_kg, excess_melted_mass_kg, unapplied_energy_j)
    enddo
    call assert_close(split_water_volume_m3, one_step_water_volume_m3, 1.0e-12_JPRB, &
    &   'substep-invariant complete-melt liquid volume [m3]')
    call assert_close(split_surface_ice_volume_m3, one_step_surface_ice_volume_m3, 1.0e-12_JPRB, &
    &   'substep-invariant complete-melt ice volume [m3]')
    call assert_close(split_water_temperature_k, one_step_water_temperature_k, 1.0e-12_JPRB, &
    &   'substep-invariant complete-melt temperature [K]')
end subroutine test_separate_substep_invariance


subroutine test_tiny_negative_state_normalization()
    real(kind=JPRB) :: &
    &   water_volume_m3, water_temperature_k, & ! [m3], [K] Liquid-water state.
    &   surface_ice_volume_m3, excess_ice_volume_m3, & ! [m3] Ice-pool states.
    &   water_correction_m3, surface_ice_correction_m3, & ! [m3] Tiny negative-volume magnitudes.
    &   expected_mass_adjustment_kg, expected_energy_adjustment_j, & ! [kg], [J] Expected diagnostics.
    &   frozen_mass_kg, surface_melted_mass_kg, excess_melted_mass_kg, & ! [kg] Phase changes.
    &   unapplied_energy_j, mass_budget_error_kg, energy_budget_error_j, & ! [J], [kg], [J] Diagnostics.
    &   maximum_negative_volume_m3              ! [m3] Largest negative input-volume magnitude.
    logical :: &
    &   state_is_valid, &                       ! [-] Local-state validation result.
    &   nonfinite_input_detected                ! [-] Nonfinite-input detection result.

    water_correction_m3 = 0.25_JPRB * NEGATIVE_VOLUME_TOLERANCE_M3
    surface_ice_correction_m3 = 0.5_JPRB * NEGATIVE_VOLUME_TOLERANCE_M3
    water_volume_m3 = -water_correction_m3
    water_temperature_k = TMELT + 2.0_JPRB
    surface_ice_volume_m3 = -surface_ice_correction_m3
    excess_ice_volume_m3 = 0.0_JPRB
    expected_mass_adjustment_kg = RW * water_correction_m3 + &
    &   RI * surface_ice_correction_m3
    expected_energy_adjustment_j = CW * RW * water_correction_m3 * 2.0_JPRB - &
    &   RI * surface_ice_correction_m3 * HFUS

    call update_local_water_ice_state( &
    &   liquid_water_volume_m3=water_volume_m3, &
    &   liquid_water_temperature_k=water_temperature_k, &
    &   surface_ice_volume_m3=surface_ice_volume_m3, &
    &   excess_ice_volume_m3=excess_ice_volume_m3, &
    &   liquid_water_added_energy_j=0.0_JPRB, &
    &   surface_ice_added_energy_j=0.0_JPRB, &
    &   excess_ice_added_energy_j=0.0_JPRB, &
    &   frozen_water_mass_kg=frozen_mass_kg, &
    &   surface_ice_melted_mass_kg=surface_melted_mass_kg, &
    &   excess_ice_melted_mass_kg=excess_melted_mass_kg, &
    &   unapplied_energy_j=unapplied_energy_j, &
    &   mass_budget_error_kg=mass_budget_error_kg, &
    &   energy_budget_error_j=energy_budget_error_j, &
    &   state_is_valid=state_is_valid, &
    &   nonfinite_input_detected=nonfinite_input_detected, &
    &   maximum_negative_volume_m3=maximum_negative_volume_m3)

    call assert_true(state_is_valid, 'tiny negative volumes are accepted')
    call assert_true(.not. nonfinite_input_detected, 'tiny negative inputs are finite')
    call assert_close(water_volume_m3, 0.0_JPRB, 0.0_JPRB, &
    &   'tiny negative liquid-water volume is normalized [m3]')
    call assert_close(surface_ice_volume_m3, 0.0_JPRB, 0.0_JPRB, &
    &   'tiny negative surface-ice volume is normalized [m3]')
    call assert_close(maximum_negative_volume_m3, surface_ice_correction_m3, 1.0e-13_JPRB, &
    &   'tiny negative maximum volume magnitude [m3]')
    call assert_close(mass_budget_error_kg, expected_mass_adjustment_kg, 1.0e-13_JPRB, &
    &   'tiny negative normalization mass adjustment [kg]')
    call assert_close(energy_budget_error_j, expected_energy_adjustment_j, 1.0e-13_JPRB, &
    &   'tiny negative normalization energy adjustment [J]')
end subroutine test_tiny_negative_state_normalization


subroutine test_invalid_local_state_detection()
    real(kind=JPRB) :: &
    &   water_volume_m3, water_temperature_k, & ! [m3], [K] Liquid-water state.
    &   surface_ice_volume_m3, excess_ice_volume_m3, & ! [m3] Ice-pool states.
    &   liquid_added_energy_j, &                ! [J] Energy added directly to liquid water.
    &   frozen_mass_kg, surface_melted_mass_kg, excess_melted_mass_kg, & ! [kg] Phase changes.
    &   unapplied_energy_j, mass_budget_error_kg, energy_budget_error_j, & ! [J], [kg], [J] Diagnostics.
    &   maximum_negative_volume_m3              ! [m3] Largest negative input-volume magnitude.
    logical :: &
    &   state_is_valid, &                       ! [-] Local-state validation result.
    &   nonfinite_input_detected                ! [-] Nonfinite-input detection result.

    water_volume_m3 = -2.0_JPRB * NEGATIVE_VOLUME_TOLERANCE_M3
    water_temperature_k = TMELT
    surface_ice_volume_m3 = 0.0_JPRB
    excess_ice_volume_m3 = 0.0_JPRB
    liquid_added_energy_j = 0.0_JPRB
    call update_local_water_ice_state( &
    &   liquid_water_volume_m3=water_volume_m3, &
    &   liquid_water_temperature_k=water_temperature_k, &
    &   surface_ice_volume_m3=surface_ice_volume_m3, &
    &   excess_ice_volume_m3=excess_ice_volume_m3, &
    &   liquid_water_added_energy_j=liquid_added_energy_j, &
    &   surface_ice_added_energy_j=0.0_JPRB, &
    &   excess_ice_added_energy_j=0.0_JPRB, &
    &   frozen_water_mass_kg=frozen_mass_kg, &
    &   surface_ice_melted_mass_kg=surface_melted_mass_kg, &
    &   excess_ice_melted_mass_kg=excess_melted_mass_kg, &
    &   unapplied_energy_j=unapplied_energy_j, &
    &   mass_budget_error_kg=mass_budget_error_kg, &
    &   energy_budget_error_j=energy_budget_error_j, &
    &   state_is_valid=state_is_valid, &
    &   nonfinite_input_detected=nonfinite_input_detected, &
    &   maximum_negative_volume_m3=maximum_negative_volume_m3)
    call assert_true(.not. state_is_valid, 'large negative volume is rejected')
    call assert_true(.not. nonfinite_input_detected, 'large negative volume remains finite')
    call assert_close(maximum_negative_volume_m3, &
    &   2.0_JPRB * NEGATIVE_VOLUME_TOLERANCE_M3, 1.0e-13_JPRB, &
    &   'large negative volume magnitude [m3]')
    call assert_close(water_volume_m3, &
    &   -2.0_JPRB * NEGATIVE_VOLUME_TOLERANCE_M3, 0.0_JPRB, &
    &   'invalid negative volume is not modified [m3]')

    water_volume_m3 = 1.0_JPRB
    water_temperature_k = ieee_value(0.0_JPRB, ieee_quiet_nan)
    call update_local_water_ice_state( &
    &   liquid_water_volume_m3=water_volume_m3, &
    &   liquid_water_temperature_k=water_temperature_k, &
    &   surface_ice_volume_m3=surface_ice_volume_m3, &
    &   excess_ice_volume_m3=excess_ice_volume_m3, &
    &   liquid_water_added_energy_j=0.0_JPRB, &
    &   surface_ice_added_energy_j=0.0_JPRB, &
    &   excess_ice_added_energy_j=0.0_JPRB, &
    &   frozen_water_mass_kg=frozen_mass_kg, &
    &   surface_ice_melted_mass_kg=surface_melted_mass_kg, &
    &   excess_ice_melted_mass_kg=excess_melted_mass_kg, &
    &   unapplied_energy_j=unapplied_energy_j, &
    &   mass_budget_error_kg=mass_budget_error_kg, &
    &   energy_budget_error_j=energy_budget_error_j, &
    &   state_is_valid=state_is_valid, &
    &   nonfinite_input_detected=nonfinite_input_detected, &
    &   maximum_negative_volume_m3=maximum_negative_volume_m3)
    call assert_true(.not. state_is_valid, 'NaN state is rejected')
    call assert_true(nonfinite_input_detected, 'NaN state is reported as nonfinite')

    water_temperature_k = TMELT
    liquid_added_energy_j = ieee_value(0.0_JPRB, ieee_positive_inf)
    call update_local_water_ice_state( &
    &   liquid_water_volume_m3=water_volume_m3, &
    &   liquid_water_temperature_k=water_temperature_k, &
    &   surface_ice_volume_m3=surface_ice_volume_m3, &
    &   excess_ice_volume_m3=excess_ice_volume_m3, &
    &   liquid_water_added_energy_j=liquid_added_energy_j, &
    &   surface_ice_added_energy_j=0.0_JPRB, &
    &   excess_ice_added_energy_j=0.0_JPRB, &
    &   frozen_water_mass_kg=frozen_mass_kg, &
    &   surface_ice_melted_mass_kg=surface_melted_mass_kg, &
    &   excess_ice_melted_mass_kg=excess_melted_mass_kg, &
    &   unapplied_energy_j=unapplied_energy_j, &
    &   mass_budget_error_kg=mass_budget_error_kg, &
    &   energy_budget_error_j=energy_budget_error_j, &
    &   state_is_valid=state_is_valid, &
    &   nonfinite_input_detected=nonfinite_input_detected, &
    &   maximum_negative_volume_m3=maximum_negative_volume_m3)
    call assert_true(.not. state_is_valid, 'infinite energy input is rejected')
    call assert_true(nonfinite_input_detected, 'infinite energy input is reported as nonfinite')
end subroutine test_invalid_local_state_detection


subroutine check_separate_update( &
    &   water_volume_m3, water_temperature_k, surface_ice_volume_m3, excess_ice_volume_m3, &
    &   water_added_energy_j, surface_ice_added_energy_j, excess_ice_added_energy_j, &
    &   frozen_mass_kg, surface_melted_mass_kg, excess_melted_mass_kg, unapplied_energy_j)
    real(kind=JPRB), intent(inout) :: &
    &   water_volume_m3, &          ! [m3] Liquid-water volume before and after the update.
    &   water_temperature_k, &      ! [K] Liquid-water temperature before and after the update.
    &   surface_ice_volume_m3, &    ! [m3] Water-surface ice volume before and after the update.
    &   excess_ice_volume_m3        ! [m3] Immobile excess-ice volume before and after the update.
    real(kind=JPRB), intent(in) :: &
    &   water_added_energy_j, &       ! [J] Energy added directly to liquid water.
    &   surface_ice_added_energy_j, & ! [J] Energy added to water-surface ice.
    &   excess_ice_added_energy_j     ! [J] Energy added to immobile excess ice.
    real(kind=JPRB), intent(out) :: &
    &   frozen_mass_kg, &             ! [kg] Liquid-water mass converted to water-surface ice.
    &   surface_melted_mass_kg, &     ! [kg] Water-surface ice mass melted.
    &   excess_melted_mass_kg, &      ! [kg] Immobile excess-ice mass melted.
    &   unapplied_energy_j             ! [J] Energy not applied by the update.
    real(kind=JPRB) :: &
    &   mass_budget_error_kg, &        ! [kg] Mass-conservation error returned by the update.
    &   energy_budget_error_j, &       ! [J] Energy-conservation error returned by the update.
    &   maximum_negative_volume_m3     ! [m3] Largest negative input-volume magnitude.
    logical :: &
    &   state_is_valid, &              ! [-] Local-state validation result.
    &   nonfinite_input_detected       ! [-] Nonfinite-input detection result.

    call update_local_water_ice_state( &
    &   liquid_water_volume_m3=water_volume_m3, &
    &   liquid_water_temperature_k=water_temperature_k, &
    &   surface_ice_volume_m3=surface_ice_volume_m3, &
    &   excess_ice_volume_m3=excess_ice_volume_m3, &
    &   liquid_water_added_energy_j=water_added_energy_j, &
    &   surface_ice_added_energy_j=surface_ice_added_energy_j, &
    &   excess_ice_added_energy_j=excess_ice_added_energy_j, &
    &   frozen_water_mass_kg=frozen_mass_kg, &
    &   surface_ice_melted_mass_kg=surface_melted_mass_kg, &
    &   excess_ice_melted_mass_kg=excess_melted_mass_kg, &
    &   unapplied_energy_j=unapplied_energy_j, &
    &   mass_budget_error_kg=mass_budget_error_kg, &
    &   energy_budget_error_j=energy_budget_error_j, &
    &   state_is_valid=state_is_valid, &
    &   nonfinite_input_detected=nonfinite_input_detected, &
    &   maximum_negative_volume_m3=maximum_negative_volume_m3)

    call assert_true(state_is_valid, 'separate-budget local state is valid')
    call assert_true(.not. nonfinite_input_detected, 'separate-budget inputs are finite')
    call assert_close(mass_budget_error_kg, 0.0_JPRB, 1.0e-12_JPRB, &
    &   'separate-budget mass conservation [kg]')
    call assert_close(energy_budget_error_j, 0.0_JPRB, 1.0e-6_JPRB, &
    &   'separate-budget energy conservation [J]')
end subroutine check_separate_update


subroutine test_ice_longwave_fluxes()
    real(kind=JPRB) :: &
    &   downward_longwave_w_m2, &       ! [W m-2] Downward longwave radiation incident on ice.
    &   ice_surface_temperature_k, &    ! [K] Prescribed upper-surface ice temperature.
    &   absorbed_longwave_w_m2, &       ! [W m-2] Longwave radiation absorbed by ice.
    &   emitted_longwave_w_m2, &        ! [W m-2] Longwave radiation emitted by ice.
    &   expected_absorbed_w_m2, &       ! [W m-2] Hand-calculated absorbed longwave radiation.
    &   expected_emitted_w_m2           ! [W m-2] Hand-calculated emitted longwave radiation.

    downward_longwave_w_m2 = 300.0_JPRB
    ice_surface_temperature_k = 260.0_JPRB
    expected_absorbed_w_m2 = ICE_LONGWAVE_EMISSIVITY * downward_longwave_w_m2
    expected_emitted_w_m2 = ICE_LONGWAVE_EMISSIVITY * SB * ice_surface_temperature_k**4

    absorbed_longwave_w_m2 = calc_ice_absorbed_longwave_flux(downward_longwave_w_m2)
    emitted_longwave_w_m2 = calc_ice_emitted_longwave_flux(ice_surface_temperature_k)

    call assert_close(absorbed_longwave_w_m2, expected_absorbed_w_m2, 1.0e-13_JPRB, &
    &   'ice absorbed longwave flux [W m-2]')
    call assert_close(emitted_longwave_w_m2, expected_emitted_w_m2, 1.0e-13_JPRB, &
    &   'ice emitted longwave flux [W m-2]')
end subroutine test_ice_longwave_fluxes


subroutine test_ice_surface_heat_flux()
    real(kind=JPRB) :: &
    &   net_ice_heat_flux_w_m2, &      ! [W m-2] Computed net atmospheric heat flux into ice.
    &   transmitted_shortwave_w_m2, & ! [W m-2] Computed shortwave radiation below the ice.
    &   upward_conductive_heat_flux_w_m2, & ! [W m-2] Computed bottom-to-surface ice conduction.
    &   downward_shortwave_w_m2, &    ! [W m-2] Downward shortwave radiation above the ice.
    &   downward_longwave_w_m2, &     ! [W m-2] Downward longwave radiation above the ice.
    &   air_temperature_k, &          ! [K] Near-surface air temperature.
    &   ice_thickness_m, &            ! [m] Mean ice thickness.
    &   ice_surface_temperature_k, &  ! [K] Computed upper-surface ice temperature.
    &   bottom_thermal_conductance_w_m2_k, & ! [W m-2 K-1] Conductance to ice bottom at TMELT.
    &   expected_transmitted_w_m2, &  ! [W m-2] Expected shortwave transmission through the ice.
    &   expected_net_flux_w_m2, &     ! [W m-2] Expected net atmospheric heat flux into ice.
    &   newton_residual_w_m2          ! [W m-2] Ice-surface energy-balance residual.
    integer :: &
    &   newton_iteration_count        ! [-] Number of Newton updates performed.
    logical :: &
    &   newton_converged              ! [-] True when the Newton residual meets its tolerance.

    downward_shortwave_w_m2 = 120.0_JPRB
    downward_longwave_w_m2 = 280.0_JPRB
    ice_thickness_m = 0.2_JPRB
    bottom_thermal_conductance_w_m2_k = KI / ice_thickness_m
    expected_transmitted_w_m2 = (1.0_JPRB - iceSWref) * &
    &   downward_shortwave_w_m2 * exp(-iceSWatten * ice_thickness_m)

    ! Cold forcing produces a linear temperature gradient whose upward
    ! conductive flux balances the atmospheric loss at the upper surface.
    air_temperature_k = TMELT - 10.0_JPRB
    call calc_ice_surface_heat_flux( &
    &   net_ice_heat_flux_w_m2, transmitted_shortwave_w_m2, &
    &   ice_surface_temperature_k, upward_conductive_heat_flux_w_m2, &
    &   newton_iteration_count, newton_residual_w_m2, newton_converged, &
    &   downward_shortwave_w_m2, downward_longwave_w_m2, &
    &   air_temperature_k, ice_thickness_m, bottom_thermal_conductance_w_m2_k, 4)

    call assert_close(transmitted_shortwave_w_m2, expected_transmitted_w_m2, 1.0e-13_JPRB, &
    &   'cold ice shortwave transmission [W m-2]')
    call assert_close(net_ice_heat_flux_w_m2 + upward_conductive_heat_flux_w_m2, &
    &   0.0_JPRB, ICE_SURFACE_NEWTON_RESIDUAL_TOLERANCE_W_M2, &
    &   'cold ice surface energy balance [W m-2]')
    call assert_true(newton_converged, 'cold ice Newton solve converged')
    call assert_true(newton_iteration_count >= 1 .and. newton_iteration_count < 4, &
    &   'cold ice Newton solve exits before its maximum iteration count')
    call assert_close(newton_residual_w_m2, &
    &   abs(net_ice_heat_flux_w_m2 + upward_conductive_heat_flux_w_m2), &
    &   1.0e-13_JPRB, 'cold ice reported Newton residual [W m-2]')
    call assert_close(atmospheric_ice_flux_w_m2( &
    &   ice_surface_temperature_k, downward_shortwave_w_m2, &
    &   transmitted_shortwave_w_m2, downward_longwave_w_m2, air_temperature_k), &
    &   net_ice_heat_flux_w_m2, 1.0e-12_JPRB, 'cold ice atmospheric flux [W m-2]')
    call assert_close(min(ice_surface_temperature_k, TMELT), &
    &   ice_surface_temperature_k, 0.0_JPRB, 'cold ice surface temperature cap [K]')
    if (ice_surface_temperature_k >= TMELT .or. &
    &   upward_conductive_heat_flux_w_m2 <= 0.0_JPRB) then
        error stop '[TEST FAILED] cold ice must have a positive temperature gradient'
    endif

    ! Warm forcing leaves the surface at TMELT and supplies energy for melting.
    air_temperature_k = TMELT + 3.0_JPRB
    expected_net_flux_w_m2 = &
    &   (1.0_JPRB - iceSWref) * downward_shortwave_w_m2 - expected_transmitted_w_m2 + &
    &   ICE_LONGWAVE_EMISSIVITY * downward_longwave_w_m2 - &
    &   ICE_LONGWAVE_EMISSIVITY * SB * TMELT**4 + &
    &   Kice2air * (air_temperature_k - TMELT)

    call calc_ice_surface_heat_flux( &
    &   net_ice_heat_flux_w_m2, transmitted_shortwave_w_m2, &
    &   ice_surface_temperature_k, upward_conductive_heat_flux_w_m2, &
    &   newton_iteration_count, newton_residual_w_m2, newton_converged, &
    &   downward_shortwave_w_m2, downward_longwave_w_m2, &
    &   air_temperature_k, ice_thickness_m, bottom_thermal_conductance_w_m2_k, 4)

    call assert_close(ice_surface_temperature_k, TMELT, 0.0_JPRB, &
    &   'melting ice surface temperature [K]')
    call assert_close(upward_conductive_heat_flux_w_m2, 0.0_JPRB, 0.0_JPRB, &
    &   'melting ice conductive flux [W m-2]')
    call assert_close(net_ice_heat_flux_w_m2, expected_net_flux_w_m2, 1.0e-13_JPRB, &
    &   'melting ice atmospheric flux [W m-2]')
    call assert_true(newton_converged, 'melting ice does not require a Newton solve')
    call assert_true(newton_iteration_count == 0, &
    &   'melting ice uses zero Newton iterations')
    call assert_close(newton_residual_w_m2, 0.0_JPRB, 0.0_JPRB, &
    &   'melting ice Newton residual [W m-2]')

    ! An insulated ice body cools until its atmospheric surface flux is zero.
    air_temperature_k = TMELT - 10.0_JPRB
    call calc_ice_surface_heat_flux( &
    &   net_ice_heat_flux_w_m2, transmitted_shortwave_w_m2, &
    &   ice_surface_temperature_k, upward_conductive_heat_flux_w_m2, &
    &   newton_iteration_count, newton_residual_w_m2, newton_converged, &
    &   downward_shortwave_w_m2, downward_longwave_w_m2, &
    &   air_temperature_k, ice_thickness_m, 0.0_JPRB, 4)

    call assert_close(net_ice_heat_flux_w_m2, 0.0_JPRB, &
    &   ICE_SURFACE_NEWTON_RESIDUAL_TOLERANCE_W_M2, &
    &   'insulated cold ice atmospheric flux [W m-2]')
    call assert_close(upward_conductive_heat_flux_w_m2, 0.0_JPRB, 0.0_JPRB, &
    &   'insulated cold ice conductive flux [W m-2]')
    call assert_close(atmospheric_ice_flux_w_m2( &
    &   ice_surface_temperature_k, downward_shortwave_w_m2, &
    &   transmitted_shortwave_w_m2, downward_longwave_w_m2, air_temperature_k), &
    &   0.0_JPRB, ICE_SURFACE_NEWTON_RESIDUAL_TOLERANCE_W_M2, &
    &   'insulated cold ice surface balance [W m-2]')
    call assert_true(newton_converged, 'insulated cold ice Newton solve converged')
    call assert_true(newton_iteration_count >= 1 .and. newton_iteration_count < 4, &
    &   'insulated cold ice Newton solve exits before its maximum iteration count')

    ! A deliberately insufficient iteration limit must be reported rather than
    ! hiding the remaining surface-balance residual in a component flux.
    call calc_ice_surface_heat_flux( &
    &   net_ice_heat_flux_w_m2, transmitted_shortwave_w_m2, &
    &   ice_surface_temperature_k, upward_conductive_heat_flux_w_m2, &
    &   newton_iteration_count, newton_residual_w_m2, newton_converged, &
    &   downward_shortwave_w_m2, downward_longwave_w_m2, &
    &   air_temperature_k, ice_thickness_m, 0.0_JPRB, 1)
    call assert_true(.not. newton_converged, &
    &   'one-iteration ice-surface solve reports nonconvergence')
    call assert_true(newton_iteration_count == 1, &
    &   'ice-surface solve respects configured maximum iterations')
    call assert_true(newton_residual_w_m2 > &
    &   ICE_SURFACE_NEWTON_RESIDUAL_TOLERANCE_W_M2, &
    &   'nonconverged ice-surface solve retains its residual')
    call assert_close(atmospheric_ice_flux_w_m2( &
    &   ice_surface_temperature_k, downward_shortwave_w_m2, &
    &   transmitted_shortwave_w_m2, downward_longwave_w_m2, air_temperature_k), &
    &   net_ice_heat_flux_w_m2, 1.0e-12_JPRB, &
    &   'nonconverged ice atmospheric flux remains physically evaluated [W m-2]')
end subroutine test_ice_surface_heat_flux


subroutine test_ice_surface_newton_convergence_range()
    integer, parameter :: &
    &   case_count = 4                  ! [-] Number of forcing and ice-geometry cases.
    real(kind=JPRB), parameter :: &
    &   downward_shortwave_w_m2(case_count) = [ &
    &       0.0_JPRB, 0.0_JPRB, 0.0_JPRB, 500.0_JPRB], &
    &   downward_longwave_w_m2(case_count) = [ &
    &       250.0_JPRB, 250.0_JPRB, 100.0_JPRB, 100.0_JPRB], &
    &   air_temperature_k(case_count) = [ &
    &       260.0_JPRB, 240.0_JPRB, 200.0_JPRB, 250.0_JPRB], &
    &   ice_thickness_m(case_count) = [ &
    &       0.01_JPRB, 20.0_JPRB, 1.0_JPRB, 1.0_JPRB], &
    &   bottom_thermal_conductance_w_m2_k(case_count) = [ &
    &       KI / 0.01_JPRB, KI / 20.0_JPRB, 0.0_JPRB, KI]
    real(kind=JPRB) :: &
    &   net_ice_heat_flux_w_m2, &       ! [W m-2] Atmospheric heat flux into ice.
    &   transmitted_shortwave_w_m2, &   ! [W m-2] Shortwave transmitted through ice.
    &   ice_surface_temperature_k, &    ! [K] Diagnosed upper-surface ice temperature.
    &   upward_conductive_heat_flux_w_m2, & ! [W m-2] Bottom-to-surface conductive heat flux.
    &   newton_residual_w_m2             ! [W m-2] Cold-surface energy-balance residual.
    integer :: &
    &   case_index, &                    ! [-] Test-case index.
    &   newton_iteration_count           ! [-] Newton updates performed.
    logical :: &
    &   newton_converged                 ! [-] True when the Newton residual meets its tolerance.

    do case_index = 1, case_count
        call calc_ice_surface_heat_flux( &
        &   net_ice_heat_flux_w_m2, transmitted_shortwave_w_m2, &
        &   ice_surface_temperature_k, upward_conductive_heat_flux_w_m2, &
        &   newton_iteration_count, newton_residual_w_m2, newton_converged, &
        &   downward_shortwave_w_m2(case_index), downward_longwave_w_m2(case_index), &
        &   air_temperature_k(case_index), ice_thickness_m(case_index), &
        &   bottom_thermal_conductance_w_m2_k(case_index), 4)

        call assert_true(newton_converged, &
        &   'ice-surface Newton convergence across geometry and forcing range')
        call assert_true(newton_iteration_count >= 1 .and. newton_iteration_count <= 4, &
        &   'ice-surface Newton iteration count across geometry and forcing range')
        call assert_true(ice_surface_temperature_k >= 1.0_JPRB .and. &
        &   ice_surface_temperature_k < TMELT, &
        &   'ice-surface temperature bounds across geometry and forcing range')
        call assert_abs_le(newton_residual_w_m2, &
        &   ICE_SURFACE_NEWTON_RESIDUAL_TOLERANCE_W_M2, &
        &   'ice-surface Newton residual across geometry and forcing range [W m-2]')
        call assert_close(newton_residual_w_m2, &
        &   abs(net_ice_heat_flux_w_m2 + upward_conductive_heat_flux_w_m2), &
        &   1.0e-13_JPRB, &
        &   'reported ice-surface residual across geometry and forcing range [W m-2]')
    enddo
end subroutine test_ice_surface_newton_convergence_range


pure real(kind=JPRB) function atmospheric_ice_flux_w_m2( &
    &   surface_temperature_k, downward_shortwave_w_m2, &
    &   transmitted_shortwave_w_m2, downward_longwave_w_m2, &
    &   air_temperature_k) result(net_flux_w_m2)
    real(kind=JPRB), intent(in) :: &
    &   surface_temperature_k, &       ! [K] Ice upper-surface temperature.
    &   downward_shortwave_w_m2, &     ! [W m-2] Downward shortwave radiation above the ice.
    &   transmitted_shortwave_w_m2, &  ! [W m-2] Shortwave radiation transmitted through the ice.
    &   downward_longwave_w_m2, &      ! [W m-2] Downward longwave radiation above the ice.
    &   air_temperature_k              ! [K] Near-surface air temperature.

    net_flux_w_m2 = &
    &   (1.0_JPRB - iceSWref) * downward_shortwave_w_m2 - transmitted_shortwave_w_m2 + &
    &   ICE_LONGWAVE_EMISSIVITY * downward_longwave_w_m2 - &
    &   ICE_LONGWAVE_EMISSIVITY * SB * surface_temperature_k**4 + &
    &   Kice2air * (air_temperature_k - surface_temperature_k)
end function atmospheric_ice_flux_w_m2


subroutine check_equilibration( &
    &   liquid_water_volume_m3, liquid_water_temperature_k, &
    &   ice_volume_m3, ice_temperature_k, added_energy_j, &
    &   ice_mass_change_kg, residual_energy_j)
    real(kind=JPRB), intent(inout) :: &
    &   liquid_water_volume_m3, &     ! [m3] Liquid-water volume before and after equilibration.
    &   liquid_water_temperature_k, & ! [K] Liquid-water temperature before and after equilibration.
    &   ice_volume_m3, &              ! [m3] Ice volume before and after equilibration.
    &   ice_temperature_k             ! [K] Ice temperature before and after equilibration.
    real(kind=JPRB), intent(in) :: &
    &   added_energy_j                 ! [J] Energy added to the local water-ice system.
    real(kind=JPRB), intent(out) :: &
    &   ice_mass_change_kg, &          ! [kg] Ice-mass change; positive freezes and negative melts.
    &   residual_energy_j              ! [J] Energy not applied by the phase-change kernel.
    real(kind=JPRB) :: &
    &   mass_before_kg, mass_after_kg, & ! [kg] Total mass before and after equilibration.
    &   energy_before_j, energy_after_j  ! [J] Total energy before and after equilibration.

    mass_before_kg = water_ice_mass_kg(liquid_water_volume_m3, ice_volume_m3)
    energy_before_j = water_ice_energy_j( &
    &   liquid_water_volume_m3, liquid_water_temperature_k, &
    &   ice_volume_m3, ice_temperature_k)

    call equilibrate_water_ice( &
    &   liquid_water_volume_m3, liquid_water_temperature_k, &
    &   ice_volume_m3, ice_temperature_k, added_energy_j, &
    &   ice_mass_change_kg, residual_energy_j)

    mass_after_kg = water_ice_mass_kg(liquid_water_volume_m3, ice_volume_m3)
    energy_after_j = water_ice_energy_j( &
    &   liquid_water_volume_m3, liquid_water_temperature_k, &
    &   ice_volume_m3, ice_temperature_k)

    call assert_close(mass_after_kg, mass_before_kg, 1.0e-12_JPRB, &
    &   'phase-change mass conservation [kg]')
    call assert_close(energy_after_j, &
    &   energy_before_j + added_energy_j - residual_energy_j, 1.0e-12_JPRB, &
    &   'phase-change energy conservation [J]')
    call assert_close(residual_energy_j, 0.0_JPRB, 0.0_JPRB, &
    &   'non-empty system residual energy [J]')
end subroutine check_equilibration


subroutine assert_close(actual_value, expected_value, relative_tolerance, label)
    real(kind=JPRB), intent(in) :: &
    &   actual_value, &       ! [caller-defined unit] Computed value.
    &   expected_value, &     ! [caller-defined unit] Expected value.
    &   relative_tolerance    ! [-] Relative comparison tolerance.
    character(len=*), intent(in) :: &
    &   label                 ! [-] Human-readable assertion label.
    real(kind=JPRB) :: &
    &   absolute_tolerance    ! [caller-defined unit] Scaled absolute tolerance.

    absolute_tolerance = relative_tolerance * max(1.0_JPRB, abs(expected_value))
    if (abs(actual_value - expected_value) <= absolute_tolerance) return

    write(*, '(a)') '[TEST FAILED] '//trim(label)
    write(*, '(a,es24.15)') '  actual   = ', actual_value
    write(*, '(a,es24.15)') '  expected = ', expected_value
    write(*, '(a,es24.15)') '  tolerance= ', absolute_tolerance
    error stop 1
end subroutine assert_close


subroutine assert_abs_le(actual_value, maximum_absolute_value, label)
    real(kind=JPRB), intent(in) :: &
    &   actual_value, &             ! [caller-defined unit] Computed value.
    &   maximum_absolute_value      ! [caller-defined unit] Maximum accepted absolute value.
    character(len=*), intent(in) :: &
    &   label                       ! [-] Human-readable assertion label.

    if (abs(actual_value) <= maximum_absolute_value) return

    write(*, '(a)') '[TEST FAILED] '//trim(label)
    write(*, '(a,es24.15)') '  actual absolute value = ', abs(actual_value)
    write(*, '(a,es24.15)') '  allowed maximum       = ', maximum_absolute_value
    error stop 1
end subroutine assert_abs_le


subroutine assert_finite(actual_value, label)
    real(kind=JPRB), intent(in) :: &
    &   actual_value             ! [caller-defined unit] Computed value.
    character(len=*), intent(in) :: &
    &   label                    ! [-] Human-readable assertion label.

    if (ieee_is_finite(actual_value)) return

    write(*, '(a)') '[TEST FAILED] '//trim(label)
    write(*, '(a,es24.15)') '  actual = ', actual_value
    error stop 1
end subroutine assert_finite


subroutine assert_true(condition, label)
    logical, intent(in) :: &
    &   condition             ! [-] Condition expected to be true.
    character(len=*), intent(in) :: &
    &   label                 ! [-] Human-readable assertion label.

    if (condition) return

    write(*, '(a)') '[TEST FAILED] '//trim(label)
    error stop 1
end subroutine assert_true

end program test_heat_budget
