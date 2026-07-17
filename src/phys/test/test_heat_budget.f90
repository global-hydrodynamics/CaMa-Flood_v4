program test_heat_budget
    use PARKIND1, only: &
    &   JPRB
    use const_mod, only: &
    &   STO_IGNORE
    use phys_const_mod, only: &
    &   CW, RW, CI, RI, HFUS, TMELT, SB, &
    &   iceSWref, iceLWref, iceSWatten, Kice2air
    use heat_flux_mod, only: &
    &   calc_ice_surface_heat_flux
    use heat_budget_mod, only: &
    &   water_ice_mass_kg, water_ice_energy_j, &
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
    call test_ice_surface_heat_flux()

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
    &   energy_budget_error_j          ! [J] Energy-conservation error returned by the update.

    call update_local_water_ice_state( &
    &   water_volume_m3, water_temperature_k, surface_ice_volume_m3, excess_ice_volume_m3, &
    &   water_added_energy_j, surface_ice_added_energy_j, excess_ice_added_energy_j, &
    &   frozen_mass_kg, surface_melted_mass_kg, excess_melted_mass_kg, unapplied_energy_j, &
    &   mass_budget_error_kg, energy_budget_error_j)

    call assert_close(mass_budget_error_kg, 0.0_JPRB, 1.0e-12_JPRB, &
    &   'separate-budget mass conservation [kg]')
    call assert_close(energy_budget_error_j, 0.0_JPRB, 1.0e-6_JPRB, &
    &   'separate-budget energy conservation [J]')
end subroutine check_separate_update


subroutine test_ice_surface_heat_flux()
    real(kind=JPRB) :: &
    &   net_ice_heat_flux_w_m2, &      ! [W m-2] Computed net atmospheric heat flux into ice.
    &   transmitted_shortwave_w_m2, & ! [W m-2] Computed shortwave radiation below the ice.
    &   downward_shortwave_w_m2, &    ! [W m-2] Downward shortwave radiation above the ice.
    &   downward_longwave_w_m2, &     ! [W m-2] Downward longwave radiation above the ice.
    &   air_temperature_k, &          ! [K] Near-surface air temperature.
    &   ice_thickness_m, &            ! [m] Mean ice thickness.
    &   surface_temperature_k, &      ! [K] Expected ice-surface temperature.
    &   expected_transmitted_w_m2, &  ! [W m-2] Expected shortwave transmission through the ice.
    &   expected_net_flux_w_m2        ! [W m-2] Expected net atmospheric heat flux into ice.

    downward_shortwave_w_m2 = 120.0_JPRB
    downward_longwave_w_m2 = 280.0_JPRB
    air_temperature_k = TMELT + 3.0_JPRB
    ice_thickness_m = 0.2_JPRB
    surface_temperature_k = TMELT
    expected_transmitted_w_m2 = (1.0_JPRB - iceSWref) * &
    &   downward_shortwave_w_m2 * exp(-iceSWatten * ice_thickness_m)
    expected_net_flux_w_m2 = &
    &   (1.0_JPRB - iceSWref) * downward_shortwave_w_m2 - expected_transmitted_w_m2 + &
    &   (1.0_JPRB - iceLWref) * downward_longwave_w_m2 - &
    &   (1.0_JPRB - iceLWref) * SB * surface_temperature_k**4 + &
    &   Kice2air * (air_temperature_k - surface_temperature_k)

    call calc_ice_surface_heat_flux( &
    &   net_ice_heat_flux_w_m2, transmitted_shortwave_w_m2, &
    &   downward_shortwave_w_m2, downward_longwave_w_m2, &
    &   air_temperature_k, ice_thickness_m)

    call assert_close(transmitted_shortwave_w_m2, expected_transmitted_w_m2, 1.0e-13_JPRB, &
    &   'ice surface shortwave transmission [W m-2]')
    call assert_close(net_ice_heat_flux_w_m2, expected_net_flux_w_m2, 1.0e-13_JPRB, &
    &   'ice surface net atmospheric flux [W m-2]')
end subroutine test_ice_surface_heat_flux


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

end program test_heat_budget
