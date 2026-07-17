program test_heat_budget
    use PARKIND1, only: &
    &   JPRB
    use const_mod, only: &
    &   STO_IGNORE
    use phys_const_mod, only: &
    &   CW, RW, CI, RI, HFUS, TMELT
    use heat_budget_mod, only: &
    &   water_ice_mass_kg, water_ice_energy_j, &
    &   update_liquid_temperature_no_phase_change, equilibrate_water_ice
    implicit none

    call test_liquid_temperature_update()
    call test_partial_freezing()
    call test_complete_freezing_and_ice_cooling()
    call test_partial_melting()
    call test_complete_melting_and_water_warming()
    call test_empty_system_residual()

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
