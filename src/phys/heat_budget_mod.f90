module heat_budget_mod
    use PARKIND1, only: &
    &   JPRB
    use const_mod, only: &
    &   STO_IGNORE
    use phys_const_mod, only: &
    &   CW, RW, CI, RI, HFUS, TMELT
    implicit none
    private

    public :: &
    &   liquid_water_energy_j, ice_energy_j, &
    &   water_ice_mass_kg, water_ice_energy_j, &
    &   update_liquid_temperature_no_phase_change, &
    &   equilibrate_water_ice

    real(kind=JPRB), parameter :: &
    &   MASS_IGNORE_KG = RW * real(STO_IGNORE, kind=JPRB)

contains

pure elemental real(kind=JPRB) function liquid_water_energy_j( &
    &   liquid_water_volume_m3, liquid_water_temperature_k) result(energy_j)
    real(kind=JPRB), intent(in) :: &
    &   liquid_water_volume_m3, &     ! [m3] Liquid-water volume.
    &   liquid_water_temperature_k    ! [K] Liquid-water temperature.

    ! Sensible energy is referenced to liquid water at the melting temperature.
    energy_j = CW * RW * liquid_water_volume_m3 * &
    &   (liquid_water_temperature_k - TMELT)
end function liquid_water_energy_j


pure elemental real(kind=JPRB) function ice_energy_j( &
    &   ice_volume_m3, ice_temperature_k) result(energy_j)
    real(kind=JPRB), intent(in) :: &
    &   ice_volume_m3, &       ! [m3] Ice volume.
    &   ice_temperature_k      ! [K] Ice temperature.

    ! Energy is referenced to liquid water at the melting temperature.
    energy_j = RI * ice_volume_m3 * &
    &   (CI * (ice_temperature_k - TMELT) - HFUS)
end function ice_energy_j


pure elemental real(kind=JPRB) function water_ice_mass_kg( &
    &   liquid_water_volume_m3, ice_volume_m3) result(mass_kg)
    real(kind=JPRB), intent(in) :: &
    &   liquid_water_volume_m3, & ! [m3] Liquid-water volume.
    &   ice_volume_m3              ! [m3] Ice volume.

    mass_kg = RW * liquid_water_volume_m3 + RI * ice_volume_m3
end function water_ice_mass_kg


pure elemental real(kind=JPRB) function water_ice_energy_j( &
    &   liquid_water_volume_m3, liquid_water_temperature_k, &
    &   ice_volume_m3, ice_temperature_k) result(energy_j)
    real(kind=JPRB), intent(in) :: &
    &   liquid_water_volume_m3, &  ! [m3] Liquid-water volume.
    &   liquid_water_temperature_k, & ! [K] Liquid-water temperature.
    &   ice_volume_m3, &           ! [m3] Ice volume.
    &   ice_temperature_k          ! [K] Ice temperature.

    energy_j = &
    &   liquid_water_energy_j(liquid_water_volume_m3, liquid_water_temperature_k) + &
    &   ice_energy_j(ice_volume_m3, ice_temperature_k)
end function water_ice_energy_j


pure elemental subroutine update_liquid_temperature_no_phase_change( &
    &   liquid_water_temperature_k, liquid_water_volume_m3, added_energy_j)
    real(kind=JPRB), intent(inout) :: &
    &   liquid_water_temperature_k ! [K] Liquid-water temperature before and after heating.
    real(kind=JPRB), intent(in) :: &
    &   liquid_water_volume_m3, &  ! [m3] Liquid-water volume.
    &   added_energy_j              ! [J] Energy added to liquid water; positive warms water.
    real(kind=JPRB) :: &
    &   temperature_change_k        ! [K] Temperature increment caused by added energy.

    if (liquid_water_volume_m3 < real(STO_IGNORE, kind=JPRB)) return

    temperature_change_k = added_energy_j / (CW * RW * liquid_water_volume_m3)
    liquid_water_temperature_k = liquid_water_temperature_k + temperature_change_k
end subroutine update_liquid_temperature_no_phase_change


pure elemental subroutine equilibrate_water_ice( &
    &   liquid_water_volume_m3, liquid_water_temperature_k, &
    &   ice_volume_m3, ice_temperature_k, &
    &   added_energy_j, ice_mass_change_kg, residual_energy_j)
    real(kind=JPRB), intent(inout) :: &
    &   liquid_water_volume_m3, &     ! [m3] Liquid-water volume before and after equilibration.
    &   liquid_water_temperature_k, & ! [K] Liquid-water temperature before and after equilibration.
    &   ice_volume_m3, &              ! [m3] Ice volume before and after equilibration.
    &   ice_temperature_k             ! [K] Ice temperature before and after equilibration.
    real(kind=JPRB), intent(in) :: &
    &   added_energy_j                 ! [J] Energy added to the local water-ice system.
    real(kind=JPRB), intent(out) :: &
    &   ice_mass_change_kg, &          ! [kg] Ice-mass change; positive freezes and negative melts.
    &   residual_energy_j              ! [J] Energy not applied because the system contains no mass.
    real(kind=JPRB) :: &
    &   initial_ice_mass_kg, &         ! [kg] Ice mass before equilibration.
    &   final_ice_mass_kg, &           ! [kg] Ice mass after equilibration.
    &   total_mass_kg, &               ! [kg] Conserved water-plus-ice mass.
    &   total_energy_j                 ! [J] System energy after applying added energy.

    initial_ice_mass_kg = RI * ice_volume_m3
    total_mass_kg = water_ice_mass_kg(liquid_water_volume_m3, ice_volume_m3)

    ice_mass_change_kg = 0.0_JPRB
    residual_energy_j = 0.0_JPRB
    if (total_mass_kg < MASS_IGNORE_KG) then
        residual_energy_j = added_energy_j
        return
    endif

    total_energy_j = water_ice_energy_j( &
    &   liquid_water_volume_m3, liquid_water_temperature_k, &
    &   ice_volume_m3, ice_temperature_k) + added_energy_j

    if (total_energy_j >= 0.0_JPRB) then
        ! All mass is liquid; remaining energy becomes liquid sensible heat.
        liquid_water_volume_m3 = total_mass_kg / RW
        liquid_water_temperature_k = TMELT + total_energy_j / (CW * total_mass_kg)
        ice_volume_m3 = 0.0_JPRB
        ice_temperature_k = TMELT
    else if (total_energy_j <= -total_mass_kg * HFUS) then
        ! All mass is ice; energy below the latent-heat threshold cools the ice.
        liquid_water_volume_m3 = 0.0_JPRB
        liquid_water_temperature_k = TMELT
        ice_volume_m3 = total_mass_kg / RI
        ice_temperature_k = TMELT + &
        &   (total_energy_j / total_mass_kg + HFUS) / CI
    else
        ! Liquid water and ice coexist at the melting temperature.
        final_ice_mass_kg = -total_energy_j / HFUS
        liquid_water_volume_m3 = (total_mass_kg - final_ice_mass_kg) / RW
        liquid_water_temperature_k = TMELT
        ice_volume_m3 = final_ice_mass_kg / RI
        ice_temperature_k = TMELT
    endif

    final_ice_mass_kg = RI * ice_volume_m3
    ice_mass_change_kg = final_ice_mass_kg - initial_ice_mass_kg
end subroutine equilibrate_water_ice

end module heat_budget_mod
