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
    &   update_local_water_ice_state, &
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


pure elemental subroutine update_local_water_ice_state( &
    &   liquid_water_volume_m3, liquid_water_temperature_k, &
    &   surface_ice_volume_m3, excess_ice_volume_m3, &
    &   liquid_water_added_energy_j, surface_ice_added_energy_j, &
    &   excess_ice_added_energy_j, &
    &   frozen_water_mass_kg, surface_ice_melted_mass_kg, &
    &   excess_ice_melted_mass_kg, unapplied_energy_j, &
    &   mass_budget_error_kg, energy_budget_error_j)
    real(kind=JPRB), intent(inout) :: &
    &   liquid_water_volume_m3, &     ! [m3] Liquid-water volume before and after the local update.
    &   liquid_water_temperature_k, & ! [K] Liquid-water temperature before and after the local update.
    &   surface_ice_volume_m3, &      ! [m3] Water-surface ice volume before and after the local update.
    &   excess_ice_volume_m3          ! [m3] Immobile excess-ice volume before and after the local update.
    real(kind=JPRB), intent(in) :: &
    &   liquid_water_added_energy_j, & ! [J] Energy added directly to liquid water; positive warms water.
    &   surface_ice_added_energy_j, &  ! [J] Energy added to water-surface ice; positive melts ice.
    &   excess_ice_added_energy_j      ! [J] Energy added to immobile excess ice; only positive energy is applied.
    real(kind=JPRB), intent(out) :: &
    &   frozen_water_mass_kg, &        ! [kg] Liquid-water mass converted to water-surface ice.
    &   surface_ice_melted_mass_kg, &  ! [kg] Water-surface ice mass converted to liquid water.
    &   excess_ice_melted_mass_kg, &   ! [kg] Immobile excess-ice mass converted to liquid water.
    &   unapplied_energy_j, &           ! [J] Input energy not applied; it retains the input-energy sign.
    &   mass_budget_error_kg, &         ! [kg] Final minus initial total water-plus-ice mass.
    &   energy_budget_error_j           ! [J] Error in final = initial + input - unapplied energy.
    real(kind=JPRB) :: &
    &   initial_mass_kg, final_mass_kg, &       ! [kg] Total local water-plus-ice mass.
    &   initial_energy_j, final_energy_j, &     ! [J] Total local water-plus-ice energy.
    &   total_input_energy_j, &                 ! [J] Sum of the three external energy increments.
    &   pending_liquid_energy_j, &              ! [J] Energy to apply after ice melting.
    &   liquid_sensible_energy_j, &             ! [J] Liquid sensible energy relative to water at TMELT.
    &   surface_ice_mass_kg, excess_ice_mass_kg, & ! [kg] Ice mass available before melting.
    &   surface_freeze_energy_demand_j, &       ! [J] Positive latent-energy magnitude requested by surface cooling.
    &   liquid_freeze_energy_demand_j, &        ! [J] Positive latent-energy magnitude requested by water cooling.
    &   total_freeze_energy_demand_j, &         ! [J] Total positive energy magnitude to release by freezing.
    &   freeze_energy_per_mass_j_kg, &          ! [J kg-1] Energy removed when warm liquid becomes ice at TMELT.
    &   requested_frozen_mass_kg, &             ! [kg] Water mass needed to satisfy the freezing-energy demand.
    &   available_liquid_mass_kg, &             ! [kg] Liquid-water mass available for freezing.
    &   total_melted_mass_kg, &                 ! [kg] Meltwater added from both ice pools.
    &   surface_melt_energy_j, &                ! [J] Energy consumed by water-surface ice melting.
    &   excess_melt_energy_j, &                 ! [J] Energy consumed by excess-ice melting.
    &   remaining_freeze_energy_j               ! [J] Positive cooling demand that cannot be satisfied.

    ! Both ice pools are represented at TMELT in this local river-ice scheme.
    initial_mass_kg = water_ice_mass_kg( &
    &   liquid_water_volume_m3, surface_ice_volume_m3 + excess_ice_volume_m3)
    initial_energy_j = water_ice_energy_j( &
    &   liquid_water_volume_m3, liquid_water_temperature_k, &
    &   surface_ice_volume_m3 + excess_ice_volume_m3, TMELT)
    total_input_energy_j = liquid_water_added_energy_j + &
    &   surface_ice_added_energy_j + excess_ice_added_energy_j

    frozen_water_mass_kg = 0.0_JPRB
    surface_ice_melted_mass_kg = 0.0_JPRB
    excess_ice_melted_mass_kg = 0.0_JPRB
    unapplied_energy_j = 0.0_JPRB
    pending_liquid_energy_j = liquid_water_added_energy_j
    surface_freeze_energy_demand_j = 0.0_JPRB
    liquid_freeze_energy_demand_j = 0.0_JPRB

    ! Diagnose the water-surface ice response. Positive excess energy after
    ! complete melting is passed to the liquid-water budget.
    surface_ice_mass_kg = max(surface_ice_volume_m3, 0.0_JPRB) * RI
    if (surface_ice_added_energy_j > 0.0_JPRB) then
        if (surface_ice_mass_kg > 0.0_JPRB) then
            surface_melt_energy_j = min( &
            &   surface_ice_added_energy_j, surface_ice_mass_kg * HFUS)
            surface_ice_melted_mass_kg = surface_melt_energy_j / HFUS
            pending_liquid_energy_j = pending_liquid_energy_j + &
            &   surface_ice_added_energy_j - surface_melt_energy_j
        else
            unapplied_energy_j = unapplied_energy_j + surface_ice_added_energy_j
        endif
    else if (surface_ice_added_energy_j < 0.0_JPRB) then
        if (surface_ice_mass_kg > 0.0_JPRB) then
            surface_freeze_energy_demand_j = -surface_ice_added_energy_j
        else
            unapplied_energy_j = unapplied_energy_j + surface_ice_added_energy_j
        endif
    endif

    ! Immobile excess ice can melt locally but cannot grow. Negative energy is
    ! therefore reported as unapplied instead of creating new excess ice.
    excess_ice_mass_kg = max(excess_ice_volume_m3, 0.0_JPRB) * RI
    if (excess_ice_added_energy_j > 0.0_JPRB) then
        if (excess_ice_mass_kg > 0.0_JPRB) then
            excess_melt_energy_j = min( &
            &   excess_ice_added_energy_j, excess_ice_mass_kg * HFUS)
            excess_ice_melted_mass_kg = excess_melt_energy_j / HFUS
            pending_liquid_energy_j = pending_liquid_energy_j + &
            &   excess_ice_added_energy_j - excess_melt_energy_j
        else
            unapplied_energy_j = unapplied_energy_j + excess_ice_added_energy_j
        endif
    else if (excess_ice_added_energy_j < 0.0_JPRB) then
        unapplied_energy_j = unapplied_energy_j + excess_ice_added_energy_j
    endif

    ! Melted ice enters the liquid pool at TMELT. Recomputing the liquid
    ! temperature from its unchanged sensible energy performs conservative mixing.
    liquid_sensible_energy_j = liquid_water_energy_j( &
    &   liquid_water_volume_m3, liquid_water_temperature_k)
    surface_ice_volume_m3 = max(surface_ice_volume_m3 - &
    &   surface_ice_melted_mass_kg / RI, 0.0_JPRB)
    excess_ice_volume_m3 = max(excess_ice_volume_m3 - &
    &   excess_ice_melted_mass_kg / RI, 0.0_JPRB)
    total_melted_mass_kg = surface_ice_melted_mass_kg + excess_ice_melted_mass_kg
    liquid_water_volume_m3 = liquid_water_volume_m3 + total_melted_mass_kg / RW
    if (liquid_water_volume_m3 > 0.0_JPRB) then
        liquid_water_temperature_k = TMELT + liquid_sensible_energy_j / &
        &   (CW * RW * liquid_water_volume_m3)
    else
        liquid_water_temperature_k = TMELT
    endif

    ! Apply the liquid-water energy budget without imposing instantaneous
    ! equilibrium with existing ice. Cooling below TMELT is converted into a
    ! separate freezing-energy demand.
    if (liquid_water_volume_m3 > 0.0_JPRB) then
        liquid_sensible_energy_j = liquid_water_energy_j( &
        &   liquid_water_volume_m3, liquid_water_temperature_k) + pending_liquid_energy_j
        if (liquid_sensible_energy_j >= 0.0_JPRB) then
            liquid_water_temperature_k = TMELT + liquid_sensible_energy_j / &
            &   (CW * RW * liquid_water_volume_m3)
        else
            liquid_water_temperature_k = TMELT
            liquid_freeze_energy_demand_j = -liquid_sensible_energy_j
        endif
    else
        unapplied_energy_j = unapplied_energy_j + pending_liquid_energy_j
    endif

    ! Surface cooling and liquid supercooling both create water-surface ice.
    ! Warm liquid requires additional sensible cooling for each frozen kilogram.
    total_freeze_energy_demand_j = surface_freeze_energy_demand_j + &
    &   liquid_freeze_energy_demand_j
    if (total_freeze_energy_demand_j > 0.0_JPRB) then
        available_liquid_mass_kg = RW * max(liquid_water_volume_m3, 0.0_JPRB)
        freeze_energy_per_mass_j_kg = HFUS + CW * &
        &   max(liquid_water_temperature_k - TMELT, 0.0_JPRB)
        requested_frozen_mass_kg = total_freeze_energy_demand_j / &
        &   freeze_energy_per_mass_j_kg
        frozen_water_mass_kg = min(requested_frozen_mass_kg, available_liquid_mass_kg)

        liquid_water_volume_m3 = max( &
        &   liquid_water_volume_m3 - frozen_water_mass_kg / RW, 0.0_JPRB)
        surface_ice_volume_m3 = surface_ice_volume_m3 + frozen_water_mass_kg / RI
        if (liquid_water_volume_m3 <= 0.0_JPRB) liquid_water_temperature_k = TMELT

        remaining_freeze_energy_j = total_freeze_energy_demand_j - &
        &   frozen_water_mass_kg * freeze_energy_per_mass_j_kg
        if (remaining_freeze_energy_j > 0.0_JPRB) then
            unapplied_energy_j = unapplied_energy_j - remaining_freeze_energy_j
        endif
    endif

    final_mass_kg = water_ice_mass_kg( &
    &   liquid_water_volume_m3, surface_ice_volume_m3 + excess_ice_volume_m3)
    final_energy_j = water_ice_energy_j( &
    &   liquid_water_volume_m3, liquid_water_temperature_k, &
    &   surface_ice_volume_m3 + excess_ice_volume_m3, TMELT)
    mass_budget_error_kg = final_mass_kg - initial_mass_kg
    energy_budget_error_j = final_energy_j - &
    &   (initial_energy_j + total_input_energy_j - unapplied_energy_j)
end subroutine update_local_water_ice_state


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
