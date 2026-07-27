module heat_budget_mod
    use, intrinsic :: ieee_arithmetic, only: &
    &   ieee_is_finite
    use PARKIND1, only: &
    &   JPRB
    use const_mod, only: &
    &   STO_IGNORE
    use phys_const_mod, only: &
    &   CW, RW, CI, RI, HFUS, TMELT
    implicit none
    private

    public :: &
    &   NEGATIVE_VOLUME_TOLERANCE_M3, &
    &   liquid_water_energy_j, ice_energy_j, &
    &   water_ice_mass_kg, water_ice_energy_j, &
    &   update_liquid_temperature_no_phase_change, &
    &   update_local_water_ice_state, &
    &   equilibrate_water_ice

    real(kind=JPRB), parameter :: &
    &   MASS_IGNORE_KG = RW * real(STO_IGNORE, kind=JPRB) ! [kg] Mass threshold for empty-system handling.
    real(kind=JPRB), parameter :: &
    &   NEGATIVE_VOLUME_TOLERANCE_M3 = 1.0e-10_JPRB ! [m3] Largest roundoff-scale negative volume normalized to zero.

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
    &   mass_budget_error_kg, energy_budget_error_j, &
    &   state_is_valid, nonfinite_input_detected, maximum_negative_volume_m3)
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
    &   mass_budget_error_kg, &         ! [kg] Final minus unnormalized initial water-plus-ice mass.
    &   energy_budget_error_j, &        ! [J] Error against unnormalized initial plus input minus unapplied energy.
    &   maximum_negative_volume_m3      ! [m3] Largest negative input volume magnitude.
    logical, intent(out) :: &
    &   state_is_valid, &               ! [-] True when all inputs are finite and volumes are within tolerance.
    &   nonfinite_input_detected        ! [-] True when any state or energy input is NaN or infinite.
    real(kind=JPRB) :: &
    &   initial_mass_kg, final_mass_kg, &       ! [kg] Total local water-plus-ice mass.
    &   initial_energy_j, final_energy_j, &     ! [J] Total local water-plus-ice energy.
    &   total_input_energy_j, &                 ! [J] Sum of the three external energy increments.
    &   normalization_mass_adjustment_kg, &     ! [kg] Mass introduced by normalizing tiny negative volumes.
    &   normalization_energy_adjustment_j, &    ! [J] Energy introduced by normalizing tiny negative volumes.
    &   pending_liquid_energy_j, &              ! [J] Energy to apply after ice melting.
    &   surface_freeze_energy_demand_j, &       ! [J] Positive latent-energy magnitude requested by surface cooling.
    &   excess_freeze_energy_demand_j, &        ! [J] Unused growth demand returned for immobile excess ice.
    &   liquid_freeze_energy_demand_j, &        ! [J] Positive latent-energy magnitude requested by water cooling.
    &   surface_liquid_excess_energy_j, &       ! [J] Surface-ice energy left after complete melting.
    &   excess_liquid_excess_energy_j, &        ! [J] Excess-ice energy left after complete melting.
    &   surface_unapplied_energy_j, &           ! [J] Surface-ice energy that cannot be applied.
    &   excess_unapplied_energy_j               ! [J] Excess-ice energy that cannot be applied.

    frozen_water_mass_kg = 0.0_JPRB
    surface_ice_melted_mass_kg = 0.0_JPRB
    excess_ice_melted_mass_kg = 0.0_JPRB
    unapplied_energy_j = 0.0_JPRB
    mass_budget_error_kg = 0.0_JPRB
    energy_budget_error_j = 0.0_JPRB

    call validate_and_normalize_local_state( &
    &   liquid_water_volume_m3=liquid_water_volume_m3, &
    &   liquid_water_temperature_k=liquid_water_temperature_k, &
    &   surface_ice_volume_m3=surface_ice_volume_m3, &
    &   excess_ice_volume_m3=excess_ice_volume_m3, &
    &   liquid_water_added_energy_j=liquid_water_added_energy_j, &
    &   surface_ice_added_energy_j=surface_ice_added_energy_j, &
    &   excess_ice_added_energy_j=excess_ice_added_energy_j, &
    &   state_is_valid=state_is_valid, &
    &   nonfinite_input_detected=nonfinite_input_detected, &
    &   maximum_negative_volume_m3=maximum_negative_volume_m3, &
    &   normalization_mass_adjustment_kg=normalization_mass_adjustment_kg, &
    &   normalization_energy_adjustment_j=normalization_energy_adjustment_j)
    if (.not. state_is_valid) return

    ! The diagnostic zero-layer profiles have no prognostic sensible-heat
    ! storage. Both ice pools are therefore referenced to ice at TMELT here.
    initial_mass_kg = water_ice_mass_kg( &
    &   liquid_water_volume_m3, surface_ice_volume_m3 + excess_ice_volume_m3)
    initial_energy_j = water_ice_energy_j( &
    &   liquid_water_volume_m3, liquid_water_temperature_k, &
    &   surface_ice_volume_m3 + excess_ice_volume_m3, TMELT)
    total_input_energy_j = liquid_water_added_energy_j + &
    &   surface_ice_added_energy_j + excess_ice_added_energy_j

    call diagnose_ice_pool_response( &
    &   ice_volume_m3=surface_ice_volume_m3, &
    &   added_energy_j=surface_ice_added_energy_j, &
    &   ice_growth_is_allowed=.true., &
    &   melted_ice_mass_kg=surface_ice_melted_mass_kg, &
    &   freeze_energy_demand_j=surface_freeze_energy_demand_j, &
    &   liquid_excess_energy_j=surface_liquid_excess_energy_j, &
    &   unapplied_energy_j=surface_unapplied_energy_j)
    call diagnose_ice_pool_response( &
    &   ice_volume_m3=excess_ice_volume_m3, &
    &   added_energy_j=excess_ice_added_energy_j, &
    &   ice_growth_is_allowed=.false., &
    &   melted_ice_mass_kg=excess_ice_melted_mass_kg, &
    &   freeze_energy_demand_j=excess_freeze_energy_demand_j, &
    &   liquid_excess_energy_j=excess_liquid_excess_energy_j, &
    &   unapplied_energy_j=excess_unapplied_energy_j)
    pending_liquid_energy_j = liquid_water_added_energy_j + &
    &   surface_liquid_excess_energy_j + excess_liquid_excess_energy_j
    unapplied_energy_j = surface_unapplied_energy_j + excess_unapplied_energy_j

    call mix_meltwater_into_liquid( &
    &   liquid_water_volume_m3=liquid_water_volume_m3, &
    &   liquid_water_temperature_k=liquid_water_temperature_k, &
    &   surface_ice_volume_m3=surface_ice_volume_m3, &
    &   excess_ice_volume_m3=excess_ice_volume_m3, &
    &   surface_ice_melted_mass_kg=surface_ice_melted_mass_kg, &
    &   excess_ice_melted_mass_kg=excess_ice_melted_mass_kg)
    call apply_liquid_energy( &
    &   liquid_water_volume_m3=liquid_water_volume_m3, &
    &   liquid_water_temperature_k=liquid_water_temperature_k, &
    &   added_energy_j=pending_liquid_energy_j, &
    &   freeze_energy_demand_j=liquid_freeze_energy_demand_j, &
    &   unapplied_energy_j=unapplied_energy_j)
    call freeze_liquid_water( &
    &   liquid_water_volume_m3=liquid_water_volume_m3, &
    &   liquid_water_temperature_k=liquid_water_temperature_k, &
    &   surface_ice_volume_m3=surface_ice_volume_m3, &
    &   freeze_energy_demand_j=surface_freeze_energy_demand_j + &
    &       liquid_freeze_energy_demand_j, &
    &   frozen_water_mass_kg=frozen_water_mass_kg, &
    &   unapplied_energy_j=unapplied_energy_j)

    final_mass_kg = water_ice_mass_kg( &
    &   liquid_water_volume_m3, surface_ice_volume_m3 + excess_ice_volume_m3)
    final_energy_j = water_ice_energy_j( &
    &   liquid_water_volume_m3, liquid_water_temperature_k, &
    &   surface_ice_volume_m3 + excess_ice_volume_m3, TMELT)
    mass_budget_error_kg = final_mass_kg - initial_mass_kg + &
    &   normalization_mass_adjustment_kg
    energy_budget_error_j = final_energy_j - &
    &   (initial_energy_j + total_input_energy_j - unapplied_energy_j) + &
    &   normalization_energy_adjustment_j
end subroutine update_local_water_ice_state


pure elemental subroutine validate_and_normalize_local_state( &
    &   liquid_water_volume_m3, liquid_water_temperature_k, &
    &   surface_ice_volume_m3, excess_ice_volume_m3, &
    &   liquid_water_added_energy_j, surface_ice_added_energy_j, &
    &   excess_ice_added_energy_j, state_is_valid, nonfinite_input_detected, &
    &   maximum_negative_volume_m3, normalization_mass_adjustment_kg, &
    &   normalization_energy_adjustment_j)
    real(kind=JPRB), intent(inout) :: &
    &   liquid_water_volume_m3, &     ! [m3] Liquid-water volume to validate and normalize.
    &   liquid_water_temperature_k, & ! [K] Liquid-water temperature to validate.
    &   surface_ice_volume_m3, &      ! [m3] Water-surface ice volume to validate and normalize.
    &   excess_ice_volume_m3          ! [m3] Immobile excess-ice volume to validate and normalize.
    real(kind=JPRB), intent(in) :: &
    &   liquid_water_added_energy_j, & ! [J] Energy added directly to liquid water.
    &   surface_ice_added_energy_j, &  ! [J] Energy added to water-surface ice.
    &   excess_ice_added_energy_j      ! [J] Energy added to immobile excess ice.
    logical, intent(out) :: &
    &   state_is_valid, &              ! [-] True when the inputs may enter the physical update.
    &   nonfinite_input_detected       ! [-] True when any state or energy input is NaN or infinite.
    real(kind=JPRB), intent(out) :: &
    &   maximum_negative_volume_m3, &  ! [m3] Largest negative input volume magnitude.
    &   normalization_mass_adjustment_kg, & ! [kg] Mass introduced by normalization.
    &   normalization_energy_adjustment_j   ! [J] Energy introduced by normalization.
    real(kind=JPRB) :: &
    &   mass_before_normalization_kg, & ! [kg] Total mass before correcting tiny negative volumes.
    &   energy_before_normalization_j   ! [J] Total energy before correcting tiny negative volumes.

    nonfinite_input_detected = .not. ( &
    &   ieee_is_finite(liquid_water_volume_m3) .and. &
    &   ieee_is_finite(liquid_water_temperature_k) .and. &
    &   ieee_is_finite(surface_ice_volume_m3) .and. &
    &   ieee_is_finite(excess_ice_volume_m3) .and. &
    &   ieee_is_finite(liquid_water_added_energy_j) .and. &
    &   ieee_is_finite(surface_ice_added_energy_j) .and. &
    &   ieee_is_finite(excess_ice_added_energy_j))
    maximum_negative_volume_m3 = 0.0_JPRB
    normalization_mass_adjustment_kg = 0.0_JPRB
    normalization_energy_adjustment_j = 0.0_JPRB
    if (nonfinite_input_detected) then
        state_is_valid = .false.
        return
    endif

    maximum_negative_volume_m3 = max( &
    &   -liquid_water_volume_m3, -surface_ice_volume_m3, &
    &   -excess_ice_volume_m3, 0.0_JPRB)
    if (maximum_negative_volume_m3 > NEGATIVE_VOLUME_TOLERANCE_M3) then
        state_is_valid = .false.
        return
    endif

    mass_before_normalization_kg = water_ice_mass_kg( &
    &   liquid_water_volume_m3, surface_ice_volume_m3 + excess_ice_volume_m3)
    energy_before_normalization_j = water_ice_energy_j( &
    &   liquid_water_volume_m3, liquid_water_temperature_k, &
    &   surface_ice_volume_m3 + excess_ice_volume_m3, TMELT)
    liquid_water_volume_m3 = max(liquid_water_volume_m3, 0.0_JPRB)
    surface_ice_volume_m3 = max(surface_ice_volume_m3, 0.0_JPRB)
    excess_ice_volume_m3 = max(excess_ice_volume_m3, 0.0_JPRB)
    normalization_mass_adjustment_kg = water_ice_mass_kg( &
    &   liquid_water_volume_m3, surface_ice_volume_m3 + excess_ice_volume_m3) - &
    &   mass_before_normalization_kg
    normalization_energy_adjustment_j = water_ice_energy_j( &
    &   liquid_water_volume_m3, liquid_water_temperature_k, &
    &   surface_ice_volume_m3 + excess_ice_volume_m3, TMELT) - &
    &   energy_before_normalization_j
    state_is_valid = .true.
end subroutine validate_and_normalize_local_state


pure elemental subroutine diagnose_ice_pool_response( &
    &   ice_volume_m3, added_energy_j, ice_growth_is_allowed, &
    &   melted_ice_mass_kg, freeze_energy_demand_j, &
    &   liquid_excess_energy_j, unapplied_energy_j)
    real(kind=JPRB), intent(in) :: &
    &   ice_volume_m3, &       ! [m3] Ice volume available for melting.
    &   added_energy_j         ! [J] Energy added to the ice pool; positive melts ice.
    logical, intent(in) :: &
    &   ice_growth_is_allowed  ! [-] True when negative energy may freeze liquid water onto this pool.
    real(kind=JPRB), intent(out) :: &
    &   melted_ice_mass_kg, &      ! [kg] Ice mass converted to liquid water.
    &   freeze_energy_demand_j, &  ! [J] Positive cooling demand available for freezing liquid water.
    &   liquid_excess_energy_j, &  ! [J] Positive energy remaining after complete melting.
    &   unapplied_energy_j          ! [J] Energy that cannot be applied to this ice pool.
    real(kind=JPRB) :: &
    &   available_ice_mass_kg, &   ! [kg] Ice mass available before melting.
    &   melt_energy_j              ! [J] Energy consumed by melting ice.

    available_ice_mass_kg = ice_volume_m3 * RI
    melted_ice_mass_kg = 0.0_JPRB
    freeze_energy_demand_j = 0.0_JPRB
    liquid_excess_energy_j = 0.0_JPRB
    unapplied_energy_j = 0.0_JPRB
    if (added_energy_j > 0.0_JPRB) then
        if (available_ice_mass_kg > 0.0_JPRB) then
            if (added_energy_j >= available_ice_mass_kg * HFUS) then
                melt_energy_j = available_ice_mass_kg * HFUS
                melted_ice_mass_kg = available_ice_mass_kg
            else
                melt_energy_j = added_energy_j
                melted_ice_mass_kg = melt_energy_j / HFUS
            endif
            liquid_excess_energy_j = added_energy_j - melt_energy_j
        else
            unapplied_energy_j = added_energy_j
        endif
    else if (added_energy_j < 0.0_JPRB) then
        if (ice_growth_is_allowed .and. available_ice_mass_kg > 0.0_JPRB) then
            freeze_energy_demand_j = -added_energy_j
        else
            unapplied_energy_j = added_energy_j
        endif
    endif
end subroutine diagnose_ice_pool_response


pure elemental subroutine mix_meltwater_into_liquid( &
    &   liquid_water_volume_m3, liquid_water_temperature_k, &
    &   surface_ice_volume_m3, excess_ice_volume_m3, &
    &   surface_ice_melted_mass_kg, excess_ice_melted_mass_kg)
    real(kind=JPRB), intent(inout) :: &
    &   liquid_water_volume_m3, &     ! [m3] Liquid-water volume before and after adding meltwater.
    &   liquid_water_temperature_k, & ! [K] Liquid-water temperature before and after mixing.
    &   surface_ice_volume_m3, &      ! [m3] Water-surface ice volume before and after melting.
    &   excess_ice_volume_m3          ! [m3] Immobile excess-ice volume before and after melting.
    real(kind=JPRB), intent(in) :: &
    &   surface_ice_melted_mass_kg, & ! [kg] Melted water-surface ice mass.
    &   excess_ice_melted_mass_kg     ! [kg] Melted immobile excess-ice mass.
    real(kind=JPRB) :: &
    &   liquid_sensible_energy_j, &   ! [J] Liquid sensible energy before adding meltwater at TMELT.
    &   total_melted_mass_kg          ! [kg] Total meltwater mass entering the liquid pool.

    liquid_sensible_energy_j = liquid_water_energy_j( &
    &   liquid_water_volume_m3, liquid_water_temperature_k)
    if (surface_ice_melted_mass_kg >= surface_ice_volume_m3 * RI) then
        surface_ice_volume_m3 = 0.0_JPRB
    else
        surface_ice_volume_m3 = max( &
        &   surface_ice_volume_m3 - surface_ice_melted_mass_kg / RI, 0.0_JPRB)
    endif
    if (excess_ice_melted_mass_kg >= excess_ice_volume_m3 * RI) then
        excess_ice_volume_m3 = 0.0_JPRB
    else
        excess_ice_volume_m3 = max( &
        &   excess_ice_volume_m3 - excess_ice_melted_mass_kg / RI, 0.0_JPRB)
    endif
    total_melted_mass_kg = surface_ice_melted_mass_kg + excess_ice_melted_mass_kg
    liquid_water_volume_m3 = liquid_water_volume_m3 + total_melted_mass_kg / RW
    if (liquid_water_volume_m3 > 0.0_JPRB) then
        liquid_water_temperature_k = TMELT + liquid_sensible_energy_j / &
        &   (CW * RW * liquid_water_volume_m3)
    else
        liquid_water_temperature_k = TMELT
    endif
end subroutine mix_meltwater_into_liquid


pure elemental subroutine apply_liquid_energy( &
    &   liquid_water_volume_m3, liquid_water_temperature_k, added_energy_j, &
    &   freeze_energy_demand_j, unapplied_energy_j)
    real(kind=JPRB), intent(in) :: &
    &   liquid_water_volume_m3, & ! [m3] Liquid-water volume receiving the energy increment.
    &   added_energy_j            ! [J] Energy added to liquid water; positive warms water.
    real(kind=JPRB), intent(inout) :: &
    &   liquid_water_temperature_k, & ! [K] Liquid-water temperature before and after heating.
    &   unapplied_energy_j             ! [J] Accumulated energy that cannot be applied.
    real(kind=JPRB), intent(out) :: &
    &   freeze_energy_demand_j         ! [J] Positive cooling demand available for freezing.
    real(kind=JPRB) :: &
    &   liquid_sensible_energy_j       ! [J] Liquid sensible energy after applying the increment.

    freeze_energy_demand_j = 0.0_JPRB
    if (liquid_water_volume_m3 > 0.0_JPRB) then
        liquid_sensible_energy_j = liquid_water_energy_j( &
        &   liquid_water_volume_m3, liquid_water_temperature_k) + added_energy_j
        if (liquid_sensible_energy_j >= 0.0_JPRB) then
            liquid_water_temperature_k = TMELT + liquid_sensible_energy_j / &
            &   (CW * RW * liquid_water_volume_m3)
        else
            liquid_water_temperature_k = TMELT
            freeze_energy_demand_j = -liquid_sensible_energy_j
        endif
    else
        unapplied_energy_j = unapplied_energy_j + added_energy_j
    endif
end subroutine apply_liquid_energy


pure elemental subroutine freeze_liquid_water( &
    &   liquid_water_volume_m3, liquid_water_temperature_k, &
    &   surface_ice_volume_m3, freeze_energy_demand_j, &
    &   frozen_water_mass_kg, unapplied_energy_j)
    real(kind=JPRB), intent(inout) :: &
    &   liquid_water_volume_m3, &     ! [m3] Liquid-water volume before and after freezing.
    &   liquid_water_temperature_k, & ! [K] Liquid-water temperature before and after freezing.
    &   surface_ice_volume_m3, &      ! [m3] Water-surface ice volume before and after freezing.
    &   unapplied_energy_j             ! [J] Accumulated energy that cannot be applied.
    real(kind=JPRB), intent(in) :: &
    &   freeze_energy_demand_j         ! [J] Positive cooling demand to satisfy by freezing water.
    real(kind=JPRB), intent(out) :: &
    &   frozen_water_mass_kg           ! [kg] Liquid-water mass converted to water-surface ice.
    real(kind=JPRB) :: &
    &   available_liquid_mass_kg, &    ! [kg] Liquid-water mass available for freezing.
    &   freeze_energy_per_mass_j_kg, & ! [J kg-1] Energy removed when liquid becomes ice at TMELT.
    &   remaining_freeze_energy_j      ! [J] Cooling demand left after all available water freezes.

    frozen_water_mass_kg = 0.0_JPRB
    if (freeze_energy_demand_j <= 0.0_JPRB) return
    available_liquid_mass_kg = RW * liquid_water_volume_m3
    freeze_energy_per_mass_j_kg = HFUS + CW * &
    &   max(liquid_water_temperature_k - TMELT, 0.0_JPRB)
    if (freeze_energy_demand_j >= &
    &   available_liquid_mass_kg * freeze_energy_per_mass_j_kg) then
        frozen_water_mass_kg = available_liquid_mass_kg
        liquid_water_volume_m3 = 0.0_JPRB
    else
        frozen_water_mass_kg = freeze_energy_demand_j / freeze_energy_per_mass_j_kg
        liquid_water_volume_m3 = max( &
        &   liquid_water_volume_m3 - frozen_water_mass_kg / RW, 0.0_JPRB)
    endif
    surface_ice_volume_m3 = surface_ice_volume_m3 + frozen_water_mass_kg / RI
    if (liquid_water_volume_m3 <= 0.0_JPRB) liquid_water_temperature_k = TMELT
    remaining_freeze_energy_j = freeze_energy_demand_j - &
    &   frozen_water_mass_kg * freeze_energy_per_mass_j_kg
    if (remaining_freeze_energy_j > 0.0_JPRB) then
        unapplied_energy_j = unapplied_energy_j - remaining_freeze_energy_j
    endif
end subroutine freeze_liquid_water


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
