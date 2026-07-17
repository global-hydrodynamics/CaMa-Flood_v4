module thermo_mod
#ifdef heatlink
    use PARKIND1, only: &
    &   JPIM, JPRB
    use YOS_CMF_MAP, only: &
    &   NSEQALL
    use const_mod, only: &
    &   STO_IGNORE
    use phys_const_mod, only: &
    &   RW, CW, RIVDPH_MIN, TMELT, Kice2wat, &
    &   watSWref, watLWref, ew
    use heat_flux_mod, only: &
    &   calc_LWd, calc_LWu, calc_bulk, &
    &   calc_SWd, calc_SWd_penetration_river, calc_SWd_penetration_flood, &
    &   calc_friction
    use heat_budget_mod, only: &
    &   update_liquid_temperature_no_phase_change, &
    &   update_local_water_ice_state
    use output_mod, only: &
    &   update_output
    use topo_mod, only: &
    &   D2RIVSLP
    implicit none
    private

    public :: &
    &   init_thermo_mod, fin_thermo_mod, &
    &   calc_surface_heat_flux, calc_body_heat_flux, &
    &   solve_heat_budget, solve_water_ice_heat_budget

    ! Surface-related flux components (per unit area, W/m2)
    real(kind=JPRB), allocatable, save :: &
    &   hflx_lwd(:), & ! absorbed downward LW at surface
    &   hflx_lwu(:), & ! upward LW emission from surface
    &   hflx_shf(:), & ! sensible heat flux (sign per calc_bulk)
    &   hflx_lhf(:)    ! latent heat flux (sign per calc_bulk)

    ! Body-related components (per unit area, W/m2)
    real(kind=JPRB), allocatable, save :: &
    &   hflx_swa(:), &   ! SW absorbed in water body (river+flood weighted)
    &   hflx_frc(:)      ! frictional heating (river+flood weighted)

contains

subroutine init_thermo_mod()
    allocate(hflx_lwd(NSEQALL), source=0.0_JPRB)
    allocate(hflx_lwu(NSEQALL), source=0.0_JPRB)
    allocate(hflx_shf(NSEQALL),  source=0.0_JPRB)
    allocate(hflx_lhf(NSEQALL),  source=0.0_JPRB)

    allocate(hflx_swa(NSEQALL), source=0.0_JPRB)
    allocate(hflx_frc(NSEQALL), source=0.0_JPRB)
end subroutine init_thermo_mod


subroutine fin_thermo_mod()
    if (allocated(hflx_lwd))  deallocate(hflx_lwd)
    if (allocated(hflx_lwu))  deallocate(hflx_lwu)
    if (allocated(hflx_shf))   deallocate(hflx_shf)
    if (allocated(hflx_lhf))   deallocate(hflx_lhf)

    if (allocated(hflx_swa)) deallocate(hflx_swa)
    if (allocated(hflx_frc)) deallocate(hflx_frc)
end subroutine fin_thermo_mod

! ==============================================================================================
! Surface fluxes
!   hflx_srf = hflx_lwd - hflx_lwu - hflx_shf - hflx_lhf (positive into water)
! ==============================================================================================
subroutine calc_surface_heat_flux( &
    &   wattmp, watvol, &
    &   LWdn_in, Tair_in, Psrf_in, Qair_in, Wind_in, &
    &   hflx_srf)
    real(kind=JPRB), intent(in) :: &
    &   wattmp(:), & ! [K] water temperature
    &   watvol(:)    ! [m3] water volume
    real(kind=JPRB), intent(in) :: & ! atmospheric forcing
    &   LWdn_in(:), & ! [W m-2] downward longwave radiation
    &   Tair_in(:), & ! [K] air temperature
    &   Psrf_in(:), & ! [hPa] surface pressure
    &   Qair_in(:), & ! [kg kg-1] specific humidity
    &   Wind_in(:)    ! [m s-1] wind speed
    real(kind=JPRB), intent(out) :: &
    &   hflx_srf(:) ! [W m-2] net surface heat flux (positive into water)
    integer(kind=JPIM) :: &
    &   iseq

    do iseq = 1, NSEQALL
        hflx_lwd(iseq) = 0.0_JPRB
        hflx_lwu(iseq) = 0.0_JPRB
        hflx_shf(iseq) = 0.0_JPRB
        hflx_lhf(iseq) = 0.0_JPRB
        hflx_srf(iseq) = 0.0_JPRB
        if (watvol(iseq) <= STO_IGNORE) cycle

        call calc_LWd(hflx_lwd(iseq), LWdn_in(iseq), watLWref)
        call calc_LWu(hflx_lwu(iseq), wattmp(iseq), ew)
        call calc_bulk(hflx_shf(iseq), hflx_lhf(iseq), Psrf_in(iseq), Qair_in(iseq), Tair_in(iseq), Wind_in(iseq), wattmp(iseq))

        hflx_srf(iseq) = hflx_lwd(iseq) - hflx_lwu(iseq) - hflx_shf(iseq) - hflx_lhf(iseq)
    end do

    call update_output('RAW_HFLX_LWD', hflx_lwd)
    call update_output('RAW_HFLX_LWU', hflx_lwu)
    call update_output('RAW_HFLX_SHF', hflx_shf)
    call update_output('RAW_HFLX_LHF', hflx_lhf)
    call update_output('RAW_HFLX_SRF', hflx_srf)
end subroutine calc_surface_heat_flux


! ==============================================================================================
! Body fluxes: SW absorption and friction heating, river+flood width-weighted
!   hflx_bdy = hflx_swa + hflx_frc
! ==============================================================================================
subroutine calc_body_heat_flux( &
        &   watvol, &
        &   SWdn_in, &
        &   rivdph, rivare, rivvel, &
        &   flddph, fldare, fldvel, &
        &   hflx_bdy)

    real(kind=JPRB), intent(in) :: &
    &   watvol(:) ! [m3] water volume
    real(kind=JPRB), intent(in) :: & ! atmospheric forcing
    &   SWdn_in(:) ! [W m-2] downward shortwave radiation
    real(kind=JPRB), intent(in) :: &
    &   rivdph(:), & ! [m] river depth
    &   rivare(:), & ! [m2] river area
    &   rivvel(:), & ! [m s-1] river velocity
    &   flddph(:), & ! [m] flood depth
    &   fldare(:), & ! [m2] flood area
    &   fldvel(:)    ! [m s-1] flood velocity
    real(kind=JPRB), intent(out) :: &
    &   hflx_bdy(:) ! [W m-2] net body heat flux (positive into water)

    integer(kind=JPIM) :: &
    &   iseq
    real(kind=JPRB) :: &
    &   sw_in, sw_pen, &
    &   abs_riv, abs_fld, &
    &   hfrc_riv, hfrc_fld, &
    &   wsum

    do iseq = 1, NSEQALL
        hflx_swa(iseq) = 0.0_JPRB
        hflx_frc(iseq) = 0.0_JPRB
        hflx_bdy(iseq) = 0.0_JPRB
        if (watvol(iseq) <= STO_IGNORE) cycle

        call calc_SWd(sw_in, SWdn_in(iseq), watSWref)
        call calc_SWd_penetration_river(sw_pen, SWdn_in(iseq), rivdph(iseq))
        abs_riv = sw_in - sw_pen

        !hfrc_riv = 0.0_JPRB
        !if (rivdph(iseq) > RIVDPH_MIN) then
        call calc_friction(hfrc_riv, rivdph(iseq), rivvel(iseq), D2RIVSLP(iseq))
        !end if
        hflx_swa(iseq) = abs_riv
        hflx_frc(iseq) = hfrc_riv

        if (fldare(iseq) > 0.0_JPRB) then
            call calc_SWd(sw_in, SWdn_in(iseq), watSWref)
            call calc_SWd_penetration_flood(sw_pen, SWdn_in(iseq), flddph(iseq))
            abs_fld = sw_in - sw_pen

            !hfrc_fld = 0.0_JPRB
            !if (flddph(iseq) > RIVDPH_MIN) then
            call calc_friction(hfrc_fld, 0.5_JPRB * flddph(iseq), fldvel(iseq), D2RIVSLP(iseq))
            !end if

            wsum = rivare(iseq) + fldare(iseq)
            hflx_swa(iseq) = (hflx_swa(iseq)  * rivare(iseq) + abs_fld  * fldare(iseq)) / wsum
            hflx_frc(iseq) = (hfrc_riv * rivare(iseq) + hfrc_fld * fldare(iseq)) / wsum
        endif

        hflx_bdy(iseq) = hflx_swa(iseq) + hflx_frc(iseq)
    end do

    call update_output('RAW_HFLX_SWA', hflx_swa)
    call update_output('RAW_HFLX_FRC', hflx_frc)
    call update_output('RAW_HFLX_BDY', hflx_bdy)
end subroutine calc_body_heat_flux

! ==============================================================================================
! Solve (liquid only): update wattmp by energy increment dE
!   dT = dE / (RW * CW * V)
! ==============================================================================================
subroutine solve_heat_budget(wattmp, watvol, hflx_srf, hflx_bdy, srfare, dt)
    real(kind=JPRB), intent(inout) :: &
    &   wattmp(:) ! [K] water temperature
    real(kind=JPRB), intent(in)    :: &
    &   watvol(:), &   ! [m3] water volume
    &   hflx_srf(:), & ! [W m-2] net surface heat flux (positive into water)
    &   hflx_bdy(:), & ! [W m-2] net body heat flux (positive into water)
    &   srfare(:), &   ! [m2] surface area (river+flood)
    &   dt             ! [s] time step
    real(kind=JPRB) :: &
    &   dE, q_net
    integer(kind=JPIM) :: &
    &   iseq

    do iseq = 1, NSEQALL
        q_net = hflx_srf(iseq) + hflx_bdy(iseq)
        dE    = q_net * srfare(iseq) * dt
        call update_liquid_temperature_no_phase_change( &
        &   wattmp(iseq), watvol(iseq), dE)
    end do
end subroutine solve_heat_budget


! ==============================================================================================
! Solve separate liquid-water, water-surface ice, and immobile excess-ice budgets.
! Ice is represented at TMELT; there is no prognostic ice sensible-temperature state.
! ==============================================================================================
subroutine solve_water_ice_heat_budget( &
    &   water_temperature_k, liquid_water_volume_m3, &
    &   surface_ice_volume_m3, excess_ice_volume_m3, &
    &   water_surface_area_m2, surface_ice_area_m2, excess_ice_area_m2, &
    &   open_water_surface_heat_flux_w_m2, water_body_heat_flux_w_m2, &
    &   surface_ice_atmospheric_heat_flux_w_m2, &
    &   excess_ice_atmospheric_heat_flux_w_m2, timestep_s, &
    &   unapplied_energy_j, mass_budget_error_kg, energy_budget_error_j)
    real(kind=JPRB), intent(inout) :: &
    &   water_temperature_k(:), &   ! [K] Liquid-water temperature before and after the local update.
    &   liquid_water_volume_m3(:), & ! [m3] Liquid-water volume before and after the local update.
    &   surface_ice_volume_m3(:), &  ! [m3] Water-surface ice volume before and after the local update.
    &   excess_ice_volume_m3(:)      ! [m3] Immobile excess-ice volume before and after the local update.
    real(kind=JPRB), intent(in) :: &
    &   water_surface_area_m2(:), &  ! [m2] Combined river and inundated water-surface area.
    &   surface_ice_area_m2(:), &    ! [m2] Water-surface area covered by ice.
    &   excess_ice_area_m2(:), &     ! [m2] Effective atmospheric-exchange area of immobile excess ice.
    &   open_water_surface_heat_flux_w_m2(:), & ! [W m-2] Atmospheric flux into uncovered liquid water.
    &   water_body_heat_flux_w_m2(:), & ! [W m-2] Shortwave and frictional heat flux into the water body.
    &   surface_ice_atmospheric_heat_flux_w_m2(:), & ! [W m-2] Atmospheric flux into water-surface ice.
    &   excess_ice_atmospheric_heat_flux_w_m2(:), & ! [W m-2] Atmospheric flux into immobile excess ice.
    &   timestep_s                     ! [s] Coupling time-step duration.
    real(kind=JPRB), intent(out) :: &
    &   unapplied_energy_j(:), &       ! [J] Energy not applied by the local phase-change kernel.
    &   mass_budget_error_kg(:), &     ! [kg] Final minus initial local water-plus-ice mass.
    &   energy_budget_error_j(:)       ! [J] Local energy-conservation error after accounting for unapplied energy.
    real(kind=JPRB) :: &
    &   open_water_area_m2, &          ! [m2] Water-surface area not covered by ice.
    &   water_to_ice_heat_flux_w_m2, & ! [W m-2] Conductive heat flux from liquid water into surface ice.
    &   water_added_energy_j, &        ! [J] Net energy increment applied directly to liquid water.
    &   surface_ice_added_energy_j, &  ! [J] Net energy increment applied to water-surface ice.
    &   excess_ice_added_energy_j, &   ! [J] Net atmospheric energy increment presented to excess ice.
    &   frozen_water_mass_kg, &        ! [kg] Liquid-water mass frozen during this update.
    &   surface_ice_melted_mass_kg, &  ! [kg] Water-surface ice mass melted during this update.
    &   excess_ice_melted_mass_kg      ! [kg] Immobile excess-ice mass melted during this update.
    integer(kind=JPIM) :: &
    &   iseq                            ! [-] Vector index of the river cell.

    do iseq = 1, NSEQALL
        open_water_area_m2 = max( &
        &   water_surface_area_m2(iseq) - surface_ice_area_m2(iseq), 0.0_JPRB)
        water_to_ice_heat_flux_w_m2 = Kice2wat * &
        &   max(water_temperature_k(iseq) - TMELT, 0.0_JPRB)

        water_added_energy_j = ( &
        &   open_water_surface_heat_flux_w_m2(iseq) * open_water_area_m2 + &
        &   water_body_heat_flux_w_m2(iseq) * water_surface_area_m2(iseq) - &
        &   water_to_ice_heat_flux_w_m2 * surface_ice_area_m2(iseq)) * timestep_s
        surface_ice_added_energy_j = ( &
        &   surface_ice_atmospheric_heat_flux_w_m2(iseq) + &
        &   water_to_ice_heat_flux_w_m2) * surface_ice_area_m2(iseq) * timestep_s
        excess_ice_added_energy_j = &
        &   excess_ice_atmospheric_heat_flux_w_m2(iseq) * &
        &   excess_ice_area_m2(iseq) * timestep_s

        call update_local_water_ice_state( &
        &   liquid_water_volume_m3(iseq), water_temperature_k(iseq), &
        &   surface_ice_volume_m3(iseq), excess_ice_volume_m3(iseq), &
        &   water_added_energy_j, surface_ice_added_energy_j, &
        &   excess_ice_added_energy_j, &
        &   frozen_water_mass_kg, surface_ice_melted_mass_kg, &
        &   excess_ice_melted_mass_kg, unapplied_energy_j(iseq), &
        &   mass_budget_error_kg(iseq), energy_budget_error_j(iseq))
    enddo
end subroutine solve_water_ice_heat_budget
#endif
end module thermo_mod
