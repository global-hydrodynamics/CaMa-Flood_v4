module heatlink_river_mod
#ifdef heatlink
    use PARKIND1, only: &
    &   JPIM, JPRB, JPRD
    use YOS_CMF_INPUT, only: &
    &   LOGNAM, LRESTART, LUPSINF, LPTHOUT
    use heatlink_config_mod, only: &
    &   LICE, NNEWTON_MAX_ICE
    use YOS_CMF_MAP, only: &
    &   NSEQMAX, NSEQALL, &
    &   D2GRAREA, D2RIVLEN, D2RIVWTH
    use YOS_CMF_DIAG, only: &
    &   D2STORGE, D2OUTFLW, D1PTHFLWSUM, &
    &   D2RIVDPH, D2RIVVEL, D2FLDDPH, D2FLDVEL, D2FLDARE
    use YOS_CMF_PROG, only: &
    &   P2RIVSTO, P2FLDSTO, D2RUNOFF, D2GDWRTN, D2UPSINF
    use datetime_mod, only: &
    &   DateTime

    use const_mod, only: &
    &   STO_IGNORE
    use phys_const_mod, only: &
    &   TMELT, RIVDPH_MIN, KI, RW, CW, RI, HFUS
    use ice_cover_mod, only: &
    &   ICE_THICKNESS_MIN_M, diagnose_ice_geometry, enforce_surface_ice_capacity
    use heat_flux_mod, only: &
    &   ICE_SURFACE_NEWTON_RESIDUAL_TOLERANCE_W_M2, calc_ice_surface_heat_flux
    use heat_budget_mod, only: &
    &   water_ice_mass_kg, water_ice_energy_j
    use water_storage_adapter_mod, only: &
    &   apply_liquid_volume_delta_to_storage
    use river_water_advection_mod, only: &
    &   advect_river_water_sensible_heat
    use river_ice_advection_mod, only: &
    &   advect_river_surface_ice
    use input_mod, only: &
    &   add_input, get_input
    use output_mod, only: &
    &   update_output
    use restart_mod, only: &
    &   read_restart, write_restart
    use thermo_mod, only: &
    &   calc_surface_heat_flux, calc_body_heat_flux, &
    &   solve_heat_budget, solve_water_ice_heat_budget
    implicit none
    private
    public :: &
    &   init_heatlink_river_mod, prepare_heatlink_input, &
    &   capture_river_water_advection_state, advance_river_water_advection, &
    &   finalize_river_ice_advection_state, &
    &   calc_heatlink, &
    &   write_heatlink_restart, fin_heatlink_river_mod

    real(kind=JPRB), allocatable, save :: &
    &   wattmp(:) ! [K] river water temperature

    real(kind=JPRD), allocatable, save :: &
    &   advection_initial_liquid_volume_m3(:), & ! [m3] Liquid storage before the current hydraulic update.
    &   advection_heat_budget_error_j(:), & ! [J] Cell sensible-heat reconstruction error.
    &   advection_water_budget_error_m3(:), & ! [m3] Cell water-balance difference for supplied flows.
    &   advection_unapplied_sensible_heat_j(:), & ! [J] Heat not representable in zero liquid volume.
    &   advection_ice_budget_error_m3(:), & ! [m3] Expected minus represented mobile surface ice.
    &   advection_combined_energy_budget_error_j(:) ! [J] Water sensible plus ice latent-energy error.
    real(kind=JPRB), allocatable, save :: &
    &   advection_runoff_flow_m3s(:), & ! [m3 s-1] Runoff plus groundwater return flow.
    &   advection_upstream_flow_m3s(:) ! [m3 s-1] External upstream inflow or zero when disabled.
    real(kind=JPRD), save :: &
    &   advection_domain_heat_budget_error_j = 0.0_JPRD, & ! [J] Current internal-step domain closure error.
    &   advection_domain_ice_budget_error_m3 = 0.0_JPRD, & ! [m3] Current boundary-aware ice closure error.
    &   advection_domain_combined_energy_budget_error_j = 0.0_JPRD, & ! [J] Current water-plus-ice closure error.
    &   maximum_advection_heat_budget_error_j = 0.0_JPRD, & ! [J] Maximum cell error since the last local heat update.
    &   maximum_advection_water_budget_error_m3 = 0.0_JPRD, & ! [m3] Maximum cell water error since last update.
    &   maximum_advection_unapplied_heat_j = 0.0_JPRD, & ! [J] Maximum cell unapplied heat since last update.
    &   maximum_advection_domain_heat_budget_error_j = 0.0_JPRD, & ! [J] Maximum domain closure error since last update.
    &   maximum_advection_relative_domain_heat_budget_error = 0.0_JPRD, & ! [-] Maximum normalized heat error.
    &   maximum_advection_ice_mass_budget_error_kg = 0.0_JPRD, & ! [kg] Maximum cell ice-mass closure error.
    &   maximum_advection_combined_energy_budget_error_j = 0.0_JPRD, & ! [J] Maximum cell water-plus-ice error.
    &   maximum_advection_domain_ice_mass_budget_error_kg = 0.0_JPRD, & ! [kg] Maximum domain ice-mass error.
    &   maximum_advection_domain_combined_energy_budget_error_j = 0.0_JPRD, & ! [J] Maximum domain energy error.
    &   maximum_advection_relative_domain_combined_energy_budget_error = 0.0_JPRD ! [-] Normalized domain energy error.

    ! River-ice state and diagnostics. Excess ice remains in the source cell,
    ! is reserved for local melting, and is excluded from future river transport.
    real(kind=JPRB), allocatable, save :: &
    &   icevol(:), &       ! [m3] Ice retained on the water surface and eligible for river transport.
    &   icevol_excess(:), & ! [m3] Immobile excess ice retained in the river grid cell.
    &   icearea(:), &      ! [m2] Horizontal area covered by river ice.
    &   icethickness(:), & ! [m] Mean thickness over the ice-covered area.
    &   icefraction(:), &  ! [-] Fraction of the water surface covered by ice.
    &   icearea_excess(:), & ! [m2] Effective atmospheric-exchange area of immobile excess ice.
    &   icethickness_excess(:), & ! [m] Mean thickness over the effective excess-ice area.
    &   ice_surface_temperature(:), & ! [K] Temperature of the massless upper skin of water-surface ice.
    &   ice_upward_conductive_heat_flux(:), & ! [W m-2] Melting-point bulk-to-skin effective heat flux.
    &   ice_excess_surface_temperature(:) ! [K] Surface temperature of insulated immobile excess ice.

    real(kind=JPRB), parameter :: &
    &   RIVER_ICE_THICKNESS_MAX_M = 20.0_JPRB ! [m] Maximum ice thickness retained on the water surface.

    ! atmospheric forcing
    real(kind=JPRB), allocatable, save :: &
    &   lwdn(:), & ! [W m-2] downward longwave radiation
    &   psrf(:), & ! [hPa] surface pressure
    &   qair(:), & ! [kg kg-1] specific humidity
    &   swdn(:), & ! [W m-2] downward shortwave radiation
    &   tair(:), & ! [K] air temperature
    &   trof(:), & ! [K] Liquid-water temperature of runoff and external upstream inflow.
    &   wind(:)  ! [m s-1] wind speed

    real(kind=JPRB), allocatable, save :: &
    &   hflx_srf(:), & ! [W m-2] surface heat flux (+: into water)
    &   hflx_bdy(:), & ! [W m-2] body heat flux (+: into water)
    &   hflx_ice_srf(:), & ! [W m-2] atmospheric heat flux into water-surface ice.
    &   hflx_ice_excess_srf(:), & ! [W m-2] atmospheric heat flux into immobile excess ice.
    &   swdn_to_water(:) ! [W m-2] Area-weighted shortwave radiation reaching the water surface.

    real(kind=JPRB), allocatable, save :: &
    &   phase_unapplied_energy(:), & ! [J] Energy not applied by the local phase-change kernel.
    &   phase_mass_budget_error(:), & ! [kg] Local water-plus-ice mass-conservation error.
    &   phase_energy_budget_error(:) ! [J] Local water-plus-ice energy-conservation error.

    real(kind=JPRB), allocatable, save :: &
    &   watsto(:), & ! [m3] water storage (volume) in river + floodplain
    &   rivdph(:), & ! [m] river depth
    &   rivare(:), & ! [m2] river area
    &   rivvel(:), & ! [m s-1] river velocity
    &   flddph(:), & ! [m] flood depth
    &   fldare(:), & ! [m2] flood area
    &   fldvel(:)    ! [m s-1] flood velocity

contains

subroutine init_heatlink_river_mod(dt)
    use topo_mod, only: &
    &   init_topo_mod
    use thermo_mod, only: &
    &   init_thermo_mod
    type(DateTime), intent(in) :: dt
    logical :: is_found

    write(LOGNAM, '(a)') '[heatlink_river_mod/init_heatlink_river_mod]'
    if (LICE) then
        write(LOGNAM, '(a,i0)') &
        &   '  maximum river-ice surface Newton iterations = ', NNEWTON_MAX_ICE
        write(LOGNAM, '(a,es12.4)') &
        &   '  river-ice surface Newton residual tolerance [W m-2] = ', &
        &   ICE_SURFACE_NEWTON_RESIDUAL_TOLERANCE_W_M2
    endif

    write(LOGNAM, '(a)') '  read the first-step input'
    call add_input('LWDN', dt) ! [W m-2]
    call add_input('PSRF', dt) ! [hPa]
    call add_input('QAIR', dt) ! [kg kg-1]
    call add_input('SWDN', dt) ! [W m-2]
    call add_input('TAIR', dt) ! [K]
    call add_input('TROF', dt) ! [K]
    call add_input('WIND', dt) ! [m s-1]
    call init_topo_mod()
    call init_thermo_mod()

    allocate(wattmp(NSEQMAX), source=0.0_JPRB)
    allocate(advection_initial_liquid_volume_m3(NSEQMAX), source=0.0_JPRD)
    allocate(advection_heat_budget_error_j(NSEQMAX), source=0.0_JPRD)
    allocate(advection_water_budget_error_m3(NSEQMAX), source=0.0_JPRD)
    allocate(advection_unapplied_sensible_heat_j(NSEQMAX), source=0.0_JPRD)
    allocate(advection_runoff_flow_m3s(NSEQMAX), source=0.0_JPRB)
    allocate(advection_upstream_flow_m3s(NSEQMAX), source=0.0_JPRB)
    allocate(hflx_srf(NSEQMAX), source=0.0_JPRB)
    allocate(hflx_bdy(NSEQMAX), source=0.0_JPRB)
    if (LICE) then
        allocate(advection_ice_budget_error_m3(NSEQMAX), source=0.0_JPRD)
        allocate(advection_combined_energy_budget_error_j(NSEQMAX), source=0.0_JPRD)
        allocate(icevol(NSEQMAX), source=0.0_JPRB)
        allocate(icevol_excess(NSEQMAX), source=0.0_JPRB)
        allocate(icearea(NSEQMAX), source=0.0_JPRB)
        allocate(icethickness(NSEQMAX), source=0.0_JPRB)
        allocate(icefraction(NSEQMAX), source=0.0_JPRB)
        allocate(icearea_excess(NSEQMAX), source=0.0_JPRB)
        allocate(icethickness_excess(NSEQMAX), source=0.0_JPRB)
        allocate(ice_surface_temperature(NSEQMAX), source=TMELT)
        allocate(ice_upward_conductive_heat_flux(NSEQMAX), source=0.0_JPRB)
        allocate(ice_excess_surface_temperature(NSEQMAX), source=TMELT)
        allocate(hflx_ice_srf(NSEQMAX), source=0.0_JPRB)
        allocate(hflx_ice_excess_srf(NSEQMAX), source=0.0_JPRB)
        allocate(swdn_to_water(NSEQMAX), source=0.0_JPRB)
        allocate(phase_unapplied_energy(NSEQMAX), source=0.0_JPRB)
        allocate(phase_mass_budget_error(NSEQMAX), source=0.0_JPRB)
        allocate(phase_energy_budget_error(NSEQMAX), source=0.0_JPRB)
    endif

    allocate(lwdn(NSEQMAX), source=0.0_JPRB)
    allocate(psrf(NSEQMAX), source=0.0_JPRB)
    allocate(qair(NSEQMAX), source=0.0_JPRB)
    allocate(swdn(NSEQMAX), source=0.0_JPRB)
    allocate(tair(NSEQMAX), source=0.0_JPRB)
    allocate(trof(NSEQMAX), source=0.0_JPRB)
    allocate(wind(NSEQMAX), source=0.0_JPRB)

    allocate(watsto(NSEQMAX), source=0.0_JPRB)
    allocate(rivdph(NSEQMAX), source=0.0_JPRB)
    allocate(rivare(NSEQMAX), source=0.0_JPRB)
    allocate(rivvel(NSEQMAX), source=0.0_JPRB)
    allocate(flddph(NSEQMAX), source=0.0_JPRB)
    allocate(fldare(NSEQMAX), source=0.0_JPRB)
    allocate(fldvel(NSEQMAX), source=0.0_JPRB)

    if (LRESTART) then
        call read_restart('RIVWAT_TMP', dt, is_found, wattmp)
        if (.not. is_found) stop 'RIVWAT_TMP restart was not found.'
        if (LICE) then
            call read_restart('RIVICE_VOL', dt, is_found, icevol)
            if (.not. is_found) stop 'RIVICE_VOL restart was not found.'
            call read_restart('RIVICE_VOL_EXCESS', dt, is_found, icevol_excess)
            if (.not. is_found) stop 'RIVICE_VOL_EXCESS restart was not found.'
        endif
    else
        write(LOGNAM, '(a)') '  initialize river water temperature -> air temperature'
        call get_input('TAIR', tair)
        wattmp(:) = max(tair(:), TMELT)
        if (LICE) then
            icevol(:) = 0.0_JPRB
            icevol_excess(:) = 0.0_JPRB
        endif
    endif
    ! The first water/ice geometry diagnosis is performed immediately before
    ! the first hydraulic advection step, after CaMa has diagnosed its stage.
    write(LOGNAM, *)
end subroutine init_heatlink_river_mod


subroutine prepare_heatlink_input()
    call get_input('TROF', trof)
    call enforce_liquid_inflow_temperature(trof)
end subroutine prepare_heatlink_input


subroutine enforce_liquid_inflow_temperature(temperature_k)
    real(kind=JPRB), intent(inout) :: &
    &   temperature_k(:) ! [K] Liquid-water inflow temperature; returned no colder than TMELT.

    temperature_k(:) = max(temperature_k(:), TMELT)
end subroutine enforce_liquid_inflow_temperature


subroutine capture_river_water_advection_state()
    advection_initial_liquid_volume_m3(:) = P2RIVSTO(:,1) + P2FLDSTO(:,1)
    if (LICE) then
        ! Refresh the mobile water-surface pool using the hydraulic geometry
        ! that supplies the following internal-step discharge calculation.
        call get_water()
        call enforce_river_ice_capacity()
        call diagnose_river_ice_geometry()
    endif
end subroutine capture_river_water_advection_state


subroutine advance_river_water_advection(dt_seconds)
    real(kind=JPRB), intent(in) :: &
    &   dt_seconds ! [s] Current adaptive hydraulic time step.
    real(kind=JPRD) :: &
    &   domain_sensible_heat_scale_j, & ! [J] Sum of absolute represented cell sensible heat.
    &   domain_combined_energy_scale_j, & ! [J] Absolute water-sensible plus ice-latent energy scale.
    &   volumetric_ice_latent_energy_j_m3 ! [J m-3] Magnitude of melting-point ice latent energy.

    volumetric_ice_latent_energy_j_m3 = real(RI, kind=JPRD) * real(HFUS, kind=JPRD)

    advection_runoff_flow_m3s(:) = D2RUNOFF(:,1) + D2GDWRTN(:,1)
    advection_upstream_flow_m3s(:) = 0.0_JPRB
    if (LUPSINF) advection_upstream_flow_m3s(:) = D2UPSINF(:,1)
    if (LPTHOUT) then
        call advect_river_water_sensible_heat( &
        &   water_temperature_k=wattmp(:NSEQALL), &
        &   liquid_volume_before_m3=advection_initial_liquid_volume_m3(:NSEQALL), &
        &   liquid_volume_after_m3=P2RIVSTO(:NSEQALL,1) + P2FLDSTO(:NSEQALL,1), &
        &   normal_flow_m3s=D2OUTFLW(:NSEQALL,1), &
        &   dt_seconds=dt_seconds, &
        &   bifurcation_flow_m3s=D1PTHFLWSUM, &
        &   runoff_flow_m3s=advection_runoff_flow_m3s(:NSEQALL), &
        &   upstream_inflow_m3s=advection_upstream_flow_m3s(:NSEQALL), &
        &   inflow_temperature_k=trof(:NSEQALL), &
        &   heat_budget_error_j=advection_heat_budget_error_j(:NSEQALL), &
        &   water_budget_error_m3=advection_water_budget_error_m3(:NSEQALL), &
        &   unapplied_sensible_heat_j=advection_unapplied_sensible_heat_j(:NSEQALL), &
        &   domain_heat_budget_error_j=advection_domain_heat_budget_error_j)
    else
        call advect_river_water_sensible_heat( &
        &   water_temperature_k=wattmp(:NSEQALL), &
        &   liquid_volume_before_m3=advection_initial_liquid_volume_m3(:NSEQALL), &
        &   liquid_volume_after_m3=P2RIVSTO(:NSEQALL,1) + P2FLDSTO(:NSEQALL,1), &
        &   normal_flow_m3s=D2OUTFLW(:NSEQALL,1), &
        &   dt_seconds=dt_seconds, &
        &   runoff_flow_m3s=advection_runoff_flow_m3s(:NSEQALL), &
        &   upstream_inflow_m3s=advection_upstream_flow_m3s(:NSEQALL), &
        &   inflow_temperature_k=trof(:NSEQALL), &
        &   heat_budget_error_j=advection_heat_budget_error_j(:NSEQALL), &
        &   water_budget_error_m3=advection_water_budget_error_m3(:NSEQALL), &
        &   unapplied_sensible_heat_j=advection_unapplied_sensible_heat_j(:NSEQALL), &
        &   domain_heat_budget_error_j=advection_domain_heat_budget_error_j)
    endif

    if (LICE) then
        if (LPTHOUT) then
            call advect_river_surface_ice( &
            &   surface_ice_volume_m3=icevol(:NSEQALL), &
            &   surface_ice_fraction=icefraction(:NSEQALL), &
            &   liquid_volume_before_m3=advection_initial_liquid_volume_m3(:NSEQALL), &
            &   normal_flow_m3s=D2OUTFLW(:NSEQALL,1), &
            &   dt_seconds=dt_seconds, &
            &   bifurcation_flow_m3s=D1PTHFLWSUM, &
            &   ice_budget_error_m3=advection_ice_budget_error_m3(:NSEQALL), &
            &   domain_ice_budget_error_m3=advection_domain_ice_budget_error_m3)
        else
            call advect_river_surface_ice( &
            &   surface_ice_volume_m3=icevol(:NSEQALL), &
            &   surface_ice_fraction=icefraction(:NSEQALL), &
            &   liquid_volume_before_m3=advection_initial_liquid_volume_m3(:NSEQALL), &
            &   normal_flow_m3s=D2OUTFLW(:NSEQALL,1), &
            &   dt_seconds=dt_seconds, &
            &   ice_budget_error_m3=advection_ice_budget_error_m3(:NSEQALL), &
            &   domain_ice_budget_error_m3=advection_domain_ice_budget_error_m3)
        endif
        advection_combined_energy_budget_error_j(:NSEQALL) = &
        &   advection_heat_budget_error_j(:NSEQALL) - &
        &   volumetric_ice_latent_energy_j_m3 * &
        &   advection_ice_budget_error_m3(:NSEQALL)
        advection_domain_combined_energy_budget_error_j = &
        &   advection_domain_heat_budget_error_j - &
        &   volumetric_ice_latent_energy_j_m3 * &
        &   advection_domain_ice_budget_error_m3
        maximum_advection_ice_mass_budget_error_kg = max( &
        &   maximum_advection_ice_mass_budget_error_kg, &
        &   real(RI, kind=JPRD) * &
        &   maxval(abs(advection_ice_budget_error_m3(:NSEQALL))))
        maximum_advection_combined_energy_budget_error_j = max( &
        &   maximum_advection_combined_energy_budget_error_j, &
        &   maxval(abs(advection_combined_energy_budget_error_j(:NSEQALL))))
        maximum_advection_domain_ice_mass_budget_error_kg = max( &
        &   maximum_advection_domain_ice_mass_budget_error_kg, &
        &   real(RI, kind=JPRD) * abs(advection_domain_ice_budget_error_m3))
        maximum_advection_domain_combined_energy_budget_error_j = max( &
        &   maximum_advection_domain_combined_energy_budget_error_j, &
        &   abs(advection_domain_combined_energy_budget_error_j))
    endif

    maximum_advection_heat_budget_error_j = max( &
    &   maximum_advection_heat_budget_error_j, &
    &   maxval(abs(advection_heat_budget_error_j(:NSEQALL))))
    maximum_advection_water_budget_error_m3 = max( &
    &   maximum_advection_water_budget_error_m3, &
    &   maxval(abs(advection_water_budget_error_m3(:NSEQALL))))
    maximum_advection_unapplied_heat_j = max( &
    &   maximum_advection_unapplied_heat_j, &
    &   maxval(abs(advection_unapplied_sensible_heat_j(:NSEQALL))))
    maximum_advection_domain_heat_budget_error_j = max( &
    &   maximum_advection_domain_heat_budget_error_j, &
    &   abs(advection_domain_heat_budget_error_j))
    domain_sensible_heat_scale_j = real(RW, kind=JPRD) * real(CW, kind=JPRD) * sum( &
    &   (P2RIVSTO(:NSEQALL,1) + P2FLDSTO(:NSEQALL,1)) * &
    &   abs(real(wattmp(:NSEQALL) - TMELT, kind=JPRD)))
    maximum_advection_relative_domain_heat_budget_error = max( &
    &   maximum_advection_relative_domain_heat_budget_error, &
    &   abs(advection_domain_heat_budget_error_j) / &
    &   max(domain_sensible_heat_scale_j, 1.0_JPRD))
    if (LICE) then
        domain_combined_energy_scale_j = domain_sensible_heat_scale_j + &
        &   volumetric_ice_latent_energy_j_m3 * sum(real( &
        &   icevol(:NSEQALL) + icevol_excess(:NSEQALL), kind=JPRD))
        maximum_advection_relative_domain_combined_energy_budget_error = max( &
        &   maximum_advection_relative_domain_combined_energy_budget_error, &
        &   abs(advection_domain_combined_energy_budget_error_j) / &
        &   max(domain_combined_energy_scale_j, 1.0_JPRD))
    endif
end subroutine advance_river_water_advection


subroutine finalize_river_ice_advection_state()
    if (.not. LICE) return

    ! CMF_PHYSICS_FLDSTG has already diagnosed the post-update hydraulic
    ! geometry. Synchronize that state before repartitioning newly received
    ! mobile ice; pre-existing icevol_excess remains immobile.
    call get_water()
    call enforce_river_ice_capacity()
    call diagnose_river_ice_geometry()
end subroutine finalize_river_ice_advection_state


subroutine calc_heatlink(dt)
    real(kind=JPRB), intent(in) :: dt ! time step (seconds)

    write(LOGNAM, '(a)') '[heatlink_river_mod/calc_heatlink]'
    call get_input('LWDN', lwdn)
    call get_input('PSRF', psrf)
    call get_input('QAIR', qair)
    call get_input('SWDN', swdn)
    call get_input('TAIR', tair)
    call get_input('WIND', wind)

    call update_output('LWDN', lwdn)
    call update_output('PSRF', psrf)
    call update_output('QAIR', qair)
    call update_output('SWDN', swdn)
    call update_output('TAIR', tair)
    call update_output('TROF', trof)
    call update_output('WIND', wind)

    call get_water()
    call calc_surface_heat_flux( &
    &   wattmp, watsto, lwdn, tair, psrf, qair, wind, &
    &   hflx_srf)
    if (LICE) then
        call enforce_river_ice_capacity()
        call diagnose_river_ice_geometry()
        call calc_ice_heat_fluxes()
        call calc_body_heat_flux( &
        &   watsto, &
        &   swdn_to_water, &
        &   rivdph, rivare, rivvel, &
        &   flddph, fldare, fldvel, &
        &   hflx_bdy)
        call solve_water_ice_heat_budget( &
        &   wattmp, watsto, icevol, icevol_excess, &
        &   rivare + fldare, icearea, icearea_excess, &
        &   hflx_srf, hflx_bdy, hflx_ice_srf, hflx_ice_excess_srf, dt, &
        &   phase_unapplied_energy, phase_mass_budget_error, phase_energy_budget_error)
        call apply_phase_change_to_water_storage()
        call enforce_river_ice_capacity()
        call diagnose_river_ice_geometry()
        call diagnose_ice_temperatures_at_timestep_end()
    else
        call calc_body_heat_flux( &
        &   watsto, &
        &   swdn, &
        &   rivdph, rivare, rivvel, &
        &   flddph, fldare, fldvel, &
        &   hflx_bdy)
        call solve_heat_budget(wattmp, &
        &   watsto, hflx_srf, hflx_bdy, rivare + fldare, dt)
        wattmp(:) = max(wattmp(:), TMELT)
    endif

    ! State diagnostics are synchronized to the end of the current time step.
    call update_output('RIVWAT_TMP', wattmp)
    if (LICE) then
        call update_output('RIVICE_VOL', icevol)
        call update_output('RIVICE_ARE', icearea)
        call update_output('RIVICE_THK', icethickness)
        call update_output('RIVICE_FRC', icefraction)
        call update_output('RIVICE_VOL_EXCESS', icevol_excess)
        call update_output('RIVICE_EXCESS_ARE', icearea_excess)
        call update_output('RIVICE_EXCESS_THK', icethickness_excess)
        call update_output('RIVICE_SRF_TMP', ice_surface_temperature)
        call update_output('RIVICE_EXCESS_TMP', ice_excess_surface_temperature)

        ! Flux diagnostics retain values used by the just-completed update.
        call update_output('RIVICE_COND_FLX', ice_upward_conductive_heat_flux)
        call update_output('RIVICE_ATM_FLX', hflx_ice_srf)
        call update_output('RIVICE_EXCESS_ATM_FLX', hflx_ice_excess_srf)
        call update_output('SWDN_TO_WATER', swdn_to_water)

        ! Conservation diagnostics describe the just-completed local update.
        call update_output('RIVICE_MASS_RESIDUAL', phase_mass_budget_error)
        call update_output('RIVICE_ENERGY_RESIDUAL', phase_energy_budget_error)
        call update_output('RIVICE_ENERGY_UNAPPLIED', phase_unapplied_energy)
    endif
    write(LOGNAM, *) minval(wattmp(:NSEQALL)), maxval(wattmp(:NSEQALL))
    write(LOGNAM, '(a,5(1x,es12.4))') &
    &   '  advection budget max: heat[J], water[m3], unapplied[J], domain_heat[J], domain_relative[-] =', &
    &   maximum_advection_heat_budget_error_j, &
    &   maximum_advection_water_budget_error_m3, &
    &   maximum_advection_unapplied_heat_j, &
    &   maximum_advection_domain_heat_budget_error_j, &
    &   maximum_advection_relative_domain_heat_budget_error
    if (LICE) then
        write(LOGNAM, '(a,5(1x,es12.4))') &
        &   '  ice advection budget max: cell_mass[kg], cell_energy[J], domain_mass[kg], domain_energy[J], domain_relative[-] =', &
        &   maximum_advection_ice_mass_budget_error_kg, &
        &   maximum_advection_combined_energy_budget_error_j, &
        &   maximum_advection_domain_ice_mass_budget_error_kg, &
        &   maximum_advection_domain_combined_energy_budget_error_j, &
        &   maximum_advection_relative_domain_combined_energy_budget_error
    endif
    maximum_advection_heat_budget_error_j = 0.0_JPRD
    maximum_advection_water_budget_error_m3 = 0.0_JPRD
    maximum_advection_unapplied_heat_j = 0.0_JPRD
    maximum_advection_domain_heat_budget_error_j = 0.0_JPRD
    maximum_advection_relative_domain_heat_budget_error = 0.0_JPRD
    maximum_advection_ice_mass_budget_error_kg = 0.0_JPRD
    maximum_advection_combined_energy_budget_error_j = 0.0_JPRD
    maximum_advection_domain_ice_mass_budget_error_kg = 0.0_JPRD
    maximum_advection_domain_combined_energy_budget_error_j = 0.0_JPRD
    maximum_advection_relative_domain_combined_energy_budget_error = 0.0_JPRD
    if (LICE) then
        call log_ice_budget()
    endif
end subroutine calc_heatlink


subroutine write_heatlink_restart(dt)
    type(DateTime), intent(in) :: dt

    call write_restart('RIVWAT_TMP', dt, wattmp)
    if (LICE) then
        call write_restart('RIVICE_VOL', dt, icevol)
        call write_restart('RIVICE_VOL_EXCESS', dt, icevol_excess)
    endif
end subroutine write_heatlink_restart


subroutine enforce_river_ice_capacity()
    real(kind=JPRB) :: &
    &   water_surface_area_m2       ! [m2] Combined river and inundated water-surface area.
    integer(kind=JPIM) :: &
    &   iseq                        ! [-] Vector index of the river cell.

    !$omp simd private(water_surface_area_m2)
    do iseq = 1, NSEQALL
        water_surface_area_m2 = rivare(iseq) + fldare(iseq)
        call enforce_surface_ice_capacity( &
        &   icevol(iseq), icevol_excess(iseq), &
        &   water_surface_area_m2, RIVER_ICE_THICKNESS_MAX_M)
    enddo
end subroutine enforce_river_ice_capacity


subroutine diagnose_river_ice_geometry()
    real(kind=JPRB) :: &
    &   water_surface_area_m2, &      ! [m2] Combined river and inundated water-surface area.
    &   land_surface_area_m2, &       ! [m2] Grid-cell area not occupied by the diagnosed water surface.
    &   excess_surface_area_limit_m2, & ! [m2] Effective area available to immobile excess ice.
    &   excess_ice_fraction           ! [-] Fraction of the effective excess-ice area covered by ice.
    integer(kind=JPIM) :: &
    &   iseq                          ! [-] Vector index of the river cell.

    !$omp simd private(water_surface_area_m2, land_surface_area_m2, excess_surface_area_limit_m2, excess_ice_fraction)
    do iseq = 1, NSEQALL
        water_surface_area_m2 = rivare(iseq) + fldare(iseq)
        call diagnose_ice_geometry( &
        &   icevol(iseq), water_surface_area_m2, &
        &   icearea(iseq), icethickness(iseq), icefraction(iseq))

        ! Existing excess ice never returns to the water-surface pool. Its
        ! effective area follows the non-water part of the source grid cell.
        ! Fully inundated cells fall back to the grid-cell footprint so that
        ! retained excess ice can still exchange heat and melt locally.
        land_surface_area_m2 = max(D2GRAREA(iseq,1) - water_surface_area_m2, 0.0_JPRB)
        if (land_surface_area_m2 > 0.0_JPRB) then
            excess_surface_area_limit_m2 = land_surface_area_m2
        else
            excess_surface_area_limit_m2 = max(D2GRAREA(iseq,1), 0.0_JPRB)
        endif
        call diagnose_ice_geometry( &
        &   icevol_excess(iseq), excess_surface_area_limit_m2, &
        &   icearea_excess(iseq), icethickness_excess(iseq), excess_ice_fraction)
    enddo
end subroutine diagnose_river_ice_geometry


subroutine calc_ice_heat_fluxes()
    call evaluate_ice_surface_thermodynamics(.true.)
end subroutine calc_ice_heat_fluxes


subroutine diagnose_ice_temperatures_at_timestep_end()
    call evaluate_ice_surface_thermodynamics(.false.)
end subroutine diagnose_ice_temperatures_at_timestep_end


subroutine evaluate_ice_surface_thermodynamics(store_applied_fluxes)
    logical, intent(in) :: &
    &   store_applied_fluxes           ! [-] True to retain fluxes applied during the current time step.
    real(kind=JPRB) :: &
    &   surface_atmospheric_heat_flux_w_m2, & ! [W m-2] Atmospheric heat flux into water-surface ice.
    &   surface_transmitted_shortwave_w_m2, & ! [W m-2] Shortwave transmitted through water-surface ice.
    &   surface_temperature_k, &       ! [K] Diagnosed upper temperature of water-surface ice.
    &   surface_upward_conductive_heat_flux_w_m2, & ! [W m-2] Bottom-to-surface water-ice conduction.
    &   excess_atmospheric_heat_flux_w_m2, & ! [W m-2] Atmospheric heat flux into immobile excess ice.
    &   excess_transmitted_shortwave_w_m2, & ! [W m-2] Shortwave transmitted through excess ice.
    &   excess_surface_temperature_k, & ! [K] Diagnosed upper temperature of immobile excess ice.
    &   bottom_thermal_conductance_w_m2_k, & ! [W m-2 K-1] Effective skin-to-bulk conductance; bulk ice is held at TMELT.
    &   excess_upward_conductive_heat_flux_w_m2, & ! [W m-2] Bottom-to-surface flux within excess ice.
    &   newton_residual_w_m2, &       ! [W m-2] Residual from one ice-surface Newton solve.
    &   maximum_newton_residual_w_m2  ! [W m-2] Maximum residual among all ice-surface solves.
    integer(kind=JPIM) :: &
    &   iseq, &                       ! [-] Vector index of the river cell.
    &   newton_iteration_count, &     ! [-] Newton updates used by one ice-surface solve.
    &   maximum_newton_iterations_used, & ! [-] Maximum Newton updates used across the domain.
    &   nonconverged_newton_solve_count ! [-] Number of ice-surface solves that failed to converge.
    logical :: &
    &   newton_converged              ! [-] True when one ice-surface Newton solve converged.

    maximum_newton_residual_w_m2 = 0.0_JPRB
    maximum_newton_iterations_used = 0
    nonconverged_newton_solve_count = 0

    !$omp simd private(surface_atmospheric_heat_flux_w_m2, surface_transmitted_shortwave_w_m2, &
    !$omp& surface_temperature_k, surface_upward_conductive_heat_flux_w_m2, &
    !$omp& excess_atmospheric_heat_flux_w_m2, excess_transmitted_shortwave_w_m2, &
    !$omp& excess_surface_temperature_k, bottom_thermal_conductance_w_m2_k, &
    !$omp& excess_upward_conductive_heat_flux_w_m2, newton_residual_w_m2, &
    !$omp& newton_iteration_count, newton_converged) &
    !$omp& reduction(max:maximum_newton_residual_w_m2, maximum_newton_iterations_used) &
    !$omp& reduction(+:nonconverged_newton_solve_count)
    do iseq = 1, NSEQALL
        if (icearea(iseq) > 0.0_JPRB .and. icevol(iseq) > 0.0_JPRB) then
            if (watsto(iseq) > real(STO_IGNORE, kind=JPRB)) then
                bottom_thermal_conductance_w_m2_k = &
                &   KI / max(icethickness(iseq), ICE_THICKNESS_MIN_M)
            else
                bottom_thermal_conductance_w_m2_k = 0.0_JPRB
            endif
            call calc_ice_surface_heat_flux( &
            &   surface_atmospheric_heat_flux_w_m2, surface_transmitted_shortwave_w_m2, &
            &   surface_temperature_k, surface_upward_conductive_heat_flux_w_m2, &
            &   newton_iteration_count, newton_residual_w_m2, newton_converged, &
            &   swdn(iseq), lwdn(iseq), tair(iseq), icethickness(iseq), &
            &   bottom_thermal_conductance_w_m2_k, NNEWTON_MAX_ICE)
            maximum_newton_residual_w_m2 = max( &
            &   maximum_newton_residual_w_m2, newton_residual_w_m2)
            maximum_newton_iterations_used = max( &
            &   maximum_newton_iterations_used, newton_iteration_count)
            if (.not. newton_converged) then
                nonconverged_newton_solve_count = nonconverged_newton_solve_count + 1
            endif
            ice_surface_temperature(iseq) = surface_temperature_k
            if (store_applied_fluxes) then
                hflx_ice_srf(iseq) = surface_atmospheric_heat_flux_w_m2
                ice_upward_conductive_heat_flux(iseq) = &
                &   surface_upward_conductive_heat_flux_w_m2
                swdn_to_water(iseq) = (1.0_JPRB - icefraction(iseq)) * swdn(iseq) + &
                &   icefraction(iseq) * surface_transmitted_shortwave_w_m2
            endif
        else
            ice_surface_temperature(iseq) = TMELT
            if (store_applied_fluxes) then
                hflx_ice_srf(iseq) = 0.0_JPRB
                swdn_to_water(iseq) = swdn(iseq)
                ice_upward_conductive_heat_flux(iseq) = 0.0_JPRB
            endif
        endif

        if (icearea_excess(iseq) > 0.0_JPRB .and. icevol_excess(iseq) > 0.0_JPRB) then
            ! Immobile excess ice has no underlying liquid-water boundary. Its
            ! zero-layer bottom is therefore insulated (zero conductance).
            call calc_ice_surface_heat_flux( &
            &   excess_atmospheric_heat_flux_w_m2, excess_transmitted_shortwave_w_m2, &
            &   excess_surface_temperature_k, &
            &   excess_upward_conductive_heat_flux_w_m2, &
            &   newton_iteration_count, newton_residual_w_m2, newton_converged, &
            &   swdn(iseq), lwdn(iseq), tair(iseq), icethickness_excess(iseq), &
            &   0.0_JPRB, NNEWTON_MAX_ICE)
            maximum_newton_residual_w_m2 = max( &
            &   maximum_newton_residual_w_m2, newton_residual_w_m2)
            maximum_newton_iterations_used = max( &
            &   maximum_newton_iterations_used, newton_iteration_count)
            if (.not. newton_converged) then
                nonconverged_newton_solve_count = nonconverged_newton_solve_count + 1
            endif
            ice_excess_surface_temperature(iseq) = excess_surface_temperature_k
            if (store_applied_fluxes) then
                hflx_ice_excess_srf(iseq) = excess_atmospheric_heat_flux_w_m2
            endif
        else
            ice_excess_surface_temperature(iseq) = TMELT
            if (store_applied_fluxes) hflx_ice_excess_srf(iseq) = 0.0_JPRB
        endif
    enddo

    if (nonconverged_newton_solve_count > 0) then
        write(LOGNAM, '(a,i0)') &
        &   'ERROR: nonconverged river-ice surface Newton solves = ', &
        &   nonconverged_newton_solve_count
        write(LOGNAM, '(a,i0)') &
        &   'ERROR: maximum Newton iterations used = ', maximum_newton_iterations_used
        write(LOGNAM, '(a,es12.4)') &
        &   'ERROR: maximum Newton residual [W m-2] = ', maximum_newton_residual_w_m2
        error stop 'River-ice surface temperature Newton solve did not converge.'
    endif
end subroutine evaluate_ice_surface_thermodynamics


subroutine apply_phase_change_to_water_storage()
    real(kind=JPRD) :: &
    &   canonical_total_volume_m3, & ! [m3] Canonical CaMa liquid storage before applying local phase change.
    &   liquid_volume_delta_m3, &    ! [m3] Final minus initial liquid-water volume from local phase change.
    &   unavailable_liquid_volume_m3, & ! [m3] Requested removal exceeding available liquid storage.
    &   maximum_unavailable_liquid_volume_m3 ! [m3] Largest unavailable removal across invalid cells.
    real(kind=JPRB) :: &
    &   phase_initial_liquid_volume_m3 ! [m3] Liquid storage presented to the JPRB phase-change kernel.
    integer(kind=JPIM) :: &
    &   iseq, &                      ! [-] Vector index of the river cell.
    &   invalid_update_cell_count    ! [-] Number of storage deltas that cannot be applied.
    logical :: &
    &   update_is_valid              ! [-] True when the current storage delta was applied safely.

    invalid_update_cell_count = 0
    maximum_unavailable_liquid_volume_m3 = 0.0_JPRD
    do iseq = 1, NSEQALL
        canonical_total_volume_m3 = P2RIVSTO(iseq,1) + P2FLDSTO(iseq,1)
        phase_initial_liquid_volume_m3 = real(canonical_total_volume_m3, kind=JPRB)
        liquid_volume_delta_m3 = real( &
        &   watsto(iseq) - phase_initial_liquid_volume_m3, kind=JPRD)
        call apply_liquid_volume_delta_to_storage( &
        &   river_storage_volume_m3=P2RIVSTO(iseq,1), &
        &   floodplain_storage_volume_m3=P2FLDSTO(iseq,1), &
        &   liquid_volume_delta_m3=liquid_volume_delta_m3, &
        &   update_is_valid=update_is_valid, &
        &   unavailable_liquid_volume_m3=unavailable_liquid_volume_m3)
        if (.not. update_is_valid) then
            invalid_update_cell_count = invalid_update_cell_count + 1
            maximum_unavailable_liquid_volume_m3 = max( &
            &   maximum_unavailable_liquid_volume_m3, unavailable_liquid_volume_m3)
        endif
        D2STORGE(iseq,1) = real(P2RIVSTO(iseq,1) + P2FLDSTO(iseq,1), kind=JPRB)
        watsto(iseq) = D2STORGE(iseq,1)
    enddo
    if (invalid_update_cell_count > 0) then
        write(LOGNAM, '(a,i0)') &
        &   'ERROR: invalid phase-change storage-update cell count = ', &
        &   invalid_update_cell_count
        write(LOGNAM, '(a,es12.4)') &
        &   'ERROR: maximum unavailable liquid-water removal [m3] = ', &
        &   maximum_unavailable_liquid_volume_m3
        error stop 'Invalid phase-change update to canonical CaMa storage.'
    endif
end subroutine apply_phase_change_to_water_storage


subroutine log_ice_budget()
    real(kind=JPRB) :: &
    &   local_mass_scale_kg, &     ! [kg] Local total water-plus-ice mass used to normalize mass error.
    &   local_energy_scale_j, &    ! [J] Local water-plus-ice energy magnitude used to normalize energy error.
    &   maximum_relative_mass_error, & ! [-] Maximum cellwise relative mass-conservation error.
    &   maximum_relative_energy_error  ! [-] Maximum cellwise relative energy-conservation error.
    integer(kind=JPIM) :: &
    &   iseq                         ! [-] Vector index of the river cell.

    maximum_relative_mass_error = 0.0_JPRB
    maximum_relative_energy_error = 0.0_JPRB
    do iseq = 1, NSEQALL
        local_mass_scale_kg = max(abs(water_ice_mass_kg( &
        &   watsto(iseq), icevol(iseq) + icevol_excess(iseq))), 1.0_JPRB)
        local_energy_scale_j = max(abs(water_ice_energy_j( &
        &   watsto(iseq), wattmp(iseq), &
        &   icevol(iseq) + icevol_excess(iseq), TMELT)), &
        &   abs(phase_unapplied_energy(iseq)), 1.0_JPRB)
        maximum_relative_mass_error = max(maximum_relative_mass_error, &
        &   abs(phase_mass_budget_error(iseq)) / local_mass_scale_kg)
        maximum_relative_energy_error = max(maximum_relative_energy_error, &
        &   abs(phase_energy_budget_error(iseq)) / local_energy_scale_j)
    enddo

    write(LOGNAM, '(a,3(1x,es12.4))') &
    &   '  ice budget max(abs): mass_residual[kg], energy_residual[J], unapplied_energy[J] =', &
    &   maxval(abs(phase_mass_budget_error(:NSEQALL))), &
    &   maxval(abs(phase_energy_budget_error(:NSEQALL))), &
    &   maxval(abs(phase_unapplied_energy(:NSEQALL)))
    write(LOGNAM, '(a,2(1x,es12.4))') &
    &   '  ice budget max(relative): mass_residual[-], energy_residual[-] =', &
    &   maximum_relative_mass_error, maximum_relative_energy_error
end subroutine log_ice_budget


subroutine get_water
    real(kind=JPRB) :: &
    &   dph_new, wth_new, sto_new, m
    integer(kind=JPIM) :: &
    &   iseq

    watsto(:) = real(P2RIVSTO(:,1) + P2FLDSTO(:,1), kind=JPRB)

    rivdph(:) = D2RIVDPH(:, 1)
    rivare(:) = D2RIVLEN(:, 1) * D2RIVWTH(:, 1)
    rivvel(:) = D2RIVVEL(:, 1)
    flddph(:) = D2FLDDPH(:, 1)
    fldare(:) = D2FLDARE(:, 1)
    fldvel(:) = D2FLDVEL(:, 1)

    ! correct shallow flooded water
    !$omp simd private(m, sto_new)
    do iseq = 1, NSEQALL
        m = merge(1.0_JPRB, 0.0_JPRB, (P2FLDSTO(iseq,1) > 0.0_JPRD) .and. (flddph(iseq) < RIVDPH_MIN))
        sto_new = real(P2RIVSTO(iseq,1) + m * P2FLDSTO(iseq,1), JPRB)
        rivdph(iseq) = (1.0_JPRB - m) * rivdph(iseq) + m * (sto_new / rivare(iseq))

        flddph(iseq) = (1.0_JPRB - m) * flddph(iseq)
        fldare(iseq) = (1.0_JPRB - m) * fldare(iseq)
        fldvel(iseq) = (1.0_JPRB - m) * fldvel(iseq)
    end do

    ! correct shallow river water
    !$omp simd private(dph_new, wth_new, m)
    do iseq = 1, NSEQALL
        dph_new = max(rivdph(iseq), RIVDPH_MIN)
        m = merge(1.0_JPRB, 0.0_JPRB, dph_new > rivdph(iseq)) ! m = 1 if corrected, else 0

        ! rivare_new = rivwth_old * rivdph_old / dph_new
        wth_new = (1.0_JPRB - m) * D2RIVWTH(iseq,1) + m * D2RIVWTH(iseq,1) * rivdph(iseq) / dph_new
        rivare(iseq) = D2RIVLEN(iseq,1) * wth_new
        rivdph(iseq) = dph_new
    end do
end subroutine get_water


subroutine fin_heatlink_river_mod()
    use thermo_mod, only: &
    &   fin_thermo_mod

    write(LOGNAM, '(a)') '[fin_heatlink_river_mod]'
    if (allocated(wattmp)) deallocate(wattmp)
    if (allocated(advection_initial_liquid_volume_m3)) deallocate(advection_initial_liquid_volume_m3)
    if (allocated(advection_heat_budget_error_j)) deallocate(advection_heat_budget_error_j)
    if (allocated(advection_water_budget_error_m3)) deallocate(advection_water_budget_error_m3)
    if (allocated(advection_unapplied_sensible_heat_j)) deallocate(advection_unapplied_sensible_heat_j)
    if (allocated(advection_ice_budget_error_m3)) deallocate(advection_ice_budget_error_m3)
    if (allocated(advection_combined_energy_budget_error_j)) deallocate(advection_combined_energy_budget_error_j)
    if (allocated(advection_runoff_flow_m3s)) deallocate(advection_runoff_flow_m3s)
    if (allocated(advection_upstream_flow_m3s)) deallocate(advection_upstream_flow_m3s)
    if (allocated(hflx_srf)) deallocate(hflx_srf)
    if (allocated(hflx_bdy)) deallocate(hflx_bdy)
    if (allocated(hflx_ice_srf)) deallocate(hflx_ice_srf)
    if (allocated(hflx_ice_excess_srf)) deallocate(hflx_ice_excess_srf)
    if (allocated(swdn_to_water)) deallocate(swdn_to_water)
    if (allocated(phase_unapplied_energy)) deallocate(phase_unapplied_energy)
    if (allocated(phase_mass_budget_error)) deallocate(phase_mass_budget_error)
    if (allocated(phase_energy_budget_error)) deallocate(phase_energy_budget_error)
    if (allocated(icevol)) deallocate(icevol)
    if (allocated(icevol_excess)) deallocate(icevol_excess)
    if (allocated(icearea)) deallocate(icearea)
    if (allocated(icethickness)) deallocate(icethickness)
    if (allocated(icefraction)) deallocate(icefraction)
    if (allocated(icearea_excess)) deallocate(icearea_excess)
    if (allocated(icethickness_excess)) deallocate(icethickness_excess)
    if (allocated(ice_surface_temperature)) deallocate(ice_surface_temperature)
    if (allocated(ice_upward_conductive_heat_flux)) deallocate(ice_upward_conductive_heat_flux)
    if (allocated(ice_excess_surface_temperature)) deallocate(ice_excess_surface_temperature)
    if (allocated(lwdn)) deallocate(lwdn)
    if (allocated(psrf)) deallocate(psrf)
    if (allocated(qair)) deallocate(qair)
    if (allocated(swdn)) deallocate(swdn)
    if (allocated(tair)) deallocate(tair)
    if (allocated(trof)) deallocate(trof)
    if (allocated(wind)) deallocate(wind)
    if (allocated(watsto)) deallocate(watsto)
    if (allocated(rivdph)) deallocate(rivdph)
    if (allocated(rivare)) deallocate(rivare)
    if (allocated(rivvel)) deallocate(rivvel)
    if (allocated(flddph)) deallocate(flddph)
    if (allocated(fldare)) deallocate(fldare)
    if (allocated(fldvel)) deallocate(fldvel)
    call fin_thermo_mod()
end subroutine fin_heatlink_river_mod
#endif
end module heatlink_river_mod
