module heatlink_river_mod
#ifdef heatlink
    use PARKIND1, only: &
    &   JPIM, JPRB, JPRD
    use YOS_CMF_INPUT, only: &
    &   LOGNAM, LRESTART, LICE
    use YOS_CMF_MAP, only: &
    &   NSEQMAX, NSEQALL, &
    &   D2GRAREA, D2RIVLEN, D2RIVWTH
    use YOS_CMF_DIAG, only: &
    &   D2STORGE, &
    &   D2RIVDPH, D2RIVVEL, D2FLDDPH, D2FLDVEL, D2FLDARE
    use YOS_CMF_PROG, only: &
    &   P2RIVSTO, P2FLDSTO
    use datetime_mod, only: &
    &   DateTime

    use const_mod, only: &
    &   STO_IGNORE
    use phys_const_mod, only: &
    &   TMELT, RIVDPH_MIN, KI
    use ice_cover_mod, only: &
    &   ICE_THICKNESS_MIN_M, diagnose_ice_shape, update_ice_cover_state
    use heat_flux_mod, only: &
    &   calc_ice_surface_heat_flux
    use heat_budget_mod, only: &
    &   water_ice_mass_kg, water_ice_energy_j
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
    &   init_heatlink_river_mod, calc_heatlink, &
    &   write_heatlink_restart, fin_heatlink_river_mod

    real(kind=JPRB), allocatable, save :: &
    &   wattmp(:) ! [K] river water temperature

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
    &   ice_surface_temperature(:), & ! [K] Upper-surface temperature of water-surface ice.
    &   ice_mean_temperature(:), & ! [K] Vertical-mean temperature of water-surface ice.
    &   ice_upward_conductive_heat_flux(:), & ! [W m-2] Bottom-to-surface conductive flux within water-surface ice.
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
    &   trof(:), & ! [K] tropopause temperature
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
    allocate(icevol(NSEQMAX), source=0.0_JPRB)
    allocate(icevol_excess(NSEQMAX), source=0.0_JPRB)
    allocate(icearea(NSEQMAX), source=0.0_JPRB)
    allocate(icethickness(NSEQMAX), source=0.0_JPRB)
    allocate(icefraction(NSEQMAX), source=0.0_JPRB)
    allocate(icearea_excess(NSEQMAX), source=0.0_JPRB)
    allocate(icethickness_excess(NSEQMAX), source=0.0_JPRB)
    allocate(ice_surface_temperature(NSEQMAX), source=TMELT)
    allocate(ice_mean_temperature(NSEQMAX), source=TMELT)
    allocate(ice_upward_conductive_heat_flux(NSEQMAX), source=0.0_JPRB)
    allocate(ice_excess_surface_temperature(NSEQMAX), source=TMELT)
    allocate(hflx_srf(NSEQMAX), source=0.0_JPRB)
    allocate(hflx_bdy(NSEQMAX), source=0.0_JPRB)
    allocate(hflx_ice_srf(NSEQMAX), source=0.0_JPRB)
    allocate(hflx_ice_excess_srf(NSEQMAX), source=0.0_JPRB)
    allocate(swdn_to_water(NSEQMAX), source=0.0_JPRB)
    allocate(phase_unapplied_energy(NSEQMAX), source=0.0_JPRB)
    allocate(phase_mass_budget_error(NSEQMAX), source=0.0_JPRB)
    allocate(phase_energy_budget_error(NSEQMAX), source=0.0_JPRB)

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
        icevol(:) = 0.0_JPRB
        icevol_excess(:) = 0.0_JPRB
    endif
    ! Defer the first water/ice geometry diagnosis to calc_heatlink, after
    ! CaMa advances. Diagnosing here would add an extra irreversible transfer
    ! from water-surface ice to immobile excess ice only on restart runs.
    write(LOGNAM, *)
end subroutine init_heatlink_river_mod


subroutine calc_heatlink(dt)
    real(kind=JPRB), intent(in) :: dt ! time step (seconds)

    write(LOGNAM, '(a)') '[heatlink_river_mod/calc_heatlink]'
    call get_input('LWDN', lwdn)
    call get_input('PSRF', psrf)
    call get_input('QAIR', qair)
    call get_input('SWDN', swdn)
    call get_input('TAIR', tair)
    call get_input('TROF', trof)
    call get_input('WIND', wind)
    trof(:) = max(trof(:), TMELT)

    call update_output('LWDN', lwdn)
    call update_output('PSRF', psrf)
    call update_output('QAIR', qair)
    call update_output('SWDN', swdn)
    call update_output('TAIR', tair)
    call update_output('TROF', trof)
    call update_output('WIND', wind)

    call get_water()
    call update_ice_cover()
    call calc_surface_heat_flux( &
    &   wattmp, watsto, lwdn, tair, psrf, qair, wind, &
    &   hflx_srf)
    if (LICE) then
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
        call put_water_storage()
        call update_ice_cover()
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
        call update_output('RIVICE_MEAN_TMP', ice_mean_temperature)
        call update_output('RIVICE_COND_FLX', ice_upward_conductive_heat_flux)
        call update_output('RIVICE_EXCESS_TMP', ice_excess_surface_temperature)
        call update_output('RIVICE_MASS_ERROR', phase_mass_budget_error)
        call update_output('RIVICE_ENERGY_ERROR', phase_energy_budget_error)
        call update_output('RIVICE_ENERGY_UNAPPLIED', phase_unapplied_energy)
    endif
    write(LOGNAM, *) minval(wattmp(:NSEQALL)), maxval(wattmp(:NSEQALL))
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


subroutine update_ice_cover()
    real(kind=JPRB) :: &
    &   water_surface_area_m2, &      ! [m2] Combined river and inundated water-surface area.
    &   land_surface_area_m2, &       ! [m2] Grid-cell area not occupied by the diagnosed water surface.
    &   excess_surface_area_limit_m2, & ! [m2] Effective area available to immobile excess ice.
    &   excess_ice_fraction           ! [-] Fraction of the effective excess-ice area covered by ice.
    integer(kind=JPIM) :: &
    &   iseq                          ! [-] Vector index of the river cell.

    if (.not. LICE) then
        icevol(:) = 0.0_JPRB
        icearea(:) = 0.0_JPRB
        icethickness(:) = 0.0_JPRB
        icefraction(:) = 0.0_JPRB
        icevol_excess(:) = 0.0_JPRB
        icearea_excess(:) = 0.0_JPRB
        icethickness_excess(:) = 0.0_JPRB
        return
    endif

    !$omp simd private(water_surface_area_m2, land_surface_area_m2, excess_surface_area_limit_m2, excess_ice_fraction)
    do iseq = 1, NSEQALL
        water_surface_area_m2 = rivare(iseq) + fldare(iseq)
        call update_ice_cover_state( &
        &   icevol(iseq), icevol_excess(iseq), &
        &   water_surface_area_m2, RIVER_ICE_THICKNESS_MAX_M, &
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
        call diagnose_ice_shape( &
        &   icevol_excess(iseq), excess_surface_area_limit_m2, &
        &   icearea_excess(iseq), icethickness_excess(iseq), excess_ice_fraction)
    enddo
end subroutine update_ice_cover


subroutine calc_ice_heat_fluxes()
    real(kind=JPRB) :: &
    &   transmitted_shortwave_w_m2, & ! [W m-2] Shortwave radiation transmitted through excess ice.
    &   bottom_thermal_conductance_w_m2_k, & ! [W m-2 K-1] Ice conductance to a bottom boundary at TMELT.
    &   excess_upward_conductive_heat_flux_w_m2 ! [W m-2] Bottom-to-surface flux within excess ice.
    integer(kind=JPIM) :: &
    &   iseq                         ! [-] Vector index of the river cell.

    !$omp simd private(transmitted_shortwave_w_m2, bottom_thermal_conductance_w_m2_k, &
    !$omp& excess_upward_conductive_heat_flux_w_m2)
    do iseq = 1, NSEQALL
        if (icearea(iseq) > 0.0_JPRB .and. icevol(iseq) > 0.0_JPRB) then
            if (watsto(iseq) > real(STO_IGNORE, kind=JPRB)) then
                bottom_thermal_conductance_w_m2_k = &
                &   KI / max(icethickness(iseq), ICE_THICKNESS_MIN_M)
            else
                bottom_thermal_conductance_w_m2_k = 0.0_JPRB
            endif
            call calc_ice_surface_heat_flux( &
            &   hflx_ice_srf(iseq), swdn_to_water(iseq), &
            &   ice_surface_temperature(iseq), ice_upward_conductive_heat_flux(iseq), &
            &   swdn(iseq), lwdn(iseq), tair(iseq), icethickness(iseq), &
            &   bottom_thermal_conductance_w_m2_k)
            if (bottom_thermal_conductance_w_m2_k > 0.0_JPRB) then
                ice_mean_temperature(iseq) = &
                &   0.5_JPRB * (TMELT + ice_surface_temperature(iseq))
            else
                ice_mean_temperature(iseq) = ice_surface_temperature(iseq)
            endif
            swdn_to_water(iseq) = (1.0_JPRB - icefraction(iseq)) * swdn(iseq) + &
            &   icefraction(iseq) * swdn_to_water(iseq)
        else
            hflx_ice_srf(iseq) = 0.0_JPRB
            swdn_to_water(iseq) = swdn(iseq)
            ice_surface_temperature(iseq) = TMELT
            ice_mean_temperature(iseq) = TMELT
            ice_upward_conductive_heat_flux(iseq) = 0.0_JPRB
        endif

        if (icearea_excess(iseq) > 0.0_JPRB .and. icevol_excess(iseq) > 0.0_JPRB) then
            ! Immobile excess ice has no underlying liquid-water boundary. Its
            ! zero-layer bottom is therefore insulated (zero conductance).
            call calc_ice_surface_heat_flux( &
            &   hflx_ice_excess_srf(iseq), transmitted_shortwave_w_m2, &
            &   ice_excess_surface_temperature(iseq), &
            &   excess_upward_conductive_heat_flux_w_m2, &
            &   swdn(iseq), lwdn(iseq), tair(iseq), icethickness_excess(iseq), &
            &   0.0_JPRB)
        else
            hflx_ice_excess_srf(iseq) = 0.0_JPRB
            ice_excess_surface_temperature(iseq) = TMELT
        endif
    enddo
end subroutine calc_ice_heat_fluxes


subroutine put_water_storage()
    real(kind=JPRD) :: &
    &   old_river_volume_m3, &      ! [m3] River storage before applying local phase change.
    &   old_floodplain_volume_m3, & ! [m3] Floodplain storage before applying local phase change.
    &   old_total_volume_m3, &      ! [m3] Total liquid-water storage before applying local phase change.
    &   new_total_volume_m3, &      ! [m3] Total liquid-water storage after applying local phase change.
    &   river_storage_fraction      ! [-] Fraction of pre-update liquid water held in the river storage.
    integer(kind=JPIM) :: &
    &   iseq                         ! [-] Vector index of the river cell.

    do iseq = 1, NSEQALL
        old_river_volume_m3 = max(P2RIVSTO(iseq,1), 0.0_JPRD)
        old_floodplain_volume_m3 = max(P2FLDSTO(iseq,1), 0.0_JPRD)
        old_total_volume_m3 = old_river_volume_m3 + old_floodplain_volume_m3
        new_total_volume_m3 = max(real(watsto(iseq), kind=JPRD), 0.0_JPRD)

        if (old_total_volume_m3 > 0.0_JPRD) then
            river_storage_fraction = old_river_volume_m3 / old_total_volume_m3
            P2RIVSTO(iseq,1) = new_total_volume_m3 * river_storage_fraction
            P2FLDSTO(iseq,1) = new_total_volume_m3 - P2RIVSTO(iseq,1)
        else
            P2RIVSTO(iseq,1) = new_total_volume_m3
            P2FLDSTO(iseq,1) = 0.0_JPRD
        endif
        D2STORGE(iseq,1) = real(P2RIVSTO(iseq,1) + P2FLDSTO(iseq,1), kind=JPRB)
    enddo
end subroutine put_water_storage


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
    &   '  ice budget max(abs): mass_error[kg], energy_error[J], unapplied_energy[J] =', &
    &   maxval(abs(phase_mass_budget_error(:NSEQALL))), &
    &   maxval(abs(phase_energy_budget_error(:NSEQALL))), &
    &   maxval(abs(phase_unapplied_energy(:NSEQALL)))
    write(LOGNAM, '(a,2(1x,es12.4))') &
    &   '  ice budget max(relative): mass_error[-], energy_error[-] =', &
    &   maximum_relative_mass_error, maximum_relative_energy_error
end subroutine log_ice_budget


subroutine get_water
    real(kind=JPRB) :: &
    &   dph_new, wth_new, sto_new, m
    integer(kind=JPIM) :: &
    &   iseq

    watsto(:) = D2STORGE(:, 1)

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
    deallocate(wattmp, hflx_srf, hflx_bdy)
    deallocate(hflx_ice_srf, hflx_ice_excess_srf, swdn_to_water)
    deallocate(phase_unapplied_energy, phase_mass_budget_error, phase_energy_budget_error)
    deallocate(icevol, icevol_excess, icearea, icethickness, icefraction)
    deallocate(icearea_excess, icethickness_excess)
    deallocate(ice_surface_temperature, ice_mean_temperature)
    deallocate(ice_upward_conductive_heat_flux, ice_excess_surface_temperature)
    deallocate(lwdn, psrf, qair, swdn, tair, trof, wind)
    deallocate(watsto, rivdph, rivare, rivvel, flddph, fldare, fldvel)
    call fin_thermo_mod()
end subroutine fin_heatlink_river_mod
#endif
end module heatlink_river_mod
