module heatlink_river_mod
#ifdef heatlink
    use PARKIND1, only: &
    &   JPIM, JPRB, JPRD
    use YOS_CMF_INPUT, only: &
    &   LOGNAM, LRESTART, LICE
    use YOS_CMF_MAP, only: &
    &   NSEQMAX, NSEQALL, &
    &   D2RIVLEN, D2RIVWTH
    use YOS_CMF_DIAG, only: &
    &   D2STORGE, &
    &   D2RIVDPH, D2RIVVEL, D2FLDDPH, D2FLDVEL, D2FLDARE
    use YOS_CMF_PROG, only: &
    &   P2RIVSTO, P2FLDSTO
    use datetime_mod, only: &
    &   DateTime

    use phys_const_mod, only: &
    &   TMELT, RIVDPH_MIN
    use ice_cover_mod, only: &
    &   update_ice_cover_state
    use input_mod, only: &
    &   add_input, get_input
    use output_mod, only: &
    &   update_output
    use restart_mod, only: &
    &   read_restart, write_restart
    use thermo_mod, only: &
    &   calc_surface_heat_flux, calc_body_heat_flux, solve_heat_budget
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
    &   icefraction(:)     ! [-] Fraction of the water surface covered by ice.

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
    &   hflx_bdy(:)    ! [W m-2] body heat flux (+: into water)

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
    allocate(hflx_srf(NSEQMAX), source=0.0_JPRB)
    allocate(hflx_bdy(NSEQMAX), source=0.0_JPRB)

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
    call get_water()
    call update_ice_cover()
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
    call calc_body_heat_flux( &
    &   watsto, &
    &   swdn, &
    &   rivdph, rivare, rivvel, &
    &   flddph, fldare, fldvel, &
    &   hflx_bdy)
    call solve_heat_budget(wattmp, &
    &   watsto, hflx_srf, hflx_bdy, rivare + fldare, dt)
    wattmp(:) = max(wattmp(:), TMELT)

    call update_output('RIVWAT_TMP', wattmp)
    if (LICE) then
        call update_output('RIVICE_VOL', icevol)
        call update_output('RIVICE_ARE', icearea)
        call update_output('RIVICE_THK', icethickness)
        call update_output('RIVICE_FRC', icefraction)
        call update_output('RIVICE_VOL_EXCESS', icevol_excess)
    endif
    write(LOGNAM, *) minval(wattmp), maxval(wattmp)
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
    &   water_surface_area_m2        ! [m2] Combined river and inundated water-surface area.
    integer(kind=JPIM) :: &
    &   iseq                          ! [-] Vector index of the river cell.

    if (.not. LICE) then
        icevol(:) = 0.0_JPRB
        icearea(:) = 0.0_JPRB
        icethickness(:) = 0.0_JPRB
        icefraction(:) = 0.0_JPRB
        icevol_excess(:) = 0.0_JPRB
        return
    endif

    !$omp simd private(water_surface_area_m2)
    do iseq = 1, NSEQALL
        water_surface_area_m2 = rivare(iseq) + fldare(iseq)
        call update_ice_cover_state( &
        &   icevol(iseq), icevol_excess(iseq), &
        &   water_surface_area_m2, RIVER_ICE_THICKNESS_MAX_M, &
        &   icearea(iseq), icethickness(iseq), icefraction(iseq))
    enddo
end subroutine update_ice_cover


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
    deallocate(icevol, icevol_excess, icearea, icethickness, icefraction)
    deallocate(lwdn, psrf, qair, swdn, tair, trof, wind)
    deallocate(watsto, rivdph, rivare, rivvel, flddph, fldare, fldvel)
    call fin_thermo_mod()
end subroutine fin_heatlink_river_mod
#endif
end module heatlink_river_mod
