module heatlink_river_mod
    use PARKIND1, only: &
    &   JPRB
    use YOS_CMF_INPUT, only: &
    &   LOGNAM, LRESTART
    use YOS_CMF_MAP, only: &
    &   NSEQMAX
    use datetime_mod, only: &
    &   DateTime

    use input_mod, only: &
    &   add_input, get_input
    use output_mod, only: &
    &   update_output
    use restart_mod, only: &
    &   read_restart, write_restart
    implicit none
    private
    public :: &
    &   init_heatlink_river_mod, calc_heatlink, &
    &   write_heatlink_restart, fin_heatlink_river_mod

    real(kind=JPRB), allocatable, save :: &
    &   wattmp(:) ! [K] river water temperature

    ! atmospheric forcing
    real(kind=JPRB), allocatable, save :: &
    &   lwdn(:), & ! [W m-2] downward longwave radiation
    &   psrf(:), & ! [hPa] surface pressure
    &   qair(:), & ! [kg kg-1] specific humidity
    &   swdn(:), & ! [W m-2] downward shortwave radiation
    &   tair(:), & ! [K] air temperature
    &   trof(:), & ! [K] tropopause temperature
    &   wind(:)  ! [m s-1] wind speed

contains

subroutine init_heatlink_river_mod(dt)
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

    allocate(wattmp(NSEQMAX), source=0.0_JPRB)
    allocate(lwdn(NSEQMAX), source=0.0_JPRB)
    allocate(psrf(NSEQMAX), source=0.0_JPRB)
    allocate(qair(NSEQMAX), source=0.0_JPRB)
    allocate(swdn(NSEQMAX), source=0.0_JPRB)
    allocate(tair(NSEQMAX), source=0.0_JPRB)
    allocate(trof(NSEQMAX), source=0.0_JPRB)
    allocate(wind(NSEQMAX), source=0.0_JPRB)

    if (LRESTART) then
        call read_restart('RIVWAT_TMP', dt, is_found, wattmp)
        if (.not. is_found) stop 'RIVWAT_TMP restart was not found.'
    endif
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
    trof(:) = max(trof(:), 273.15_JPRB)

    call update_output('LWDN', lwdn)
    call update_output('PSRF', psrf)
    call update_output('QAIR', qair)
    call update_output('SWDN', swdn)
    call update_output('TAIR', tair)
    call update_output('TROF', trof)
    call update_output('WIND', wind)

    wattmp(:) = tair(:) + 10.0_JPRB
    call update_output('RIVWAT_TMP', wattmp)
end subroutine calc_heatlink

subroutine write_heatlink_restart(dt)
    type(DateTime), intent(in) :: dt

    call write_restart('RIVWAT_TMP', dt, wattmp)
end subroutine write_heatlink_restart

subroutine fin_heatlink_river_mod()
    write(LOGNAM, '(a)') '[fin_heatlink_river_mod]'
    deallocate(wattmp)
end subroutine fin_heatlink_river_mod

end module heatlink_river_mod
