module heatlink_river_mod
    use PARKIND1, only: &
    &   JPIM, JPRB, JPRM
    use YOS_CMF_INPUT, only: &
    &   LOGNAM
    use YOS_CMF_MAP, only: &
    &   NSEQMAX

    use input_mod, only: &
    &   update_input, get_input
    use output_mod, only: &
    &   update_output, write_output
    implicit none
    private
    public :: &
    &   init_heatlink_river_mod, calc_heatlink, output_heatlink, fin_heatlink_river_mod

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

subroutine init_heatlink_river_mod()
    !use glob_mod, only: &
    !&   init_glob_mod
    use dim_converter, only: &
    &   init_dim_converter
    use input_mod, only: &
    &   init_input_mod
    use output_mod, only: &
    &   init_output_mod

    write(LOGNAM, '(a)') '[init_heatlink_river_mod]'
    !call init_glob_mod
    call init_dim_converter
    call init_input_mod(0)
    write(LOGNAM, '(a)') '  read the first-step input'
    call update_input(0)
    call init_output_mod()
    allocate(wattmp(NSEQMAX), source=0.0_JPRB)
    allocate(lwdn(NSEQMAX), source=0.0_JPRB)
    allocate(psrf(NSEQMAX), source=0.0_JPRB)
    allocate(qair(NSEQMAX), source=0.0_JPRB)
    allocate(swdn(NSEQMAX), source=0.0_JPRB)
    allocate(tair(NSEQMAX), source=0.0_JPRB)
    allocate(trof(NSEQMAX), source=0.0_JPRB)
    allocate(wind(NSEQMAX), source=0.0_JPRB)
end subroutine init_heatlink_river_mod


subroutine calc_heatlink(t, dt)
    integer(kind=JPIM), intent(in) :: t ! current time (seconds)
    real(kind=JPRB), intent(in) :: dt ! time step (seconds)

    write(LOGNAM, '(a)') '[calc_heatlink]'
    call update_input(t)
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

subroutine output_heatlink
    write(LOGNAM, '(a)') '[output_heatlink]'
    call write_output()
end subroutine output_heatlink

subroutine fin_heatlink_river_mod()
    use output_mod, only: &
    &   fin_output_mod
    write(LOGNAM, '(a)') '[fin_heatlink_river_mod]'
    deallocate(wattmp)
    call fin_output_mod()
end subroutine fin_heatlink_river_mod

end module heatlink_river_mod