module heatlink_river_mod
    use PARKIND1, only: &
    &   JPIM, JPRB, JPRM
    use YOS_CMF_INPUT, only: &
    &   LOGNAM
    use YOS_CMF_MAP, only: &
    &   NSEQMAX
    implicit none
    private
    public :: &
    &   init_heatlink_river_mod, fin_heatlink_river_mod

    real(kind=JPRB), allocatable, save :: &
    &   wattmp(:)
contains

subroutine init_heatlink_river_mod()
    use input_mod, only: &
    &   init_input_mod
    write(LOGNAM, '(a)') '[init_heatlink_river_mod]'
    call init_input_mod(0)
    allocate(wattmp(NSEQMAX), source=0.0_JPRB)
end subroutine init_heatlink_river_mod

subroutine fin_heatlink_river_mod()
    write(LOGNAM, '(a)') '[fin_heatlink_river_mod]'
    deallocate(wattmp)
end subroutine fin_heatlink_river_mod

end module heatlink_river_mod