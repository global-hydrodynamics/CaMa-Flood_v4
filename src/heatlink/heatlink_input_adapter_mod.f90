module heatlink_input_adapter_mod
#ifdef heatlink
    use PARKIND1, only: &
    &   JPRB
    use phys_const_mod, only: &
    &   TMELT
    implicit none
    private
    public :: &
    &   enforce_liquid_inflow_temperature

contains

subroutine enforce_liquid_inflow_temperature(temperature_k)
    real(kind=JPRB), intent(inout) :: &
    &   temperature_k(:) ! [K] Liquid-water inflow temperature; returned no colder than TMELT.

    temperature_k(:) = max(temperature_k(:), TMELT)
end subroutine enforce_liquid_inflow_temperature
#endif
end module heatlink_input_adapter_mod
