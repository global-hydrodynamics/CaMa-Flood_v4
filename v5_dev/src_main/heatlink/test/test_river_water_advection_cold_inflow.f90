program test_river_water_advection_cold_inflow
    use PARKIND1, only: &
    &   JPIM, JPRB, JPRD
    use YOS_CMF_MAP, only: &
    &   I1NEXT, NSEQALL, NSEQRIV, NPTHOUT
    use phys_const_mod, only: &
    &   TMELT
    use river_water_advection_mod, only: &
    &   advect_river_water_sensible_heat
    implicit none
    real(kind=JPRB) :: &
    &   water_temperature_k(1), normal_flow_m3s(1), runoff_flow_m3s(1), &
    &   inflow_temperature_k(1)
    real(kind=JPRD) :: &
    &   liquid_volume_before_m3(1), liquid_volume_after_m3(1)

    NSEQALL = 1_JPIM
    NSEQRIV = 0_JPIM
    NPTHOUT = 0_JPIM
    if (allocated(I1NEXT)) deallocate(I1NEXT)
    allocate(I1NEXT(NSEQALL), source=-9_JPIM)

    water_temperature_k(:) = TMELT + 1.0_JPRB
    liquid_volume_before_m3(:) = 10.0_JPRD
    liquid_volume_after_m3(:) = 11.0_JPRD
    normal_flow_m3s(:) = 0.0_JPRB
    runoff_flow_m3s(:) = 1.0_JPRB
    inflow_temperature_k(:) = TMELT - 1.0_JPRB

    call advect_river_water_sensible_heat( &
    &   water_temperature_k=water_temperature_k, &
    &   liquid_volume_before_m3=liquid_volume_before_m3, &
    &   liquid_volume_after_m3=liquid_volume_after_m3, &
    &   normal_flow_m3s=normal_flow_m3s, &
    &   dt_seconds=1.0_JPRB, &
    &   runoff_flow_m3s=runoff_flow_m3s, &
    &   inflow_temperature_k=inflow_temperature_k)

    error stop 'Cold liquid inflow was not rejected by the advection kernel.'
end program test_river_water_advection_cold_inflow
