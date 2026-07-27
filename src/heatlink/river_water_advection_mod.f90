module river_water_advection_mod
#ifdef heatlink
    use PARKIND1, only: &
    &   JPIM, JPRB, JPRD
    use YOS_CMF_MAP, only: &
    &   I1NEXT, NSEQALL, NSEQRIV, NPTHOUT, PTH_UPST, PTH_DOWN
    use phys_const_mod, only: &
    &   CW, RW, TMELT
    implicit none
    private
    public :: &
    &   advect_river_water_sensible_heat, liquid_inflow_temperature_is_valid

contains

subroutine advect_river_water_sensible_heat( &
    &   water_temperature_k, liquid_volume_before_m3, liquid_volume_after_m3, &
    &   normal_flow_m3s, dt_seconds, bifurcation_flow_m3s, runoff_flow_m3s, &
    &   upstream_inflow_m3s, inflow_temperature_k, heat_budget_error_j, &
    &   water_budget_error_m3, unapplied_sensible_heat_j, &
    &   domain_heat_budget_error_j)
    real(kind=JPRB), intent(inout) :: &
    &   water_temperature_k(NSEQALL) ! [K] Cell liquid-water temperature before and after advection.
    real(kind=JPRD), intent(in) :: &
    &   liquid_volume_before_m3(NSEQALL), & ! [m3] Cell liquid-water volume before the water-balance update.
    &   liquid_volume_after_m3(NSEQALL) ! [m3] Cell liquid-water volume after the water-balance update.
    real(kind=JPRB), intent(in) :: &
    &   normal_flow_m3s(NSEQALL), & ! [m3 s-1] Final river-plus-floodplain flow on each normal link.
    &   dt_seconds ! [s] Hydraulic internal time step used for the water-balance update.
    real(kind=JPRB), intent(in), optional :: &
    &   bifurcation_flow_m3s(NPTHOUT), & ! [m3 s-1] Final signed flow on each PTH_UPST-to-PTH_DOWN link.
    &   runoff_flow_m3s(NSEQALL), & ! [m3 s-1] Nonnegative local runoff plus groundwater return flow.
    &   upstream_inflow_m3s(NSEQALL), & ! [m3 s-1] Nonnegative prescribed external upstream inflow.
    &   inflow_temperature_k(NSEQALL) ! [K] Runoff and upstream-inflow temperature, no colder than TMELT.
    real(kind=JPRD), intent(out), optional :: &
    &   heat_budget_error_j(NSEQALL), & ! [J] Cell reconstruction error after accounting for unapplied heat.
    &   water_budget_error_m3(NSEQALL), & ! [m3] Actual minus flow-derived post-update liquid volume.
    &   unapplied_sensible_heat_j(NSEQALL), & ! [J] Heat that cannot be represented because post-update volume is zero.
    &   domain_heat_budget_error_j ! [J] Boundary-aware domain sensible-heat closure error.
    real(kind=JPRD) :: &
    &   sensible_heat_j(NSEQALL), & ! [J] Cell liquid-water sensible heat relative to TMELT.
    &   expected_volume_after_m3(NSEQALL), & ! [m3] Post-update volume reconstructed from all supplied water flows.
    &   local_unapplied_heat_j(NSEQALL), & ! [J] Heat not representable in a zero-volume cell.
    &   d2heatout(NSEQALL), & ! [W] Signed sensible-heat flow on each normal link.
    &   d1pthheatout(NPTHOUT), & ! [W] Signed sensible-heat flow on each bifurcation link.
    &   sOut(NSEQALL), & ! [J] Requested total outgoing sensible heat from each source cell.
    &   srate(NSEQALL) ! [-] Available-heat limiter applied to all outflows from a source cell.
    real(kind=JPRD), parameter :: &
    &   volumetric_heat_capacity_j_m3_k = real(RW, kind=JPRD) * real(CW, kind=JPRD)
    real(kind=JPRD) :: &
    &   domain_expected_heat_j, & ! [J] Initial heat plus external inflow minus mouth outflow.
    &   represented_heat_j ! [J] Heat reconstructed from the returned temperature and volume.
    integer(kind=JPIM) :: &
    &   ipth, iseq
    integer(kind=JPIM), save :: &
    &   iseq0, iseq1
    !$omp threadprivate (iseq0, iseq1)

    ! Preconditions: all volumes and dt_seconds are nonnegative, I1NEXT(1:NSEQRIV)
    ! identifies valid cells, and water_temperature_k is no colder than TMELT.
    if ((present(runoff_flow_m3s) .or. present(upstream_inflow_m3s)) .and. &
    &   .not. present(inflow_temperature_k)) then
        error stop 'Liquid inflow temperature is required with runoff or upstream inflow.'
    endif
    if (present(inflow_temperature_k)) then
        if (.not. liquid_inflow_temperature_is_valid(inflow_temperature_k)) then
            error stop 'Liquid inflow temperature is below the melting point.'
        endif
    endif
    if (present(runoff_flow_m3s)) then
        if (any(runoff_flow_m3s(:) < 0.0_JPRB)) then
            error stop 'Runoff flow must be nonnegative.'
        endif
    endif
    if (present(upstream_inflow_m3s)) then
        if (any(upstream_inflow_m3s(:) < 0.0_JPRB)) then
            error stop 'External upstream inflow must be nonnegative.'
        endif
    endif

    sensible_heat_j(:) = volumetric_heat_capacity_j_m3_k * &
    &   liquid_volume_before_m3(:) * real( &
    &   max(water_temperature_k(:) - TMELT, 0.0_JPRB), kind=JPRD)
    expected_volume_after_m3(:) = liquid_volume_before_m3(:)
    domain_expected_heat_j = sum(sensible_heat_j(:))
    d2heatout(:) = 0.0_JPRD
    d1pthheatout(:) = 0.0_JPRD
    sOut(:) = 0.0_JPRD
#ifndef NoAtom_CMF
    !$omp parallel do
#endif
    do iseq = 1, NSEQRIV
        if (normal_flow_m3s(iseq) >= 0.0_JPRB) then
            iseq0 = iseq
            iseq1 = I1NEXT(iseq)
        else
            iseq0 = I1NEXT(iseq)
            iseq1 = iseq
        endif

        if (normal_flow_m3s(iseq) == 0.0_JPRB) then
            d2heatout(iseq) = 0.0_JPRD
            cycle
        endif

        if (iseq0 < 0) then
            d2heatout(iseq) = 0.0_JPRD
        else
            d2heatout(iseq) = volumetric_heat_capacity_j_m3_k * real( &
            &   water_temperature_k(iseq0) - TMELT, kind=JPRD) * &
            &   real(normal_flow_m3s(iseq), kind=JPRD)
#ifndef NoAtom_CMF
            !$omp atomic
#endif
            sOut(iseq0) = sOut(iseq0) + &
            &   abs(d2heatout(iseq)) * real(dt_seconds, kind=JPRD)
        endif
    enddo
#ifndef NoAtom_CMF
    !$omp end parallel do
#endif

    ! River-mouth positive flow exports source-cell heat. Negative flow imports
    ! water at the mouth-cell temperature (a zero-temperature-gradient boundary).
    !$omp parallel do
    do iseq = NSEQRIV + 1, NSEQALL
        d2heatout(iseq) = volumetric_heat_capacity_j_m3_k * real( &
        &   water_temperature_k(iseq) - TMELT, kind=JPRD) * &
        &   real(normal_flow_m3s(iseq), kind=JPRD)
        if (normal_flow_m3s(iseq) > 0.0_JPRB) then
            sOut(iseq) = sOut(iseq) + &
            &   d2heatout(iseq) * real(dt_seconds, kind=JPRD)
        endif
    enddo
    !$omp end parallel do

    if (present(bifurcation_flow_m3s)) then
#ifndef NoAtom_CMF
        !$omp parallel do
#endif
        do ipth = 1, NPTHOUT
            if (bifurcation_flow_m3s(ipth) >= 0.0_JPRB) then
                iseq0 = PTH_UPST(ipth)
                iseq1 = PTH_DOWN(ipth)
            else
                iseq0 = PTH_DOWN(ipth)
                iseq1 = PTH_UPST(ipth)
            endif

            if (bifurcation_flow_m3s(ipth) == 0.0_JPRB) cycle
            if (iseq0 <= 0 .or. iseq1 <= 0) cycle
            d1pthheatout(ipth) = volumetric_heat_capacity_j_m3_k * real( &
            &   water_temperature_k(iseq0) - TMELT, kind=JPRD) * &
            &   real(bifurcation_flow_m3s(ipth), kind=JPRD)
#ifndef NoAtom_CMF
            !$omp atomic
#endif
            sOut(iseq0) = sOut(iseq0) + &
            &   abs(d1pthheatout(ipth)) * real(dt_seconds, kind=JPRD)
        enddo
#ifndef NoAtom_CMF
        !$omp end parallel do
#endif
    endif

    ! Adjust all outflows from a cell by the same factor if their requested
    ! sensible heat is larger than the heat available in that source cell.
    srate(:) = 1.0_JPRD
    !$omp parallel do
    do iseq = 1, NSEQALL
        if (sOut(iseq) > 0.0_JPRD) then
            srate(iseq) = min(sensible_heat_j(iseq) / sOut(iseq), 1.0_JPRD)
        endif
    enddo
    !$omp end parallel do

    do iseq = 1, NSEQRIV
        if (normal_flow_m3s(iseq) >= 0.0_JPRB) then
            iseq0 = iseq
            iseq1 = I1NEXT(iseq)
        else
            iseq0 = I1NEXT(iseq)
            iseq1 = iseq
        endif

        if (iseq0 > 0) then
            d2heatout(iseq) = d2heatout(iseq) * srate(iseq0)
            sensible_heat_j(iseq0) = max( &
            &   sensible_heat_j(iseq0) - abs(d2heatout(iseq)) * &
            &   real(dt_seconds, kind=JPRD), 0.0_JPRD)
        endif
        if (iseq1 > 0) then
            sensible_heat_j(iseq1) = sensible_heat_j(iseq1) + &
            &   abs(d2heatout(iseq)) * real(dt_seconds, kind=JPRD)
        endif
        if (iseq0 > 0) then
            expected_volume_after_m3(iseq0) = expected_volume_after_m3(iseq0) - &
            &   abs(real(normal_flow_m3s(iseq), kind=JPRD)) * &
            &   real(dt_seconds, kind=JPRD)
        endif
        if (iseq1 > 0) then
            expected_volume_after_m3(iseq1) = expected_volume_after_m3(iseq1) + &
            &   abs(real(normal_flow_m3s(iseq), kind=JPRD)) * &
            &   real(dt_seconds, kind=JPRD)
        endif
    enddo

    do iseq = NSEQRIV + 1, NSEQALL
        if (normal_flow_m3s(iseq) >= 0.0_JPRB) then
            d2heatout(iseq) = d2heatout(iseq) * srate(iseq)
            sensible_heat_j(iseq) = max( &
            &   sensible_heat_j(iseq) - d2heatout(iseq) * &
            &   real(dt_seconds, kind=JPRD), 0.0_JPRD)
            domain_expected_heat_j = domain_expected_heat_j - &
            &   d2heatout(iseq) * real(dt_seconds, kind=JPRD)
            expected_volume_after_m3(iseq) = expected_volume_after_m3(iseq) - &
            &   real(normal_flow_m3s(iseq), kind=JPRD) * &
            &   real(dt_seconds, kind=JPRD)
        else
            sensible_heat_j(iseq) = sensible_heat_j(iseq) + &
            &   abs(d2heatout(iseq)) * real(dt_seconds, kind=JPRD)
            domain_expected_heat_j = domain_expected_heat_j + &
            &   abs(d2heatout(iseq)) * real(dt_seconds, kind=JPRD)
            expected_volume_after_m3(iseq) = expected_volume_after_m3(iseq) + &
            &   abs(real(normal_flow_m3s(iseq), kind=JPRD)) * &
            &   real(dt_seconds, kind=JPRD)
        endif
    enddo

    if (present(bifurcation_flow_m3s)) then
        do ipth = 1, NPTHOUT
            if (bifurcation_flow_m3s(ipth) >= 0.0_JPRB) then
                iseq0 = PTH_UPST(ipth)
                iseq1 = PTH_DOWN(ipth)
            else
                iseq0 = PTH_DOWN(ipth)
                iseq1 = PTH_UPST(ipth)
            endif
            if (iseq0 <= 0 .or. iseq1 <= 0) cycle

            d1pthheatout(ipth) = d1pthheatout(ipth) * srate(iseq0)
            sensible_heat_j(iseq0) = max( &
            &   sensible_heat_j(iseq0) - abs(d1pthheatout(ipth)) * &
            &   real(dt_seconds, kind=JPRD), 0.0_JPRD)
            sensible_heat_j(iseq1) = sensible_heat_j(iseq1) + &
            &   abs(d1pthheatout(ipth)) * real(dt_seconds, kind=JPRD)
            expected_volume_after_m3(iseq0) = expected_volume_after_m3(iseq0) - &
            &   abs(real(bifurcation_flow_m3s(ipth), kind=JPRD)) * &
            &   real(dt_seconds, kind=JPRD)
            expected_volume_after_m3(iseq1) = expected_volume_after_m3(iseq1) + &
            &   abs(real(bifurcation_flow_m3s(ipth), kind=JPRD)) * &
            &   real(dt_seconds, kind=JPRD)
        enddo
    endif

    if (present(runoff_flow_m3s)) then
        sensible_heat_j(:) = sensible_heat_j(:) + &
        &   volumetric_heat_capacity_j_m3_k * real(runoff_flow_m3s(:), kind=JPRD) * &
        &   real(inflow_temperature_k(:) - TMELT, kind=JPRD) * &
        &   real(dt_seconds, kind=JPRD)
        expected_volume_after_m3(:) = expected_volume_after_m3(:) + &
        &   real(runoff_flow_m3s(:), kind=JPRD) * real(dt_seconds, kind=JPRD)
        domain_expected_heat_j = domain_expected_heat_j + &
        &   volumetric_heat_capacity_j_m3_k * sum( &
        &   real(runoff_flow_m3s(:), kind=JPRD) * &
        &   real(inflow_temperature_k(:) - TMELT, kind=JPRD)) * &
        &   real(dt_seconds, kind=JPRD)
    endif
    if (present(upstream_inflow_m3s)) then
        sensible_heat_j(:) = sensible_heat_j(:) + &
        &   volumetric_heat_capacity_j_m3_k * real(upstream_inflow_m3s(:), kind=JPRD) * &
        &   real(inflow_temperature_k(:) - TMELT, kind=JPRD) * &
        &   real(dt_seconds, kind=JPRD)
        expected_volume_after_m3(:) = expected_volume_after_m3(:) + &
        &   real(upstream_inflow_m3s(:), kind=JPRD) * real(dt_seconds, kind=JPRD)
        domain_expected_heat_j = domain_expected_heat_j + &
        &   volumetric_heat_capacity_j_m3_k * sum( &
        &   real(upstream_inflow_m3s(:), kind=JPRD) * &
        &   real(inflow_temperature_k(:) - TMELT, kind=JPRD)) * &
        &   real(dt_seconds, kind=JPRD)
    endif

    local_unapplied_heat_j(:) = 0.0_JPRD
    !$omp parallel do
    do iseq = 1, NSEQALL
        if (liquid_volume_after_m3(iseq) > 0.0_JPRD) then
            water_temperature_k(iseq) = TMELT + real( &
            &   sensible_heat_j(iseq) / (volumetric_heat_capacity_j_m3_k * &
            &   liquid_volume_after_m3(iseq)), kind=JPRB)
        else
            water_temperature_k(iseq) = TMELT
            local_unapplied_heat_j(iseq) = sensible_heat_j(iseq)
        endif
    enddo
    !$omp end parallel do

    if (present(unapplied_sensible_heat_j)) then
        unapplied_sensible_heat_j(:) = local_unapplied_heat_j(:)
    endif
    if (present(water_budget_error_m3)) then
        water_budget_error_m3(:) = liquid_volume_after_m3(:) - &
        &   expected_volume_after_m3(:)
    endif
    if (present(heat_budget_error_j)) then
        heat_budget_error_j(:) = sensible_heat_j(:) - &
        &   volumetric_heat_capacity_j_m3_k * liquid_volume_after_m3(:) * &
        &   real(water_temperature_k(:) - TMELT, kind=JPRD) - &
        &   local_unapplied_heat_j(:)
    endif
    if (present(domain_heat_budget_error_j)) then
        represented_heat_j = volumetric_heat_capacity_j_m3_k * sum( &
        &   liquid_volume_after_m3(:) * &
        &   real(water_temperature_k(:) - TMELT, kind=JPRD))
        domain_heat_budget_error_j = domain_expected_heat_j - &
        &   represented_heat_j - sum(local_unapplied_heat_j(:))
    endif
end subroutine advect_river_water_sensible_heat


pure function liquid_inflow_temperature_is_valid( &
    &   inflow_temperature_k) result(is_valid)
    real(kind=JPRB), intent(in) :: &
    &   inflow_temperature_k(:) ! [K] Candidate liquid inflow temperature.
    logical :: &
    &   is_valid ! [-] True when every liquid inflow is at or above TMELT.

    is_valid = all(inflow_temperature_k(:) >= TMELT)
end function liquid_inflow_temperature_is_valid
#endif
end module river_water_advection_mod
