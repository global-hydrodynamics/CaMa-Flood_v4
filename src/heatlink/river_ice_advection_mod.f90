module river_ice_advection_mod
#ifdef heatlink
    use PARKIND1, only: &
    &   JPIM, JPRB, JPRD
    use YOS_CMF_MAP, only: &
    &   I1NEXT, NSEQALL, NSEQRIV, &
    &   D2RIVLEN, D2RIVWTH, &
    &   NPTHOUT, PTH_UPST, PTH_DOWN
    implicit none
    private
    public :: &
    &   advect_river_surface_ice, diagnose_surface_ice_transport_fraction

    real(kind=JPRD), parameter :: &
    &   MINIMUM_MOBILE_WATER_DEPTH_M = 0.01_JPRD, & ! [m] TCHOIR river-ice mobility threshold.
    &   FULLY_FROZEN_ICE_VELOCITY_FRACTION = 0.5_JPRD ! [-] TCHOIR ice/water velocity ratio.

contains

subroutine advect_river_surface_ice( &
    &   surface_ice_volume_m3, surface_ice_fraction, &
    &   liquid_volume_before_m3, normal_flow_m3s, dt_seconds, &
    &   bifurcation_flow_m3s, ice_budget_error_m3, &
    &   domain_ice_budget_error_m3)
    real(kind=JPRB), intent(inout) :: &
    &   surface_ice_volume_m3(NSEQALL) ! [m3] Mobile water-surface ice before and after advection.
    real(kind=JPRB), intent(in) :: &
    &   surface_ice_fraction(NSEQALL) ! [-] Water-surface ice-covered fraction, from zero to one.
    real(kind=JPRD), intent(in) :: &
    &   liquid_volume_before_m3(NSEQALL) ! [m3] Liquid-water volume before the water-balance update.
    real(kind=JPRB), intent(in) :: &
    &   normal_flow_m3s(NSEQALL), & ! [m3 s-1] Final river-plus-floodplain flow on each normal link.
    &   dt_seconds ! [s] Hydraulic internal time step used for the water-balance update.
    real(kind=JPRB), intent(in), optional :: &
    &   bifurcation_flow_m3s(NPTHOUT) ! [m3 s-1] Final signed flow on each PTH_UPST-to-PTH_DOWN link.
    real(kind=JPRD), intent(out), optional :: &
    &   ice_budget_error_m3(NSEQALL), & ! [m3] Expected minus represented mobile surface-ice volume.
    &   domain_ice_budget_error_m3 ! [m3] Boundary-aware domain surface-ice closure error.
    real(kind=JPRD) :: &
    &   surface_ice_storage_m3(NSEQALL), & ! [m3] Double-precision working copy of mobile surface ice.
    &   expected_surface_ice_volume_m3(NSEQALL), & ! [m3] Surface ice reconstructed from applied link flows.
    &   d2iceout(NSEQALL), & ! [m3 s-1] Signed surface-ice flow on each normal link.
    &   d1pthiceout(NPTHOUT), & ! [m3 s-1] Signed surface-ice flow on each bifurcation link.
    &   sOut(NSEQALL), & ! [m3] Requested total outgoing surface ice from each source cell.
    &   srate(NSEQALL), & ! [-] Available-ice limiter applied to all outflows from a source cell.
    &   transport_fraction, & ! [-] Source surface-ice fraction requested by the current link.
    &   ice_velocity_fraction, & ! [-] TCHOIR ice velocity divided by water velocity.
    &   minimum_mobile_liquid_volume_m3, & ! [m3] Liquid volume below which TCHOIR keeps ice immobile.
    &   domain_expected_surface_ice_volume_m3 ! [m3] Initial ice minus river-mouth export.
    integer(kind=JPIM) :: &
    &   ipth, iseq
    integer(kind=JPIM), save :: &
    &   iseq0, iseq1
    !$omp threadprivate (iseq0, iseq1)

    ! Preconditions: surface ice, liquid volume, and dt_seconds are nonnegative,
    ! and I1NEXT(1:NSEQRIV) identifies valid normal-link destination cells.
    surface_ice_storage_m3(:) = real(max(surface_ice_volume_m3(:), 0.0_JPRB), kind=JPRD)
    expected_surface_ice_volume_m3(:) = surface_ice_storage_m3(:)
    domain_expected_surface_ice_volume_m3 = sum(surface_ice_storage_m3(:))
    d2iceout(:) = 0.0_JPRD
    d1pthiceout(:) = 0.0_JPRD
    sOut(:) = 0.0_JPRD
#ifndef NoAtom_CMF
    !$omp parallel do private(transport_fraction, ice_velocity_fraction, minimum_mobile_liquid_volume_m3)
#endif
    do iseq = 1, NSEQRIV
        if (normal_flow_m3s(iseq) >= 0.0_JPRB) then
            iseq0 = iseq
            iseq1 = I1NEXT(iseq)
        else
            iseq0 = I1NEXT(iseq)
            iseq1 = iseq
        endif

        if (normal_flow_m3s(iseq) == 0.0_JPRB .or. dt_seconds <= 0.0_JPRB) then
            d2iceout(iseq) = 0.0_JPRD
            cycle
        endif

        if (iseq0 <= 0) then
            d2iceout(iseq) = 0.0_JPRD
        else
            minimum_mobile_liquid_volume_m3 = MINIMUM_MOBILE_WATER_DEPTH_M * &
            &   real(D2RIVWTH(iseq0, 1), kind=JPRD) * &
            &   real(D2RIVLEN(iseq0, 1), kind=JPRD)
            if (liquid_volume_before_m3(iseq0) < minimum_mobile_liquid_volume_m3) then
                d2iceout(iseq) = 0.0_JPRD
                cycle
            endif

            transport_fraction = diagnose_surface_ice_transport_fraction( &
            &   liquid_volume_before_m3(iseq0), &
            &   abs(real(normal_flow_m3s(iseq), kind=JPRD)) * &
            &   real(dt_seconds, kind=JPRD))

            ! TCHOIR assumes that surface ice moves with water except across a
            ! fully frozen source or receiving cell, where its velocity is halved.
            ice_velocity_fraction = 1.0_JPRD
            if (surface_ice_fraction(iseq0) == 1.0_JPRB) then
                ice_velocity_fraction = min( &
                &   FULLY_FROZEN_ICE_VELOCITY_FRACTION, ice_velocity_fraction)
            endif
            if (iseq1 > 0) then
                if (surface_ice_fraction(iseq1) == 1.0_JPRB) then
                    ice_velocity_fraction = min( &
                    &   FULLY_FROZEN_ICE_VELOCITY_FRACTION, ice_velocity_fraction)
                endif
            endif
            d2iceout(iseq) = sign( &
            &   surface_ice_storage_m3(iseq0) * transport_fraction * &
            &   ice_velocity_fraction / &
            &   real(dt_seconds, kind=JPRD), &
            &   real(normal_flow_m3s(iseq), kind=JPRD))
#ifndef NoAtom_CMF
            !$omp atomic
#endif
            sOut(iseq0) = sOut(iseq0) + &
            &   abs(d2iceout(iseq)) * real(dt_seconds, kind=JPRD)
        endif
    enddo
#ifndef NoAtom_CMF
    !$omp end parallel do
#endif

    ! River-mouth positive flow exports surface ice. As in TCHOIR, negative
    ! mouth flow imports liquid water but no surface ice from the ocean.
    !$omp parallel do private(transport_fraction, ice_velocity_fraction, minimum_mobile_liquid_volume_m3)
    do iseq = NSEQRIV + 1, NSEQALL
        if (normal_flow_m3s(iseq) <= 0.0_JPRB .or. dt_seconds <= 0.0_JPRB) cycle
        minimum_mobile_liquid_volume_m3 = MINIMUM_MOBILE_WATER_DEPTH_M * &
        &   real(D2RIVWTH(iseq, 1), kind=JPRD) * &
        &   real(D2RIVLEN(iseq, 1), kind=JPRD)
        if (liquid_volume_before_m3(iseq) < minimum_mobile_liquid_volume_m3) cycle

        transport_fraction = diagnose_surface_ice_transport_fraction( &
        &   liquid_volume_before_m3(iseq), &
        &   real(normal_flow_m3s(iseq), kind=JPRD) * real(dt_seconds, kind=JPRD))
        ice_velocity_fraction = 1.0_JPRD
        if (surface_ice_fraction(iseq) == 1.0_JPRB) then
            ice_velocity_fraction = min( &
            &   FULLY_FROZEN_ICE_VELOCITY_FRACTION, ice_velocity_fraction)
        endif
        d2iceout(iseq) = surface_ice_storage_m3(iseq) * transport_fraction * &
        &   ice_velocity_fraction / real(dt_seconds, kind=JPRD)
        sOut(iseq) = sOut(iseq) + d2iceout(iseq) * real(dt_seconds, kind=JPRD)
    enddo
    !$omp end parallel do

    if (present(bifurcation_flow_m3s)) then
#ifndef NoAtom_CMF
        !$omp parallel do private(transport_fraction, ice_velocity_fraction, minimum_mobile_liquid_volume_m3)
#endif
        do ipth = 1, NPTHOUT
            if (bifurcation_flow_m3s(ipth) >= 0.0_JPRB) then
                iseq0 = PTH_UPST(ipth)
                iseq1 = PTH_DOWN(ipth)
            else
                iseq0 = PTH_DOWN(ipth)
                iseq1 = PTH_UPST(ipth)
            endif
            if (bifurcation_flow_m3s(ipth) == 0.0_JPRB .or. dt_seconds <= 0.0_JPRB) cycle
            if (iseq0 <= 0 .or. iseq1 <= 0) cycle

            minimum_mobile_liquid_volume_m3 = MINIMUM_MOBILE_WATER_DEPTH_M * &
            &   real(D2RIVWTH(iseq0, 1), kind=JPRD) * &
            &   real(D2RIVLEN(iseq0, 1), kind=JPRD)
            if (liquid_volume_before_m3(iseq0) < minimum_mobile_liquid_volume_m3) cycle

            transport_fraction = diagnose_surface_ice_transport_fraction( &
            &   liquid_volume_before_m3(iseq0), &
            &   abs(real(bifurcation_flow_m3s(ipth), kind=JPRD)) * &
            &   real(dt_seconds, kind=JPRD))
            ice_velocity_fraction = 1.0_JPRD
            if (surface_ice_fraction(iseq0) == 1.0_JPRB) then
                ice_velocity_fraction = min( &
                &   FULLY_FROZEN_ICE_VELOCITY_FRACTION, ice_velocity_fraction)
            endif
            if (surface_ice_fraction(iseq1) == 1.0_JPRB) then
                ice_velocity_fraction = min( &
                &   FULLY_FROZEN_ICE_VELOCITY_FRACTION, ice_velocity_fraction)
            endif
            d1pthiceout(ipth) = sign( &
            &   surface_ice_storage_m3(iseq0) * transport_fraction * &
            &   ice_velocity_fraction / real(dt_seconds, kind=JPRD), &
            &   real(bifurcation_flow_m3s(ipth), kind=JPRD))
#ifndef NoAtom_CMF
            !$omp atomic
#endif
            sOut(iseq0) = sOut(iseq0) + &
            &   abs(d1pthiceout(ipth)) * real(dt_seconds, kind=JPRD)
        enddo
#ifndef NoAtom_CMF
        !$omp end parallel do
#endif
    endif

    ! Adjust all outflows from a cell by the same factor if their requested
    ! surface-ice volume is larger than the mobile ice available in that cell.
    srate(:) = 1.0_JPRD
    !$omp parallel do
    do iseq = 1, NSEQALL
        if (sOut(iseq) > 0.0_JPRD) then
            srate(iseq) = min(surface_ice_storage_m3(iseq) / sOut(iseq), 1.0_JPRD)
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
            d2iceout(iseq) = d2iceout(iseq) * srate(iseq0)
            surface_ice_storage_m3(iseq0) = max( &
            &   surface_ice_storage_m3(iseq0) - abs(d2iceout(iseq)) * &
            &   real(dt_seconds, kind=JPRD), 0.0_JPRD)
            expected_surface_ice_volume_m3(iseq0) = &
            &   expected_surface_ice_volume_m3(iseq0) - &
            &   abs(d2iceout(iseq)) * real(dt_seconds, kind=JPRD)
        endif
        if (iseq1 > 0) then
            surface_ice_storage_m3(iseq1) = surface_ice_storage_m3(iseq1) + &
            &   abs(d2iceout(iseq)) * real(dt_seconds, kind=JPRD)
            expected_surface_ice_volume_m3(iseq1) = &
            &   expected_surface_ice_volume_m3(iseq1) + &
            &   abs(d2iceout(iseq)) * real(dt_seconds, kind=JPRD)
        endif
    enddo

    do iseq = NSEQRIV + 1, NSEQALL
        if (normal_flow_m3s(iseq) <= 0.0_JPRB) cycle
        d2iceout(iseq) = d2iceout(iseq) * srate(iseq)
        surface_ice_storage_m3(iseq) = max( &
        &   surface_ice_storage_m3(iseq) - d2iceout(iseq) * &
        &   real(dt_seconds, kind=JPRD), 0.0_JPRD)
        expected_surface_ice_volume_m3(iseq) = &
        &   expected_surface_ice_volume_m3(iseq) - &
        &   d2iceout(iseq) * real(dt_seconds, kind=JPRD)
        domain_expected_surface_ice_volume_m3 = &
        &   domain_expected_surface_ice_volume_m3 - &
        &   d2iceout(iseq) * real(dt_seconds, kind=JPRD)
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

            d1pthiceout(ipth) = d1pthiceout(ipth) * srate(iseq0)
            surface_ice_storage_m3(iseq0) = max( &
            &   surface_ice_storage_m3(iseq0) - abs(d1pthiceout(ipth)) * &
            &   real(dt_seconds, kind=JPRD), 0.0_JPRD)
            expected_surface_ice_volume_m3(iseq0) = &
            &   expected_surface_ice_volume_m3(iseq0) - &
            &   abs(d1pthiceout(ipth)) * real(dt_seconds, kind=JPRD)
            surface_ice_storage_m3(iseq1) = surface_ice_storage_m3(iseq1) + &
            &   abs(d1pthiceout(ipth)) * real(dt_seconds, kind=JPRD)
            expected_surface_ice_volume_m3(iseq1) = &
            &   expected_surface_ice_volume_m3(iseq1) + &
            &   abs(d1pthiceout(ipth)) * real(dt_seconds, kind=JPRD)
        enddo
    endif

    surface_ice_volume_m3(:) = real(surface_ice_storage_m3(:), kind=JPRB)
    if (present(ice_budget_error_m3)) then
        ice_budget_error_m3(:) = expected_surface_ice_volume_m3(:) - &
        &   real(surface_ice_volume_m3(:), kind=JPRD)
    endif
    if (present(domain_ice_budget_error_m3)) then
        domain_ice_budget_error_m3 = domain_expected_surface_ice_volume_m3 - &
        &   sum(real(surface_ice_volume_m3(:), kind=JPRD))
    endif
end subroutine advect_river_surface_ice


pure elemental function diagnose_surface_ice_transport_fraction( &
    &   liquid_volume_m3, transported_water_volume_m3) result(transport_fraction)
    real(kind=JPRD), intent(in) :: &
    &   liquid_volume_m3, & ! [m3] Source-cell liquid-water volume before transport.
    &   transported_water_volume_m3 ! [m3] Nonnegative water volume transported on one link.
    real(kind=JPRD) :: &
    &   transport_fraction ! [-] Fraction of source surface ice requested before the total-outflow limiter.

    transport_fraction = 0.0_JPRD
    if (liquid_volume_m3 <= 0.0_JPRD) return
    if (transported_water_volume_m3 <= 0.0_JPRD) return
    ! Match TCHOIR's iceflow = icevol * waterflow / water-storage relation.
    ! This requested fraction may exceed one; srate subsequently limits the
    ! combined outflow from all links to the mobile ice available in the cell.
    transport_fraction = transported_water_volume_m3 / liquid_volume_m3
end function diagnose_surface_ice_transport_fraction
#endif
end module river_ice_advection_mod
