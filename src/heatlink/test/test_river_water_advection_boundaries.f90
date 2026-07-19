program test_river_water_advection_boundaries
    use PARKIND1, only: &
    &   JPIM, JPRB, JPRD
    use YOS_CMF_MAP, only: &
    &   I1NEXT, NSEQALL, NSEQRIV, NPTHOUT, PTH_UPST, PTH_DOWN
    use phys_const_mod, only: &
    &   CW, RW, TMELT
    use river_water_advection_mod, only: &
    &   advect_river_water_sensible_heat, liquid_inflow_temperature_is_valid
    implicit none

    call test_runoff_and_upstream_inflow_to_dry_cell()
    call test_river_mouth_outflow_and_reverse_flow()
    call test_bifurcation_forward_and_reverse_flow()
    call test_bifurcation_and_normal_outflow_share_limiter()
    call test_cold_liquid_inflow_is_rejected_by_contract()

    write(*, '(a)') '[ALL TESTS PASSED] test_river_water_advection_boundaries'

contains

subroutine test_runoff_and_upstream_inflow_to_dry_cell()
    real(kind=JPRB) :: &
    &   water_temperature_k(3), normal_flow_m3s(3), &
    &   runoff_flow_m3s(3), upstream_inflow_m3s(3), inflow_temperature_k(3)
    real(kind=JPRD) :: &
    &   liquid_volume_before_m3(3), liquid_volume_after_m3(3), expected_heat_j

    call set_three_cell_topology(0_JPIM)
    water_temperature_k(:) = [TMELT, TMELT + 2.0_JPRB, TMELT]
    liquid_volume_before_m3(:) = [0.0_JPRD, 5.0_JPRD, 0.0_JPRD]
    liquid_volume_after_m3(:) = [3.0_JPRD, 5.0_JPRD, 0.0_JPRD]
    normal_flow_m3s(:) = 0.0_JPRB
    runoff_flow_m3s(:) = [2.0_JPRB, 0.0_JPRB, 0.0_JPRB]
    upstream_inflow_m3s(:) = [1.0_JPRB, 0.0_JPRB, 0.0_JPRB]
    inflow_temperature_k(:) = TMELT + 6.0_JPRB
    expected_heat_j = total_sensible_heat_j( &
    &   water_temperature_k, liquid_volume_before_m3) + &
    &   volumetric_heat_capacity() * 3.0_JPRD * 6.0_JPRD

    call advect_river_water_sensible_heat( &
    &   water_temperature_k, liquid_volume_before_m3, liquid_volume_after_m3, &
    &   normal_flow_m3s, 1.0_JPRB, &
    &   runoff_flow_m3s=runoff_flow_m3s, &
    &   upstream_inflow_m3s=upstream_inflow_m3s, &
    &   inflow_temperature_k=inflow_temperature_k)

    call assert_close(water_temperature_k(1), TMELT + 6.0_JPRB, &
    &   'runoff and upstream inflow temperature in dry cell [K]')
    call assert_heat(expected_heat_j, water_temperature_k, liquid_volume_after_m3, &
    &   'runoff and upstream inflow')
end subroutine test_runoff_and_upstream_inflow_to_dry_cell


subroutine test_river_mouth_outflow_and_reverse_flow()
    real(kind=JPRB) :: &
    &   water_temperature_k(3), normal_flow_m3s(3)
    real(kind=JPRD) :: &
    &   liquid_volume_before_m3(3), liquid_volume_after_m3(3), &
    &   initial_heat_j, expected_heat_j

    call set_three_cell_topology(0_JPIM)
    water_temperature_k(:) = [TMELT, TMELT, TMELT + 8.0_JPRB]
    liquid_volume_before_m3(:) = [10.0_JPRD, 10.0_JPRD, 10.0_JPRD]
    liquid_volume_after_m3(:) = [10.0_JPRD, 10.0_JPRD, 8.0_JPRD]
    normal_flow_m3s(:) = [0.0_JPRB, 0.0_JPRB, 2.0_JPRB]
    initial_heat_j = total_sensible_heat_j( &
    &   water_temperature_k, liquid_volume_before_m3)
    expected_heat_j = initial_heat_j - &
    &   volumetric_heat_capacity() * 2.0_JPRD * 8.0_JPRD

    call advect_river_water_sensible_heat( &
    &   water_temperature_k, liquid_volume_before_m3, liquid_volume_after_m3, &
    &   normal_flow_m3s, 1.0_JPRB)

    call assert_close(water_temperature_k(3), TMELT + 8.0_JPRB, &
    &   'river-mouth outflow preserves source temperature [K]')
    call assert_heat(expected_heat_j, water_temperature_k, liquid_volume_after_m3, &
    &   'river-mouth domain heat loss')

    water_temperature_k(:) = [TMELT, TMELT, TMELT + 8.0_JPRB]
    liquid_volume_before_m3(:) = [10.0_JPRD, 10.0_JPRD, 10.0_JPRD]
    liquid_volume_after_m3(:) = [10.0_JPRD, 10.0_JPRD, 12.0_JPRD]
    normal_flow_m3s(:) = [0.0_JPRB, 0.0_JPRB, -2.0_JPRB]
    initial_heat_j = total_sensible_heat_j( &
    &   water_temperature_k, liquid_volume_before_m3)
    expected_heat_j = initial_heat_j + &
    &   volumetric_heat_capacity() * 2.0_JPRD * 8.0_JPRD

    call advect_river_water_sensible_heat( &
    &   water_temperature_k, liquid_volume_before_m3, liquid_volume_after_m3, &
    &   normal_flow_m3s, 1.0_JPRB)

    call assert_close(water_temperature_k(3), TMELT + 8.0_JPRB, &
    &   'zero-gradient river-mouth reverse-flow temperature [K]')
    call assert_heat(expected_heat_j, water_temperature_k, liquid_volume_after_m3, &
    &   'river-mouth reverse-flow heat gain')
end subroutine test_river_mouth_outflow_and_reverse_flow


subroutine test_bifurcation_forward_and_reverse_flow()
    real(kind=JPRB) :: &
    &   water_temperature_k(3), normal_flow_m3s(3), bifurcation_flow_m3s(1)
    real(kind=JPRD) :: &
    &   liquid_volume_before_m3(3), liquid_volume_after_m3(3), initial_heat_j

    call set_three_cell_topology(1_JPIM)
    PTH_UPST(1) = 1_JPIM
    PTH_DOWN(1) = 3_JPIM
    water_temperature_k(:) = [TMELT + 9.0_JPRB, TMELT, TMELT]
    liquid_volume_before_m3(:) = 10.0_JPRD
    liquid_volume_after_m3(:) = [8.0_JPRD, 10.0_JPRD, 12.0_JPRD]
    normal_flow_m3s(:) = 0.0_JPRB
    bifurcation_flow_m3s(:) = 2.0_JPRB
    initial_heat_j = total_sensible_heat_j( &
    &   water_temperature_k, liquid_volume_before_m3)

    call advect_river_water_sensible_heat( &
    &   water_temperature_k, liquid_volume_before_m3, liquid_volume_after_m3, &
    &   normal_flow_m3s, 1.0_JPRB, &
    &   bifurcation_flow_m3s=bifurcation_flow_m3s)

    call assert_close(water_temperature_k(1), TMELT + 9.0_JPRB, &
    &   'forward bifurcation source temperature [K]')
    call assert_close(water_temperature_k(3), TMELT + 1.5_JPRB, &
    &   'forward bifurcation receiving temperature [K]')
    call assert_heat(initial_heat_j, water_temperature_k, liquid_volume_after_m3, &
    &   'forward bifurcation closed system')

    water_temperature_k(:) = [TMELT, TMELT, TMELT + 9.0_JPRB]
    liquid_volume_after_m3(:) = [12.0_JPRD, 10.0_JPRD, 8.0_JPRD]
    bifurcation_flow_m3s(:) = -2.0_JPRB
    initial_heat_j = total_sensible_heat_j( &
    &   water_temperature_k, liquid_volume_before_m3)

    call advect_river_water_sensible_heat( &
    &   water_temperature_k, liquid_volume_before_m3, liquid_volume_after_m3, &
    &   normal_flow_m3s, 1.0_JPRB, &
    &   bifurcation_flow_m3s=bifurcation_flow_m3s)

    call assert_close(water_temperature_k(1), TMELT + 1.5_JPRB, &
    &   'reverse bifurcation receiving temperature [K]')
    call assert_close(water_temperature_k(3), TMELT + 9.0_JPRB, &
    &   'reverse bifurcation source temperature [K]')
    call assert_heat(initial_heat_j, water_temperature_k, liquid_volume_after_m3, &
    &   'reverse bifurcation closed system')
end subroutine test_bifurcation_forward_and_reverse_flow


subroutine test_bifurcation_and_normal_outflow_share_limiter()
    real(kind=JPRB) :: &
    &   water_temperature_k(3), normal_flow_m3s(3), bifurcation_flow_m3s(1)
    real(kind=JPRD) :: &
    &   liquid_volume_before_m3(3), liquid_volume_after_m3(3), initial_heat_j

    call set_three_cell_topology(1_JPIM)
    PTH_UPST(1) = 1_JPIM
    PTH_DOWN(1) = 3_JPIM
    water_temperature_k(:) = [TMELT + 12.0_JPRB, TMELT, TMELT]
    liquid_volume_before_m3(:) = 10.0_JPRD
    liquid_volume_after_m3(:) = [0.0_JPRD, 15.0_JPRD, 15.0_JPRD]
    normal_flow_m3s(:) = [6.0_JPRB, 0.0_JPRB, 0.0_JPRB]
    bifurcation_flow_m3s(:) = 6.0_JPRB
    initial_heat_j = total_sensible_heat_j( &
    &   water_temperature_k, liquid_volume_before_m3)

    call advect_river_water_sensible_heat( &
    &   water_temperature_k, liquid_volume_before_m3, liquid_volume_after_m3, &
    &   normal_flow_m3s, 1.0_JPRB, &
    &   bifurcation_flow_m3s=bifurcation_flow_m3s)

    call assert_close(water_temperature_k(1), TMELT, &
    &   'shared-limiter source becomes dry [K]')
    call assert_close(water_temperature_k(2), TMELT + 4.0_JPRB, &
    &   'shared-limiter normal-link temperature [K]')
    call assert_close(water_temperature_k(3), TMELT + 4.0_JPRB, &
    &   'shared-limiter bifurcation temperature [K]')
    call assert_heat(initial_heat_j, water_temperature_k, liquid_volume_after_m3, &
    &   'normal and bifurcation shared limiter')
end subroutine test_bifurcation_and_normal_outflow_share_limiter


subroutine test_cold_liquid_inflow_is_rejected_by_contract()
    real(kind=JPRB) :: &
    &   inflow_temperature_k(3)

    inflow_temperature_k(:) = [TMELT, TMELT - 1.0e-6_JPRB, TMELT + 1.0_JPRB]
    call assert_true(.not. liquid_inflow_temperature_is_valid(inflow_temperature_k), &
    &   'cold liquid inflow violates the advection contract')
    inflow_temperature_k(2) = TMELT
    call assert_true(liquid_inflow_temperature_is_valid(inflow_temperature_k), &
    &   'melting-point liquid inflow satisfies the advection contract')
end subroutine test_cold_liquid_inflow_is_rejected_by_contract


subroutine set_three_cell_topology(number_of_bifurcations)
    integer(kind=JPIM), intent(in) :: &
    &   number_of_bifurcations

    if (allocated(I1NEXT)) deallocate(I1NEXT)
    if (allocated(PTH_UPST)) deallocate(PTH_UPST)
    if (allocated(PTH_DOWN)) deallocate(PTH_DOWN)
    NSEQALL = 3_JPIM
    NSEQRIV = 2_JPIM
    NPTHOUT = number_of_bifurcations
    allocate(I1NEXT(NSEQALL), PTH_UPST(NPTHOUT), PTH_DOWN(NPTHOUT))
    I1NEXT(:) = [2_JPIM, 3_JPIM, -9_JPIM]
end subroutine set_three_cell_topology


pure function volumetric_heat_capacity() result(heat_capacity_j_m3_k)
    real(kind=JPRD) :: &
    &   heat_capacity_j_m3_k

    heat_capacity_j_m3_k = real(RW, kind=JPRD) * real(CW, kind=JPRD)
end function volumetric_heat_capacity


function total_sensible_heat_j( &
    &   water_temperature_k, liquid_volume_m3) result(sensible_heat_j)
    real(kind=JPRB), intent(in) :: &
    &   water_temperature_k(:)
    real(kind=JPRD), intent(in) :: &
    &   liquid_volume_m3(:)
    real(kind=JPRD) :: &
    &   sensible_heat_j

    sensible_heat_j = volumetric_heat_capacity() * sum( &
    &   liquid_volume_m3(:) * real( &
    &   water_temperature_k(:) - TMELT, kind=JPRD))
end function total_sensible_heat_j


subroutine assert_heat( &
    &   expected_heat_j, water_temperature_k, liquid_volume_m3, label)
    real(kind=JPRD), intent(in) :: &
    &   expected_heat_j, liquid_volume_m3(:)
    real(kind=JPRB), intent(in) :: &
    &   water_temperature_k(:)
    character(len=*), intent(in) :: &
    &   label
    real(kind=JPRD) :: &
    &   actual_heat_j, tolerance_j

    actual_heat_j = total_sensible_heat_j(water_temperature_k, liquid_volume_m3)
    tolerance_j = 1.0e-12_JPRD * max(1.0_JPRD, abs(expected_heat_j))
    if (abs(actual_heat_j - expected_heat_j) <= tolerance_j) return
    write(*, '(a)') '[TEST FAILED] '//trim(label)//' sensible-heat budget [J]'
    write(*, '(a,es24.15)') '  actual   = ', actual_heat_j
    write(*, '(a,es24.15)') '  expected = ', expected_heat_j
    error stop 1
end subroutine assert_heat


subroutine assert_close(actual_value, expected_value, label)
    real(kind=JPRB), intent(in) :: &
    &   actual_value, expected_value
    character(len=*), intent(in) :: &
    &   label
    real(kind=JPRB) :: &
    &   tolerance

    tolerance = 1.0e-12_JPRB * max(1.0_JPRB, abs(expected_value))
    if (abs(actual_value - expected_value) <= tolerance) return
    write(*, '(a)') '[TEST FAILED] '//trim(label)
    write(*, '(a,es24.15)') '  actual   = ', actual_value
    write(*, '(a,es24.15)') '  expected = ', expected_value
    error stop 1
end subroutine assert_close


subroutine assert_true(condition, label)
    logical, intent(in) :: &
    &   condition
    character(len=*), intent(in) :: &
    &   label

    if (condition) return
    write(*, '(a)') '[TEST FAILED] '//trim(label)
    error stop 1
end subroutine assert_true

end program test_river_water_advection_boundaries
