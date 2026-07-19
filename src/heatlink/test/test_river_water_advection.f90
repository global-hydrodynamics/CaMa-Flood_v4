program test_river_water_advection
    use PARKIND1, only: &
    &   JPIM, JPRB, JPRD
    use YOS_CMF_MAP, only: &
    &   I1NEXT, NSEQALL, NSEQRIV
    use phys_const_mod, only: &
    &   CW, RW, TMELT
    use river_water_advection_mod, only: &
    &   advect_river_water_sensible_heat
    implicit none

    call test_forward_temperature_pulse()
    call test_uniform_temperature_is_unchanged()
    call test_reverse_and_zero_flow()
    call test_total_outflow_is_limited_by_available_heat()
    call test_inflow_to_dry_cell()
    call test_zero_and_tiny_liquid_volume()

    write(*, '(a)') '[ALL TESTS PASSED] test_river_water_advection'

contains

subroutine test_forward_temperature_pulse()
    real(kind=JPRB) :: &
    &   water_temperature_k(3), normal_flow_m3s(3)
    real(kind=JPRD) :: &
    &   liquid_volume_before_m3(3), liquid_volume_after_m3(3), initial_heat_j

    call set_three_cell_topology()
    water_temperature_k(:) = [TMELT + 10.0_JPRB, TMELT, TMELT]
    liquid_volume_before_m3(:) = [10.0_JPRD, 10.0_JPRD, 10.0_JPRD]
    liquid_volume_after_m3(:) = [9.0_JPRD, 11.0_JPRD, 10.0_JPRD]
    normal_flow_m3s(:) = [1.0_JPRB, 0.0_JPRB, 0.0_JPRB]
    initial_heat_j = total_sensible_heat_j( &
    &   water_temperature_k, liquid_volume_before_m3)

    call advect_river_water_sensible_heat( &
    &   water_temperature_k, liquid_volume_before_m3, liquid_volume_after_m3, &
    &   normal_flow_m3s, 1.0_JPRB)

    call assert_close(water_temperature_k(1), TMELT + 10.0_JPRB, &
    &   'forward pulse source temperature [K]')
    call assert_close(water_temperature_k(2), TMELT + 10.0_JPRB / 11.0_JPRB, &
    &   'forward pulse receiving temperature [K]')
    call assert_close(water_temperature_k(3), TMELT, &
    &   'forward pulse untouched temperature [K]')
    call assert_heat_conserved( &
    &   initial_heat_j, water_temperature_k, liquid_volume_after_m3, &
    &   'forward temperature pulse')
end subroutine test_forward_temperature_pulse


subroutine test_uniform_temperature_is_unchanged()
    real(kind=JPRB) :: &
    &   water_temperature_k(3), normal_flow_m3s(3)
    real(kind=JPRD) :: &
    &   liquid_volume_before_m3(3), liquid_volume_after_m3(3), initial_heat_j

    call set_three_cell_topology()
    water_temperature_k(:) = TMELT + 5.0_JPRB
    liquid_volume_before_m3(:) = [10.0_JPRD, 10.0_JPRD, 10.0_JPRD]
    liquid_volume_after_m3(:) = [9.0_JPRD, 10.0_JPRD, 11.0_JPRD]
    normal_flow_m3s(:) = [1.0_JPRB, 1.0_JPRB, 0.0_JPRB]
    initial_heat_j = total_sensible_heat_j( &
    &   water_temperature_k, liquid_volume_before_m3)

    call advect_river_water_sensible_heat( &
    &   water_temperature_k, liquid_volume_before_m3, liquid_volume_after_m3, &
    &   normal_flow_m3s, 1.0_JPRB)

    call assert_close(water_temperature_k(1), TMELT + 5.0_JPRB, &
    &   'uniform field upstream temperature [K]')
    call assert_close(water_temperature_k(2), TMELT + 5.0_JPRB, &
    &   'uniform field middle temperature [K]')
    call assert_close(water_temperature_k(3), TMELT + 5.0_JPRB, &
    &   'uniform field downstream temperature [K]')
    call assert_heat_conserved( &
    &   initial_heat_j, water_temperature_k, liquid_volume_after_m3, &
    &   'uniform temperature field')
end subroutine test_uniform_temperature_is_unchanged


subroutine test_reverse_and_zero_flow()
    real(kind=JPRB) :: &
    &   water_temperature_k(3), normal_flow_m3s(3)
    real(kind=JPRD) :: &
    &   liquid_volume_before_m3(3), liquid_volume_after_m3(3), initial_heat_j

    call set_three_cell_topology()
    water_temperature_k(:) = [TMELT, TMELT + 10.0_JPRB, TMELT + 2.0_JPRB]
    liquid_volume_before_m3(:) = [10.0_JPRD, 10.0_JPRD, 10.0_JPRD]
    liquid_volume_after_m3(:) = [11.0_JPRD, 9.0_JPRD, 10.0_JPRD]
    normal_flow_m3s(:) = [-1.0_JPRB, 0.0_JPRB, 0.0_JPRB]
    initial_heat_j = total_sensible_heat_j( &
    &   water_temperature_k, liquid_volume_before_m3)

    call advect_river_water_sensible_heat( &
    &   water_temperature_k, liquid_volume_before_m3, liquid_volume_after_m3, &
    &   normal_flow_m3s, 1.0_JPRB)

    call assert_close(water_temperature_k(1), TMELT + 10.0_JPRB / 11.0_JPRB, &
    &   'reverse-flow receiving temperature [K]')
    call assert_close(water_temperature_k(2), TMELT + 10.0_JPRB, &
    &   'reverse-flow source temperature [K]')
    call assert_close(water_temperature_k(3), TMELT + 2.0_JPRB, &
    &   'zero-flow temperature [K]')
    call assert_heat_conserved( &
    &   initial_heat_j, water_temperature_k, liquid_volume_after_m3, &
    &   'reverse and zero flow')
end subroutine test_reverse_and_zero_flow


subroutine test_total_outflow_is_limited_by_available_heat()
    real(kind=JPRB) :: &
    &   water_temperature_k(3), normal_flow_m3s(3)
    real(kind=JPRD) :: &
    &   liquid_volume_before_m3(3), liquid_volume_after_m3(3), initial_heat_j

    call set_three_cell_topology()
    water_temperature_k(:) = [TMELT, TMELT + 12.0_JPRB, TMELT]
    liquid_volume_before_m3(:) = [10.0_JPRD, 10.0_JPRD, 10.0_JPRD]
    liquid_volume_after_m3(:) = [15.0_JPRD, 0.0_JPRD, 15.0_JPRD]
    normal_flow_m3s(:) = [-6.0_JPRB, 6.0_JPRB, 0.0_JPRB]
    initial_heat_j = total_sensible_heat_j( &
    &   water_temperature_k, liquid_volume_before_m3)

    call advect_river_water_sensible_heat( &
    &   water_temperature_k, liquid_volume_before_m3, liquid_volume_after_m3, &
    &   normal_flow_m3s, 1.0_JPRB)

    call assert_close(water_temperature_k(1), TMELT + 4.0_JPRB, &
    &   'limited reverse branch receiving temperature [K]')
    call assert_close(water_temperature_k(2), TMELT, &
    &   'limited source becomes dry at melting point [K]')
    call assert_close(water_temperature_k(3), TMELT + 4.0_JPRB, &
    &   'limited forward branch receiving temperature [K]')
    call assert_heat_conserved( &
    &   initial_heat_j, water_temperature_k, liquid_volume_after_m3, &
    &   'available sensible-heat limiter')
end subroutine test_total_outflow_is_limited_by_available_heat


subroutine test_inflow_to_dry_cell()
    real(kind=JPRB) :: &
    &   water_temperature_k(3), normal_flow_m3s(3)
    real(kind=JPRD) :: &
    &   liquid_volume_before_m3(3), liquid_volume_after_m3(3), initial_heat_j

    call set_three_cell_topology()
    water_temperature_k(:) = [TMELT, TMELT + 7.0_JPRB, TMELT]
    liquid_volume_before_m3(:) = [0.0_JPRD, 10.0_JPRD, 0.0_JPRD]
    liquid_volume_after_m3(:) = [2.0_JPRD, 8.0_JPRD, 0.0_JPRD]
    normal_flow_m3s(:) = [-2.0_JPRB, 0.0_JPRB, 0.0_JPRB]
    initial_heat_j = total_sensible_heat_j( &
    &   water_temperature_k, liquid_volume_before_m3)

    call advect_river_water_sensible_heat( &
    &   water_temperature_k, liquid_volume_before_m3, liquid_volume_after_m3, &
    &   normal_flow_m3s, 1.0_JPRB)

    call assert_close(water_temperature_k(1), TMELT + 7.0_JPRB, &
    &   'dry cell receives source temperature [K]')
    call assert_close(water_temperature_k(2), TMELT + 7.0_JPRB, &
    &   'source temperature after dry-cell inflow [K]')
    call assert_close(water_temperature_k(3), TMELT, &
    &   'dry cell without inflow remains at melting point [K]')
    call assert_heat_conserved( &
    &   initial_heat_j, water_temperature_k, liquid_volume_after_m3, &
    &   'inflow to dry cell')
end subroutine test_inflow_to_dry_cell


subroutine test_zero_and_tiny_liquid_volume()
    real(kind=JPRB) :: &
    &   water_temperature_k(3), normal_flow_m3s(3)
    real(kind=JPRD) :: &
    &   liquid_volume_before_m3(3), liquid_volume_after_m3(3)

    call set_three_cell_topology()
    water_temperature_k(:) = [TMELT + 1.0_JPRB, TMELT + 3.0_JPRB, TMELT]
    liquid_volume_before_m3(:) = [0.0_JPRD, 1.0e-20_JPRD, 0.0_JPRD]
    liquid_volume_after_m3(:) = liquid_volume_before_m3(:)
    normal_flow_m3s(:) = 0.0_JPRB

    call advect_river_water_sensible_heat( &
    &   water_temperature_k, liquid_volume_before_m3, liquid_volume_after_m3, &
    &   normal_flow_m3s, 1.0_JPRB)

    call assert_close(water_temperature_k(1), TMELT, &
    &   'zero-volume cell is reset to melting point [K]')
    call assert_close(water_temperature_k(2), TMELT + 3.0_JPRB, &
    &   'tiny-volume cell remains finite and unchanged [K]')
    call assert_close(water_temperature_k(3), TMELT, &
    &   'second zero-volume cell remains at melting point [K]')
end subroutine test_zero_and_tiny_liquid_volume


subroutine set_three_cell_topology()
    if (allocated(I1NEXT)) deallocate(I1NEXT)
    NSEQALL = 3_JPIM
    NSEQRIV = 2_JPIM
    allocate(I1NEXT(NSEQALL))
    I1NEXT(:) = [2_JPIM, 3_JPIM, -9_JPIM]
end subroutine set_three_cell_topology


function total_sensible_heat_j( &
    &   water_temperature_k, liquid_volume_m3) result(sensible_heat_j)
    real(kind=JPRB), intent(in) :: &
    &   water_temperature_k(:)
    real(kind=JPRD), intent(in) :: &
    &   liquid_volume_m3(:)
    real(kind=JPRD) :: &
    &   sensible_heat_j

    sensible_heat_j = real(RW, kind=JPRD) * real(CW, kind=JPRD) * sum( &
    &   liquid_volume_m3(:) * real( &
    &   water_temperature_k(:) - TMELT, kind=JPRD))
end function total_sensible_heat_j


subroutine assert_heat_conserved( &
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
    write(*, '(a)') '[TEST FAILED] '//trim(label)//' sensible-heat conservation [J]'
    write(*, '(a,es24.15)') '  actual   = ', actual_heat_j
    write(*, '(a,es24.15)') '  expected = ', expected_heat_j
    error stop 1
end subroutine assert_heat_conserved


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

end program test_river_water_advection
