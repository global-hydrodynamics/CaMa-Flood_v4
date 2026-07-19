program test_river_ice_advection
    use PARKIND1, only: &
    &   JPIM, JPRB, JPRD
    use YOS_CMF_MAP, only: &
    &   I1NEXT, NSEQALL, NSEQRIV, &
    &   D2RIVLEN, D2RIVWTH
    use river_ice_advection_mod, only: &
    &   advect_river_surface_ice, diagnose_surface_ice_transport_fraction
    implicit none

    call test_transport_fraction_zero_one_and_partial()
    call test_forward_surface_ice_pulse()
    call test_reverse_and_zero_flow()
    call test_complete_transport()
    call test_total_outflow_is_limited_by_available_ice()
    call test_fully_frozen_cells_slow_surface_ice()
    call test_shallow_source_retains_surface_ice()
    call test_dry_source_retains_surface_ice()
    call test_immobile_excess_ice_is_unchanged()

    write(*, '(a)') '[ALL TESTS PASSED] test_river_ice_advection'

contains

subroutine test_transport_fraction_zero_one_and_partial()
    call assert_close_jprd( &
    &   diagnose_surface_ice_transport_fraction(10.0_JPRD, 0.0_JPRD), &
    &   0.0_JPRD, 'zero transported water gives zero ice fraction')
    call assert_close_jprd( &
    &   diagnose_surface_ice_transport_fraction(10.0_JPRD, 2.5_JPRD), &
    &   0.25_JPRD, 'partial transported water gives partial ice fraction')
    call assert_close_jprd( &
    &   diagnose_surface_ice_transport_fraction(10.0_JPRD, 10.0_JPRD), &
    &   1.0_JPRD, 'full source-water turnover gives unit ice fraction')
    call assert_close_jprd( &
    &   diagnose_surface_ice_transport_fraction(10.0_JPRD, 20.0_JPRD), &
    &   2.0_JPRD, 'TCHOIR requested fraction remains uncapped before srate')
    call assert_close_jprd( &
    &   diagnose_surface_ice_transport_fraction(0.0_JPRD, 2.0_JPRD), &
    &   0.0_JPRD, 'dry source gives zero ice fraction')
end subroutine test_transport_fraction_zero_one_and_partial


subroutine test_forward_surface_ice_pulse()
    real(kind=JPRB) :: &
    &   surface_ice_volume_m3(3), surface_ice_fraction(3), normal_flow_m3s(3)
    real(kind=JPRD) :: &
    &   liquid_volume_before_m3(3), initial_total_ice_m3

    call set_three_cell_topology()
    surface_ice_volume_m3(:) = [5.0_JPRB, 0.0_JPRB, 0.0_JPRB]
    surface_ice_fraction(:) = 0.0_JPRB
    liquid_volume_before_m3(:) = 10.0_JPRD
    normal_flow_m3s(:) = [2.0_JPRB, 0.0_JPRB, 0.0_JPRB]
    initial_total_ice_m3 = sum(real(surface_ice_volume_m3, kind=JPRD))

    call advect_river_surface_ice( &
    &   surface_ice_volume_m3, surface_ice_fraction, &
    &   liquid_volume_before_m3, normal_flow_m3s, 1.0_JPRB)

    call assert_close(surface_ice_volume_m3(1), 4.0_JPRB, &
    &   'forward pulse source surface ice [m3]')
    call assert_close(surface_ice_volume_m3(2), 1.0_JPRB, &
    &   'forward pulse receiving surface ice [m3]')
    call assert_close(surface_ice_volume_m3(3), 0.0_JPRB, &
    &   'forward pulse untouched surface ice [m3]')
    call assert_ice_conserved(initial_total_ice_m3, surface_ice_volume_m3, &
    &   'forward surface-ice pulse')
end subroutine test_forward_surface_ice_pulse


subroutine test_reverse_and_zero_flow()
    real(kind=JPRB) :: &
    &   surface_ice_volume_m3(3), surface_ice_fraction(3), normal_flow_m3s(3)
    real(kind=JPRD) :: &
    &   liquid_volume_before_m3(3), initial_total_ice_m3

    call set_three_cell_topology()
    surface_ice_volume_m3(:) = [0.0_JPRB, 5.0_JPRB, 2.0_JPRB]
    surface_ice_fraction(:) = 0.0_JPRB
    liquid_volume_before_m3(:) = 10.0_JPRD
    normal_flow_m3s(:) = [-2.0_JPRB, 0.0_JPRB, 0.0_JPRB]
    initial_total_ice_m3 = sum(real(surface_ice_volume_m3, kind=JPRD))

    call advect_river_surface_ice( &
    &   surface_ice_volume_m3, surface_ice_fraction, &
    &   liquid_volume_before_m3, normal_flow_m3s, 1.0_JPRB)

    call assert_close(surface_ice_volume_m3(1), 1.0_JPRB, &
    &   'reverse-flow receiving surface ice [m3]')
    call assert_close(surface_ice_volume_m3(2), 4.0_JPRB, &
    &   'reverse-flow source surface ice [m3]')
    call assert_close(surface_ice_volume_m3(3), 2.0_JPRB, &
    &   'zero-flow surface ice [m3]')
    call assert_ice_conserved(initial_total_ice_m3, surface_ice_volume_m3, &
    &   'reverse and zero surface-ice flow')
end subroutine test_reverse_and_zero_flow


subroutine test_complete_transport()
    real(kind=JPRB) :: &
    &   surface_ice_volume_m3(3), surface_ice_fraction(3), normal_flow_m3s(3)
    real(kind=JPRD) :: &
    &   liquid_volume_before_m3(3)

    call set_three_cell_topology()
    surface_ice_volume_m3(:) = [3.0_JPRB, 0.0_JPRB, 0.0_JPRB]
    surface_ice_fraction(:) = 0.0_JPRB
    liquid_volume_before_m3(:) = 10.0_JPRD
    normal_flow_m3s(:) = [10.0_JPRB, 0.0_JPRB, 0.0_JPRB]

    call advect_river_surface_ice( &
    &   surface_ice_volume_m3, surface_ice_fraction, &
    &   liquid_volume_before_m3, normal_flow_m3s, 1.0_JPRB)

    call assert_close(surface_ice_volume_m3(1), 0.0_JPRB, &
    &   'unit-rate source surface ice [m3]')
    call assert_close(surface_ice_volume_m3(2), 3.0_JPRB, &
    &   'unit-rate receiving surface ice [m3]')
end subroutine test_complete_transport


subroutine test_total_outflow_is_limited_by_available_ice()
    real(kind=JPRB) :: &
    &   surface_ice_volume_m3(3), surface_ice_fraction(3), normal_flow_m3s(3)
    real(kind=JPRD) :: &
    &   liquid_volume_before_m3(3), initial_total_ice_m3

    call set_three_cell_topology()
    surface_ice_volume_m3(:) = [0.0_JPRB, 5.0_JPRB, 0.0_JPRB]
    surface_ice_fraction(:) = 0.0_JPRB
    liquid_volume_before_m3(:) = 10.0_JPRD
    normal_flow_m3s(:) = [-20.0_JPRB, 5.0_JPRB, 0.0_JPRB]
    initial_total_ice_m3 = sum(real(surface_ice_volume_m3, kind=JPRD))

    call advect_river_surface_ice( &
    &   surface_ice_volume_m3, surface_ice_fraction, &
    &   liquid_volume_before_m3, normal_flow_m3s, 1.0_JPRB)

    call assert_close(surface_ice_volume_m3(1), 4.0_JPRB, &
    &   'limited reverse branch surface ice [m3]')
    call assert_close(surface_ice_volume_m3(2), 0.0_JPRB, &
    &   'limited source surface ice [m3]')
    call assert_close(surface_ice_volume_m3(3), 1.0_JPRB, &
    &   'limited forward branch surface ice [m3]')
    call assert_ice_conserved(initial_total_ice_m3, surface_ice_volume_m3, &
    &   'available surface-ice limiter')
end subroutine test_total_outflow_is_limited_by_available_ice


subroutine test_fully_frozen_cells_slow_surface_ice()
    real(kind=JPRB) :: &
    &   surface_ice_volume_m3(3), surface_ice_fraction(3), normal_flow_m3s(3)
    real(kind=JPRD) :: &
    &   liquid_volume_before_m3(3)

    call set_three_cell_topology()
    liquid_volume_before_m3(:) = 10.0_JPRD
    normal_flow_m3s(:) = [2.0_JPRB, 0.0_JPRB, 0.0_JPRB]

    surface_ice_volume_m3(:) = [5.0_JPRB, 0.0_JPRB, 0.0_JPRB]
    surface_ice_fraction(:) = [1.0_JPRB, 0.0_JPRB, 0.0_JPRB]
    call advect_river_surface_ice( &
    &   surface_ice_volume_m3, surface_ice_fraction, &
    &   liquid_volume_before_m3, normal_flow_m3s, 1.0_JPRB)
    call assert_close(surface_ice_volume_m3(1), 4.5_JPRB, &
    &   'fully frozen source halves surface-ice velocity [m3]')
    call assert_close(surface_ice_volume_m3(2), 0.5_JPRB, &
    &   'fully frozen source sends half-speed surface ice [m3]')

    surface_ice_volume_m3(:) = [5.0_JPRB, 0.0_JPRB, 0.0_JPRB]
    surface_ice_fraction(:) = [1.0_JPRB, 1.0_JPRB, 0.0_JPRB]
    call advect_river_surface_ice( &
    &   surface_ice_volume_m3, surface_ice_fraction, &
    &   liquid_volume_before_m3, normal_flow_m3s, 1.0_JPRB)
    call assert_close(surface_ice_volume_m3(2), 0.5_JPRB, &
    &   'two fully frozen cells still use one half-speed factor [m3]')

    surface_ice_volume_m3(:) = [5.0_JPRB, 0.0_JPRB, 0.0_JPRB]
    surface_ice_fraction(:) = [0.0_JPRB, 1.0_JPRB, 0.0_JPRB]
    call advect_river_surface_ice( &
    &   surface_ice_volume_m3, surface_ice_fraction, &
    &   liquid_volume_before_m3, normal_flow_m3s, 1.0_JPRB)
    call assert_close(surface_ice_volume_m3(2), 0.5_JPRB, &
    &   'fully frozen receiver halves surface-ice velocity [m3]')
end subroutine test_fully_frozen_cells_slow_surface_ice


subroutine test_shallow_source_retains_surface_ice()
    real(kind=JPRB) :: &
    &   surface_ice_volume_m3(3), surface_ice_fraction(3), normal_flow_m3s(3)
    real(kind=JPRD) :: &
    &   liquid_volume_before_m3(3)

    call set_three_cell_topology()
    surface_ice_volume_m3(:) = [2.0_JPRB, 0.0_JPRB, 0.0_JPRB]
    surface_ice_fraction(:) = 0.0_JPRB
    liquid_volume_before_m3(:) = [0.005_JPRD, 10.0_JPRD, 10.0_JPRD]
    normal_flow_m3s(:) = [0.001_JPRB, 0.0_JPRB, 0.0_JPRB]

    call advect_river_surface_ice( &
    &   surface_ice_volume_m3, surface_ice_fraction, &
    &   liquid_volume_before_m3, normal_flow_m3s, 1.0_JPRB)

    call assert_close(surface_ice_volume_m3(1), 2.0_JPRB, &
    &   'TCHOIR shallow-water threshold retains source surface ice [m3]')
    call assert_close(surface_ice_volume_m3(2), 0.0_JPRB, &
    &   'TCHOIR shallow-water threshold exports no surface ice [m3]')
end subroutine test_shallow_source_retains_surface_ice


subroutine test_dry_source_retains_surface_ice()
    real(kind=JPRB) :: &
    &   surface_ice_volume_m3(3), surface_ice_fraction(3), normal_flow_m3s(3)
    real(kind=JPRD) :: &
    &   liquid_volume_before_m3(3)

    call set_three_cell_topology()
    surface_ice_volume_m3(:) = [2.0_JPRB, 0.0_JPRB, 0.0_JPRB]
    surface_ice_fraction(:) = 0.0_JPRB
    liquid_volume_before_m3(:) = [0.0_JPRD, 10.0_JPRD, 10.0_JPRD]
    normal_flow_m3s(:) = [2.0_JPRB, 0.0_JPRB, 0.0_JPRB]

    call advect_river_surface_ice( &
    &   surface_ice_volume_m3, surface_ice_fraction, &
    &   liquid_volume_before_m3, normal_flow_m3s, 1.0_JPRB)

    call assert_close(surface_ice_volume_m3(1), 2.0_JPRB, &
    &   'dry source retains surface ice [m3]')
    call assert_close(surface_ice_volume_m3(2), 0.0_JPRB, &
    &   'dry source exports no surface ice [m3]')
end subroutine test_dry_source_retains_surface_ice


subroutine test_immobile_excess_ice_is_unchanged()
    real(kind=JPRB) :: &
    &   surface_ice_volume_m3(3), immobile_excess_ice_volume_m3(3), &
    &   initial_immobile_excess_ice_volume_m3(3), surface_ice_fraction(3), &
    &   normal_flow_m3s(3)
    real(kind=JPRD) :: &
    &   liquid_volume_before_m3(3)

    call set_three_cell_topology()
    surface_ice_volume_m3(:) = [5.0_JPRB, 0.0_JPRB, 0.0_JPRB]
    immobile_excess_ice_volume_m3(:) = [7.0_JPRB, 11.0_JPRB, 13.0_JPRB]
    initial_immobile_excess_ice_volume_m3(:) = immobile_excess_ice_volume_m3(:)
    surface_ice_fraction(:) = 0.0_JPRB
    liquid_volume_before_m3(:) = 10.0_JPRD
    normal_flow_m3s(:) = [2.0_JPRB, 0.0_JPRB, 0.0_JPRB]

    call advect_river_surface_ice( &
    &   surface_ice_volume_m3, surface_ice_fraction, &
    &   liquid_volume_before_m3, normal_flow_m3s, 1.0_JPRB)

    call assert_exact_array(immobile_excess_ice_volume_m3, &
    &   initial_immobile_excess_ice_volume_m3, &
    &   'immobile excess ice is excluded from advection')
end subroutine test_immobile_excess_ice_is_unchanged


subroutine set_three_cell_topology()
    if (allocated(I1NEXT)) deallocate(I1NEXT)
    if (allocated(D2RIVLEN)) deallocate(D2RIVLEN)
    if (allocated(D2RIVWTH)) deallocate(D2RIVWTH)
    NSEQALL = 3_JPIM
    NSEQRIV = 2_JPIM
    allocate(I1NEXT(NSEQALL))
    allocate(D2RIVLEN(NSEQALL, 1), source=1.0_JPRB)
    allocate(D2RIVWTH(NSEQALL, 1), source=1.0_JPRB)
    I1NEXT(:) = [2_JPIM, 3_JPIM, -9_JPIM]
end subroutine set_three_cell_topology


subroutine assert_ice_conserved(expected_total_ice_m3, surface_ice_volume_m3, label)
    real(kind=JPRD), intent(in) :: &
    &   expected_total_ice_m3
    real(kind=JPRB), intent(in) :: &
    &   surface_ice_volume_m3(:)
    character(len=*), intent(in) :: &
    &   label

    call assert_close_jprd(sum(real(surface_ice_volume_m3, kind=JPRD)), &
    &   expected_total_ice_m3, trim(label)//' closed-system ice volume [m3]')
end subroutine assert_ice_conserved


subroutine assert_exact_array(actual_values, expected_values, label)
    real(kind=JPRB), intent(in) :: &
    &   actual_values(:), expected_values(:)
    character(len=*), intent(in) :: &
    &   label
    integer(kind=JPIM) :: &
    &   iseq

    do iseq = 1, size(actual_values)
        if (actual_values(iseq) == expected_values(iseq)) cycle
        write(*, '(a,i0)') '[TEST FAILED] '//trim(label)//' at index ', iseq
        error stop 1
    enddo
end subroutine assert_exact_array


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


subroutine assert_close_jprd(actual_value, expected_value, label)
    real(kind=JPRD), intent(in) :: &
    &   actual_value, expected_value
    character(len=*), intent(in) :: &
    &   label
    real(kind=JPRD) :: &
    &   tolerance

    tolerance = 1.0e-13_JPRD * max(1.0_JPRD, abs(expected_value))
    if (abs(actual_value - expected_value) <= tolerance) return
    write(*, '(a)') '[TEST FAILED] '//trim(label)
    write(*, '(a,es24.15)') '  actual   = ', actual_value
    write(*, '(a,es24.15)') '  expected = ', expected_value
    error stop 1
end subroutine assert_close_jprd

end program test_river_ice_advection
