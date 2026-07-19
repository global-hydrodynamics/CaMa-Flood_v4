program test_river_ice_advection_boundaries
    use PARKIND1, only: &
    &   JPIM, JPRB, JPRD
    use YOS_CMF_MAP, only: &
    &   I1NEXT, NSEQALL, NSEQRIV, &
    &   D2RIVLEN, D2RIVWTH, &
    &   NPTHOUT, PTH_UPST, PTH_DOWN
    use river_ice_advection_mod, only: &
    &   advect_river_surface_ice
    implicit none

    call test_river_mouth_outflow_and_reverse_flow()
    call test_bifurcation_forward_and_reverse_flow()
    call test_bifurcation_fully_frozen_velocity_factor()
    call test_normal_and_bifurcation_outflow_share_limiter()
    call test_mouth_and_bifurcation_outflow_share_limiter()

    write(*, '(a)') '[ALL TESTS PASSED] test_river_ice_advection_boundaries'

contains

subroutine test_river_mouth_outflow_and_reverse_flow()
    real(kind=JPRB) :: &
    &   surface_ice_volume_m3(3), surface_ice_fraction(3), normal_flow_m3s(3)
    real(kind=JPRD) :: &
    &   liquid_volume_before_m3(3)

    call set_three_cell_topology(0_JPIM)
    surface_ice_volume_m3(:) = [0.0_JPRB, 0.0_JPRB, 5.0_JPRB]
    surface_ice_fraction(:) = 0.0_JPRB
    liquid_volume_before_m3(:) = 10.0_JPRD
    normal_flow_m3s(:) = [0.0_JPRB, 0.0_JPRB, 2.0_JPRB]

    call advect_river_surface_ice( &
    &   surface_ice_volume_m3, surface_ice_fraction, &
    &   liquid_volume_before_m3, normal_flow_m3s, 1.0_JPRB)

    call assert_close(surface_ice_volume_m3(3), 4.0_JPRB, &
    &   'river-mouth positive flow exports source surface ice [m3]')

    surface_ice_volume_m3(:) = [0.0_JPRB, 0.0_JPRB, 5.0_JPRB]
    normal_flow_m3s(3) = -2.0_JPRB
    call advect_river_surface_ice( &
    &   surface_ice_volume_m3, surface_ice_fraction, &
    &   liquid_volume_before_m3, normal_flow_m3s, 1.0_JPRB)

    call assert_close(surface_ice_volume_m3(3), 5.0_JPRB, &
    &   'TCHOIR river-mouth reverse flow imports no surface ice [m3]')
end subroutine test_river_mouth_outflow_and_reverse_flow


subroutine test_bifurcation_forward_and_reverse_flow()
    real(kind=JPRB) :: &
    &   surface_ice_volume_m3(3), surface_ice_fraction(3), &
    &   normal_flow_m3s(3), bifurcation_flow_m3s(1)
    real(kind=JPRD) :: &
    &   liquid_volume_before_m3(3), initial_total_ice_m3

    call set_three_cell_topology(1_JPIM)
    PTH_UPST(1) = 1_JPIM
    PTH_DOWN(1) = 3_JPIM
    surface_ice_volume_m3(:) = [5.0_JPRB, 0.0_JPRB, 0.0_JPRB]
    surface_ice_fraction(:) = 0.0_JPRB
    liquid_volume_before_m3(:) = 10.0_JPRD
    normal_flow_m3s(:) = 0.0_JPRB
    bifurcation_flow_m3s(:) = 2.0_JPRB
    initial_total_ice_m3 = sum(real(surface_ice_volume_m3, kind=JPRD))

    call advect_river_surface_ice( &
    &   surface_ice_volume_m3, surface_ice_fraction, &
    &   liquid_volume_before_m3, normal_flow_m3s, 1.0_JPRB, &
    &   bifurcation_flow_m3s=bifurcation_flow_m3s)

    call assert_close(surface_ice_volume_m3(1), 4.0_JPRB, &
    &   'forward bifurcation source surface ice [m3]')
    call assert_close(surface_ice_volume_m3(3), 1.0_JPRB, &
    &   'forward bifurcation receiving surface ice [m3]')
    call assert_ice_conserved(initial_total_ice_m3, surface_ice_volume_m3, &
    &   'forward bifurcation')

    surface_ice_volume_m3(:) = [0.0_JPRB, 0.0_JPRB, 5.0_JPRB]
    bifurcation_flow_m3s(:) = -2.0_JPRB
    initial_total_ice_m3 = sum(real(surface_ice_volume_m3, kind=JPRD))
    call advect_river_surface_ice( &
    &   surface_ice_volume_m3, surface_ice_fraction, &
    &   liquid_volume_before_m3, normal_flow_m3s, 1.0_JPRB, &
    &   bifurcation_flow_m3s=bifurcation_flow_m3s)

    call assert_close(surface_ice_volume_m3(1), 1.0_JPRB, &
    &   'reverse bifurcation receiving surface ice [m3]')
    call assert_close(surface_ice_volume_m3(3), 4.0_JPRB, &
    &   'reverse bifurcation source surface ice [m3]')
    call assert_ice_conserved(initial_total_ice_m3, surface_ice_volume_m3, &
    &   'reverse bifurcation')
end subroutine test_bifurcation_forward_and_reverse_flow


subroutine test_bifurcation_fully_frozen_velocity_factor()
    real(kind=JPRB) :: &
    &   surface_ice_volume_m3(3), surface_ice_fraction(3), &
    &   normal_flow_m3s(3), bifurcation_flow_m3s(1)
    real(kind=JPRD) :: &
    &   liquid_volume_before_m3(3)

    call set_three_cell_topology(1_JPIM)
    PTH_UPST(1) = 1_JPIM
    PTH_DOWN(1) = 3_JPIM
    surface_ice_volume_m3(:) = [5.0_JPRB, 0.0_JPRB, 0.0_JPRB]
    surface_ice_fraction(:) = [0.0_JPRB, 0.0_JPRB, 1.0_JPRB]
    liquid_volume_before_m3(:) = 10.0_JPRD
    normal_flow_m3s(:) = 0.0_JPRB
    bifurcation_flow_m3s(:) = 2.0_JPRB

    call advect_river_surface_ice( &
    &   surface_ice_volume_m3, surface_ice_fraction, &
    &   liquid_volume_before_m3, normal_flow_m3s, 1.0_JPRB, &
    &   bifurcation_flow_m3s=bifurcation_flow_m3s)

    call assert_close(surface_ice_volume_m3(3), 0.5_JPRB, &
    &   'fully frozen bifurcation receiver halves surface-ice velocity [m3]')
end subroutine test_bifurcation_fully_frozen_velocity_factor


subroutine test_normal_and_bifurcation_outflow_share_limiter()
    real(kind=JPRB) :: &
    &   surface_ice_volume_m3(3), surface_ice_fraction(3), &
    &   normal_flow_m3s(3), bifurcation_flow_m3s(1)
    real(kind=JPRD) :: &
    &   liquid_volume_before_m3(3), initial_total_ice_m3

    call set_three_cell_topology(1_JPIM)
    PTH_UPST(1) = 1_JPIM
    PTH_DOWN(1) = 3_JPIM
    surface_ice_volume_m3(:) = [5.0_JPRB, 0.0_JPRB, 0.0_JPRB]
    surface_ice_fraction(:) = 0.0_JPRB
    liquid_volume_before_m3(:) = 10.0_JPRD
    normal_flow_m3s(:) = [20.0_JPRB, 0.0_JPRB, 0.0_JPRB]
    bifurcation_flow_m3s(:) = 5.0_JPRB
    initial_total_ice_m3 = sum(real(surface_ice_volume_m3, kind=JPRD))

    call advect_river_surface_ice( &
    &   surface_ice_volume_m3, surface_ice_fraction, &
    &   liquid_volume_before_m3, normal_flow_m3s, 1.0_JPRB, &
    &   bifurcation_flow_m3s=bifurcation_flow_m3s)

    call assert_close(surface_ice_volume_m3(1), 0.0_JPRB, &
    &   'normal-path shared limiter source surface ice [m3]')
    call assert_close(surface_ice_volume_m3(2), 4.0_JPRB, &
    &   'normal-path shared limiter normal branch [m3]')
    call assert_close(surface_ice_volume_m3(3), 1.0_JPRB, &
    &   'normal-path shared limiter bifurcation branch [m3]')
    call assert_ice_conserved(initial_total_ice_m3, surface_ice_volume_m3, &
    &   'normal and bifurcation shared limiter')
end subroutine test_normal_and_bifurcation_outflow_share_limiter


subroutine test_mouth_and_bifurcation_outflow_share_limiter()
    real(kind=JPRB) :: &
    &   surface_ice_volume_m3(3), surface_ice_fraction(3), &
    &   normal_flow_m3s(3), bifurcation_flow_m3s(1)
    real(kind=JPRD) :: &
    &   liquid_volume_before_m3(3)

    call set_three_cell_topology(1_JPIM)
    PTH_UPST(1) = 3_JPIM
    PTH_DOWN(1) = 1_JPIM
    surface_ice_volume_m3(:) = [0.0_JPRB, 0.0_JPRB, 5.0_JPRB]
    surface_ice_fraction(:) = 0.0_JPRB
    liquid_volume_before_m3(:) = 10.0_JPRD
    normal_flow_m3s(:) = [0.0_JPRB, 0.0_JPRB, 20.0_JPRB]
    bifurcation_flow_m3s(:) = 5.0_JPRB

    call advect_river_surface_ice( &
    &   surface_ice_volume_m3, surface_ice_fraction, &
    &   liquid_volume_before_m3, normal_flow_m3s, 1.0_JPRB, &
    &   bifurcation_flow_m3s=bifurcation_flow_m3s)

    call assert_close(surface_ice_volume_m3(1), 1.0_JPRB, &
    &   'mouth-path shared limiter bifurcation branch [m3]')
    call assert_close(surface_ice_volume_m3(3), 0.0_JPRB, &
    &   'mouth-path shared limiter source surface ice [m3]')
    call assert_close_jprd(sum(real(surface_ice_volume_m3, kind=JPRD)), 1.0_JPRD, &
    &   'mouth-path shared limiter retains only the internal branch [m3]')
end subroutine test_mouth_and_bifurcation_outflow_share_limiter


subroutine set_three_cell_topology(number_of_bifurcations)
    integer(kind=JPIM), intent(in) :: &
    &   number_of_bifurcations

    if (allocated(I1NEXT)) deallocate(I1NEXT)
    if (allocated(D2RIVLEN)) deallocate(D2RIVLEN)
    if (allocated(D2RIVWTH)) deallocate(D2RIVWTH)
    if (allocated(PTH_UPST)) deallocate(PTH_UPST)
    if (allocated(PTH_DOWN)) deallocate(PTH_DOWN)
    NSEQALL = 3_JPIM
    NSEQRIV = 2_JPIM
    NPTHOUT = number_of_bifurcations
    allocate(I1NEXT(NSEQALL))
    allocate(D2RIVLEN(NSEQALL, 1), source=1.0_JPRB)
    allocate(D2RIVWTH(NSEQALL, 1), source=1.0_JPRB)
    allocate(PTH_UPST(NPTHOUT), PTH_DOWN(NPTHOUT))
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

end program test_river_ice_advection_boundaries
