program test_river_water_ice_advection_energy
    use PARKIND1, only: &
    &   JPIM, JPRB, JPRD
    use YOS_CMF_MAP, only: &
    &   I1NEXT, NSEQALL, NSEQRIV, &
    &   D2RIVLEN, D2RIVWTH, &
    &   NPTHOUT, PTH_UPST, PTH_DOWN
    use phys_const_mod, only: &
    &   CW, HFUS, RI, RW, TMELT
    use river_water_advection_mod, only: &
    &   advect_river_water_sensible_heat
    use river_ice_advection_mod, only: &
    &   advect_river_surface_ice
    implicit none

    real(kind=JPRB) :: &
    &   water_temperature_k(3), surface_ice_volume_m3(3), &
    &   surface_ice_fraction(3), immobile_excess_ice_volume_m3(3), &
    &   initial_immobile_excess_ice_volume_m3(3), &
    &   normal_flow_m3s(3), bifurcation_flow_m3s(1)
    real(kind=JPRD) :: &
    &   liquid_volume_before_m3(3), liquid_volume_after_m3(3), &
    &   heat_budget_error_j(3), water_budget_error_m3(3), &
    &   unapplied_sensible_heat_j(3), domain_heat_budget_error_j, &
    &   ice_budget_error_m3(3), domain_ice_budget_error_m3, &
    &   combined_energy_budget_error_j(3), domain_combined_energy_budget_error_j, &
    &   initial_energy_j, final_energy_j

    call set_three_cell_topology()
    water_temperature_k(:) = [TMELT + 10.0_JPRB, TMELT + 2.0_JPRB, TMELT]
    surface_ice_volume_m3(:) = [5.0_JPRB, 0.0_JPRB, 2.0_JPRB]
    surface_ice_fraction(:) = 0.0_JPRB
    immobile_excess_ice_volume_m3(:) = [7.0_JPRB, 11.0_JPRB, 13.0_JPRB]
    initial_immobile_excess_ice_volume_m3(:) = immobile_excess_ice_volume_m3(:)
    liquid_volume_before_m3(:) = 10.0_JPRD
    liquid_volume_after_m3(:) = [4.0_JPRD, 14.0_JPRD, 12.0_JPRD]
    normal_flow_m3s(:) = [4.0_JPRB, 0.0_JPRB, 0.0_JPRB]
    bifurcation_flow_m3s(:) = 2.0_JPRB
    initial_energy_j = total_water_ice_energy_j( &
    &   water_temperature_k, liquid_volume_before_m3, &
    &   surface_ice_volume_m3, immobile_excess_ice_volume_m3)

    call advect_river_water_sensible_heat( &
    &   water_temperature_k, liquid_volume_before_m3, liquid_volume_after_m3, &
    &   normal_flow_m3s, 1.0_JPRB, &
    &   bifurcation_flow_m3s=bifurcation_flow_m3s, &
    &   heat_budget_error_j=heat_budget_error_j, &
    &   water_budget_error_m3=water_budget_error_m3, &
    &   unapplied_sensible_heat_j=unapplied_sensible_heat_j, &
    &   domain_heat_budget_error_j=domain_heat_budget_error_j)
    call advect_river_surface_ice( &
    &   surface_ice_volume_m3, surface_ice_fraction, &
    &   liquid_volume_before_m3, normal_flow_m3s, 1.0_JPRB, &
    &   bifurcation_flow_m3s=bifurcation_flow_m3s, &
    &   ice_budget_error_m3=ice_budget_error_m3, &
    &   domain_ice_budget_error_m3=domain_ice_budget_error_m3)

    final_energy_j = total_water_ice_energy_j( &
    &   water_temperature_k, liquid_volume_after_m3, &
    &   surface_ice_volume_m3, immobile_excess_ice_volume_m3)
    combined_energy_budget_error_j(:) = heat_budget_error_j(:) - &
    &   real(RI, kind=JPRD) * real(HFUS, kind=JPRD) * ice_budget_error_m3(:)
    domain_combined_energy_budget_error_j = domain_heat_budget_error_j - &
    &   real(RI, kind=JPRD) * real(HFUS, kind=JPRD) * domain_ice_budget_error_m3

    call assert_small(abs(final_energy_j - initial_energy_j), abs(initial_energy_j), &
    &   'closed-system water-plus-ice energy conservation [J]')
    call assert_small(maxval(abs(combined_energy_budget_error_j)), abs(initial_energy_j), &
    &   'cell water-plus-ice advection energy error [J]')
    call assert_small(abs(domain_combined_energy_budget_error_j), abs(initial_energy_j), &
    &   'domain water-plus-ice advection energy error [J]')
    call assert_small(maxval(abs(water_budget_error_m3)), sum(liquid_volume_before_m3), &
    &   'water-volume budget error [m3]')
    call assert_exact_array(immobile_excess_ice_volume_m3, &
    &   initial_immobile_excess_ice_volume_m3, &
    &   'immobile excess ice remains in its source cells')

    write(*, '(a)') '[ALL TESTS PASSED] test_river_water_ice_advection_energy'

contains

subroutine set_three_cell_topology()
    if (allocated(I1NEXT)) deallocate(I1NEXT)
    if (allocated(D2RIVLEN)) deallocate(D2RIVLEN)
    if (allocated(D2RIVWTH)) deallocate(D2RIVWTH)
    if (allocated(PTH_UPST)) deallocate(PTH_UPST)
    if (allocated(PTH_DOWN)) deallocate(PTH_DOWN)
    NSEQALL = 3_JPIM
    NSEQRIV = 2_JPIM
    NPTHOUT = 1_JPIM
    allocate(I1NEXT(NSEQALL))
    allocate(D2RIVLEN(NSEQALL, 1), source=1.0_JPRB)
    allocate(D2RIVWTH(NSEQALL, 1), source=1.0_JPRB)
    allocate(PTH_UPST(NPTHOUT), PTH_DOWN(NPTHOUT))
    I1NEXT(:) = [2_JPIM, 3_JPIM, -9_JPIM]
    PTH_UPST(1) = 1_JPIM
    PTH_DOWN(1) = 3_JPIM
end subroutine set_three_cell_topology


function total_water_ice_energy_j( &
    &   water_temperature_k, liquid_volume_m3, &
    &   surface_ice_volume_m3, immobile_excess_ice_volume_m3) result(total_energy_j)
    real(kind=JPRB), intent(in) :: &
    &   water_temperature_k(:), surface_ice_volume_m3(:), &
    &   immobile_excess_ice_volume_m3(:)
    real(kind=JPRD), intent(in) :: &
    &   liquid_volume_m3(:)
    real(kind=JPRD) :: &
    &   total_energy_j

    total_energy_j = real(RW, kind=JPRD) * real(CW, kind=JPRD) * sum( &
    &   liquid_volume_m3(:) * &
    &   real(water_temperature_k(:) - TMELT, kind=JPRD)) - &
    &   real(RI, kind=JPRD) * real(HFUS, kind=JPRD) * sum(real( &
    &   surface_ice_volume_m3(:) + immobile_excess_ice_volume_m3(:), kind=JPRD))
end function total_water_ice_energy_j


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


subroutine assert_small(actual_error, scale, label)
    real(kind=JPRD), intent(in) :: &
    &   actual_error, scale
    character(len=*), intent(in) :: &
    &   label
    real(kind=JPRD) :: &
    &   tolerance

    tolerance = 1.0e-12_JPRD * max(scale, 1.0_JPRD)
    if (actual_error <= tolerance) return
    write(*, '(a)') '[TEST FAILED] '//trim(label)
    write(*, '(a,es24.15)') '  error     = ', actual_error
    write(*, '(a,es24.15)') '  tolerance = ', tolerance
    error stop 1
end subroutine assert_small

end program test_river_water_ice_advection_energy
