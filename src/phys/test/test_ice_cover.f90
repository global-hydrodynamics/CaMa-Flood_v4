program test_ice_cover
    use PARKIND1, only: &
    &   JPRB
    use ice_cover_mod, only: &
    &   diagnose_ice_shape, diagnose_ice_cover, update_ice_cover_state
    implicit none

    call check_shape(0.0_JPRB, 100.0_JPRB, 20.0_JPRB, &
    &   0.0_JPRB, 0.0_JPRB, 0.0_JPRB, 0.0_JPRB, 0.0_JPRB, 'no ice')
    call check_shape(0.25_JPRB, 100.0_JPRB, 20.0_JPRB, &
    &   0.25_JPRB, 0.0_JPRB, 5.0_JPRB, 0.05_JPRB, 0.05_JPRB, 'sparse ice')
    call check_shape(12.5_JPRB, 100.0_JPRB, 20.0_JPRB, &
    &   12.5_JPRB, 0.0_JPRB, 50.0_JPRB, 0.25_JPRB, 0.5_JPRB, 'partial cover')
    call check_shape(50.0_JPRB, 100.0_JPRB, 20.0_JPRB, &
    &   50.0_JPRB, 0.0_JPRB, 100.0_JPRB, 0.5_JPRB, 1.0_JPRB, 'full cover')
    call check_shape(200.0_JPRB, 100.0_JPRB, 20.0_JPRB, &
    &   200.0_JPRB, 0.0_JPRB, 100.0_JPRB, 2.0_JPRB, 1.0_JPRB, 'thick ice')
    call check_shape(2500.0_JPRB, 100.0_JPRB, 20.0_JPRB, &
    &   2000.0_JPRB, 500.0_JPRB, 100.0_JPRB, 20.0_JPRB, 1.0_JPRB, 'excess ice')
    call check_shape(10.0_JPRB, 0.0_JPRB, 20.0_JPRB, &
    &   0.0_JPRB, 10.0_JPRB, 0.0_JPRB, 0.0_JPRB, 0.0_JPRB, 'zero surface area')
    call check_immobile_excess_state()
    call check_immobile_excess_shape()

    write(*, '(a)') '[ALL TESTS PASSED] test_ice_cover'

contains

subroutine check_immobile_excess_shape()
    real(kind=JPRB) :: &
    &   ice_area_m2, &       ! [m2] Diagnosed effective excess-ice area.
    &   ice_thickness_m, &   ! [m] Diagnosed excess-ice thickness.
    &   ice_fraction         ! [-] Diagnosed fraction of the effective excess-ice area.

    ! Immobile excess ice has no second thickness cap; it remains in the cell
    ! and becomes thicker after filling its effective exchange area.
    call diagnose_ice_shape( &
    &   2500.0_JPRB, 100.0_JPRB, ice_area_m2, ice_thickness_m, ice_fraction)
    call assert_close(ice_area_m2, 100.0_JPRB, 'immobile excess shape area')
    call assert_close(ice_thickness_m, 25.0_JPRB, 'immobile excess shape thickness')
    call assert_close(ice_fraction, 1.0_JPRB, 'immobile excess shape fraction')

    call diagnose_ice_shape( &
    &   10.0_JPRB, 0.0_JPRB, ice_area_m2, ice_thickness_m, ice_fraction)
    call assert_close(ice_area_m2, 0.0_JPRB, 'immobile excess zero-area shape area')
    call assert_close(ice_thickness_m, 0.0_JPRB, 'immobile excess zero-area shape thickness')
    call assert_close(ice_fraction, 0.0_JPRB, 'immobile excess zero-area shape fraction')
end subroutine check_immobile_excess_shape

subroutine check_immobile_excess_state()
    real(kind=JPRB) :: &
    &   ice_volume_m3, &          ! [m3] Ice retained on the water surface.
    &   excess_ice_volume_m3, &   ! [m3] Immobile excess ice retained in the grid cell.
    &   initial_total_volume_m3, & ! [m3] Total ice volume before applying the capacity limit.
    &   ice_area_m2, &            ! [m2] Diagnosed water-surface ice area.
    &   ice_thickness_m, &        ! [m] Diagnosed water-surface ice thickness.
    &   ice_fraction              ! [-] Diagnosed water-surface ice fraction.

    ice_volume_m3 = 2500.0_JPRB
    excess_ice_volume_m3 = 25.0_JPRB
    initial_total_volume_m3 = ice_volume_m3 + excess_ice_volume_m3
    call update_ice_cover_state( &
    &   ice_volume_m3, excess_ice_volume_m3, 100.0_JPRB, 20.0_JPRB, &
    &   ice_area_m2, ice_thickness_m, ice_fraction)

    call assert_close(ice_volume_m3, 2000.0_JPRB, 'immobile excess retained water-surface volume')
    call assert_close(excess_ice_volume_m3, 525.0_JPRB, 'immobile excess transferred volume')
    call assert_close(ice_volume_m3 + excess_ice_volume_m3, &
    &   initial_total_volume_m3, 'immobile excess total-volume conservation')

    ! Increasing water-surface capacity must not remobilize existing excess ice.
    call update_ice_cover_state( &
    &   ice_volume_m3, excess_ice_volume_m3, 200.0_JPRB, 20.0_JPRB, &
    &   ice_area_m2, ice_thickness_m, ice_fraction)
    call assert_close(ice_volume_m3, 2000.0_JPRB, 'immobile excess no remobilization water-surface volume')
    call assert_close(excess_ice_volume_m3, 525.0_JPRB, 'immobile excess no remobilization excess volume')
    call assert_close(ice_volume_m3 + excess_ice_volume_m3, &
    &   initial_total_volume_m3, 'immobile excess no-remobilization conservation')
end subroutine check_immobile_excess_state

subroutine check_shape( &
    &   ice_volume_m3, water_surface_area_m2, maximum_ice_thickness_m, &
    &   expected_retained_volume_m3, expected_excess_volume_m3, &
    &   expected_ice_area_m2, expected_ice_thickness_m, expected_ice_fraction, label)
    real(kind=JPRB), intent(in) :: &
    &   ice_volume_m3, &                  ! [m3] Input ice volume.
    &   water_surface_area_m2, &          ! [m2] Available water-surface area.
    &   maximum_ice_thickness_m, &        ! [m] Maximum retained ice thickness.
    &   expected_retained_volume_m3, &    ! [m3] Expected retained ice volume.
    &   expected_excess_volume_m3, &      ! [m3] Expected ice volume transferred to the immobile pool.
    &   expected_ice_area_m2, &           ! [m2] Expected ice-covered area.
    &   expected_ice_thickness_m, &       ! [m] Expected mean ice thickness.
    &   expected_ice_fraction             ! [-] Expected ice-covered fraction.
    character(len=*), intent(in) :: &
    &   label                             ! [-] Human-readable test-case label.
    real(kind=JPRB) :: &
    &   retained_ice_volume_m3, &         ! [m3] Diagnosed retained ice volume.
    &   excess_ice_volume_m3, &           ! [m3] Diagnosed ice volume transferred to the immobile pool.
    &   ice_area_m2, &                    ! [m2] Diagnosed ice-covered area.
    &   ice_thickness_m, &                ! [m] Diagnosed mean ice thickness.
    &   ice_fraction                      ! [-] Diagnosed ice-covered fraction.

    call diagnose_ice_cover( &
    &   ice_volume_m3, water_surface_area_m2, maximum_ice_thickness_m, &
    &   retained_ice_volume_m3, excess_ice_volume_m3, &
    &   ice_area_m2, ice_thickness_m, ice_fraction)

    call assert_close(retained_ice_volume_m3, expected_retained_volume_m3, label//' retained volume')
    call assert_close(excess_ice_volume_m3, expected_excess_volume_m3, label//' excess volume')
    call assert_close(ice_area_m2, expected_ice_area_m2, label//' area')
    call assert_close(ice_thickness_m, expected_ice_thickness_m, label//' thickness')
    call assert_close(ice_fraction, expected_ice_fraction, label//' fraction')
    call assert_close(retained_ice_volume_m3 + excess_ice_volume_m3, &
    &   max(ice_volume_m3, 0.0_JPRB), label//' volume conservation')
end subroutine check_shape


subroutine assert_close(actual_value, expected_value, label)
    real(kind=JPRB), intent(in) :: &
    &   actual_value, &       ! [caller-defined unit] Computed value.
    &   expected_value        ! [caller-defined unit] Expected value.
    character(len=*), intent(in) :: &
    &   label                 ! [-] Human-readable assertion label.
    real(kind=JPRB) :: &
    &   tolerance             ! [caller-defined unit] Scaled comparison tolerance.

    tolerance = 1.0e-12_JPRB * max(1.0_JPRB, abs(expected_value))
    if (abs(actual_value - expected_value) <= tolerance) return

    write(*, '(a)') '[TEST FAILED] '//trim(label)
    write(*, '(a,es24.15)') '  actual   = ', actual_value
    write(*, '(a,es24.15)') '  expected = ', expected_value
    write(*, '(a,es24.15)') '  tolerance= ', tolerance
    error stop 1
end subroutine assert_close

end program test_ice_cover
