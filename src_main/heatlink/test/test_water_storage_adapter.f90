program test_water_storage_adapter
    use PARKIND1, only: &
    &   JPIB, JPRB, JPRD
    use water_storage_adapter_mod, only: &
    &   apply_liquid_volume_delta_to_storage
    implicit none

    call test_zero_delta_preserves_storage_bits()
    call test_melt_and_freeze_distribution()
    call test_melt_from_zero_liquid_storage()
    call test_excessive_removal_is_rejected()
    call test_double_precision_delta_application()

    write(*, '(a)') '[ALL TESTS PASSED] test_water_storage_adapter'

contains

subroutine test_zero_delta_preserves_storage_bits()
    real(kind=JPRD) :: &
    &   river_storage_volume_m3, &       ! [m3] Canonical river storage under test.
    &   floodplain_storage_volume_m3, &  ! [m3] Canonical floodplain storage under test.
    &   unavailable_liquid_volume_m3     ! [m3] Unavailable removal reported by the adapter.
    integer(kind=JPIB) :: &
    &   initial_river_bits, &            ! [-] Bit pattern of initial river storage.
    &   initial_floodplain_bits           ! [-] Bit pattern of initial floodplain storage.
    logical :: &
    &   update_is_valid                  ! [-] Storage-update validation result.

    river_storage_volume_m3 = 2.0_JPRD**40 + 0.125_JPRD
    floodplain_storage_volume_m3 = 2.0_JPRD**20 + 0.0625_JPRD
    initial_river_bits = transfer(river_storage_volume_m3, initial_river_bits)
    initial_floodplain_bits = transfer( &
    &   floodplain_storage_volume_m3, initial_floodplain_bits)

    call apply_liquid_volume_delta_to_storage( &
    &   river_storage_volume_m3=river_storage_volume_m3, &
    &   floodplain_storage_volume_m3=floodplain_storage_volume_m3, &
    &   liquid_volume_delta_m3=0.0_JPRD, &
    &   update_is_valid=update_is_valid, &
    &   unavailable_liquid_volume_m3=unavailable_liquid_volume_m3)

    call assert_true(update_is_valid, 'zero-delta update is valid')
    call assert_true(transfer(river_storage_volume_m3, initial_river_bits) == &
    &   initial_river_bits, 'zero delta preserves river-storage bits')
    call assert_true(transfer(floodplain_storage_volume_m3, initial_floodplain_bits) == &
    &   initial_floodplain_bits, 'zero delta preserves floodplain-storage bits')
end subroutine test_zero_delta_preserves_storage_bits


subroutine test_melt_and_freeze_distribution()
    real(kind=JPRD) :: &
    &   river_storage_volume_m3, &       ! [m3] Canonical river storage under test.
    &   floodplain_storage_volume_m3, &  ! [m3] Canonical floodplain storage under test.
    &   unavailable_liquid_volume_m3     ! [m3] Unavailable removal reported by the adapter.
    logical :: &
    &   update_is_valid                  ! [-] Storage-update validation result.

    river_storage_volume_m3 = 60.0_JPRD
    floodplain_storage_volume_m3 = 40.0_JPRD
    call apply_liquid_volume_delta_to_storage( &
    &   river_storage_volume_m3, floodplain_storage_volume_m3, 10.0_JPRD, &
    &   update_is_valid, unavailable_liquid_volume_m3)
    call assert_true(update_is_valid, 'meltwater storage update is valid')
    call assert_close(river_storage_volume_m3, 66.0_JPRD, &
    &   'meltwater river-storage distribution [m3]')
    call assert_close(floodplain_storage_volume_m3, 44.0_JPRD, &
    &   'meltwater floodplain-storage distribution [m3]')

    call apply_liquid_volume_delta_to_storage( &
    &   river_storage_volume_m3, floodplain_storage_volume_m3, -25.0_JPRD, &
    &   update_is_valid, unavailable_liquid_volume_m3)
    call assert_true(update_is_valid, 'freezing storage update is valid')
    call assert_close(river_storage_volume_m3, 51.0_JPRD, &
    &   'freezing river-storage distribution [m3]')
    call assert_close(floodplain_storage_volume_m3, 34.0_JPRD, &
    &   'freezing floodplain-storage distribution [m3]')
    call assert_close(river_storage_volume_m3 + floodplain_storage_volume_m3, &
    &   85.0_JPRD, 'phase-change total liquid storage [m3]')

    call apply_liquid_volume_delta_to_storage( &
    &   river_storage_volume_m3, floodplain_storage_volume_m3, -85.0_JPRD, &
    &   update_is_valid, unavailable_liquid_volume_m3)
    call assert_true(update_is_valid, 'complete-freezing storage update is valid')
    call assert_close(river_storage_volume_m3, 0.0_JPRD, &
    &   'complete freezing clears river storage [m3]')
    call assert_close(floodplain_storage_volume_m3, 0.0_JPRD, &
    &   'complete freezing clears floodplain storage [m3]')
end subroutine test_melt_and_freeze_distribution


subroutine test_melt_from_zero_liquid_storage()
    real(kind=JPRD) :: &
    &   river_storage_volume_m3, &       ! [m3] Canonical river storage under test.
    &   floodplain_storage_volume_m3, &  ! [m3] Canonical floodplain storage under test.
    &   unavailable_liquid_volume_m3     ! [m3] Unavailable removal reported by the adapter.
    logical :: &
    &   update_is_valid                  ! [-] Storage-update validation result.

    river_storage_volume_m3 = 0.0_JPRD
    floodplain_storage_volume_m3 = 0.0_JPRD
    call apply_liquid_volume_delta_to_storage( &
    &   river_storage_volume_m3, floodplain_storage_volume_m3, 3.5_JPRD, &
    &   update_is_valid, unavailable_liquid_volume_m3)
    call assert_true(update_is_valid, 'zero-storage meltwater update is valid')
    call assert_close(river_storage_volume_m3, 3.5_JPRD, &
    &   'zero-storage meltwater enters river [m3]')
    call assert_close(floodplain_storage_volume_m3, 0.0_JPRD, &
    &   'zero-storage floodplain remains empty [m3]')
end subroutine test_melt_from_zero_liquid_storage


subroutine test_excessive_removal_is_rejected()
    real(kind=JPRD) :: &
    &   river_storage_volume_m3, &       ! [m3] Canonical river storage under test.
    &   floodplain_storage_volume_m3, &  ! [m3] Canonical floodplain storage under test.
    &   unavailable_liquid_volume_m3     ! [m3] Unavailable removal reported by the adapter.
    logical :: &
    &   update_is_valid                  ! [-] Storage-update validation result.

    river_storage_volume_m3 = 2.0_JPRD
    floodplain_storage_volume_m3 = 1.0_JPRD
    call apply_liquid_volume_delta_to_storage( &
    &   river_storage_volume_m3, floodplain_storage_volume_m3, -4.0_JPRD, &
    &   update_is_valid, unavailable_liquid_volume_m3)
    call assert_true(.not. update_is_valid, 'excessive removal is rejected')
    call assert_close(unavailable_liquid_volume_m3, 1.0_JPRD, &
    &   'unavailable liquid removal [m3]')
    call assert_close(river_storage_volume_m3, 2.0_JPRD, &
    &   'rejected removal preserves river storage [m3]')
    call assert_close(floodplain_storage_volume_m3, 1.0_JPRD, &
    &   'rejected removal preserves floodplain storage [m3]')
end subroutine test_excessive_removal_is_rejected


subroutine test_double_precision_delta_application()
    real(kind=JPRD) :: &
    &   river_storage_volume_m3, &       ! [m3] Canonical river storage under test.
    &   floodplain_storage_volume_m3, &  ! [m3] Canonical floodplain storage under test.
    &   initial_river_storage_m3, &      ! [m3] River storage before applying a precise delta.
    &   expected_river_storage_m3, &     ! [m3] Exact expected river storage after the update.
    &   whole_volume_roundtrip_m3, &     ! [m3] Expected storage after a legacy JPRB whole-volume round trip.
    &   unavailable_liquid_volume_m3     ! [m3] Unavailable removal reported by the adapter.
    logical :: &
    &   update_is_valid                  ! [-] Storage-update validation result.

    river_storage_volume_m3 = 2.0_JPRD**40 + 0.125_JPRD
    floodplain_storage_volume_m3 = 0.0_JPRD
    initial_river_storage_m3 = river_storage_volume_m3
    expected_river_storage_m3 = initial_river_storage_m3 + 1.0_JPRD
    whole_volume_roundtrip_m3 = real( &
    &   real(expected_river_storage_m3, kind=JPRB), kind=JPRD)
    if (precision(0.0_JPRB) < precision(0.0_JPRD)) then
        call assert_true(transfer(whole_volume_roundtrip_m3, 0_JPIB) /= &
        &   transfer(expected_river_storage_m3, 0_JPIB), &
        &   'JPRB whole-volume round trip loses canonical low bits')
    endif
    call apply_liquid_volume_delta_to_storage( &
    &   river_storage_volume_m3, floodplain_storage_volume_m3, 1.0_JPRD, &
    &   update_is_valid, unavailable_liquid_volume_m3)
    call assert_true(update_is_valid, 'double-precision delta update is valid')
    call assert_true(transfer(river_storage_volume_m3, 0_JPIB) == &
    &   transfer(expected_river_storage_m3, 0_JPIB), &
    &   'double-precision delta preserves canonical low bits')
end subroutine test_double_precision_delta_application


subroutine assert_close(actual_value, expected_value, label)
    real(kind=JPRD), intent(in) :: &
    &   actual_value, &       ! [caller-defined unit] Computed value.
    &   expected_value        ! [caller-defined unit] Expected value.
    character(len=*), intent(in) :: &
    &   label                 ! [-] Human-readable assertion label.
    real(kind=JPRD) :: &
    &   tolerance             ! [caller-defined unit] Scaled comparison tolerance.

    tolerance = 1.0e-13_JPRD * max(1.0_JPRD, abs(expected_value))
    if (abs(actual_value - expected_value) <= tolerance) return
    write(*, '(a)') '[TEST FAILED] '//trim(label)
    write(*, '(a,es24.15)') '  actual   = ', actual_value
    write(*, '(a,es24.15)') '  expected = ', expected_value
    error stop 1
end subroutine assert_close


subroutine assert_true(condition, label)
    logical, intent(in) :: &
    &   condition       ! [-] Condition expected to be true.
    character(len=*), intent(in) :: &
    &   label           ! [-] Human-readable assertion label.

    if (condition) return
    write(*, '(a)') '[TEST FAILED] '//trim(label)
    error stop 1
end subroutine assert_true

end program test_water_storage_adapter
