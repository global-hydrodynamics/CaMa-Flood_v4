module water_storage_adapter_mod
    use, intrinsic :: ieee_arithmetic, only: &
    &   ieee_is_finite
    use PARKIND1, only: &
    &   JPRD
    implicit none
    private

    public :: &
    &   apply_liquid_volume_delta_to_storage

contains

pure elemental subroutine apply_liquid_volume_delta_to_storage( &
    &   river_storage_volume_m3, floodplain_storage_volume_m3, &
    &   liquid_volume_delta_m3, update_is_valid, unavailable_liquid_volume_m3)
    real(kind=JPRD), intent(inout) :: &
    &   river_storage_volume_m3, &      ! [m3] Canonical CaMa river storage before and after phase change.
    &   floodplain_storage_volume_m3    ! [m3] Canonical CaMa floodplain storage before and after phase change.
    real(kind=JPRD), intent(in) :: &
    &   liquid_volume_delta_m3          ! [m3] Final minus initial liquid-water volume from local phase change.
    logical, intent(out) :: &
    &   update_is_valid                 ! [-] True when the requested delta can be applied safely.
    real(kind=JPRD), intent(out) :: &
    &   unavailable_liquid_volume_m3    ! [m3] Requested removal exceeding available liquid storage.
    real(kind=JPRD) :: &
    &   initial_river_storage_m3, &     ! [m3] River storage before applying the liquid-volume delta.
    &   initial_floodplain_storage_m3, & ! [m3] Floodplain storage before applying the liquid-volume delta.
    &   initial_total_storage_m3, &     ! [m3] Total liquid storage before applying the delta.
    &   river_storage_fraction, &       ! [-] Initial fraction of liquid storage held in the river.
    &   requested_river_delta_m3, &     ! [m3] Delta allocated to river storage before rounding.
    &   applied_river_delta_m3          ! [m3] Delta represented after updating river storage.

    update_is_valid = .false.
    unavailable_liquid_volume_m3 = 0.0_JPRD
    if (.not. ( &
    &   ieee_is_finite(river_storage_volume_m3) .and. &
    &   ieee_is_finite(floodplain_storage_volume_m3) .and. &
    &   ieee_is_finite(liquid_volume_delta_m3))) return
    if (river_storage_volume_m3 < 0.0_JPRD .or. &
    &   floodplain_storage_volume_m3 < 0.0_JPRD) return

    update_is_valid = .true.
    if (liquid_volume_delta_m3 == 0.0_JPRD) return

    initial_river_storage_m3 = river_storage_volume_m3
    initial_floodplain_storage_m3 = floodplain_storage_volume_m3
    initial_total_storage_m3 = initial_river_storage_m3 + &
    &   initial_floodplain_storage_m3
    if (liquid_volume_delta_m3 < 0.0_JPRD) then
        unavailable_liquid_volume_m3 = max( &
        &   -liquid_volume_delta_m3 - initial_total_storage_m3, 0.0_JPRD)
        if (unavailable_liquid_volume_m3 > 0.0_JPRD) then
            update_is_valid = .false.
            return
        endif
        if (-liquid_volume_delta_m3 == initial_total_storage_m3) then
            river_storage_volume_m3 = 0.0_JPRD
            floodplain_storage_volume_m3 = 0.0_JPRD
            return
        endif
    endif

    if (initial_total_storage_m3 > 0.0_JPRD) then
        river_storage_fraction = initial_river_storage_m3 / initial_total_storage_m3
        requested_river_delta_m3 = liquid_volume_delta_m3 * river_storage_fraction
        river_storage_volume_m3 = initial_river_storage_m3 + requested_river_delta_m3
        applied_river_delta_m3 = river_storage_volume_m3 - initial_river_storage_m3
        floodplain_storage_volume_m3 = initial_floodplain_storage_m3 + &
        &   (liquid_volume_delta_m3 - applied_river_delta_m3)
    else
        river_storage_volume_m3 = initial_river_storage_m3 + liquid_volume_delta_m3
    endif
end subroutine apply_liquid_volume_delta_to_storage

end module water_storage_adapter_mod
