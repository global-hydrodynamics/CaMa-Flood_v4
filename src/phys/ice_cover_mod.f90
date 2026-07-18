module ice_cover_mod
    use PARKIND1, only: &
    &   JPRB
    implicit none
    private

    public :: &
    &   ICE_THICKNESS_MIN_M, ICE_THICKNESS_FULL_COVER_M, &
    &   ICE_FRACTION_AT_MIN_THICKNESS, &
    &   diagnose_ice_geometry, enforce_surface_ice_capacity

    real(kind=JPRB), parameter :: &
    &   ICE_THICKNESS_MIN_M = 0.05_JPRB, &
    &   ICE_THICKNESS_FULL_COVER_M = 0.5_JPRB, &
    &   ICE_FRACTION_AT_MIN_THICKNESS = 0.1_JPRB

contains

pure elemental subroutine diagnose_ice_geometry( &
    &   ice_volume_m3, maximum_surface_area_m2, &
    &   ice_area_m2, ice_thickness_m, ice_fraction)
    real(kind=JPRB), intent(in) :: &
    &   ice_volume_m3, &          ! [m3] Ice volume represented by this shape.
    &   maximum_surface_area_m2   ! [m2] Maximum horizontal area available to the ice.
    real(kind=JPRB), intent(out) :: &
    &   ice_area_m2, &            ! [m2] Diagnosed horizontal ice area.
    &   ice_thickness_m, &        ! [m] Mean thickness over the diagnosed ice area.
    &   ice_fraction              ! [-] Fraction of the maximum surface area covered by ice.
    real(kind=JPRB) :: &
    &   available_ice_volume_m3, & ! [m3] Nonnegative ice volume used by the diagnosis.
    &   available_surface_area_m2  ! [m2] Nonnegative maximum surface area.

    available_ice_volume_m3 = max(ice_volume_m3, 0.0_JPRB)
    available_surface_area_m2 = max(maximum_surface_area_m2, 0.0_JPRB)
    ice_area_m2 = 0.0_JPRB
    ice_thickness_m = 0.0_JPRB
    ice_fraction = 0.0_JPRB
    if (available_ice_volume_m3 <= 0.0_JPRB .or. &
    &   available_surface_area_m2 <= 0.0_JPRB) return

    if (available_ice_volume_m3 >= &
    &   available_surface_area_m2 * ICE_THICKNESS_FULL_COVER_M) then
        ice_area_m2 = available_surface_area_m2
        ice_thickness_m = available_ice_volume_m3 / ice_area_m2
        ice_fraction = 1.0_JPRB
    else if (available_ice_volume_m3 < available_surface_area_m2 * &
    &   ICE_THICKNESS_MIN_M * ICE_FRACTION_AT_MIN_THICKNESS) then
        ice_thickness_m = ICE_THICKNESS_MIN_M
        ice_area_m2 = available_ice_volume_m3 / ice_thickness_m
        ice_fraction = ice_area_m2 / available_surface_area_m2
    else
        ice_fraction = sqrt(available_ice_volume_m3 / &
        &   (available_surface_area_m2 * ICE_THICKNESS_FULL_COVER_M))
        ice_area_m2 = available_surface_area_m2 * ice_fraction
        ice_thickness_m = available_ice_volume_m3 / ice_area_m2
    endif
end subroutine diagnose_ice_geometry

pure elemental subroutine enforce_surface_ice_capacity( &
    &   surface_ice_volume_m3, excess_ice_volume_m3, &
    &   water_surface_area_m2, maximum_ice_thickness_m)
    real(kind=JPRB), intent(inout) :: &
    &   surface_ice_volume_m3, &     ! [m3] Ice retained on the water surface.
    &   excess_ice_volume_m3         ! [m3] Immobile excess ice retained in the river grid cell.
    real(kind=JPRB), intent(in) :: &
    &   water_surface_area_m2, &     ! [m2] Water-surface area available for ice cover.
    &   maximum_ice_thickness_m      ! [m] Maximum ice thickness retained on the water surface.
    real(kind=JPRB) :: &
    &   available_surface_ice_volume_m3, & ! [m3] Nonnegative water-surface ice available before transfer.
    &   maximum_surface_ice_volume_m3, &   ! [m3] Ice capacity of the available water surface.
    &   transferred_excess_m3        ! [m3] Newly immobile ice volume transferred during this call.

    available_surface_ice_volume_m3 = max(surface_ice_volume_m3, 0.0_JPRB)
    maximum_surface_ice_volume_m3 = max(water_surface_area_m2, 0.0_JPRB) * &
    &   max(maximum_ice_thickness_m, 0.0_JPRB)
    surface_ice_volume_m3 = min( &
    &   available_surface_ice_volume_m3, maximum_surface_ice_volume_m3)
    transferred_excess_m3 = available_surface_ice_volume_m3 - surface_ice_volume_m3
    excess_ice_volume_m3 = excess_ice_volume_m3 + transferred_excess_m3
end subroutine enforce_surface_ice_capacity

end module ice_cover_mod
