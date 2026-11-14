module heatlink_velocity_mod
    use PARKIND1, only: &
    &   JPRB
    implicit none
    private

    real(kind=JPRB), parameter :: &
    &   FLOW_CROSS_SECTION_MIN_M2 = 1.0e-5_JPRB

    public :: diagnose_flow_velocity, floodplain_flow_cross_section

contains

    pure elemental function diagnose_flow_velocity( &
        &   discharge_m3s, cross_section_m2) result(velocity_ms)
        real(kind=JPRB), intent(in) :: &
        &   discharge_m3s, cross_section_m2
        real(kind=JPRB) :: &
        &   velocity_ms

        if (cross_section_m2 > FLOW_CROSS_SECTION_MIN_M2) then
            velocity_ms = discharge_m3s / cross_section_m2
        else
            velocity_ms = 0.0_JPRB
        endif
    end function diagnose_flow_velocity


    pure elemental function floodplain_flow_cross_section( &
        &   flood_storage_m3, river_length_m, flood_depth_m, &
        &   river_width_m) result(cross_section_m2)
        real(kind=JPRB), intent(in) :: &
        &   flood_storage_m3, river_length_m, flood_depth_m, river_width_m
        real(kind=JPRB) :: &
        &   cross_section_m2

        if (river_length_m > 0.0_JPRB) then
            cross_section_m2 = max( &
            &   flood_storage_m3 / river_length_m - &
            &   flood_depth_m * river_width_m, 0.0_JPRB)
        else
            cross_section_m2 = 0.0_JPRB
        endif
    end function floodplain_flow_cross_section

end module heatlink_velocity_mod
