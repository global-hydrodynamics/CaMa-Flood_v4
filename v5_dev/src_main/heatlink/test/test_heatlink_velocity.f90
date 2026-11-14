program test_heatlink_velocity
    use PARKIND1, only: &
    &   JPRB
    use heatlink_velocity_mod, only: &
    &   diagnose_flow_velocity, floodplain_flow_cross_section
    implicit none

    real(kind=JPRB), parameter :: TOLERANCE = 1.0e-12_JPRB
    real(kind=JPRB) :: cross_section_m2

    call assert_close( &
    &   diagnose_flow_velocity(24.0_JPRB, 6.0_JPRB), 4.0_JPRB, &
    &   'positive discharge velocity')
    call assert_close( &
    &   diagnose_flow_velocity(-12.0_JPRB, 6.0_JPRB), -2.0_JPRB, &
    &   'negative discharge velocity')
    call assert_close( &
    &   diagnose_flow_velocity(1.0_JPRB, 1.0e-6_JPRB), 0.0_JPRB, &
    &   'dry cross-section velocity')

    cross_section_m2 = floodplain_flow_cross_section( &
    &   240.0_JPRB, 10.0_JPRB, 2.0_JPRB, 5.0_JPRB)
    call assert_close(cross_section_m2, 14.0_JPRB, &
    &   'floodplain cross-section')
    call assert_close( &
    &   floodplain_flow_cross_section(1.0_JPRB, 0.0_JPRB, &
    &       0.0_JPRB, 0.0_JPRB), &
    &   0.0_JPRB, 'zero-length cross-section')

    write(*, '(a)') 'test_heatlink_velocity: PASS'

contains

    subroutine assert_close(actual, expected, label)
        real(kind=JPRB), intent(in) :: actual, expected
        character(len=*), intent(in) :: label

        if (abs(actual - expected) > TOLERANCE) then
            write(*, '(a,2(1x,es16.8))') trim(label), actual, expected
            error stop 1
        endif
    end subroutine assert_close

end program test_heatlink_velocity
