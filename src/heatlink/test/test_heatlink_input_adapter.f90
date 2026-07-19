program test_heatlink_input_adapter
    use PARKIND1, only: &
    &   JPIM, JPRB
    use phys_const_mod, only: &
    &   TMELT
    use heatlink_input_adapter_mod, only: &
    &   enforce_liquid_inflow_temperature
    implicit none

    call test_temperature_is_limited_at_melting_point()
    call test_valid_temperature_is_unchanged()

    write(*, '(a)') '[ALL TESTS PASSED] test_heatlink_input_adapter'

contains

subroutine test_temperature_is_limited_at_melting_point()
    real(kind=JPRB) :: &
    &   temperature_k(5) ! [K] Synthetic liquid-water inflow temperatures.

    temperature_k(:) = [ &
    &   TMELT - 20.0_JPRB, &
    &   TMELT - 1.0e-6_JPRB, &
    &   TMELT, &
    &   TMELT + 1.0e-6_JPRB, &
    &   300.0_JPRB]

    call enforce_liquid_inflow_temperature(temperature_k)

    call assert_exact(temperature_k(1), TMELT, 'cold inflow is limited to TMELT')
    call assert_exact(temperature_k(2), TMELT, 'slightly cold inflow is limited to TMELT')
    call assert_exact(temperature_k(3), TMELT, 'TMELT inflow is unchanged')
    call assert_exact(temperature_k(4), TMELT + 1.0e-6_JPRB, 'warm inflow is unchanged')
    call assert_exact(temperature_k(5), 300.0_JPRB, 'hot inflow is unchanged')
end subroutine test_temperature_is_limited_at_melting_point


subroutine test_valid_temperature_is_unchanged()
    real(kind=JPRB) :: &
    &   temperature_k(3), & ! [K] Valid liquid-water inflow temperatures.
    &   initial_temperature_k(3) ! [K] Exact reference temperatures.
    integer(kind=JPIM) :: &
    &   iseq ! [-] Synthetic cell index.

    temperature_k(:) = [TMELT, TMELT + 0.125_JPRB, 315.75_JPRB]
    initial_temperature_k(:) = temperature_k(:)

    call enforce_liquid_inflow_temperature(temperature_k)

    do iseq = 1, size(temperature_k)
        if (temperature_k(iseq) == initial_temperature_k(iseq)) cycle
        write(*, '(a,i0)') '[TEST FAILED] valid temperature changed at index ', iseq
        error stop 1
    enddo
end subroutine test_valid_temperature_is_unchanged


subroutine assert_exact(actual_value, expected_value, label)
    real(kind=JPRB), intent(in) :: &
    &   actual_value, & ! [K] Computed temperature.
    &   expected_value ! [K] Exact expected temperature.
    character(len=*), intent(in) :: &
    &   label ! [-] Human-readable assertion label.

    if (actual_value == expected_value) return
    write(*, '(a)') '[TEST FAILED] '//trim(label)
    write(*, '(a,es24.15)') '  actual   = ', actual_value
    write(*, '(a,es24.15)') '  expected = ', expected_value
    error stop 1
end subroutine assert_exact

end program test_heatlink_input_adapter
