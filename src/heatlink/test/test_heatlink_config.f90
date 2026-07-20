program test_heatlink_config
    use heatlink_config_mod, only: &
    &   LICE, NNEWTON_MAX_ICE, init_heatlink_config
    implicit none

    call init_heatlink_config( &
    &   'test/heatlink_config_defaults.nml', 6, .false., .false.)
    if (LICE) error stop 'LICE default is not false'
    if (NNEWTON_MAX_ICE /= 4) error stop 'NNEWTON_MAX_ICE default is not 4'

    call init_heatlink_config( &
    &   'test/heatlink_config_enabled.nml', 6, .false., .false.)
    if (.not. LICE) error stop 'LICE was not read from NHEATLINK'
    if (NNEWTON_MAX_ICE /= 7) error stop 'NNEWTON_MAX_ICE was not read from NHEATLINK'

    write(*, '(a)') 'test_heatlink_config: PASS'
end program test_heatlink_config
