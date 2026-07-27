module heatlink_config_mod
    implicit none
    private

    integer, parameter :: CONFIG_INTEGER_KIND = selected_int_kind(9)
    logical, public, save :: LICE = .false.
    integer(kind=CONFIG_INTEGER_KIND), public, save :: NNEWTON_MAX_ICE = 4

    public :: init_heatlink_config

contains

    subroutine init_heatlink_config(nml_path, log_unit, lwevap, llevee)
        character(len=*), intent(in) :: nml_path
        integer, intent(in) :: log_unit
        logical, intent(in) :: lwevap, llevee

        integer :: nml_unit, ios
        namelist /NHEATLINK/ LICE, NNEWTON_MAX_ICE

        LICE = .false.
        NNEWTON_MAX_ICE = 4

        open(newunit=nml_unit, file=trim(nml_path), status='old', &
        &   action='read', iostat=ios)
        if (ios /= 0) then
            write(log_unit, '(a,1x,a)') &
            &   'ERROR: heatlink could not open namelist:', trim(nml_path)
            error stop 1
        endif

        read(nml_unit, nml=NHEATLINK, iostat=ios)
        close(nml_unit)
        if (ios > 0) then
            write(log_unit, '(a,1x,a)') &
            &   'ERROR: invalid NHEATLINK namelist in:', trim(nml_path)
            error stop 1
        endif

        if (NNEWTON_MAX_ICE < 1) then
            write(log_unit, '(a)') &
            &   'ERROR: NNEWTON_MAX_ICE must be at least one.'
            error stop 1
        endif
        if (lwevap) then
            write(log_unit, '(a)') &
            &   'ERROR: LHEATLINK and LWEVAP cannot be enabled together.'
            write(log_unit, '(a)') &
            &   '       Heatlink does not yet include evaporation water, sensible-heat, and latent-heat losses.'
            error stop 1
        endif
        if (llevee) then
            write(log_unit, '(a)') &
            &   'ERROR: LHEATLINK and LLEVEE cannot be enabled together.'
            write(log_unit, '(a)') &
            &   '       Heatlink does not support levee storage in its local heat budget.'
            error stop 1
        endif

        write(log_unit, '(a)') ''
        write(log_unit, '(a)') '=== NAMELIST, NHEATLINK ==='
        write(log_unit, '(a,l2)') 'LICE             ', LICE
        write(log_unit, '(a,i0)') 'NNEWTON_MAX_ICE  ', NNEWTON_MAX_ICE
    end subroutine init_heatlink_config

end module heatlink_config_mod
