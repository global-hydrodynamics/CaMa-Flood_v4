module time_mod
    use PARKIND1, only: &
    &   JPIM
    use const_mod, only: &
    &   CLEN_SHORT
    use text_mod, only: &
    &   to_lowercase
    implicit none
contains

integer(kind=JPIM) function dt2sec(dt, dt_unit) result(dt_sec)
    integer(kind=JPIM), intent(in) :: dt
    character(len=CLEN_SHORT), intent(in) :: dt_unit

    character(len=CLEN_SHORT) :: u

    u = to_lowercase(trim(dt_unit))

    select case (trim(u))
    case ('sec')
        dt_sec = dt
    case ('hour')
        dt_sec = dt * 3600_JPIM
    case ('day')
        dt_sec = dt * 86400_JPIM
    case default
        write(*, *) '[dt2sec ERROR] invalid dt_unit: ', trim(dt_unit)
        stop
    end select
end function dt2sec

end module time_mod