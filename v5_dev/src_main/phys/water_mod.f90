module water_mod
    use PARKIND1, only: &
    &   JPRB
    use phys_const_mod, only: &
    &   TMELT
    implicit none
    private
    public :: &
    &   get_tetens_params, &
    &   saturated_vapour_pressure, vapour_pressure2specific_humidity, saturated_specific_humidity, &
    &   specific_humidity2vapour_pressure, water_latent

contains

subroutine get_tetens_params( a, b, t )
    real(kind=JPRB), intent(out) :: a, b
    real(kind=JPRB), intent(in)  :: t ! [K]
    if ( t >= TMELT ) then
        a = 7.5d0
        b = 237.3d0
    else
        a = 9.5d0
        b = 265.d0
    endif
end subroutine get_tetens_params


function saturated_vapour_pressure(t) result(ew)
    ! Tetens, 1930 [hPa] = [mbar]
    real(kind=JPRB) ew ! [hPa]
    real(kind=JPRB), intent(in) :: t ! [K]
    real(kind=JPRB) pow, a, b, Tc ! [oC]
    call get_tetens_params( a, b, t )
    Tc  = t - TMELT
    pow = a * Tc / ( Tc + b )
    ew  = 6.1078d0 * (10.d0 ** pow)
end function saturated_vapour_pressure


function vapour_pressure2specific_humidity(ew, Pall) result(q)
    real(kind=JPRB) q
    real(kind=JPRB), intent(in) :: ew, Pall
    real(kind=JPRB) r
    r = ew / Pall                          ! [hPa/hPa]
    q = 0.622d0 * r / (1.d0 - 0.378d0 * r) ! [kg/kg]
end function vapour_pressure2specific_humidity


function saturated_specific_humidity(T, Psrf) result(Qsat)
    real(kind=JPRB) Qsat
    real(kind=JPRB), intent(in)  :: T, Psrf
    real(kind=JPRB) ew
    ew = saturated_vapour_pressure(T) ! [hPa]
    Qsat = vapour_pressure2specific_humidity(ew, Psrf)
end function saturated_specific_humidity


function specific_humidity2vapour_pressure(q, Pair) result(ew)
    real(kind=JPRB) ew
    real(kind=JPRB), intent(in) :: q, Pair
    ew = q * Pair / 0.622d0
end function specific_humidity2vapour_pressure

! ===================================================================================================
function water_latent(t) result(l)
    real(kind=JPRB) l ! [J/kg] latent heat of water
    real(kind=JPRB) t ! [K]    temperature
    l = 2.5d6 - 2400.d0 * (t - TMELT)
end function water_latent

end module water_mod
