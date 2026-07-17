module heat_flux_mod
    use PARKIND1,  only: &
    &   JPIM, JPRB
    use phys_const_mod, only: &
    &   EW, SB, TMELT, iceSWatten, Rsrf, Dbtm, Kw, Ksed, porosity, dsoil, &
    &   RW, GRAVITY_ACCEL, watSWref, ATTEN_SWD_RIV, ATTEN_SWD_LAK, &
    &   AIR_DENSITY, AIR_SPCHEAT, EVAP_EFF, &
    &   iceSWref, iceLWref, Kice2air
    use water_mod, only: &
    &   get_tetens_params, water_latent, saturated_specific_humidity, saturated_vapour_pressure
    !$ use omp_lib
    implicit none

    real(kind=JPRB), parameter :: CH_DEF = 1.2d-3, CE_DEF = 1.2d-3 ! default bulk coefficient

contains

! deriv
! T' = T + dT
! Flux(T') = F(T) + F'(T) * dT

! ===================================================================================================
! Shortwave radiation
! ===================================================================================================
subroutine calc_SWd(Hswd, &
&                   SWdn, SWref)
    real(kind=JPRB), intent(out) :: Hswd
    real(kind=JPRB), intent(in)  :: SWdn, SWref
    Hswd = (1.d0 - SWref) * SWdn
end subroutine calc_SWd

! ---------------------------------------------------------------------------------------------------
subroutine calc_SWd_absorb_surface( &
&   swdsrf, swdrst, &
&   swdin)
    real(kind=JPRB), intent(out) :: &
    &   swdsrf,& ! [W/m2] absorption by surface
    &   swdrst   ! [W/m2] rest
    real(kind=JPRB), intent(in) :: &
    &   swdin ! [W/m2]
    real(kind=JPRB), parameter :: &
    &   absorp_ratio = 0.4d0
    swdsrf = swdin * absorp_ratio
    swdrst = swdin - swdsrf
end subroutine calc_SWd_absorb_surface


subroutine calc_SWd_absorb_layer( &
&   swdlyr, swdrst, &
&   thklyr, swdin)
    real(kind=JPRB), intent(out) :: &
    &   swdlyr(:), & ! [W/m2] shortwave radiation absorbed in layers
    &   swdrst       ! [W/m2] rest (reach to bottom)
    real(kind=JPRB), intent(in)  :: &
    &   thklyr(:), & ! [m]    layer thickness
    &   swdin        ! [W/m2] incoming shortwave radiation
#if 0
    ! Bonan, 1996
    real(kind=JPRB), parameter :: &
    &   za_def = 0.6d0 ! [m]
    real(kind=JPRB) :: &
    &   za, z ! [m]
#endif
    integer(kind=JPIM) :: ilyr, nlyr
    nlyr      = size(swdlyr)
    swdlyr(:) = 0.d0
    swdrst = swdin
!    za     = min(za_def, thklyr(1))
    do ilyr = 1, nlyr
        swdlyr(ilyr) = swdrst * (1.d0 - exp(-ATTEN_SWD_LAK * thklyr(ilyr)))
!        z = max(thklyr(ilyr) - za, 0.d0)
!        swdlyr(ilyr) = swdrst * (1.d0 - exp(-ATTEN_SWD_LAK * z))
        swdrst       = swdrst - swdlyr(ilyr)
    enddo
end subroutine calc_SWd_absorb_layer


subroutine calc_SWd_absorb_layer_DA( &
&   swdsfc, swdlyr, swdbtm, &
&   thklyr, arelyr, swdin)
    ! consider the depth-area relationship
    real(kind=JPRB), intent(out) :: &
    &   swdsfc, swdlyr(:), swdbtm ! [W/m2]
    real(kind=JPRB), intent(in)  :: &
    &   thklyr(:), & ! [m]  layer thickness
    &   arelyr(:), & ! [m2] area of boundaries between layers
    &   swdin        ! [W/m2] incoming shortwave radiation
    real(kind=JPRB), parameter :: sfc_absorp = 0.4d0
    real(kind=JPRB) :: swdrst, k, ld, eld, uare, dare
    integer(kind=JPIM) :: ilyr, nlyr
    nlyr      = size(swdlyr)
    swdrst    = (1.d0 - watSWref) * swdin
    swdsfc    = swdrst * sfc_absorp
    swdrst    = swdrst - swdsfc
    do ilyr = 1, nlyr
!        if ( thklyr(ilyr) <= 0.d0 ) exit
        uare = arelyr(ilyr)
        if ( ilyr < nlyr ) then
            dare = arelyr(ilyr+1)
        else
            dare = arelyr(ilyr)!0.d0
        endif
        k   = ( uare - dare ) / thklyr(ilyr)
        ld  = ATTEN_SWD_LAK * thklyr(ilyr)
        eld = exp( -ld )
        swdlyr(ilyr) = swdrst * ( 1.d0 - eld ) &
        &            - swdrst * k * ( 1.d0 - ( ATTEN_SWD_LAK + 1.d0 ) * eld ) / ( ATTEN_SWD_LAK * uare )
        swdrst       = swdrst * eld
    enddo
    swdbtm = swdrst
end subroutine calc_SWd_absorb_layer_DA

! ===================================================================================================
! Downward Longwave Radiation
! ===================================================================================================
subroutine calc_LWd(Hlwd, &
&                   LWdn, LWref)
    real(kind=JPRB), intent(out) :: Hlwd
    real(kind=JPRB), intent(in)  :: LWdn, LWref
    Hlwd = (1.d0 - LWref) * LWdn
end subroutine calc_LWd


subroutine calc_LWdown(Hlwd, &
&                      LWdn, LWref)
    real(kind=JPRB), intent(out) :: Hlwd
    real(kind=JPRB), intent(in)  :: LWdn, LWref
    Hlwd = (1.d0 - LWref) * LWdn
end subroutine calc_LWdown

! ===================================================================================================
! Upward Longwave Radiation
! ===================================================================================================
subroutine calc_LWu(Hlwu, &
&                   Twat, LWemt)
    real(kind=JPRB), intent(out) :: Hlwu
    real(kind=JPRB), intent(in)  :: Twat, LWemt
    real(kind=JPRB) Twat_pow2

    Twat_pow2 = Twat * Twat
    Hlwu      = LWemt * sb * Twat_pow2 * Twat_pow2
end subroutine calc_LWu


subroutine deriv_LWup(Hlwd, Hlwd_deriv, Tsrf)
    real(kind=JPRB), intent(out) :: Hlwd, Hlwd_deriv
    real(kind=JPRB), intent(in)  :: Tsrf
    real(kind=JPRB) Tsrf2
    Tsrf2      = Tsrf * Tsrf
    Hlwd       =        EW * SB * Tsrf2 * Tsrf2
    Hlwd_deriv = 4.d0 * EW * SB * Tsrf2 * Tsrf
end subroutine deriv_LWup

! ===================================================================================================
subroutine calc_SWd_penetration_ice(Hswp, &
&                                   Hswd, iceD)
    real(kind=JPRB), intent(out) :: Hswp
    real(kind=JPRB), intent(in)  :: Hswd, iceD

    Hswp = exp(-1.d0 * iceSWatten * iceD) * Hswd
end subroutine calc_SWd_penetration_ice


pure elemental subroutine calc_ice_surface_heat_flux( &
    &   net_ice_heat_flux_w_m2, transmitted_shortwave_w_m2, &
    &   downward_shortwave_w_m2, downward_longwave_w_m2, &
    &   air_temperature_k, ice_thickness_m)
    real(kind=JPRB), intent(out) :: &
    &   net_ice_heat_flux_w_m2, &        ! [W m-2] Net atmospheric heat flux into ice; positive melts ice.
    &   transmitted_shortwave_w_m2       ! [W m-2] Shortwave radiation transmitted through the ice.
    real(kind=JPRB), intent(in) :: &
    &   downward_shortwave_w_m2, &       ! [W m-2] Downward shortwave radiation above the ice.
    &   downward_longwave_w_m2, &        ! [W m-2] Downward longwave radiation above the ice.
    &   air_temperature_k, &             ! [K] Near-surface air temperature.
    &   ice_thickness_m                   ! [m] Mean ice thickness over the ice-covered area.
    real(kind=JPRB) :: &
    &   absorbed_shortwave_before_attenuation_w_m2, & ! [W m-2] Non-reflected shortwave entering the ice.
    &   absorbed_shortwave_in_ice_w_m2, &             ! [W m-2] Shortwave absorbed within the ice.
    &   absorbed_downward_longwave_w_m2, &            ! [W m-2] Downward longwave absorbed by the ice.
    &   emitted_upward_longwave_w_m2, &               ! [W m-2] Upward longwave emitted by the ice surface.
    &   sensible_heat_into_ice_w_m2, &                ! [W m-2] Sensible heat transfer from air to ice.
    &   ice_surface_temperature_k, &                  ! [K] Diagnosed ice-surface temperature.
    &   surface_temperature_squared_k2                ! [K2] Squared ice-surface temperature.

    ice_surface_temperature_k = min(air_temperature_k, TMELT)
    absorbed_shortwave_before_attenuation_w_m2 = &
    &   (1.0_JPRB - iceSWref) * downward_shortwave_w_m2
    transmitted_shortwave_w_m2 = exp( &
    &   -iceSWatten * max(ice_thickness_m, 0.0_JPRB)) * &
    &   absorbed_shortwave_before_attenuation_w_m2
    absorbed_shortwave_in_ice_w_m2 = &
    &   absorbed_shortwave_before_attenuation_w_m2 - transmitted_shortwave_w_m2
    absorbed_downward_longwave_w_m2 = &
    &   (1.0_JPRB - iceLWref) * downward_longwave_w_m2
    surface_temperature_squared_k2 = &
    &   ice_surface_temperature_k * ice_surface_temperature_k
    emitted_upward_longwave_w_m2 = (1.0_JPRB - iceLWref) * SB * &
    &   surface_temperature_squared_k2 * surface_temperature_squared_k2
    sensible_heat_into_ice_w_m2 = Kice2air * &
    &   (air_temperature_k - ice_surface_temperature_k)

    net_ice_heat_flux_w_m2 = absorbed_shortwave_in_ice_w_m2 + &
    &   absorbed_downward_longwave_w_m2 - emitted_upward_longwave_w_m2 + &
    &   sensible_heat_into_ice_w_m2
end subroutine calc_ice_surface_heat_flux


subroutine calc_SWd_penetration_river(Hswp, &
&                                      Hswd, rivD)
    real(kind=JPRB), intent(out) :: Hswp
    real(kind=JPRB), intent(in)  :: Hswd, rivD
    real(kind=JPRB) frc, Rpnt

    frc  = (1.d0 - Rsrf) * (1.d0 - Dbtm)
    Rpnt = exp(-ATTEN_SWD_RIV * rivD)
    Hswp = frc * Rpnt * Hswd
end subroutine calc_SWd_penetration_river


subroutine calc_SWd_penetration_flood(Hswp, &
&                                      Hswd, fldD)
    real(kind=JPRB), intent(out) :: Hswp
    real(kind=JPRB), intent(in)  :: Hswd, fldD
    real(kind=JPRB) frc, Rpnt, kiw

    frc  = (1.d0 - Rsrf) * (1.d0 - Dbtm)
    kiw  = max(ATTEN_SWD_RIV * fldD, 1d-5)
    Rpnt = (1.0d0 - exp(-1.0d0 * kiw)) / kiw
    Hswp = frc * Rpnt * Hswd
end subroutine calc_SWd_penetration_flood


function sum_river_flood(rivFlx, fldFlx, rivWdh, fldWdh) result(allFlx)
    real(kind=JPRB) allFlx
    real(kind=JPRB), intent(in)  :: rivFlx, fldFlx, rivWdh, fldWdh
    if (fldWdh > 0.d0) then
        allFlx = (rivFlx * rivWdh + fldFlx * fldWdh) / (rivWdh + fldWdh)
    else
        allFlx = rivFlx
    endif
end function sum_river_flood


subroutine sum_river_flood_SWd(watHswd, &
&                               watHswd_riv, watHswp_riv, watHswd_fld, watHswp_fld, &
&                               rivW, fldW)
    real(kind=JPRB), intent(out) :: watHswd
    real(kind=JPRB), intent(in)  :: watHswd_riv, watHswp_riv, watHswd_fld, watHswp_fld, &
    &                                rivW, fldW
    real(kind=JPRB) rivHswd, fldHswd

    if (fldW > 0.d0) then
        rivHswd = watHswd_riv - watHswp_riv
        fldHswd = watHswd_fld - watHswp_fld
        watHswd = (rivHswd * rivW + fldHswd * fldW) &
        &       / (rivW + fldW)
    else
        watHswd = watHswd_riv - watHswp_riv
    endif
end subroutine sum_river_flood_SWd

! ===================================================================================================
! bulk method
! ===================================================================================================
function bulk_cor(Wind, Triv, Tair) result(rc)
    real(kind=JPRB) :: rc
    real(kind=JPRB), intent(in) :: Wind, Triv, Tair
    real(kind=JPRB) :: stability, stability0

    if (Wind == 0.0_JPRB) then
        rc = 0.0_JPRB
    else
        stability0 = (Triv - Tair) / (Wind * Wind)
        stability  = stability0 * (abs(stability0) / (abs(stability0) + 0.01_JPRB))
        if (Triv < Tair) then
            if (stability < -3.3_JPRB) then
                rc = 0.0_JPRB
            elseif (stability < 0.0_JPRB) then
                rc = 0.1_JPRB + 0.03_JPRB * stability + 0.9_JPRB * exp(4.8_JPRB * stability)
           else
                rc = 1.0_JPRB
            endif
        else
            rc = 1.0_JPRB + 0.63_JPRB * sqrt(stability)
        endif
    endif
end function bulk_cor


function sensible_coef(CHD, Wind) result(coef)
    real(kind=JPRB) :: coef
    real(kind=JPRB), intent(in) :: CHD, Wind
    coef = AIR_DENSITY * AIR_SPCHEAT * CHD * Wind
end function sensible_coef


function latent_coef(Twat, CHD, Wind) result(coef)
    real(kind=JPRB) :: coef
    real(kind=JPRB), intent(in) :: Twat, CHD, Wind
    real(kind=JPRB) :: L
    L    = water_latent(Twat)
    coef = AIR_DENSITY * L * CHD * Wind * EVAP_EFF
end function latent_coef

! ---------------------------------------------------------------------------------------------------
subroutine calc_bulk(Hsns, Hltn, &
&                    Psrf, Qair, Tair, Wind, Triv)
    real(kind=JPRB), intent(out) :: Hsns, Hltn
    real(kind=JPRB), intent(in)  :: Psrf, Qair, Tair, Wind, Triv
    real(kind=JPRB) CHD, Qsat, rc
    rc   = bulk_cor(Wind, Triv, Tair)
    CHD  = CH_DEF * rc
    Hsns = sensible_coef(CHD, Wind) * (Triv - Tair)
    Hltn = 0.d0
    Qsat = saturated_specific_humidity(Triv, Psrf)
    Hltn = latent_coef(Triv, CHD, Wind) * (Qsat - Qair)
end subroutine calc_bulk


subroutine deriv_bulk(Hsns, dHsns_dT, Hltn, dHltn_dT, &
&                     Wind, Tair, Qair, Psrf, Tsrf, sns_only)
    real(kind=JPRB), intent(out) :: Hsns, dHsns_dT, Hltn, dHltn_dT
    real(kind=JPRB), intent(in)  :: Wind, Tair, Qair, Psrf, Tsrf
    logical, intent(in), optional :: sns_only
    real(kind=JPRB) rc, CHcor, CEcor, coef, esat, desat_dT, K1, Qsat, Tetensa, Tetensb

    rc    = bulk_cor(Wind, Tsrf, Tair)
    CHcor = rc * CH_DEF
    CEcor = rc * CE_DEF
    coef  = sensible_coef(CHcor, Wind)

    Hsns     = coef * (Tsrf - Tair)
    dHsns_dT = coef
    if ( present( sns_only ) .and. sns_only ) return

    K1       = latent_coef(Tsrf, CHcor, Wind)
    esat     = saturated_vapour_pressure(Tsrf)
    call get_tetens_params( Tetensa, Tetensb, Tsrf )
    desat_dT = esat * log(10.d0) * Tetensa * Tetensb / ((Tsrf + Tetensb) * (Tsrf + Tetensb))
    Qsat     = saturated_specific_humidity(Tsrf, Psrf)

    Hltn     = K1 * (Qsat - Qair)
    dHltn_dT = K1 * (0.622d0 / Psrf) * desat_dT
end subroutine deriv_bulk

! ===================================================================================================
! Hbotm : Webb and Zhang, 1997; soil temperature gradient(Tg) : Westhoff, 2007
subroutine calc_bottom_heat(Hbotm, &
&                            Tgnd, Triv)
    real(kind=JPRB), intent(out) :: Hbotm
    real(kind=JPRB), intent(in)  :: Tgnd, Triv
    real(kind=JPRB) Ksoil

    Ksoil = Ksed * (1.d0 - porosity) + Kw * porosity
    Hbotm = Ksoil * (Tgnd - Triv) / dsoil
end subroutine calc_bottom_heat

! ===================================================================================================
subroutine calc_friction(Hfric, waterDepth, waterVelocity, bedSlope)
    real(kind=JPRB), intent(out) :: Hfric
    real(kind=JPRB), intent(in)  :: waterDepth, waterVelocity, bedSlope
    real(kind=JPRB), parameter   :: MAX_VELOCITY = 5.d0
    real(kind=JPRB) absVelocity
    absVelocity = min(abs(waterVelocity), MAX_VELOCITY)
    Hfric = RW * waterDepth * GRAVITY_ACCEL * absVelocity * abs(bedSlope)
end subroutine calc_friction

! ===================================================================================================
! F = - k * ( Tu - Td ) / dz, +: upward
subroutine calc_heat_conduction( F, &
&                                Tu, Td, dz, k )
    real(kind=JPRB), intent(out) :: F ! [W/m2]   conductive heat flux (upward)
    real(kind=JPRB), intent(in)  :: &
    &   Tu, & ! [K] upper temperature
    &   Td, & ! [K] lower temperature
    &   dz, & ! [m] distance
    &   k     ! [W/m/K] thermal conductivity
    real(kind=JPRB) :: coef
    real(kind=JPRB), parameter :: dz_min = 0.01d0 ! threshold to calculate the flux
    F = 0.d0
    if ( dz < dz_min ) return
    coef  = -k / dz
    F     = coef * ( Tu - Td )
end subroutine calc_heat_conduction


subroutine deriv_heat_conduction( F, dF_dT, &
&                                 Tu, Td, dz, k )
    real(kind=JPRB), intent(out) :: &
    &   F, &  ! [W/m2]   conductive heat flux (upward)
    &   dF_dT ! [W/m2/K] dF/dTu
    real(kind=JPRB), intent(in)  :: &
    &   Tu, & ! [K] upper temperature
    &   Td, & ! [K] lower temperature
    &   dz, & ! [m] distance
    &   k     ! [W/m/K] thermal conductivity
    real(kind=JPRB) :: coef
    real(kind=JPRB), parameter :: dz_min = 0.01d0 ! threshold to calculate the flux
    F     = 0.d0
    dF_dT = 0.d0
    if ( dz < dz_min ) return
    coef  = -k / dz
    F     = coef * ( Tu - Td )
    dF_dT = coef
end subroutine deriv_heat_conduction

end module heat_flux_mod
