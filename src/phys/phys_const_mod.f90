module phys_const_mod
    use PARKIND1, only: &
    &   JPRB
!    use funit_lib, only: TMP_UNIT, LOG_UNIT
!    use glob_mod, only: NML_PATH
    implicit none
    private
    public :: &
!    &   init_phys_const_mod, &
    &   CW, RW, VISC_WAT, FUSION_HEAT_WAT, &
    &   RIVDPH_MIN, ICETHK_MIN, &
    &   watSWref, watLWref, iceSWref, ICE_LONGWAVE_EMISSIVITY, iceSWatten, &
    &   ew, sb, Rsrf, Dbtm, ATTEN_SWD_RIV, ATTEN_SWD_LAK, &
    &   AIR_DENSITY, AIR_SPCHEAT, EVAP_EFF, &
    &   Tdelta, cp, g, rair, cair, Dc, Kw, Ksed, porosity, dsoil, &
    &   Tfreeze, TMELT, CI, RI, HFUS, KI, iceFrac_stop, LAIratio, Kice2wat, Kice2air, &
    &   WAT2ICE_VOL, ICE2WAT_VOL, PI, FEET2METER, DAY2SEC, GRAVITY_ACCEL, &
    &   DIV_RI, DIV_RW, RI_HFUS, RI_DIV_RW, RW_DIV_RI, &
    &   LAKICE_SHAPE_COEF, &
    &   DRAG_COEF_10, ANGULER_SPEED_EARTH, KARMAN_CONST

    real(kind=JPRB), parameter :: CW = 4184.d0   ! water heat capacity [J/kg*K]
    real(kind=JPRB), parameter :: RW = 1000.d0   ! water density [kg/m3]
    real(kind=JPRB), parameter :: VISC_WAT = 0.01007d0 * 1.d-4 ! [m2/s] viscosity
    real(kind=JPRB), parameter :: FUSION_HEAT_WAT = 2442.d6 ![J/kg]

    ! river section
    real(kind=JPRB), parameter :: RIVDPH_MIN = 0.3d0
    real(kind=JPRB), parameter :: ICETHK_MIN = 0.01d0

    ! radiation
    real(kind=JPRB), parameter :: watSWref = 0.1d0  ! albedo []
    real(kind=JPRB), parameter :: watLWref = 0.03d0
    real(kind=JPRB), parameter :: iceSWref = 0.6d0
    ! Broadband longwave emissivity of bare ice from Wang et al. (2010),
    ! Journal of Geophysical Research: Oceans, doi:10.1029/2009JC005857.
    real(kind=JPRB), parameter :: ICE_LONGWAVE_EMISSIVITY = 0.988_JPRB ! [-]
    real(kind=JPRB), parameter :: iceSWatten = 10.d0
    real(kind=JPRB), parameter :: ew = 0.97d0   ! water emissivity []
    real(kind=JPRB), parameter :: sb = 5.670367d-8 ! Stefan-Boltzmann constant
    ! attenuation
    real(kind=JPRB), parameter :: Rsrf = 0.6d0 ! fraction absorbed in water surface layer : Webb and Zhang, 1997
    real(kind=JPRB), parameter :: Dbtm = 0.2d0 ! fraction reflected by stream bed surface
    real(kind=JPRB) :: ATTEN_SWD_RIV = 0.1d0 ! [/m] attenuation coefficient for short wave (river)
    real(kind=JPRB) :: ATTEN_SWD_LAK = 0.1d0 ! [/m] attenuation coefficient for short wave (lake )
    namelist /water_radiation/ ATTEN_SWD_RIV, ATTEN_SWD_LAK


    ! sensible / latent heat
    real(kind=JPRB), parameter :: AIR_DENSITY = 1.19d0
    real(kind=JPRB), parameter :: AIR_SPCHEAT = 1005.d0
    real(kind=JPRB), parameter :: EVAP_EFF = 1.02d0
    real(kind=JPRB), parameter :: Tdelta = 273.15d0   ! conversion celsuys and abs temperature
    real(kind=JPRB), parameter :: cp = 1.007d0   ! specific heat at const press [kJ/kg K]
    real(kind=JPRB), parameter :: g  = 0.66d0    ! [kPa / C] : Psychrometric constant : Dingman, 2002
    real(kind=JPRB), parameter :: rair = 1.2d0   ! [kg / m3] : denstity of air : Williams, 2006
    real(kind=JPRB), parameter :: cair = 1004.d0 ! [J / kg C] : Specific heat capacity of air : Dingman, 2002
    real(kind=JPRB), parameter :: Dc = 1.466d-7 ! diffusion coefficient [m2/sec]
    real(kind=JPRB), parameter :: Kw = 0.6d0 ! thermal conductivity of water[W/mC] : Boyd and Kasper, 2003
    real(kind=JPRB), parameter :: Ksed = 3.4d0 ! thermal conductivity of the sediment[W/mC] : Shi et al., 1996
    real(kind=JPRB), parameter :: porosity = 0.3d0 ! porosity[] : Westhoff, 2007
    real(kind=JPRB), parameter :: dsoil = 2.0d0!0.071d0 ! thickness of substrate layer[m] : Westhoff, 2007

    ! ice
    real(kind=JPRB), parameter :: Tfreeze = 273.15d0 ! [K] freezing point
    real(kind=JPRB), parameter :: TMELT = 273.15d0
    real(kind=JPRB), parameter :: CI = 2100.d0 ! [J/kg*k] : ice heat capacity
    real(kind=JPRB), parameter :: RI = 916.71d0 ! [kg/m3] : ice density
    real(kind=JPRB), parameter :: HFUS = 333500.d0 ! [J/kg] : heat of fusion
    real(kind=JPRB), parameter :: KI = 2.25 ! [W/m/K] thermal conductivity Goyette et al. 2000

    real(kind=JPRB), parameter :: iceFrac_stop = 0.8d0
    real(kind=JPRB), parameter :: LAIratio = 0.3d0

    real(kind=JPRB), parameter :: Kice2wat = 8.d0, Kice2air = 20.d0
    real(kind=JPRB), parameter :: WAT2ICE_VOL = RW / RI
    real(kind=JPRB), parameter :: ICE2WAT_VOL = RI / RW


    real(kind=JPRB), parameter :: PI = acos(-1.d0)
    real(kind=JPRB), parameter :: FEET2METER = 1.d0 / 0.3048d0
    real(kind=JPRB), parameter :: DAY2SEC = 1.d0 / 86400.d0

    real(kind=JPRB), parameter :: GRAVITY_ACCEL = 9.80665d0 ! gravity acceleration [m/s2]

    real(kind=JPRB) :: &
    &   DIV_RI = 1.d0 / RI, &
    &   DIV_RW = 1.d0 / RW, &
    &   RI_HFUS = RI * HFUS, &
    &   RI_DIV_RW = RI / RW, & ! e.g. ice volume to water volume
    &   RW_DIV_RI = RW / RI

    real(kind=JPRB), parameter :: LAKICE_SHAPE_COEF = 9.2d8

    ! Layer
    real(kind=JPRB), parameter :: DRAG_COEF_10 = 1.3d-3 ! C10 in Bonnet, 2010
    real(kind=JPRB), parameter :: ANGULER_SPEED_EARTH = 7.29212d-5 ! [-s]
    real(kind=JPRB), parameter :: KARMAN_CONST = 0.4d0

!contains
!
!subroutine init_phys_const_mod
!    write(LOG_UNIT, '(a)') '[phys_const_mod/init_phys_const_mod]'
!    open (TMP_UNIT, file=trim(NML_PATH), status='old')
!    read (TMP_UNIT, water_radiation)
!    close(TMP_UNIT)
!    write(LOG_UNIT, '(a,f0.3)') '    ATTEN_SWD_RIV = ', ATTEN_SWD_RIV
!    write(LOG_UNIT, '(a,f0.3)') '    ATTEN_SWD_LAK = ', ATTEN_SWD_LAK
!    write(LOG_UNIT, *) ''
!end subroutine init_phys_const_mod

end module phys_const_mod
