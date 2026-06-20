module eddy_mod
    use PARKIND1,  only: &
    &   JPIM, JPRB
    !use funit_lib, only: TMP_UNIT, LOG_UNIT
    !use glob_mod, only: &
    !&   NML_PATH
    use phys_const_mod, only: &
    &   AIR_DENSITY, DRAG_COEF_10, PI, ANGULER_SPEED_EARTH, KARMAN_CONST, &
    &   TMELT, GRAVITY_ACCEL, KW
    implicit none
    private
    public :: &
    !&   init_eddy_mod, &
    &   eddy_diffusion, density_mixing

    real(kind=JPRB),save :: &
    &   Kz_scale = 1.d0 ! [-] scale factor for vertical diffusivity, Kz
    !namelist /eddy_config/ Kz_scale

contains

!subroutine init_eddy_mod
!    write(LOG_UNIT, '(a)') '[eddy_mod/init_eddy_mod]'
!    open (TMP_UNIT, file=trim(NML_PATH), status='old')
!    read (TMP_UNIT, eddy_config)
!    close(TMP_UNIT)
!    write(LOG_UNIT, '(a,f0.2)') '    Kz_scale = ', Kz_scale
!    write(LOG_UNIT, *) ''
!end subroutine init_eddy_mod

! ===================================================================================================
! Wind Velocity profile (Kondo)
! ===================================================================================================
function change_wind_height(u1, z1, z2, z0) result(u2)
    ! assume log profile: u = a * log10(z/z0)
    ! Kondo, p.89
    real(kind=JPRB), intent(in) :: u1, z1, z2, z0
    real(kind=JPRB)             :: u2
    u2 = u1 * (log10(z2/z0) / log10(z1/z0))
end function change_wind_height


function wind10m_to_wind2m(u10) result(u2)
    ! same as change_wind_height, but z1 = 10, z2 = 2, z0 = 1.d-4
    ! Kondo, p.90
    real(kind=JPRB), intent(in) :: u10
    real(kind=JPRB)             :: u2
    u2 = 0.86d0 * u10
end function wind10m_to_wind2m

! ===================================================================================================
! Wind shear [Henderson-Sellers, 1985]
! ===================================================================================================
function calc_surface_shear_velocity(wind_2m) result(us)
    real(kind=JPRB) us
    real(kind=JPRB), intent(in) :: wind_2m
    real(kind=JPRB), parameter  :: us_min = 1d-8
    us = max( 1.2d-3 * abs( wind_2m ), us_min )
end function calc_surface_shear_velocity


function calc_shear_attenuation(wind_2m, lat_deg) result(ks)
    ! us(z) = us(z=0) * exp( -ks * z )
    real(kind=JPRB) :: ks
    real(kind=JPRB), intent(in) :: wind_2m, lat_deg
    real(kind=JPRB) :: lat_rad
    lat_rad = abs(lat_deg) * PI / 180.d0
    ks      = 6.6d0 * sqrt(sin(lat_rad)) / (wind_2m ** 1.84)
end function calc_shear_attenuation


function calc_shear_velocity( wind_10m, watdph, lat_deg ) result( us )
    real(kind=JPRB), intent(in) :: wind_10m, & ! [m/s] wind velocity (z=10m)
    &                      watdph, &   ! [m]   water depth
    &                      lat_deg     ! [deg] latitude
    real(kind=JPRB)             :: us ! [m/s] friction velocity
    real(kind=JPRB) :: wind_2m, us0, ks
    wind_2m = wind10m_to_wind2m( wind_10m )
    us0     = calc_surface_shear_velocity( wind_2m )
    ks      = calc_shear_attenuation( wind_2m, lat_deg )
    us      = us0 * exp( -ks * watdph )
end function calc_shear_velocity

! ===================================================================================================
! Water density
! ===================================================================================================
function calc_water_density(T_K) result(rw)
    real(kind=JPRB) :: &
    &   rw ! [kg/m3]
    real(kind=JPRB), intent(in) :: &
    &   T_K ! [K]
    real(kind=JPRB), parameter :: &
    &   rw_max = 0.99997d3, a = 0.00043d0
    rw = rw_max / (1.d0 + a * abs(T_K - TMELT - 4.d0))
end function calc_water_density

! ===================================================================================================
! Eddy diffusion
! ===================================================================================================
subroutine eddy_diffusion( Kzsfc, Kz, tmpsfc, tmplyr, thklyr, wind_10m, lat_deg)
    ! Henderson-Sellers, 1985
    real(kind=JPRB), intent(out) :: Kzsfc, Kz(:) ! [?] eddy diffusion coefficient (N-1 layers)
    ! flux = CW * RW * Kz * dT (dZ is not needed)
    real(kind=JPRB), intent(in)  :: &
    &   tmpsfc   , & ! [K]
    &   tmplyr(:), & ! [K] temperature of each layer (N layers)
    &   thklyr(:), & ! [m] thickness of each layer (N layers)
    &   wind_10m, & ! [m/s] wind velocity (z=10m)
    &   lat_deg ! [deg] latitude
    real(kind=JPRB) wind_2m, us, ks, z, dz, rw_up, rw_down, drw_dz, K0, Ri, rwsfc
    real(kind=JPRB), allocatable :: rw(:)
    integer(kind=JPIM) :: nlyr, ilyr

    nlyr    = size(thklyr)
    allocate( rw(nlyr) ); rw(:) = 0.d0
    do ilyr = 1, nlyr
        rw(ilyr) = calc_water_density(tmplyr(ilyr))
    enddo
    rwsfc = calc_water_density( tmpsfc )

    wind_2m = wind10m_to_wind2m(wind_10m)
    us      = calc_surface_shear_velocity(wind_2m)
    ks      = calc_shear_attenuation(wind_2m, lat_deg)
    z = 0.d0
!write(LOG_UNIT, *) wind_2m, us, ks

    z      = thklyr(1) * 0.25d0
    dz     = thklyr(1) * 0.5d0
    drw_dz = ( rw(1) - rwsfc ) / dz
    K0     = calc_K0(z, us, ks)
    Ri     = calc_Richardson_number(z, us, ks, drw_dz, (rw(1) + rwsfc) * 0.5d0)
    Kzsfc  = calc_Kz( K0, Ri, drw_dz )
    Kzsfc  = Kzsfc / dz

    do ilyr = 1, nlyr - 1
        z        = z + thklyr(ilyr)
        dz       = (thklyr(ilyr) + thklyr(ilyr+1)) * 0.5d0
        rw_up    = rw(ilyr)
        rw_down  = rw(ilyr+1)
        drw_dz   = (rw_down - rw_up) / dz
        K0       = calc_K0(z, us, ks)
!write(LOG_UNIT, *) us, ks, drw_dz, (rw_up + rw_down) * 0.5d0
        Ri       = calc_Richardson_number(z, us, ks, drw_dz, (rw_up + rw_down) * 0.5d0)
        Kz(ilyr) = calc_Kz(K0, Ri, drw_dz)
!write(LOG_UNIT, *) 'Kz', K0, Ri, Kz(ilyr)
        Kz(ilyr) = Kz(ilyr) / dz
    enddo

contains

    function calc_K0(z, us, ks) result(K0)
        real(kind=JPRB), intent(in) :: z, us, ks
        real(kind=JPRB)             :: K0
        K0 = KARMAN_CONST * us * z * exp(-ks * z)
    end function calc_K0

    function calc_Richardson_number(z, us, ks, drdz, rw) result(Ri)
        real(kind=JPRB), intent(in) :: z, us, ks, drdz, rw
        real(kind=JPRB)             :: Ri
        real(kind=JPRB), parameter :: maxRi = 1.d10, max_ksz = 3.d0
        real(kind=JPRB) :: x, x1, n2, tmp
        x1 = KARMAN_CONST * z / ( us * exp( -min( ks * z, 3.d0 ) ) )
!        n2 = ( GRAVITY_ACCEL / rw ) * drdz
!        x  = n2 * x1 * x1
        x  = drdz * x1 * x1
        tmp = 1.d0 + 40.d0 * x
!write(LOG_UNIT, *) x1, x2, x
        if ( x < 0.d0 ) then ! drdz < 0 ! deeper is lighter
            Ri = maxRi
        elseif ( tmp > 0.d0 ) then
            Ri = (-1.d0 + sqrt(tmp)) * 0.05d0
            Ri = min(Ri, maxRi)
!        elseif ( x < 0.d0 ) then
!            tmp = 1.d0 - 40.d0 * x
!            Ri = (-1.d0 + sqrt(tmp)) * 0.05d0
!            Ri = min(Ri, maxRi)
!            Ri = 0.d0
        else
            Ri = -0.05d0
        endif
!write(LOG_UNIT, *) 'Ri', x1, n2, Ri
    end function calc_Richardson_number

    function calc_Kz(K0, Ri, drdz) result(Kz)
        real(kind=JPRB), intent(in) :: K0, Ri, drdz
        real(kind=JPRB) :: Kz
        real(kind=JPRB), parameter :: &
        &   minKz = 1.43d-7, maxKz = 5.d-3, min_f = 1.d-2
        real(kind=JPRB) :: f
!        if ( Ri <= 0.d0 ) then
!            f = 1.d0
!        else
        if ( drdz < 0.d0 ) then
            Kz = maxKz
!            Kz = K0
        else
            f = 1.d0 / (1.d0 + 37.d0 * Ri * Ri)
            f = max(f, min_f)
            Kz = K0 * f
!            Kz = max(Kz, minKz)
!            Kz = min(Kz, maxKz)
        endif
!        if ( Ri < 0.d0 ) Kz = -Kz
!        Kz = Kz * scale_factor
        Kz = Kz * Kz_scale
    end function calc_Kz

end subroutine eddy_diffusion

! ===================================================================================================
! density mixing [Subin et al., 2012] p.6
! ===================================================================================================
subroutine density_mixing(tmplyr, stolyr)
    real(kind=JPRB), intent(inout) :: &
    &   tmplyr(:) ! [K] temperature (layer)
    real(kind=JPRB), intent(in) :: &
    &   stolyr(:) ! [m3] storage (layer)
    real(kind=JPRB), allocatable :: &
    &   rw(:)
    real(kind=JPRB) :: &
    &   tmp, sto
    integer(kind=JPIM) :: &
    &   ilyr, nlyr, itr
    integer(kind=JPIM), parameter :: &
    &   itr_max = 5
    logical :: &
    &   all_stable
    nlyr = size(tmplyr)
    allocate(rw(nlyr)); rw(:) = 0.d0
    do ilyr = 1, nlyr
        rw(ilyr) = calc_water_density(tmplyr(ilyr))
    enddo
    do itr = 1, itr_max
        all_stable = .TRUE.
        do ilyr = 1, nlyr-1
            if (rw(ilyr) > rw(ilyr+1)) then
                sto = stolyr(ilyr) + stolyr(ilyr+1)
                if (sto > 0.d0) then
                    tmp = (tmplyr(ilyr) * stolyr(ilyr) + tmplyr(ilyr+1) * stolyr(ilyr+1)) &
                    &   / (stolyr(ilyr) + stolyr(ilyr+1))
                else
                    if (stolyr(ilyr) >= stolyr(ilyr+1)) then
                        tmp = tmplyr(ilyr)
                    else
                        tmp = tmplyr(ilyr+1)
                    endif
                endif
                tmplyr(ilyr  ) = tmp
                tmplyr(ilyr+1) = tmp
                rw(ilyr  ) = calc_water_density(tmp)
                rw(ilyr+1) = rw(ilyr)
                all_stable = .FALSE.
            endif
        enddo
        if (all_stable) exit
    enddo
end subroutine density_mixing


end module eddy_mod
