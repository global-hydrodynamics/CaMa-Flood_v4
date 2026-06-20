module heat_budget_mod
    use PARKIND1,  only: &
    &   JPIM, JPRB
    use const_mod, only: &
    &   STO_IGNORE
    use phys_const_mod, only : &
    &   CW, RW, CI, RI, HFUS, TMELT, DIV_RW, DIV_RI, RI_HFUS, RW_DIV_RI, RI_DIV_RW
    use LU      , only : solve_matrix
!    use error_monitor, only : calcErr_water_ice_mass
    implicit none

    interface water_energy
        module procedure :: water_energy_scalar
        module procedure :: water_energy_layer
    end interface water_energy

contains

! ===================================================================================================
real(kind=JPRB) function water_energy_scalar(watvol, wattmp)
    real(kind=JPRB), intent(in) :: &
    &   watvol, & ! [m3] water volume
    &   wattmp    ! [K]  water temperature
    water_energy_scalar = CW * RW * watvol * (wattmp - TMELT)
end function water_energy_scalar

real(kind=JPRB) function water_energy_layer(watvol, wattmp, nlyr)
    real(kind=JPRB), intent(in) :: &
    &   watvol(:), & ! [m3] water volume
    &   wattmp(:)    ! [K]  water temperature
    integer(kind=JPIM), intent(in) :: &
    &   nlyr
    integer(kind=JPIM) :: ilyr
    water_energy_layer = 0.d0
    do ilyr = 1, nlyr
        water_energy_layer = water_energy_layer + water_energy_scalar(watvol(ilyr), wattmp(ilyr))
    enddo
end function water_energy_layer

real(kind=JPRB) function ice_energy(icevol)
    real(kind=JPRB), intent(in) :: &
    &   icevol ! [m3] ice volume
    ice_energy = RI * icevol * (-HFUS)
end function ice_energy

! ===================================================================================================
subroutine water_ice_thermal_equilibrium(watS, watT, iceS, iceT, addHeat)
    real(kind=JPRB), intent(inout) :: watS, watT, iceS, iceT
    real(kind=JPRB), intent(in)    :: addHeat ! [J]
    real(kind=JPRB), parameter :: MASS_IGNORED = 1.d-10
    real(kind=JPRB) watM, watE, iceM, iceE, totM, totE

    if (iceS == 0.d0  .and. watT >=  TMELT) return ! all water
    if (watS == 0.d0  .and. iceT <= TMELT) return ! all ice
    if (watT == TMELT .and. iceT == TMELT) return ! water / ice

    watM = RW * watS
    iceM = RI * iceS
    totM = watM + iceM
    if (totM < MASS_IGNORED) return

    watE = CW * (watT - TMELT) ! from 0C water
    iceE = CI * (iceT - TMELT) - HFUS
    totE = watM * watE + iceM * iceE + addHeat

    if     (totE >= 0.d0) then         ! all water
        iceS = 0.d0
        iceT = TMELT
        watS = totM / RW
        watT = TMELT + (totE / (CW * totM))
    elseif (totE <= -totM * HFUS) then ! all ice
        watS = 0.d0
        watT = TMELT
        iceS = totM / RI
        iceT = ((totE / totM) + HFUS) / CI + TMELT
    else ! water / ice coexist
        watT = TMELT
        iceT = TMELT
        iceM = -totE / HFUS
        iceS = iceM / RI
        watS = (totM - iceM) / RW
    endif
end subroutine water_ice_thermal_equilibrium

#ifdef OPT_CHKBUDGET
subroutine check_budget_phase_change( &
&   watSold, watTold, iceSold, iceTold, &
&   watSnew, watTnew, iceSnew, iceTnew, &
&   maserr, engerr)
    real(kind=JPRB), intent(in) :: watSold, watTold, iceSold, iceTold, &
    &                      watSnew, watTnew, iceSnew, iceTnew ! [m3] or [K]
    real(kind=JPRB), intent(out) :: &
    &   maserr, & ! [kg] new - old
    &   engerr ! [J]
    real(kind=JPRB) :: watMold, watMnew, iceMold, iceMnew ! [kg]
    watMold = RW * watSold
    watMnew = RW * watSnew
    iceMold = RI * iceSold
    iceMnew = RI * iceSnew
    maserr = watMnew + iceMnew - watMold - iceMold
    engerr = watMnew * CW * (watTnew - TMELT) + iceMnew * (CI * (iceTnew - TMELT) - HFUS) &
    &      - watMold * CW * (watTold - TMELT) - iceMold * (CI * (iceTold - TMELT) - HFUS)
end subroutine check_budget_phase_change
#endif
! ===================================================================================================
subroutine calc_ice_heat_budget( &
&   ice2wat, wat2ice, rsteng, &
&   icevol, inceng)
    ! If liquid water in TMELT is set as standard,
    !   ice still exists: cM * 0 - MH + Q = c(M-dM) * 0 - (M-dM)H
    !   ice fully melted: cM * 0 - MH + Q = 0 + dQ, dQ = Q - MH
    real(kind=JPRB), intent(out) :: &
    &   ice2wat, & ! [kg] mass change from ice to water
    &   wat2ice, & ! [kg] mass change from water to ice
    &   rsteng     ! [J]  rest of energy if ice fully melted
    real(kind=JPRB), intent(in) :: &
    &   icevol, & ! [m3] ice volume
    &   inceng    ! [J] flux (+: melt)
    real(kind=JPRB) :: &
    &   iceinc_vol ! [m3] increase in ice volume (+: freeze, -: melt)
    wat2ice = 0.d0
    ice2wat = 0.d0
    rsteng  = 0.d0
    if (icevol <= 0.d0 .or. inceng == 0.d0) return

    iceinc_vol = -inceng / RI_HFUS
    if (icevol + iceinc_vol >= 0.d0) then
        if (iceinc_vol >= 0.d0) then
            wat2ice = iceinc_vol * RI
        else
            ice2wat = -iceinc_vol * RI
        endif
    else ! fully melted
        ice2wat = icevol * RI
        rsteng  = max(inceng - RI_HFUS * icevol, 0.d0)
    endif
end subroutine calc_ice_heat_budget


subroutine calc_water_heat_budget( &
&   watvol, wattmp, inceng, &
&   wat2ice, rsteng)
    real(kind=JPRB), intent(in) :: &
    &   watvol, & ! [m3] water volume (not changed in this subroutine)
    &   inceng    ! [J] increment energy (+: warm)
    real(kind=JPRB), intent(inout) :: &
    &   wattmp    ! [K] water temperature
    real(kind=JPRB), intent(out) :: &
    &   wat2ice, & ! [kg] mass change from water to ice
    &   rsteng     ! [J]  rest of energy if water fully frozen (< 0)
    real(kind=JPRB) :: &
    &   watmas, & ! [kg] water mass before phase change
    &   watcap ! [J/K] water heat capacity
    wat2ice = 0.d0
    rsteng  = 0.d0
    if (watvol <= 0.d0 .or. inceng == 0.d0) return

    watmas = RW * watvol
    watcap = CW * watmas
    wattmp = wattmp + inceng / watcap
    if (wattmp >= TMELT) return

    wat2ice = watcap * (TMELT - wattmp) / HFUS
    if (watmas < wat2ice) then
        wat2ice = watmas
        rsteng  = watmas * HFUS - watcap * (TMELT - wattmp)
    endif
    wattmp  = TMELT
end subroutine calc_water_heat_budget


subroutine water2ice( &
&   watvol, wattmp, icevol, mass, engrst)
    real(kind=JPRB), intent(inout) :: &
    &   watvol, & ! [m3] water volume
    &   icevol, & ! [m3] ice   volume
    &   mass      ! [kg] mass from water to ice
    real(kind=JPRB), intent(out) :: &
    &   engrst ! [J] rest energy if water fully frozen (< 0)
    real(kind=JPRB), intent(in) :: &
    &   wattmp ! [K]  water temperature
    real(kind=JPRB) :: &
    &   dwatvol ! [m3]
    engrst = 0.d0
    if (mass == 0.d0) return
    dwatvol = DIV_RW * mass
    if (watvol < dwatvol) then ! fully frozen
        engrst  = (watvol * RW - mass) * (HFUS + CW * (wattmp - TMELT)) ! < 0
        mass    = watvol * RW
        dwatvol = watvol
    endif
    watvol  = max(watvol - dwatvol, 0.d0)
    icevol  = icevol + dwatvol * RW_DIV_RI
end subroutine water2ice


subroutine ice2water( &
&   watvol, wattmp, icevol, mass, engrst)
    real(kind=JPRB), intent(inout) :: &
    &   watvol, & ! [m3] water volume
    &   wattmp, & ! [K]  water temp.
    &   icevol, & ! [m3] ice   volume
    &   mass      ! [kg] mass from ice to water
    real(kind=JPRB), intent(out) :: &
    &   engrst ! [J] rest energy if water fully melted (> 0)
    real(kind=JPRB) :: &
    &   dicevol, & ! [m3] ice
    &   dwatvol    ! [m3] water
    engrst = 0.d0
    if (mass == 0.d0) return
    dicevol = DIV_RI * mass
    if (icevol < dicevol) then ! fully melt
        dicevol = icevol
        engrst  = (mass - icevol * RI) * HFUS ! > 0
        mass    = icevol * RI
    endif
    dwatvol = DIV_RW * mass
    wattmp  = (watvol * wattmp + dwatvol * TMELT) / (watvol + dwatvol)
    watvol  = watvol + dwatvol
    icevol  = max(icevol - dicevol, 0.d0)
end subroutine ice2water


real(kind=JPRB) function calc_mass_warmer_water_frozen(frzmas, wattmp)
    real(kind=JPRB), intent(in) :: &
    &   frzmas, & ! [kg] mass from water (0C) to ice
    &   wattmp    ! [K]  water temperature
    if (wattmp == TMELT) then
        calc_mass_warmer_water_frozen = frzmas
    else
        calc_mass_warmer_water_frozen = frzmas * HFUS / (HFUS + CW * (wattmp - TMELT))
    endif
end function calc_mass_warmer_water_frozen

! ===================================================================================================
subroutine calc_water_heat_budget_noice( &
&   watT, &
&   watS, watHsrc)
    real(kind=JPRB), intent(inout) :: &
    &   watT ! [K] water temperature
    real(kind=JPRB), intent(in) :: &
    &   watS, & ! [m3] water volume
    &   watHsrc ! [J]  incoming energy
    real(kind=JPRB) :: &
    &   dwatT ! [K] change in water temperature
    if (watS < STO_IGNORE) return
    dwatT = watHsrc / (CW * RW * watS)
    watT  = watT + dwatT
end subroutine calc_water_heat_budget_noice

! ===================================================================================================
subroutine solve_diffusion_equation( &
&   lyrtmp, &
&   sfcflx, swdown, lyrthk, veredd, delt)
    ! dy/dz = d/dz(K * dy/dz) + swdown
    ! update layer temperature considering shortwave radiation and layer mixing
    ! assume flux from bottom sediment = 0 (if assume not 0, btmtmp is added into arguments)
    real(kind=JPRB), intent(inout) :: &
    &   lyrtmp(:) ! [K] water temperature of each layer
    real(kind=JPRB), intent(in) :: &
    &   sfcflx(:), & ! [W/m2] 1: flux at surface, 2: d(flux)/dT
    &   swdown(:), & ! [W/m2] absorption of shortwave radiation by each layer
    &   lyrthk(:), & ! [m] layer thickness
    &   veredd(:), & ! vertical eddy coeficient, flux = CW * RW * veredd * (lyrtmp - lyrtmp)
    &   delt         ! [s] time step width
    real(kind=JPRB), allocatable :: &
    &   A(:,:), b(:), dT(:) ! [A][dT] = [b], [T'] = [T] + [dT]
    integer(kind=JPIM) ilyr, nlyr
    nlyr = size(lyrtmp)
    allocate(A(nlyr,nlyr)); A(:,:) = 0.d0
    allocate(b(nlyr     )); b(:)   = 0.d0
    do ilyr = 1, nlyr
        A(ilyr,ilyr) = 1.d0
    enddo
    call add_sfcflx(A(1,1), b(1), sfcflx(:), lyrthk(1), delt)
    call add_swdown(b(:), swdown(:), lyrthk(:), delt)

    allocate(dT(nlyr)); dT(:) = 0.d0
    if (nlyr == 1) then
        dT(1) = b(1) / A(1,1)
    else
        call add_lyrmix(A(:,:), b(:), veredd(:), lyrthk(:), lyrtmp(:), delt)
        call solve_matrix(dT(:), A(:,:), b(:), nlyr)
    endif
    call allocate_error(dT(:), lyrthk(:), sfcflx(1), swdown(:), delt, nlyr)

    deallocate(A, b)
    lyrtmp(:) = lyrtmp(:) + dT(:)
    deallocate(dT)

contains

    subroutine add_sfcflx(A, b, sfcflx, lyrthk, delt)
        real(kind=JPRB), intent(inout) :: A, b
        real(kind=JPRB), intent(in)    :: sfcflx(:), lyrthk, delt
        real(kind=JPRB) :: coef
        coef = delt / (CW * RW * lyrthk)
        A = A - sfcflx(2) * coef
        b = b + sfcflx(1) * coef
    end subroutine add_sfcflx

    subroutine add_swdown(b, swdown, lyrthk, delt)
        real(kind=JPRB), intent(inout) :: b(:)
        real(kind=JPRB), intent(in)    :: swdown(:), lyrthk(:), delt
        real(kind=JPRB) coef
        integer(kind=JPIM) ilyr, nlyr
        nlyr = size(b)
        do ilyr = 1, nlyr
            coef    = delt / (CW * RW * lyrthk(ilyr))
            b(ilyr) = b(ilyr) + coef * swdown(ilyr)
        enddo
    end subroutine add_swdown

    subroutine add_lyrmix(A, b, Kz, lyrthk, lyrtmp, delt)
        real(kind=JPRB), intent(inout) :: A(:,:), b(:)
        real(kind=JPRB), intent(in)    :: Kz(:), lyrthk(:), lyrtmp(:), delt
        real(kind=JPRB) :: cup, cdn
        integer(kind=JPIM) :: nlyr, ilyr
        nlyr = size(b)
        do ilyr = 1, nlyr-1 ! b/w ilyr and ilyr+1
            cdn = Kz(ilyr) * delt / lyrthk(ilyr)
            A(ilyr,ilyr  ) = A(ilyr  ,ilyr) + cdn
            A(ilyr,ilyr+1) = A(ilyr,ilyr+1) - cdn
!            A(ilyr+1,ilyr) = A(ilyr+1,ilyr) - cdn ! NG
            b(ilyr) = b(ilyr) + cdn * (lyrtmp(ilyr+1) - lyrtmp(ilyr))
        enddo
        do ilyr = 2, nlyr ! b/w ilyr and ilyr-1
            cup = Kz(ilyr-1) * delt / lyrthk(ilyr)
            A(ilyr,ilyr  ) = A(ilyr,ilyr  ) + cup
            A(ilyr,ilyr-1) = A(ilyr,ilyr-1) - cup
!            A(ilyr-1,ilyr) = A(ilyr-1,ilyr) - cup ! NG
            b(ilyr) = b(ilyr) - cup * (lyrtmp(ilyr) - lyrtmp(ilyr-1))
        enddo
    end subroutine add_lyrmix

    subroutine allocate_error(dT, lyrthk, srfflx, swdown, delt, nlyr)
        ! allocate error by converting it into even temperature change
        real(kind=JPRB), intent(inout) :: &
        &   dT(nlyr) ! temperature change
        real(kind=JPRB), intent(in) :: &
        &   lyrthk(nlyr), & ! [m]    layer thickness
        &   srfflx, &       ! [W/m2] surface heat flux
        &   swdown(nlyr), & ! [W/m2] shortwave radiation
        &   delt            ! [s]    timestep
        integer(kind=JPIM), intent(in) :: &
        &   nlyr ! [-] layer number
        real(kind=JPRB) :: &
        &   rsteng ! [mK] rest energy per area
        rsteng = (srfflx + sum(swdown(:))) * delt / (CW * RW) - dot_product(lyrthk(:), dT(:))
        dT(:)  = dT(:) + rsteng / sum(lyrthk(:))
        !write(*, *) rsteng / sum(lyrthk(:))
    end subroutine allocate_error

end subroutine solve_diffusion_equation


subroutine solve_diffusion_equation_DA( &
&   lyrtmp, &
&   sfcflx, source, lyrsto, bndare, veredd, delt)
    ! solve diffusion equation with considering depth-area relationship
    ! update layer temperature considering shortwave radiation and layer mixing
    ! assume flux from bottom sediment = 0 (if assume not 0, btmtmp is added into arguments)
    real(kind=JPRB), intent(inout) :: &
    &   lyrtmp(:) ! [K] water temperature of each layer
    real(kind=JPRB), intent(in) :: &
    &   sfcflx(:), & ! [W/m2] 1: flux at surface, 2: d(flux)/dT
    &   source(:), & ! [W/m2] absorption of shortwave radiation by each layer
    &   lyrsto(:), & ! [m3] layer storage
    &   bndare(:), & ! [m2] boundary area
    &   veredd(:), & ! vertical eddy coeficient, flux = CW * RW * veredd * (lyrtmp - lyrtmp)
    &   delt         ! [s] time step width
    real(kind=JPRB), allocatable :: A(:,:), b(:), dT(:) ! [A][dT] = [b], [T'] = [T] + [dT]
    integer(kind=JPIM) ilyr, nlyr

    nlyr = size(lyrtmp)
    allocate(A(nlyr,nlyr)); A(:,:) = 0.d0
    allocate(b(nlyr     )); b(:)   = 0.d0
    do ilyr = 1, nlyr
        A(ilyr,ilyr) = 1.d0
    enddo
    call add_sfcflx
!if ( nlyr==1 ) write(*, *)  A(1,1), b(1)
!if ( nlyr==1 ) write(*, *)  source(1), bndare(1), lyrsto(1)
    call add_source
!if ( nlyr==1 ) write(*, *)  A(1,1), b(1)
    allocate(dT(nlyr)); dT(:) = 0.d0
    if (nlyr == 1) then
        dT(1) = b(1) / A(1,1)
    else
        call add_lyrmix
        call solve_matrix(dT(:), A(:,:), b(:), nlyr)
    endif
!if ( nlyr==1 ) write(*, *)  dT(1)
!if (abs(dT(1)>1.d0)) write(*, *) 'updlyr', bndare(1), lyrsto(1), sfcflx(1), sfcflx(2), source(1)
    deallocate(A, b)
    lyrtmp(:) = lyrtmp(:) + dT(:)
    deallocate(dT)

contains

    subroutine add_sfcflx
        real(kind=JPRB) :: coef
        coef   = bndare(1) * delt / ( CW * RW * lyrsto(1) )
        A(1,1) = A(1,1) - sfcflx(2) * coef
        b(1)   = b(1)   + sfcflx(1) * coef
    end subroutine add_sfcflx

    subroutine add_source
        real(kind=JPRB) :: coef
        integer(kind=JPIM) :: ilyr
        do ilyr = 1, nlyr-1
!            coef    = 0.5d0 * ( bndare(ilyr) + bndare(ilyr+1) ) * delt / (CW * RW * lyrsto(ilyr))
            coef    = bndare(ilyr) * delt / (CW * RW * lyrsto(ilyr))
            b(ilyr) = b(ilyr) + coef * source(ilyr)
        enddo
        coef = bndare(nlyr) * delt / ( CW * RW * lyrsto(nlyr) )
        b(nlyr) = b(nlyr) + coef * source(nlyr)
    end subroutine add_source

    subroutine add_lyrmix
        real(kind=JPRB) :: cup, cdn
        integer(kind=JPIM) :: ilyr
        do ilyr = 1, nlyr-1 ! b/w ilyr and ilyr+1
            cdn = bndare(ilyr+1) * veredd(ilyr) * delt / lyrsto(ilyr)
            A(ilyr,ilyr  ) = A(ilyr,ilyr)   + cdn
            A(ilyr,ilyr+1) = A(ilyr,ilyr+1) - cdn
            b(ilyr) = b(ilyr) + cdn * (lyrtmp(ilyr+1) - lyrtmp(ilyr))
        enddo
        do ilyr = 2, nlyr ! b/w ilyr and ilyr-1
            cup = bndare(ilyr) * veredd(ilyr-1) * delt / lyrsto(ilyr)
            A(ilyr,ilyr  ) = A(ilyr,ilyr  ) + cup
            A(ilyr,ilyr-1) = A(ilyr,ilyr-1) - cup
            b(ilyr) = b(ilyr) - cup * (lyrtmp(ilyr) - lyrtmp(ilyr-1))
        enddo
    end subroutine add_lyrmix

end subroutine solve_diffusion_equation_DA

end module heat_budget_mod
