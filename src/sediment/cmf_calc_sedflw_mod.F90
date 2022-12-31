module cmf_calc_sedflw_mod
!==========================================================
!* PURPOSE: physics for sediment transport
!
! (C) M.Hatono  (Hiroshima-U)  May 2021
!==========================================================
contains
!####################################################################
! -- CMF_CALC_SEDFLW
! --
! --
!####################################################################
subroutine cmf_calc_sedflw
  use PARKIND1,                only: JPIM, JPRB
  use YOS_CMF_INPUT,           only: PGRV
  use YOS_CMF_MAP,             only: B2RIVLEN, B2RIVWTH, NSEQALL
  use YOS_CMF_PROG,            only: D2RIVSTO
  use YOS_CMF_DIAG,            only: B2RIVDPH
  use yos_cmf_sed,             only: lambda, nsed, sedDT, setVel, &
                                     b2layer, b2sedcon, b2rivsto_pre
  use sed_utils_mod,           only: sed_diag_average, sed_diag_reset

  implicit none
  !$ SAVE
  save
  integer(kind=JPIM)              :: ISEQ
  real(kind=JPRB)                 :: sedsto(NSEQALL,nsed)
  real(kind=JPRB)                 :: shearVel(NSEQALL)
  real(kind=JPRB)                 :: critShearVel(NSEQALL,nsed), dMean(NSEQALL), susVel(NSEQALL,nsed)
  real(kind=JPRB), parameter      :: IGNORE_DPH = 0.05_JPRB
  !================================================

  call sed_diag_average 
  !$omp parallel do
  do iseq = 1, NSEQALL
    sedsto(iseq,:) = b2sedcon(iseq,:) * max(b2rivsto_pre(iseq), 0._JPRB)
  enddo
  !$omp end parallel do

  call calc_params
  call calc_advection
  call calc_entrainment
  call calc_exchange

  !$omp parallel do
  do iseq = 1, NSEQALL
    if ( D2RIVSTO(iseq,1) < B2RIVWTH(iseq,1)*B2RIVLEN(iseq,1)*IGNORE_DPH ) cycle
    b2sedcon(iseq,:) = sedsto(iseq,:) / D2RIVSTO(iseq,1)
  enddo
  !$omp end parallel do

  call sed_diag_reset

contains
!==========================================================
!+ calc_params
!+ calc_advection
!+ calc_entrainment
!+ calc_exchange
!==========================================================
  subroutine calc_params
    use yos_cmf_sed,           only: pset, revEgia, sDiam, visKin, B2RIVvel_sed
    use cmf_calc_sedpar_mod,   only: calc_criticalShearVelocity, calc_shearVelocity, calc_suspendVelocity
    implicit none
    save
    integer(kind=JPIM)            ::  ised, iseq
    real(kind=JPRB)               ::  csVel0, sTmp, sTmp1(nsed)
    !=====================================================
    
    do iseq = 1, NSEQALL
      !-------------------------!
      ! critical shear velocity !
      !-------------------------!
      
      if ( sum(b2layer(iseq,:)) <= 0._JPRB ) then
        critShearVel(iseq,:) = 1E20
      else if ( revEgia ) then
        dMean(iseq) = 0._JPRB
        do ised = 1, nsed
          dMean(iseq) = dMean(iseq) + sDiam(ised)*b2layer(iseq,ised)/sum(b2layer(iseq,:))
        enddo
        csVel0 = calc_criticalShearVelocity(dMean(iseq))
        do ised = 1, nsed
          if ( sDiam(ised) / dMean(iseq) >= 0.4_JPRB ) then
            critShearVel(iseq,ised) = sqrt( csVel0*sDiam(ised)/dMean(iseq) ) * &
              & ( log10(19._JPRB)/log10(19._JPRB*sDiam(ised)/dMean(iseq)) ) * 0.01_JPRB
          else
            critShearVel(iseq,ised) = sqrt( 0.85*csVel0 ) * 0.01_JPRB
          endif
        enddo      
      else
        do ised = 1, nsed
          critShearVel(iseq,ised) = sqrt( calc_criticalShearVelocity(sDiam(ised)) ) * 0.01_JPRB
        enddo
      endif
    
      !------------------------------------------------------!
      ! shear velocity, suspend velocity, Karman coefficient !
      !------------------------------------------------------!
      if ( B2RIVvel_sed(iseq) == 0._JPRB .or. B2RIVDPH(iseq,1) < IGNORE_DPH ) then
        shearVel(iseq) = 0._JPRB
        susVel(iseq,:) = 0._JPRB
      else
        shearVel(iseq) = calc_shearVelocity(B2RIVvel_sed(iseq), B2RIVDPH(iseq,1))
        susVel(iseq,:) = calc_suspendVelocity(critShearVel(iseq,:), shearVel(iseq), setVel(:))
      endif
    enddo
  end subroutine calc_params
  !=====================================================

  subroutine calc_advection
    use YOS_CMF_MAP,           only:  I1NEXT
    use yos_cmf_sed,           only:  b2rivout_sed, b2bedout, b2sedout, &
                                      b2bedout_avg, b2sedout_avg, psedD, pwatD
    implicit none
    real(kind=JPRB)               ::  bOut(NSEQALL,nsed), brate(NSEQALL,nsed)
    real(kind=JPRB)               ::  sOut(NSEQALL,nsed), srate(NSEQALL,nsed)
    integer(kind=JPIM)            ::  ised, iseq
    ! save for omop
    real(kind=JPRB), save         ::  plusVel, minusVel
    integer(kind=JPIM), save      ::  iseq0, iseq1
    !$omp threadprivate ( plusVel, minusVel, iseq0, iseq1 )
    !========

    bOut(:,:) = 0._JPRB
    sOut(:,:) = 0._JPRB
    !$omp parallel do
    do iseq = 1, NSEQALL
      
      if ( b2rivout_sed(iseq) >= 0._JPRB ) then
        iseq0 = iseq
        iseq1 = I1NEXT(iseq)
      else
        iseq0 = I1NEXT(iseq)
        iseq1 = iseq
      endif

      if ( b2rivout_sed(iseq) == 0._JPRB ) then
        b2sedout(iseq,:) = 0._JPRB
        b2bedout(iseq,:) = 0._JPRB
        cycle
      endif

      !-------------------!
      ! calc suspend flow !
      !-------------------!
      if ( iseq0 < 0 ) then
        b2sedout(iseq,:) = b2sedcon(iseq1,:) * b2rivout_sed(iseq)
      else
        b2sedout(iseq,:) = b2sedcon(iseq0,:) * b2rivout_sed(iseq)
        sOut(iseq0,:) = sOut(iseq0,:) + abs(b2sedout(iseq,:))*sedDT
      endif

      !--------------!
      ! calc bedflow !
      !--------------!
      if ( minval(critShearVel(iseq,:)) >= shearVel(iseq) .or. sum(b2layer(iseq,:)) == 0._JPRB .or. iseq0 < 0  ) then
        b2bedout(iseq,:) = 0._JPRB
      else 
        do ised = 1, nsed
          if ( critShearVel(iseq,ised) >= shearVel(iseq) .or. b2layer(iseq,ised) == 0._JPRB ) then
            b2bedout(iseq,ised) = 0._JPRB
            cycle
          endif
          plusVel = shearVel(iseq) + critShearVel(iseq,ised)
          minusVel = shearVel(iseq) - critShearVel(iseq,ised)
          b2bedout(iseq,ised) = 17._JPRB * B2RIVWTH(iseq,1) * plusVel * minusVel * minusVel & 
           & / ((psedD-pwatD)/pwatD) / PGRV * b2layer(iseq,ised) / sum(b2layer(iseq,:)) 
          bOut(iseq0,ised) = bOut(iseq0,ised) + b2bedout(iseq,ised)*sedDT
        enddo
      endif
    enddo
    !$omp end parallel do

    !--------------------------------------------!
    ! adjust outflow if larget than sedsto/layer !
    !--------------------------------------------!
    brate(:,:) = 1._JPRB
    srate(:,:) = 1._JPRB
    !$omp parallel do
    do iseq = 1, NSEQALL
      if ( minval(sOut(iseq,:)) <= 1e-8 ) then
        do ised = 1, nsed
          if ( sOut(iseq,ised) > 1e-8 ) then
            srate(iseq,ised) = min ( sedsto(iseq,ised) / sOut(iseq,ised), 1._JPRB )
          endif
        enddo
      else
        srate(iseq,:) = min ( sedsto(iseq,:) / sOut(iseq,:), 1._JPRB )
      endif
      if ( minval(bOut(iseq,:)) <= 1e-8 ) then
        do ised = 1, nsed
          if ( bOut(iseq,ised) > 1e-8 ) then
            brate(iseq,ised) = min( b2layer(iseq,ised) / bOut(iseq,ised), 1._JPRB )
          endif
        enddo
      else
        brate(iseq,:) = min( b2layer(iseq,:) / bOut(iseq,:), 1._JPRB )
      endif
    enddo
    !$omp end parallel do
    
    do iseq = 1, NSEQALL
      if ( b2rivout_sed(iseq) >= 0._JPRB ) then
        iseq0 = iseq
        iseq1 = I1NEXT(iseq)
      else
        iseq0 = I1NEXT(iseq)
        iseq1 = iseq
      endif

      if ( iseq0 > 0 ) then
        b2sedout(iseq,:) = b2sedout(iseq,:) * srate(iseq0,:)
        sedsto(iseq0,:) = max( sedsto(iseq0,:)-abs(b2sedout(iseq,:))*sedDT, 0._JPRB )
        b2bedout(iseq,:) = b2bedout(iseq,:) * brate(iseq0,:)
        b2layer(iseq0,:) = max( b2layer(iseq0,:)-abs(b2bedout(iseq,:))*sedDT, 0._JPRB )
      endif
      if ( iseq1 > 0 ) then
        sedsto(iseq1,:) = max( sedsto(iseq1,:)+abs(b2sedout(iseq,:))*sedDT, 0._JPRB )
        b2layer(iseq1,:) = max( b2layer(iseq1,:)+abs(b2bedout(iseq,:))*sedDT, 0._JPRB ) 
      endif

      b2bedout_avg(iseq,:) = b2bedout_avg(iseq,:) + b2bedout(iseq,:)*sedDT
      b2sedout_avg(iseq,:) = b2sedout_avg(iseq,:) + b2sedout(iseq,:)*sedDT
    enddo

  end subroutine calc_advection
  !=====================================================

  subroutine calc_entrainment
    use yos_cmf_sed,           only:  vonKar, b2netflw, b2netflw_avg, b2sedinp, b2seddep, totlyrnum
    
    implicit none
    real(kind=JPRB)               ::  dTmp(NSEQALL,nsed), D(NSEQALL,nsed), Es(NSEQALL,nsed), Zd(NSEQALL,nsed)
    integer(kind=JPIM)            ::  ilyr, ised, iseq
    real(kind=JPRB),save          ::  dTmp1, layerP
    !$omp threadprivate  ( dTmp1, layerP )
    !========

    !$omp parallel do
    do iseq = 1, NSEQALL
      if ( B2RIVDPH(iseq,1) < IGNORE_DPH ) then
        b2netflw(iseq,:) = 0._JPRB
        cycle
      endif

      !----------------------!
      ! calculate suspension !
      !----------------------!
      if ( sum(b2layer(iseq,:)) == 0._JPRB .or. all(susVel(iseq,:)==0._JPRB) ) then
        Es(iseq,:) = 0._JPRB
      else
        Es(iseq,:) = susVel(iseq,:) * (1._JPRB-lambda) * B2RIVWTH(iseq,1) * B2RIVLEN(iseq,1) * b2layer(iseq,:) / sum(b2layer(iseq,:))
        Es(iseq,:) = max( Es(iseq,:), 0._JPRB )
      endif

      !----------------------!
      ! calculate deposition !
      !----------------------!
      if ( shearVel(iseq) == 0._JPRB .or. all(setVel(:)==0._JPRB) ) then
        D(iseq,:) = 0._JPRB
      else
        Zd(iseq,:) = 6._JPRB * setVel(:) / vonKar / shearVel(iseq)
        D(iseq,:) = setVel(:) * B2RIVWTH(iseq,1) * B2RIVLEN(iseq,1) * b2sedcon(iseq,:) * Zd(iseq,:) / (1._JPRB-exp(-Zd(iseq,:))) 
        D(iseq,:) = max( D(iseq,:), 0._JPRB )
      endif
      b2netflw(iseq,:) = Es(iseq,:) - D(iseq,:)
   
      !-------------------------------------------!
      ! if >0, suspension ; if <0, deposition     !
      ! adjust netflw if larger than sedsto/layer !
      !-------------------------------------------!
      do ised = 1, nsed
        if ( b2netflw(iseq,ised) == 0._JPRB ) then
          cycle
        else if ( b2netflw(iseq,ised) > 0._JPRB ) then
          dTmp1 = b2netflw(iseq,ised)*sedDT/(1._JPRB-lambda)
          if ( dTmp1 < b2layer(iseq,ised) ) then
            b2layer(iseq,ised) = b2layer(iseq,ised) - dTmp1
          else
            b2netflw(iseq,ised) = b2layer(iseq,ised) * (1._JPRB-lambda) / sedDT
            b2layer(iseq,ised) = 0._JPRB
          endif
          sedsto(iseq,ised) = sedsto(iseq,ised) + b2netflw(iseq,ised) * sedDT
        else
          if ( abs(b2netflw(iseq,ised))*sedDT < sedsto(iseq,ised) ) then
            sedsto(iseq,ised) = max (sedsto(iseq,ised) - abs(b2netflw(iseq,ised))*sedDT, 0._JPRB )
          else
            b2netflw(iseq,ised) = - sedsto(iseq,ised) / sedDT
            sedsto(iseq,ised) = 0._JPRB
          endif
          b2layer(iseq,ised) = b2layer(iseq,ised) + abs(b2netflw(iseq,ised))*sedDT/(1._JPRB-lambda)
        endif
      enddo

      sedsto(iseq,:) = sedsto(iseq,:) + b2sedinp(iseq,:)*sedDT    
      if ( sum(sedsto(iseq,:)) > D2RIVSTO(iseq,1) * 0.01_JPRB ) then
        dTmp(iseq,:) = ( sum(sedsto(iseq,:)) - D2RIVSTO(iseq,1)*0.01_JPRB ) * sedsto(iseq,:)/sum(sedsto(iseq,:))
        b2netflw(iseq,:) = b2netflw(iseq,:) - dTmp(iseq,:)/sedDT
        sedsto(iseq,:) = sedsto(iseq,:) - dTmp(iseq,:)
        b2layer(iseq,:) = b2layer(iseq,:) + dTmp(iseq,:)/(1._JPRB-lambda)
      endif
        
      b2netflw_avg(iseq,:) = b2netflw_avg(iseq,:) + b2netflw(iseq,:)*sedDT
    enddo
    !$omp end parallel do
  end subroutine calc_entrainment
  !=====================================================

  subroutine calc_exchange
    ! redistribute into vertical bed layers
    use yos_cmf_sed,           only:  b2seddep, lyrdph, totlyrnum
    
    implicit none
    integer(kind=JPIM)            ::  ilyr, ised, iseq, jlyr, slyr
    real(kind=JPRB)               ::  diff, lyrvol, layerP(nsed), seddepP(totlyrnum+1,nsed), tmp(nsed)

    do iseq = 1, NSEQALL
      lyrvol = lyrdph * B2RIVWTH(iseq,1) * B2RIVLEN(iseq,1)

      if ( minval(b2layer(iseq,:)) < 0._JPRB ) b2layer(iseq,:) = max( b2layer(iseq,:), 0._JPRB )
      if ( minval(b2seddep(iseq,:,:)) < 0._JPRB ) b2seddep(iseq,:,:) = max( b2seddep(iseq,:,:), 0._JPRB )

      !---------------------------------------!
      ! if bed storage less than layer volume !
      !---------------------------------------!
      if ( sum(b2layer(iseq,:)) + sum(b2seddep(iseq,:,:)) <= lyrvol ) then
        b2layer(iseq,:) = b2layer(iseq,:) + sum(b2seddep(iseq,:,:),dim=1)
        b2seddep(iseq,:,:) = 0._JPRB
        cycle
      endif

      !------------------------------------!
      ! distribute into top exchange layer !
      !------------------------------------!
      layerP(:) = b2layer(iseq,:)
      if ( sum(layerP(:)) >= lyrvol ) then
        b2layer(iseq,:) = layerP(:) * min( lyrvol/sum(layerP(:)), 1._JPRB )
        layerP(:) = max( layerP(:) - b2layer(iseq,:), 0._JPRB )
        slyr = 0
      else if ( sum(b2seddep(iseq,:,:)) > 0._JPRB ) then
        layerP(:) = 0._JPRB
        do ilyr = 1, totlyrnum
          diff = lyrvol - sum(b2layer(iseq,:))
          if ( diff <= 0._JPRB ) exit
          if ( sum(b2seddep(iseq,ilyr,:)) <= diff ) then
            b2layer(iseq,:) = b2layer(iseq,:) + b2seddep(iseq,ilyr,:)
            b2seddep(iseq,ilyr,:) = 0._JPRB
            slyr = ilyr + 1
          else
            tmp(:) = diff * b2seddep(iseq,ilyr,:) / sum(b2seddep(iseq,ilyr,:))
            b2layer(iseq,:) = b2layer(iseq,:) + tmp(:)
            b2seddep(iseq,ilyr,:) = max( b2seddep(iseq,ilyr,:) - tmp(:), 0._JPRB )
            slyr = ilyr
            exit
          endif
        enddo
      else
        b2seddep(iseq,:,:) = 0._JPRB
        cycle
      endif
      if ( sum(b2seddep(iseq,:,:)) == 0._JPRB ) cycle

      !-----------------------------------!
      ! distribute remaining bedload into !
      ! vertical deposition layers        !
      !-----------------------------------!
      seddepP(1,:) = layerP(:)
      seddepP(2:,:) = b2seddep(iseq,:,:)
      b2seddep(iseq,:,:) = 0._JPRB
      do ilyr = 1, totlyrnum - 1
        if ( sum(b2seddep(iseq,ilyr,:)) == lyrvol ) cycle
        do jlyr = slyr+1, totlyrnum + 1
          diff = lyrvol - sum(b2seddep(iseq,ilyr,:))
          if ( diff <= 0._JPRB ) exit
          if ( sum(seddepP(jlyr,:)) <= diff ) then
            b2seddep(iseq,ilyr,:) = b2seddep(iseq,ilyr,:) + seddepP(jlyr,:)
            seddepP(jlyr,:) = 0._JPRB
          else
            tmp(:) = diff * seddepP(jlyr,:) / sum(seddepP(jlyr,:))
            b2seddep(iseq,ilyr,:) = b2seddep(iseq,ilyr,:) + tmp(:)
            seddepP(jlyr,:) = max(seddepP(jlyr,:) - tmp(:), 0._JPRB)
            exit
          endif
        enddo
      enddo
      
      if ( sum(seddepP) > 0._JPRB ) then
        b2seddep(iseq,totlyrnum,:) = sum(seddepP, dim=1)
      endif
    enddo

  end subroutine calc_exchange
end subroutine cmf_calc_sedflw
!####################################################################

end module cmf_calc_sedflw_mod
