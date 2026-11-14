module cmf_calc_sedflw_mod
!==========================================================
!* PURPOSE: physics for sediment transport
! (C) M.Hatono  (Hiroshima-U)  May 2021
!
! Licensed under the Apache License, Version 2.0 (the "License");
!   You may not use this file except in compliance with the License.
!   You may obtain a copy of the License at: http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software distributed under the License is 
!  distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
! See the License for the specific language governing permissions and limitations under the License.
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
  use YOS_CMF_MAP,             only: D2RIVLEN, D2RIVWTH, NSEQALL
  use YOS_CMF_PROG,            only: P2RIVSTO
  use YOS_CMF_DIAG,            only: D2RIVDPH
  use yos_cmf_sed,             only: lambda, nsed, sedDT, setVel, &
                                     d2layer, d2sedcon, d2rivsto_pre
  use sed_utils_mod,           only: sed_diag_average, sed_diag_reset

  implicit none
  !$ SAVE
  save
  integer(kind=JPIM)              :: ISEQ
  real(kind=JPRB)                 :: sedsto(NSEQALL,nsed)
  real(kind=JPRB)                 :: shearVel(NSEQALL)
  real(kind=JPRB)                 :: critShearVel(NSEQALL,nsed), dMean(NSEQALL), susVel(NSEQALL,nsed)
  real(kind=JPRB), parameter      :: IGNORE_DPH = 0.05d0
  !================================================

  !! calculate average of river water variables within sediment time step
  call sed_diag_average

  !! sediment concentration
  !$omp parallel do
  do iseq = 1, NSEQALL 
    sedsto(iseq,:) = d2sedcon(iseq,:) * max(d2rivsto_pre(iseq), 0.d0)
  enddo
  !$omp end parallel do

  call calc_params         !! calculate sediment transfer parameters
  call calc_advection
  call calc_entrainment
  call calc_exchange

  !$omp parallel do
  do iseq = 1, NSEQALL
    if ( P2RIVSTO(iseq,1) < D2RIVWTH(iseq,1)*D2RIVLEN(iseq,1)*IGNORE_DPH ) cycle
    d2sedcon(iseq,:) = sedsto(iseq,:) / P2RIVSTO(iseq,1)
  enddo
  !$omp end parallel do

  call sed_diag_reset

contains
!==========================================================
!+ calc_params       !! calculate sediment transfer parameters
!+ calc_advection
!+ calc_entrainment
!+ calc_exchange
!==========================================================
  subroutine calc_params
    use yos_cmf_sed,           only: pset, revEgia, sDiam, visKin, d2rivvel_sed
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
      
      if ( sum(d2layer(iseq,:)) <= 0.d0 ) then
        critShearVel(iseq,:) = 1e20
      else if ( revEgia ) then       !! use Egiazoroff equation for mixed-size sedimnt
        dMean(iseq) = 0.d0
        do ised = 1, nsed
          dMean(iseq) = dMean(iseq) + sDiam(ised)*d2layer(iseq,ised)/sum(d2layer(iseq,:))
        enddo
        csVel0 = calc_criticalShearVelocity(dMean(iseq))
        do ised = 1, nsed
          if ( sDiam(ised) / dMean(iseq) >= 0.4d0 ) then
            critShearVel(iseq,ised) = sqrt( csVel0*sDiam(ised)/dMean(iseq) ) * &
              & ( log10(19.d0)/log10(19.d0*sDiam(ised)/dMean(iseq)) ) * 0.01d0
          else
            critShearVel(iseq,ised) = sqrt( 0.85*csVel0 ) * 0.01d0
          endif
        enddo      
      else
        do ised = 1, nsed
          critShearVel(iseq,ised) = sqrt( calc_criticalShearVelocity(sDiam(ised)) ) * 0.01d0
        enddo
      endif
    
      !------------------------------------------------------!
      ! shear velocity, suspend velocity, Karman coefficient !
      !------------------------------------------------------!
      if ( d2rivvel_sed(iseq) == 0.d0 .or. D2RIVDPH(iseq,1) < IGNORE_DPH ) then
        shearVel(iseq) = 0.d0
        susVel(iseq,:) = 0.d0
      else
        shearVel(iseq) = calc_shearVelocity(d2rivvel_sed(iseq), D2RIVDPH(iseq,1))
        susVel(iseq,:) = calc_suspendVelocity(critShearVel(iseq,:), shearVel(iseq), setVel(:))
      endif
    enddo
  end subroutine calc_params
  !=====================================================

  subroutine calc_advection
    use YOS_CMF_MAP,           only:  I1NEXT
    use yos_cmf_sed,           only:  d2rivout_sed, d2bedout, d2sedout, &
                                      d2bedout_avg, d2sedout_avg, psedD, pwatD
    implicit none
    real(kind=JPRB)               ::  bOut(NSEQALL,nsed), brate(NSEQALL,nsed)
    real(kind=JPRB)               ::  sOut(NSEQALL,nsed), srate(NSEQALL,nsed)
    integer(kind=JPIM)            ::  ised, iseq
    ! save for omop
    real(kind=JPRB), save         ::  plusVel, minusVel
    integer(kind=JPIM), save      ::  iseq0, iseq1
    !$omp threadprivate ( plusVel, minusVel, iseq0, iseq1 )
    !========

    bOut(:,:) = 0.d0
    sOut(:,:) = 0.d0
    !$omp parallel do
    do iseq = 1, NSEQALL
      
      if ( d2rivout_sed(iseq) >= 0.d0 ) then
        iseq0 = iseq
        iseq1 = I1NEXT(iseq)
      else
        iseq0 = I1NEXT(iseq)
        iseq1 = iseq
      endif

      if ( d2rivout_sed(iseq) == 0.d0 ) then
        d2sedout(iseq,:) = 0.d0
        d2bedout(iseq,:) = 0.d0
        cycle
      endif

      !-------------------!
      ! calc suspend flow !
      !-------------------!
      if ( iseq0 < 0 ) then
        d2sedout(iseq,:) = d2sedcon(iseq1,:) * d2rivout_sed(iseq)
      else
        d2sedout(iseq,:) = d2sedcon(iseq0,:) * d2rivout_sed(iseq)
        sOut(iseq0,:) = sOut(iseq0,:) + abs(d2sedout(iseq,:))*sedDT
      endif

      !--------------!
      ! calc bedflow !
      !--------------!
      if ( minval(critShearVel(iseq,:)) >= shearVel(iseq) .or. sum(d2layer(iseq,:)) == 0.d0 .or. iseq0 < 0  ) then
        d2bedout(iseq,:) = 0.d0
      else 
        do ised = 1, nsed
          if ( critShearVel(iseq,ised) >= shearVel(iseq) .or. d2layer(iseq,ised) == 0.d0 ) then
            d2bedout(iseq,ised) = 0.d0
            cycle
          endif
          plusVel = shearVel(iseq) + critShearVel(iseq,ised)
          minusVel = shearVel(iseq) - critShearVel(iseq,ised)
          d2bedout(iseq,ised) = 17.d0 * D2RIVWTH(iseq,1) * plusVel * minusVel * minusVel & 
           & / ((psedD-pwatD)/pwatD) / PGRV * d2layer(iseq,ised) / sum(d2layer(iseq,:)) 
          bOut(iseq0,ised) = bOut(iseq0,ised) + d2bedout(iseq,ised)*sedDT
        enddo
      endif
    enddo
    !$omp end parallel do

    !--------------------------------------------!
    ! adjust outflow if larget than sedsto/layer !
    !--------------------------------------------!
    brate(:,:) = 1.d0
    srate(:,:) = 1.d0
    !$omp parallel do
    do iseq = 1, NSEQALL
      if ( minval(sOut(iseq,:)) <= 1e-8 ) then
        do ised = 1, nsed
          if ( sOut(iseq,ised) > 1e-8 ) then
            srate(iseq,ised) = min ( sedsto(iseq,ised) / sOut(iseq,ised), 1.d0 )
          endif
        enddo
      else
        srate(iseq,:) = min ( sedsto(iseq,:) / sOut(iseq,:), 1.d0 )
      endif
      if ( minval(bOut(iseq,:)) <= 1e-8 ) then
        do ised = 1, nsed
          if ( bOut(iseq,ised) > 1e-8 ) then
            brate(iseq,ised) = min( d2layer(iseq,ised) / bOut(iseq,ised), 1.d0 )
          endif
        enddo
      else
        brate(iseq,:) = min( d2layer(iseq,:) / bOut(iseq,:), 1.d0 )
      endif
    enddo
    !$omp end parallel do
    
    do iseq = 1, NSEQALL
      if ( d2rivout_sed(iseq) >= 0.d0 ) then
        iseq0 = iseq
        iseq1 = I1NEXT(iseq)
      else
        iseq0 = I1NEXT(iseq)
        iseq1 = iseq
      endif

      if ( iseq0 > 0 ) then
        d2sedout(iseq,:) = d2sedout(iseq,:) * srate(iseq0,:)
        sedsto(iseq0,:) = max( sedsto(iseq0,:)-abs(d2sedout(iseq,:))*sedDT, 0.d0 )
        d2bedout(iseq,:) = d2bedout(iseq,:) * brate(iseq0,:)
        d2layer(iseq0,:) = max( d2layer(iseq0,:)-abs(d2bedout(iseq,:))*sedDT, 0.d0 )
      endif
      if ( iseq1 > 0 ) then
        sedsto(iseq1,:) = max( sedsto(iseq1,:)+abs(d2sedout(iseq,:))*sedDT, 0.d0 )
        d2layer(iseq1,:) = max( d2layer(iseq1,:)+abs(d2bedout(iseq,:))*sedDT, 0.d0 ) 
      endif

      d2bedout_avg(iseq,:) = d2bedout_avg(iseq,:) + d2bedout(iseq,:)*sedDT
      d2sedout_avg(iseq,:) = d2sedout_avg(iseq,:) + d2sedout(iseq,:)*sedDT
    enddo

  end subroutine calc_advection
  !=====================================================

  subroutine calc_entrainment
    use yos_cmf_sed,           only:  vonKar, d2netflw, d2netflw_avg, d2sedinp, d2seddep, totlyrnum
    
    implicit none
    real(kind=JPRB)               ::  dTmp(NSEQALL,nsed), D(NSEQALL,nsed), Es(NSEQALL,nsed), Zd(NSEQALL,nsed)
    integer(kind=JPIM)            ::  ilyr, ised, iseq
    real(kind=JPRB),save          ::  dTmp1, layerP
    !$omp threadprivate  ( dTmp1, layerP )
    !========

    !$omp parallel do
    do iseq = 1, NSEQALL
      if ( D2RIVDPH(iseq,1) < IGNORE_DPH ) then
        d2netflw(iseq,:) = 0.d0
        cycle
      endif

      !----------------------!
      ! calculate suspension !
      !----------------------!
      if ( sum(d2layer(iseq,:)) == 0.d0 .or. all(susVel(iseq,:)==0.d0) ) then
        Es(iseq,:) = 0.d0
      else
        Es(iseq,:) = susVel(iseq,:) * (1.d0-lambda) * D2RIVWTH(iseq,1) * D2RIVLEN(iseq,1) * d2layer(iseq,:) / sum(d2layer(iseq,:))
        Es(iseq,:) = max( Es(iseq,:), 0.d0 )
      endif

      !----------------------!
      ! calculate deposition !
      !----------------------!
      if ( shearVel(iseq) == 0.d0 .or. all(setVel(:)==0.d0) ) then
        D(iseq,:) = 0.d0
      else
        Zd(iseq,:) = 6.d0 * setVel(:) / vonKar / shearVel(iseq)
        D(iseq,:) = setVel(:) * D2RIVWTH(iseq,1) * D2RIVLEN(iseq,1) * d2sedcon(iseq,:) * Zd(iseq,:) / (1.d0-exp(-Zd(iseq,:))) 
        D(iseq,:) = max( D(iseq,:), 0.d0 )
      endif
      d2netflw(iseq,:) = Es(iseq,:) - D(iseq,:)
   
      !-------------------------------------------!
      ! if >0, suspension ; if <0, deposition     !
      ! adjust netflw if larger than sedsto/layer !
      !-------------------------------------------!
      do ised = 1, nsed
        if ( d2netflw(iseq,ised) == 0.d0 ) then
          cycle
        else if ( d2netflw(iseq,ised) > 0.d0 ) then
          dTmp1 = d2netflw(iseq,ised)*sedDT/(1.d0-lambda)
          if ( dTmp1 < d2layer(iseq,ised) ) then
            d2layer(iseq,ised) = d2layer(iseq,ised) - dTmp1
          else
            d2netflw(iseq,ised) = d2layer(iseq,ised) * (1.d0-lambda) / sedDT
            d2layer(iseq,ised) = 0.d0
          endif
          sedsto(iseq,ised) = sedsto(iseq,ised) + d2netflw(iseq,ised) * sedDT
        else
          if ( abs(d2netflw(iseq,ised))*sedDT < sedsto(iseq,ised) ) then
            sedsto(iseq,ised) = max (sedsto(iseq,ised) - abs(d2netflw(iseq,ised))*sedDT, 0.d0 )
          else
            d2netflw(iseq,ised) = - sedsto(iseq,ised) / sedDT
            sedsto(iseq,ised) = 0.d0
          endif
          d2layer(iseq,ised) = d2layer(iseq,ised) + abs(d2netflw(iseq,ised))*sedDT/(1.d0-lambda)
        endif
      enddo

      sedsto(iseq,:) = sedsto(iseq,:) + d2sedinp(iseq,:)*sedDT    
      if ( sum(sedsto(iseq,:)) > P2RIVSTO(iseq,1) * 0.01d0 ) then
        dTmp(iseq,:) = ( sum(sedsto(iseq,:)) - P2RIVSTO(iseq,1)*0.01d0 ) * sedsto(iseq,:)/sum(sedsto(iseq,:))
        d2netflw(iseq,:) = d2netflw(iseq,:) - dTmp(iseq,:)/sedDT
        sedsto(iseq,:) = sedsto(iseq,:) - dTmp(iseq,:)
        d2layer(iseq,:) = d2layer(iseq,:) + dTmp(iseq,:)/(1.d0-lambda)
      endif
        
      d2netflw_avg(iseq,:) = d2netflw_avg(iseq,:) + d2netflw(iseq,:)*sedDT
    enddo
    !$omp end parallel do
  end subroutine calc_entrainment
  !=====================================================

  subroutine calc_exchange
    ! redistribute into vertical bed layers
    use yos_cmf_sed,           only:  d2seddep, lyrdph, totlyrnum
    
    implicit none
    integer(kind=JPIM)            ::  ilyr, ised, iseq, jlyr, slyr
    real(kind=JPRB)               ::  diff, lyrvol, layerP(nsed), seddepP(totlyrnum+1,nsed), tmp(nsed)

    do iseq = 1, NSEQALL
      lyrvol = lyrdph * D2RIVWTH(iseq,1) * D2RIVLEN(iseq,1)

      if ( minval(d2layer(iseq,:)) < 0.d0 ) d2layer(iseq,:) = max( d2layer(iseq,:), 0.d0 )
      if ( minval(d2seddep(iseq,:,:)) < 0.d0 ) d2seddep(iseq,:,:) = max( d2seddep(iseq,:,:), 0.d0 )

      !---------------------------------------!
      ! if bed storage less than layer volume !
      !---------------------------------------!
      if ( sum(d2layer(iseq,:)) + sum(d2seddep(iseq,:,:)) <= lyrvol ) then
        d2layer(iseq,:) = d2layer(iseq,:) + sum(d2seddep(iseq,:,:),dim=1)
        d2seddep(iseq,:,:) = 0.d0
        cycle
      endif

      !------------------------------------!
      ! distribute into top exchange layer !
      !------------------------------------!
      layerP(:) = d2layer(iseq,:)
      if ( sum(layerP(:)) >= lyrvol ) then
        d2layer(iseq,:) = layerP(:) * min( lyrvol/sum(layerP(:)), 1.d0 )
        layerP(:) = max( layerP(:) - d2layer(iseq,:), 0.d0 )
        slyr = 0
      else if ( sum(d2seddep(iseq,:,:)) > 0.d0 ) then
        layerP(:) = 0.d0
        do ilyr = 1, totlyrnum
          diff = lyrvol - sum(d2layer(iseq,:))
          if ( diff <= 0.d0 ) exit
          if ( sum(d2seddep(iseq,ilyr,:)) <= diff ) then
            d2layer(iseq,:) = d2layer(iseq,:) + d2seddep(iseq,ilyr,:)
            d2seddep(iseq,ilyr,:) = 0.d0
            slyr = ilyr + 1
          else
            tmp(:) = diff * d2seddep(iseq,ilyr,:) / sum(d2seddep(iseq,ilyr,:))
            d2layer(iseq,:) = d2layer(iseq,:) + tmp(:)
            d2seddep(iseq,ilyr,:) = max( d2seddep(iseq,ilyr,:) - tmp(:), 0.d0 )
            slyr = ilyr
            exit
          endif
        enddo
      else
        d2seddep(iseq,:,:) = 0.d0
        cycle
      endif
      if ( sum(d2seddep(iseq,:,:)) == 0.d0 ) cycle

      !-----------------------------------!
      ! distribute remaining bedload into !
      ! vertical deposition layers        !
      !-----------------------------------!
      seddepP(1,:) = layerP(:)
      seddepP(2:,:) = d2seddep(iseq,:,:)
      d2seddep(iseq,:,:) = 0.d0
      do ilyr = 1, totlyrnum - 1
        if ( sum(d2seddep(iseq,ilyr,:)) == lyrvol ) cycle
        do jlyr = slyr+1, totlyrnum + 1
          diff = lyrvol - sum(d2seddep(iseq,ilyr,:))
          if ( diff <= 0.d0 ) exit
          if ( sum(seddepP(jlyr,:)) <= diff ) then
            d2seddep(iseq,ilyr,:) = d2seddep(iseq,ilyr,:) + seddepP(jlyr,:)
            seddepP(jlyr,:) = 0.d0
          else
            tmp(:) = diff * seddepP(jlyr,:) / sum(seddepP(jlyr,:))
            d2seddep(iseq,ilyr,:) = d2seddep(iseq,ilyr,:) + tmp(:)
            seddepP(jlyr,:) = max(seddepP(jlyr,:) - tmp(:), 0.d0)
            exit
          endif
        enddo
      enddo
      
      if ( sum(seddepP) > 0.d0 ) then
        d2seddep(iseq,totlyrnum,:) = sum(seddepP, dim=1)
      endif
    enddo

  end subroutine calc_exchange
end subroutine cmf_calc_sedflw
!####################################################################

end module cmf_calc_sedflw_mod
