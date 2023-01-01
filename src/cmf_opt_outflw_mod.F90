MODULE CMF_OPT_OUTFLW_MOD
!==========================================================
!* PURPOSE: CaMa-Flood optional physics for discharge 
!
!* CONTAINS:
! -- CMF_CALC_OUTFLW_KINE:    calculate outflow by kinematic wave
! -- CMF_CALC_OUTFLW_KINEMIX: calculate outflow by kinematic wave + local inercial using slope threshold
! -- CMF_CALC_OUTPRE:         reconstruct previous time-step outflow by diffusion waev, for LSTOONLY restart
!
! (C) D.Yamazaki & E. Dutra  (U-Tokyo/FCUL)  Aug 2019
!
! Licensed under the Apache License, Version 2.0 (the "License");
!   You may not use this file except in compliance with the License.
!   You may obtain a copy of the License at: http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software distributed under the License is 
!  distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
! See the License for the specific language governing permissions and limitations under the License.
!==========================================================
CONTAINS
!####################################################################
! CMF_CALC_OUTFLW_KINE:    calculate outflow by kinematic wave
! CMF_CALC_OUTFLW_KINEMIX: calculate outflow by kinematic wave + local inercial using slope threshold
! CMF_CALC_OUTPRE:         reconstruct previous time-step outflow by diffusion waev, for LSTOONLY restart
! CMF_CALC_OUTINS:         calculate instantaneous discharge using river network map
!####################################################################
SUBROUTINE CMF_CALC_OUTFLW_KINE
! Calculate discharge, mix kinematic & local inertial, depending on slope
USE PARKIND1,           ONLY: JPIM, JPRB, JPRD
USE YOS_CMF_INPUT,      ONLY: DT,       PMANFLD,  PMINSLP,  LFLBOUT, LSLOPEMOUTH
USE YOS_CMF_MAP,        ONLY: I1NEXT,   NSEQALL,  NSEQRIV
USE YOS_CMF_MAP,        ONLY: B2RIVELV, B2ELEVTN, B2NXTDST, B2RIVWTH
USE YOS_CMF_MAP,        ONLY: B2RIVLEN, B2RIVMAN, B2ELEVSLOPE
USE YOS_CMF_PROG,       ONLY: D2RIVSTO, B2RIVOUT, D2FLDSTO, B2FLBOUT
USE YOS_CMF_DIAG,       ONLY: B2RIVDPH, B2RIVVEL, B2RIVINF, B2FLDDPH, B2FLDINF, B2SFCELV

IMPLICIT NONE
INTEGER(KIND=JPIM),SAVE   ::  ISEQ, JSEQ
REAL(KIND=JPRB),SAVE      ::  BSLOPE,   BAREA , BVEL,   BSLOPE_F, BARE_F, BVEL_F
!$OMP THREADPRIVATE    (JSEQ, BSLOPE,   BAREA , BVEL,   BSLOPE_F, BARE_F, BVEL_F)
!================================================

!*** 0. calculate surface water elevation, reset inflow

!$OMP PARALLEL DO
DO ISEQ=1, NSEQALL
  B2SFCELV(ISEQ,1) = B2RIVELV(ISEQ,1) + B2RIVDPH(ISEQ,1)
  B2RIVINF(ISEQ,1) = 0._JPRB
  B2FLDINF(ISEQ,1) = 0._JPRB
END DO
!$OMP END PARALLEL DO

!============================
!*** 1a. discharge for usual river grid

!$OMP PARALLEL DO
DO ISEQ=1, NSEQRIV
  JSEQ   = I1NEXT(ISEQ)
! === river flow
  BSLOPE = (B2ELEVTN(ISEQ,1)-B2ELEVTN(JSEQ,1)) * B2NXTDST(ISEQ,1)**(-1.)
  BSLOPE = max(BSLOPE,PMINSLP)
  BVEL   = B2RIVMAN(ISEQ,1)**(-1.) * BSLOPE**0.5 * B2RIVDPH(ISEQ,1)**(2./3.)
  BAREA  = B2RIVWTH(ISEQ,1) * B2RIVDPH(ISEQ,1)

  B2RIVVEL(ISEQ,1) = BVEL
  B2RIVOUT(ISEQ,1) = BAREA * BVEL
  B2RIVOUT(ISEQ,1) = MIN(  B2RIVOUT(ISEQ,1)*1._JPRD, D2RIVSTO(ISEQ,1)/DT )
!=== floodplain flow
  IF( LFLBOUT )THEN
    BSLOPE_F = min( 0.005_JPRB,BSLOPE )    !! set min [instead of using weir equation for efficiency]
    BVEL_F   = PMANFLD**(-1.) * BSLOPE_F**0.5 * B2FLDDPH(ISEQ,1)**(2./3.)
    BARE_F   = D2FLDSTO(ISEQ,1) * B2RIVLEN(ISEQ,1)**(-1.)
    BARE_F   = MAX( BARE_F - B2FLDDPH(ISEQ,1)*B2RIVWTH(ISEQ,1), 0._JPRB )   !! remove above river channel area

    B2FLBOUT(ISEQ,1) = BARE_F * BVEL_F
    B2FLBOUT(ISEQ,1) = MIN(  B2FLBOUT(ISEQ,1)*1._JPRD, D2FLDSTO(ISEQ,1)/DT )
  ENDIF
END DO
!$OMP END PARALLEL DO

#ifndef NoAtom_CMF
!$OMP PARALLEL DO  !! No OMP Atomic for bit-identical simulation (set in Mkinclude)
#endif
DO ISEQ=1, NSEQRIV ! for normal pixels
  JSEQ=I1NEXT(ISEQ)
!$OMP ATOMIC
  B2RIVINF(JSEQ,1) = B2RIVINF(JSEQ,1) + B2RIVOUT(ISEQ,1)             !! total inflow to a grid (from upstream)
!$OMP ATOMIC
  B2FLDINF(JSEQ,1) = B2FLDINF(JSEQ,1) + B2FLBOUT(ISEQ,1)
END DO
#ifndef NoAtom_CMF
!$OMP END PARALLEL DO  !! No OMP Atomic for bit-identical simulation (set in Mkinclude)
#endif
!============================
!*** 1b. discharge for river mouth grids

!$OMP PARALLEL DO
DO ISEQ=NSEQRIV+1, NSEQALL
!=== Kinematic approach, river mouth flow
  IF ( LSLOPEMOUTH ) THEN
    ! prescribed slope 
    BSLOPE = B2ELEVSLOPE(ISEQ,1)
  ELSE
    BSLOPE = PMINSLP
  ENDIF
  BVEL   = B2RIVMAN(ISEQ,1)**(-1.) * BSLOPE**0.5 * B2RIVDPH(ISEQ,1)**(2./3.)
  BAREA  = B2RIVWTH(ISEQ,1) * B2RIVDPH(ISEQ,1)

  B2RIVVEL(ISEQ,1) = BVEL
  B2RIVOUT(ISEQ,1) = BAREA * BVEL
  B2RIVOUT(ISEQ,1) = MIN(  B2RIVOUT(ISEQ,1)*1._JPRD, D2RIVSTO(ISEQ,1)/DT )

!=== kinematic, floodplain mouth flow
  IF( LFLBOUT )THEN
    BSLOPE_F = min( 0.005_JPRB,BSLOPE )    !! set min [instead of using weir equation for efficiency]
    BVEL_F  = PMANFLD**(-1.) * BSLOPE_F**0.5 * B2FLDDPH(ISEQ,1)**(2./3.)
    BARE_F   = D2FLDSTO(ISEQ,1) * B2RIVLEN(ISEQ,1)**(-1.)
    BARE_F   = MAX( BARE_F - B2FLDDPH(ISEQ,1)*B2RIVWTH(ISEQ,1), 0._JPRB )   !! remove above river channel area

    B2FLBOUT(ISEQ,1) = BARE_F * BVEL_F
    B2FLBOUT(ISEQ,1) = MIN(  B2FLBOUT(ISEQ,1)*1._JPRD, D2FLDSTO(ISEQ,1)/DT )
  ENDIF
END DO
!$OMP END PARALLEL DO

END SUBROUTINE CMF_CALC_OUTFLW_KINE
!####################################################################





!####################################################################
SUBROUTINE CMF_CALC_OUTFLW_KINEMIX
! Calculate discharge, mix kinematic & local inertial, depending on slope
USE PARKIND1,           ONLY: JPIM, JPRB, JPRD
USE YOS_CMF_INPUT,      ONLY: DT,       LFLBOUT
USE YOS_CMF_INPUT,      ONLY: PDSTMTH,  PMANFLD,  PGRV , PMINSLP
USE YOS_CMF_MAP,        ONLY: I1NEXT,   NSEQALL,  NSEQRIV,  NSEQMAX
USE YOS_CMF_MAP,        ONLY: B2RIVELV, B2ELEVTN, B2NXTDST, B2RIVWTH, B2RIVHGT
USE YOS_CMF_MAP,        ONLY: B2RIVLEN, B2RIVMAN, I2MASK, B2DWNELV
USE YOS_CMF_PROG,       ONLY: D2RIVSTO, B2RIVOUT, D2FLDSTO, B2FLBOUT
USE YOS_CMF_PROG,       ONLY: B2RIVOUT_PRE, B2RIVDPH_PRE, B2FLBOUT_PRE, B2FLDSTO_PRE
USE YOS_CMF_DIAG,       ONLY: B2RIVDPH, B2RIVVEL, B2RIVINF, B2FLDDPH, B2FLDINF, B2SFCELV
IMPLICIT NONE
!*** Local
REAL(KIND=JPRB)            :: B2SFCELV_PRE(NSEQMAX,1)                  !! water surface elevation (t-1) [m]
REAL(KIND=JPRB)            :: B2FLDDPH_PRE(NSEQMAX,1)                  !! floodplain depth (t-1)        [m]
REAL(KIND=JPRB)            :: B2STOOUT(NSEQMAX,1)                      !! total outflow from a grid     [m3]
REAL(KIND=JPRB)            :: B2RATE(NSEQMAX,1)                        !! outflow correction
!
INTEGER(KIND=JPIM),SAVE    :: ISEQ, JSEQ
REAL(KIND=JPRB),SAVE       :: BSLOPE,   BOUT_PRE,   BFLW,   BFLW_PRE,   BFLW_IMP,   BAREA , BVEL
REAL(KIND=JPRB),SAVE       :: BSLOPE_F, BOUT_PRE_F, BFLW_F, BFLW_PRE_F, BFLW_IMP_F, BARE_F, BVEL_F, BARE_PRE_F, BARE_IMP_F
REAL(KIND=JPRB),SAVE       :: OUT_R1, OUT_R2, OUT_F1, OUT_F2, BIUP, BIDW, BSFCMAX, BSFCMAX_PRE
!$OMP THREADPRIVATE    (JSEQ, BSLOPE,   BOUT_PRE,   BFLW,   BFLW_PRE,   BFLW_IMP,   BAREA,  BVEL)
!$OMP THREADPRIVATE    (      BSLOPE_F, BOUT_PRE_F, BFLW_F, BFLW_PRE_F, BFLW_IMP_F, BARE_F, BVEL_F, BARE_PRE_F, BARE_IMP_F)
!$OMP THREADPRIVATE    (      OUT_R1, OUT_R2, OUT_F1, OUT_F2, BIUP, BIDW, BSFCMAX, BSFCMAX_PRE)
!================================================

!$OMP PARALLEL DO
DO ISEQ=1, NSEQALL
  B2SFCELV(ISEQ,1)     = B2RIVELV(ISEQ,1) + B2RIVDPH(ISEQ,1)
  B2SFCELV_PRE(ISEQ,1) = B2RIVELV(ISEQ,1) + B2RIVDPH_PRE(ISEQ,1)
  B2FLDDPH_PRE(ISEQ,1) = MAX( B2RIVDPH_PRE(ISEQ,1)-B2RIVHGT(ISEQ,1), 0._JPRB )

  B2RIVINF(ISEQ,1) = 0._JPRB
  B2FLDINF(ISEQ,1) = 0._JPRB
  B2STOOUT(ISEQ,1) = 0._JPRB
  B2RATE(ISEQ,1)   = 1._JPRB
END DO
!$OMP END PARALLEL DO

!$OMP PARALLEL DO
DO ISEQ=1, NSEQRIV                                                    !! for normal cells
  JSEQ=I1NEXT(ISEQ) ! next cell's pixel
  
  IF (I2MASK(ISEQ,1) == 0 ) THEN

    BSFCMAX    =MAX( B2SFCELV(ISEQ,1),    B2SFCELV(JSEQ,1) )
    BSFCMAX_PRE=MAX( B2SFCELV_PRE(ISEQ,1),B2SFCELV_PRE(JSEQ,1) )
    BSLOPE = ( B2SFCELV(ISEQ,1)-B2SFCELV(JSEQ,1) ) * B2NXTDST(ISEQ,1)**(-1.)
    BSLOPE_F = MAX( -0.005_JPRB, min( 0.005_JPRB,BSLOPE ))    !! set max&min [instead of using weir equation for efficiency]

  !=== River Flow ===
    BFLW   = BSFCMAX - B2RIVELV(ISEQ,1)        !!  flow cross-section depth
    BAREA  = B2RIVWTH(ISEQ,1) * BFLW                                            !!  flow cross-section area

    BFLW_PRE=BSFCMAX_PRE - B2RIVELV(ISEQ,1)
    BFLW_IMP=MAX( (BFLW*BFLW_PRE)**0.5 ,1.E-6_JPRB )                                            !! semi implicit flow depth

    IF( BFLW_IMP>1.E-5 .and. BAREA>1.E-5 )THEN 
      BOUT_PRE= B2RIVOUT_PRE(ISEQ,1) * B2RIVWTH(ISEQ,1)**(-1.)                         !! outflow (t-1) [m2/s] (unit width)
      B2RIVOUT(ISEQ,1) = B2RIVWTH(ISEQ,1) * ( BOUT_PRE + PGRV*DT*BFLW_IMP*BSLOPE ) &
                               * ( 1. + PGRV*DT*B2RIVMAN(ISEQ,1)**2. * abs(BOUT_PRE)*BFLW_IMP**(-7./3.) )**(-1.)
      B2RIVVEL(ISEQ,1) = B2RIVOUT(ISEQ,1) * BAREA**(-1.)
    ELSE
      B2RIVOUT(ISEQ,1) = 0._JPRB
      B2RIVVEL(ISEQ,1) = 0._JPRB
    ENDIF

  !=== Floodplain Flow ===
    IF( LFLBOUT )THEN
      BFLW_F   = MAX( BSFCMAX-B2ELEVTN(ISEQ,1), 0._JPRB )
      BARE_F   = D2FLDSTO(ISEQ,1) * B2RIVLEN(ISEQ,1)**(-1.)
      BARE_F   = MAX( BARE_F - B2FLDDPH(ISEQ,1)*B2RIVWTH(ISEQ,1), 0._JPRB )   !! remove above river channel area
  
      BFLW_PRE_F = BSFCMAX_PRE - B2ELEVTN(ISEQ,1)
      BFLW_IMP_F = MAX( (MAX(BFLW_F*BFLW_PRE_F,0._JPRB))**0.5, 1.E-6_JPRB )
  
      BARE_PRE_F = B2FLDSTO_PRE(ISEQ,1) * B2RIVLEN(ISEQ,1)**(-1.)
      BARE_PRE_F = MAX( BARE_PRE_F - B2FLDDPH_PRE(ISEQ,1)*B2RIVWTH(ISEQ,1), 1.E-6_JPRB )   !! remove above river channel area
      BARE_IMP_F = max( (BARE_F*BARE_PRE_F)**0.5, 1.E-6_JPRB )
  
      IF( BFLW_IMP_F>1.E-5 .and. BARE_IMP_F>1.E-5 )THEN 
        BOUT_PRE_F = B2FLBOUT_PRE(ISEQ,1)
        B2FLBOUT(ISEQ,1) = ( BOUT_PRE_F + PGRV*DT*BARE_IMP_F*BSLOPE_F ) &
                          * (1. + PGRV*DT*PMANFLD**2. * abs(BOUT_PRE_F)*BFLW_IMP_F**(-4./3.)*BARE_IMP_F**(-1.) )**(-1.)
      ELSE
        B2FLBOUT(ISEQ,1) = 0._JPRB
      ENDIF
  
      IF( B2FLBOUT(ISEQ,1)*B2RIVOUT(ISEQ,1)<0._JPRB ) B2FLBOUT(ISEQ,1)=0._JPRB  !! stabilization
    ENDIF

  ELSE
    ! Kinematic wave, river flow
    BSLOPE = (B2ELEVTN(ISEQ,1)-B2ELEVTN(JSEQ,1)) * B2NXTDST(ISEQ,1)**(-1.)
    BSLOPE = max(BSLOPE,PMINSLP)
    BVEL   = B2RIVMAN(ISEQ,1)**(-1.) * BSLOPE**0.5 * B2RIVDPH(ISEQ,1)**(2./3.)
    BAREA  = B2RIVWTH(ISEQ,1) * B2RIVDPH(ISEQ,1)

    B2RIVVEL(ISEQ,1) = BVEL
    B2RIVOUT(ISEQ,1) = BAREA * BVEL
    B2RIVOUT(ISEQ,1) = MIN(  B2RIVOUT(ISEQ,1)*1._JPRD, D2RIVSTO(ISEQ,1)/DT )
    !! kinematic wave, floodplain flow
    IF( LFLBOUT )THEN
      BSLOPE_F = min( 0.005_JPRB,BSLOPE )    !! set max&min [instead of using weir equation for efficiency]
      BVEL_F  = PMANFLD**(-1.) * BSLOPE_F**0.5 * B2FLDDPH(ISEQ,1)**(2./3.)
      BARE_F   = D2FLDSTO(ISEQ,1) * B2RIVLEN(ISEQ,1)**(-1.D0)
      BARE_F   = MAX( BARE_F - B2FLDDPH(ISEQ,1)*B2RIVWTH(ISEQ,1), 0._JPRB )   !! remove above river channel area
    
      B2FLBOUT(ISEQ,1) = BARE_F * BVEL_F
      B2FLBOUT(ISEQ,1) = MIN(  B2FLBOUT(ISEQ,1)*1._JPRD, D2FLDSTO(ISEQ,1)/DT )
    ENDIF
  ENDIF 
    
END DO
!$OMP END PARALLEL DO

#ifndef NoAtom_CMF
!$OMP PARALLEL DO  !! No OMP Atomic for bit-identical simulation (set in Mkinclude)
#endif
DO ISEQ=1, NSEQRIV                                                    !! for normal cells
  JSEQ=I1NEXT(ISEQ) ! next cell's pixel
  OUT_R1 = max(  B2RIVOUT(ISEQ,1),0._JPRB )
  OUT_R2 = max( -B2RIVOUT(ISEQ,1),0._JPRB )
  OUT_F1 = max(  B2FLBOUT(ISEQ,1),0._JPRB )
  OUT_F2 = max( -B2FLBOUT(ISEQ,1),0._JPRB )
  BIUP=(OUT_R1+OUT_F1)*DT
  BIDW=(OUT_R2+OUT_F2)*DT
!$OMP ATOMIC
  B2STOOUT(ISEQ,1) = B2STOOUT(ISEQ,1) + BIUP 
!$OMP ATOMIC
  B2STOOUT(JSEQ,1) = B2STOOUT(JSEQ,1) + BIDW 
END DO
#ifndef NoAtom_CMF
!$OMP END PARALLEL DO  !! No OMP Atomic for bit-identical simulation (set in Mkinclude)
#endif


!$OMP PARALLEL DO                                                     !! for river mouth grids
DO ISEQ=NSEQRIV+1, NSEQALL
  IF (I2MASK(ISEQ,1) == 0 ) THEN

    BSLOPE = ( B2SFCELV(ISEQ,1) - B2DWNELV(ISEQ,1) ) * PDSTMTH ** (-1.)
    BSLOPE_F = MAX( -0.005_JPRB, min( 0.005_JPRB,BSLOPE ))    !! set max&min [instead of using weir equation for efficiency]
  !=== river mouth flow ===

    BFLW   = B2RIVDPH(ISEQ,1)
    BAREA  = B2RIVWTH(ISEQ,1) * BFLW

    BFLW_PRE=B2RIVDPH_PRE(ISEQ,1)
    BFLW_IMP=MAX( (BFLW*BFLW_PRE)**0.5, 1.E-6_JPRB )                                    !! semi implicit flow depth

    IF( BFLW_IMP>1.E-5 .and. BAREA>1.E-5 )THEN 
      BOUT_PRE = B2RIVOUT_PRE(ISEQ,1) * B2RIVWTH(ISEQ,1)**(-1.)
      B2RIVOUT(ISEQ,1) = B2RIVWTH(ISEQ,1) * ( BOUT_PRE + PGRV*DT*BFLW_IMP*BSLOPE ) &
                               * ( 1. + PGRV*DT*B2RIVMAN(ISEQ,1)**2. * abs(BOUT_PRE)*BFLW_IMP**(-7./3.) )**(-1.)
      B2RIVVEL(ISEQ,1) = B2RIVOUT(ISEQ,1) * BAREA**(-1.)
    ELSE
      B2RIVOUT(ISEQ,1) = 0._JPRB
      B2RIVVEL(ISEQ,1) = 0._JPRB
    ENDIF

  !=== floodplain mouth flow ===
    IF( LFLBOUT )THEN
      BFLW_F   = B2SFCELV(ISEQ,1)-B2ELEVTN(ISEQ,1)

      BARE_F   = D2FLDSTO(ISEQ,1) * B2RIVLEN(ISEQ,1)**(-1.)
      BARE_F   = MAX( BARE_F - B2FLDDPH(ISEQ,1)*B2RIVWTH(ISEQ,1), 0._JPRB )   !! remove above river channel area

      BFLW_PRE_F = B2SFCELV_PRE(ISEQ,1)-B2ELEVTN(ISEQ,1)
      BFLW_IMP_F = MAX( (MAX(BFLW_F*BFLW_PRE_F,0._JPRB))**0.5, 1.E-6_JPRB )

      BARE_PRE_F = B2FLDSTO_PRE(ISEQ,1) * B2RIVLEN(ISEQ,1)**(-1.)
      BARE_PRE_F = MAX( BARE_PRE_F - B2FLDDPH_PRE(ISEQ,1)*B2RIVWTH(ISEQ,1), 1.E-6_JPRB )   !! remove above river channel area
      BARE_IMP_F = max( (BARE_F*BARE_PRE_F)**0.5, 1.E-6_JPRB )

      IF( BFLW_IMP_F>1.E-5 .and. BARE_IMP_F>1.E-5 )THEN 
        BOUT_PRE_F = B2FLBOUT_PRE(ISEQ,1)
        B2FLBOUT(ISEQ,1) = ( BOUT_PRE_F + PGRV*DT*BARE_IMP_F*BSLOPE_F ) &
                          * (1. + PGRV*DT*PMANFLD**2.*abs(BOUT_PRE_F)*BFLW_IMP_F**(-4./3.)*BARE_IMP_F**(-1.) )**(-1.)
      ELSE
        B2FLBOUT(ISEQ,1) = 0._JPRB
      ENDIF

      IF( B2FLBOUT(ISEQ,1)*B2RIVOUT(ISEQ,1)<0._JPRB ) B2FLBOUT(ISEQ,1)=0._JPRB  !! stabilization
    ENDIF

  ELSE
    ! Kinematic approach, river channel flow
    BSLOPE = PMINSLP
    BVEL   = B2RIVMAN(ISEQ,1)**(-1.) * BSLOPE**0.5 * B2RIVDPH(ISEQ,1)**(2./3.)
    BAREA  = B2RIVWTH(ISEQ,1) * B2RIVDPH(ISEQ,1)

    B2RIVVEL(ISEQ,1) = BVEL
    B2RIVOUT(ISEQ,1) = BAREA * BVEL
    B2RIVOUT(ISEQ,1) = MIN(  B2RIVOUT(ISEQ,1)*1._JPRD, D2RIVSTO(ISEQ,1)/DT )

    !! kinematic wave, floodplain flow
    IF( LFLBOUT )THEN
      BSLOPE_F = min( 0.005_JPRB,BSLOPE )    !! set max&min [instead of using weir equation for efficiency]
      BVEL_F  = PMANFLD**(-1.) * BSLOPE_F**0.5 * B2FLDDPH(ISEQ,1)**(2./3.)
      BARE_F   = D2FLDSTO(ISEQ,1) * B2RIVLEN(ISEQ,1)**(-1.)
      BARE_F   = MAX( BARE_F - B2FLDDPH(ISEQ,1)*B2RIVWTH(ISEQ,1), 0._JPRB )   !! remove above river channel area
    
      B2FLBOUT(ISEQ,1) = BARE_F * BVEL_F
      B2FLBOUT(ISEQ,1) = MIN(  B2FLBOUT(ISEQ,1)*1._JPRD, D2FLDSTO(ISEQ,1)/DT )
    ENDIF
  ENDIF 
 
!=== check outflow ===
  OUT_R1 = max(  B2RIVOUT(ISEQ,1),0._JPRB )
  OUT_F1 = max(  B2FLBOUT(ISEQ,1),0._JPRB )
  B2STOOUT(ISEQ,1) = B2STOOUT(ISEQ,1) + OUT_R1*DT + OUT_F1*DT
END DO
!$OMP END PARALLEL DO


!! === ourflow modification to avoid negative storage ======

!$OMP PARALLEL DO                                                     !! outflow correcttion if total outflow > storage
DO ISEQ=1, NSEQALL
  IF ( B2STOOUT(ISEQ,1) > 1.E-8 .AND. I2MASK(ISEQ,1) == 0 ) THEN
    B2RATE(ISEQ,1)   = min( (D2RIVSTO(ISEQ,1)+D2FLDSTO(ISEQ,1)) * B2STOOUT(ISEQ,1)**(-1.), 1._JPRD )
  ENDIF
END DO
!$OMP END PARALLEL DO

#ifndef NoAtom_CMF
!$OMP PARALLEL DO  !! No OMP Atomic for bit-identical simulation (set in Mkinclude)
#endif
DO ISEQ=1, NSEQRIV ! for normal pixels
  JSEQ=I1NEXT(ISEQ)
  IF( B2RIVOUT(ISEQ,1) >= 0._JPRB )THEN
    B2RIVOUT(ISEQ,1) = B2RIVOUT(ISEQ,1)*B2RATE(ISEQ,1)
    B2FLBOUT(ISEQ,1) = B2FLBOUT(ISEQ,1)*B2RATE(ISEQ,1)
  ELSE
    B2RIVOUT(ISEQ,1) = B2RIVOUT(ISEQ,1)*B2RATE(JSEQ,1)
    B2FLBOUT(ISEQ,1) = B2FLBOUT(ISEQ,1)*B2RATE(JSEQ,1)
  ENDIF
!$OMP ATOMIC
  B2RIVINF(JSEQ,1) = B2RIVINF(JSEQ,1) + B2RIVOUT(ISEQ,1)             !! total inflow to a grid (from upstream)
!$OMP ATOMIC
  B2FLDINF(JSEQ,1) = B2FLDINF(JSEQ,1) + B2FLBOUT(ISEQ,1)
END DO
#ifndef NoAtom_CMF
!$OMP END PARALLEL DO  !! No OMP Atomic for bit-identical simulation (set in Mkinclude)
#endif

!$OMP PARALLEL DO
DO ISEQ=NSEQRIV+1, NSEQALL ! for river mouth
  B2RIVOUT(ISEQ,1) = B2RIVOUT(ISEQ,1)*B2RATE(ISEQ,1)
  B2FLBOUT(ISEQ,1) = B2FLBOUT(ISEQ,1)*B2RATE(ISEQ,1)
END DO
!$OMP END PARALLEL DO

END SUBROUTINE CMF_CALC_OUTFLW_KINEMIX
!####################################################################





!####################################################################
SUBROUTINE CMF_CALC_OUTPRE
! to Calculate discharge, diffusive wave, initialization for storage only restart
USE PARKIND1,        ONLY: JPIM, JPRB
USE YOS_CMF_INPUT,   ONLY: LFLBOUT,  LPTHOUT
USE YOS_CMF_INPUT,   ONLY: PMANFLD,  PDSTMTH
USE YOS_CMF_MAP,     ONLY: I1NEXT,   NSEQALL,  NSEQRIV,  NPTHOUT
USE YOS_CMF_MAP,     ONLY: B2RIVELV, B2ELEVTN, B2NXTDST, B2RIVWTH, B2RIVLEN, B2RIVMAN, B2DWNELV
USE YOS_CMF_MAP,     ONLY: NPTHOUT,  NPTHLEV, PTH_UPST, PTH_DOWN, PTH_DST, PTH_ELV, PTH_WTH, PTH_MAN
USE YOS_CMF_PROG,    ONLY: B2RIVOUT_PRE, B2FLBOUT_PRE, B1PTHFLW_PRE ,B2RIVDPH_PRE         !! output
USE YOS_CMF_PROG,    ONLY: D2FLDSTO                                                       !! input
USE YOS_CMF_DIAG,    ONLY: B2RIVDPH, B2SFCELV, B2FLDDPH                                   !! input
IMPLICIT NONE
INTEGER(KIND=JPIM),SAVE :: ISEQ, JSEQ, IPTH, ILEV, ISEQP, JSEQP
REAL(KIND=JPRB),SAVE    :: BSFCMAX, BSLOPE, BAREA, BFLW, BSLOPE_F, BARE_F, BFLW_F
!$OMP THREADPRIVATE       (BSFCMAX, BSLOPE, BAREA, BFLW, BSLOPE_F, BARE_F, BFLW_F, JSEQ, ILEV, ISEQP, JSEQP)
!================================================

!$OMP PARALLEL DO
DO ISEQ=1, NSEQALL
  B2SFCELV(ISEQ,1)     = B2RIVELV(ISEQ,1) + B2RIVDPH(ISEQ,1)
  B2RIVDPH_PRE(ISEQ,1) = B2RIVDPH(ISEQ,1)                           !! bugfix v362
END DO
!$OMP END PARALLEL DO

!$OMP PARALLEL DO
DO ISEQ=1, NSEQRIV                                                    !! for normal cells
  JSEQ=I1NEXT(ISEQ)

  BSFCMAX    =MAX( B2SFCELV(ISEQ,1),    B2SFCELV(JSEQ,1) )
  BSLOPE = ( B2SFCELV(ISEQ,1)-B2SFCELV(JSEQ,1) ) * B2NXTDST(ISEQ,1)**(-1.)
  BSLOPE_F = MAX( -0.005_JPRB, min( 0.005_JPRB,BSLOPE ))    !! set max&min [instead of using weir equation for efficiency]

!=== River Flow ===
  BFLW  = BSFCMAX - B2RIVELV(ISEQ,1)
  BAREA = B2RIVWTH(ISEQ,1) * BFLW

  IF( BAREA>1.E-5 )THEN
    B2RIVOUT_PRE(ISEQ,1) = BAREA * ( B2RIVMAN(ISEQ,1)**(-1.) * BFLW**(2./3.) * abs(BSLOPE)**(0.5) )
    IF( BSLOPE<0._JPRB ) B2RIVOUT_PRE(ISEQ,1)=-B2RIVOUT_PRE(ISEQ,1)
  ELSE
    B2RIVOUT_PRE(ISEQ,1) = 0._JPRB
  ENDIF

!=== Floodplain Flow ===
  BFLW_F   = MAX( BSFCMAX-B2ELEVTN(ISEQ,1), 0._JPRB )
  BARE_F   = D2FLDSTO(ISEQ,1) * B2RIVLEN(ISEQ,1)**(-1.)
  BARE_F   = MAX( BARE_F - B2FLDDPH(ISEQ,1)*B2RIVWTH(ISEQ,1), 0._JPRB )   !! remove above river channel area

  IF( BARE_F>1.E-5 )THEN 
    B2FLBOUT_PRE(ISEQ,1) = BARE_F * ( PMANFLD**(-1.) * BFLW_F**(2./3.) * abs(BSLOPE_F)**(0.5) )
    IF( BSLOPE_F<0._JPRB ) B2FLBOUT_PRE(ISEQ,1)=-B2FLBOUT_PRE(ISEQ,1)
  ELSE
    B2FLBOUT_PRE(ISEQ,1) = 0._JPRB
  ENDIF
END DO
!$OMP END PARALLEL DO


!$OMP PARALLEL DO                                                     !! for river mouth grids
DO ISEQ=NSEQRIV+1, NSEQALL
  BSLOPE = ( B2SFCELV(ISEQ,1)-B2DWNELV(ISEQ,1) ) * PDSTMTH**(-1.)
  BSLOPE_F = MAX( -0.005_JPRB, min( 0.005_JPRB,BSLOPE ))    !! set max&min [instead of using weir equation for efficiency]

!=== river mouth flow ===
  BFLW   = B2RIVDPH(ISEQ,1)
  BAREA  = B2RIVWTH(ISEQ,1) * BFLW
  IF( BAREA>1.E-5 )THEN
    B2RIVOUT_PRE(ISEQ,1) = BAREA * ( B2RIVMAN(ISEQ,1)**(-1.) * BFLW**(2./3.) * abs(BSLOPE)**(0.5) )
    IF( BSLOPE<0._JPRB ) B2RIVOUT_PRE(ISEQ,1)=-B2RIVOUT_PRE(ISEQ,1)
  ELSE
    B2RIVOUT_PRE(ISEQ,1) = 0._JPRB
  ENDIF
!=== floodplain mouth flow ===
  BFLW_F   = B2SFCELV(ISEQ,1)-B2ELEVTN(ISEQ,1)
  BARE_F   = D2FLDSTO(ISEQ,1) * B2RIVLEN(ISEQ,1)**(-1.)
  BARE_F   = MAX( BARE_F - B2FLDDPH(ISEQ,1)*B2RIVWTH(ISEQ,1), 0._JPRB )   !! remove above river channel area
  IF( BARE_F>1.E-5 )THEN
    B2FLBOUT_PRE(ISEQ,1) = BARE_F * ( PMANFLD**(-1.) * BFLW_F**(2./3.) * abs(BSLOPE_F)**(0.5) )
    IF( BSLOPE_F<0._JPRB ) B2FLBOUT_PRE(ISEQ,1)=-B2FLBOUT_PRE(ISEQ,1)
  ELSE
    B2FLBOUT_PRE(ISEQ,1) = 0._JPRB
  ENDIF
END DO
!$OMP END PARALLEL DO


IF( LPTHOUT )THEN
 !$OMP PARALLEL DO
 DO IPTH=1, NPTHOUT  
  ISEQP=PTH_UPST(IPTH)
  JSEQP=PTH_DOWN(IPTH)

  BSLOPE  = (B2SFCELV(ISEQP,1)-B2SFCELV(JSEQP,1)) * PTH_DST(IPTH)**(-1.)
  DO ILEV=1, NPTHLEV
    BFLW = MAX(B2SFCELV(ISEQP,1),B2SFCELV(JSEQP,1)) - PTH_ELV(IPTH,ILEV) 
    BFLW = MAX(BFLW,0._JPRB)

    IF( BFLW>1.E-5 )THEN
      B1PTHFLW_PRE(IPTH,ILEV) = PTH_WTH(IPTH,ILEV) * BFLW * ( PTH_MAN(ILEV)**(-1.) * BFLW**(2./3.) * abs(BSLOPE)**(0.5) )
      IF( BSLOPE<0._JPRB ) B1PTHFLW_PRE(IPTH,ILEV)=-B1PTHFLW_PRE(IPTH,ILEV)
    ELSE
      B1PTHFLW_PRE(IPTH,ILEV) = 0._JPRB
    ENDIF
  END DO
 END DO
 !$OMP END PARALLEL DO
ENDIF

!! when high-water flow is not defined
IF( .not. LFLBOUT )THEN
  B2FLBOUT_PRE(:,:)=0.D0
ENDIF

END SUBROUTINE CMF_CALC_OUTPRE
!####################################################################



!####################################################################
SUBROUTINE CMF_CALC_OUTINS
! to   Calculate discharge, INST. NO ROUTING DELAY
USE PARKIND1,       ONLY: JPIM, JPRB
USE YOS_CMF_MAP,    ONLY: I1NEXT, NSEQMAX
USE YOS_CMF_PROG,   ONLY: B2RUNOFF
USE YOS_CMF_DIAG,   ONLY: B2OUTINS
IMPLICIT NONE
!!** LOCAL
INTEGER(KIND=JPIM)     :: ISEQ,JSEQ
!!==============================
B2OUTINS(:,:)=B2RUNOFF(:,:)

!! Do not use OpenMP
DO ISEQ=1, NSEQMAX
  JSEQ=I1NEXT(ISEQ)
  IF( JSEQ>0 )THEN
    B2OUTINS(JSEQ,1)=B2OUTINS(JSEQ,1)+B2OUTINS(ISEQ,1)
  ENDIF
END DO

END SUBROUTINE CMF_CALC_OUTINS
!####################################################################

END MODULE CMF_OPT_OUTFLW_MOD
