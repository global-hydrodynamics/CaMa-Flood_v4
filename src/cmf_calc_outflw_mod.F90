MODULE CMF_CALC_OUTFLW_MOD
!==========================================================
!* PURPOSE: CaMa-Flood physics for river&floodplain discharge
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
! -- CMF_CALC_OUTFLW
! --
! --
!####################################################################
SUBROUTINE CMF_CALC_OUTFLW
USE PARKIND1,           ONLY: JPIM, JPRB, JPRD
USE YOS_CMF_INPUT,      ONLY: DT,       PDSTMTH,  PMANFLD,  PGRV,     LFLDOUT, LSLOPEMOUTH
USE YOS_CMF_MAP,        ONLY: I1NEXT,   NSEQALL,  NSEQRIV,  NSEQMAX
USE YOS_CMF_MAP,        ONLY: B2RIVELV, B2ELEVTN, B2NXTDST, B2RIVWTH, B2RIVHGT
USE YOS_CMF_MAP,        ONLY: B2RIVLEN, B2RIVMAN, B2DWNELV, B2ELEVSLOPE
USE YOS_CMF_PROG,       ONLY: D2RIVSTO, B2RIVOUT, D2FLDSTO, B2FLDOUT
USE YOS_CMF_PROG,       ONLY: B2RIVOUT_PRE, B2RIVDPH_PRE, B2FLDOUT_PRE, B2FLDSTO_PRE
USE YOS_CMF_DIAG,       ONLY: B2RIVDPH, B2RIVVEL, B2RIVINF, B2FLDDPH, B2FLDINF, B2SFCELV
IMPLICIT NONE
!*** Local
REAL(KIND=JPRB)            :: B2SFCELV_PRE(NSEQMAX,1)                  !! water surface elevation (t-1) [m]
REAL(KIND=JPRB)            :: B2FLDDPH_PRE(NSEQMAX,1)                      !! floodplain depth (t-1)        [m]
REAL(KIND=JPRB)            :: B2STOOUT(NSEQMAX,1)                      !! total outflow from a grid     [m3]
REAL(KIND=JPRB)            :: B2RATE(NSEQMAX,1)                        !! outflow correction
! save for OpenMP
INTEGER(KIND=JPIM),SAVE    :: ISEQ, JSEQ
REAL(KIND=JPRB),SAVE       :: BSLOPE,   BOUT_PRE,   BFLW,   BFLW_PRE,   BFLW_IMP,   BAREA 
REAL(KIND=JPRB),SAVE       :: BSLOPE_F, BOUT_PRE_F, BFLW_F, BFLW_PRE_F, BFLW_IMP_F, BARE_F, BARE_PRE_F, BARE_IMP_F
REAL(KIND=JPRB),SAVE       :: OUT_R1,   OUT_R2,     OUT_F1, OUT_F2,     BIUP,       BIDW,   BSFCMAX,    BSFCMAX_PRE
!$OMP THREADPRIVATE    (JSEQ, BSLOPE,   BOUT_PRE,   BFLW,   BFLW_PRE,   BFLW_IMP,   BAREA  )
!$OMP THREADPRIVATE    (      BSLOPE_F, BOUT_PRE_F, BFLW_F, BFLW_PRE_F, BFLW_IMP_F, BARE_F, BARE_PRE_F, BARE_IMP_F  )
!$OMP THREADPRIVATE    (      OUT_R1,   OUT_R2,     OUT_F1, OUT_F2,     BIUP,       BIDW,   BSFCMAX,    BSFCMAX_PRE )
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
                             * ( 1. + PGRV*DT*B2RIVMAN(ISEQ,1)**2.*abs(BOUT_PRE)*BFLW_IMP**(-7./3.) )**(-1.)
    B2RIVVEL(ISEQ,1) = B2RIVOUT(ISEQ,1) * BAREA**(-1.)
  ELSE
    B2RIVOUT(ISEQ,1) = 0._JPRB
    B2RIVVEL(ISEQ,1) = 0._JPRB
  ENDIF

!=== Floodplain Flow ===
  IF( LFLDOUT )THEN
    BFLW_F   = MAX( BSFCMAX-B2ELEVTN(ISEQ,1), 0._JPRB )
    BARE_F   = D2FLDSTO(ISEQ,1) * B2RIVLEN(ISEQ,1)**(-1.)
    BARE_F   = MAX( BARE_F - B2FLDDPH(ISEQ,1)*B2RIVWTH(ISEQ,1), 0._JPRB )   !! remove above river channel area
  
    BFLW_PRE_F = BSFCMAX_PRE - B2ELEVTN(ISEQ,1)
    BFLW_IMP_F = MAX( (MAX(BFLW_F*BFLW_PRE_F,0._JPRB))**0.5, 1.E-6_JPRB )
  
    BARE_PRE_F = B2FLDSTO_PRE(ISEQ,1) * B2RIVLEN(ISEQ,1)**(-1.)
    BARE_PRE_F = MAX( BARE_PRE_F - B2FLDDPH_PRE(ISEQ,1)*B2RIVWTH(ISEQ,1), 1.E-6_JPRB )   !! remove above river channel area
    BARE_IMP_F = max( (BARE_F*BARE_PRE_F)**0.5, 1.E-6_JPRB )
  
    IF( BFLW_IMP_F>1.E-5 .and. BARE_IMP_F>1.E-5 )THEN 
      BOUT_PRE_F = B2FLDOUT_PRE(ISEQ,1)
      B2FLDOUT(ISEQ,1) = ( BOUT_PRE_F + PGRV*DT*BARE_IMP_F*BSLOPE_F ) &
                        * (1. + PGRV*DT*PMANFLD**2. * abs(BOUT_PRE_F)*BFLW_IMP_F**(-4./3.)*BARE_IMP_F**(-1.) )**(-1._JPRB)
    ELSE
      B2FLDOUT(ISEQ,1) = 0._JPRB
    ENDIF
  
    IF( B2FLDOUT(ISEQ,1)*B2RIVOUT(ISEQ,1)<0._JPRB ) B2FLDOUT(ISEQ,1)=0._JPRB  !! stabilization
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
  OUT_F1 = max(  B2FLDOUT(ISEQ,1),0._JPRB )
  OUT_F2 = max( -B2FLDOUT(ISEQ,1),0._JPRB )
  BIUP=(OUT_R1+OUT_F1)*DT
  BIDW=(OUT_R2+OUT_F2)*DT
!$OMP ATOMIC
  B2STOOUT(ISEQ,1) = B2STOOUT(ISEQ,1) + BIUP 
!$OMP ATOMIC
  B2STOOUT(JSEQ,1) = B2STOOUT(JSEQ,1) + BIDW 
END DO
#ifndef NoAtom_CMF
!$OMP END PARALLEL DO
#endif


!$OMP PARALLEL DO                                                     !! for river mouth grids
DO ISEQ=NSEQRIV+1, NSEQALL
  IF ( LSLOPEMOUTH ) THEN
    ! prescribed slope 
    BSLOPE = B2ELEVSLOPE(ISEQ,1)
  ELSE
    BSLOPE = ( B2SFCELV(ISEQ,1) - B2DWNELV(ISEQ,1) ) * PDSTMTH ** (-1.)
  ENDIF
  BSLOPE_F = max( -0.005_JPRB, min( 0.005_JPRB,BSLOPE ))    !! set max&min [instead of using weir equation for efficiency]
!=== river mouth flow ===

  BFLW   = B2RIVDPH(ISEQ,1)
  BAREA  = B2RIVWTH(ISEQ,1) * BFLW

  BFLW_PRE=B2RIVDPH_PRE(ISEQ,1)
  BFLW_IMP=MAX( (BFLW*BFLW_PRE)**0.5, 1.E-6_JPRB )                                    !! semi implicit flow depth

  IF( BFLW_IMP>1.E-5 .and. BAREA>1.E-5 )THEN 
    BOUT_PRE = B2RIVOUT_PRE(ISEQ,1) * B2RIVWTH(ISEQ,1)**(-1.)
    B2RIVOUT(ISEQ,1) = B2RIVWTH(ISEQ,1) * ( BOUT_PRE + PGRV*DT*BFLW_IMP*BSLOPE ) &
                             * ( 1. + PGRV*DT*B2RIVMAN(ISEQ,1)**2. * abs(BOUT_PRE)*BFLW_IMP**(-7./3.) )**(-1.)
    B2RIVVEL(ISEQ,1) = B2RIVOUT(ISEQ,1) * BAREA**(-1._JPRB)
  ELSE
    B2RIVOUT(ISEQ,1) = 0._JPRB
    B2RIVVEL(ISEQ,1) = 0._JPRB
  ENDIF

!=== floodplain mouth flow ===
  IF( LFLDOUT )THEN
    BFLW_F   = B2SFCELV(ISEQ,1)-B2ELEVTN(ISEQ,1)
  
    BARE_F   = D2FLDSTO(ISEQ,1) * B2RIVLEN(ISEQ,1)**(-1.)
    BARE_F   = MAX( BARE_F - B2FLDDPH(ISEQ,1)*B2RIVWTH(ISEQ,1), 0._JPRB )   !! remove above river channel area
  
    BFLW_PRE_F = B2SFCELV_PRE(ISEQ,1)-B2ELEVTN(ISEQ,1)
    BFLW_IMP_F = MAX( (MAX(BFLW_F*BFLW_PRE_F,0._JPRB))**0.5, 1.E-6_JPRB )
  
    BARE_PRE_F = B2FLDSTO_PRE(ISEQ,1) * B2RIVLEN(ISEQ,1)**(-1.)
    BARE_PRE_F = MAX( BARE_PRE_F - B2FLDDPH_PRE(ISEQ,1)*B2RIVWTH(ISEQ,1), 1.E-6_JPRB )   !! remove above river channel area
    BARE_IMP_F = max( (BARE_F*BARE_PRE_F)**0.5, 1.E-6_JPRB )
  
    IF( BFLW_IMP_F>1.E-5 .and. BARE_IMP_F>1.E-5 )THEN 
      BOUT_PRE_F = B2FLDOUT_PRE(ISEQ,1)
      B2FLDOUT(ISEQ,1) = ( BOUT_PRE_F + PGRV*DT*BARE_IMP_F*BSLOPE_F ) &
                        * (1. + PGRV*DT*PMANFLD**2. * abs(BOUT_PRE_F)*BFLW_IMP_F**(-4./3.)*BARE_IMP_F**(-1.) )**(-1.)
    ELSE
      B2FLDOUT(ISEQ,1) = 0._JPRB
    ENDIF
  
    IF( B2FLDOUT(ISEQ,1)*B2RIVOUT(ISEQ,1)<0._JPRB ) B2FLDOUT(ISEQ,1)=0._JPRB  !! stabilization
  ENDIF
!=== check outflow ===
  OUT_R1 = max(  B2RIVOUT(ISEQ,1),0._JPRB )
  OUT_F1 = max(  B2FLDOUT(ISEQ,1),0._JPRB )
  B2STOOUT(ISEQ,1) = B2STOOUT(ISEQ,1) + OUT_R1*DT + OUT_F1*DT
END DO
!$OMP END PARALLEL DO



!! === ourflow modification to avoid negative storage ======

!$OMP PARALLEL DO                                                     !! outflow correcttion if total outflow > storage
DO ISEQ=1, NSEQALL
  IF ( B2STOOUT(ISEQ,1) > 1.E-8 ) THEN
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
    B2FLDOUT(ISEQ,1) = B2FLDOUT(ISEQ,1)*B2RATE(ISEQ,1)
  ELSE
    B2RIVOUT(ISEQ,1) = B2RIVOUT(ISEQ,1)*B2RATE(JSEQ,1)
    B2FLDOUT(ISEQ,1) = B2FLDOUT(ISEQ,1)*B2RATE(JSEQ,1)
  ENDIF
!$OMP ATOMIC
  B2RIVINF(JSEQ,1) = B2RIVINF(JSEQ,1) + B2RIVOUT(ISEQ,1)             !! total inflow to a grid (from upstream)
!$OMP ATOMIC
  B2FLDINF(JSEQ,1) = B2FLDINF(JSEQ,1) + B2FLDOUT(ISEQ,1)
END DO
#ifndef NoAtom_CMF
!$OMP END PARALLEL DO
#endif

!$OMP PARALLEL DO
DO ISEQ=NSEQRIV+1, NSEQALL ! for river mouth
  B2RIVOUT(ISEQ,1) = B2RIVOUT(ISEQ,1)*B2RATE(ISEQ,1)
  B2FLDOUT(ISEQ,1) = B2FLDOUT(ISEQ,1)*B2RATE(ISEQ,1)
END DO
!$OMP END PARALLEL DO

END SUBROUTINE CMF_CALC_OUTFLW
!####################################################################

END MODULE CMF_CALC_OUTFLW_MOD
