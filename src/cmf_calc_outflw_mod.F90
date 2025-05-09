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
USE PARKIND1,           ONLY: JPIM, JPRB, JPRD
USE YOS_CMF_INPUT,      ONLY: DT,       PDSTMTH,  PMANFLD,  PGRV,     LFLDOUT, LPTHOUT, LSLOPEMOUTH
USE YOS_CMF_MAP,        ONLY: I1NEXT,   NSEQALL,  NSEQRIV,  NSEQMAX
USE YOS_CMF_MAP,        ONLY: D2RIVELV, D2ELEVTN, D2NXTDST, D2RIVWTH, D2RIVHGT
USE YOS_CMF_MAP,        ONLY: D2RIVLEN, D2RIVMAN, D2DWNELV, D2ELEVSLOPE
USE YOS_CMF_MAP,        ONLY: I1UPST,   I1UPN,    I1P_OUT,  I1P_OUTN, I1P_INF, I1P_INFN
USE YOS_CMF_PROG,       ONLY: P2RIVSTO, D2RIVOUT, P2FLDSTO, D2FLDOUT
USE YOS_CMF_PROG,       ONLY: D2RIVOUT_PRE, D2RIVDPH_PRE, D2FLDOUT_PRE, D2FLDSTO_PRE
USE YOS_CMF_DIAG,       ONLY: D2RIVDPH, D2RIVVEL, D2RIVINF, D2FLDDPH, D2FLDINF, D2SFCELV, D2STORGE
!
CONTAINS
!####################################################################
! -- CMF_CALC_OUTFLW
! -- CMF_CALC_INFLOW
! --
!####################################################################
SUBROUTINE CMF_CALC_OUTFLW
IMPLICIT NONE
!*** Local
REAL(KIND=JPRB)            :: D2SFCELV_PRE(NSEQMAX,1)                  !! water surface elevation (t-1) [m]
REAL(KIND=JPRB)            :: D2FLDDPH_PRE(NSEQMAX,1)                      !! floodplain depth (t-1)        [m]
! save for OpenMP
INTEGER(KIND=JPIM),SAVE    :: ISEQ, JSEQ
REAL(KIND=JPRB),SAVE       :: DSLOPE,   DOUT_PRE,   DFLW,   DFLW_PRE,   DFLW_IMP,   DAREA 
REAL(KIND=JPRB),SAVE       :: DSLOPE_F, DOUT_PRE_F, DFLW_F, DFLW_PRE_F, DFLW_IMP_F, DARE_F, DARE_PRE_F, DARE_IMP_F
REAL(KIND=JPRB),SAVE       :: DSFCMAX,  DSFCMAX_PRE, RATE
!$OMP THREADPRIVATE    (JSEQ, DSLOPE,   DOUT_PRE,   DFLW,   DFLW_PRE,   DFLW_IMP,   DAREA  )
!$OMP THREADPRIVATE    (      DSLOPE_F, DOUT_PRE_F, DFLW_F, DFLW_PRE_F, DFLW_IMP_F, DARE_F, DARE_PRE_F, DARE_IMP_F  )
!$OMP THREADPRIVATE    (      DSFCMAX,  DSFCMAX_PRE, RATE )
!================================================

!$OMP PARALLEL DO
DO ISEQ=1, NSEQALL
  D2SFCELV(ISEQ,1)     = D2RIVELV(ISEQ,1) + D2RIVDPH(ISEQ,1)
  D2SFCELV_PRE(ISEQ,1) = D2RIVELV(ISEQ,1) + D2RIVDPH_PRE(ISEQ,1)
  D2FLDDPH_PRE(ISEQ,1) = MAX( D2RIVDPH_PRE(ISEQ,1)-D2RIVHGT(ISEQ,1), 0._JPRB )
END DO
!$OMP END PARALLEL DO

!$OMP PARALLEL DO
DO ISEQ=1, NSEQRIV                                                    !! for normal cells
  JSEQ=I1NEXT(ISEQ) ! next cell's pixel
  
  DSFCMAX    =MAX( D2SFCELV(ISEQ,1),    D2SFCELV(JSEQ,1) )
  DSFCMAX_PRE=MAX( D2SFCELV_PRE(ISEQ,1),D2SFCELV_PRE(JSEQ,1) )
  DSLOPE = ( D2SFCELV(ISEQ,1)-D2SFCELV(JSEQ,1) ) * D2NXTDST(ISEQ,1)**(-1.)
  DSLOPE_F = MAX( -0.005_JPRB, min( 0.005_JPRB,DSLOPE ))    !! set max&min [instead of using weir equation for efficiency]

!=== River Flow ===
  DFLW   = DSFCMAX - D2RIVELV(ISEQ,1)        !!  flow cross-section depth
  DAREA  = D2RIVWTH(ISEQ,1) * DFLW                                            !!  flow cross-section area

  DFLW_PRE=DSFCMAX_PRE - D2RIVELV(ISEQ,1)
  DFLW_IMP=MAX( (DFLW*DFLW_PRE)**0.5 ,1.E-6_JPRB )                                            !! semi implicit flow depth

  IF( DFLW_IMP>1.E-5 .and. DAREA>1.E-5 )THEN 
    DOUT_PRE= D2RIVOUT_PRE(ISEQ,1) * D2RIVWTH(ISEQ,1)**(-1.)                         !! outflow (t-1) [m2/s] (unit width)
    D2RIVOUT(ISEQ,1) = D2RIVWTH(ISEQ,1) * ( DOUT_PRE + PGRV*DT*DFLW_IMP*DSLOPE ) &
                             * ( 1. + PGRV*DT*D2RIVMAN(ISEQ,1)**2.*abs(DOUT_PRE)*DFLW_IMP**(-7./3.) )**(-1.)
    D2RIVVEL(ISEQ,1) = D2RIVOUT(ISEQ,1) * DAREA**(-1.)
  ELSE
    D2RIVOUT(ISEQ,1) = 0._JPRB
    D2RIVVEL(ISEQ,1) = 0._JPRB
  ENDIF

!=== Floodplain Flow ===
  IF( LFLDOUT )THEN
    DFLW_F   = MAX( DSFCMAX-D2ELEVTN(ISEQ,1), 0._JPRB )
    DARE_F   = P2FLDSTO(ISEQ,1) * D2RIVLEN(ISEQ,1)**(-1.)
    DARE_F   = MAX( DARE_F - D2FLDDPH(ISEQ,1)*D2RIVWTH(ISEQ,1), 0._JPRB )   !! remove above river channel area
  
    DFLW_PRE_F = DSFCMAX_PRE - D2ELEVTN(ISEQ,1)
    DFLW_IMP_F = MAX( (MAX(DFLW_F*DFLW_PRE_F,0._JPRB))**0.5, 1.E-6_JPRB )
  
    DARE_PRE_F = D2FLDSTO_PRE(ISEQ,1) * D2RIVLEN(ISEQ,1)**(-1.)
    DARE_PRE_F = MAX( DARE_PRE_F - D2FLDDPH_PRE(ISEQ,1)*D2RIVWTH(ISEQ,1), 1.E-6_JPRB )   !! remove above river channel area
    DARE_IMP_F = max( (DARE_F*DARE_PRE_F)**0.5, 1.E-6_JPRB )
  
    IF( DFLW_IMP_F>1.E-5 .and. DARE_IMP_F>1.E-5 )THEN 
      DOUT_PRE_F = D2FLDOUT_PRE(ISEQ,1)
      D2FLDOUT(ISEQ,1) = ( DOUT_PRE_F + PGRV*DT*DARE_IMP_F*DSLOPE_F ) &
                        * (1. + PGRV*DT*PMANFLD**2. * abs(DOUT_PRE_F)*DFLW_IMP_F**(-4./3.)*DARE_IMP_F**(-1.) )**(-1._JPRB)
    ELSE
      D2FLDOUT(ISEQ,1) = 0._JPRB
    ENDIF
  
    IF( D2FLDOUT(ISEQ,1)*D2RIVOUT(ISEQ,1)<0._JPRB ) D2FLDOUT(ISEQ,1)=0._JPRB  !! stabilization
  ENDIF

!! Storage change limitter to prevent sudden increase of "upstream" water level when backwardd flow (v423)
  IF( D2RIVOUT(ISEQ,1)<0._JPRB )THEN
    RATE = 0.05 * D2STORGE(ISEQ,1) / ( (-D2RIVOUT(ISEQ,1)-D2FLDOUT(ISEQ,1))*DT )
    RATE= min(RATE, 1.0_JPRB )
    D2RIVOUT(ISEQ,1)=D2RIVOUT(ISEQ,1)*RATE
    D2FLDOUT(ISEQ,1)=D2FLDOUT(ISEQ,1)*RATE
  ENDIF
END DO
!$OMP END PARALLEL DO

!$OMP PARALLEL DO                                                     !! for river mouth grids
DO ISEQ=NSEQRIV+1, NSEQALL
  IF ( LSLOPEMOUTH ) THEN
    ! prescribed slope 
    DSLOPE = D2ELEVSLOPE(ISEQ,1)
  ELSE
    DSLOPE = ( D2SFCELV(ISEQ,1) - D2DWNELV(ISEQ,1) ) * PDSTMTH ** (-1.)
  ENDIF
  DSLOPE_F = max( -0.005_JPRB, min( 0.005_JPRB,DSLOPE ))    !! set max&min [instead of using weir equation for efficiency]
!=== river mouth flow ===

  DFLW   = D2RIVDPH(ISEQ,1)
  DAREA  = D2RIVWTH(ISEQ,1) * DFLW

  DFLW_PRE=D2RIVDPH_PRE(ISEQ,1)
  DFLW_IMP=MAX( (DFLW*DFLW_PRE)**0.5, 1.E-6_JPRB )                                    !! semi implicit flow depth

  IF( DFLW_IMP>1.E-5 .and. DAREA>1.E-5 )THEN 
    DOUT_PRE = D2RIVOUT_PRE(ISEQ,1) * D2RIVWTH(ISEQ,1)**(-1.)
    D2RIVOUT(ISEQ,1) = D2RIVWTH(ISEQ,1) * ( DOUT_PRE + PGRV*DT*DFLW_IMP*DSLOPE ) &
                             * ( 1. + PGRV*DT*D2RIVMAN(ISEQ,1)**2. * abs(DOUT_PRE)*DFLW_IMP**(-7./3.) )**(-1.)
    D2RIVVEL(ISEQ,1) = D2RIVOUT(ISEQ,1) * DAREA**(-1._JPRB)
  ELSE
    D2RIVOUT(ISEQ,1) = 0._JPRB
    D2RIVVEL(ISEQ,1) = 0._JPRB
  ENDIF

!=== floodplain mouth flow ===
  IF( LFLDOUT )THEN
    DFLW_F   = D2SFCELV(ISEQ,1)-D2ELEVTN(ISEQ,1)
  
    DARE_F   = P2FLDSTO(ISEQ,1) * D2RIVLEN(ISEQ,1)**(-1.)
    DARE_F   = MAX( DARE_F - D2FLDDPH(ISEQ,1)*D2RIVWTH(ISEQ,1), 0._JPRB )   !! remove above river channel area
  
    DFLW_PRE_F = D2SFCELV_PRE(ISEQ,1)-D2ELEVTN(ISEQ,1)
    DFLW_IMP_F = MAX( (MAX(DFLW_F*DFLW_PRE_F,0._JPRB))**0.5, 1.E-6_JPRB )
  
    DARE_PRE_F = D2FLDSTO_PRE(ISEQ,1) * D2RIVLEN(ISEQ,1)**(-1.)
    DARE_PRE_F = MAX( DARE_PRE_F - D2FLDDPH_PRE(ISEQ,1)*D2RIVWTH(ISEQ,1), 1.E-6_JPRB )   !! remove above river channel area
    DARE_IMP_F = max( (DARE_F*DARE_PRE_F)**0.5, 1.E-6_JPRB )
  
    IF( DFLW_IMP_F>1.E-5 .and. DARE_IMP_F>1.E-5 )THEN 
      DOUT_PRE_F = D2FLDOUT_PRE(ISEQ,1)
      D2FLDOUT(ISEQ,1) = ( DOUT_PRE_F + PGRV*DT*DARE_IMP_F*DSLOPE_F ) &
                        * (1. + PGRV*DT*PMANFLD**2. * abs(DOUT_PRE_F)*DFLW_IMP_F**(-4./3.)*DARE_IMP_F**(-1.) )**(-1.)
    ELSE
      D2FLDOUT(ISEQ,1) = 0._JPRB
    ENDIF
  
    IF( D2FLDOUT(ISEQ,1)*D2RIVOUT(ISEQ,1)<0._JPRB ) D2FLDOUT(ISEQ,1)=0._JPRB  !! stabilization
  ENDIF
END DO
!$OMP END PARALLEL DO

END SUBROUTINE CMF_CALC_OUTFLW
!####################################################################
!+
!+
!+
!####################################################################
SUBROUTINE CMF_CALC_INFLOW
USE PARKIND1,           ONLY: JPIM, JPRB, JPRD
USE YOS_CMF_MAP,        ONLY: NSEQMAX,  NPTHOUT, NPTHLEV, I2MASK, PTH_UPST, PTH_DOWN
USE YOS_CMF_PROG,       ONLY: D1PTHFLW
USE YOS_CMF_DIAG,       ONLY: D2PTHOUT, D1PTHFLWSUM
IMPLICIT NONE
!*** Local
REAL(KIND=JPRD)            :: P2STOOUT(NSEQMAX,1)                      !! total outflow from a grid     [m3]
REAL(KIND=JPRD)            :: P2RIVINF(NSEQMAX,1)                      !! 
REAL(KIND=JPRD)            :: P2FLDINF(NSEQMAX,1)                      !! 

REAL(KIND=JPRD)            :: P2PTHOUT(NSEQMAX,1)                  !! for water conservation

REAL(KIND=JPRB)            :: D2RATE(NSEQMAX,1)                        !! outflow correction
! SAVE for OpenMP
INTEGER(KIND=JPIM),SAVE    :: ISEQ, JSEQ, IPTH, ILEV
REAL(KIND=JPRB),SAVE       :: OUT_R1, OUT_R2, OUT_F1, OUT_F2, DIUP, DIDW, ISEQP, JSEQP
!$OMP THREADPRIVATE     (JSEQ,OUT_R1, OUT_R2, OUT_F1, OUT_F2, DIUP, DIDW, ISEQP, JSEQP,ILEV)
!================================================
  
!*** 1. initialize & calculate P2STOOUT for normal cells

!$OMP PARALLEL DO
DO ISEQ=1, NSEQALL
  P2RIVINF(ISEQ,1) = 0._JPRD
  P2FLDINF(ISEQ,1) = 0._JPRD
  P2PTHOUT(ISEQ,1) = 0._JPRD
  P2STOOUT(ISEQ,1) = 0._JPRD
  D2RATE(ISEQ,1) = 1._JPRB
END DO
!$OMP END PARALLEL DO

!! for normal cells ---------
#ifndef NoAtom_CMF
!$OMP PARALLEL DO
#endif
DO ISEQ=1, NSEQRIV                                                    !! for normalcells
  JSEQ=I1NEXT(ISEQ) ! next cell's pixel
  OUT_R1 = max(  D2RIVOUT(ISEQ,1),0._JPRB )
  OUT_R2 = max( -D2RIVOUT(ISEQ,1),0._JPRB )
  OUT_F1 = max(  D2FLDOUT(ISEQ,1),0._JPRB )
  OUT_F2 = max( -D2FLDOUT(ISEQ,1),0._JPRB )
  DIUP=(OUT_R1+OUT_F1)*DT
  DIDW=(OUT_R2+OUT_F2)*DT
#ifndef NoAtom_CMF
!$OMP ATOMIC
#endif
  P2STOOUT(ISEQ,1) = P2STOOUT(ISEQ,1) + DIUP 
#ifndef NoAtom_CMF
!$OMP ATOMIC
#endif
  P2STOOUT(JSEQ,1) = P2STOOUT(JSEQ,1) + DIDW 
END DO
#ifndef NoAtom_CMF
!$OMP END PARALLEL DO
#endif

!! for river mouth grids ------------
!$OMP PARALLEL DO
DO ISEQ=NSEQRIV+1, NSEQALL
  OUT_R1 = max( D2RIVOUT(ISEQ,1), 0._JPRB )
  OUT_F1 = max( D2FLDOUT(ISEQ,1), 0._JPRB )
  P2STOOUT(ISEQ,1) = P2STOOUT(ISEQ,1) + OUT_R1*DT + OUT_F1*DT
END DO
!$OMP END PARALLEL DO

!! for bifurcation channels ------------
IF( LPTHOUT )THEN
#ifndef NoAtom_CMF
!$OMP PARALLEL DO  !! No OMP Atomic for bit-identical simulation (set in Mkinclude)
#endif
  DO IPTH=1, NPTHOUT  
    ISEQP=PTH_UPST(IPTH)
    JSEQP=PTH_DOWN(IPTH)
    !! Avoid calculation outside of domain
    IF (ISEQP<=0 .OR. JSEQP<=0 ) CYCLE
    IF (I2MASK(ISEQP,1)>0 .OR. I2MASK(JSEQP,1)>0 ) CYCLE  !! I2MASK is for 1: kinemacit 2: dam  no bifurcation
  
    OUT_R1 = max(  D1PTHFLWSUM(IPTH),0._JPRB )
    OUT_R2 = max( -D1PTHFLWSUM(IPTH),0._JPRB )
    DIUP=(OUT_R1)*DT
    DIDW=(OUT_R2)*DT
#ifndef NoAtom_CMF
!$OMP ATOMIC
#endif
    P2STOOUT(ISEQP,1) = P2STOOUT(ISEQP,1) + DIUP
#ifndef NoAtom_CMF
!$OMP ATOMIC
#endif
    P2STOOUT(JSEQP,1) = P2STOOUT(JSEQP,1) + DIDW
  END DO
#ifndef NoAtom_CMF
!$OMP END PARALLEL DO  !! No OMP Atomic for bit-identical simulation (set in Mkinclude)
#endif
ENDIF

!============================
!*** 2. modify outflow

!$OMP PARALLEL DO
DO ISEQ=1, NSEQALL
  IF ( P2STOOUT(ISEQ,1) > 1.E-8 ) THEN
    D2RATE(ISEQ,1) = min( (P2RIVSTO(ISEQ,1)+P2FLDSTO(ISEQ,1)) * P2STOOUT(ISEQ,1)**(-1.), 1._JPRD )
  ENDIF
END DO
!$OMP END PARALLEL DO

!! normal pixels------
#ifndef NoAtom_CMF
!$OMP PARALLEL DO  !! No OMP Atomic for bit-identical simulation (set in Mkinclude)
#endif
DO ISEQ=1, NSEQRIV ! for normal pixels
  JSEQ=I1NEXT(ISEQ)
  IF( D2RIVOUT(ISEQ,1) >= 0._JPRB )THEN
    D2RIVOUT(ISEQ,1) = D2RIVOUT(ISEQ,1)*D2RATE(ISEQ,1)
    D2FLDOUT(ISEQ,1) = D2FLDOUT(ISEQ,1)*D2RATE(ISEQ,1)
  ELSE
    D2RIVOUT(ISEQ,1) = D2RIVOUT(ISEQ,1)*D2RATE(JSEQ,1)
    D2FLDOUT(ISEQ,1) = D2FLDOUT(ISEQ,1)*D2RATE(JSEQ,1)
  ENDIF
#ifndef NoAtom_CMF
!$OMP ATOMIC
#endif
  P2RIVINF(JSEQ,1) = P2RIVINF(JSEQ,1) + D2RIVOUT(ISEQ,1)             !! total inflow to a grid (from upstream)
#ifndef NoAtom_CMF
!$OMP ATOMIC
#endif
  P2FLDINF(JSEQ,1) = P2FLDINF(JSEQ,1) + D2FLDOUT(ISEQ,1)
END DO
#ifndef NoAtom_CMF
!$OMP END PARALLEL DO
#endif

!! river mouth-----------------
!$OMP PARALLEL DO
DO ISEQ=NSEQRIV+1, NSEQALL
  D2RIVOUT(ISEQ,1) = D2RIVOUT(ISEQ,1)*D2RATE(ISEQ,1)
  D2FLDOUT(ISEQ,1) = D2FLDOUT(ISEQ,1)*D2RATE(ISEQ,1)
END DO
!$OMP END PARALLEL DO

!! bifurcation channels --------
IF( LPTHOUT )THEN
#ifndef NoAtom_CMF
!$OMP PARALLEL DO  !! No OMP Atomic for bit-identical simulation (set in Mkinclude)
#endif
  DO IPTH=1, NPTHOUT  
    ISEQP=PTH_UPST(IPTH)
    JSEQP=PTH_DOWN(IPTH)
    !! Avoid calculation outside of domain
    IF (ISEQP<=0 .OR. JSEQP<=0 ) CYCLE
    IF (I2MASK(ISEQP,1)>0 .OR. I2MASK(JSEQP,1)>0 ) CYCLE  !! I2MASK is for 1: kinemacit 2: dam  no bifurcation
  
    DO ILEV=1, NPTHLEV
      IF( D1PTHFLW(IPTH,ILEV) >= 0._JPRB )THEN                                  !! total outflow from each grid
        D1PTHFLW(IPTH,ILEV) = D1PTHFLW(IPTH,ILEV)*D2RATE(ISEQP,1)
      ELSE
        D1PTHFLW(IPTH,ILEV) = D1PTHFLW(IPTH,ILEV)*D2RATE(JSEQP,1)
      ENDIF
    END DO

    IF( D1PTHFLWSUM(IPTH) >= 0._JPRB )THEN                                  !! total outflow from each grid
      D1PTHFLWSUM(IPTH) = D1PTHFLWSUM(IPTH)*D2RATE(ISEQP,1)
    ELSE
      D1PTHFLWSUM(IPTH) = D1PTHFLWSUM(IPTH)*D2RATE(JSEQP,1)
    ENDIF    

#ifndef NoAtom_CMF
!$OMP ATOMIC
#endif
    P2PTHOUT(ISEQP,1) = P2PTHOUT(ISEQP,1) + D1PTHFLWSUM(IPTH)
#ifndef NoAtom_CMF
!$OMP ATOMIC
#endif
    P2PTHOUT(JSEQP,1) = P2PTHOUT(JSEQP,1) - D1PTHFLWSUM(IPTH)

  END DO
#ifndef NoAtom_CMF
!$OMP END PARALLEL DO  !! No OMP Atomic for bit-identical simulation (set in Mkinclude)
#endif
ENDIF

D2RIVINF(:,:)=P2RIVINF(:,:)  !! needed for SinglePrecisionMode
D2FLDINF(:,:)=P2FLDINF(:,:)
D2PTHOUT(:,:)=P2PTHOUT(:,:)

END SUBROUTINE CMF_CALC_INFLOW
!####################################################################
!+
!+
!+
!####################################################################
SUBROUTINE CMF_CALC_INFLOW_LSMAPAT
USE PARKIND1,           ONLY: JPIM, JPRB, JPRD
USE YOS_CMF_MAP,        ONLY: NSEQMAX,  NPTHOUT, NPTHLEV, I2MASK, PTH_UPST, PTH_DOWN
USE YOS_CMF_PROG,       ONLY: D1PTHFLW
USE YOS_CMF_DIAG,       ONLY: D2PTHOUT, D1PTHFLWSUM
IMPLICIT NONE
!*** Local
REAL(KIND=JPRD)            :: P2STOOUT(NSEQMAX,1)                      !! total outflow from a grid     [m3]
REAL(KIND=JPRD)            :: P2RIVINF(NSEQMAX,1)                      !! 
REAL(KIND=JPRD)            :: P2FLDINF(NSEQMAX,1)                      !! 

REAL(KIND=JPRD)            :: P2PTHOUT(NSEQMAX,1)                  !! for water conservation

REAL(KIND=JPRB)            :: D2RATE(NSEQMAX,1)                        !! outflow correction
! SAVE for OpenMP
INTEGER(KIND=JPIM),SAVE    :: ISEQ, JSEQ, IPTH, ILEV, INUM, JPTH
REAL(KIND=JPRB),SAVE       :: OUT_R1, OUT_R2, OUT_F1, OUT_F2, DIUP, DIDW, ISEQP, JSEQP
!$OMP THREADPRIVATE     (JSEQ,OUT_R1, OUT_R2, OUT_F1, OUT_F2, DIUP, DIDW, ISEQP, JSEQP, ILEV, INUM, JPTH)
!================================================
  
!*** 1. initialize & calculate P2STOOUT for normal cells

!$OMP PARALLEL DO
DO ISEQ=1, NSEQALL
  P2RIVINF(ISEQ,1) = 0._JPRD
  P2FLDINF(ISEQ,1) = 0._JPRD
  P2PTHOUT(ISEQ,1) = 0._JPRD
  P2STOOUT(ISEQ,1) = 0._JPRD
  D2RATE(ISEQ,1) = 1._JPRB
END DO
!$OMP END PARALLEL DO

!! for normal cells ---------
!$OMP PARALLEL DO
DO ISEQ=1, NSEQALL
  P2STOOUT(ISEQ,1) = max( D2RIVOUT(ISEQ,1),0._JPRB ) + max( D2FLDOUT(ISEQ,1),0._JPRB )
  IF( I1UPN(ISEQ)>0 )THEN
    DO INUM=1, I1UPN(ISEQ)
      JSEQ=I1UPST(ISEQ,INUM)
      P2STOOUT(ISEQ,1) = P2STOOUT(ISEQ,1) + max( -D2RIVOUT(JSEQ,1),0._JPRB ) + max( -D2FLDOUT(JSEQ,1),0._JPRB )
    END DO
  ENDIF
  P2STOOUT(ISEQ,1) = P2STOOUT(ISEQ,1) *DT
END DO
!$OMP END PARALLEL DO

!! for bifurcation channels ------------
IF( LPTHOUT )THEN
!$OMP PARALLEL DO  !! No OMP Atomic for bit-identical simulation (set in Mkinclude)
  DO ISEQ=1, NSEQALL
    IF( I1P_OUTN(ISEQ)>0 )THEN
      DO INUM=1, I1P_OUTN(ISEQ)
        JPTH=I1P_OUT(ISEQ,INUM)
        P2STOOUT(ISEQ,1) = P2STOOUT(ISEQ,1) + max(  D1PTHFLWSUM(JPTH),0._JPRB )*DT
      END DO
    ENDIF

    IF( I1P_INFN(ISEQ)>0 )THEN
      DO INUM=1, I1P_INFN(ISEQ)
        JPTH=I1P_INF(ISEQ,INUM)
        P2STOOUT(ISEQ,1) = P2STOOUT(ISEQ,1) + max( -D1PTHFLWSUM(JPTH),0._JPRB )*DT
      END DO
    ENDIF
  END DO
!$OMP END PARALLEL DO
ENDIF

!============================
!*** 2. modify outflow

!$OMP PARALLEL DO
DO ISEQ=1, NSEQALL
  IF ( P2STOOUT(ISEQ,1) > 1.E-8 ) THEN
    D2RATE(ISEQ,1) = min( (P2RIVSTO(ISEQ,1)+P2FLDSTO(ISEQ,1)) * P2STOOUT(ISEQ,1)**(-1.), 1._JPRD )
  ENDIF
END DO
!$OMP END PARALLEL DO

!! normal pixels------
!$OMP PARALLEL DO  !! No OMP Atomic for bit-identical simulation (set in Mkinclude)
DO ISEQ=1, NSEQRIV ! for normal pixels
  JSEQ=I1NEXT(ISEQ)
  IF( D2RIVOUT(ISEQ,1) >= 0._JPRB )THEN
    D2RIVOUT(ISEQ,1) = D2RIVOUT(ISEQ,1)*D2RATE(ISEQ,1)
    D2FLDOUT(ISEQ,1) = D2FLDOUT(ISEQ,1)*D2RATE(ISEQ,1)
  ELSE
    D2RIVOUT(ISEQ,1) = D2RIVOUT(ISEQ,1)*D2RATE(JSEQ,1)
    D2FLDOUT(ISEQ,1) = D2FLDOUT(ISEQ,1)*D2RATE(JSEQ,1)
  ENDIF
END DO
!$OMP END PARALLEL DO

!! river mouth-----------------
!$OMP PARALLEL DO
DO ISEQ=NSEQRIV+1, NSEQALL
  D2RIVOUT(ISEQ,1) = D2RIVOUT(ISEQ,1)*D2RATE(ISEQ,1)
  D2FLDOUT(ISEQ,1) = D2FLDOUT(ISEQ,1)*D2RATE(ISEQ,1)
END DO
!$OMP END PARALLEL DO


!$OMP PARALLEL DO 
DO ISEQ=1, NSEQALL ! for normal pixels
  IF( I1UPN(ISEQ)>0 )THEN
    DO INUM=1, I1UPN(ISEQ)
      JSEQ=I1UPST(ISEQ,INUM)
      P2RIVINF(ISEQ,1) = P2RIVINF(ISEQ,1) + D2RIVOUT(JSEQ,1)
      P2FLDINF(ISEQ,1) = P2FLDINF(ISEQ,1) + D2FLDOUT(JSEQ,1)
    END DO
  ENDIF
END DO
!$OMP END PARALLEL DO


!! bifurcation channels --------
IF( LPTHOUT )THEN
!$OMP PARALLEL DO  
  DO IPTH=1, NPTHOUT  
    ISEQP=PTH_UPST(IPTH)
    JSEQP=PTH_DOWN(IPTH)
    !! Avoid calculation outside of domain
    IF (ISEQP<=0 .OR. JSEQP<=0 ) CYCLE
    IF (I2MASK(ISEQP,1)>0 .OR. I2MASK(JSEQP,1)>0 ) CYCLE  !! I2MASK is for 1: kinemacit 2: dam  no bifurcation
  
    DO ILEV=1, NPTHLEV
      IF( D1PTHFLW(IPTH,ILEV) >= 0._JPRB )THEN                                  !! total outflow from each grid
        D1PTHFLW(IPTH,ILEV) = D1PTHFLW(IPTH,ILEV)*D2RATE(ISEQP,1)
      ELSE
        D1PTHFLW(IPTH,ILEV) = D1PTHFLW(IPTH,ILEV)*D2RATE(JSEQP,1)
      ENDIF
    END DO

    IF( D1PTHFLWSUM(IPTH) >= 0._JPRB )THEN                                  !! total outflow from each grid
      D1PTHFLWSUM(IPTH) = D1PTHFLWSUM(IPTH)*D2RATE(ISEQP,1)
    ELSE
      D1PTHFLWSUM(IPTH) = D1PTHFLWSUM(IPTH)*D2RATE(JSEQP,1)
    ENDIF    
  END DO
!$OMP END PARALLEL DO  

!$OMP PARALLEL DO  
  DO ISEQ=1, NSEQALL
    IF( I1P_OUTN(ISEQ)>0 )THEN
      DO INUM=1, I1P_OUTN(ISEQ)
        JPTH=I1P_OUT(ISEQ,INUM)
        P2PTHOUT(ISEQ,1) = P2PTHOUT(ISEQ,1) + D1PTHFLWSUM(JPTH)
      END DO
    ENDIF

    IF( I1P_INFN(ISEQ)>0 )THEN
      DO INUM=1, I1P_INFN(ISEQ)
        JPTH=I1P_INF(ISEQ,INUM)
        P2PTHOUT(ISEQ,1) = P2PTHOUT(ISEQ,1) - D1PTHFLWSUM(JPTH)
      END DO
    ENDIF
  END DO
!$OMP END PARALLEL DO  
ENDIF

D2RIVINF(:,:)=P2RIVINF(:,:)  !! needed for SinglePrecisionMode
D2FLDINF(:,:)=P2FLDINF(:,:)
D2PTHOUT(:,:)=P2PTHOUT(:,:)

END SUBROUTINE CMF_CALC_INFLOW_LSMAPAT
!####################################################################
END MODULE CMF_CALC_OUTFLW_MOD
