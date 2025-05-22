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
USE YOS_CMF_MAP,        ONLY: I1NEXT,   NSEQALL,  NSEQRIV
USE YOS_CMF_MAP,        ONLY: D2RIVELV, D2ELEVTN, D2NXTDST, D2RIVWTH, D2RIVHGT
USE YOS_CMF_MAP,        ONLY: D2RIVLEN, D2RIVMAN, D2DWNELV, D2ELEVSLOPE
USE YOS_CMF_MAP,        ONLY: I1UPST,   I1UPN,    I1P_OUT,  I1P_OUTN, I1P_INF, I1P_INFN
USE YOS_CMF_PROG,       ONLY: P2RIVSTO, D2RIVOUT, P2FLDSTO, D2FLDOUT
USE YOS_CMF_PROG,       ONLY: D2RIVOUT_PRE, D2RIVDPH_PRE, D2FLDOUT_PRE, D2FLDSTO_PRE
USE YOS_CMF_DIAG,       ONLY: D2RIVDPH, D2RIVVEL, D2RIVINF, D2FLDDPH, D2FLDINF, D2SFCELV, D2STORGE
USE YOS_CMF_DIAG,       ONLY: D2SFCELV_PRE, D2DWNELV_PRE, D2FLDDPH_PRE
!
CONTAINS
!####################################################################
! -- CMF_CALC_OUTFLW
! -- CMF_CALC_INFLOW
! --
!####################################################################
SUBROUTINE CMF_CALC_OUTFLW
IMPLICIT NONE
! save for OpenMP
INTEGER(KIND=JPIM),SAVE    :: ISEQ, JSEQ
REAL(KIND=JPRB),SAVE       :: DFSTO,DSFC,DSFC_pr,DSLP,DFLW,DFLW_pr,DFLW_im,DARE,DARE_pr,DARE_im,DOUT_pr,DOUT,DVEL
LOGICAL,SAVE               :: Mask
REAL(KIND=JPRB),SAVE       :: RATE
!$OMP THREADPRIVATE          (JSEQ)
!================================================

! calculate water surface elevation
!$OMP PARALLEL DO SIMD
DO ISEQ=1, NSEQALL
  D2SFCELV(ISEQ,1)     = D2RIVELV(ISEQ,1) + D2RIVDPH(ISEQ,1)
  D2SFCELV_PRE(ISEQ,1) = D2RIVELV(ISEQ,1) + D2RIVDPH_PRE(ISEQ,1)
  D2FLDDPH_PRE(ISEQ,1) = MAX( D2RIVDPH_PRE(ISEQ,1)-D2RIVHGT(ISEQ,1), 0._JPRB )
END DO
!$OMP END PARALLEL DO SIMD

!Update downstream elevation
!$OMP PARALLEL DO
DO ISEQ=1, NSEQRIV
  JSEQ=I1NEXT(ISEQ) ! next cell's pixel
  D2DWNELV(ISEQ,1)     = D2SFCELV(JSEQ,1)
  D2DWNELV_PRE(ISEQ,1) = D2SFCELV_PRE(JSEQ,1)
END DO
!$OMP END PARALLEL DO

!$OMP PARALLEL DO SIMD PRIVATE(DSFC,DSFC_pr,DSLP,DFLW,DFLW_pr,DFLW_im,DARE,DARE_pr,DARE_im,DOUT_pr,DOUT,DVEL,Mask)
DO ISEQ=1, NSEQRIV                                                !! for normal cells
  DSFC = MAX( D2SFCELV(ISEQ,1),    D2DWNELV(ISEQ,1) )
  DSLP = ( D2SFCELV(ISEQ,1)-D2DWNELV(ISEQ,1) ) * D2NXTDST(ISEQ,1)**(-1._JPRB)
!=== River Flow ===
  DFLW = DSFC - D2RIVELV(ISEQ,1)                             !!  flow cross-section depth
  DARE = MAX( D2RIVWTH(ISEQ,1)*DFLW, 10._JPRB**-10 )           !!  flow cross-section area

  DSFC_pr=MAX( D2SFCELV_PRE(ISEQ,1),D2DWNELV_PRE(ISEQ,1) )
  DFLW_pr=DSFC_pr - D2RIVELV(ISEQ,1)
  DFLW_im=MAX( (DFLW*DFLW_pr)**0.5_JPRB ,1.E-6_JPRB )             !! semi implicit flow depth

  DOUT_pr= D2RIVOUT_PRE(ISEQ,1) * D2RIVWTH(ISEQ,1)**(-1._JPRB)    !! outflow (t-1) [m2/s] (unit width)
  DOUT=D2RIVWTH(ISEQ,1) * ( DOUT_pr + PGRV*DT*DFLW_im*DSLP ) &
                           * ( 1._JPRB + PGRV*DT*D2RIVMAN(ISEQ,1)**2._JPRB*abs(DOUT_pr)*DFLW_im**(-7._JPRB/3._JPRB) )**(-1._JPRB)
  DVEL= D2RIVOUT(ISEQ,1) * DARE**(-1._JPRB)

  Mask=(DFLW_im>1.E-5 .and. DARE>1.E-5)   !! replace small depth location with zero
  D2RIVOUT(ISEQ,1) = merge( DOUT, 0._JPRB, Mask)
  D2RIVVEL(ISEQ,1) = merge( DVEL, 0._JPRB, Mask)
END DO
!$OMP END PARALLEL DO SIMD

!=== Floodplain Flow ===
IF( LFLDOUT )THEN
  !$OMP PARALLEL DO SIMD PRIVATE(DFSTO,DSFC,DSFC_pr,DSLP,DFLW,DFLW_pr,DFLW_im,DARE,DARE_pr,DARE_im,DOUT_pr,DOUT,Mask)
  DO ISEQ=1, NSEQRIV      
    DFSTO=REAL( P2FLDSTO(ISEQ,1),KIND=JPRB)
    DSFC   = MAX( D2SFCELV(ISEQ,1),    D2DWNELV(ISEQ,1) )
    DSLP   = ( D2SFCELV(ISEQ,1)-D2DWNELV(ISEQ,1) ) * D2NXTDST(ISEQ,1)**(-1._JPRB)
    DSLP   = MAX( -0.005_JPRB, min( 0.005_JPRB, DSLP ))    !! set max&min [instead of using weir equation for efficiency]

    DFLW   = MAX( DSFC-D2ELEVTN(ISEQ,1), 0._JPRB )
    DARE   = DFSTO * D2RIVLEN(ISEQ,1)**(-1._JPRB)
    DARE   = MAX( DARE - D2FLDDPH(ISEQ,1)*D2RIVWTH(ISEQ,1), 0._JPRB )   !! remove above river channel area
  
    DSFC_pr = MAX( D2SFCELV_PRE(ISEQ,1),D2DWNELV_PRE(ISEQ,1) )
    DFLW_pr = DSFC_pr - D2ELEVTN(ISEQ,1)
    DFLW_im = MAX( (MAX(DFLW*DFLW_pr,0._JPRB))**0.5_JPRB, 1.E-6_JPRB )
  
    DARE_pr = D2FLDSTO_PRE(ISEQ,1) * D2RIVLEN(ISEQ,1)**(-1._JPRB)
    DARE_pr = MAX( DARE_pr - D2FLDDPH_PRE(ISEQ,1)*D2RIVWTH(ISEQ,1), 1.E-6_JPRB )   !! remove above river channel area
    DARE_im = MAX( (DARE*DARE_pr)**0.5_JPRB, 1.E-6_JPRB )

    DOUT_pr = D2FLDOUT_PRE(ISEQ,1)
    DOUT    = ( DOUT_pr + PGRV*DT*DARE_im*DSLP ) &
                      * (1._JPRB + PGRV*DT*PMANFLD**2._JPRB * abs(DOUT_pr)*DFLW_im**(-4._JPRB/3._JPRB)*DARE_im**(-1._JPRB) )**(-1._JPRB)

    Mask=(DFLW_im>1.E-5_JPRB .and. DARE>1.E-5_JPRB)  !! replace small depth location with zero
    D2FLDOUT(ISEQ,1) = merge( DOUT, 0._JPRB, Mask)

    DOUT=D2FLDOUT(ISEQ,1)
    Mask=( D2FLDOUT(ISEQ,1)*D2RIVOUT(ISEQ,1)>0._JPRB ) !! river and floodplain different direction
    D2FLDOUT(ISEQ,1) = merge( DOUT, 0._JPRB, Mask)
  END DO
  !$OMP END PARALLEL DO SIMD
ENDIF

!=== river mouth flow ===
!$OMP PARALLEL DO SIMD PRIVATE(DSFC,DSFC_pr,DSLP,DFLW,DFLW_pr,DFLW_im,DARE,DARE_pr,DARE_im,DOUT_pr,DOUT,DVEL,Mask)
DO ISEQ=NSEQRIV+1, NSEQALL
  DSLP = ( D2SFCELV(ISEQ,1) - D2DWNELV(ISEQ,1) ) * PDSTMTH ** (-1._JPRB)
  IF( LSLOPEMOUTH) DSLP = D2ELEVSLOPE(ISEQ,1)

  DFLW = D2RIVDPH(ISEQ,1)
  DARE = D2RIVWTH(ISEQ,1) * DFLW
  DARE = MAX( DARE, 1.E-10_JPRB )             !!  flow cross-section area (min value for stability)

  DFLW_pr=D2RIVDPH_PRE(ISEQ,1)
  DFLW_im=MAX( (DFLW*DFLW_pr)**0.5_JPRB, 1.E-6_JPRB )                                    !! semi implicit flow depth

  DOUT_pr = D2RIVOUT_PRE(ISEQ,1) * D2RIVWTH(ISEQ,1)**(-1._JPRB)
  DOUT = D2RIVWTH(ISEQ,1) * ( DOUT_pr + PGRV*DT*DFLW_im*DSLP ) &
           * ( 1._JPRB + PGRV*DT*D2RIVMAN(ISEQ,1)**2._JPRB * abs(DOUT_pr)*DFLW_im**(-7._JPRB/3._JPRB) )**(-1._JPRB)
  DVEL = D2RIVOUT(ISEQ,1) * DARE**(-1._JPRB)

  Mask=(DFLW_im>1.E-5 .and. DARE>1.E-5)   !! replace small depth location with zero
  D2RIVOUT(ISEQ,1) = merge( DOUT, 0._JPRB, Mask)
  D2RIVVEL(ISEQ,1) = merge( DVEL, 0._JPRB, Mask)
END DO
!$OMP END PARALLEL DO SIMD

!=== floodplain mouth flow ===
IF( LFLDOUT )THEN
  !$OMP PARALLEL DO SIMD PRIVATE(DFSTO,DSFC,DSFC_pr,DSLP,DFLW,DFLW_pr,DFLW_im,DARE,DARE_pr,DARE_im,DOUT_pr,DOUT,Mask)
  DO ISEQ=NSEQRIV+1, NSEQALL
    DFSTO =REAL( P2FLDSTO(ISEQ,1),KIND=JPRB)
    DSLP = ( D2SFCELV(ISEQ,1) - D2DWNELV(ISEQ,1) ) * PDSTMTH ** (-1._JPRB)
    DSLP = max( -0.005_JPRB, min( 0.005_JPRB,DSLP ))    !! set max&min [instead of using weir equation for efficiency]
    IF( LSLOPEMOUTH) DSLP = D2ELEVSLOPE(ISEQ,1)

    DFLW = D2SFCELV(ISEQ,1)-D2ELEVTN(ISEQ,1)
    DARE = MAX( DFSTO * D2RIVLEN(ISEQ,1)**(-1._JPRB) - D2FLDDPH(ISEQ,1)*D2RIVWTH(ISEQ,1), 0._JPRB ) !! remove above river channel area
  
    DFLW_pr = D2SFCELV_PRE(ISEQ,1)-D2ELEVTN(ISEQ,1)
    DFLW_im = MAX( (MAX(DFLW*DFLW_pr,0._JPRB))**0.5_JPRB, 1.E-6_JPRB )
  
    DARE_pr = MAX( D2FLDSTO_PRE(ISEQ,1) * D2RIVLEN(ISEQ,1)**(-1._JPRB) - D2FLDDPH_PRE(ISEQ,1)*D2RIVWTH(ISEQ,1), 1.E-6_JPRB )   
    DARE_im = MAX( (DARE*DARE_pr)**0.5_JPRB, 1.E-6_JPRB )
  
    DOUT_pr = D2FLDOUT_PRE(ISEQ,1)
    DOUT    = ( DOUT_pr + PGRV*DT*DARE_im*DSLP ) &
                        * (1._JPRB + PGRV*DT*PMANFLD**2._JPRB * abs(DOUT_pr)*DFLW_im**(-4._JPRB/3._JPRB)*DARE_im**(-1._JPRB) )**(-1._JPRB)

    Mask=(DFLW_im>1.E-5 .and. DARE>1.E-5)   !! replace small depth location with zero
    D2FLDOUT(ISEQ,1) = merge( DOUT, 0._JPRB, Mask)

    DOUT=D2FLDOUT(ISEQ,1)
    Mask=( D2FLDOUT(ISEQ,1)*D2RIVOUT(ISEQ,1)>0._JPRB ) !! river and floodplain different direction
    D2FLDOUT(ISEQ,1) = merge( DOUT, 0._JPRB, Mask)
  END DO
  !$OMP END PARALLEL DO SIMD
ENDIF


!$OMP PARALLEL DO SIMD PRIVATE(RATE,DOUT)
DO ISEQ=1, NSEQRIV
  !! Storage change limitter to prevent sudden increase of "upstream" water level when backwardd flow (v423)
  DOUT = max( (-D2RIVOUT(ISEQ,1)-D2FLDOUT(ISEQ,1))*DT, 1.E-10 )
  RATE = min( 0.05_JPRB*D2STORGE(ISEQ,1)/DOUT, 1._JPRB)
  D2RIVOUT(ISEQ,1)=D2RIVOUT(ISEQ,1)*RATE
  D2FLDOUT(ISEQ,1)=D2FLDOUT(ISEQ,1)*RATE
END DO
!$OMP END PARALLEL DO SIMD

END SUBROUTINE CMF_CALC_OUTFLW
!####################################################################
!+
!+
!+
!####################################################################
SUBROUTINE CMF_CALC_INFLOW
USE YOS_CMF_MAP,        ONLY: NSEQALL,  NPTHOUT, NPTHLEV, I2MASK, PTH_UPST, PTH_DOWN
USE YOS_CMF_PROG,       ONLY: D1PTHFLW
USE YOS_CMF_DIAG,       ONLY: D2PTHOUT, D1PTHFLWSUM
IMPLICIT NONE
!*** Local
REAL(KIND=JPRD)            :: P2STOOUT(NSEQALL,1)                      !! total outflow from a grid     [m3]
REAL(KIND=JPRD)            :: P2RIVINF(NSEQALL,1)                      !! 
REAL(KIND=JPRD)            :: P2FLDINF(NSEQALL,1)                      !! 

REAL(KIND=JPRD)            :: P2PTHOUT(NSEQALL,1)                      !! for water conservation

REAL(KIND=JPRB)            :: D2RATE(NSEQALL,1)                        !! outflow correction
! SAVE for OpenMP
INTEGER(KIND=JPIM),SAVE    :: ISEQ, JSEQ, IPTH, ILEV, ISEQP, JSEQP
REAL(KIND=JPRB),SAVE       :: OUT_R1, OUT_R2, OUT_F1, OUT_F2, DIUP, DIDW
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
    D2RATE(ISEQ,1) = REAL( min( (P2RIVSTO(ISEQ,1)+P2FLDSTO(ISEQ,1)) * P2STOOUT(ISEQ,1)**(-1._JPRB), 1._JPRD ), KIND=JPRB)
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

!$OMP PARALLEL DO SIMD
DO ISEQ=1, NSEQALL
  D2RIVINF(ISEQ,1)=REAL( P2RIVINF(ISEQ,1), KIND=JPRB)  !! needed for SinglePrecisionMode
  D2FLDINF(ISEQ,1)=REAL( P2FLDINF(ISEQ,1), KIND=JPRB)
  D2PTHOUT(ISEQ,1)=REAL( P2PTHOUT(ISEQ,1), KIND=JPRB)
END DO
!$OMP END PARALLEL DO SIMD

END SUBROUTINE CMF_CALC_INFLOW
!####################################################################
!+
!+
!+
!####################################################################
SUBROUTINE CMF_CALC_INFLOW_LSPAMAT
USE YOS_CMF_MAP,        ONLY: NSEQALL,  NPTHOUT, NPTHLEV, I2MASK, PTH_UPST, PTH_DOWN
USE YOS_CMF_PROG,       ONLY: D1PTHFLW
USE YOS_CMF_DIAG,       ONLY: D2PTHOUT, D1PTHFLWSUM, P2STOOUT, P2RIVINF, P2FLDINF, P2PTHOUT, D2RATE
IMPLICIT NONE
! SAVE for OpenMP
REAL(KIND=JPRB),SAVE       :: DOUT, DSTO
INTEGER(KIND=JPIM),SAVE    :: ISEQ, IPTH, JSEQ, ILEV, INUM, JPTH, ISEQP, JSEQP
!$OMP THREADPRIVATE                      (JSEQ, ILEV, INUM, JPTH, ISEQP, JSEQP)
!================================================
  
!*** 1. initialize & calculate P2STOOUT for normal cells

!$OMP PARALLEL DO SIMD
DO ISEQ=1, NSEQALL
  P2RIVINF(ISEQ,1) = 0._JPRD
  P2FLDINF(ISEQ,1) = 0._JPRD
  P2PTHOUT(ISEQ,1) = 0._JPRD
  P2STOOUT(ISEQ,1) = 0._JPRD
END DO
!$OMP END PARALLEL DO SIMD

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

!$OMP PARALLEL DO SIMD PRIVATE(DOUT,DSTO)
DO ISEQ=1, NSEQALL
  DOUT=max( REAL(P2STOOUT(ISEQ,1),KIND=JPRB),1.E-10_JPRB)
  DSTO=REAL( (P2RIVSTO(ISEQ,1)+P2FLDSTO(ISEQ,1)),KIND=JPRB )
  D2RATE(ISEQ,1) = min( DSTO/DOUT, 1._JPRB )
END DO
!$OMP END PARALLEL DO SIMD

!! normal pixels------
!$OMP PARALLEL DO !! No OMP Atomic for bit-identical simulation (set in Mkinclude)
DO ISEQ=1, NSEQRIV ! for normal pixels
  JSEQ=I1NEXT(ISEQ)
  IF( D2RIVOUT(ISEQ,1) > 0._JPRB )THEN
    D2RIVOUT(ISEQ,1) = D2RIVOUT(ISEQ,1)*D2RATE(ISEQ,1)
    D2FLDOUT(ISEQ,1) = D2FLDOUT(ISEQ,1)*D2RATE(ISEQ,1)
  ELSE
    D2RIVOUT(ISEQ,1) = D2RIVOUT(ISEQ,1)*D2RATE(JSEQ,1)
    D2FLDOUT(ISEQ,1) = D2FLDOUT(ISEQ,1)*D2RATE(JSEQ,1)
  ENDIF
END DO
!$OMP END PARALLEL DO

!! river mouth-----------------
!$OMP PARALLEL DO SIMD
DO ISEQ=NSEQRIV+1, NSEQALL
  D2RIVOUT(ISEQ,1) = D2RIVOUT(ISEQ,1)*D2RATE(ISEQ,1)
  D2FLDOUT(ISEQ,1) = D2FLDOUT(ISEQ,1)*D2RATE(ISEQ,1)
END DO
!$OMP END PARALLEL DO SIMD

!============================
!*** 3. Inflow calculation


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

!$OMP PARALLEL DO SIMD
DO ISEQ=1, NSEQALL
  D2RIVINF(ISEQ,1)=REAL( P2RIVINF(ISEQ,1), KIND=JPRB)  !! needed for SinglePrecisionMode
  D2FLDINF(ISEQ,1)=REAL( P2FLDINF(ISEQ,1), KIND=JPRB)
  D2PTHOUT(ISEQ,1)=REAL( P2PTHOUT(ISEQ,1), KIND=JPRB)
END DO
!$OMP END PARALLEL DO SIMD

END SUBROUTINE CMF_CALC_INFLOW_LSPAMAT
!####################################################################
END MODULE CMF_CALC_OUTFLW_MOD
