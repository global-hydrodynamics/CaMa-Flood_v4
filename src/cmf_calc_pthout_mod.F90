MODULE CMF_CALC_PTHOUT_MOD
!==========================================================
!* PURPOSE: subroutine for bifurcation channel flow
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
! -- CMF_CALC_PTHOUT
! --
!####################################################################
SUBROUTINE CMF_CALC_PTHOUT
USE PARKIND1,           ONLY: JPIM, JPRB
USE YOS_CMF_INPUT,      ONLY: DT, PGRV, DMIS
USE YOS_CMF_MAP,        ONLY: NSEQALL, NSEQMAX, NPTHOUT, NPTHLEV, PTH_UPST, PTH_DOWN, PTH_DST, &
                            & PTH_ELV, PTH_WTH, PTH_MAN, I2MASK
USE YOS_CMF_MAP,        ONLY: B2RIVELV
USE YOS_CMF_PROG,       ONLY: D2RIVSTO, D2FLDSTO, B1PTHFLW, B2RIVOUT, B2FLDOUT
USE YOS_CMF_PROG,       ONLY: B1PTHFLW_PRE, B2RIVDPH_PRE
USE YOS_CMF_DIAG,       ONLY: B2PTHOUT, B2PTHINF, B2RIVINF, B2FLDINF, B2SFCELV
IMPLICIT NONE
!*** Local
REAL(KIND=JPRB)         ::  B2SFCELV_PRE(NSEQMAX,1)                  !! water surface elev (t-1) [m] (for stable calculation)
REAL(KIND=JPRB)         ::  B2RATE(NSEQMAX,1)                        !! outflow correction

! Save for OpenMP
INTEGER(KIND=JPIM),SAVE ::  IPTH, ILEV, ISEQ, ISEQP, JSEQP
REAL(KIND=JPRB),SAVE    ::  BSLOPE, BFLW, BOUT_PRE, BFLW_PRE, BFLW_IMP, BSTO_TMP
!$OMP THREADPRIVATE        (BSLOPE, BFLW, BOUT_PRE, BFLW_PRE, BFLW_IMP, BSTO_TMP, ILEV, ISEQP, JSEQP)
!================================================
!$OMP PARALLEL DO
DO ISEQ=1, NSEQALL
  B2SFCELV_PRE(ISEQ,1) = B2RIVELV(ISEQ,1)+B2RIVDPH_PRE(ISEQ,1)
  B2PTHOUT(ISEQ,1) = 0._JPRB
  B2PTHINF(ISEQ,1) = 0._JPRB
  B2RATE(ISEQ,1)   =-999._JPRB
END DO
!$OMP END PARALLEL DO

B1PTHFLW(:,:) = DMIS
!$OMP PARALLEL DO
DO IPTH=1, NPTHOUT  
  ISEQP=PTH_UPST(IPTH)
  JSEQP=PTH_DOWN(IPTH)
  !! Avoid calculation outside of domain
  IF (ISEQP == 0 .OR. JSEQP== 0 ) CYCLE
  IF (I2MASK(ISEQP,1)>0 .OR. I2MASK(JSEQP,1)>0 ) CYCLE  !! I2MASK is for 1: kinemacit 2: dam  no bifurcation
  
  BSLOPE  = (B2SFCELV(ISEQP,1)-B2SFCELV(JSEQP,1)) * PTH_DST(IPTH)**(-1.)
  BSLOPE = max(-0.005_JPRB,min(0.005_JPRB,BSLOPE))                                    !! v390 stabilization

  DO ILEV=1, NPTHLEV

    BFLW = MAX(B2SFCELV(ISEQP,1),B2SFCELV(JSEQP,1)) - PTH_ELV(IPTH,ILEV) 
    BFLW = MAX(BFLW,0._JPRB)

    BFLW_PRE = MAX(B2SFCELV_PRE(ISEQP,1),B2SFCELV_PRE(JSEQP,1)) - PTH_ELV(IPTH,ILEV)
    BFLW_PRE = MAX(BFLW_PRE,0._JPRB)

    BFLW_IMP = (BFLW*BFLW_PRE)**0.5                                       !! semi implicit flow depth
    IF( BFLW_IMP<=0._JPRB ) BFLW_IMP=BFLW

    IF( BFLW_IMP>1.E-5 )THEN                         !! local inertial equation, see [Bates et al., 2010, J.Hydrol.]
      BOUT_PRE = B1PTHFLW_PRE(IPTH,ILEV) * PTH_WTH(IPTH,ILEV)**(-1.)                         !! outflow (t-1) [m2/s] (unit width)
      B1PTHFLW(IPTH,ILEV) = PTH_WTH(IPTH,ILEV) * ( BOUT_PRE + PGRV*DT*BFLW_IMP*BSLOPE ) &
                         * ( 1. + PGRV*DT*PTH_MAN(ILEV)**2. * abs(BOUT_PRE)*BFLW_IMP**(-7./3.) )**(-1.)
    ELSE
      B1PTHFLW(IPTH,ILEV) = 0._JPRB
    ENDIF
  END DO
END DO
!$OMP END PARALLEL DO

#ifndef NoAtom_CMF
!$OMP PARALLEL DO  !! No OMP Atomic for bit-identical simulation (set in Mkinclude)
#endif
DO IPTH=1, NPTHOUT  
  ISEQP=PTH_UPST(IPTH)
  JSEQP=PTH_DOWN(IPTH)
  !! Avoid calculation outside of domain
  IF (ISEQP==0 .OR. JSEQP==0 ) CYCLE
  IF (I2MASK(ISEQP,1)>0 .OR. I2MASK(JSEQP,1)>0 ) CYCLE  !! I2MASK is for 1: kinemacit 2: dam  no bifurcation

  DO ILEV=1, NPTHLEV
    IF( B1PTHFLW(IPTH,ILEV) >= 0._JPRB )THEN                                  !! total outflow from each grid
!$OMP ATOMIC
      B2PTHOUT(ISEQP,1) = B2PTHOUT(ISEQP,1) + B1PTHFLW(IPTH,ILEV)
    ELSE
!$OMP ATOMIC
      B2PTHOUT(JSEQP,1) = B2PTHOUT(JSEQP,1) - B1PTHFLW(IPTH,ILEV)
    ENDIF
  END DO
END DO
#ifndef NoAtom_CMF
!$OMP END PARALLEL DO  !! No OMP Atomic for bit-identical simulation (set in Mkinclude)
#endif

!$OMP PARALLEL DO                                              !! calculate total outflow from a grid
DO ISEQ=1, NSEQALL
  IF( B2PTHOUT(ISEQ,1) > 1.E-10 )THEN
    BSTO_TMP = ( D2RIVSTO(ISEQ,1)+D2FLDSTO(ISEQ,1) ) &
                  - B2RIVOUT(ISEQ,1)*DT + B2RIVINF(ISEQ,1)*DT - B2FLDOUT(ISEQ,1)*DT + B2FLDINF(ISEQ,1)*DT
    B2RATE(ISEQ,1) = MIN( BSTO_TMP * (B2PTHOUT(ISEQ,1)*DT)**(-1.), 1._JPRB )
  ELSE
    B2RATE(ISEQ,1) = 1._JPRB
  ENDIF
  B2PTHOUT(ISEQ,1) = B2PTHOUT(ISEQ,1) * B2RATE(ISEQ,1)
END DO
!$OMP END PARALLEL DO

#ifndef NoAtom_CMF
!$OMP PARALLEL DO  !! No OMP Atomic for bit-identical simulation (set in Mkinclude)
#endif
DO IPTH=1, NPTHOUT
  ISEQP=PTH_UPST(IPTH)
  JSEQP=PTH_DOWN(IPTH)
  !! Avoid calculation outside of domain
  IF (ISEQP == 0 .OR. JSEQP== 0 ) CYCLE
  IF (I2MASK(ISEQP,1)>0 .OR. I2MASK(JSEQP,1)>0 ) CYCLE  !! I2MASK is for 1: kinemacit 2: dam  no bifurcation
  
  DO ILEV=1, NPTHLEV
    IF( B1PTHFLW(IPTH,ILEV) >= 0._JPRB )THEN
      B1PTHFLW(IPTH,ILEV) = B1PTHFLW(IPTH,ILEV)*B2RATE(ISEQP,1)
!$OMP ATOMIC
      B2PTHINF(JSEQP,1) = B2PTHINF(JSEQP,1) + B1PTHFLW(IPTH,ILEV)             !! total inflow [m3/s] (from upstream)
    ELSE
      B1PTHFLW(IPTH,ILEV) = B1PTHFLW(IPTH,ILEV)*B2RATE(JSEQP,1)
!$OMP ATOMIC
      B2PTHINF(ISEQP,1) = B2PTHINF(ISEQP,1) - B1PTHFLW(IPTH,ILEV)             !! total inflow [m3/s] (from upstream)
    ENDIF
    B1PTHFLW_PRE(IPTH,ILEV)=B1PTHFLW(IPTH,ILEV)
  END DO
END DO
#ifndef NoAtom_CMF
!$OMP END PARALLEL DO  !! No OMP Atomic for bit-identical simulation (set in Mkinclude)
#endif

END SUBROUTINE CMF_CALC_PTHOUT
!####################################################################




!####################################################################
END MODULE CMF_CALC_PTHOUT_MOD
