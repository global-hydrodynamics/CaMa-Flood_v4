MODULE CMF_CALC_STONXT_MOD
!==========================================================
!* PURPOSE: calculate the storage in the next time step in FTCS diff. eq.
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
! -- CMF_CALC_STONXT
!
!####################################################################
SUBROUTINE CMF_CALC_STONXT
USE PARKIND1,           ONLY: JPIM, JPRB, JPRD
USE YOS_CMF_INPUT,      ONLY: LGDWDLY,   DT,  LWEVAP, LLEVEE, LROSPLIT
USE YOS_CMF_MAP,        ONLY: NSEQALL
USE YOS_CMF_PROG,       ONLY: D2RIVOUT,  D2FLDOUT,  P2RIVSTO,  P2FLDSTO,  D2RUNOFF, P2LEVSTO
USE YOS_CMF_PROG,       ONLY: P2GDWSTO,  D2GDWRTN,  D2ROFSUB,  D2WEVAP
USE YOS_CMF_DIAG,       ONLY: D2RIVINF,  D2FLDINF,  D2PTHOUT,  D2FLDFRC, &
                            & D2OUTFLW,  D2STORGE,  D2WEVAPEX
USE YOS_CMF_DIAG,       ONLY: P0GLBSTOPRE,P0GLBSTONXT,P0GLBSTONEW,P0GLBRIVINF,P0GLBRIVOUT
IMPLICIT NONE
! Save for OpenMP
INTEGER(KIND=JPIM),SAVE    ::  ISEQ
REAL(KIND=JPRB),SAVE       ::  DRIVROF, DFLDROF, DWEVAPEX
!$OMP THREADPRIVATE           (DRIVROF, DFLDROF, DWEVAPEX)
!================================================
IF ( LGDWDLY ) THEN
  CALL CALC_GDWDLY
ELSEIF( LROSPLIT )THEN  !! when using sub-surface ruboff without groundwater delay
  ! No ground water delay 
  !$OMP PARALLEL DO SIMD
  DO ISEQ=1,NSEQALL
    D2GDWRTN(ISEQ,1) = D2ROFSUB(ISEQ,1)
    P2GDWSTO(ISEQ,1) = 0._JPRD
  ENDDO
  !$OMP END PARALLEL DO SIMD
ENDIF
!!==============================
P0GLBSTOPRE=0._JPRD
P0GLBSTONXT=0._JPRD
P0GLBSTONEW=0._JPRD
P0GLBRIVINF=0._JPRD
P0GLBRIVOUT=0._JPRD

!$OMP PARALLEL DO SIMD REDUCTION(+:P0GLBSTOPRE,P0GLBRIVINF,P0GLBRIVOUT,P0GLBSTONXT,P0GLBSTONEW)
DO ISEQ=1, NSEQALL

  P0GLBSTOPRE = P0GLBSTOPRE + P2RIVSTO(ISEQ,1)    + P2FLDSTO(ISEQ,1)
!!  P0GLBRIVINF = P0GLBRIVINF + D2RIVINF(ISEQ,1)*DT + D2FLDINF(ISEQ,1)*DT + D2PTHINF(ISEQ,1)*DT  !! pthinf not used v4.3
  P0GLBRIVINF = P0GLBRIVINF + D2RIVINF(ISEQ,1)*DT + D2FLDINF(ISEQ,1)*DT
  P0GLBRIVOUT = P0GLBRIVOUT + D2RIVOUT(ISEQ,1)*DT + D2FLDOUT(ISEQ,1)*DT + D2PTHOUT(ISEQ,1)*DT

  P2RIVSTO(ISEQ,1) = P2RIVSTO(ISEQ,1) + D2RIVINF(ISEQ,1)*DT - D2RIVOUT(ISEQ,1)*DT
  IF ( P2RIVSTO(ISEQ,1) < 0._JPRD ) THEN
    P2FLDSTO(ISEQ,1) = P2FLDSTO(ISEQ,1) + P2RIVSTO(ISEQ,1)
    P2RIVSTO(ISEQ,1) = 0._JPRD
  ENDIF

  P2FLDSTO(ISEQ,1) = P2FLDSTO(ISEQ,1) + D2FLDINF(ISEQ,1)*DT - D2FLDOUT(ISEQ,1)*DT &
                                      - D2PTHOUT(ISEQ,1)*DT
!!                                      + D2PTHINF(ISEQ,1)*DT - D2PTHOUT(ISEQ,1)*DT  !! pthinf not used v4.3
  IF( P2FLDSTO(ISEQ,1) < 0._JPRD )THEN
    P2RIVSTO(ISEQ,1)=MAX( P2RIVSTO(ISEQ,1)+P2FLDSTO(ISEQ,1), 0._JPRD )
    P2FLDSTO(ISEQ,1)=0._JPRD
  ENDIF

  P0GLBSTONXT = P0GLBSTONXT + P2RIVSTO(ISEQ,1) + P2FLDSTO(ISEQ,1)
  D2OUTFLW(ISEQ,1)=D2RIVOUT(ISEQ,1)+D2FLDOUT(ISEQ,1)
!!  D2OUTFLW(ISEQ,1)=D2RIVOUT(ISEQ,1)+D2FLDOUT(ISEQ,1)+D2PTHOUT(ISEQ,1)   !! bug before v4.2 (pthout shoudl not be added)

  DRIVROF = ( D2RUNOFF(ISEQ,1)+D2GDWRTN(ISEQ,1) ) * ( 1._JPRB-D2FLDFRC(ISEQ,1) ) * DT
  DFLDROF = ( D2RUNOFF(ISEQ,1)+D2GDWRTN(ISEQ,1) ) *           D2FLDFRC(ISEQ,1)   * DT
  P2RIVSTO(ISEQ,1) = P2RIVSTO(ISEQ,1) + DRIVROF
  P2FLDSTO(ISEQ,1) = P2FLDSTO(ISEQ,1) + DFLDROF

  IF (LWEVAP) THEN
    !! Find out amount of water to be extracted from flooplain reservoir
    !! Assuming "potential water evaporation", multiplied by flood area fraction
    !! Limited by total amount of flooplain storage 
    DWEVAPEX = MIN( REAL(P2FLDSTO(ISEQ,1),KIND=JPRB),D2FLDFRC(ISEQ,1)*DT*D2WEVAP(ISEQ,1) )
    P2FLDSTO(ISEQ,1) = P2FLDSTO(ISEQ,1) - DWEVAPEX 
    D2WEVAPEX(ISEQ,1) = DWEVAPEX/DT ! keept for output as flux 
  ENDIF 

  D2STORGE(ISEQ,1)=REAL( P2RIVSTO(ISEQ,1)+P2FLDSTO(ISEQ,1), KIND=JPRB)
  IF( LLEVEE ) D2STORGE(ISEQ,1) = REAL( D2STORGE(ISEQ,1)+P2LEVSTO(ISEQ,1), KIND=JPRB)

  P0GLBSTONEW=P0GLBSTONEW+D2STORGE(ISEQ,1)

END DO
!$OMP END PARALLEL DO SIMD
CONTAINS
!==========================================================
!+ CALC_GDWDLY
!+ 
!+ 
!==========================================================
SUBROUTINE CALC_GDWDLY
USE YOS_CMF_MAP,        ONLY: D2GDWDLY
IMPLICIT NONE
!*** LOCAL
REAL(KIND=JPRB)            ::  ZDTI
! SAVE for OpenMP
INTEGER(KIND=JPIM),SAVE    ::  ISEQ
REAL(KIND=JPRB),SAVE       ::  ZMULGW
!$OMP THREADPRIVATE           (ZMULGW)
!=====================================================
ZDTI = 1._JPRB / DT ! Inverse time-step 
!$OMP PARALLEL DO SIMD
DO ISEQ=1,NSEQALL
  IF (D2GDWDLY(ISEQ,1)>0._JPRB) THEN 
    ! Only if GW delay > 0 
    ZMULGW = 1._JPRB / ( ZDTI + 1._JPRB/D2GDWDLY(ISEQ,1) ) 
    P2GDWSTO(ISEQ,1) = ( D2ROFSUB(ISEQ,1) + P2GDWSTO(ISEQ,1)*ZDTI ) *ZMULGW
    D2GDWRTN(ISEQ,1) = REAL(P2GDWSTO(ISEQ,1),KIND=JPRB) / D2GDWDLY(ISEQ,1)
  ELSE
    ! Zero GW delay 
    P2GDWSTO(ISEQ,1) = 0._JPRD
    D2GDWRTN(ISEQ,1) = D2ROFSUB(ISEQ,1)
  ENDIF
ENDDO
!$OMP END PARALLEL DO SIMD

END SUBROUTINE CALC_GDWDLY
!==========================================================

END SUBROUTINE CMF_CALC_STONXT
!####################################################################

END MODULE CMF_CALC_STONXT_MOD
