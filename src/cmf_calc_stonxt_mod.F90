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
USE PARKIND1,           ONLY: JPIM, JPRB
USE YOS_CMF_INPUT,      ONLY: LGDWDLY,   DT,  LWEVAP
USE YOS_CMF_MAP,        ONLY: NSEQALL
USE YOS_CMF_PROG,       ONLY: D2RIVOUT,  D2FLDOUT,  D2RIVSTO,  D2FLDSTO,  D2RUNOFF
USE YOS_CMF_PROG,       ONLY: D2GDWSTO,  D2GDWRTN,  D2ROFSUB,  D2WEVAP
USE YOS_CMF_DIAG,       ONLY: D2RIVINF,  D2FLDINF,  D2PTHOUT,  D2PTHINF,  D2FLDFRC, &
                            & D2OUTFLW,  D2STORGE,  D2WEVAPEX
USE YOS_CMF_DIAG,       ONLY: DGLBSTOPRE,DGLBSTONXT,DGLBSTONEW,DGLBRIVINF,DGLBRIVOUT
IMPLICIT NONE
! Save for OpenMP
INTEGER(KIND=JPIM),SAVE    ::  ISEQ
REAL(KIND=JPRB),SAVE       ::  DRIVROF, DFLDROF, DWEVAPEX
!$OMP THREADPRIVATE           (DRIVROF, DFLDROF, DWEVAPEX)
!================================================
IF ( LGDWDLY ) THEN
  CALL CALC_GDWDLY
ELSE
  ! No ground water delay 
  !$OMP PARALLEL DO
  DO ISEQ=1,NSEQALL
    D2GDWRTN(ISEQ,1) = D2ROFSUB(ISEQ,1)
    D2GDWSTO(ISEQ,1) = 0._JPRB
  ENDDO
  !$OMP END PARALLEL DO
ENDIF
!!==============================
DGLBSTOPRE=0._JPRB
DGLBSTONXT=0._JPRB
DGLBSTONEW=0._JPRB
DGLBRIVINF=0._JPRB
DGLBRIVOUT=0._JPRB

!$OMP PARALLEL DO REDUCTION(+:DGLBSTOPRE,DGLBRIVINF,DGLBRIVOUT,DGLBSTONXT,DGLBSTONEW)
DO ISEQ=1, NSEQALL

  DGLBSTOPRE = DGLBSTOPRE + D2RIVSTO(ISEQ,1)    + D2FLDSTO(ISEQ,1)
  DGLBRIVINF = DGLBRIVINF + D2RIVINF(ISEQ,1)*DT + D2FLDINF(ISEQ,1)*DT + D2PTHINF(ISEQ,1)*DT
  DGLBRIVOUT = DGLBRIVOUT + D2RIVOUT(ISEQ,1)*DT + D2FLDOUT(ISEQ,1)*DT + D2PTHOUT(ISEQ,1)*DT

  D2RIVSTO(ISEQ,1) = D2RIVSTO(ISEQ,1) + D2RIVINF(ISEQ,1)*DT - D2RIVOUT(ISEQ,1)*DT
  IF ( D2RIVSTO(ISEQ,1) < 0._JPRB ) THEN
    D2FLDSTO(ISEQ,1) = D2FLDSTO(ISEQ,1) + D2RIVSTO(ISEQ,1)
    D2RIVSTO(ISEQ,1) = 0._JPRB
  ENDIF

  D2FLDSTO(ISEQ,1) = D2FLDSTO(ISEQ,1) + D2FLDINF(ISEQ,1)*DT - D2FLDOUT(ISEQ,1)*DT &
                                      + D2PTHINF(ISEQ,1)*DT - D2PTHOUT(ISEQ,1)*DT
  IF( D2FLDSTO(ISEQ,1) < 0._JPRB )THEN
    D2RIVSTO(ISEQ,1)=MAX( D2RIVSTO(ISEQ,1)+D2FLDSTO(ISEQ,1), 0._JPRB )
    D2FLDSTO(ISEQ,1)=0._JPRB
  ENDIF

  DGLBSTONXT = DGLBSTONXT + D2RIVSTO(ISEQ,1) + D2FLDSTO(ISEQ,1)
  D2OUTFLW(ISEQ,1)=D2RIVOUT(ISEQ,1)+D2FLDOUT(ISEQ,1)+D2PTHOUT(ISEQ,1)

  DRIVROF = ( D2RUNOFF(ISEQ,1)+D2GDWRTN(ISEQ,1) ) * ( 1._JPRB-D2FLDFRC(ISEQ,1) ) * DT
  DFLDROF = ( D2RUNOFF(ISEQ,1)+D2GDWRTN(ISEQ,1) ) *           D2FLDFRC(ISEQ,1)   * DT
  D2RIVSTO(ISEQ,1) = D2RIVSTO(ISEQ,1) + DRIVROF
  D2FLDSTO(ISEQ,1) = D2FLDSTO(ISEQ,1) + DFLDROF

  IF (LWEVAP) THEN
    !! Find out amount of water to be extracted from flooplain reservoir
    !! Assuming "potential water evaporation", multiplied by flood area fraction
    !! Limited by total amount of flooplain storage 
    DWEVAPEX = MIN(D2FLDSTO(ISEQ,1),D2FLDFRC(ISEQ,1)*DT*D2WEVAP(ISEQ,1))
    D2FLDSTO(ISEQ,1) = D2FLDSTO(ISEQ,1) - DWEVAPEX 
    D2WEVAPEX(ISEQ,1) = DWEVAPEX/DT ! keept for output as flux 
  ENDIF 

  D2STORGE(ISEQ,1)=D2RIVSTO(ISEQ,1)+D2FLDSTO(ISEQ,1)
  DGLBSTONEW=DGLBSTONEW+D2STORGE(ISEQ,1)

END DO
!$OMP END PARALLEL DO
CONTAINS
!==========================================================
!+ CALC_GDWDLY
!+ 
!+ 
!==========================================================
SUBROUTINE CALC_GDWDLY
USE YOS_CMF_MAP,        ONLY: NSEQALL,D2GDWDLY
IMPLICIT NONE
!*** LOCAL
REAL(KIND=JPRB)            ::  ZDTI
! SAVE for OpenMP
INTEGER(KIND=JPIM),SAVE    ::  ISEQ
REAL(KIND=JPRB),SAVE       ::  ZMULGW
!$OMP THREADPRIVATE           (ZMULGW)
!=====================================================
ZDTI = 1._JPRB / DT ! Inverse time-step 
!$OMP PARALLEL DO
DO ISEQ=1,NSEQALL
  IF (D2GDWDLY(ISEQ,1)>0._JPRB) THEN 
    ! Only if GW delay > 0 
    ZMULGW = 1._JPRB / ( ZDTI + 1._JPRB/D2GDWDLY(ISEQ,1) ) 
    D2GDWSTO(ISEQ,1) = ( D2ROFSUB(ISEQ,1) + D2GDWSTO(ISEQ,1)*ZDTI ) *ZMULGW
    D2GDWRTN(ISEQ,1) = D2GDWSTO(ISEQ,1) / D2GDWDLY(ISEQ,1)
  ELSE
    ! Zero GW delay 
    D2GDWSTO(ISEQ,1) = 0._JPRB
    D2GDWRTN(ISEQ,1) = D2ROFSUB(ISEQ,1)
  ENDIF
ENDDO
!$OMP END PARALLEL DO

END SUBROUTINE CALC_GDWDLY
!==========================================================

END SUBROUTINE CMF_CALC_STONXT
!####################################################################

END MODULE CMF_CALC_STONXT_MOD
