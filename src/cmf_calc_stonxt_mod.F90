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
USE YOS_CMF_INPUT,      ONLY: LGDWDLY,   DT,  LWEVAP
USE YOS_CMF_MAP,        ONLY: NSEQALL
USE YOS_CMF_PROG,       ONLY: B2RIVOUT,  B2FLBOUT,  D2RIVSTO,  D2FLDSTO,  B2RUNOFF
USE YOS_CMF_PROG,       ONLY: B2GDWSTO,  B2GDWRTN,  B2ROFSUB,  B2WEVAP
USE YOS_CMF_DIAG,       ONLY: B2RIVINF,  B2FLDINF,  B2PTHOUT,  B2PTHINF,  B2FLDFRC, &
                            & B2OUTFLW,  B2STORGE,  B2WEVAPEX
USE YOS_CMF_DIAG,       ONLY: DGLBSTOPRE,DGLBSTONXT,DGLBSTONEW,DGLBRIVINF,DGLBRIVOUT
IMPLICIT NONE
! Save for OpenMP
INTEGER(KIND=JPIM),SAVE    ::  ISEQ
REAL(KIND=JPRB),SAVE       ::  BRIVROF, BFLDROF, BWEVAPEX
!$OMP THREADPRIVATE           (BRIVROF, BFLDROF, BWEVAPEX)
!================================================
IF ( LGDWDLY ) THEN
  CALL CALC_GDWDLY
ELSE
  ! No ground water delay 
  !$OMP PARALLEL DO
  DO ISEQ=1,NSEQALL
    B2GDWRTN(ISEQ,1) = B2ROFSUB(ISEQ,1)
    B2GDWSTO(ISEQ,1) = 0._JPRD
  ENDDO
  !$OMP END PARALLEL DO
ENDIF
!!==============================
DGLBSTOPRE=0._JPRD
DGLBSTONXT=0._JPRD
DGLBSTONEW=0._JPRD
DGLBRIVINF=0._JPRD
DGLBRIVOUT=0._JPRD

!$OMP PARALLEL DO REDUCTION(+:DGLBSTOPRE,DGLBRIVINF,DGLBRIVOUT,DGLBSTONXT,DGLBSTONEW)
DO ISEQ=1, NSEQALL

  DGLBSTOPRE = DGLBSTOPRE + D2RIVSTO(ISEQ,1)    + D2FLDSTO(ISEQ,1)
  DGLBRIVINF = DGLBRIVINF + B2RIVINF(ISEQ,1)*DT + B2FLDINF(ISEQ,1)*DT + B2PTHINF(ISEQ,1)*DT
  DGLBRIVOUT = DGLBRIVOUT + B2RIVOUT(ISEQ,1)*DT + B2FLBOUT(ISEQ,1)*DT + B2PTHOUT(ISEQ,1)*DT

  D2RIVSTO(ISEQ,1) = D2RIVSTO(ISEQ,1) + B2RIVINF(ISEQ,1)*DT - B2RIVOUT(ISEQ,1)*DT
  IF ( D2RIVSTO(ISEQ,1) < 0._JPRB ) THEN
    D2FLDSTO(ISEQ,1) = D2FLDSTO(ISEQ,1) + D2RIVSTO(ISEQ,1)
    D2RIVSTO(ISEQ,1) = 0._JPRD
  ENDIF

  D2FLDSTO(ISEQ,1) = D2FLDSTO(ISEQ,1) + B2FLDINF(ISEQ,1)*DT - B2FLBOUT(ISEQ,1)*DT &
                                      + B2PTHINF(ISEQ,1)*DT - B2PTHOUT(ISEQ,1)*DT
  IF( D2FLDSTO(ISEQ,1) < 0._JPRD )THEN
    D2RIVSTO(ISEQ,1)=MAX( D2RIVSTO(ISEQ,1)+D2FLDSTO(ISEQ,1), 0._JPRD )
    D2FLDSTO(ISEQ,1)=0._JPRD
  ENDIF

  DGLBSTONXT = DGLBSTONXT + D2RIVSTO(ISEQ,1) + D2FLDSTO(ISEQ,1)
  B2OUTFLW(ISEQ,1)=B2RIVOUT(ISEQ,1)+B2FLBOUT(ISEQ,1)+B2PTHOUT(ISEQ,1)

  BRIVROF = ( B2RUNOFF(ISEQ,1)+B2GDWRTN(ISEQ,1) ) * ( 1._JPRB-B2FLDFRC(ISEQ,1) ) * DT
  BFLDROF = ( B2RUNOFF(ISEQ,1)+B2GDWRTN(ISEQ,1) ) *           B2FLDFRC(ISEQ,1)   * DT
  D2RIVSTO(ISEQ,1) = D2RIVSTO(ISEQ,1) + BRIVROF
  D2FLDSTO(ISEQ,1) = D2FLDSTO(ISEQ,1) + BFLDROF

  IF (LWEVAP) THEN
    !! Find out amount of water to be extracted from flooplain reservoir
    !! Assuming "potential water evaporation", multiplied by flood area fraction
    !! Limited by total amount of flooplain storage 
    BWEVAPEX = MIN(D2FLDSTO(ISEQ,1),B2FLDFRC(ISEQ,1)*DT*B2WEVAP(ISEQ,1)*1._JPRD)
    D2FLDSTO(ISEQ,1) = D2FLDSTO(ISEQ,1) - BWEVAPEX 
    B2WEVAPEX(ISEQ,1) = BWEVAPEX/DT ! keept for output as flux 
  ENDIF 

  B2STORGE(ISEQ,1)=D2RIVSTO(ISEQ,1)+D2FLDSTO(ISEQ,1)
  DGLBSTONEW=DGLBSTONEW+B2STORGE(ISEQ,1)

END DO
!$OMP END PARALLEL DO
CONTAINS
!==========================================================
!+ CALC_GDWDLY
!+ 
!+ 
!==========================================================
SUBROUTINE CALC_GDWDLY
USE YOS_CMF_MAP,        ONLY: NSEQALL,B2GDWDLY
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
  IF (B2GDWDLY(ISEQ,1)>0._JPRB) THEN 
    ! Only if GW delay > 0 
    ZMULGW = 1._JPRB / ( ZDTI + 1._JPRB/B2GDWDLY(ISEQ,1) ) 
    B2GDWSTO(ISEQ,1) = ( B2ROFSUB(ISEQ,1) + B2GDWSTO(ISEQ,1)*ZDTI ) *ZMULGW
    B2GDWRTN(ISEQ,1) = B2GDWSTO(ISEQ,1) / B2GDWDLY(ISEQ,1)
  ELSE
    ! Zero GW delay 
    B2GDWSTO(ISEQ,1) = 0._JPRD
    B2GDWRTN(ISEQ,1) = B2ROFSUB(ISEQ,1)
  ENDIF
ENDDO
!$OMP END PARALLEL DO

END SUBROUTINE CALC_GDWDLY
!==========================================================

END SUBROUTINE CMF_CALC_STONXT
!####################################################################

END MODULE CMF_CALC_STONXT_MOD
