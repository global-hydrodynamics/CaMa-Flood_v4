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
USE YOS_CMF_INPUT,      ONLY: LGDWDLY,   DT
USE YOS_CMF_MAP,        ONLY: NSEQALL
USE YOS_CMF_PROG,       ONLY: D2RIVOUT,  D2FLDOUT,  D2RIVSTO,  D2FLDSTO,  D2RUNOFF
USE YOS_CMF_PROG,       ONLY: D2GDWSTO,  D2GDWRTN,  D2ROFSUB
USE YOS_CMF_DIAG,       ONLY: D2RIVINF,  D2FLDINF,  D2PTHOUT,  D2PTHINF,  D2FLDFRC, D2OUTFLW, D2STORGE
USE YOS_CMF_DIAG,       ONLY: DGLBSTOPRE,DGLBSTONXT,DGLBSTONEW,DGLBRIVINF,DGLBRIVOUT
IMPLICIT NONE
!$ SAVE
INTEGER(KIND=JPIM)         ::  ISEQ
REAL(KIND=JPRB)            ::  DRIVROF, DFLDROF
!$OMP THREADPRIVATE           (DRIVROF, DFLDROF)
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
DGLBSTOPRE=0.D0
DGLBSTONXT=0.D0
DGLBSTONEW=0.D0
DGLBRIVINF=0.D0
DGLBRIVOUT=0.D0

!$OMP PARALLEL DO REDUCTION(+:DGLBSTOPRE,DGLBRIVINF,DGLBRIVOUT,DGLBSTONXT,DGLBSTONEW)
DO ISEQ=1, NSEQALL

  DGLBSTOPRE = DGLBSTOPRE + D2RIVSTO(ISEQ,1)    + D2FLDSTO(ISEQ,1)
  DGLBRIVINF = DGLBRIVINF + D2RIVINF(ISEQ,1)*DT + D2FLDINF(ISEQ,1)*DT + D2PTHINF(ISEQ,1)*DT
  DGLBRIVOUT = DGLBRIVOUT + D2RIVOUT(ISEQ,1)*DT + D2FLDOUT(ISEQ,1)*DT + D2PTHOUT(ISEQ,1)*DT

  D2RIVSTO(ISEQ,1) = D2RIVSTO(ISEQ,1) + D2RIVINF(ISEQ,1)*DT - D2RIVOUT(ISEQ,1)*DT
  IF ( D2RIVSTO(ISEQ,1) < 0.D0 ) THEN
    D2FLDSTO(ISEQ,1) = D2FLDSTO(ISEQ,1) + D2RIVSTO(ISEQ,1)
    D2RIVSTO(ISEQ,1) = 0.D0
  ENDIF

  D2FLDSTO(ISEQ,1) = D2FLDSTO(ISEQ,1) + D2FLDINF(ISEQ,1)*DT - D2FLDOUT(ISEQ,1)*DT &
                                      + D2PTHINF(ISEQ,1)*DT - D2PTHOUT(ISEQ,1)*DT
  IF( D2FLDSTO(ISEQ,1) < 0.D0 )THEN
    D2RIVSTO(ISEQ,1)=MAX( D2RIVSTO(ISEQ,1)+D2FLDSTO(ISEQ,1), 0.D0 )
    D2FLDSTO(ISEQ,1)=0.D0
  ENDIF

  DGLBSTONXT = DGLBSTONXT + D2RIVSTO(ISEQ,1) + D2FLDSTO(ISEQ,1)
  D2OUTFLW(ISEQ,1)=D2RIVOUT(ISEQ,1)+D2FLDOUT(ISEQ,1)+D2PTHOUT(ISEQ,1)

  DRIVROF = ( D2RUNOFF(ISEQ,1)+D2GDWRTN(ISEQ,1) ) * ( 1.D0-D2FLDFRC(ISEQ,1) ) * DT
  DFLDROF = ( D2RUNOFF(ISEQ,1)+D2GDWRTN(ISEQ,1) ) *        D2FLDFRC(ISEQ,1)   * DT
  D2RIVSTO(ISEQ,1) = D2RIVSTO(ISEQ,1) + DRIVROF
  D2FLDSTO(ISEQ,1) = D2FLDSTO(ISEQ,1) + DFLDROF

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
!$ SAVE
INTEGER(KIND=JPIM)         ::  ISEQ
REAL(KIND=JPRB)            ::  ZMULGW
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
