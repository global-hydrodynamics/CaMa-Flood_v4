MODULE CMF_CALC_DIAG_MOD
!==========================================================
!* PURPOSE: Manage prognostic/diagnostic variables in CaMa-Flood
!
!* CONTAINS:
! -- CMF_PROG_INIT      : Initialize Prognostic variables (include restart data handling)
! -- CMF_DIAG_INIT      : Initialize Diagnostic variables
! -- CMF_DIAG_AVERAGE   : Calculate time-average of Diagnostic Variables
! -- CMF_DIAG_RESET     : Reset Diagnostic Variables (Average & Maximum )
!!
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
USE PARKIND1,           ONLY: JPIM, JPRM, JPRB
USE YOS_CMF_INPUT,      ONLY: LOGNAM
IMPLICIT NONE
CONTAINS 
!####################################################################
! -- CMF_DIAG_AVE_MAX   : Add / Max of diagnostic variables at time step
! -- CMF_DIAG_AVERAGE   : Calculate time-average of Diagnostic Variables
! -- CMF_DIAG_RESET     : Reset Diagnostic Variables (Average & Maximum )
!
!####################################################################
SUBROUTINE CMF_DIAG_AVEMAX
USE YOS_CMF_INPUT,      ONLY: DT, LPTHOUT,  LDAMOUT,  LWEVAP
USE YOS_CMF_MAP,        ONLY: NSEQALL,      NPTHOUT
USE YOS_CMF_PROG,       ONLY: B2RIVOUT,     B2FLDOUT,     B1PTHFLW,     B2GDWRTN, &
                            & B2RUNOFF,     B2ROFSUB,     B2DAMINF
USE YOS_CMF_DIAG,       ONLY: B2OUTFLW,     B2RIVVEL,     B2PTHOUT,     B2PTHINF, &
                            & B2RIVDPH,     B2STORGE,     B2WEVAPEX,    NADD, &
                            & B2RIVOUT_AVG, B2FLDOUT_AVG, B1PTHFLW_AVG, B2GDWRTN_AVG, B2RUNOFF_AVG, B2ROFSUB_AVG, &
                            & B2OUTFLW_AVG, B2RIVVEL_AVG, B2PTHOUT_AVG, B2DAMINF_AVG, B2WEVAPEX_AVG, &
                            & B2OUTFLW_MAX, B2RIVDPH_MAX, B2STORGE_MAX
#ifdef sediment
USE YOS_CMF_INPUT,      ONLY: LSEDOUT
USE yos_cmf_sed,        ONLY: b2rivout_sed, b2rivvel_sed, sadd_riv
#endif

IMPLICIT NONE
INTEGER(KIND=JPIM),SAVE  ::  ISEQ, IPTH
!====================
NADD=NADD+DT
!$OMP PARALLEL DO
DO ISEQ=1, NSEQALL
  B2RIVOUT_AVG(ISEQ,1)=B2RIVOUT_AVG(ISEQ,1)+B2RIVOUT(ISEQ,1)*DT
  B2FLDOUT_AVG(ISEQ,1)=B2FLDOUT_AVG(ISEQ,1)+B2FLDOUT(ISEQ,1)*DT
  B2RIVVEL_AVG(ISEQ,1)=B2RIVVEL_AVG(ISEQ,1)+B2RIVVEL(ISEQ,1)*DT
  B2OUTFLW_AVG(ISEQ,1)=B2OUTFLW_AVG(ISEQ,1)+B2OUTFLW(ISEQ,1)*DT

  B2PTHOUT_AVG(ISEQ,1)=B2PTHOUT_AVG(ISEQ,1)+B2PTHOUT(ISEQ,1)*DT-B2PTHINF(ISEQ,1)*DT

  B2GDWRTN_AVG(ISEQ,1)=B2GDWRTN_AVG(ISEQ,1)+B2GDWRTN(ISEQ,1)*DT
  B2RUNOFF_AVG(ISEQ,1)=B2RUNOFF_AVG(ISEQ,1)+B2RUNOFF(ISEQ,1)*DT
  B2ROFSUB_AVG(ISEQ,1)=B2ROFSUB_AVG(ISEQ,1)+B2ROFSUB(ISEQ,1)*DT

  B2OUTFLW_MAX(ISEQ,1)=max( B2OUTFLW_MAX(ISEQ,1), abs(B2OUTFLW(ISEQ,1)) )
  B2RIVDPH_MAX(ISEQ,1)=max( B2RIVDPH_MAX(ISEQ,1),     B2RIVDPH(ISEQ,1)  )
  B2STORGE_MAX(ISEQ,1)=max( B2STORGE_MAX(ISEQ,1),     B2STORGE(ISEQ,1)  )

  IF( LWEVAP )THEN
    B2WEVAPEX_AVG(ISEQ,1)= B2WEVAPEX_AVG(ISEQ,1) +B2WEVAPEX(ISEQ,1)*DT
  ENDIF
END DO
!$OMP END PARALLEL DO

!! loop for optional variable (separated for computational efficiency)
IF( LDAMOUT )THEN
  !$OMP PARALLEL DO
  DO ISEQ=1, NSEQALL
    B2DAMINF_AVG(ISEQ,1)=B2DAMINF_AVG(ISEQ,1)+B2DAMINF(ISEQ,1)*DT
  END DO
  !$OMP END PARALLEL DO
ENDIF

IF( LPTHOUT )THEN
  !$OMP PARALLEL DO
  DO IPTH=1, NPTHOUT
    B1PTHFLW_AVG(IPTH,:)=B1PTHFLW_AVG(IPTH,:)+B1PTHFLW(IPTH,:)*DT
  END DO
  !$OMP END PARALLEL DO
ENDIF

#ifdef sediment
!calculate average rivout and rivvel for sediment timestep
IF( LSEDOUT )THEN
  sadd_riv = sadd_riv + DT
  !$OMP PARALLEL DO
  DO ISEQ=1, NSEQALL
    b2rivout_sed(ISEQ) = b2rivout_sed(ISEQ)+B2RIVOUT(ISEQ,1)*DT
    b2rivvel_sed(ISEQ) = b2rivvel_sed(ISEQ)+B2RIVVEL(ISEQ,1)*DT
  END DO
  !$OMP END PARALLEL DO
ENDIF
#endif

END SUBROUTINE CMF_DIAG_AVEMAX
!####################################################################





!####################################################################
SUBROUTINE CMF_DIAG_AVERAGE
USE YOS_CMF_TIME,       ONLY: JYYYYMMDD, JHHMM
USE YOS_CMF_DIAG,       ONLY: B2DIAG_AVG, B1PTHFLW_AVG, NADD
IMPLICIT NONE
!================================================
WRITE(LOGNAM,*) "CMF::DIAG_AVERAGE: time-average", NADD, JYYYYMMDD, JHHMM
B2DIAG_AVG(:,:,:) = B2DIAG_AVG(:,:,:) /DBLE(NADD)
B1PTHFLW_AVG(:,:) = B1PTHFLW_AVG(:,:) /DBLE(NADD)
END SUBROUTINE CMF_DIAG_AVERAGE
!####################################################################





!####################################################################
SUBROUTINE CMF_DIAG_RESET
USE YOS_CMF_TIME,       ONLY: JYYYYMMDD, JHHMM
USE YOS_CMF_DIAG,       ONLY: B2DIAG_AVG, B1PTHFLW_AVG, B2DIAG_MAX, NADD
IMPLICIT NONE
!================================================
WRITE(LOGNAM,*) "CMF::DIAG_AVERAGE: reset", JYYYYMMDD, JHHMM
NADD=0
B2DIAG_AVG(:,:,:) = 0._JPRB
B1PTHFLW_AVG(:,:) = 0._JPRB 
B2DIAG_MAX(:,:,:) = 0._JPRB
END SUBROUTINE CMF_DIAG_RESET
!####################################################################

END MODULE CMF_CALC_DIAG_MOD
