MODULE CMF_CALC_DIAG_MOD
!==========================================================
!* PURPOSE: Manage average and max diagnostic vars for output in CaMa-Flood
!
!* CONTAINS:
! -- CMF_DIAG_AVEMAX_OUTPUT   : Add / Max of diagnostic variables at time step
! -- CMF_DIAG_GETAVE_OUTPUT   : Calculate time-average of Diagnostic Variables
! -- CMF_DIAG_RESET_OUTPUT    : Reset Diagnostic Variables (Average & Maximum )
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
USE YOS_CMF_INPUT,      ONLY: DT, LPTHOUT,  LDAMOUT,  LWEVAP
USE YOS_CMF_MAP,        ONLY: NSEQALL,      NPTHOUT,      NPTHLEV
USE YOS_CMF_PROG,       ONLY: D2RIVOUT,     D2FLDOUT,     D1PTHFLW,     D2GDWRTN, &
                            & D2RUNOFF,     D2ROFSUB,     P2DAMINF
USE YOS_CMF_DIAG,       ONLY: D2OUTFLW,     D2RIVVEL,     D2PTHOUT,     D2PTHINF, &
                            & D2RIVDPH,     D2STORGE,     D2WEVAPEX
USE YOS_CMF_DIAG,       ONLY: D2RIVOUT_aAVG, D2FLDOUT_aAVG, D1PTHFLW_aAVG, D2GDWRTN_aAVG, D2RUNOFF_aAVG, D2ROFSUB_aAVG, &
                            & D2OUTFLW_aAVG, D2RIVVEL_aAVG, D2PTHOUT_aAVG, D2DAMINF_aAVG, D2WEVAPEX_aAVG, &
                            & D2OUTFLW_aMAX, D2RIVDPH_aMAX, D2STORGE_aMAX, NADD_adp,      D1PTHFLWSUM_aAVG
USE YOS_CMF_DIAG,       ONLY: D2RIVOUT_oAVG, D2FLDOUT_oAVG, D1PTHFLW_oAVG, D2GDWRTN_oAVG, D2RUNOFF_oAVG, D2ROFSUB_oAVG, &
                            & D2OUTFLW_oAVG, D2RIVVEL_oAVG, D2PTHOUT_oAVG, D2DAMINF_oAVG, D2WEVAPEX_oAVG, &
                            & D2OUTFLW_oMAX, D2RIVDPH_oMAX, D2STORGE_oMAX, NADD_out
#ifdef sediment
USE YOS_CMF_INPUT,      ONLY: LSEDOUT
USE yos_cmf_sed,        ONLY: d2rivout_sed, d2rivvel_sed, sadd_riv
#endif
IMPLICIT NONE
CONTAINS 
!####################################################################
! -- CMF_DIAG_AVEMAX_ADPSTP   : Add / Max of diagnostic variables within adaptive time step
! -- CMF_DIAG_GETAVE_ADPSTP   : Calculate time-average of Diagnostic Variables for adaptive steps
! -- CMF_DIAG_RESET_ADPSTP    : Reset Diagnostic Variables (Average & Maximum ) for adaptive steps
!
! -- CMF_DIAG_AVEMAX_OUTPUT   : Add / Max of diagnostic variables at time step for output time step
! -- CMF_DIAG_GETAVE_OUTPUT   : Calculate time-average of Diagnostic Variables for output
! -- CMF_DIAG_RESET_OUTPUT    : Reset Diagnostic Variables (Average & Maximum ) for output
!
!####################################################################
SUBROUTINE CMF_DIAG_RESET_ADPSTP
USE YOS_CMF_TIME,       ONLY: JYYYYMMDD, JHHMM
IMPLICIT NONE
!================================================
WRITE(LOGNAM,*) "CMF::DIAG_AVERAGE: reset", JYYYYMMDD, JHHMM
NADD_adp=0
D2RIVOUT_aAVG(:,:) = 0._JPRB
D2FLDOUT_aAVG(:,:) = 0._JPRB
D2OUTFLW_aAVG(:,:) = 0._JPRB
D2RIVVEL_aAVG(:,:) = 0._JPRB
D2PTHOUT_aAVG(:,:) = 0._JPRB
D2GDWRTN_aAVG(:,:) = 0._JPRB
D2RUNOFF_aAVG(:,:) = 0._JPRB
D2ROFSUB_aAVG(:,:) = 0._JPRB
IF ( LDAMOUT ) THEN
  D2DAMINF_aAVG(:,:)  = 0._JPRB
ENDIF
IF ( LWEVAP ) THEN
  D2WEVAPEX_aAVG(:,:) = 0._JPRB
ENDIF

D1PTHFLW_aAVG(:,:) = 0._JPRB 
D1PTHFLWSUM_aAVG(:)= 0._JPRB

D2STORGE_aMAX(:,:)=0._JPRB
D2OUTFLW_aMAX(:,:)=0._JPRB
D2RIVDPH_aMAX(:,:)=0._JPRB

END SUBROUTINE CMF_DIAG_RESET_ADPSTP
!####################################################################
!
!
!
!####################################################################
SUBROUTINE CMF_DIAG_AVEMAX_ADPSTP
IMPLICIT NONE
INTEGER(KIND=JPIM),SAVE  ::  ISEQ, IPTH
!====================
NADD_adp=NADD_adp+DT
!$OMP PARALLEL DO
DO ISEQ=1, NSEQALL
  D2RIVOUT_aAVG(ISEQ,1)=D2RIVOUT_aAVG(ISEQ,1)+D2RIVOUT(ISEQ,1)*DT
  D2FLDOUT_aAVG(ISEQ,1)=D2FLDOUT_aAVG(ISEQ,1)+D2FLDOUT(ISEQ,1)*DT
  D2RIVVEL_aAVG(ISEQ,1)=D2RIVVEL_aAVG(ISEQ,1)+D2RIVVEL(ISEQ,1)*DT
  D2OUTFLW_aAVG(ISEQ,1)=D2OUTFLW_aAVG(ISEQ,1)+D2OUTFLW(ISEQ,1)*DT

  D2PTHOUT_aAVG(ISEQ,1)=D2PTHOUT_aAVG(ISEQ,1)+D2PTHOUT(ISEQ,1)*DT-D2PTHINF(ISEQ,1)*DT

  D2GDWRTN_aAVG(ISEQ,1)=D2GDWRTN_aAVG(ISEQ,1)+D2GDWRTN(ISEQ,1)*DT
  D2RUNOFF_aAVG(ISEQ,1)=D2RUNOFF_aAVG(ISEQ,1)+D2RUNOFF(ISEQ,1)*DT
  D2ROFSUB_aAVG(ISEQ,1)=D2ROFSUB_aAVG(ISEQ,1)+D2ROFSUB(ISEQ,1)*DT

  D2OUTFLW_aMAX(ISEQ,1)=max( D2OUTFLW_aMAX(ISEQ,1), abs(D2OUTFLW(ISEQ,1)) )
  D2RIVDPH_aMAX(ISEQ,1)=max( D2RIVDPH_aMAX(ISEQ,1),     D2RIVDPH(ISEQ,1)  )
  D2STORGE_aMAX(ISEQ,1)=max( D2STORGE_aMAX(ISEQ,1),     D2STORGE(ISEQ,1)  )

  IF( LWEVAP )THEN
    D2WEVAPEX_aAVG(ISEQ,1)= D2WEVAPEX_aAVG(ISEQ,1) +D2WEVAPEX(ISEQ,1)*DT
  ENDIF
END DO
!$OMP END PARALLEL DO

!! loop for optional variable (separated for computational efficiency)
IF( LDAMOUT )THEN
  !$OMP PARALLEL DO
  DO ISEQ=1, NSEQALL
    D2DAMINF_aAVG(ISEQ,1)=D2DAMINF_aAVG(ISEQ,1)+P2DAMINF(ISEQ,1)*DT
  END DO
  !$OMP END PARALLEL DO
ENDIF

IF( LPTHOUT )THEN
  !$OMP PARALLEL DO
  DO IPTH=1, NPTHOUT
    D1PTHFLW_aAVG(IPTH,:)=D1PTHFLW_aAVG(IPTH,:)+D1PTHFLW(IPTH,:)*DT
  END DO
  !$OMP END PARALLEL DO
ENDIF

#ifdef sediment
!calculate average rivout and rivvel for sediment timestep
IF( LSEDOUT )THEN
  sadd_riv = sadd_riv + DT
  !$OMP PARALLEL DO
  DO ISEQ=1, NSEQALL
    d2rivout_sed(ISEQ) = d2rivout_sed(ISEQ)+D2RIVOUT(ISEQ,1)*DT
    d2rivvel_sed(ISEQ) = d2rivvel_sed(ISEQ)+D2RIVVEL(ISEQ,1)*DT
  END DO
  !$OMP END PARALLEL DO
ENDIF
#endif

END SUBROUTINE CMF_DIAG_AVEMAX_ADPSTP
!####################################################################
!
!
!
!####################################################################
SUBROUTINE CMF_DIAG_GETAVE_ADPSTP
USE YOS_CMF_TIME,       ONLY: JYYYYMMDD, JHHMM
IMPLICIT NONE
INTEGER(KIND=JPIM),SAVE  ::  ILEV
!================================================
WRITE(LOGNAM,*) "CMF::DIAG_AVERAGE: time-average", NADD_adp, JYYYYMMDD, JHHMM

D2RIVOUT_aAVG(:,:) = D2RIVOUT_aAVG(:,:) / DBLE(NADD_adp)
D2FLDOUT_aAVG(:,:) = D2FLDOUT_aAVG(:,:) / DBLE(NADD_adp)
D2OUTFLW_aAVG(:,:) = D2OUTFLW_aAVG(:,:) / DBLE(NADD_adp)
D2RIVVEL_aAVG(:,:) = D2RIVVEL_aAVG(:,:) / DBLE(NADD_adp)
D2PTHOUT_aAVG(:,:) = D2PTHOUT_aAVG(:,:) / DBLE(NADD_adp)
D2GDWRTN_aAVG(:,:) = D2GDWRTN_aAVG(:,:) / DBLE(NADD_adp)
D2RUNOFF_aAVG(:,:) = D2RUNOFF_aAVG(:,:) / DBLE(NADD_adp)
D2ROFSUB_aAVG(:,:) = D2ROFSUB_aAVG(:,:) / DBLE(NADD_adp)

IF ( LDAMOUT ) THEN
  D2DAMINF_aAVG(:,:)  = D2DAMINF_aAVG(:,:)  / DBLE(NADD_adp)
ENDIF
IF ( LWEVAP ) THEN
  D2WEVAPEX_aAVG(:,:) = D2WEVAPEX_aAVG(:,:) / DBLE(NADD_adp)
ENDIF

D1PTHFLW_aAVG(:,:) = D1PTHFLW_aAVG(:,:) / DBLE(NADD_adp)
DO ILEV=1, NPTHLEV
  D1PTHFLWSUM_aAVG(:)=D1PTHFLWSUM_aAVG(:)+D1PTHFLW_aAVG(:,ILEV)  !! bifurcation height layer summation
END DO

END SUBROUTINE CMF_DIAG_GETAVE_ADPSTP
!####################################################################
!++
!++
!++
!++
!####################################################################
SUBROUTINE CMF_DIAG_RESET_OUTPUT
USE YOS_CMF_TIME,       ONLY: JYYYYMMDD, JHHMM
IMPLICIT NONE
!================================================
WRITE(LOGNAM,*) "CMF::DIAG_AVERAGE: reset", JYYYYMMDD, JHHMM
NADD_out=0
D2RIVOUT_oAVG(:,:) = 0._JPRB
D2FLDOUT_oAVG(:,:) = 0._JPRB
D2OUTFLW_oAVG(:,:) = 0._JPRB
D2RIVVEL_oAVG(:,:) = 0._JPRB
D2PTHOUT_oAVG(:,:) = 0._JPRB
D2GDWRTN_oAVG(:,:) = 0._JPRB
D2RUNOFF_oAVG(:,:) = 0._JPRB
D2ROFSUB_oAVG(:,:) = 0._JPRB
IF ( LDAMOUT ) THEN
  D2DAMINF_oAVG(:,:)  = 0._JPRB
ENDIF
IF ( LWEVAP ) THEN
  D2WEVAPEX_oAVG(:,:) = 0._JPRB
ENDIF

D1PTHFLW_oAVG(:,:) = 0._JPRB 

D2STORGE_oMAX(:,:)=0._JPRB
D2OUTFLW_oMAX(:,:)=0._JPRB
D2RIVDPH_oMAX(:,:)=0._JPRB

END SUBROUTINE CMF_DIAG_RESET_OUTPUT
!####################################################################
!
!
!
!####################################################################
SUBROUTINE CMF_DIAG_AVEMAX_OUTPUT
IMPLICIT NONE
INTEGER(KIND=JPIM),SAVE  ::  ISEQ, IPTH
!====================
NADD_out=NADD_out+DT
!$OMP PARALLEL DO
DO ISEQ=1, NSEQALL
  D2RIVOUT_oAVG(ISEQ,1)=D2RIVOUT_oAVG(ISEQ,1)+D2RIVOUT_aAVG(ISEQ,1)*DT
  D2FLDOUT_oAVG(ISEQ,1)=D2FLDOUT_oAVG(ISEQ,1)+D2FLDOUT_aAVG(ISEQ,1)*DT
  D2RIVVEL_oAVG(ISEQ,1)=D2RIVVEL_oAVG(ISEQ,1)+D2RIVVEL_aAVG(ISEQ,1)*DT
  D2OUTFLW_oAVG(ISEQ,1)=D2OUTFLW_oAVG(ISEQ,1)+D2OUTFLW_aAVG(ISEQ,1)*DT

  D2PTHOUT_oAVG(ISEQ,1)=D2PTHOUT_oAVG(ISEQ,1)+D2PTHOUT_aAVG(ISEQ,1)*DT

  D2GDWRTN_oAVG(ISEQ,1)=D2GDWRTN_oAVG(ISEQ,1)+D2GDWRTN_aAVG(ISEQ,1)*DT
  D2RUNOFF_oAVG(ISEQ,1)=D2RUNOFF_oAVG(ISEQ,1)+D2RUNOFF_aAVG(ISEQ,1)*DT
  D2ROFSUB_oAVG(ISEQ,1)=D2ROFSUB_oAVG(ISEQ,1)+D2ROFSUB_aAVG(ISEQ,1)*DT

  D2OUTFLW_oMAX(ISEQ,1)=max( D2OUTFLW_oMAX(ISEQ,1), abs(D2OUTFLW_aMAX(ISEQ,1)) )
  D2RIVDPH_oMAX(ISEQ,1)=max( D2RIVDPH_oMAX(ISEQ,1),     D2RIVDPH_aMAX(ISEQ,1)  )
  D2STORGE_oMAX(ISEQ,1)=max( D2STORGE_oMAX(ISEQ,1),     D2STORGE_aMAX(ISEQ,1)  )

  IF( LWEVAP )THEN
    D2WEVAPEX_oAVG(ISEQ,1)= D2WEVAPEX_oAVG(ISEQ,1) +D2WEVAPEX_aAVG(ISEQ,1)*DT
  ENDIF
END DO
!$OMP END PARALLEL DO

!! loop for optional variable (separated for computational efficiency)
IF( LDAMOUT )THEN
  !$OMP PARALLEL DO
  DO ISEQ=1, NSEQALL
    D2DAMINF_oAVG(ISEQ,1)=D2DAMINF_oAVG(ISEQ,1)+D2DAMINF_aAVG(ISEQ,1)*DT
  END DO
  !$OMP END PARALLEL DO
ENDIF

IF( LPTHOUT )THEN
  !$OMP PARALLEL DO
  DO IPTH=1, NPTHOUT
    D1PTHFLW_oAVG(IPTH,:)=D1PTHFLW_oAVG(IPTH,:)+D1PTHFLW_aAVG(IPTH,:)*DT
  END DO
  !$OMP END PARALLEL DO
ENDIF

END SUBROUTINE CMF_DIAG_AVEMAX_OUTPUT
!####################################################################
!
!
!
!####################################################################
SUBROUTINE CMF_DIAG_GETAVE_OUTPUT
USE YOS_CMF_TIME,       ONLY: JYYYYMMDD, JHHMM
IMPLICIT NONE
!================================================
WRITE(LOGNAM,*) "CMF::DIAG_AVERAGE: time-average", NADD_out, JYYYYMMDD, JHHMM

D2RIVOUT_oAVG(:,:) = D2RIVOUT_oAVG(:,:) / DBLE(NADD_out)
D2FLDOUT_oAVG(:,:) = D2FLDOUT_oAVG(:,:) / DBLE(NADD_out)
D2OUTFLW_oAVG(:,:) = D2OUTFLW_oAVG(:,:) / DBLE(NADD_out)
D2RIVVEL_oAVG(:,:) = D2RIVVEL_oAVG(:,:) / DBLE(NADD_out)
D2PTHOUT_oAVG(:,:) = D2PTHOUT_oAVG(:,:) / DBLE(NADD_out)
D2GDWRTN_oAVG(:,:) = D2GDWRTN_oAVG(:,:) / DBLE(NADD_out)
D2RUNOFF_oAVG(:,:) = D2RUNOFF_oAVG(:,:) / DBLE(NADD_out)
D2ROFSUB_oAVG(:,:) = D2ROFSUB_oAVG(:,:) / DBLE(NADD_out)

IF ( LDAMOUT ) THEN
  D2DAMINF_oAVG(:,:)  = D2DAMINF_oAVG(:,:)  / DBLE(NADD_out)
ENDIF
IF ( LWEVAP ) THEN
  D2WEVAPEX_oAVG(:,:) = D2WEVAPEX_oAVG(:,:) / DBLE(NADD_out)
ENDIF

D1PTHFLW_oAVG(:,:) = D1PTHFLW_oAVG(:,:) / DBLE(NADD_out)

END SUBROUTINE CMF_DIAG_GETAVE_OUTPUT
!####################################################################



END MODULE CMF_CALC_DIAG_MOD
