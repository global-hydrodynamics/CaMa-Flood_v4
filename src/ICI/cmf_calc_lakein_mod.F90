MODULE CMF_CALC_LAKEIN_MOD
!==========================================================
!* PURPOSE: Lake-river coupling in ILS
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
USE PARKIND1,                ONLY: JPIM, JPRM, JPRB
CONTAINS
!####################################################################
! -- CMF_CALC_LAKEIN
! -- CMF_LAKEIN_AVE
! -- CMF_LAKEIN_AVERAGE
! -- CMF_RESET_LAKEIN
!####################################################################
SUBROUTINE CMF_CALC_LAKEIN
USE YOS_CMF_INPUT,      ONLY: DT
USE YOS_CMF_MAP,        ONLY: NSEQALL, B2GRAREA
USE YOS_CMF_PROG,       ONLY: D2RIVSTO, D2FLDSTO
USE YOS_CMF_DIAG,       ONLY: B2STORGE
USE YOS_CMF_ICI,        ONLY: B2LAKEFRC, B2RUNIN
IMPLICIT NONE
!*** Local
INTEGER(KIND=JPIM)         :: ISEQ
REAL(KIND=JPRB)            :: BRIVRIN, BFLDRIN
!*** Lake Parameter
REAL(KIND=JPRB)            :: RINDMP
!================================================
RINDMP = 1._JPRB

DO ISEQ=1, NSEQALL
  BRIVRIN = D2RIVSTO(ISEQ,1) * B2LAKEFRC(ISEQ,1) / (RINDMP * 8.64D4) * DT !! m3
  BFLDRIN = D2FLDSTO(ISEQ,1) * B2LAKEFRC(ISEQ,1) / (RINDMP * 8.64D4) * DT !! m3
  D2RIVSTO(ISEQ,1) = D2RIVSTO(ISEQ,1) - BRIVRIN
  D2FLDSTO(ISEQ,1) = D2FLDSTO(ISEQ,1) - BFLDRIN
  B2RUNIN(ISEQ,1) = (BRIVRIN + BFLDRIN) / DT / B2GRAREA(ISEQ,1) * 1.D3 !! kg/m2/s

  B2STORGE(ISEQ,1)=D2RIVSTO(ISEQ,1)+D2FLDSTO(ISEQ,1)
ENDDO

END SUBROUTINE CMF_CALC_LAKEIN
!####################################################################






!####################################################################
SUBROUTINE CMF_LAKEIN_AVE
USE YOS_CMF_INPUT,      ONLY: DT
USE YOS_CMF_MAP,        ONLY: NSEQALL
USE YOS_CMF_ICI,        ONLY: B2RUNIN, B2RUNIN_AVG
IMPLICIT NONE
!*** Local
INTEGER(KIND=JPIM)         :: ISEQ
!================================================
DO ISEQ=1, NSEQALL
  B2RUNIN_AVG(ISEQ,1)=B2RUNIN_AVG(ISEQ,1)+B2RUNIN(ISEQ,1)*DT
END DO

END SUBROUTINE CMF_LAKEIN_AVE
!####################################################################





!####################################################################
SUBROUTINE CMF_LAKEIN_AVERAGE
USE YOS_CMF_DIAG,       ONLY: NADD
USE YOS_CMF_MAP,        ONLY: NSEQALL
USE YOS_CMF_ICI,        ONLY: B2RUNIN_AVG
IMPLICIT NONE
!*** Local
INTEGER(KIND=JPIM)         :: ISEQ
!================================================
DO ISEQ=1, NSEQALL
  B2RUNIN_AVG(ISEQ,1)=B2RUNIN_AVG(ISEQ,1)/DBLE(NADD)
END DO

END SUBROUTINE CMF_LAKEIN_AVERAGE
!####################################################################





!####################################################################
SUBROUTINE CMF_RESET_LAKEIN
USE YOS_CMF_ICI,        ONLY: B2RUNIN_AVG
IMPLICIT NONE
!================================================
B2RUNIN_AVG(:,:)=0._JPRB

END SUBROUTINE CMF_RESET_LAKEIN
!####################################################################

END MODULE CMF_CALC_LAKEIN_MOD
