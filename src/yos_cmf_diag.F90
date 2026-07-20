MODULE YOS_CMF_DIAG
!==========================================================
!* PURPOSE: Shared diagnostic variables for CaMa-Flood
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
USE PARKIND1, ONLY: JPIM, JPRB, JPRM, JPRD
IMPLICIT NONE
SAVE
!================================================
! Pointer was removed in v4.12 in order to keep simple codes when activating Single Precision Mode
!*** prognostics / state variables initial conditions

!*** Inst. diagnostics 
REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2RIVINF(:,:)           !! river      inflow   [m3/s] (from upstream)
REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2RIVDPH(:,:)           !! river      depth    [m]
REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2RIVVEL(:,:)           !! flow velocity       [m/s]

REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2FLDINF(:,:)           !! floodplain inflow   [m3/s]
REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2FLDDPH(:,:)           !! floodplain depth    [m]
REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2FLDFRC(:,:)           !! flooded    fractipn [m2/m2]
REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2FLDARE(:,:)           !! flooded    area     [m2]

REAL(KIND=JPRB),ALLOCATABLE                :: D1PTHFLWSUM(:)          !! bifurcation flow (1D, not 2D variable), all layer sum
REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2PTHOUT(:,:)           !! flood path outflow   [m3/s]
REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2PTHINF(:,:)           !! flood path inflow   [m3/s]

REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2SFCELV(:,:)           !! water surface elev  [m]    (elevtn - rivhgt + rivdph)
REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2OUTFLW(:,:)           !! total outflow       [m3/s] (rivout + fldout)
REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2STORGE(:,:)           !! total storage       [m3]   (rivsto + fldsto)

REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2OUTINS(:,:)           !! instantaneous discharge [m3/s] (unrouted runoff)
REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2WEVAPEX(:,:)          !! Evaporation water extracted
!================================================

!** local temporal variables in subroutine
REAL(KIND=JPRB),ALLOCATABLE                :: D2SFCELV_PRE(:,:)       
REAL(KIND=JPRB),ALLOCATABLE                :: D2DWNELV_PRE(:,:)   
REAL(KIND=JPRB),ALLOCATABLE                :: D2FLDDPH_PRE(:,:)   

REAL(KIND=JPRD),ALLOCATABLE                :: P2STOOUT(:,:)
REAL(KIND=JPRD),ALLOCATABLE                :: P2RIVINF(:,:)
REAL(KIND=JPRD),ALLOCATABLE                :: P2FLDINF(:,:)
REAL(KIND=JPRD),ALLOCATABLE                :: P2PTHOUT(:,:)
REAL(KIND=JPRB),ALLOCATABLE                :: D2RATE(:,:)

!================================================
!*** Average diagnostics for adaptive time step
REAL(KIND=JPRB)                            :: NADD_adp                    !! sum DT to calculate average

REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2RIVOUT_aAVG(:,:)       !! average river       discharge
REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2OUTFLW_aAVG(:,:)       !! average total outflow [m3/s] (rivout + fldout)
REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2FLDOUT_aAVG(:,:)       !! average floodplain  discharge
REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2RIVVEL_aAVG(:,:)       !! average flow velocity
REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2PTHOUT_aAVG(:,:)       !! flood pathway net outflow (2D)

REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2GDWRTN_aAVG(:,:)       !! average ground water return flow
REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2RUNOFF_aAVG(:,:)       !! average input runoff
REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2ROFSUB_aAVG(:,:)       !! average input sub-surface runoff
REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2WEVAPEX_aAVG(:,:)      !! average extracted water evaporation

REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2DAMINF_aAVG(:,:)       !! average reservoir inflow [m3/s]  !!!added

!*** Average diagnostics (1D) for output
REAL(KIND=JPRB),ALLOCATABLE                :: D1PTHFLW_aAVG(:,:)       !! bifurcation flow (1D, not 2D variable)
REAL(KIND=JPRB),ALLOCATABLE                :: D1PTHFLWSUM_aAVG(:)      !! bifurcation flow (1D, not 2D variable), all layer sum

!*** Daily max diagnostics for output
REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2OUTFLW_aMAX(:,:)       !! max total outflow       [m3/s] (rivout + fldout)
REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2STORGE_aMAX(:,:)       !! max total outflow       [m3/s] (rivout + fldout)
REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2RIVDPH_aMAX(:,:)       !! max total outflow       [m3/s] (rivout + fldout)

!================================================
!*** Average diagnostics for output
REAL(KIND=JPRB)                            :: NADD_out                    !! sum DT to calculate average

REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2RIVOUT_oAVG(:,:)       !! average river       discharge
REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2OUTFLW_oAVG(:,:)       !! average total outflow [m3/s] (rivout+fldout) 
REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2FLDOUT_oAVG(:,:)       !! average floodplain  discharge
REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2RIVVEL_oAVG(:,:)       !! average flow velocity
REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2PTHOUT_oAVG(:,:)       !! flood pathway net outflow (2D)

REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2GDWRTN_oAVG(:,:)       !! average ground water return flow
REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2RUNOFF_oAVG(:,:)       !! average input runoff
REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2ROFSUB_oAVG(:,:)       !! average input sub-surface runoff
REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2WEVAPEX_oAVG(:,:)      !! average extracted water evaporation

REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2DAMINF_oAVG(:,:)       !! average reservoir inflow [m3/s]  !!!added

!*** Average diagnostics (1D) for output
REAL(KIND=JPRB),ALLOCATABLE                :: D1PTHFLW_oAVG(:,:)          !! bifurcation channel flow (1D, not 2D variable)

!*** Daily max diagnostics for output
REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2OUTFLW_oMAX(:,:)       !! max total outflow       [m3/s] (rivout + fldout)
REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2STORGE_oMAX(:,:)       !! max total outflow       [m3/s] (rivout + fldout)
REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2RIVDPH_oMAX(:,:)       !! max total outflow       [m3/s] (rivout + fldout)


!================================================
!*** Global total
! discharge calculation budget
REAL(KIND=JPRD)                 :: P0GLBSTOPRE              !! global water storage      [m3] (befre flow calculation)
REAL(KIND=JPRD)                 :: P0GLBSTONXT              !! global water storage      [m3] (after flow calculation)
REAL(KIND=JPRD)                 :: P0GLBSTONEW              !! global water storage      [m3] (after runoff input)
REAL(KIND=JPRD)                 :: P0GLBRIVINF              !! global inflow             [m3] (rivinf + fldinf)
REAL(KIND=JPRD)                 :: P0GLBRIVOUT              !! global outflow            [m3] (rivout + fldout)

! stage calculation budget
REAL(KIND=JPRD)                 :: P0GLBSTOPRE2             !! global water storage      [m3] (befre stage calculation)
REAL(KIND=JPRD)                 :: P0GLBSTONEW2             !! global water storage      [m3] (after stage calculation)
REAL(KIND=JPRD)                 :: P0GLBRIVSTO              !! global river storage      [m3]
REAL(KIND=JPRD)                 :: P0GLBFLDSTO              !! global floodplain storage [m3]
REAL(KIND=JPRD)                 :: P0GLBLEVSTO              !! global protected-side storage [m3] (levee scheme)
REAL(KIND=JPRD)                 :: P0GLBFLDARE              !! global flooded area       [m2]

!================================================
!!!*** levee variables
REAL(KIND=JPRB),ALLOCATABLE,TARGET         :: D2LEVDPH(:,:)  !! flood depth in protected side (water depth betwen river & levee)



END MODULE YOS_CMF_DIAG
