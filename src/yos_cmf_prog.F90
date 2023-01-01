MODULE YOS_CMF_PROG
!==========================================================
!* PURPOSE: Shared prognostic variables for CaMa-Flood
! (C) D.Yamazaki & E. Dutra  (U-Tokyo/FCUL)  2Aug 2019
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
! Pointer was removed in v4.08 in order to keep simple codes when activating Single Precision Mode
!*** prognostics / state variables initial conditions
!REAL(KIND=JPRB),ALLOCATABLE,TARGET :: D2PROG(:,:,:)      !! Array to store 2D variables 
!INTEGER(KIND=JPIM)              :: ND2PROG
!
!REAL(KIND=JPRD),ALLOCATABLE,TARGET :: D2PROG_DBL(:,:,:)      !! Array to store 2D variables 
!INTEGER(KIND=JPIM)              :: ND2PROG_DBL

! Dammy variable for input/output
REAL(KIND=JPRD),ALLOCATABLE,TARGET     :: D2JPRD(:,:)       !! Dammy Array for Float64
REAL(KIND=JPRB),ALLOCATABLE,TARGET     :: B2JPRB(:,:)       !! Dammy Array for Float64/32 switch
REAL(KIND=JPRM),ALLOCATABLE,TARGET     :: R2JPRM(:,:)       !! Dammy Array for Float32

!================================================
!*** input runoff (interporlated)
REAL(KIND=JPRB),ALLOCATABLE,TARGET     :: B2RUNOFF(:,:)         !! input runoff             [m3/s]
REAL(KIND=JPRB),ALLOCATABLE,TARGET     :: B2ROFSUB(:,:)         !! input sub-surface runoff [m3/s]
REAL(KIND=JPRB),ALLOCATABLE,TARGET     :: B2WEVAP(:,:)          !! input Evaporation [m3/s]

!================================================
!*** river & floodpain
! storage variables are always in double precision
REAL(KIND=JPRD),ALLOCATABLE,TARGET     :: D2RIVSTO(:,:)         !! river      storage [m3]
REAL(KIND=JPRD),ALLOCATABLE,TARGET     :: D2FLDSTO(:,:)         !! floodplain storage [m3]

REAL(KIND=JPRB),ALLOCATABLE,TARGET     :: B2RIVOUT(:,:)         !! river      outflow [m3/s]
REAL(KIND=JPRB),ALLOCATABLE,TARGET     :: B2FLDOUT(:,:)         !! floodplain outflow [m3/s]

!================================================
!*** for implicit schemes of the local inertial equation
REAL(KIND=JPRB),ALLOCATABLE,TARGET     :: B2RIVOUT_PRE(:,:)     !! river      outflow [m3/s] (prev t-step)
REAL(KIND=JPRB),ALLOCATABLE,TARGET     :: B2RIVDPH_PRE(:,:)     !! river      depth   [m]    (prev t-step)
REAL(KIND=JPRB),ALLOCATABLE,TARGET     :: B2FLDOUT_PRE(:,:)     !! floodplain outflow [m3/s] (prev t-step)
REAL(KIND=JPRB),ALLOCATABLE,TARGET     :: B2FLDSTO_PRE(:,:)     !! floodplain storage [m3]   (prev t-step)

!================================================
!*** Groundwater Delay
REAL(KIND=JPRD),ALLOCATABLE,TARGET     :: B2GDWSTO(:,:)         !! ground water storage  [m3]
REAL(KIND=JPRB),ALLOCATABLE,TARGET     :: B2GDWRTN(:,:)         !! Ground water return flow [m3/s]

!================================================
!*** These have a different share, not part of the D2PROG array
REAL(KIND=JPRB),ALLOCATABLE,TARGET     :: B1PTHFLW(:,:)         !! flood path outflow [m3/s]
REAL(KIND=JPRB),ALLOCATABLE,TARGET     :: B1PTHFLW_PRE(:,:)     !! flood path outflow [m3/s] (prev t-step)

!================================================
!!!*** dam variables
REAL(KIND=JPRD),ALLOCATABLE,TARGET     :: D2DAMSTO(:,:)         !! reservoir storage [m3]
REAL(KIND=JPRB),ALLOCATABLE,TARGET     :: B2DAMINF(:,:)         !! reservoir inflow [m3/s]; discharge before operatio

!================================================
!!!*** levee variables
REAL(KIND=JPRD),ALLOCATABLE,TARGET     :: D2LEVSTO(:,:)         !! flood storage in protected side (storage betwen river & levee)

END MODULE YOS_CMF_PROG
