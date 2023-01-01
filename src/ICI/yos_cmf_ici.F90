MODULE YOS_CMF_ICI
!==========================================================
!* PURPOSE: Shared variables for CaMa-Flood in ILS
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
USE PARKIND1, ONLY: JPRB
IMPLICIT NONE
SAVE
!================================================
LOGICAL                     :: LLAKEIN
REAL(KIND=JPRB),ALLOCATABLE :: D2LAKEFRC(:,:)
REAL(KIND=JPRB),ALLOCATABLE :: D2RUNIN(:,:)
REAL(KIND=JPRB),ALLOCATABLE :: D2RUNIN_AVG(:,:)

!================================================
END MODULE YOS_CMF_ICI
