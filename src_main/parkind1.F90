MODULE PARKIND1
!==========================================================
!* PURPOSE: Define usual kinds for strong typing
! (C) E. Dutra  (FCUL)  Aug 2019
!
! Licensed under the Apache License, Version 2.0 (the "License");
!   You may not use this file except in compliance with the License.
!   You may obtain a copy of the License at: http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software distributed under the License is 
!  distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
! See the License for the specific language governing permissions and limitations under the License.
!==========================================================
IMPLICIT NONE
SAVE
!================================================
!*** Integer Kinds
INTEGER, PARAMETER :: JPIT = SELECTED_INT_KIND(2)
INTEGER, PARAMETER :: JPIS = SELECTED_INT_KIND(4)
INTEGER, PARAMETER :: JPIM = SELECTED_INT_KIND(9)  !! 4 byte integer
INTEGER, PARAMETER :: JPIB = SELECTED_INT_KIND(12) !! 8 byte long integer
!Special integer type to be used for sensative adress calculations
!should be *8 for a machine with 8byte adressing for optimum performance
#ifdef ADDRESS64
INTEGER, PARAMETER :: JPIA = JPIB
#else
INTEGER, PARAMETER :: JPIA = JPIM
#endif
!================================================
!*** Real Kinds
INTEGER, PARAMETER :: JPRT = SELECTED_REAL_KIND(2,1)
INTEGER, PARAMETER :: JPRS = SELECTED_REAL_KIND(4,2)
INTEGER, PARAMETER :: JPRM = SELECTED_REAL_KIND(6,37)  !! 4 byte float
#ifdef SinglePrec_CMF
INTEGER, PARAMETER :: JPRB = SELECTED_REAL_KIND(6,37)  !! JPRB is switchable (4 byte in Single Precision Mode)
#else
INTEGER, PARAMETER :: JPRB = SELECTED_REAL_KIND(13,300)
#endif
! Double real for C code and special places requiring 
!    higher precision. 
INTEGER, PARAMETER :: JPRD = SELECTED_REAL_KIND(13,300)  !! 8 byte double-precison float (primary used for precise water budget)

!================================================
! Logical Kinds for RTTOV....
INTEGER, PARAMETER :: JPLM = JPIM   !Standard logical type

END MODULE PARKIND1
