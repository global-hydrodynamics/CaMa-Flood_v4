MODULE CMF_DRV_ADVANCE_MOD
!==========================================================
!* PURPOSE: Advance CaMa-Flood time integration  
!
!* CONTAINS:
! -- CMF_DRV_ADVANCE : Advance integration for KSPETS (given as argument)
!
!* INTERFACE:
! -- Called from "Main Program" or "Coupler"
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
USE YOS_CMF_INPUT,           ONLY: LOGNAM
IMPLICIT NONE
CONTAINS
!####################################################################
! -- CMF_DRV_ADVANCE : Advance integration for KSPETS
!
!
!####################################################################
SUBROUTINE CMF_DRV_ADVANCE(KSTEPS)
USE YOS_CMF_INPUT,           ONLY: LOUTPUT, LSEALEV, IFRQ_OUT
USE YOS_CMF_TIME,            ONLY: KSTEP, JYYYYMMDD, JHHMM, JHOUR, JMIN
!
USE CMF_CTRL_TIME_MOD,       ONLY: CMF_TIME_NEXT, CMF_TIME_UPDATE
USE CMF_CTRL_PHYSICS_MOD,    ONLY: CMF_PHYSICS_ADVANCE, CMF_PHYSICS_FLDSTG
USE CMF_CTRL_RESTART_MOD,    ONLY: CMF_RESTART_WRITE
USE CMF_CTRL_OUTPUT_MOD,     ONLY: CMF_OUTPUT_WRITE, CMF_OUTTXT_WRTE
USE CMF_CTRL_DAMOUT_MOD,     ONLY: CMF_DAMOUT_WRTE

USE CMF_CALC_DIAG_MOD,       ONLY: CMF_DIAG_AVERAGE, CMF_DIAG_RESET
USE CMF_CTRL_BOUNDARY_MOD,   ONLY: CMF_BOUNDARY_UPDATE
#ifdef sediment
USE YOS_CMF_INPUT,           ONLY: LSEDOUT
USE yos_cmf_sed,             ONLY: step_sed
USE cmf_ctrl_sedout_mod,     ONLY: cmf_sed_output
USE cmf_calc_sedflw_mod,     ONLY: cmf_calc_sedflw
#endif
!$ USE OMP_LIB
IMPLICIT NONE 
SAVE
! Input argument 
INTEGER(KIND=JPIM)              :: KSTEPS             !! Number of timesteps to advance 
!* Local variables 
INTEGER(KIND=JPIM)              :: ISTEP              !! Time Step
REAL(KIND=JPRB)                 :: ZTT0, ZTT1, ZTT2   !! Time elapsed related 
!$ INTEGER(KIND=JPIM)           :: NTHREADS           !! OpenMP thread number
!==========================================================

!*** get OMP thread number
!$OMP PARALLEL
!$ NTHREADS=OMP_GET_MAX_THREADS()
!$OMP END PARALLEL 

!================================================
!*** START: time step loop
DO ISTEP=1,KSTEPS
  !============================
  !*** 0. get start CPU time
  CALL CPU_TIME(ZTT0)
  !$ ZTT0=OMP_GET_WTIME()

  !============================
  !*** 1. Set next time
  CALL CMF_TIME_NEXT               !! set KMINNEXT, JYYYYMMDD, JHHMM

  !*** (optional)
  IF( LSEALEV )THEN
    CALL CMF_BOUNDARY_UPDATE
  ENDIF

  !============================
  !*** 2. Advance model integration 
  CALL CMF_PHYSICS_ADVANCE

#ifdef sediment
  !*** 2b.  Advance sediment model integration
  IF( LSEDOUT .and. MOD(KSTEP,step_sed)==0 )THEN
    CALL cmf_calc_sedflw
  ENDIF
#endif

  CALL CPU_TIME(ZTT1)
  !$ ZTT1=OMP_GET_WTIME()

  !============================
  !*** 3. Write output file (when needed)
  IF( LOUTPUT .and. MOD(JHOUR,IFRQ_OUT)==0 .and. JMIN==0 )then
    !*** average variable
    CALL CMF_DIAG_AVERAGE

    !*** write output data
    CALL CMF_OUTPUT_WRITE

#ifdef sediment
    IF ( LSEDOUT ) THEN
      CALL cmf_sed_output
    ENDIF
#endif

    ! --- Optional: text file output
    CALL CMF_OUTTXT_WRTE            !! reservoir operation
    CALL CMF_DAMOUT_WRTE            !! reservoir operation


    !*** reset variable
    CALL CMF_DIAG_RESET
  ENDIF

  !============================ 
  !*** 4. Write restart file 
  CALL CMF_RESTART_WRITE

  !============================ 
  !*** 5. Update current time      !! Update KMIN, IYYYYMMDD, IHHMM (to KMINNEXT, JYYYYMMDD, JHHMM)
  CALL CMF_TIME_UPDATE

  !============================
  !*** 6. Check CPU time 
  CALL CPU_TIME(ZTT2)
  !$ ZTT2=OMP_GET_WTIME()
  WRITE(LOGNAM,*) "CMF::DRV_ADVANCE END: KSTEP, time (end of Tstep):", KSTEP, JYYYYMMDD, JHHMM
  WRITE(LOGNAM,'(a,f8.1,a,f8.1,a)') "Elapsed cpu time", ZTT2-ZTT0,"Sec. // File output ", ZTT2-ZTT1, "Sec"

ENDDO
!*** END:time step loop
!================================================

END SUBROUTINE CMF_DRV_ADVANCE
!####################################################################

END MODULE CMF_DRV_ADVANCE_MOD
