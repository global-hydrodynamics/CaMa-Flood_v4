MODULE CMF_CTRL_ICI_MOD
!==========================================================
!* PURPOSE: manage ICI coupler for CaMa-Flood
!
!* CONTAINS:
! -- CMF_ICI_INPUT          : Read setting from Namelist
! -- CMF_ICI_INIT           : Initialize ICI coupler
! -- CMF_ICI_FORCING_GET    : Update time, read forcing data from file and convert unit
! -- CMF_ICI_OUTPUT         : Send output to ICI
! -- CMF_ICI_END            : Finalize   ICI coupler
!
! (C) M.Hatono and D.Yamazaki (Tohoku-U, U-Tokyo)  Sep 2019
!
! Licensed under the Apache License, Version 2.0 (the "License");
!   You may not use this file except in compliance with the License.
!   You may obtain a copy of the License at: http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software distributed under the License is
!  distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and limitations under the License.
!==========================================================
! shared variables in module
USE mpi
USE PARKIND1,                ONLY: JPIM, JPRB
USE YOS_CMF_INPUT,           ONLY: LOGNAM
USE YOS_CMF_ICI,             ONLY: LLAKEIN
USE palmtime,                ONLY: palm_TimeInit, palm_TimeStart, palm_TimeEnd, palm_TimeFinalize
IMPLICIT NONE
SAVE
!*** NAMELIST/cama_ici/
CHARACTER(LEN=256)              ::  my_comp
CHARACTER(LEN=256)              ::  my_grid
CHARACTER(LEN=256)              ::  namelist_ici_file
INTEGER(KIND=JPIM)              ::  intpl_num
NAMELIST/cama_ici/  my_comp, my_grid, namelist_ici_file, intpl_num

!*** NAMELIST/cama_ici_intpl/
CHARACTER(LEN=256)              ::  send_comp
CHARACTER(LEN=256)              ::  send_grid
CHARACTER(LEN=256)              ::  recv_comp
CHARACTER(LEN=256)              ::  recv_grid
CHARACTER(LEN=256)              ::  map_file
CHARACTER(LEN=256)              ::  intpl_file
INTEGER(KIND=JPIM)              ::  intpl_map

NAMELIST/cama_ici_intpl/  send_comp, send_grid, recv_comp, recv_grid, intpl_map, map_file, intpl_file

!*** NAMELIST/cama_ici_force/
REAL(KIND=JPRB)                 ::  mrofunit
NAMELIST/cama_ici_force/  mrofunit

!*** NAMELIST/cama_ici_lake/
LOGICAL                         :: nm_llakein = .true.
NAMELIST/cama_ici_lake/  nm_llakein

! local variables
INTEGER(KIND=JPIM)              :: time_array(6) = (/2000,1,1,0,0,0/)          ! simulation time step

REAL(KIND=JPRB)                 :: ZTT0, ZTT1, ZTT2                            ! Time elapsed related
INTEGER(KIND=JPIM)              :: ierr, Nproc, Nid, my_comm                   ! MPI related
!==========================================================
CONTAINS
!####################################################################
! -- CMF_ICI_INPUT      : Read setting from Namelist
! -- CMF_ICI_INIT        : Initialize ICI coupler
! -- CMF_ICI_END         : Finalize   ICI coupler
!
!####################################################################
SUBROUTINE CMF_ICI_INPUT
! Set ici namelist
USE YOS_CMF_INPUT,           ONLY: NSETFILE, CLOGOUT
USE YOS_CMF_MAP,             ONLY: REGIONALL,REGIONTHIS, MPI_COMM_CAMA
USE CMF_UTILS_MOD,           ONLY: INQUIRE_FID
USE ici_api,                 ONLY: ici_init, ici_get_numpe_local, ici_get_irank_local, ici_get_comm_local
IMPLICIT NONE
!* local variables
!================================================
NSETFILE=INQUIRE_FID()
OPEN(NSETFILE,FILE='input_cmf.nam',STATUS="OLD")
REWIND(NSETFILE)
READ(NSETFILE,NML=cama_ici)
REWIND(NSETFILE)
READ(NSETFILE,NML=cama_ici_force)
REWIND(NSETFILE)
READ(NSETFILE,NML=cama_ici_lake)
LLAKEIN = nm_llakein
CLOSE(NSETFILE)

!*** Initialize MPI
CALL MPI_Init(ierr)
CALL ici_init(my_comp, namelist_ici_file)
CALL palm_TimeInit("CAMA",comm=ici_get_comm_local())

Nproc = ici_get_numpe_local()
Nid   = ici_get_irank_local()
MPI_COMM_CAMA = ici_get_comm_local()
REGIONALL = Nproc
REGIONTHIS = Nid+1

END SUBROUTINE CMF_ICI_INPUT
!####################################################################





!####################################################################
SUBROUTINE CMF_ICI_INIT
! Initialize ICI
!$ USE OMP_LIB
IMPLICIT NONE
!================================================
WRITE(LOGNAM,*) "CMF::ICI_INIT: (1) Set Time & Map"
CALL palm_TimeStart( 'Setup' )

!*** 1a. Set mapping table
CALL ICI_MAPTABLE_INIT

!*** 1b. Set time related
CALL ICI_TIME_INIT

!*** 1c. Allocate lake variables
IF (LLAKEIN) THEN
  CALL ICI_LAKE_INIT
ENDIF

!================================================
WRITE(LOGNAM,*) "CMF::ICI_INIT: (2) Set Output, Forcing, Boundary"

!*** 2. Create first data output
CALL ICI_OUTPUT_INIT

!===============================================
CALL palm_TimeEnd  ( 'Setup' )

CONTAINS
!==========================================================
!+ ICI_MAPTABLE_INIT : Define CaMa grids and set mapping table
!==========================================================
SUBROUTINE ICI_MAPTABLE_INIT
! Define CaMa grids and set mapping table
! -- call from CMF_ICI_INIT
USE YOS_CMF_INPUT,           ONLY: NSETFILE,NX
USE YOS_CMF_MAP,             ONLY: I1SEQX, I1SEQY, NSEQALL
USE CMF_UTILS_MOD,           ONLY: INQUIRE_FID
USE ici_api,                 ONLY: ici_def_grid, ici_end_grid_def
IMPLICIT NONE
!* local variables
INTEGER(KIND=JPIM)              :: ix,iy,iseq,i
INTEGER(KIND=JPIM)              :: cama_grid(NSEQALL)
!================================================
DO iseq=1,NSEQALL
  ix=I1SEQX(ISEQ)
  iy=I1SEQY(ISEQ)
  cama_grid(ISEQ)=ix+(iy-1)*NX
ENDDO

CALL ici_def_grid(my_grid,NSEQALL,1,1,cama_grid)
CALL ici_end_grid_def()

END SUBROUTINE ICI_MAPTABLE_INIT
!==========================================================
!+
!+
!+
!==========================================================
SUBROUTINE ICI_TIME_INIT
! Initialize time for ICI
USE YOS_CMF_TIME,            ONLY: ISYYYY,ISMM,ISDD,ISHOUR
USE ici_api,                 ONLY: ici_init_time
IMPLICIT NONE
!================================================
time_array(1)=ISYYYY
time_array(2)=ISMM
time_array(3)=ISDD
time_array(4)=ISHOUR
CALL ici_init_time(time_array)

END SUBROUTINE ICI_TIME_INIT
!==========================================================
!+
!+
!+
!==========================================================
SUBROUTINE ICI_LAKE_INIT
! Initialize lake variables
USE YOS_CMF_MAP,        ONLY: NSEQMAX
USE YOS_CMF_ICI,        ONLY: D2LAKEFRC, D2RUNIN, D2RUNIN_AVG
IMPLICIT NONE
!================================================
ALLOCATE(D2LAKEFRC(NSEQMAX,1))
ALLOCATE(D2RUNIN(NSEQMAX,1))
ALLOCATE(D2RUNIN_AVG(NSEQMAX,1))
D2LAKEFRC(:,:) = 0._JPRB
D2RUNIN(:,:) = 0._JPRB
D2RUNIN_AVG(:,:) = 0._JPRB

END SUBROUTINE ICI_LAKE_INIT
!==========================================================
!+
!+
!+
!==========================================================
SUBROUTINE ICI_OUTPUT_INIT
! Create first data output
USE YOS_CMF_INPUT,      ONLY: LOUTPUT,      IFRQ_OUT,     LPTHOUT,      LGDWDLY,      LROSPLIT
USE YOS_CMF_PROG,       ONLY: P2RIVSTO,     P2FLDSTO,     P2GDWSTO
USE YOS_CMF_DIAG,       ONLY: D2RIVDPH,     D2FLDDPH,     D2FLDFRC,     D2FLDARE,     D2SFCELV,     D2STORGE, &
                            & D2OUTFLW_AVG, D2RIVOUT_AVG, D2FLDOUT_AVG, D2PTHOUT_AVG, D1PTHFLW_AVG, &
                            & D2RIVVEL_AVG, D2GDWRTN_AVG, D2RUNOFF_AVG, D2ROFSUB_AVG,               &
                            & D2OUTFLW_MAX, D2STORGE_MAX, D2RIVDPH_MAX
USE YOS_CMF_MAP,        ONLY: NSEQALL
USE YOS_CMF_ICI,        ONLY: D2RUNIN_AVG, D2LAKEFRC
USE ici_api,            ONLY: ici_put_data
IMPLICIT NONE
!================================================
call ici_put_data("rivout", D2RIVOUT_AVG(:NSEQALL,1))
call ici_put_data("rivsto", P2RIVSTO(:NSEQALL,1))
call ici_put_data("rivdph", D2RIVDPH(:NSEQALL,1))
call ici_put_data("rivvel", D2RIVVEL_AVG(:NSEQALL,1))
call ici_put_data("fldout", D2FLDOUT_AVG(:NSEQALL,1))
call ici_put_data("fldsto", P2FLDSTO(:NSEQALL,1))
call ici_put_data("flddph", D2FLDDPH(:NSEQALL,1))
call ici_put_data("fldfrc", D2FLDFRC(:NSEQALL,1))
call ici_put_data("fldare", D2FLDARE(:NSEQALL,1))
call ici_put_data("sfcelv", D2SFCELV(:NSEQALL,1))
call ici_put_data("outflw", D2OUTFLW_AVG(:NSEQALL,1))
call ici_put_data("storge", D2STORGE(:NSEQALL,1))
call ici_put_data("runoff_o", D2RUNOFF_AVG(:NSEQALL,1))
call ici_put_data("maxflw", D2OUTFLW_MAX(:NSEQALL,1))
call ici_put_data("maxsto", D2STORGE_MAX(:NSEQALL,1))
call ici_put_data("maxdph", D2RIVDPH_MAX(:NSEQALL,1))
IF (LPTHOUT) THEN
  call ici_put_data("pthout", D2PTHOUT_AVG(:NSEQALL,1))
  !call ici_put_data("pthflw", D1PTHFLW_AVG(:,:))
ENDIF
IF (LGDWDLY) THEN
  call ici_put_data("gdwsto", P2GDWSTO(:NSEQALL,1))
  call ici_put_data("gdwrtn", D2GDWRTN_AVG(:NSEQALL,1))
ENDIF
IF (LROSPLIT) THEN
  call ici_put_data("rofsub", D2ROFSUB_AVG(:NSEQALL,1))
ENDIF
IF (LLAKEIN) THEN
  call ici_put_data("lkfrac" , D2LAKEFRC(:NSEQALL,1))
  call ici_put_data("runin" , D2RUNIN_AVG(:NSEQALL,1))
  call ici_put_data("runin_2m",D2RUNIN_AVG(:NSEQALL,1))
ENDIF

IF ( .not. LOUTPUT ) THEN
  IFRQ_OUT = -1
ENDIF

END SUBROUTINE ICI_OUTPUT_INIT
!==========================================================


END SUBROUTINE CMF_ICI_INIT
!####################################################################





!####################################################################
SUBROUTINE CMF_ICI_FORCING_GET
! -- CMF_ICI_FORCING_GET    : Update time, read forcing data from file and convert unit
! read runoff from file
USE YOS_CMF_INPUT,           ONLY: DT, LROSPLIT
USE YOS_CMF_MAP,             ONLY: NSEQALL
USE YOS_CMF_PROG,            ONLY: D2RUNOFF, D2ROFSUB
USE ici_api,                 ONLY: ici_set_time, ici_get_data
USE YOS_CMF_ICI,             ONLY: D2LAKEFRC
IMPLICIT NONE
REAL(KIND=JPRB)                 :: PBUFF(NSEQALL,1)
LOGICAL                         :: is_get_ok


!================================================
CALL palm_TimeStart( 'ICI_sync' )
CALL ici_set_time(time_array, int(DT))
CALL palm_TimeEnd  ( 'ICI_sync' )

CALL ici_get_data("runoff",PBUFF(:,1),IS_GET_OK=is_get_ok)
IF (is_get_ok) THEN
  CALL roff_convert_ici(PBUFF,D2RUNOFF)
endif
IF (LROSPLIT) THEN
  CALL ici_get_data("rofsub",PBUFF(:,1),IS_GET_OK=is_get_ok)
  IF (is_get_ok) THEN
    CALL roff_convert_ici(PBUFF,D2ROFSUB)
  ENDIF
ENDIF

IF (LLAKEIN) THEN
  CALL ici_get_data("lakefrc",PBUFF(:,1),IS_GET_OK=is_get_ok)
  if( is_get_ok )then
    CALL lake_fraction_ici(PBUFF,D2LAKEFRC)
  endif
ENDIF

CONTAINS
!==========================================================
!+ roff_interp_ici
!==========================================================
SUBROUTINE roff_convert_ici(pbuffin,pbuffout)
! Convert units for runoff
USE YOS_CMF_MAP,             ONLY: NSEQALL, D2GRAREA
IMPLICIT NONE
REAL(KIND=JPRB),INTENT(IN)      :: PBUFFIN(:,:)     !! default [kg/m2/s]
REAL(KIND=JPRB),INTENT(OUT)     :: PBUFFOUT(:,:)    !! m3/s
!$ SAVE
INTEGER(KIND=JPIM)  ::  ISEQ
!============================
!$OMP PARALLEL DO
DO ISEQ=1, NSEQALL
  PBUFFOUT(ISEQ,1) = 0.D0
  PBUFFOUT(ISEQ,1) = MAX(PBUFFIN(ISEQ,1),0.D0) * D2GRAREA(ISEQ,1) / mrofunit   !! DTIN removed in v395
  PBUFFOUT(ISEQ,1) = MAX(PBUFFOUT(ISEQ,1), 0.D0)
END DO
!$OMP END PARALLEL DO
END SUBROUTINE roff_convert_ici
!==========================================================

!==========================================================
!+ lake fraction
!==========================================================
SUBROUTINE lake_fraction_ici(pbuffin,pbuffout)
! Read lake fraction
USE YOS_CMF_MAP,             ONLY: NSEQALL
IMPLICIT NONE
REAL(KIND=JPRB),INTENT(IN)      :: PBUFFIN(:,:)
REAL(KIND=JPRB),INTENT(OUT)     :: PBUFFOUT(:,:)
!$ SAVE
INTEGER(KIND=JPIM)  ::  ISEQ
!============================
!$OMP PARALLEL DO
DO ISEQ=1, NSEQALL
  PBUFFOUT(ISEQ,1) = PBUFFIN(ISEQ,1)
END DO
!$OMP END PARALLEL DO
END SUBROUTINE lake_fraction_ici
!==========================================================

END SUBROUTINE CMF_ICI_FORCING_GET
!####################################################################





!####################################################################
SUBROUTINE CMF_ICI_OUTPUT
! Send output to ICI
USE YOS_CMF_INPUT,      ONLY: LPTHOUT,      LGDWDLY,      LROSPLIT
USE YOS_CMF_PROG,       ONLY: P2RIVSTO,     P2FLDSTO,     P2GDWSTO
USE YOS_CMF_DIAG,       ONLY: D2RIVDPH,     D2FLDDPH,     D2FLDFRC,     D2FLDARE,     D2SFCELV,     D2STORGE, &
                            & D2OUTFLW_AVG, D2RIVOUT_AVG, D2FLDOUT_AVG, D2PTHOUT_AVG, D1PTHFLW_AVG, &
                            & D2RIVVEL_AVG, D2GDWRTN_AVG, D2RUNOFF_AVG, D2ROFSUB_AVG,               &
                            & D2OUTFLW_MAX, D2STORGE_MAX, D2RIVDPH_MAX
USE YOS_CMF_MAP,        ONLY: NSEQALL
USE YOS_CMF_ICI,        ONLY: D2RUNIN_AVG, D2LAKEFRC
USE YOS_CMF_TIME,       ONLY: JYYYY, JMM, JDD, JHOUR, JMIN
USE CMF_CALC_LAKEIN_MOD,ONLY: CMF_LAKEIN_AVERAGE, CMF_RESET_LAKEIN
USE CMF_CALC_DIAG_MOD,  ONLY: CMF_DIAG_AVERAGE, CMF_DIAG_RESET
USE ici_api,            ONLY: ici_put_data
IMPLICIT NONE

call CMF_DIAG_AVERAGE
call ici_put_data("rivout", D2RIVOUT_AVG(:NSEQALL,1))
call ici_put_data("rivsto", P2RIVSTO(:NSEQALL,1))
call ici_put_data("rivdph", D2RIVDPH(:NSEQALL,1))
call ici_put_data("rivvel", D2RIVVEL_AVG(:NSEQALL,1))
call ici_put_data("fldout", D2FLDOUT_AVG(:NSEQALL,1))
call ici_put_data("fldsto", P2FLDSTO(:NSEQALL,1))
call ici_put_data("flddph", D2FLDDPH(:NSEQALL,1))
call ici_put_data("fldfrc", D2FLDFRC(:NSEQALL,1))
call ici_put_data("fldare", D2FLDARE(:NSEQALL,1))
call ici_put_data("sfcelv", D2SFCELV(:NSEQALL,1))
call ici_put_data("outflw", D2OUTFLW_AVG(:NSEQALL,1))
call ici_put_data("storge", D2STORGE(:NSEQALL,1))
call ici_put_data("runoff_o", D2RUNOFF_AVG(:NSEQALL,1))
call ici_put_data("maxflw", D2OUTFLW_MAX(:NSEQALL,1))
call ici_put_data("maxsto", D2STORGE_MAX(:NSEQALL,1))
call ici_put_data("maxdph", D2RIVDPH_MAX(:NSEQALL,1))
IF (LPTHOUT) THEN
  call ici_put_data("pthout", D2PTHOUT_AVG(:NSEQALL,1))
  !call ici_put_data("pthflw", D1PTHFLW_AVG(:,:))
ENDIF
IF (LGDWDLY) THEN
  call ici_put_data("gdwsto", P2GDWSTO(:NSEQALL,1))
  call ici_put_data("gdwrtn", D2GDWRTN_AVG(:NSEQALL,1))
ENDIF
IF (LROSPLIT) THEN
  call ici_put_data("rofsub", D2ROFSUB_AVG(:NSEQALL,1))
ENDIF
IF (LLAKEIN) THEN
  call CMF_LAKEIN_AVERAGE
  call ici_put_data("lkfrac" , D2LAKEFRC(:NSEQALL,1))
  call ici_put_data("runin" , D2RUNIN_AVG(:NSEQALL,1))
  call ici_put_data("runin_2m",D2RUNIN_AVG(:NSEQALL,1))
  call CMF_RESET_LAKEIN
ENDIF
call CMF_DIAG_RESET

time_array(1) = JYYYY
time_array(2) = JMM
time_array(3) = JDD
time_array(4) = JHOUR
time_array(5) = JMIN

END SUBROUTINE CMF_ICI_OUTPUT
!####################################################################





!####################################################################
SUBROUTINE CMF_ICI_END
! Finalize ICI and MPI
USE ici_api,                 ONLY: ici_finalize
!$ USE OMP_LIB
IMPLICIT NONE
!==========================================================
CALL palm_TimeFinalize()
CALL ici_finalize(.true., .true.)

END SUBROUTINE CMF_ICI_END
!####################################################################

END MODULE CMF_CTRL_ICI_MOD
