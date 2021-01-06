MODULE CMF_CTRL_MOJ_MOD
!==========================================================
!* PURPOSE: manage MOJ coupler for CaMa-Flood
!
!* CONTAINS:
! -- CMF_MOJ_INPUT          : Read setting from Namelist
! -- CMF_MOJ_INIT           : Initialize MOJ coupler
! -- CMF_MOJ_FORCING_GET    : Update time, read forcing data from file and convert unit
! -- CMF_MOJ_OUTPUT         : Send output to MOJ
! -- CMF_MOJ_END            : Finalize   MOJ coupler
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
USE PARKIND1,                ONLY: JPIM, JPRB, JPRM
USE YOS_CMF_INPUT,           ONLY: LOGNAM
IMPLICIT NONE
SAVE
!*** NAMELIST/cama_moj/
CHARACTER(LEN=256)              ::  my_comp
CHARACTER(LEN=256)              ::  my_grid
CHARACTER(LEN=256)              ::  namelist_moj_file
INTEGER(KIND=JPIM)              ::  intpl_num
NAMELIST/cama_moj/  my_comp, my_grid, namelist_moj_file, intpl_num

!*** NAMELIST/cama_moj_intpl/
CHARACTER(LEN=256)              ::  send_comp
CHARACTER(LEN=256)              ::  send_grid
CHARACTER(LEN=256)              ::  recv_comp
CHARACTER(LEN=256)              ::  recv_grid
CHARACTER(LEN=256)              ::  map_file
CHARACTER(LEN=256)              ::  intpl_file
INTEGER(KIND=JPIM)              ::  intpl_map

NAMELIST/cama_moj_intpl/  send_comp, send_grid, recv_comp, recv_grid, intpl_map, map_file, intpl_file

!*** NAMELIST/cama_moj_force/
REAL(KIND=JPRB)                 ::  mrofunit
NAMELIST/cama_moj_force/  mrofunit

! local variables
INTEGER(KIND=JPIM)              :: time_array(6) = (/2000,1,1,0,0,0/)          ! simulation time step

REAL(KIND=JPRB)                 :: ZTT0, ZTT1, ZTT2                            ! Time elapsed related 
INTEGER(KIND=JPIM)              :: ierr, Nproc, Nid                            ! MPI related
!==========================================================
CONTAINS
!####################################################################
! -- CMF_MOJ_INPUT      : Read setting from Namelist
! -- CMF_MOJ_INIT        : Initialize MOJ coupler
! -- CMF_MOJ_END         : Finalize   MOJ coupler
!
!####################################################################
SUBROUTINE CMF_MOJ_INPUT 
! Set moj namelist
USE YOS_CMF_INPUT,           ONLY: NSETFILE
USE YOS_CMF_MAP,             ONLY: REGIONALL,REGIONTHIS
USE CMF_UTILS_MOD,           ONLY: INQUIRE_FID
USE moj_api,                 ONLY: moj_init,moj_get_numpe_local, moj_get_irank_local
IMPLICIT NONE
!* local variables
!================================================
NSETFILE=INQUIRE_FID()
OPEN(NSETFILE,FILE='cama.conf',STATUS="OLD")
REWIND(NSETFILE)
READ(NSETFILE,NML=cama_moj)
REWIND(NSETFILE)
READ(NSETFILE,NML=cama_moj_force)
CLOSE(NSETFILE)

!*** Initialize MPI
CALL MPI_Init(ierr)
CALL moj_init(my_comp, namelist_moj_file)

Nproc = moj_get_numpe_local()
Nid   = moj_get_irank_local()
REGIONALL = Nproc
REGIONTHIS = Nid+1


END SUBROUTINE CMF_MOJ_INPUT 
!####################################################################





!####################################################################
SUBROUTINE CMF_MOJ_INIT
! Initialize MOJ
!$ USE OMP_LIB    
IMPLICIT NONE
!================================================
WRITE(LOGNAM,*) "CMF::MOJ_INIT: (1) Set Time & Map"

!*** 1a. Set mapping table 
CALL MOJ_MAPTABLE_INIT

!*** 1b. Set time related
CALL MOJ_TIME_INIT

!================================================
WRITE(LOGNAM,*) "CMF::MOJ_INIT: (2) Set Output, Forcing, Boundary"

!*** 2. Create first data output
CALL MOJ_OUTPUT_INIT

CONTAINS
!==========================================================
!+ MOJ_MAPTABLE_INIT : Define CaMa grids and set mapping table
!+ CMF_INPMAT_INIT_CDF      :  open runoff interporlation matrix (inpmat)
!+ CMF_INPMAT_INIT_BIN      :  open runoff interporlation matrix (inpmat)
!==========================================================
SUBROUTINE MOJ_MAPTABLE_INIT
! Define CaMa grids and set mapping table
! -- call from CMF_MOJ_INIT
USE YOS_CMF_INPUT,           ONLY: NSETFILE,NX,NY
USE YOS_CMF_MAP,             ONLY: REGIONTHIS, I2REGION
USE YOS_CMF_MAP,             ONLY: I1SEQX, I1SEQY, I2NEXTX, I2NEXTY, NSEQALL
USE CMF_UTILS_MOD,           ONLY: INQUIRE_FID
USE moj_api,                 ONLY: moj_def_grid, moj_end_grid_def, moj_set_interpolation_table
IMPLICIT NONE
!* local variables
INTEGER(KIND=JPIM)              :: ix,iy,iseq,i
INTEGER(KIND=JPIM)              :: cama_grid(NSEQALL)
INTEGER(KIND=JPIM), ALLOCATABLE :: send_grid_index(:),recv_grid_index(:)
REAL(KIND=JPRB), ALLOCATABLE    :: coef(:)
!================================================
DO iseq=1,NSEQALL
  ix=I1SEQX(ISEQ)
  iy=I1SEQY(ISEQ)
  cama_grid(ISEQ)=ix+(iy-1)*NX
ENDDO

CALL moj_def_grid(my_grid,NSEQALL,1,1,cama_grid)
CALL moj_end_grid_def()

NSETFILE=INQUIRE_FID()
OPEN(NSETFILE,FILE='cama.conf',STATUS="OLD")
REWIND(NSETFILE)
DO i=1,intpl_num
  send_comp  = ""
  send_grid  = ""
  recv_comp  = ""
  recv_grid  = ""
  map_file   = ""
  intpl_map  = 1
  intpl_file = ""
  READ(NSETFILE,NML=cama_moj_intpl)
  IF (map_file=="") THEN
    CALL moj_set_interpolation_table(send_comp,send_grid,recv_comp,recv_grid)
  ELSE
    ALLOCATE(send_grid_index(intpl_map))
    ALLOCATE(recv_grid_index(intpl_map))
    ALLOCATE(coef(intpl_map))
    OPEN(8,FILE=map_file,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4*intpl_map)
    READ(8,REC=1) send_grid_index
    READ(8,REC=2) recv_grid_index
    CLOSE(8)
    IF (intpl_file=="") THEN
      coef(:)=1.d0
    ELSE
      OPEN(8,FILE=intpl_file,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=8*intpl_map)
      READ(8,REC=1) coef
      CLOSE(8)
    ENDIF
    CALL moj_set_interpolation_table(send_comp,send_grid,recv_comp,recv_grid,send_grid_index,recv_grid_index,coef)
    DEALLOCATE(send_grid_index,recv_grid_index,coef)
  ENDIF
ENDDO
CLOSE(NSETFILE)

END SUBROUTINE MOJ_MAPTABLE_INIT
!==========================================================
!+
!+
!+
!==========================================================
SUBROUTINE MOJ_TIME_INIT
! Initialize time for MOJ
USE YOS_CMF_TIME,            ONLY: ISYYYY,ISMM,ISDD,ISHOUR
USE moj_api,                 ONLY: moj_init_time
IMPLICIT NONE
!================================================
time_array(1)=ISYYYY
time_array(2)=ISMM
time_array(3)=ISDD
time_array(4)=ISHOUR
CALL moj_init_time(time_array)

END SUBROUTINE MOJ_TIME_INIT
!==========================================================
!+
!+
!+
!==========================================================
SUBROUTINE MOJ_OUTPUT_INIT
! Create first data output
USE YOS_CMF_PROG,       ONLY: D2RIVSTO,     D2FLDSTO,     D2GDWSTO
USE YOS_CMF_DIAG,       ONLY: D2RIVDPH,     D2FLDDPH,     D2FLDFRC,     D2FLDARE,     D2SFCELV,     D2STORGE, &
                            & D2OUTFLW_AVG, D2RIVOUT_AVG, D2FLDOUT_AVG, D2PTHOUT_AVG, D1PTHFLW_AVG, &
                            & D2RIVVEL_AVG, D2GDWRTN_AVG, D2RUNOFF_AVG, D2ROFSUB_AVG,               &
                            & D2OUTFLW_MAX, D2STORGE_MAX, D2RIVDPH_MAX
USE YOS_CMF_MAP,        ONLY: NSEQALL
USE moj_api,            ONLY: moj_put_data
IMPLICIT NONE
!================================================
call moj_put_data("rivout", D2RIVOUT_AVG(:NSEQALL,1))
call moj_put_data("rivsto", D2RIVSTO(:NSEQALL,1))
call moj_put_data("rivdph", D2RIVDPH(:NSEQALL,1))
call moj_put_data("rivvel", D2RIVVEL_AVG(:NSEQALL,1))
call moj_put_data("fldout", D2FLDOUT_AVG(:NSEQALL,1))
call moj_put_data("fldsto", D2FLDSTO(:NSEQALL,1))
call moj_put_data("flddph", D2FLDDPH(:nseqall,1))
call moj_put_data("fldfrc", D2FLDFRC(:nseqall,1))
call moj_put_data("fldare", D2FLDARE(:nseqall,1))
call moj_put_data("sfcelv", D2SFCELV(:nseqall,1))
call moj_put_data("outflw", D2OUTFLW_AVG(:nseqall,1))
!call moj_put_data("gdwsto", D2GDWSTO(:NSEQALL,1))
call moj_put_data("storge", D2STORGE(:NSEQALL,1)) 
call moj_put_data("pthout", D2PTHOUT_AVG(:NSEQALL,1)) 
call moj_put_data("pthflw", D1PTHFLW_AVG(:NSEQALL,1)) 
!call moj_put_data("gdwrtn", D2GDWRTN_AVG(:NSEQALL,1)) 
!call moj_put_data("runoff", D2RUNOFF_AVG(:NSEQALL,1)) 
!call moj_put_data("rofsub", D2ROFSUB_AVG(:NSEQALL,1)) 
call moj_put_data("maxflw", D2OUTFLW_MAX(:NSEQALL,1)) 
call moj_put_data("maxsto", D2STORGE_MAX(:NSEQALL,1)) 
call moj_put_data("maxdph", D2RIVDPH_MAX(:NSEQALL,1)) 

END SUBROUTINE MOJ_OUTPUT_INIT
!==========================================================

END SUBROUTINE CMF_MOJ_INIT
!####################################################################





!####################################################################
SUBROUTINE CMF_MOJ_FORCING_GET
! -- CMF_MOJ_FORCING_GET    : Update time, read forcing data from file and convert unit
! read runoff from file
USE YOS_CMF_INPUT,           ONLY: DT
USE YOS_CMF_MAP,             ONLY: NSEQALL
USE YOS_CMF_PROG,            ONLY: D2RUNOFF
USE moj_api,                 ONLY: moj_set_time, moj_get_data
IMPLICIT NONE
REAL(KIND=JPRB)                 :: PBUFF(NSEQALL,2)
LOGICAL                         :: is_get_ok


!================================================
CALL moj_set_time(time_array, int(dt))
CALL moj_get_data("runoff",PBUFF(:,1),IS_GET_OK=is_get_ok)
if (is_get_ok) then
  CALL roff_convert_moj(PBUFF,D2RUNOFF)
endif

CONTAINS
!==========================================================
!+ roff_interp_moj
!==========================================================
SUBROUTINE roff_convert_moj(pbuffin,pbuffout)
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
END SUBROUTINE roff_convert_moj
!==========================================================

END SUBROUTINE CMF_MOJ_FORCING_GET
!####################################################################





!####################################################################
SUBROUTINE CMF_MOJ_OUTPUT
! Send output to MOJ
USE YOS_CMF_PROG,       ONLY: D2RIVSTO,     D2FLDSTO,     D2GDWSTO
USE YOS_CMF_DIAG,       ONLY: D2RIVDPH,     D2FLDDPH,     D2FLDFRC,     D2FLDARE,     D2SFCELV,     D2STORGE, &
                            & D2OUTFLW_AVG, D2RIVOUT_AVG, D2FLDOUT_AVG, D2PTHOUT_AVG, D1PTHFLW_AVG, &
                            & D2RIVVEL_AVG, D2GDWRTN_AVG, D2RUNOFF_AVG, D2ROFSUB_AVG,               &
                            & D2OUTFLW_MAX, D2STORGE_MAX, D2RIVDPH_MAX
USE YOS_CMF_MAP,        ONLY: NSEQALL
USE CMF_CALC_DIAG_MOD,  ONLY: CMF_DIAG_AVERAGE, CMF_DIAG_RESET
USE moj_api,            ONLY: moj_put_data
IMPLICIT NONE

call CMF_DIAG_AVERAGE
call moj_put_data("rivout", D2RIVOUT_AVG(:NSEQALL,1))
call moj_put_data("rivsto", D2RIVSTO(:NSEQALL,1))
call moj_put_data("rivdph", D2RIVDPH(:NSEQALL,1))
call moj_put_data("rivvel", D2RIVVEL_AVG(:NSEQALL,1))
call moj_put_data("fldout", D2FLDOUT_AVG(:NSEQALL,1))
call moj_put_data("fldsto", D2FLDSTO(:NSEQALL,1))
call moj_put_data("flddph", D2FLDDPH(:nseqall,1))
call moj_put_data("fldfrc", D2FLDFRC(:nseqall,1))
call moj_put_data("fldare", D2FLDARE(:nseqall,1))
call moj_put_data("sfcelv", D2SFCELV(:nseqall,1))
call moj_put_data("outflw", D2OUTFLW_AVG(:nseqall,1))
!call moj_put_data("gdwsto", D2GDWSTO(:NSEQALL,1))
call moj_put_data("storge", D2STORGE(:NSEQALL,1))
call moj_put_data("pthout", D2PTHOUT_AVG(:NSEQALL,1))
call moj_put_data("pthflw", D1PTHFLW_AVG(:NSEQALL,1))
!call moj_put_data("gdwrtn", D2GDWRTN_AVG(:NSEQALL,1))
!call moj_put_data("runoff", D2RUNOFF_AVG(:NSEQALL,1))
!call moj_put_data("rofsub", D2ROFSUB_AVG(:NSEQALL,1))
call moj_put_data("maxflw", D2OUTFLW_MAX(:NSEQALL,1))
call moj_put_data("maxsto", D2STORGE_MAX(:NSEQALL,1))
call moj_put_data("maxdph", D2RIVDPH_MAX(:NSEQALL,1))
call CMF_DIAG_RESET

END SUBROUTINE CMF_MOJ_OUTPUT
!####################################################################





!####################################################################
SUBROUTINE CMF_MOJ_END
! Finalize MOJ and MPI
USE moj_api,                 ONLY: moj_finalize
!$ USE OMP_LIB    
IMPLICIT NONE 
!==========================================================
CALL moj_finalize(.true., .true.)

END SUBROUTINE CMF_MOJ_END
!####################################################################

END MODULE CMF_CTRL_MOJ_MOD
