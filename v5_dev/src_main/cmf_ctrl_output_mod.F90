MODULE CMF_CTRL_OUTPUT_MOD
!==========================================================
!* PURPOSE: Control CaMa-Flood standard output file (binary / netCDF)  
!
!* CONTAINS:
!* Principal CMF_* procedures describe the Output module workflow.
!* OUTPUT_WRITE_R4/R8 preserve the historical specific generic interfaces.
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
! shared variables in module
USE PARKIND1,                ONLY: JPIM, JPRB, JPRM
USE PARKIND1,                ONLY: JPRD
USE YOS_CMF_INPUT,           ONLY: LOGNAM,  IFRQ_OUT, CMF_BASE
USE YOS_CMF_INPUT,           ONLY: CSUFBIN, CSUFVEC, CSUFPTH, CSUFCDF
USE YOS_CMF_INPUT,           ONLY: LPTHOUT, LDAMOUT, LLEVEE,  LWEVAP, LGDWDLY, LOUTINS,LROSPLIT

IMPLICIT NONE
!============================
SAVE
!*** NAMELIST/NOUTPUT/ from inputnam
CHARACTER(LEN=256)              ::  COUTDIR           !! Output directory.
CHARACTER(LEN=256)              ::  CVARSOUT          !! Comma-separated output variable names.
CHARACTER(LEN=256)              ::  COUTTAG           !! Output tag for this experiment.
CHARACTER(LEN=512)              ::  COUTVAR_TABLE     !! Output-variable metadata CSV path.
!
LOGICAL                         ::  LOUTVEC           !! Write vector output instead of an NX-by-NY map.
LOGICAL                         ::  LOUTCDF           !! Write NetCDF output instead of Binary output.
INTEGER(KIND=JPIM)              ::  NDLEVEL           !! NetCDF deflation level.
!
LOGICAL                         ::  LOUTTXT           !! Write text output for selected gauges.
CHARACTER(LEN=256)              ::  CGAUTXT           !! Gauge list file (ID, name, IX, IY).
!
NAMELIST/NOUTPUT/ CMF_BASE,COUTVAR_TABLE,COUTDIR,CVARSOUT,COUTTAG,LOUTCDF,NDLEVEL, &
                  LOUTVEC,IFRQ_OUT,LOUTTXT,CGAUTXT
!
!*** local variables
INTEGER(KIND=JPIM), PARAMETER   :: NVARS=100          ! temporal output var number
INTEGER(KIND=JPIM)              :: NVARSOUT
INTEGER(KIND=JPIM)              :: IRECOUT            ! Output file irec
! Temporal aggregation according to CF conventions 1.7, Table E.1
CHARACTER(LEN=7), PARAMETER :: AGGREGATION_METHOD(3) = ["point  ", "mean   ", "maximum"]
!*** TYPE for output file    
TYPE TVAROUT
CHARACTER(LEN=256)              :: CVNAME             ! output variable name
CHARACTER(LEN=256)              :: CVLNAME            ! output variable long name
CHARACTER(LEN=256)              :: CVUNITS            ! output units
CHARACTER(LEN=256)              :: CFILE              ! output full path file name 
INTEGER(KIND=JPIM)              :: AGGREGATE = 1      ! index of AGGREGATION_METHODS, default is instantaneous/point
INTEGER(KIND=JPIM)              :: BINID              ! output binary output file ID
INTEGER(KIND=JPIM)              :: NCID               ! output netCDF output file ID
INTEGER(KIND=JPIM)              :: VARID              ! output netCDF output variable ID
INTEGER(KIND=JPIM)              :: TIMID              ! output netCDF time   variable ID 
INTEGER(KIND=JPIM)              :: IRECNC             ! Current time record for writting
REAL(KIND=JPRM)                 :: MISSING_VALUE      ! registry missing-value metadata when available
LOGICAL                         :: HAS_REGISTRY_METADATA = .FALSE.
LOGICAL                         :: HAS_MISSING_VALUE = .FALSE.
END TYPE TVAROUT 
TYPE(TVAROUT),ALLOCATABLE       :: VAROUT(:)          ! output variable TYPE set

! Runtime output metadata from output_variables.csv . Internal array selection remains an explicit Fortran
! dispatcher; registry internal_variable/internal_dtype columns are informational.
TYPE TOUTPUT_METADATA
  CHARACTER(LEN=256) :: NAME
  CHARACTER(LEN=256) :: ALIAS_OF
  CHARACTER(LEN=256) :: LONG_NAME
  CHARACTER(LEN=256) :: UNITS
  CHARACTER(LEN=32)  :: TEMPORAL_TYPE
  CHARACTER(LEN=32)  :: REQUIRED_OPTION
  REAL(KIND=JPRM)    :: MISSING_VALUE
  LOGICAL            :: HAS_MISSING_VALUE
END TYPE TOUTPUT_METADATA
TYPE(TOUTPUT_METADATA),ALLOCATABLE :: OUTPUT_REGISTRY(:)

INTERFACE OUTPUT_WRITE
  MODULE PROCEDURE OUTPUT_WRITE_R4
  MODULE PROCEDURE OUTPUT_WRITE_R8
END INTERFACE OUTPUT_WRITE

PUBLIC :: CMF_OUTPUT_NMLIST, CMF_OUTPUT_INIT, CMF_OUTPUT_WRITE, CMF_OUTPUT_END, CMF_OUTTXT_WRTE
PUBLIC :: OUTPUT_WRITE, OUTPUT_WRITE_R4, OUTPUT_WRITE_R8
PRIVATE :: OUTPUT_UTIL_INFER_CMF_BASE, OUTPUT_UTIL_PARSE_VARIABLES, OUTPUT_UTIL_FATAL
PRIVATE :: OUTPUT_UTIL_CREATE_OUTBIN, OUTPUT_UTIL_CREATE_OUTCDF, OUTPUT_UTIL_LOAD_REGISTRY
PRIVATE :: OUTPUT_UTIL_LOAD_METADATA, OUTPUT_UTIL_CHECK_OPTION, OUTPUT_UTIL_WRITE_R4, OUTPUT_UTIL_WRITE_R8
PRIVATE :: OUTPUT_UTIL_FIND_OUTPUT, OUTPUT_UTIL_WRITE_MAP, OUTPUT_UTIL_WRITE_PTH
PRIVATE :: OUTPUT_UTIL_WRITE_BINARY_MAP, OUTPUT_UTIL_WRITE_BINARY_PTH, OUTPUT_UTIL_WRITE_VECTOR_INDEX

CONTAINS
!####################################################################
! Principal model procedures
! -- CMF_OUTPUT_NMLIST : Read Output namelist settings.
! -- CMF_OUTPUT_INIT   : Parse variables, load metadata, and create files.
! -- CMF_OUTPUT_WRITE  : Dispatch model arrays to selected output writers.
! -- CMF_OUTPUT_END    : Close output files and write vector index metadata.
! -- CMF_OUTTXT_WRTE   : Write discharge time series at selected gauges.
! Private implementation utilities follow the principal procedures:
! -- OUTPUT_UTIL_PARSE_VARIABLES : Parse and validate CVARSOUT.
! -- OUTPUT_UTIL_LOAD_REGISTRY  : Read and validate output metadata.
! -- OUTPUT_UTIL_LOAD_METADATA  : Resolve selected names and aliases.
! -- OUTPUT_UTIL_CHECK_OPTION   : Validate required runtime options.
! -- OUTPUT_UTIL_CREATE_OUT*    : Create Binary or NetCDF output files.
! -- OUTPUT_UTIL_WRITE_*        : Find outputs and write records.
!####################################################################
SUBROUTINE CMF_OUTPUT_NMLIST
! Read output settings from the namelist.
! -- Called from CMF_DRV_NMLIST
USE YOS_CMF_INPUT,           ONLY: CSETFILE,NSETFILE
USE YOS_CMF_INPUT,           ONLY: CDIMINFO
USE CMF_UTILS_MOD,           ONLY: INQUIRE_FID

IMPLICIT NONE
!================================================
WRITE(LOGNAM,*) ""
WRITE(LOGNAM,*) "!---------------------!"

!*** 1. open namelist
NSETFILE=INQUIRE_FID()
OPEN(NSETFILE,FILE=CSETFILE,STATUS="OLD")
WRITE(LOGNAM,*) "CMF::OUTPUT_NMLIST: namelist OPEN in unit:", TRIM(CSETFILE), NSETFILE

!*** 2. default value
COUTDIR="./"
CVARSOUT="outflw,storge,rivdph"
COUTTAG="_cmf"
CMF_BASE='.'
LOUTCDF=.FALSE.
NDLEVEL=3
LOUTVEC=.FALSE.
IFRQ_OUT = 24                !! daily (24h) output
!
LOUTTXT=.FALSE.
CGAUTXT="None"
COUTVAR_TABLE=''

!*** 3. read namelist
REWIND(NSETFILE)
READ(NSETFILE,NML=NOUTPUT)
IF (TRIM(CMF_BASE)=='.') CMF_BASE=OUTPUT_UTIL_INFER_CMF_BASE(CDIMINFO)
IF (LEN_TRIM(COUTVAR_TABLE)==0) THEN
  COUTVAR_TABLE=TRIM(CMF_BASE)//'/variable_table/output_variables.csv'
ENDIF

WRITE(LOGNAM,*)   "=== NAMELIST, NOUTPUT ==="
WRITE(LOGNAM,*)   "COUTDIR:  ", TRIM(COUTDIR)
WRITE(LOGNAM,*)   "CVARSOUT: ", TRIM(CVARSOUT)
WRITE(LOGNAM,*)   "COUTTAG:  ", TRIM(COUTTAG)
WRITE(LOGNAM,*)   "CMF_BASE: ", TRIM(CMF_BASE)
WRITE(LOGNAM,*)   "COUTVAR_TABLE: ", TRIM(COUTVAR_TABLE)

WRITE(LOGNAM,*)   "LOUTCDF:  ", LOUTCDF
IF( LOUTCDF )THEN
  WRITE(LOGNAM,*) "NDLEVEL:  ", NDLEVEL
ENDIF
if( LOUTVEC )THEN
  WRITE(LOGNAM,*) "LOUTVEC:  ", LOUTVEC
ENDIF
WRITE(LOGNAM,*)   "IFRQ_OUT  ", IFRQ_OUT

WRITE(LOGNAM,*)   "IFRQ_OUT  ", LOUTTXT
WRITE(LOGNAM,*)   "CGAUTXRT  ", CGAUTXT

CLOSE(NSETFILE)

WRITE(LOGNAM,*) "CMF::OUTPUT_NMLIST: end"

END SUBROUTINE CMF_OUTPUT_NMLIST
!####################################################################



!####################################################################
SUBROUTINE CMF_OUTPUT_INIT
! Initialize output metadata and create/open the selected output files.
! -- Called from CMF_DRV_INIT
USE YOS_CMF_TIME,            ONLY: ISYYYY, ISMM, ISDD, ISHOUR, ISMIN
USE YOS_CMF_MAP,             ONLY: REGIONTHIS
USE CMF_UTILS_MOD,           ONLY: INQUIRE_FID

IMPLICIT NONE
!* Local variables
CHARACTER(LEN=256) :: CTIME
INTEGER(KIND=JPIM) :: JF
CHARACTER(LEN=256) :: CVNAMES(NVARS)
CHARACTER(LEN=256) :: REQUIRED_OPTION
LOGICAL            :: REGISTRY_HAS_MISSING_VALUE
!================================================
WRITE(LOGNAM,*) ""
WRITE(LOGNAM,*) "!---------------------!"

WRITE(LOGNAM,*) "CMF::OUTPUT_INIT: check output variables"
!*** 1. Parse the requested names before loading the registry.
CALL OUTPUT_UTIL_PARSE_VARIABLES(CVARSOUT,NVARSOUT,CVNAMES)

IF ( NVARSOUT == 0 ) THEN
  WRITE(LOGNAM,*) "CMF::OUTPUT_INIT: No output files will be produced!"
  RETURN
ENDIF

!*** 2. Load the registry (output_variables.csv) and resolve each requested variable.
CALL OUTPUT_UTIL_LOAD_REGISTRY
ALLOCATE(VAROUT(NVARSOUT))
WRITE(CTIME,'(A14,I4.4,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2)') 'seconds since ',ISYYYY,'-',ISMM,'-',ISDD,' ',ISHOUR,":",ISMIN

!* Resolve all selected metadata through the configured registry (output_variables.csv).
DO JF=1,NVARSOUT
  VAROUT(JF)%CVNAME=TRIM(CVNAMES(JF))
CALL OUTPUT_UTIL_LOAD_METADATA(CVNAMES(JF),VAROUT(JF)%CVLNAME,VAROUT(JF)%CVUNITS, &
       VAROUT(JF)%AGGREGATE,VAROUT(JF)%MISSING_VALUE,REGISTRY_HAS_MISSING_VALUE,REQUIRED_OPTION)
  VAROUT(JF)%HAS_REGISTRY_METADATA=.TRUE.
  VAROUT(JF)%HAS_MISSING_VALUE=REGISTRY_HAS_MISSING_VALUE
  IF (REQUIRED_OPTION/='none') THEN
    CALL OUTPUT_UTIL_CHECK_OPTION(CVNAMES(JF),REQUIRED_OPTION)
  ENDIF
ENDDO

!*** 3. Create files only after all selected metadata and options validate.
DO JF=1,NVARSOUT
  WRITE(LOGNAM,*) "Creating output for variable:",TRIM(VAROUT(JF)%CVNAME)
  VAROUT(JF)%BINID=INQUIRE_FID()

  IF( LOUTCDF )THEN
    IF( REGIONTHIS==1 )THEN
      CALL OUTPUT_UTIL_CREATE_OUTCDF(JF,CTIME)
    ENDIF
  ELSE
    CALL OUTPUT_UTIL_CREATE_OUTBIN(JF)
  ENDIF
END DO

IRECOUT=0  ! Initialize Output record to 1 (shared in netcdf & binary)

END SUBROUTINE CMF_OUTPUT_INIT
!####################################################################



!####################################################################
SUBROUTINE CMF_OUTPUT_WRITE
! Driver-compatible dispatcher. Array association is explicit here; CSV strings
! are never used to look up Fortran storage.
USE YOS_CMF_INPUT,           ONLY: LOUTINI
USE YOS_CMF_TIME,            ONLY: JYYYYMMDD,JHHMM,JHOUR,JMIN,KSTEP
USE YOS_CMF_PROG,            ONLY: P2RIVSTO,P2FLDSTO,P2GDWSTO,P2DAMSTO,P2LEVSTO
USE YOS_CMF_DIAG,            ONLY: D2RIVDPH,D2FLDDPH,D2FLDFRC,D2FLDARE,D2SFCELV,D2STORGE, &
  D2OUTFLW_oAVG,D2RIVOUT_oAVG,D2FLDOUT_oAVG,D2PTHOUT_oAVG,D1PTHFLW_oAVG, &
  D2RIVVEL_oAVG,D2GDWRTN_oAVG,D2RUNOFF_oAVG,D2ROFSUB_oAVG,D2WEVAPEX_oAVG, &
  D2OUTFLW_oMAX,D2STORGE_oMAX,D2RIVDPH_oMAX,D2DAMINF_oAVG,D2OUTINS,D2LEVDPH

IMPLICIT NONE
INTEGER(KIND=JPIM) :: JF
!================================================
IF (MOD(JHOUR,IFRQ_OUT)/=0 .OR. JMIN/=0) RETURN
IRECOUT=IRECOUT+1
WRITE(LOGNAM,*) 'CMF::OUTPUT_WRITE: write at time: ',JYYYYMMDD,JHHMM,IRECOUT
DO JF=1,NVARSOUT
  IF (KSTEP==0 .AND. LOUTINI) THEN
    IF (.NOT.LOUTCDF) CYCLE
    IF (VAROUT(JF)%CVNAME/='rivsto' .AND. VAROUT(JF)%CVNAME/='fldsto' .AND. &
        VAROUT(JF)%CVNAME/='gwsto') CYCLE
  ENDIF
  SELECT CASE (TRIM(VAROUT(JF)%CVNAME))
    CASE ('rivout'); CALL OUTPUT_WRITE('rivout',D2RIVOUT_oAVG)
    CASE ('rivsto'); CALL OUTPUT_WRITE('rivsto',P2RIVSTO)
    CASE ('rivdph'); CALL OUTPUT_WRITE('rivdph',D2RIVDPH)
    CASE ('rivvel'); CALL OUTPUT_WRITE('rivvel',D2RIVVEL_oAVG)
    CASE ('fldout'); CALL OUTPUT_WRITE('fldout',D2FLDOUT_oAVG)
    CASE ('fldsto'); CALL OUTPUT_WRITE('fldsto',P2FLDSTO)
    CASE ('flddph'); CALL OUTPUT_WRITE('flddph',D2FLDDPH)
    CASE ('fldfrc'); CALL OUTPUT_WRITE('fldfrc',D2FLDFRC)
    CASE ('fldare'); CALL OUTPUT_WRITE('fldare',D2FLDARE)
    CASE ('sfcelv'); CALL OUTPUT_WRITE('sfcelv',D2SFCELV)
    CASE ('totout'); CALL OUTPUT_WRITE('totout',D2OUTFLW_oAVG)
    CASE ('outflw'); CALL OUTPUT_WRITE('outflw',D2OUTFLW_oAVG)
    CASE ('totsto'); CALL OUTPUT_WRITE('totsto',D2STORGE)
    CASE ('storge'); CALL OUTPUT_WRITE('storge',D2STORGE)
    CASE ('pthout')
      IF (LPTHOUT) CALL OUTPUT_WRITE('pthout',D2PTHOUT_oAVG)
    CASE ('pthflw')
      IF (LPTHOUT) CALL OUTPUT_WRITE('pthflw',D1PTHFLW_oAVG)
    CASE ('maxflw'); CALL OUTPUT_WRITE('maxflw',D2OUTFLW_oMAX)
    CASE ('maxdph'); CALL OUTPUT_WRITE('maxdph',D2RIVDPH_oMAX)
    CASE ('maxsto'); CALL OUTPUT_WRITE('maxsto',D2STORGE_oMAX)
    CASE ('outins')
      IF (LOUTINS) CALL OUTPUT_WRITE('outins',D2OUTINS)
    CASE ('gwsto','gdwsto')
      IF (LGDWDLY) CALL OUTPUT_WRITE(TRIM(VAROUT(JF)%CVNAME),P2GDWSTO)
    CASE ('gwout','gdwrtn')
      IF (LGDWDLY) CALL OUTPUT_WRITE(TRIM(VAROUT(JF)%CVNAME),D2GDWRTN_oAVG)
    CASE ('runoff','rofsfc'); CALL OUTPUT_WRITE(TRIM(VAROUT(JF)%CVNAME),D2RUNOFF_oAVG)
    CASE ('runoffsub','rofsub')
      IF (LROSPLIT) CALL OUTPUT_WRITE(TRIM(VAROUT(JF)%CVNAME),D2ROFSUB_oAVG)
    CASE ('wevap')
      IF (LWEVAP) CALL OUTPUT_WRITE('wevap',D2WEVAPEX_oAVG)
    CASE ('damsto')
      IF (LDAMOUT) CALL OUTPUT_WRITE('damsto',P2DAMSTO)
    CASE ('daminf')
      IF (LDAMOUT) CALL OUTPUT_WRITE('daminf',D2DAMINF_oAVG)
    CASE ('levsto')
      IF (LLEVEE) CALL OUTPUT_WRITE('levsto',P2LEVSTO)
    CASE ('levdph')
      IF (LLEVEE) CALL OUTPUT_WRITE('levdph',D2LEVDPH)
    CASE DEFAULT
      WRITE(LOGNAM,*) 'CMF::OUTPUT_WRITE: unknown output ',TRIM(VAROUT(JF)%CVNAME)
      STOP 9
  END SELECT
ENDDO
WRITE(LOGNAM,*) 'CMF::OUTPUT_WRITE: end'
END SUBROUTINE CMF_OUTPUT_WRITE
!####################################################################




!####################################################################
SUBROUTINE CMF_OUTPUT_END
! Finalize output module (close files)
!+ WRTE_mapR2vecD : Write map-grid coordinates for vector output.
! -- Called from CMF_DRV_END
#ifdef UseCDF_CMF
USE NETCDF
USE CMF_UTILS_MOD,           ONLY: NCERROR
#endif
USE YOS_CMF_MAP,             ONLY: REGIONTHIS

IMPLICIT NONE
! Local variables
INTEGER(KIND=JPIM) :: JF
!================================================
WRITE(LOGNAM,*) ""
WRITE(LOGNAM,*) "!---------------------!"
WRITE(LOGNAM,*) "CMF::OUTPUT_END: finalize output module"

IF( REGIONTHIS==1 )THEN
  IF (LOUTCDF) THEN
#ifdef UseCDF_CMF
    DO JF=1,NVARSOUT
      CALL NCERROR( NF90_CLOSE(VAROUT(JF)%NCID))
      WRITE(LOGNAM,*) "Output netcdf output unit closed:",VAROUT(JF)%NCID
    ENDDO
#endif
  ELSE !! binary output
    DO JF=1,NVARSOUT
      CLOSE(VAROUT(JF)%BINID)
      WRITE(LOGNAM,*) "Output binary output unit closed:",VAROUT(JF)%BINID
    ENDDO
    IF( LOUTVEC )THEN
      CALL OUTPUT_UTIL_WRITE_VECTOR_INDEX  !! write map-vector conversion file
    ENDIF
  ENDIF
ENDIF

WRITE(LOGNAM,*) "CMF::OUTPUT_END: end"

END SUBROUTINE CMF_OUTPUT_END
!####################################################################




!####################################################################
SUBROUTINE CMF_OUTTXT_WRTE
! Write discharge time series for the configured gauge locations.
USE YOS_CMF_DIAG,            ONLY: D2OUTFLW
USE YOS_CMF_TIME,            ONLY: IYYYYMMDD,ISYYYY
USE YOS_CMF_MAP,             ONLY: I2VECTOR
USE CMF_UTILS_MOD,           ONLY: INQUIRE_FID

IMPLICIT NONE

! local
INTEGER(KIND=JPIM)                  :: GID, GIX, GIY, GISEQ
CHARACTER(len=256),SAVE             :: GNAME

INTEGER(KIND=JPIM),SAVE             :: IGAUGE, NGAUGE, NGAUGEX
INTEGER(KIND=JPIM),ALLOCATABLE,SAVE :: WriteID(:), WriteISEQ(:)
CHARACTER(len=9),ALLOCATABLE,SAVE   :: WriteName(:)
REAL(KIND=JPRB),ALLOCATABLE,SAVE    :: WriteOut(:)

! File IO
INTEGER(KIND=JPIM),SAVE             :: LOGOUTTXT
CHARACTER(len=4),SAVE               :: cYYYY
CHARACTER(len=256),SAVE             :: CLEN, CFMT
CHARACTER(len=256),SAVE             :: COUTTXT
LOGICAL,SAVE                        :: IsOpen
DATA IsOpen       /.FALSE./

!================================================
IF( LOUTTXT )THEN

  IF( .not. IsOpen)THEN
    IsOpen=.TRUE.

    NGAUGEX=0
    LOGOUTTXT=INQUIRE_FID()
    OPEN(LOGOUTTXT,FILE=CGAUTXT,FORM='formatted',STATUS='old')
    READ(LOGOUTTXT,*) NGAUGE
    DO IGAUGE=1, NGAUGE
      READ(LOGOUTTXT,*) GID, GNAME, GIX, GIY
      IF( I2VECTOR(GIX,GIY)>0 )THEN
        NGAUGEX=NGAUGEX+1
      ENDIF
    END DO
    CLOSE(LOGOUTTXT)

    ALLOCATE( WriteID(NGAUGEX),WriteISEQ(NGAUGEX),WriteOut(NGAUGEX),WriteName(NGAUGEX))

    NGAUGEX=0
    OPEN(LOGOUTTXT,FILE=CGAUTXT,FORM='formatted',STATUS='old')
    READ(LOGOUTTXT,*) NGAUGE
    DO IGAUGE=1, NGAUGE
      READ(LOGOUTTXT,*) GID, GNAME, GIX, GIY
      IF( I2VECTOR(GIX,GIY)>0 )THEN
        NGAUGEX=NGAUGEX+1
        WriteID(NGAUGEX)  =GID
        WriteName(NGAUGEX)=TRIM(GNAME)
        WriteISEQ(NGAUGEX)=I2VECTOR(GIX,GIY)
      ENDIF
    END DO
    CLOSE(LOGOUTTXT)

    ! ============
    WRITE(CYYYY,'(i4.4)') ISYYYY
    COUTTXT='./outtxt-'//TRIM(cYYYY)//'.txt'

    LOGOUTTXT=INQUIRE_FID()
    OPEN(LOGOUTTXT,FILE=COUTTXT,FORM='formatted')

    WRITE(CLEN,'(i0)') NGAUGE
    CFMT="(i10,"//TRIM(CLEN)//"(i10))"
    WRITE(LOGOUTTXT,CFMT) NGAUGEX, ( WriteID(IGAUGE),IGAUGE=1,NGAUGEX )

    CFMT="(i10,"//TRIM(CLEN)//"(x,a9))"
    WRITE(LOGOUTTXT,CFMT) NGAUGEX, ( WriteName(IGAUGE),IGAUGE=1,NGAUGEX )

    CFMT="(i10,"//TRIM(CLEN)//"(f10.2))"
  ENDIF

  DO IGAUGE=1, NGAUGEX
    GISEQ=WriteISEQ(IGAUGE)
    WriteOut(IGAUGE) = D2OUTFLW(GISEQ,1)
  END DO

  WRITE(LOGOUTTXT,CFMT) IYYYYMMDD, ( WriteOUT(IGAUGE),IGAUGE=1,NGAUGEX )

ENDIF

END SUBROUTINE CMF_OUTTXT_WRTE
!####################################################################





!####################################################################
! Public generic-specific compatibility procedures
!####################################################################
SUBROUTINE OUTPUT_WRITE_R4(NAME,ARRAY)
! Preserve the historical specific entry point behind OUTPUT_WRITE.
IMPLICIT NONE
CHARACTER(LEN=*),INTENT(IN) :: NAME
REAL(KIND=JPRM),INTENT(IN) :: ARRAY(:,:)
!================================================
CALL OUTPUT_UTIL_WRITE_R4(NAME,ARRAY)
END SUBROUTINE OUTPUT_WRITE_R4

SUBROUTINE OUTPUT_WRITE_R8(NAME,ARRAY)
! Preserve the historical specific entry point behind OUTPUT_WRITE.
IMPLICIT NONE
CHARACTER(LEN=*),INTENT(IN) :: NAME
REAL(KIND=JPRD),INTENT(IN) :: ARRAY(:,:)
!================================================
CALL OUTPUT_UTIL_WRITE_R8(NAME,ARRAY)
END SUBROUTINE OUTPUT_WRITE_R8

!####################################################################
! Private implementation utilities
!####################################################################
SUBROUTINE OUTPUT_UTIL_PARSE_VARIABLES(CVARS,NUM_VARS,VARIABLE_NAMES)
! Parse requested output names and reject overflow or duplicate names.
IMPLICIT NONE
CHARACTER(LEN=*),INTENT(IN) :: CVARS
INTEGER(KIND=JPIM),INTENT(OUT) :: NUM_VARS
CHARACTER(LEN=256),INTENT(OUT) :: VARIABLE_NAMES(NVARS)
CHARACTER(LEN=256) :: TOKEN
INTEGER(KIND=JPIM) :: JF,J,J0
!================================================
NUM_VARS=0
J0=1
DO J=1,LEN(TRIM(CVARS))
  IF( (J>J0) .AND. (CVARS(J:J) .EQ. ',') ) THEN
    TOKEN=TRIM(ADJUSTL(CVARS(J0:J-1)))
    IF (LEN(TOKEN) > 0 ) THEN
      NUM_VARS=NUM_VARS+1
      IF (NUM_VARS>NVARS) CALL OUTPUT_UTIL_FATAL('too many requested output variables')
      VARIABLE_NAMES(NUM_VARS)=TOKEN
    ENDIF
    J0=J+1
  ENDIF
ENDDO
! Last one
IF ( J0 <= LEN(TRIM(CVARS)) ) THEN
  J=LEN(TRIM(CVARS))
  TOKEN=TRIM(ADJUSTL(CVARS(J0:J)))
  IF (LEN(TOKEN) > 0 ) THEN
    NUM_VARS=NUM_VARS+1
    IF (NUM_VARS>NVARS) CALL OUTPUT_UTIL_FATAL('too many requested output variables')
    VARIABLE_NAMES(NUM_VARS)=TOKEN
  ENDIF
ENDIF
DO JF=1,NUM_VARS
  DO J=1,JF-1
    IF (TRIM(VARIABLE_NAMES(J))==TRIM(VARIABLE_NAMES(JF))) &
      CALL OUTPUT_UTIL_FATAL('duplicate requested output '//TRIM(VARIABLE_NAMES(JF)))
  ENDDO
ENDDO
END SUBROUTINE OUTPUT_UTIL_PARSE_VARIABLES

SUBROUTINE OUTPUT_UTIL_FATAL(MESSAGE)
! Log an unrecoverable Output initialization error and stop with the existing status.
IMPLICIT NONE
CHARACTER(LEN=*),INTENT(IN) :: MESSAGE
!================================================
WRITE(LOGNAM,*) 'CMF::OUTPUT_INIT: ERROR: ',TRIM(MESSAGE)
STOP 9
END SUBROUTINE OUTPUT_UTIL_FATAL
!####################################################################

!####################################################################
FUNCTION OUTPUT_UTIL_INFER_CMF_BASE(DIMINFO_PATH) RESULT(BASE)
! Infer the installation root from the map-dimension path or executable path.
IMPLICIT NONE
CHARACTER(LEN=*),INTENT(IN) :: DIMINFO_PATH
CHARACTER(LEN=512)          :: BASE
CHARACTER(LEN=1024)         :: EXECUTABLE
INTEGER(KIND=JPIM)          :: STATUS,POS
BASE='.'
POS=INDEX(TRIM(DIMINFO_PATH),'/map/',BACK=.TRUE.)
IF (POS>0) THEN
  BASE=DIMINFO_PATH(1:POS-1)
  IF (LEN_TRIM(BASE)==0) BASE='/'
  RETURN
ENDIF
CALL GET_COMMAND_ARGUMENT(0,EXECUTABLE,STATUS=STATUS)
IF (STATUS/=0) RETURN
POS=INDEX(TRIM(EXECUTABLE),'/src_main/MAIN_cmf',BACK=.TRUE.)
IF (POS>0) THEN
  BASE=EXECUTABLE(1:POS-1)
  IF (LEN_TRIM(BASE)==0) BASE='/'
  RETURN
ENDIF
POS=INDEX(TRIM(EXECUTABLE),'/out/bin/MAIN_cmf_netcdf',BACK=.TRUE.)
IF (POS==0) POS=INDEX(TRIM(EXECUTABLE),'/out/bin/MAIN_cmf',BACK=.TRUE.)
IF (POS>0) THEN
  BASE=EXECUTABLE(1:POS-1)
  IF (LEN_TRIM(BASE)==0) BASE='/'
ENDIF
END FUNCTION OUTPUT_UTIL_INFER_CMF_BASE

SUBROUTINE OUTPUT_UTIL_CREATE_OUTBIN(JF)
! Open the selected output as a Binary file.
USE YOS_CMF_INPUT,           ONLY: NX,NY
USE YOS_CMF_MAP,             ONLY: NSEQMAX,NPTHOUT,NPTHLEV,REGIONTHIS

IMPLICIT NONE
INTEGER(KIND=JPIM),INTENT(IN) :: JF
!================================================
IF( TRIM(VAROUT(JF)%CVNAME)=='pthflw' ) THEN   !! bifurcation channel
  IF( REGIONTHIS==1 )THEN
    VAROUT(JF)%CFILE=TRIM(COUTDIR)//TRIM(VAROUT(JF)%CVNAME)//TRIM(COUTTAG)//TRIM(CSUFPTH)
    OPEN(VAROUT(JF)%BINID,FILE=VAROUT(JF)%CFILE,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4*NPTHOUT*NPTHLEV)
    WRITE(LOGNAM,*) "output file opened in unit: ", TRIM(VAROUT(JF)%CFILE), VAROUT(JF)%BINID
  ENDIF
ELSEIF( LOUTVEC )THEN   !!  1D land only output
  VAROUT(JF)%CFILE=TRIM(COUTDIR)//TRIM(VAROUT(JF)%CVNAME)//TRIM(COUTTAG)//TRIM(CSUFVEC)
  OPEN(VAROUT(JF)%BINID,FILE=VAROUT(JF)%CFILE,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4*NSEQMAX)
  WRITE(LOGNAM,*) "output file opened in unit: ", TRIM(VAROUT(JF)%CFILE), VAROUT(JF)%BINID
ELSE                   !!  2D default map output
  IF( REGIONTHIS==1 )THEN
    VAROUT(JF)%CFILE=TRIM(COUTDIR)//TRIM(VAROUT(JF)%CVNAME)//TRIM(COUTTAG)//TRIM(CSUFBIN)
    WRITE(LOGNAM,*) "  -- ", TRIM(VAROUT(JF)%CFILE)
    OPEN(VAROUT(JF)%BINID,FILE=VAROUT(JF)%CFILE,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4*NX*NY)
    WRITE(LOGNAM,*) "output file opened in unit: ", TRIM(VAROUT(JF)%CFILE), VAROUT(JF)%BINID
  ENDIF
ENDIF

END SUBROUTINE OUTPUT_UTIL_CREATE_OUTBIN
!####################################################################

!####################################################################
SUBROUTINE OUTPUT_UTIL_CREATE_OUTCDF(JF,CTIME)
! Define and open the selected output as a NetCDF file.
#ifdef UseCDF_CMF
USE YOS_CMF_INPUT,           ONLY: NX,NY,RMIS
USE YOS_CMF_MAP,             ONLY: D1LON,D1LAT,NPTHOUT,NPTHLEV,PTH_ID,PTH_IX,PTH_IY,PTH_JX,PTH_JY, &
                                   PTH_START_LAT, PTH_START_LON, PTH_END_LAT, PTH_END_LON, &
                                   PTH_DST, PTH_ELEVTN, PTH_DEPTH, PTH_WTH, PTH_IBSN, PTH_JBSN
USE CMF_UTILS_MOD,           ONLY: NCERROR
USE NETCDF
#endif

IMPLICIT NONE
INTEGER(KIND=JPIM),INTENT(IN) :: JF
CHARACTER(LEN=*),INTENT(IN)   :: CTIME
#ifdef UseCDF_CMF
INTEGER(KIND=JPIM)             :: TIMEID,VARID,LATID,LONID,NPTHOUTID,NPTHLEVID,I
CHARACTER(LEN=32)              :: CMETH
CHARACTER(LEN=12),PARAMETER    :: PATH_INT_NAMES(7)=[CHARACTER(LEN=12) :: &
  'pth_id','ix','iy','jx','jy','ibsn','jbsn']
CHARACTER(LEN=12),PARAMETER    :: PATH_REAL_NAMES(7)=[CHARACTER(LEN=12) :: &
  'start_lat','start_lon','end_lat','end_lon','length','elevtn','depth']
CHARACTER(LEN=80),PARAMETER    :: PATH_INT_LONG(7)=[CHARACTER(LEN=80) :: &
  'configuration-specific bifurcation path row ID','starting map x index','starting map y index', &
  'ending map x index','ending map y index','ascending numeric basin ID (smaller)','ascending numeric basin ID (larger)']
CHARACTER(LEN=80),PARAMETER    :: PATH_REAL_LONG(7)=[CHARACTER(LEN=80) :: &
  'source pathway latitude','source pathway longitude','ending unit-catchment outlet latitude', &
  'ending unit-catchment outlet longitude','bifurcation path length','bifurcation path elevation', &
  'estimated bifurcation channel depth']
CHARACTER(LEN=16),PARAMETER    :: PATH_REAL_UNITS(7)=[CHARACTER(LEN=16) :: &
  'degrees_north','degrees_east','degrees_north','degrees_east','m','m','m']
INTEGER(KIND=JPIM),ALLOCATABLE :: PATH_INT_DATA(:,:)
REAL(KIND=JPRB),ALLOCATABLE    :: PATH_REAL_DATA(:,:)
#endif
#ifdef UseCDF_CMF
!============
VAROUT(JF)%IRECNC=1 ! initialize record current writting record to 1

!============
VAROUT(JF)%CFILE=TRIM(COUTDIR)//'o_'//TRIM(VAROUT(JF)%CVNAME)//TRIM(COUTTAG)//TRIM(CSUFCDF)
! Create file
CALL NCERROR( NF90_CREATE(VAROUT(JF)%CFILE,NF90_NETCDF4,VAROUT(JF)%NCID),&
              'CREATING FILE:'//TRIM(VAROUT(JF)%CFILE) )
!=== set dimension ===
CALL NCERROR( NF90_DEF_DIM(VAROUT(JF)%NCID, 'time', NF90_UNLIMITED, TIMEID) )
CALL NCERROR( NF90_DEF_DIM(VAROUT(JF)%NCID, 'lat', NY, LATID) )
CALL NCERROR( NF90_DEF_DIM(VAROUT(JF)%NCID, 'lon', NX, LONID) )
! Bifurcation paths are indexed separately from the geographic map grid.
IF (TRIM(VAROUT(JF)%CVNAME)=='pthflw') THEN
  CALL NCERROR( NF90_DEF_DIM(VAROUT(JF)%NCID, 'NPTHOUT', NPTHOUT, NPTHOUTID) )
  CALL NCERROR( NF90_DEF_DIM(VAROUT(JF)%NCID, 'NPTHLEV', NPTHLEV, NPTHLEVID) )
    DO I=1,SIZE(PATH_INT_NAMES)
      CALL NCERROR(NF90_DEF_VAR(VAROUT(JF)%NCID,TRIM(PATH_INT_NAMES(I)),NF90_INT,(/NPTHOUTID/),VARID))
      CALL NCERROR(NF90_PUT_ATT(VAROUT(JF)%NCID,VARID,'long_name',TRIM(PATH_INT_LONG(I))))
      CALL NCERROR(NF90_PUT_ATT(VAROUT(JF)%NCID,VARID,'units','1'))
      IF (I==1) THEN
        CALL NCERROR(NF90_PUT_ATT(VAROUT(JF)%NCID,VARID,'description', &
          'One-based row identifier in the map-specific bifurcation parameter table.'))
      ELSEIF (I>=2 .AND. I<=5) THEN
        CALL NCERROR(NF90_PUT_ATT(VAROUT(JF)%NCID,VARID,'description', &
          'One-based map grid index; x is column and y is row.'))
      ELSE
        CALL NCERROR(NF90_PUT_ATT(VAROUT(JF)%NCID,VARID,'description', &
          'Numeric basin IDs sorted in ascending order; these are not directional endpoint labels.'))
      ENDIF
    ENDDO
    DO I=1,SIZE(PATH_REAL_NAMES)
      CALL NCERROR(NF90_DEF_VAR(VAROUT(JF)%NCID,TRIM(PATH_REAL_NAMES(I)),NF90_FLOAT,(/NPTHOUTID/),VARID))
      CALL NCERROR(NF90_PUT_ATT(VAROUT(JF)%NCID,VARID,'long_name',TRIM(PATH_REAL_LONG(I))))
      CALL NCERROR(NF90_PUT_ATT(VAROUT(JF)%NCID,VARID,'units',TRIM(PATH_REAL_UNITS(I))))
      IF (I==3 .OR. I==4 .OR. I==7) &
        CALL NCERROR(NF90_PUT_ATT(VAROUT(JF)%NCID,VARID,'missing_value',-9999._JPRB))
    ENDDO
    CALL NCERROR(NF90_DEF_VAR(VAROUT(JF)%NCID,'width',NF90_FLOAT, &
      (/NPTHOUTID,NPTHLEVID/),VARID,SHUFFLE=.TRUE.,DEFLATE_LEVEL=NDLEVEL),'Creating pathway width')
    CALL NCERROR(NF90_PUT_ATT(VAROUT(JF)%NCID,VARID,'long_name','bifurcation path width by layer'))
    CALL NCERROR(NF90_PUT_ATT(VAROUT(JF)%NCID,VARID,'units','m'))
    CALL NCERROR(NF90_PUT_ATT(VAROUT(JF)%NCID,VARID,'description', &
      'Fortran dimensions (path,layer); file dimensions (layer,path); layer k equals width_k in bifprm.csv.'))
ENDIF

!=== define variables ===
CALL NCERROR( NF90_DEF_VAR(VAROUT(JF)%NCID, 'lat', NF90_FLOAT, (/LATID/), VARID) )
CALL NCERROR( NF90_PUT_ATT(VAROUT(JF)%NCID, VARID, 'long_name','latitude') )
CALL NCERROR( NF90_PUT_ATT(VAROUT(JF)%NCID, VARID, 'units','degrees_north') )

CALL NCERROR( NF90_DEF_VAR(VAROUT(JF)%NCID, 'lon', NF90_FLOAT, (/LONID/), VARID) )
CALL NCERROR( NF90_PUT_ATT(VAROUT(JF)%NCID, VARID, 'long_name','longitude') )
CALL NCERROR( NF90_PUT_ATT(VAROUT(JF)%NCID, VARID, 'units','degrees_east') )

CALL NCERROR( NF90_DEF_VAR(VAROUT(JF)%NCID, 'time', NF90_DOUBLE, (/TIMEID/), VAROUT(JF)%TIMID) )
CALL NCERROR( NF90_PUT_ATT(VAROUT(JF)%NCID, VAROUT(JF)%TIMID, 'long_name','time') )
CALL NCERROR( NF90_PUT_ATT(VAROUT(JF)%NCID, VAROUT(JF)%TIMID, 'units',CTIME) )

!===
! Match the pthflw_pre Restart dimensions and keep standard Float32 compression.
IF (TRIM(VAROUT(JF)%CVNAME)=='pthflw') THEN
  CALL NCERROR( NF90_DEF_VAR(VAROUT(JF)%NCID, VAROUT(JF)%CVNAME, NF90_FLOAT, &
                (/NPTHOUTID,NPTHLEVID,TIMEID/), VAROUT(JF)%VARID, &
                SHUFFLE=.TRUE.,DEFLATE_LEVEL=NDLEVEL), 'Creating bifurcation variable')
ELSE
  CALL NCERROR( NF90_DEF_VAR(VAROUT(JF)%NCID, VAROUT(JF)%CVNAME, NF90_FLOAT, &
                (/LONID,LATID,TIMEID/), VAROUT(JF)%VARID, &
                SHUFFLE=.TRUE.,DEFLATE_LEVEL=NDLEVEL), 'Creating Variable')
ENDIF

CALL NCERROR( NF90_PUT_ATT(VAROUT(JF)%NCID, VAROUT(JF)%VARID, 'long_name', TRIM(VAROUT(JF)%CVLNAME)) )
CALL NCERROR( NF90_PUT_ATT(VAROUT(JF)%NCID, VAROUT(JF)%VARID, 'units',     TRIM(VAROUT(JF)%CVUNITS)) )
IF (VAROUT(JF)%HAS_REGISTRY_METADATA) THEN
  IF (VAROUT(JF)%HAS_MISSING_VALUE) THEN
    CALL NCERROR( NF90_PUT_ATT(VAROUT(JF)%NCID,VAROUT(JF)%VARID,'_FillValue', &
                               VAROUT(JF)%MISSING_VALUE) )
  ENDIF
ELSE
  CALL NCERROR( NF90_PUT_ATT(VAROUT(JF)%NCID, VAROUT(JF)%VARID, '_FillValue',RMIS) )
ENDIF
CMETH = 'time: ' // AGGREGATION_METHOD(VAROUT(JF)%AGGREGATE)
CALL NCERROR( NF90_PUT_ATT(VAROUT(JF)%NCID, VAROUT(JF)%VARID, 'cell_methods', TRIM(CMETH)) )

CALL NCERROR( NF90_ENDDEF(VAROUT(JF)%NCID) )

!=== put lon lat info ===
CALL NCERROR ( NF90_INQ_VARID(VAROUT(JF)%NCID,'lon',VARID),'getting id' )
CALL NCERROR( NF90_PUT_VAR(VAROUT(JF)%NCID,VARID,D1LON))

CALL NCERROR ( NF90_INQ_VARID(VAROUT(JF)%NCID,'lat',VARID),'getting id' )
CALL NCERROR( NF90_PUT_VAR(VAROUT(JF)%NCID,VARID,D1LAT))

IF (TRIM(VAROUT(JF)%CVNAME)=='pthflw') THEN
  ALLOCATE(PATH_INT_DATA(NPTHOUT,7),PATH_REAL_DATA(NPTHOUT,7))
  PATH_INT_DATA(:,1)=PTH_ID; PATH_INT_DATA(:,2)=PTH_IX; PATH_INT_DATA(:,3)=PTH_IY
  PATH_INT_DATA(:,4)=PTH_JX; PATH_INT_DATA(:,5)=PTH_JY
  PATH_INT_DATA(:,6)=PTH_IBSN; PATH_INT_DATA(:,7)=PTH_JBSN
  PATH_REAL_DATA(:,1)=PTH_START_LAT; PATH_REAL_DATA(:,2)=PTH_START_LON
  PATH_REAL_DATA(:,3)=PTH_END_LAT; PATH_REAL_DATA(:,4)=PTH_END_LON
  PATH_REAL_DATA(:,5)=PTH_DST; PATH_REAL_DATA(:,6)=PTH_ELEVTN; PATH_REAL_DATA(:,7)=PTH_DEPTH
  DO I=1,SIZE(PATH_INT_NAMES)
    CALL NCERROR(NF90_INQ_VARID(VAROUT(JF)%NCID,TRIM(PATH_INT_NAMES(I)),VARID))
    CALL NCERROR(NF90_PUT_VAR(VAROUT(JF)%NCID,VARID,PATH_INT_DATA(:,I)))
  ENDDO
  DO I=1,SIZE(PATH_REAL_NAMES)
    CALL NCERROR(NF90_INQ_VARID(VAROUT(JF)%NCID,TRIM(PATH_REAL_NAMES(I)),VARID))
    CALL NCERROR(NF90_PUT_VAR(VAROUT(JF)%NCID,VARID,PATH_REAL_DATA(:,I)))
  ENDDO
  CALL NCERROR(NF90_INQ_VARID(VAROUT(JF)%NCID,'width',VARID))
  CALL NCERROR(NF90_PUT_VAR(VAROUT(JF)%NCID,VARID,PTH_WTH))
  DEALLOCATE(PATH_INT_DATA,PATH_REAL_DATA)
ENDIF

WRITE(LOGNAM,*) 'CFILE: ',TRIM(VAROUT(JF)%CFILE),' CVAR:',TRIM(VAROUT(JF)%CVNAME),&
                ' CLNAME: ',TRIM(VAROUT(JF)%CVLNAME),' CUNITS: ',TRIM(VAROUT(JF)%CVUNITS)
WRITE(LOGNAM,*) 'OPEN IN UNIT: ',VAROUT(JF)%NCID
#endif
END SUBROUTINE OUTPUT_UTIL_CREATE_OUTCDF
!####################################################################

!####################################################################
SUBROUTINE OUTPUT_UTIL_LOAD_REGISTRY()
! Read the CSV registry and resolve metadata inherited by aliases.
USE YOS_CMF_INPUT,           ONLY: RMIS
USE CMF_UTILS_MOD,           ONLY: SPLIT_CSV_RECORD, CSV_COLUMN

IMPLICIT NONE
CHARACTER(LEN=512)                :: LOAD_ERROR

CHARACTER(LEN=8192)                :: REGISTRY_LINE
CHARACTER(LEN=1024)                :: CSV_HEADER(32),CSV_FIELDS(32)
TYPE(TOUTPUT_METADATA),ALLOCATABLE :: TEMP_REGISTRY(:)
INTEGER(KIND=JPIM)                 :: CSV_UNIT,IOS,NHEADER,NFIELDS,NROWS,ROW,COL,PARSE_STATUS
INTEGER(KIND=JPIM)                 :: IDX_NAME,IDX_ALIAS,IDX_LONG,IDX_UNITS,IDX_TIME,IDX_MISSING,IDX_OPTION,CANON
REAL(KIND=JPRM)                    :: SENTINEL
!================================================
LOAD_ERROR=''
IF (ALLOCATED(OUTPUT_REGISTRY)) DEALLOCATE(OUTPUT_REGISTRY)
ALLOCATE(OUTPUT_REGISTRY(256))
OPEN(NEWUNIT=CSV_UNIT,FILE=TRIM(COUTVAR_TABLE),STATUS='OLD',ACTION='READ',IOSTAT=IOS)
IF (IOS/=0) THEN
  LOAD_ERROR='Cannot open output registry: '//TRIM(COUTVAR_TABLE)
  DEALLOCATE(OUTPUT_REGISTRY); CALL OUTPUT_UTIL_FATAL(LOAD_ERROR)
ENDIF
READ(CSV_UNIT,'(A)',IOSTAT=IOS) REGISTRY_LINE
IF (IOS/=0) THEN
  LOAD_ERROR='Cannot read output registry header: '//TRIM(COUTVAR_TABLE)
  CLOSE(CSV_UNIT); DEALLOCATE(OUTPUT_REGISTRY); CALL OUTPUT_UTIL_FATAL(LOAD_ERROR)
ENDIF
CALL SPLIT_CSV_RECORD(TRIM(REGISTRY_LINE),CSV_HEADER,NHEADER,PARSE_STATUS)
IF (PARSE_STATUS/=0) THEN
  LOAD_ERROR='Malformed or oversized output registry header.'
  CLOSE(CSV_UNIT); DEALLOCATE(OUTPUT_REGISTRY); CALL OUTPUT_UTIL_FATAL(LOAD_ERROR)
ENDIF
IDX_NAME=CSV_COLUMN(CSV_HEADER,NHEADER,'name'); IDX_ALIAS=CSV_COLUMN(CSV_HEADER,NHEADER,'alias_of')
IDX_LONG=CSV_COLUMN(CSV_HEADER,NHEADER,'long_name'); IDX_UNITS=CSV_COLUMN(CSV_HEADER,NHEADER,'units')
IDX_TIME=CSV_COLUMN(CSV_HEADER,NHEADER,'temporal_type'); IDX_MISSING=CSV_COLUMN(CSV_HEADER,NHEADER,'missing_value')
IDX_OPTION=CSV_COLUMN(CSV_HEADER,NHEADER,'required_option')
IF (MIN(IDX_NAME,IDX_ALIAS,IDX_LONG,IDX_UNITS,IDX_TIME,IDX_MISSING,IDX_OPTION)==0) THEN
  LOAD_ERROR='Output registry is missing a required metadata header: '//TRIM(COUTVAR_TABLE)
  CLOSE(CSV_UNIT); DEALLOCATE(OUTPUT_REGISTRY); CALL OUTPUT_UTIL_FATAL(LOAD_ERROR)
ENDIF
NROWS=0
DO
  READ(CSV_UNIT,'(A)',IOSTAT=IOS) REGISTRY_LINE
  IF (IOS/=0) EXIT
  IF (LEN_TRIM(REGISTRY_LINE)==0) CYCLE
  CALL SPLIT_CSV_RECORD(TRIM(REGISTRY_LINE),CSV_FIELDS,NFIELDS,PARSE_STATUS)
  IF (PARSE_STATUS/=0) THEN
    LOAD_ERROR='Malformed or oversized output registry row.'
    EXIT
  ENDIF
  IF (NFIELDS<NHEADER) THEN
    LOAD_ERROR='Output registry row has fewer fields than its header.'
    EXIT
  ENDIF
  IF (NROWS>=SIZE(OUTPUT_REGISTRY)) THEN
    LOAD_ERROR='Output registry exceeds the supported row count.'
    EXIT
  ENDIF
  NROWS=NROWS+1
  OUTPUT_REGISTRY(NROWS)%NAME=TRIM(CSV_FIELDS(IDX_NAME))
  OUTPUT_REGISTRY(NROWS)%ALIAS_OF=TRIM(CSV_FIELDS(IDX_ALIAS))
  OUTPUT_REGISTRY(NROWS)%LONG_NAME=TRIM(CSV_FIELDS(IDX_LONG))
  OUTPUT_REGISTRY(NROWS)%UNITS=TRIM(CSV_FIELDS(IDX_UNITS))
  OUTPUT_REGISTRY(NROWS)%TEMPORAL_TYPE=TRIM(CSV_FIELDS(IDX_TIME))
  OUTPUT_REGISTRY(NROWS)%REQUIRED_OPTION=TRIM(CSV_FIELDS(IDX_OPTION))
  OUTPUT_REGISTRY(NROWS)%MISSING_VALUE=RMIS
  OUTPUT_REGISTRY(NROWS)%HAS_MISSING_VALUE=.FALSE.
  IF (LEN_TRIM(OUTPUT_REGISTRY(NROWS)%NAME)==0) THEN
    LOAD_ERROR='Output registry contains a blank name.'
    EXIT
  ENDIF
  DO ROW=1,NROWS-1
    IF (TRIM(OUTPUT_REGISTRY(ROW)%NAME)==TRIM(OUTPUT_REGISTRY(NROWS)%NAME)) THEN
      LOAD_ERROR='Duplicate output registry definition: '//TRIM(OUTPUT_REGISTRY(NROWS)%NAME)
      EXIT
    ENDIF
  ENDDO
  IF (LEN_TRIM(LOAD_ERROR)>0) EXIT
  IF (LEN_TRIM(OUTPUT_REGISTRY(NROWS)%ALIAS_OF)==0) THEN
    IF (LEN_TRIM(OUTPUT_REGISTRY(NROWS)%LONG_NAME)==0 .OR. &
        LEN_TRIM(OUTPUT_REGISTRY(NROWS)%UNITS)==0) THEN
      LOAD_ERROR='Required output metadata is blank for '//TRIM(OUTPUT_REGISTRY(NROWS)%NAME)
      EXIT
    ENDIF
    SELECT CASE (TRIM(OUTPUT_REGISTRY(NROWS)%TEMPORAL_TYPE))
      CASE ('snapshot','point','mean','maximum')
      CASE DEFAULT
        LOAD_ERROR='Invalid temporal_type for '//TRIM(OUTPUT_REGISTRY(NROWS)%NAME)
        EXIT
    END SELECT
    IF (LEN_TRIM(CSV_FIELDS(IDX_MISSING))>0) THEN
      READ(CSV_FIELDS(IDX_MISSING),*,IOSTAT=IOS) SENTINEL
      IF (IOS/=0) THEN
        LOAD_ERROR='Invalid missing_value for '//TRIM(OUTPUT_REGISTRY(NROWS)%NAME)
        EXIT
      ENDIF
      OUTPUT_REGISTRY(NROWS)%MISSING_VALUE=SENTINEL
      OUTPUT_REGISTRY(NROWS)%HAS_MISSING_VALUE=.TRUE.
    ELSEIF (TRIM(OUTPUT_REGISTRY(NROWS)%NAME)/='pthflw') THEN
      LOAD_ERROR='Required missing_value is blank for '//TRIM(OUTPUT_REGISTRY(NROWS)%NAME)
      EXIT
    ENDIF
    IF (LEN_TRIM(OUTPUT_REGISTRY(NROWS)%REQUIRED_OPTION)==0) &
      OUTPUT_REGISTRY(NROWS)%REQUIRED_OPTION='none'
  ENDIF
ENDDO
CLOSE(CSV_UNIT)
IF (LEN_TRIM(LOAD_ERROR)>0) THEN
  DEALLOCATE(OUTPUT_REGISTRY); CALL OUTPUT_UTIL_FATAL(LOAD_ERROR)
ENDIF
ALLOCATE(TEMP_REGISTRY(NROWS)); TEMP_REGISTRY=OUTPUT_REGISTRY(1:NROWS)
CALL MOVE_ALLOC(TEMP_REGISTRY,OUTPUT_REGISTRY)
DO ROW=1,NROWS
  IF (LEN_TRIM(OUTPUT_REGISTRY(ROW)%ALIAS_OF)==0) CYCLE
  CANON=0
  DO COL=1,NROWS
    IF (TRIM(OUTPUT_REGISTRY(COL)%NAME)==TRIM(OUTPUT_REGISTRY(ROW)%ALIAS_OF)) CANON=COL
  ENDDO
  IF (CANON==0) THEN
    LOAD_ERROR='Unknown alias_of target for '//TRIM(OUTPUT_REGISTRY(ROW)%NAME)
    EXIT
  ENDIF
  IF (LEN_TRIM(OUTPUT_REGISTRY(CANON)%ALIAS_OF)>0) THEN
    LOAD_ERROR='Alias chains are not supported: '//TRIM(OUTPUT_REGISTRY(ROW)%NAME)
    EXIT
  ENDIF
  IF (LEN_TRIM(OUTPUT_REGISTRY(ROW)%REQUIRED_OPTION)>0 .AND. &
      TRIM(OUTPUT_REGISTRY(ROW)%REQUIRED_OPTION)/='none' .AND. &
      TRIM(OUTPUT_REGISTRY(ROW)%REQUIRED_OPTION)/=TRIM(OUTPUT_REGISTRY(CANON)%REQUIRED_OPTION)) THEN
    LOAD_ERROR='Alias required_option conflicts with canonical row: '//TRIM(OUTPUT_REGISTRY(ROW)%NAME)
    EXIT
  ENDIF
  OUTPUT_REGISTRY(ROW)%LONG_NAME=OUTPUT_REGISTRY(CANON)%LONG_NAME
  OUTPUT_REGISTRY(ROW)%UNITS=OUTPUT_REGISTRY(CANON)%UNITS
  OUTPUT_REGISTRY(ROW)%TEMPORAL_TYPE=OUTPUT_REGISTRY(CANON)%TEMPORAL_TYPE
  OUTPUT_REGISTRY(ROW)%MISSING_VALUE=OUTPUT_REGISTRY(CANON)%MISSING_VALUE
  OUTPUT_REGISTRY(ROW)%HAS_MISSING_VALUE=OUTPUT_REGISTRY(CANON)%HAS_MISSING_VALUE
  OUTPUT_REGISTRY(ROW)%REQUIRED_OPTION=OUTPUT_REGISTRY(CANON)%REQUIRED_OPTION
ENDDO
IF (LEN_TRIM(LOAD_ERROR)>0) THEN
  DEALLOCATE(OUTPUT_REGISTRY)
  CALL OUTPUT_UTIL_FATAL(LOAD_ERROR)
ENDIF
END SUBROUTINE OUTPUT_UTIL_LOAD_REGISTRY
!####################################################################

!####################################################################
SUBROUTINE OUTPUT_UTIL_LOAD_METADATA(NAME,LONG_NAME,UNITS,AGGREGATE,MISSING_VALUE,HAS_MISSING_VALUE, &
                                REQUIRED_OPTION)
! Return the resolved metadata for one selected output name.
IMPLICIT NONE
CHARACTER(LEN=*),INTENT(IN)    :: NAME
CHARACTER(LEN=*),INTENT(OUT)   :: LONG_NAME,UNITS,REQUIRED_OPTION
INTEGER(KIND=JPIM),INTENT(OUT) :: AGGREGATE
REAL(KIND=JPRM),INTENT(OUT)    :: MISSING_VALUE
LOGICAL,INTENT(OUT)            :: HAS_MISSING_VALUE

INTEGER(KIND=JPIM)             :: ROW
!================================================
HAS_MISSING_VALUE=.FALSE.
DO ROW=1,SIZE(OUTPUT_REGISTRY)
  IF (TRIM(OUTPUT_REGISTRY(ROW)%NAME)/=TRIM(NAME)) CYCLE
  LONG_NAME=OUTPUT_REGISTRY(ROW)%LONG_NAME; UNITS=OUTPUT_REGISTRY(ROW)%UNITS
  REQUIRED_OPTION=OUTPUT_REGISTRY(ROW)%REQUIRED_OPTION
  AGGREGATE=1
  SELECT CASE (TRIM(OUTPUT_REGISTRY(ROW)%TEMPORAL_TYPE))
    CASE ('snapshot','point'); AGGREGATE=1
    CASE ('mean'); AGGREGATE=2
    CASE ('maximum'); AGGREGATE=3
  END SELECT
  MISSING_VALUE=OUTPUT_REGISTRY(ROW)%MISSING_VALUE
  HAS_MISSING_VALUE=OUTPUT_REGISTRY(ROW)%HAS_MISSING_VALUE
  RETURN
ENDDO
CALL OUTPUT_UTIL_FATAL('Unknown output name: '//TRIM(NAME))
END SUBROUTINE OUTPUT_UTIL_LOAD_METADATA

SUBROUTINE OUTPUT_UTIL_CHECK_OPTION(NAME,REQUIRED_OPTION)
! Report whether the output's required model option is enabled.
USE YOS_CMF_INPUT,           ONLY: LOUTINS

IMPLICIT NONE
CHARACTER(LEN=*),INTENT(IN)  :: NAME,REQUIRED_OPTION
LOGICAL                      :: ENABLED
!================================================
ENABLED=.TRUE.
SELECT CASE (TRIM(REQUIRED_OPTION))
  CASE ('none',''); RETURN
  CASE ('LPTHOUT'); ENABLED=LPTHOUT
  CASE ('LDAMOUT'); ENABLED=LDAMOUT
  CASE ('LLEVEE'); ENABLED=LLEVEE
  CASE ('LGDWDLY'); ENABLED=LGDWDLY
  CASE ('LWEVAP'); ENABLED=LWEVAP
  CASE ('LOUTINS'); ENABLED=LOUTINS
  CASE ('LROSPLIT'); ENABLED=LROSPLIT
  CASE DEFAULT
    CALL OUTPUT_UTIL_FATAL('Unsupported required_option '//TRIM(REQUIRED_OPTION)//' for '//TRIM(NAME))
END SELECT
IF (.NOT.ENABLED) CALL OUTPUT_UTIL_FATAL(TRIM(NAME)//' requires '//TRIM(REQUIRED_OPTION)//'=.TRUE.')
END SUBROUTINE OUTPUT_UTIL_CHECK_OPTION
!####################################################################

!####################################################################
SUBROUTINE OUTPUT_UTIL_WRITE_R4(NAME,ARRAY)
! Write a JPRM output array.
USE YOS_CMF_INPUT,           ONLY: NX,NY
USE YOS_CMF_MAP,             ONLY: NSEQMAX,NPTHOUT,NPTHLEV
USE CMF_UTILS_MOD,           ONLY: vec2map_R2R
#ifdef UseMPI_CMF
USE CMF_CTRL_MPI_MOD,        ONLY: CMF_MPI_AllReduce_R2MAP
#endif

IMPLICIT NONE
CHARACTER(LEN=*),INTENT(IN) :: NAME
REAL(KIND=JPRM),INTENT(IN)  :: ARRAY(:,:)

REAL(KIND=JPRM)             :: MAP(NX,NY)
INTEGER(KIND=JPIM)          :: JF
!================================================
JF=OUTPUT_UTIL_FIND_OUTPUT(NAME)
IF (JF==0) STOP 'OUTPUT_WRITE: requested name is not initialized'
IF (TRIM(NAME)=='pthflw') THEN
  IF (SIZE(ARRAY,1)<NPTHOUT .OR. SIZE(ARRAY,2)<NPTHLEV) STOP 'OUTPUT_WRITE: invalid pthflw array shape'
  CALL OUTPUT_UTIL_WRITE_PTH(JF,ARRAY(1:NPTHOUT,1:NPTHLEV))
  RETURN
ENDIF
IF (SIZE(ARRAY,1)<NSEQMAX .OR. SIZE(ARRAY,2)<1) STOP 'OUTPUT_WRITE: invalid unit-catchment array shape'
IF (LOUTVEC .AND. .NOT.LOUTCDF) THEN
  WRITE(VAROUT(JF)%BINID,REC=IRECOUT) ARRAY(1:NSEQMAX,1)
  RETURN
ENDIF
CALL vec2map_R2R(ARRAY(1:NSEQMAX,1),MAP)
#ifdef UseMPI_CMF
CALL CMF_MPI_AllReduce_R2MAP(MAP)
#endif
CALL OUTPUT_UTIL_WRITE_MAP(JF,MAP)
END SUBROUTINE OUTPUT_UTIL_WRITE_R4
!####################################################################

!####################################################################
SUBROUTINE OUTPUT_UTIL_WRITE_R8(NAME,ARRAY)
! Convert and write a JPRD output array.
USE YOS_CMF_INPUT,           ONLY: NX,NY
USE YOS_CMF_MAP,             ONLY: NSEQMAX,NPTHOUT,NPTHLEV
USE CMF_UTILS_MOD,           ONLY: vec2map_D2R
#ifdef UseMPI_CMF
USE CMF_CTRL_MPI_MOD,        ONLY: CMF_MPI_AllReduce_R2MAP
#endif

IMPLICIT NONE
CHARACTER(LEN=*),INTENT(IN) :: NAME
REAL(KIND=JPRD),INTENT(IN)  :: ARRAY(:,:)

REAL(KIND=JPRM)             :: MAP(NX,NY),VEC(NSEQMAX),PTH(NPTHOUT,NPTHLEV)
INTEGER(KIND=JPIM)          :: JF
!================================================
JF=OUTPUT_UTIL_FIND_OUTPUT(NAME)
IF (JF==0) STOP 'OUTPUT_WRITE: requested name is not initialized'
IF (TRIM(NAME)=='pthflw') THEN
  IF (SIZE(ARRAY,1)<NPTHOUT .OR. SIZE(ARRAY,2)<NPTHLEV) STOP 'OUTPUT_WRITE: invalid pthflw array shape'
  PTH=REAL(ARRAY(1:NPTHOUT,1:NPTHLEV),KIND=JPRM)
  CALL OUTPUT_UTIL_WRITE_PTH(JF,PTH)
  RETURN
ENDIF
IF (SIZE(ARRAY,1)<NSEQMAX .OR. SIZE(ARRAY,2)<1) STOP 'OUTPUT_WRITE: invalid unit-catchment array shape'
IF (LOUTVEC .AND. .NOT.LOUTCDF) THEN
  VEC=REAL(ARRAY(1:NSEQMAX,1),KIND=JPRM)
  WRITE(VAROUT(JF)%BINID,REC=IRECOUT) VEC
  RETURN
ENDIF
CALL vec2map_D2R(ARRAY(1:NSEQMAX,1),MAP)
#ifdef UseMPI_CMF
CALL CMF_MPI_AllReduce_R2MAP(MAP)
#endif
CALL OUTPUT_UTIL_WRITE_MAP(JF,MAP)
END SUBROUTINE OUTPUT_UTIL_WRITE_R8

INTEGER(KIND=JPIM) FUNCTION OUTPUT_UTIL_FIND_OUTPUT(NAME)
! Return the output-table index matching NAME, or zero when absent.
IMPLICIT NONE
CHARACTER(LEN=*),INTENT(IN) :: NAME

INTEGER(KIND=JPIM)          :: JF
!================================================
OUTPUT_UTIL_FIND_OUTPUT=0
IF (.NOT.ALLOCATED(VAROUT)) RETURN
DO JF=1,NVARSOUT
  IF (TRIM(VAROUT(JF)%CVNAME)==TRIM(NAME)) THEN
    OUTPUT_UTIL_FIND_OUTPUT=JF
    RETURN
  ENDIF
ENDDO
END FUNCTION OUTPUT_UTIL_FIND_OUTPUT
!####################################################################

!####################################################################
SUBROUTINE OUTPUT_UTIL_WRITE_MAP(JF,MAP)
! Write one map output record in the selected format.
#ifdef UseCDF_CMF
USE NETCDF
USE YOS_CMF_TIME,            ONLY: KMINSTART,KMINNEXT
USE CMF_UTILS_MOD,           ONLY: NCERROR
#endif
USE YOS_CMF_MAP,             ONLY: REGIONTHIS

IMPLICIT NONE
INTEGER(KIND=JPIM),INTENT(IN) :: JF
REAL(KIND=JPRM),INTENT(IN)    :: MAP(:,:)
#ifdef UseCDF_CMF
REAL(KIND=JPRB) :: XTIME
#endif
!================================================
IF (LOUTCDF) THEN
#ifdef UseCDF_CMF
  IF (REGIONTHIS==1) THEN
    XTIME=REAL(KMINNEXT-KMINSTART,JPRB)*60._JPRB
    CALL NCERROR(NF90_PUT_VAR(VAROUT(JF)%NCID,VAROUT(JF)%TIMID,XTIME,(/VAROUT(JF)%IRECNC/)))
    CALL NCERROR(NF90_PUT_VAR(VAROUT(JF)%NCID,VAROUT(JF)%VARID,MAP, &
      (/1,1,VAROUT(JF)%IRECNC/),(/SIZE(MAP,1),SIZE(MAP,2),1/)))
    VAROUT(JF)%IRECNC=VAROUT(JF)%IRECNC+1
  ENDIF
#endif
ELSEIF (REGIONTHIS==1) THEN
  CALL OUTPUT_UTIL_WRITE_BINARY_MAP(VAROUT(JF)%BINID,IRECOUT,MAP)
ENDIF
END SUBROUTINE OUTPUT_UTIL_WRITE_MAP
!####################################################################

!####################################################################
SUBROUTINE OUTPUT_UTIL_WRITE_PTH(JF,DATA)
! Write one bifurcation-path output record in the selected format.
#ifdef UseCDF_CMF
USE NETCDF
USE YOS_CMF_TIME,            ONLY: KMINSTART,KMINNEXT
USE CMF_UTILS_MOD,           ONLY: NCERROR
#endif
USE YOS_CMF_MAP,             ONLY: REGIONTHIS,NPTHOUT,NPTHLEV
#ifdef UseMPI_CMF
USE CMF_CTRL_MPI_MOD,        ONLY: CMF_MPI_AllReduce_R1PTH
#endif

IMPLICIT NONE
INTEGER(KIND=JPIM),INTENT(IN) :: JF
REAL(KIND=JPRM),INTENT(IN)    :: DATA(NPTHOUT,NPTHLEV)

REAL(KIND=JPRM)               :: PTH(NPTHOUT,NPTHLEV)
#ifdef UseCDF_CMF
REAL(KIND=JPRB) :: XTIME
#endif
!================================================
PTH=DATA
#ifdef UseMPI_CMF
CALL CMF_MPI_AllReduce_R1PTH(PTH)
#endif
IF (REGIONTHIS/=1) RETURN
IF (LOUTCDF) THEN
#ifdef UseCDF_CMF
  XTIME=REAL(KMINNEXT-KMINSTART,JPRB)*60._JPRB
  CALL NCERROR(NF90_PUT_VAR(VAROUT(JF)%NCID,VAROUT(JF)%TIMID,XTIME,(/VAROUT(JF)%IRECNC/)))
  CALL NCERROR(NF90_PUT_VAR(VAROUT(JF)%NCID,VAROUT(JF)%VARID,PTH, &
                            (/1,1,VAROUT(JF)%IRECNC/),(/NPTHOUT,NPTHLEV,1/)))
  VAROUT(JF)%IRECNC=VAROUT(JF)%IRECNC+1
#endif
ELSE
  CALL OUTPUT_UTIL_WRITE_BINARY_PTH(VAROUT(JF)%BINID,IRECOUT,PTH)
ENDIF
END SUBROUTINE OUTPUT_UTIL_WRITE_PTH
!####################################################################

!####################################################################
SUBROUTINE OUTPUT_UTIL_WRITE_BINARY_MAP(IFN,IREC,MAP)
! Write one map record to a direct-access Binary file.
IMPLICIT NONE
INTEGER(KIND=JPIM),INTENT(IN) :: IFN,IREC
REAL(KIND=JPRM),INTENT(IN)    :: MAP(:,:)
!================================================
WRITE(IFN,REC=IREC) MAP
END SUBROUTINE OUTPUT_UTIL_WRITE_BINARY_MAP

SUBROUTINE OUTPUT_UTIL_WRITE_BINARY_PTH(IFN,IREC,MAP)
! Write one bifurcation-path record to a direct-access Binary file.
USE YOS_CMF_MAP,             ONLY: NPTHOUT,NPTHLEV

IMPLICIT NONE
INTEGER(KIND=JPIM),INTENT(IN) :: IFN,IREC
REAL(KIND=JPRM),INTENT(IN)    :: MAP(NPTHOUT,NPTHLEV)
!================================================
WRITE(IFN,REC=IREC) MAP
END SUBROUTINE OUTPUT_UTIL_WRITE_BINARY_PTH

SUBROUTINE OUTPUT_UTIL_WRITE_VECTOR_INDEX
! Write the index-to-map table needed to reconstruct vector output as a map.
USE YOS_CMF_INPUT,           ONLY: TMPNAM
USE YOS_CMF_MAP,             ONLY: I1SEQX, I1SEQY, NSEQMAX
USE CMF_UTILS_MOD,           ONLY: INQUIRE_FID

IMPLICIT NONE
!* local variable
CHARACTER(LEN=256) :: CFILE1
!================================================
IF( LOUTVEC )THEN
  CFILE1='./ind_xy'//TRIM(CSUFVEC)

  WRITE(LOGNAM,*) "LOUTVEC: write mapR2vecD conversion table", TRIM(CFILE1)

  TMPNAM=INQUIRE_FID()
  OPEN(TMPNAM,FILE=CFILE1,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4*NSEQMAX)
  WRITE(TMPNAM,REC=1) I1SEQX
  WRITE(TMPNAM,REC=2) I1SEQY
  CLOSE(TMPNAM)
ENDIF

END SUBROUTINE OUTPUT_UTIL_WRITE_VECTOR_INDEX

END MODULE CMF_CTRL_OUTPUT_MOD
