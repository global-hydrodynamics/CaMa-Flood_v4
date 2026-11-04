MODULE CMF_CTRL_FORCING_MOD
!==========================================================
!* PURPOSE: Manage CaMa-Flood forcing
!
!* CONTAINS:
! -- CMF_FORCING_NMLIST : Read and validate forcing Namelist settings
! -- CMF_FORCING_INIT   : Initialize forcing and input-matrix sources
! -- CMF_FORCING_GET    : Read and sanitize forcing fields
! -- CMF_FORCING_COM    : Interpolate flood fraction to the runoff grid
! -- CMF_FORCING_PUT    : Map runoff fields to CaMa-Flood catchments
! -- CMF_FORCING_END    : Close forcing files
!
!* SHARED UTILITIES (after CMF_FORCING_END, in source order):
! -- FORCING_UTIL_UPPER        : Normalize a configuration value
! -- FORCING_UTIL_RESOLVE_FILE : Resolve grouped filename and record index
! -- FORCING_UTIL_READ_BIN     : Reuse a direct-access Binary input file
!
! (C) D.Yamazaki & E. Dutra  (U-Tokyo/FCUL)  Aug 2019
!
! Modifications: I. Ayan-Miguez (BSC) Apr 2023: Read inpmat.nc matrix by layers and added LECMF2LAKEC switch
!
! Licensed under the Apache License, Version 2.0 (the "License");
!   You may not use this file except in compliance with the License.
!   You may obtain a copy of the License at: http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software distributed under the License is
!  distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and limitations under the License.
!==========================================================
USE PARKIND1,                ONLY: JPIM, JPIB, JPRB, JPRM
USE YOS_CMF_INPUT,           ONLY: LOGNAM
USE YOS_CMF_MAP,             ONLY: INPX, INPY, INPA, INPXI, INPYI, INPAI, INPNI
#ifdef UseCDF_CMF
USE cmf_cf_time_mod,         ONLY: cf_time_axis, cf_resolve_time_record, cf_find_time_record
#endif

IMPLICIT NONE
SAVE
!*** NAMELIST/NFORCE/: Runoff source, grouping, and input-matrix settings
LOGICAL                         :: LINPCDF             !! TRUE: read runoff from NetCDF; FALSE: read Binary runoff
LOGICAL                         :: LINPEND             !! TRUE: byte-swap Binary runoff fields on input
CHARACTER(LEN=16)               :: CROFFREQ            !! Runoff file grouping: DAY, MON, YEAR, or SEQ

LOGICAL                         :: LINTERP             !! TRUE: map runoff with the configured input matrix
LOGICAL                         :: LITRPCDF            !! TRUE: read the input matrix from NetCDF
CHARACTER(LEN=256)              :: CINPMAT             !! Runoff-to-map input-matrix filename
REAL(KIND=JPRB)                 :: DROFUNIT            !! Divide input runoff by this factor to obtain m/s

!*** Binary forcing paths; CSUB* is used for split runoff when LROSPLIT is enabled.
CHARACTER(LEN=256)              :: CROFDIR             !! Directory containing total or surface runoff files
CHARACTER(LEN=256)              :: CROFPRE             !! Prefix before the date tag in grouped runoff filenames
CHARACTER(LEN=256)              :: CROFSUF             !! Suffix after the date tag in grouped runoff filenames
CHARACTER(LEN=256)              :: CSUBDIR             !! Directory containing subsurface runoff files
CHARACTER(LEN=256)              :: CSUBPRE             !! Prefix before the date tag in subsurface runoff filenames
CHARACTER(LEN=256)              :: CSUBSUF             !! Suffix after the date tag in subsurface runoff filenames

!*** NetCDF forcing variables and optional explicit SEQ start date.
CHARACTER(LEN=256)              :: CROFCDF             !! NetCDF file used by SEQ grouping
CHARACTER(LEN=256)              :: CVNTIME             !! NetCDF time-dimension name
CHARACTER(LEN=256)              :: CVNROF              !! NetCDF total-runoff variable name
CHARACTER(LEN=256)              :: CVNSUB              !! NetCDF subsurface-runoff variable name
INTEGER(KIND=JPIM)              :: SYEARIN             !! First-record year for SEQ; <=0 selects NetCDF CF time
INTEGER(KIND=JPIM)              :: SMONIN              !! First-record month for an explicitly dated SEQ file
INTEGER(KIND=JPIM)              :: SDAYIN              !! First-record day for an explicitly dated SEQ file
INTEGER(KIND=JPIM)              :: SHOURIN             !! First-record hour for an explicitly dated SEQ file

!*** Register the forcing settings in NAMELIST/NFORCE/.
NAMELIST/NFORCE/ LINTERP, LINPEND, LINPCDF, LITRPCDF, CROFFREQ, CINPMAT, DROFUNIT, &
  CROFDIR, CROFPRE, CROFSUF, CSUBDIR, CSUBPRE, CSUBSUF, &
  CROFCDF, CVNTIME, CVNROF, CVNSUB, SYEARIN, SMONIN, SDAYIN, SHOURIN

!*** Module state for cached Binary forcing files.
INTEGER(KIND=JPIM)              :: IBINUNIT(2)=0
LOGICAL                         :: LBINOPEN(2)=.FALSE.
CHARACTER(LEN=512)              :: CBINNAME(2)=' '

#ifdef UseCDF_CMF
TYPE TYPEROF
  CHARACTER(LEN=256)              :: CNAME        !! Current NetCDF runoff filename
  CHARACTER(LEN=256)              :: CVAR(3)      !! NetCDF variable names for runoff fields
  INTEGER(KIND=JPIM)              :: NCID         !! Open NetCDF file ID
  INTEGER(KIND=JPIM)              :: NVARID(3)    !! NetCDF variable IDs
  INTEGER(KIND=JPIM)              :: NSTART       !! Start minute for the cached file
  INTEGER(KIND=JPIM)              :: NRECSTART    !! First record corresponding to the current run time
  LOGICAL                         :: LAUTOTIME    !! TRUE: resolve records using the NetCDF CF time axis
END TYPE TYPEROF
TYPE(TYPEROF)                   :: ROFCDF      !! Derived type for Runoff input
TYPE(cf_time_axis)              :: ROF_TIME_AXIS
#endif
CONTAINS
!####################################################################
! Principal model procedures
! -- CMF_FORCING_NMLIST : Read and validate forcing Namelist settings
! -- CMF_FORCING_INIT   : Initialize forcing and input-matrix sources
! -- CMF_FORCING_GET    : Read and sanitize forcing fields
! -- CMF_FORCING_COM    : Interpolate flood fraction to the runoff grid
! -- CMF_FORCING_PUT    : Map runoff fields to CaMa-Flood catchments
! -- CMF_FORCING_END    : Close forcing files
!
! Shared module-level utilities
! -- FORCING_UTIL_UPPER        : Normalize a configuration value
! -- FORCING_UTIL_RESOLVE_FILE : Resolve grouped filename and record index
! -- FORCING_UTIL_READ_BIN     : Reuse a direct-access Binary input file
!####################################################################
SUBROUTINE CMF_FORCING_NMLIST
! Read forcing settings and update the shared model time origin when needed.
! -- Called from CMF_DRV_NMLIST
USE YOS_CMF_INPUT,           ONLY: CSETFILE, NSETFILE, LROSPLIT, DTIN
USE YOS_CMF_TIME,            ONLY: YYYY0, KMINSTAIN
USE CMF_UTILS_MOD,           ONLY: INQUIRE_FID, DATE2MIN, IMDAYS

IMPLICIT NONE
INTEGER(KIND=JPIM)              :: IYEARIN, IMONIN, IDAYIN
INTEGER(KIND=JPIM)              :: IDTIN, INMLIOS
LOGICAL                         :: LSTART_DATE_SET
CHARACTER(LEN=256)              :: CNMLMSG  !! Namelist read diagnostic
!================================================
WRITE(LOGNAM,*) ''
WRITE(LOGNAM,*) '!---------------------!'
WRITE(LOGNAM,*) 'CMF::FORCING_NMLIST: START'

!*** 1. Open the namelist file
NSETFILE=INQUIRE_FID()
OPEN(NSETFILE,FILE=CSETFILE,STATUS="OLD")
WRITE(LOGNAM,*) 'CMF::FORCING_NMLIST: opened settings file ',TRIM(CSETFILE),' on unit ',NSETFILE

!*** 2. Set default forcing and input-matrix options
LINPCDF =.FALSE.
LINPEND =.FALSE.
CROFFREQ=' '
LINTERP =.FALSE.
LITRPCDF=.FALSE.
CINPMAT ="NONE"
DROFUNIT=86400*1000._JPRB     !! defaults input [mm/day] / DRofUnit => [m/s]

CROFDIR="./runoff/"
CROFPRE="Roff____"           !! defaults runoff file name Roff____YYYYMMDD.one
CROFSUF=".one"

CSUBDIR="./runoff/"
CSUBPRE="Rsub____"           !! defaults runoff file name Rsub____YYYYMMDD.one
CSUBSUF=".one"

CROFCDF="NONE"
CVNTIME="time"
CVNROF ="runoff"
CVNSUB ="NONE"
IF( LROSPLIT )THEN
  ! Split-runoff products use the standard surface (Qs) and subsurface (Qsb) variables.
  CVNROF="Qs"
  CVNSUB="Qsb"
ENDIF

SYEARIN=-9999
SMONIN =-9999
SDAYIN =-9999
SHOURIN=-9999

!*** 3. Read settings and validate the file-grouping configuration
REWIND(NSETFILE)
READ(NSETFILE,NML=NFORCE,IOSTAT=INMLIOS,IOMSG=CNMLMSG)
IF( INMLIOS/=0 ) THEN
  WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_NMLIST]: invalid NFORCE namelist: ', &
  & TRIM(CNMLMSG)
  STOP 9
ENDIF
#ifndef UseCDF_CMF
IF( LINPCDF ) THEN
  WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_NMLIST]: LINPCDF requires a NetCDF-enabled build.'
  STOP 9
ENDIF
#endif
IF( LEN_TRIM(CROFFREQ)==0 ) THEN
  WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_NMLIST]: CROFFREQ is required; ', &
  & 'choose DAY, MON, YEAR, or SEQ.'
  STOP 9
ENDIF
CROFFREQ=TRIM(FORCING_UTIL_UPPER(ADJUSTL(CROFFREQ)))
IF( TRIM(CROFFREQ)/='DAY' .AND. TRIM(CROFFREQ)/='MON' .AND. &
&   TRIM(CROFFREQ)/='YEAR' .AND. TRIM(CROFFREQ)/='SEQ' ) THEN
  WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_NMLIST]: CROFFREQ must be ', &
  & 'DAY, MON, YEAR, or SEQ: ',TRIM(CROFFREQ)
  STOP 9
ENDIF

! Sentinel defaults distinguish omitted date fields from explicit assignments.
LSTART_DATE_SET=SYEARIN/=-9999 .OR. SMONIN/=-9999 .OR. SDAYIN/=-9999 .OR. SHOURIN/=-9999
IF( TRIM(CROFFREQ)/='SEQ' .AND. LSTART_DATE_SET ) THEN
  WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_NMLIST]: explicit start-date fields ', &
  & 'are only valid with CROFFREQ=SEQ.'
  STOP 9
ENDIF
IF( .NOT. LSTART_DATE_SET ) THEN
  SYEARIN=0
  SMONIN=0
  SDAYIN=0
  SHOURIN=0
ENDIF
IF( TRIM(CROFFREQ)=='SEQ' ) THEN
  IF( LINPCDF .AND. SYEARIN<=0 ) THEN
    IF( SMONIN/=-9999 .OR. SDAYIN/=-9999 .OR. SHOURIN/=-9999 ) THEN
      WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_NMLIST]: NetCDF SEQ date fields ', &
      & 'must be omitted when SYEARIN<=0.'
      STOP 9
    ENDIF
    SMONIN=0
    SDAYIN=0
    SHOURIN=0
  ELSEIF( SYEARIN>0 ) THEN
    IF( SMONIN==-9999 .OR. SDAYIN==-9999 .OR. SHOURIN==-9999 ) THEN
      WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_NMLIST]: explicit SEQ start ', &
      & 'requires all four date fields.'
      STOP 9
    ENDIF
  ELSEIF( .NOT. LINPCDF ) THEN
    WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_NMLIST]: Binary SEQ requires ', &
    & 'explicit SYEARIN, SMONIN, SDAYIN, and SHOURIN.'
    STOP 9
  ENDIF
ENDIF

IF( TRIM(CROFFREQ)/='SEQ' ) THEN
  IDTIN=INT(DTIN,KIND=JPIM)
  IF( DTIN<=0._JPRB .OR. REAL(IDTIN,KIND=JPRB)/=DTIN ) THEN
    WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_NMLIST]: IFRQ_INP must define ', &
    & 'a positive integer-second interval.'
    STOP 9
  ENDIF
  IF( MOD(86400,IDTIN)/=0 ) THEN
    WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_NMLIST]: DAY/MON/YEAR requires ', &
    & 'IFRQ_INP to divide 24 hours.'
    STOP 9
  ENDIF
ENDIF
!*** 4. Validate an explicit SEQ start and apply it to the model time origin
! Explicit first-record dates are supported only for SEQ; grouped files use period boundaries.
IF( SYEARIN>0 .AND. TRIM(CROFFREQ)=='SEQ' ) THEN
  IYEARIN=SYEARIN
  IMONIN=SMONIN
  IDAYIN=SDAYIN
  IF( IYEARIN<1 .OR. IMONIN<1 .OR. IMONIN>12 .OR. IDAYIN<1 .OR. &
  &   SHOURIN<0 .OR. SHOURIN>23 ) THEN
    WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_NMLIST]: invalid explicit first-record datetime.'
    STOP 9
  ENDIF
  IF( IDAYIN>IMDAYS(IYEARIN,IMONIN) ) THEN
    WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_NMLIST]: invalid day in explicit first-record datetime.'
    STOP 9
  ENDIF
  ! DATE2MIN rejects dates before YYYY0, so include the forcing start year in its calendar base.
  ! KMINSTAIN then anchors input-record offsets at the first forcing record.
  YYYY0=MIN(YYYY0,SYEARIN)
  KMINSTAIN=DATE2MIN(SYEARIN*10000+SMONIN*100+SDAYIN,SHOURIN*100)
ENDIF

!*** 5. Report the selected forcing configuration and close the Namelist
WRITE(LOGNAM,*) 'CMF::FORCING_NMLIST: NFORCE settings'
WRITE(LOGNAM,*) 'CMF::FORCING_NMLIST: LINPCDF  =',LINPCDF
WRITE(LOGNAM,*) 'CMF::FORCING_NMLIST: CROFFREQ =',TRIM(CROFFREQ)
WRITE(LOGNAM,*) 'CMF::FORCING_NMLIST: LINTERP  =',LINTERP
WRITE(LOGNAM,*) 'CMF::FORCING_NMLIST: LITRPCDF =',LITRPCDF
WRITE(LOGNAM,*) 'CMF::FORCING_NMLIST: CINPMAT  =',TRIM(CINPMAT)
WRITE(LOGNAM,*) 'CMF::FORCING_NMLIST: LROSPLIT =',LROSPLIT
IF( .NOT. LINPCDF .OR. TRIM(CROFFREQ)/='SEQ' )THEN
  WRITE(LOGNAM,*) 'CMF::FORCING_NMLIST: CROFDIR =',TRIM(CROFDIR)
  WRITE(LOGNAM,*) 'CMF::FORCING_NMLIST: CROFPRE =',TRIM(CROFPRE)
  WRITE(LOGNAM,*) 'CMF::FORCING_NMLIST: CROFSUF =',TRIM(CROFSUF)
ENDIF
IF( .not. LINPCDF )THEN !! plain binary
  IF( LROSPLIT )THEN
    WRITE(LOGNAM,*) 'CMF::FORCING_NMLIST: CSUBDIR =',TRIM(CSUBDIR)
    WRITE(LOGNAM,*) 'CMF::FORCING_NMLIST: CSUBPRE =',TRIM(CSUBPRE)
    WRITE(LOGNAM,*) 'CMF::FORCING_NMLIST: CSUBSUF =',TRIM(CSUBSUF)
  ENDIF
ELSE
  IF( TRIM(CROFFREQ)=='SEQ' )THEN
    WRITE(LOGNAM,*) 'CMF::FORCING_NMLIST: CROFCDF =',TRIM(CROFCDF)
    IF ( SYEARIN > 0 ) THEN
      WRITE(LOGNAM,*) 'CMF::FORCING_NMLIST: explicit SEQ start =', &
      & SYEARIN,SMONIN,SDAYIN,SHOURIN
    ELSE
      WRITE(LOGNAM,*) 'CMF::FORCING_NMLIST: SEQ start from NetCDF CF time metadata'
    ENDIF
  ENDIF
  WRITE(LOGNAM,*) 'CMF::FORCING_NMLIST: CVNTIME =',TRIM(CVNTIME)
  WRITE(LOGNAM,*) 'CMF::FORCING_NMLIST: CVNROF  =',TRIM(CVNROF)
  IF( LROSPLIT )THEN
    WRITE(LOGNAM,*) 'CMF::FORCING_NMLIST: CVNSUB  =',TRIM(CVNSUB)
  ENDIF
ENDIF
IF( LINPEND )THEN
  WRITE(LOGNAM,*) 'CMF::FORCING_NMLIST: LINPEND =',LINPEND
ENDIF

CLOSE(NSETFILE)

!*** 6. Report the resulting start time for explicitly dated forcing
IF( TRIM(CROFFREQ)=='SEQ' .AND. SYEARIN>0 ) THEN
  WRITE(LOGNAM,*) 'CMF::FORCING_NMLIST: explicit sequence start KMIN =',KMINSTAIN
ENDIF

WRITE(LOGNAM,*) 'CMF::FORCING_NMLIST: END'
WRITE(LOGNAM,*) '!---------------------!'

END SUBROUTINE CMF_FORCING_NMLIST
!####################################################################





!####################################################################
SUBROUTINE CMF_FORCING_INIT(LECMF2LAKEC)
! Initialize runoff and input-matrix sources selected by the namelist.
! -- Called from MAIN_cmf
! Internal procedures:
!+ CMF_FORCING_INIT_CDF : Initialize the NetCDF runoff source (NetCDF build only)
!+ CMF_INPMAT_INIT_CDF  : Read and map the NetCDF input matrix (NetCDF build only)
!+ CMF_INPMAT_INIT_BIN  : Read and map the Binary input matrix
IMPLICIT NONE
INTEGER(KIND=JPIM),OPTIONAL,INTENT(IN) :: LECMF2LAKEC   !! Lake coupling: this is currently only used in ECMWF
!================================================
WRITE(LOGNAM,*) ''
WRITE(LOGNAM,*) '!---------------------!'
WRITE(LOGNAM,*) "CMF::FORCING_INIT: START"
IF(PRESENT(LECMF2LAKEC)) WRITE(LOGNAM,*) "CMF::FORCING_INIT: LECMF2LAKEC"

IBINUNIT=0
LBINOPEN=.FALSE.
CBINNAME=' '
#ifdef UseCDF_CMF
ROFCDF%NCID=-1
#endif

!*** 1. Initialize the NetCDF runoff source when selected
IF( LINPCDF ) THEN
#ifdef UseCDF_CMF
  WRITE(LOGNAM,*) "CMF::FORCING_INIT: initialize NetCDF runoff source"
  CALL CMF_FORCING_INIT_CDF
#endif
ENDIF
!*** 2. Initialize the runoff input matrix when interpolation is enabled
IF( LINTERP ) THEN
  IF( LITRPCDF )THEN
#ifdef UseCDF_CMF
    IF(PRESENT(LECMF2LAKEC)) THEN
      CALL CMF_INPMAT_INIT_CDF(LECMF2LAKEC)
    ELSE
      CALL CMF_INPMAT_INIT_CDF
    ENDIF
#endif
  ELSE
    CALL CMF_INPMAT_INIT_BIN
  ENDIF
ENDIF

WRITE(LOGNAM,*) "CMF::FORCING_INIT: END"
WRITE(LOGNAM,*) '!---------------------!'

CONTAINS
!==========================================================
!+ CMF_FORCING_INIT_CDF : Initialize the NetCDF runoff source (NetCDF build only)
!+ CMF_INPMAT_INIT_CDF  : Read and map the NetCDF input matrix (NetCDF build only)
!+ CMF_INPMAT_INIT_BIN  : Read and map the Binary input matrix
!==========================================================
#ifdef UseCDF_CMF
SUBROUTINE CMF_FORCING_INIT_CDF
! Open the SEQ NetCDF source and resolve its initial time record.
USE YOS_CMF_INPUT,           ONLY: LROSPLIT, LWEVAP, DTIN, LLEAPYR
USE YOS_CMF_TIME,            ONLY: KMINSTAIN, KMINSTART, KMINEND
USE CMF_UTILS_MOD,           ONLY: NCERROR,MIN2DATE,SPLITHOUR,DATE2MIN
USE NETCDF

IMPLICIT NONE
INTEGER(KIND=JPIM)              :: NTIMEID,NCDFSTP,IDATE,IHHMM,IHOUR0,IMIN0
INTEGER(KIND=JPIM)              :: KMINENDIN,IERR,NREQUIRED,ISTATUS
CHARACTER(LEN=256)               :: CMESSAGE
!================================================
ROFCDF%NCID=-1
IF( TRIM(CROFFREQ)=='SEQ' ) THEN

  !*** 1. Configure the persistent NetCDF runoff source
  ROFCDF%CNAME=TRIM(CROFCDF)
  ROFCDF%CVAR(1)=TRIM(CVNROF)
  ROFCDF%CVAR(2)=TRIM(CVNSUB)
  ROFCDF%LAUTOTIME=(SYEARIN<=0)
  ROFCDF%NRECSTART=1
  IF ( .not. LROSPLIT ) THEN
    ROFCDF%CVAR(2)="NONE"
    ROFCDF%NVARID(2)=-1
  ENDIF
  IF ( .NOT. LWEVAP ) THEN
    ROFCDF%CVAR(3)="NONE"
    ROFCDF%NVARID(3)=-1
  ENDIF

  WRITE(LOGNAM,*) '  CMF_FORCING_INIT_CDF: ',TRIM(ROFCDF%CNAME),TRIM(ROFCDF%CVAR(1))

  !*** 2. Open the source and identify its data and time variables
  ISTATUS=NF90_OPEN(TRIM(ROFCDF%CNAME),NF90_NOWRITE,ROFCDF%NCID)
  IF( ISTATUS/=NF90_NOERR ) THEN
    WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_INIT/CMF_FORCING_INIT_CDF]: ', &
    & 'cannot open SEQ file: ',TRIM(ROFCDF%CNAME)
    CALL NCERROR(ISTATUS,'OPENING :'//ROFCDF%CNAME)
  ENDIF
  CALL NCERROR( NF90_INQ_VARID(ROFCDF%NCID,TRIM(ROFCDF%CVAR(1)),ROFCDF%NVARID(1)) )

  IF ( LROSPLIT ) THEN
    CALL NCERROR( NF90_INQ_VARID(ROFCDF%NCID,ROFCDF%CVAR(2),ROFCDF%NVARID(2)) )
  ENDIF
  IF ( LWEVAP ) THEN
    CALL NCERROR( NF90_INQ_VARID(ROFCDF%NCID,ROFCDF%CVAR(3),ROFCDF%NVARID(3)) )
  ENDIF
  CALL NCERROR( NF90_INQ_DIMID(ROFCDF%NCID,CVNTIME,NTIMEID),'GETTING TIME ID FORCING RUNOFF')
  CALL NCERROR( NF90_INQUIRE_DIMENSION(NCID=ROFCDF%NCID,DIMID=NTIMEID,LEN=NCDFSTP),'GETTING TIME LENGTH')

  WRITE(LOGNAM,*) '  CMF_FORCING_INIT_CDF: file, ID, runoff variable ID =', &
  & TRIM(ROFCDF%CNAME),ROFCDF%NCID,ROFCDF%NVARID(1)

  !*** 3. Resolve the starting record and check time coverage
  ! CF time metadata selects the record at simulation start; explicit dates use legacy indexing.
  IF ( ROFCDF%LAUTOTIME ) THEN
    CALL MIN2DATE(KMINSTART,IDATE,IHHMM)
    CALL SPLITHOUR(IHHMM,IHOUR0,IMIN0)
    CALL cf_resolve_time_record(ROFCDF%NCID,TRIM(CVNTIME),IDATE,IHOUR0,IMIN0,LLEAPYR, &
                              & INT(DTIN,KIND=JPIM),NCDFSTP,ROF_TIME_AXIS,ROFCDF%NRECSTART,IERR,CMESSAGE)
    IF ( IERR/=0 ) THEN
      WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_INIT/CMF_FORCING_INIT_CDF]: ', &
      & 'cannot resolve runoff CF time: ',TRIM(CMESSAGE)
      STOP 9
    ENDIF
    KMINSTAIN=KMINSTART
    ROFCDF%NSTART=KMINSTART
    NREQUIRED=INT((INT(KMINEND-KMINSTART,KIND=JPIB)*60_JPIB+INT(DTIN,KIND=JPIB)-1_JPIB) &
                 & /INT(DTIN,KIND=JPIB),KIND=JPIM)
    IF ( ROFCDF%NRECSTART+NREQUIRED-1>NCDFSTP ) THEN
      WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_INIT/CMF_FORCING_INIT_CDF]: ', &
      & 'run end exceeds forcing data: ',ROFCDF%NRECSTART,NREQUIRED,NCDFSTP
      STOP 9
    ENDIF
    WRITE(LOGNAM,*) '  CMF_FORCING_INIT_CDF: CF time units: ',TRIM(ROF_TIME_AXIS%UNITS)
    WRITE(LOGNAM,*) '  CMF_FORCING_INIT_CDF: CF calendar: ',TRIM(ROF_TIME_AXIS%CALENDAR)
    WRITE(LOGNAM,*) '  CMF_FORCING_INIT_CDF: forcing interval [min]: ',ROF_TIME_AXIS%DT_MINUTES
    WRITE(LOGNAM,*) '  CMF_FORCING_INIT_CDF: first simulation record: ',ROFCDF%NRECSTART
  ELSE
    KMINSTAIN=DATE2MIN(SYEARIN*10000+SMONIN*100+SDAYIN,SHOURIN*100)
    ROFCDF%NSTART=KMINSTAIN
    IF ( KMINSTART .LT. KMINSTAIN ) THEN
      WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_INIT/CMF_FORCING_INIT_CDF]: ', &
      & 'run starts before forcing data: ',TRIM(ROFCDF%CNAME),KMINSTART,KMINSTAIN
      STOP 9
    ENDIF
    KMINENDIN=KMINSTAIN+NCDFSTP*INT(DTIN/60,JPIM)
    IF ( KMINEND .GT. KMINENDIN ) THEN
      WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_INIT/CMF_FORCING_INIT_CDF]: ', &
      & 'run end exceeds forcing data: ',TRIM(ROFCDF%CNAME),KMINEND,KMINENDIN
      STOP 9
    ENDIF
  ENDIF
ENDIF

END SUBROUTINE CMF_FORCING_INIT_CDF
#endif
!==========================================================
#ifdef UseCDF_CMF
SUBROUTINE CMF_INPMAT_INIT_CDF(LECMF2LAKEC)
! Read the forward matrix and optional inverse fields from NetCDF.
USE YOS_CMF_INPUT,           ONLY: NX, NY, INPN, NXIN,NYIN
USE YOS_CMF_MAP,             ONLY: NSEQMAX
USE CMF_UTILS_MOD,           ONLY: INQUIRE_FID, NCERROR, mapD2vecD, mapI2vecI
USE NETCDF

IMPLICIT NONE

INTEGER(KIND=JPIM),OPTIONAL,INTENT(IN) :: LECMF2LAKEC  !! for lake coupling: currently only used in ECMWF

INTEGER(KIND=JPIM),ALLOCATABLE  :: I2TMP(:,:,:)
REAL(KIND=JPRB),ALLOCATABLE     :: D2TMP(:,:,:)

INTEGER(KIND=JPIM)              :: INPI
INTEGER(KIND=JPIM)              :: VARID
INTEGER(KIND=JPIM)              :: ISTATUS,VDIMIDS(1)
INTEGER(KIND=JPIM)              :: NCID

! SAVE for OpenMP
INTEGER(KIND=JPIM),SAVE         :: IX,IY,ILEV
REAL(KIND=JPRB),SAVE            :: ZTMP
!$OMP THREADPRIVATE               (IY,ILEV,ZTMP)
!================================================
!*** 1. Allocate river-sequence input-matrix arrays
WRITE(LOGNAM,*) '  CMF_INPMAT_INIT_CDF: NX, NY, INPN =',NX,NY,INPN
ALLOCATE( INPX(NSEQMAX,INPN),INPY(NSEQMAX,INPN),INPA(NSEQMAX,INPN) )

!*** 2. Read forward input-matrix fields and map layers to river order
WRITE(LOGNAM,*) '  CMF_INPMAT_INIT_CDF: input matrix NetCDF ',TRIM(CINPMAT)

CALL NCERROR (NF90_OPEN(CINPMAT,NF90_NOWRITE,NCID),'opening '//TRIM(CINPMAT) )

!** input matrix area
ALLOCATE( D2TMP(NX,NY,INPN) )
WRITE(LOGNAM,*)'  CMF_INPMAT_INIT_CDF: reading inpa from ',TRIM(CINPMAT)
CALL NCERROR ( NF90_INQ_VARID(NCID,'inpa',VARID),'getting id' )
CALL NCERROR ( NF90_GET_VAR(NCID,VARID,D2TMP,(/1,1,1/),(/NX,NY,INPN/)),'reading data' )
DO INPI=1, INPN
  CALL mapD2vecD(D2TMP(:,:,INPI:INPI),INPA(:,INPI:INPI))
END DO
DEALLOCATE( D2TMP )

!** input matrix IXIN
ALLOCATE( I2TMP(NX,NY,INPN) )

WRITE(LOGNAM,*)'  CMF_INPMAT_INIT_CDF: reading inpx from ',TRIM(CINPMAT)
CALL NCERROR ( NF90_INQ_VARID(NCID,'inpx',VARID),'getting id' )
CALL NCERROR ( NF90_GET_VAR(NCID,VARID,I2TMP,(/1,1,1/),(/NX,NY,INPN/)),'reading data' )
DO INPI=1, INPN
  CALL mapI2vecI(I2TMP(:,:,INPI:INPI),INPX(:,INPI:INPI))
END DO

!** input matrix IYIN
WRITE(LOGNAM,*)'  CMF_INPMAT_INIT_CDF: reading inpy from ',TRIM(CINPMAT)
CALL NCERROR ( NF90_INQ_VARID(NCID,'inpy',VARID),'getting id' )
CALL NCERROR ( NF90_GET_VAR(NCID,VARID,I2TMP,(/1,1,1/),(/NX,NY,INPN/)),'reading data' )
DO INPI=1, INPN
  CALL mapI2vecI(I2TMP(:,:,INPI:INPI),INPY(:,INPI:INPI))
END DO

DEALLOCATE( I2TMP )

!*** 3. Read optional inverse fields for lake-coupling workflows
! Irregular catchments, especially near coasts with undefined basins, may not match
! land-model grid areas; this motivates normalizing the inverse weights.
! TODO(WHY): Confirm the area treatment for undefined coastal basins.
IF(PRESENT(LECMF2LAKEC) .AND. (LECMF2LAKEC .NE. 0)) THEN

  ISTATUS = NF90_INQ_VARID(NCID, 'levI', VARID)
  IF ( ISTATUS /= 0 ) THEN
    WRITE(LOGNAM,*) 'WARNING [CMF::FORCING_INIT/CMF_INPMAT_INIT_CDF]: ', &
    & 'levI is absent; inverse interpolation is unavailable.'
    INPNI=-1  ! Not available
  ELSE
    !* Find levels dimension  -- (assume "time ID" appears first. CF metadata conventions)
    CALL NCERROR( NF90_INQUIRE_VARIABLE(NCID,VARID,dimids=VDIMIDS),'getting levI dimensions ')
    CALL NCERROR( NF90_INQUIRE_DIMENSION(NCID,VDIMIDS(1),len=INPNI),'getting time len ')
    WRITE(LOGNAM,*) '  CMF_INPMAT_INIT_CDF: inverse matrix dimensions =',NXIN,NYIN,INPNI
    ALLOCATE( INPXI(NXIN,NYIN,INPNI),INPYI(NXIN,NYIN,INPNI),INPAI(NXIN,NYIN,INPNI) )

    WRITE(LOGNAM,*)'  CMF_INPMAT_INIT_CDF: reading inpaI from ',TRIM(CINPMAT)
    CALL NCERROR ( NF90_INQ_VARID(NCID,'inpaI',VARID),'getting id' )
    CALL NCERROR ( NF90_GET_VAR(NCID,VARID,INPAI,(/1,1,1/),(/NXIN,NYIN,INPNI/)),'reading data' )

    WRITE(LOGNAM,*)'  CMF_INPMAT_INIT_CDF: reading inpxI from ',TRIM(CINPMAT)
    CALL NCERROR ( NF90_INQ_VARID(NCID,'inpxI',VARID),'getting id' )
    CALL NCERROR ( NF90_GET_VAR(NCID,VARID,INPXI,(/1,1,1/),(/NXIN,NYIN,INPNI/)),'reading data' )

    WRITE(LOGNAM,*)'  CMF_INPMAT_INIT_CDF: reading inpyI from ',TRIM(CINPMAT)
    CALL NCERROR ( NF90_INQ_VARID(NCID,'inpyI',VARID),'getting id' )
    CALL NCERROR ( NF90_GET_VAR(NCID,VARID,INPYI,(/1,1,1/),(/NXIN,NYIN,INPNI/)),'reading data' )

    !! We normalize INPAI here as it is used to interpolate flood fraction (Input Area Inversed)
    WRITE(LOGNAM,*) '  CMF_INPMAT_INIT_CDF: normalize inverse area weights'
!$OMP PARALLEL DO
    DO IX=1,NXIN
      DO IY=1,NYIN
        ZTMP=0._JPRB
        DO ILEV=1,INPNI
          ZTMP=ZTMP+INPAI(IX,IY,ILEV)
        ENDDO
        IF (ZTMP > 0._JPRB) THEN
          DO ILEV=1,INPNI
            INPAI(IX,IY,ILEV) = INPAI(IX,IY,ILEV) / ZTMP
          ENDDO
        ENDIF
      ENDDO
    ENDDO
!$OMP END PARALLEL DO
  ENDIF
ENDIF

END SUBROUTINE CMF_INPMAT_INIT_CDF
#endif
!==========================================================
SUBROUTINE CMF_INPMAT_INIT_BIN
! Read the forward input matrix from direct-access Binary records.
USE YOS_CMF_INPUT,           ONLY: TMPNAM, NX, NY, INPN
USE CMF_UTILS_MOD,           ONLY: INQUIRE_FID, CONV_END,  CONV_ENDI, mapR2vecD, mapI2vecI
USE YOS_CMF_MAP,             ONLY: NSEQMAX

IMPLICIT NONE
INTEGER(KIND=JPIM)              :: INPI
INTEGER(KIND=JPIM),ALLOCATABLE  :: I2TMP(:,:)
REAL(KIND=JPRM),ALLOCATABLE     :: R2TMP(:,:)
!================================================
!*** 1. Allocate river-sequence input-matrix arrays
WRITE(LOGNAM,*) '  CMF_INPMAT_INIT_BIN: NX, NY, INPN =',NX,NY,INPN
ALLOCATE( INPX(NSEQMAX,INPN),INPY(NSEQMAX,INPN),INPA(NSEQMAX,INPN) )

!*** 2. Read Binary records and map each layer to river order
WRITE(LOGNAM,*) '  CMF_INPMAT_INIT_BIN: input matrix Binary ',TRIM(CINPMAT)

ALLOCATE( I2TMP(NX,NY) )
ALLOCATE( R2TMP(NX,NY) )

TMPNAM=INQUIRE_FID()
!OPEN(TMPNAM,FILE=CINPMAT,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4*NX*NY*INPN)
!READ(TMPNAM,REC=1) INPX
!READ(TMPNAM,REC=2) INPY
!READ(TMPNAM,REC=3) R2TMP

OPEN(TMPNAM,FILE=CINPMAT,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4*NX*NY)
DO INPI=1, INPN
  READ(TMPNAM,REC=       INPI) I2TMP
   CALL mapI2vecI(I2TMP,INPX(:,INPI:INPI))
  READ(TMPNAM,REC=  INPN+INPI) I2TMP
   CALL mapI2vecI(I2TMP,INPY(:,INPI:INPI))
  READ(TMPNAM,REC=2*INPN+INPI) R2TMP
   CALL mapR2vecD( R2TMP,INPA(:,INPI:INPI))
END DO

WRITE(LOGNAM,*) '  CMF_INPMAT_INIT_BIN: input matrix loaded'

CLOSE(TMPNAM)
DEALLOCATE(I2TMP,R2TMP)

END SUBROUTINE CMF_INPMAT_INIT_BIN
!==========================================================

END SUBROUTINE CMF_FORCING_INIT
!####################################################################





!####################################################################
SUBROUTINE CMF_FORCING_GET(PBUFF)
! Read forcing fields, replace NaNs with RMIS, and clamp negative runoff to zero.
! -- Called from CMF_DRV_ADVANCE or the standalone Coupler
! Internal procedures:
!+ CMF_FORCING_GET_BIN : Read the selected Binary forcing record
!+ CMF_FORCING_GET_CDF : Read the selected NetCDF forcing record (NetCDF build only)
USE YOS_CMF_INPUT,           ONLY: NXIN,NYIN,LROSPLIT, RMIS
USE CMF_UTILS_MOD,           ONLY: CMF_CheckNanB  !! Check for NaN values

IMPLICIT NONE
REAL(KIND=JPRB),INTENT(INOUT)   :: PBUFF(:,:,:)

INTEGER(KIND=JPIM),SAVE         ::  IXIN, IYIN
!================================================
WRITE(LOGNAM,*) ''
WRITE(LOGNAM,*) '!---------------------!'
WRITE(LOGNAM,*) "CMF::FORCING_GET: START"
!*** 1. Read the configured Binary or NetCDF forcing source
IF( LINPCDF ) THEN
#ifdef UseCDF_CMF
  CALL CMF_FORCING_GET_CDF(PBUFF(:,:,:))
#endif
ELSE
  CALL CMF_FORCING_GET_BIN(PBUFF(:,:,:))
ENDIF

!*** 2. Apply NaN replacement and the nonnegative bound to runoff fields
! Runoff components are expected to be nonnegative; signed evaporation is outside this clamp.
!$OMP PARALLEL DO SIMD PRIVATE(IXIN)
DO IYIN=1,NYIN
  DO IXIN=1,NXIN
    IF( CMF_CheckNanB(PBUFF(IXIN,IYIN,1),0._JPRB) )THEN !! Check if PRUFINN(IX,IY) is NaN (Not-A-Number) ot not
      PBUFF(IXIN,IYIN,1)=RMIS
    ENDIF
    PBUFF(IXIN,IYIN,1)=max(PBUFF(IXIN,IYIN,1),0._JPRB)    !! negative Runoff not assumed
  ENDDO
ENDDO
!$OMP END PARALLEL DO SIMD

IF ( LROSPLIT ) THEN
!$OMP PARALLEL DO SIMD PRIVATE(IXIN)
  DO IYIN=1,NYIN
    DO IXIN=1,NXIN
      IF( CMF_CheckNanB(PBUFF(IXIN,IYIN,2),0._JPRB) )THEN !! Check if PRUFINN(IX,IY) is NaN (Not-A-Number) ot not
        PBUFF(IXIN,IYIN,2)=RMIS
      ENDIF
      PBUFF(IXIN,IYIN,2)=max(PBUFF(IXIN,IYIN,2),0._JPRB)    !! negative Runoff not assumed
    ENDDO
  ENDDO
!$OMP END PARALLEL DO SIMD
ENDIF
WRITE(LOGNAM,*) "CMF::FORCING_GET: END"
WRITE(LOGNAM,*) '!---------------------!'

CONTAINS
!==========================================================
!+ CMF_FORCING_GET_BIN : Read the selected Binary forcing record
!+ CMF_FORCING_GET_CDF : Read the selected NetCDF forcing record (NetCDF build only)
!==========================================================
SUBROUTINE CMF_FORCING_GET_BIN(PBUFF)
! Resolve and read runoff for the current model time.
USE YOS_CMF_INPUT,           ONLY: NXIN,NYIN,LROSPLIT,DTIN
USE YOS_CMF_TIME,            ONLY: KMIN,KMINSTAIN
USE CMF_UTILS_MOD,           ONLY: CONV_END

IMPLICIT NONE
REAL(KIND=JPRB),INTENT(OUT) :: PBUFF(:,:,:)
INTEGER(KIND=JPIM)           :: IRECINP,KMINFILE
INTEGER(KIND=JPIB)            :: IRECOFFSET
REAL(KIND=JPRM)              :: R2TMP(NXIN,NYIN)
CHARACTER(LEN=512)           :: CIFNAME,CSUBNAME,CFILETAG

!================================================
! Resolve the current record once; split runoff uses the same time and grouping.
IF( TRIM(CROFFREQ)=='SEQ' ) THEN
  CIFNAME=TRIM(CROFDIR)//'/'//TRIM(CROFPRE)//TRIM(CROFSUF)
  CFILETAG=' '
  KMINFILE=KMINSTAIN
  IF( KMIN<KMINFILE ) THEN
    WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_GET/CMF_FORCING_GET_BIN]: ', &
    & 'model time precedes Binary SEQ start:',KMIN,KMINFILE
    STOP 9
  ENDIF
  IF( DTIN<=0 ) THEN
    WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_GET/CMF_FORCING_GET_BIN]: ', &
    & 'DTIN must be positive for Binary SEQ.'
    STOP 9
  ENDIF
  IRECOFFSET=(INT(KMIN,KIND=JPIB)-INT(KMINFILE,KIND=JPIB))*60_JPIB
  IF( MOD(IRECOFFSET,INT(DTIN,KIND=JPIB))/=0_JPIB ) THEN
    WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_GET/CMF_FORCING_GET_BIN]: ', &
    & 'model time is not aligned with Binary SEQ records:',KMIN,DTIN
    STOP 9
  ENDIF
  IRECINP=INT(IRECOFFSET/INT(DTIN,KIND=JPIB)+1_JPIB,KIND=JPIM)
ELSE
  CALL FORCING_UTIL_RESOLVE_FILE(KMIN,CIFNAME,CFILETAG,KMINFILE,IRECINP)
ENDIF
CALL FORCING_UTIL_READ_BIN(1,CIFNAME,IRECINP,KMIN,R2TMP)
IF( LINPEND ) CALL CONV_END(R2TMP,NXIN,NYIN)
PBUFF(:,:,1)=R2TMP(:,:)

PBUFF(:,:,2)=0._JPRB
IF( LROSPLIT ) THEN
  IF( TRIM(CROFFREQ)=='SEQ' ) THEN
    CSUBNAME=TRIM(CSUBDIR)//'/'//TRIM(CSUBPRE)//TRIM(CSUBSUF)
  ELSE
    CSUBNAME=TRIM(CSUBDIR)//'/'//TRIM(CSUBPRE)//TRIM(CFILETAG)//TRIM(CSUBSUF)
  ENDIF
  CALL FORCING_UTIL_READ_BIN(2,CSUBNAME,IRECINP,KMIN,R2TMP)
  IF( LINPEND ) CALL CONV_END(R2TMP,NXIN,NYIN)
  PBUFF(:,:,2)=R2TMP(:,:)
ENDIF

END SUBROUTINE CMF_FORCING_GET_BIN
! ================================================
!+
!+
!+
! ================================================
#ifdef UseCDF_CMF
SUBROUTINE CMF_FORCING_GET_CDF(PBUFF)
! Read from a cached sequence file or a cached calendar-grouped NetCDF file.
USE YOS_CMF_TIME,            ONLY: KMIN,KMINSTART
USE YOS_CMF_INPUT,           ONLY: DTIN,NXIN,NYIN,LROSPLIT,LLEAPYR
USE CMF_UTILS_MOD,           ONLY: NCERROR,MIN2DATE,SPLITHOUR
USE NETCDF

IMPLICIT NONE
INTEGER(KIND=JPIM)           :: IRECINP,IFIRSTREC,KMINFILE,IDATE,IHHMM,IHOUR0,IMIN0,NTIMEID,NCDFSTP
INTEGER(KIND=JPIM)           :: IOS,IERR
INTEGER(KIND=JPIB)            :: IOFFSET
CHARACTER(LEN=512)           :: CIFNAME,CFILETAG
CHARACTER(LEN=256)           :: CMESSAGE
REAL(KIND=JPRB),INTENT(OUT)  :: PBUFF(:,:,:)
!================================================

IF( TRIM(CROFFREQ)=='SEQ' ) THEN
  ! SEQ uses one file; Binary needs a start date, while NetCDF may resolve CF time.
  CIFNAME=TRIM(ROFCDF%CNAME)
  IF( ROFCDF%LAUTOTIME ) THEN
    IOFFSET=(INT(KMIN,KIND=JPIB)-INT(KMINSTART,KIND=JPIB))*60_JPIB
    IFIRSTREC=ROFCDF%NRECSTART
  ELSE
    IOFFSET=(INT(KMIN,KIND=JPIB)-INT(ROFCDF%NSTART,KIND=JPIB))*60_JPIB
    IFIRSTREC=1
  ENDIF
  IF( DTIN<=0 ) THEN
    WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_GET/CMF_FORCING_GET_CDF]: ', &
    & 'DTIN must be positive for NetCDF SEQ.'
    STOP 9
  ENDIF
  IF( IOFFSET<0_JPIB .OR. MOD(IOFFSET,INT(DTIN,KIND=JPIB))/=0_JPIB ) THEN
    WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_GET/CMF_FORCING_GET_CDF]: ', &
    & 'unaligned NetCDF SEQ time:',KMIN,DTIN
    STOP 9
  ENDIF
  IRECINP=IFIRSTREC+INT(IOFFSET/INT(DTIN,KIND=JPIB),KIND=JPIM)
ELSE
  CALL FORCING_UTIL_RESOLVE_FILE(KMIN,CIFNAME,CFILETAG,KMINFILE,IRECINP)
  IF( ROFCDF%NCID<=0 .OR. TRIM(ROFCDF%CNAME)/=TRIM(CIFNAME) ) THEN
    IF( ROFCDF%NCID>0 ) CALL NCERROR(NF90_CLOSE(ROFCDF%NCID),'CLOSING :'//TRIM(ROFCDF%CNAME))
    ROFCDF%CNAME=TRIM(CIFNAME)
    ROFCDF%CVAR(1)=TRIM(CVNROF)
    ROFCDF%CVAR(2)=TRIM(CVNSUB)
    ROFCDF%NVARID(2)=-1
    IOS=NF90_OPEN(TRIM(ROFCDF%CNAME),NF90_NOWRITE,ROFCDF%NCID)
    IF( IOS/=NF90_NOERR ) THEN
      WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_GET/CMF_FORCING_GET_CDF]: ', &
      & 'cannot open file for ',KMIN,TRIM(ROFCDF%CNAME)
      CALL NCERROR(IOS,'OPENING :'//TRIM(ROFCDF%CNAME))
    ENDIF
    CALL NCERROR(NF90_INQ_VARID(ROFCDF%NCID,TRIM(ROFCDF%CVAR(1)),ROFCDF%NVARID(1)))
    IF( LROSPLIT ) CALL NCERROR(NF90_INQ_VARID(ROFCDF%NCID,TRIM(ROFCDF%CVAR(2)),ROFCDF%NVARID(2)))
    CALL NCERROR(NF90_INQ_DIMID(ROFCDF%NCID,TRIM(CVNTIME),NTIMEID),'GETTING TIME ID FORCING RUNOFF')
    CALL NCERROR(NF90_INQUIRE_DIMENSION(NCID=ROFCDF%NCID,DIMID=NTIMEID,LEN=NCDFSTP), &
    & 'GETTING TIME LENGTH FOR '//TRIM(ROFCDF%CNAME))
    CALL MIN2DATE(KMIN,IDATE,IHHMM)
    CALL SPLITHOUR(IHHMM,IHOUR0,IMIN0)
    ! CF coordinates select the requested record and validate its interval and calendar.
    CALL cf_resolve_time_record(ROFCDF%NCID,TRIM(CVNTIME),IDATE,IHOUR0,IMIN0,LLEAPYR, &
    & INT(DTIN,KIND=JPIM),NCDFSTP,ROF_TIME_AXIS,IRECINP,IERR,CMESSAGE)
    IF( IERR/=0 ) THEN
      WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_GET/CMF_FORCING_GET_CDF]: ', &
      & 'CF time validation failed for ',TRIM(ROFCDF%CNAME)
      WRITE(LOGNAM,*) '  requested minute, record, reason:',KMIN,IRECINP,TRIM(CMESSAGE)
      STOP 9
    ENDIF
    CALL MIN2DATE(KMINFILE,IDATE,IHHMM)
    CALL SPLITHOUR(IHHMM,IHOUR0,IMIN0)
    ! Keep grouped filenames and CF coverage consistent: record 1 must match the period boundary.
    CALL cf_find_time_record(ROF_TIME_AXIS,IDATE,IHOUR0,IMIN0,IFIRSTREC,IERR,CMESSAGE,.TRUE.)
    IF( IERR/=0 .OR. IFIRSTREC/=1 ) THEN
      WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_GET/CMF_FORCING_GET_CDF]: ', &
      & 'first record must begin at grouped period boundary: ',TRIM(ROFCDF%CNAME),TRIM(CMESSAGE)
      STOP 9
    ENDIF
    WRITE(LOGNAM,*) '  CMF_FORCING_GET_CDF: opened ',TRIM(ROFCDF%CNAME), &
    & ' calendar=',TRIM(ROF_TIME_AXIS%CALENDAR), &
    & ' interval[min]=',ROF_TIME_AXIS%DT_MINUTES
    ROFCDF%NSTART=KMIN
    ROFCDF%NRECSTART=IRECINP
  ELSE
    IOFFSET=(INT(KMIN,KIND=JPIB)-INT(ROFCDF%NSTART,KIND=JPIB))*60_JPIB
    IRECINP=ROFCDF%NRECSTART+INT(IOFFSET/INT(DTIN,KIND=JPIB),KIND=JPIM)
  ENDIF
ENDIF

WRITE(CMESSAGE,'(A,I0,A,I0,A,A)') 'record ',IRECINP,', model minute ',KMIN,', file ',TRIM(CIFNAME)
CALL NCERROR(NF90_GET_VAR(ROFCDF%NCID,ROFCDF%NVARID(1),PBUFF(:,:,1), &
& (/1,1,IRECINP/),(/NXIN,NYIN,1/)), &
& 'READING RUNOFF 1 '//TRIM(CMESSAGE))
IF( ROFCDF%NVARID(2)>0 ) THEN
  CALL NCERROR(NF90_GET_VAR(ROFCDF%NCID,ROFCDF%NVARID(2),PBUFF(:,:,2), &
  & (/1,1,IRECINP/),(/NXIN,NYIN,1/)), &
  & 'READING RUNOFF 2 '//TRIM(CMESSAGE))
ELSE
  PBUFF(:,:,2)=0._JPRB
ENDIF
END SUBROUTINE CMF_FORCING_GET_CDF
#endif
! ================================================

END SUBROUTINE CMF_FORCING_GET
!####################################################################


!####################################################################
SUBROUTINE CMF_FORCING_COM(PBUFF)
! Map catchment-grid flood fraction to the runoff grid using inverse input-matrix weights.
! -- Called from CMF_DRV_ADVANCE or the standalone Coupler
! Internal procedures:
!+ INTERPI : Apply inverse input-matrix weights
USE CMF_UTILS_MOD,           ONLY: vecD2mapD
#ifdef UseMPI_CMF
USE CMF_CTRL_MPI_MOD,        ONLY: CMF_MPI_AllReduce_D2MAP
#endif
USE YOS_CMF_DIAG,            ONLY: D2FLDFRC
USE YOS_CMF_INPUT,           ONLY: NX,NY

IMPLICIT NONE
! Declaration of arguments
REAL(KIND=JPRB)                  :: D2MAPTMP(NX,NY)
REAL(KIND=JPRB), INTENT(OUT)     :: PBUFF(:,:,:)
!================================================
WRITE(LOGNAM,*) ''
WRITE(LOGNAM,*) '!---------------------!'
WRITE(LOGNAM,*) "CMF::FORCING_COM: START"
!*** 1. Gather catchment-grid flood fraction across MPI regions
CALL vecD2mapD(D2FLDFRC,D2MAPTMP)             !! MPI node data is gathered by vecP2mapR
#ifdef UseMPI_CMF
  CALL CMF_MPI_AllReduce_D2MAP(D2MAPTMP)
#endif

!*** 2. Interpolate the gathered field to the runoff grid
CALL INTERPI(D2MAPTMP,PBUFF(:,:,1))        !! Inverse interpolation (CaMa grid -> input runoff grid)
WRITE(LOGNAM,*) "CMF::FORCING_COM: END"
WRITE(LOGNAM,*) '!---------------------!'

CONTAINS
!==========================================================
!+ INTERPI : Apply inverse input-matrix weights
!==========================================================
SUBROUTINE INTERPI(PBUFFIN,PBUFFOUT)
! Interpolate a catchment-grid field to each runoff-grid cell using inverse weights.
USE YOS_CMF_INPUT,           ONLY: NXIN, NYIN, NX,NY

IMPLICIT NONE
REAL(KIND=JPRB),INTENT(IN)      :: PBUFFIN(:,:)     !! CaMa-Flood variable on catchment (NX*NY)
REAL(KIND=JPRB),INTENT(OUT)     :: PBUFFOUT(:,:)    !! output on target grid = input runoff grid (NXIN * NYIN)

INTEGER(KIND=JPIM)              :: IX,IY,INP,IXIN,IYIN
!================================================
!*** 1. Require inverse input-matrix data
IF ( INPNI == -1 ) THEN
  WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_COM/INTERPI]: inverse input-matrix data unavailable.'
  STOP 9
ENDIF
PBUFFOUT(:,:)=1._JPRB

!*** 2. Accumulate weighted catchment values for each runoff-grid cell
DO IYIN=1,NYIN
  DO IXIN=1,NXIN
    PBUFFOUT(IXIN,IYIN)=0._JPRB
    DO INP=1,INPNI
      IX=INPXI(IXIN,IYIN,INP)
      IY=INPYI(IXIN,IYIN,INP)
      IF ( IX > 0 .AND. IY > 0 .AND. IX <= NX .AND. IY <= NY ) THEN
        PBUFFOUT(IXIN,IYIN) = PBUFFOUT(IXIN,IYIN) + PBUFFIN(IX,IY) * INPAI(IXIN,IYIN,INP)
      ENDIF
    ENDDO
  ENDDO
ENDDO



END SUBROUTINE INTERPI
!==========================================================

END SUBROUTINE CMF_FORCING_COM
!####################################################################


!####################################################################
SUBROUTINE CMF_FORCING_PUT(PBUFF)
! Convert runoff-grid input fields to river-sequence discharge fields.
! -- Called from CMF_DRV_ADVANCE or the standalone Coupler
! Internal procedures:
!+ ROFF_INTERP : Apply input-matrix weights and convert runoff units
!+ CONV_RESOL  : Map equal-resolution runoff directly to river cells
USE YOS_CMF_INPUT,           ONLY: LROSPLIT,LWEVAP
USE YOS_CMF_PROG,            ONLY: D2RUNOFF,D2ROFSUB,D2WEVAP

IMPLICIT NONE
! Declaration of arguments
REAL(KIND=JPRB), INTENT(IN)     :: PBUFF(:,:,:)
!================================================
WRITE(LOGNAM,*) ''
WRITE(LOGNAM,*) '!---------------------!'
WRITE(LOGNAM,*) "CMF::FORCING_PUT: START"
!*** 1. Map total and optional subsurface runoff to CaMa-Flood catchments
! The Input Matrix applies area weights for mass-conserving remapping.
! Direct mapping avoids that interpolation when runoff and map resolutions are comparable.
IF (LINTERP) THEN
  CALL ROFF_INTERP(PBUFF(:,:,1),D2RUNOFF)
  IF (LROSPLIT) THEN
    CALL ROFF_INTERP(PBUFF(:,:,2),D2ROFSUB)
  ELSE
    D2ROFSUB(:,:) = 0._JPRB
  ENDIF
ELSE !  nearest point
  CALL CONV_RESOL(PBUFF(:,:,1),D2RUNOFF)
  IF (LROSPLIT) THEN
    CALL CONV_RESOL(PBUFF(:,:,2),D2ROFSUB)
  ELSE
    D2ROFSUB(:,:) = 0._JPRB
  ENDIF
ENDIF

!*** 2. Map optional evaporation input when requested
IF (LWEVAP) THEN
  IF ( SIZE(PBUFF,3) == 3 ) THEN
    CALL ROFF_INTERP(PBUFF(:,:,3),D2WEVAP)
  ELSE
    WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_PUT]: LWEVAP requires an evaporation field in PBUFF.'
    STOP 9
  ENDIF
ENDIF
WRITE(LOGNAM,*) "CMF::FORCING_PUT: END"
WRITE(LOGNAM,*) '!---------------------!'

CONTAINS
!==========================================================
!+ ROFF_INTERP : Apply input-matrix weights and convert runoff units
!+ CONV_RESOL  : Map equal-resolution runoff directly to river cells
!==========================================================
SUBROUTINE ROFF_INTERP(PBUFFIN,PBUFFOUT)
! Map runoff with input-matrix weights and convert it to discharge.
USE YOS_CMF_MAP,             ONLY: NSEQMAX
USE YOS_CMF_INPUT,           ONLY: NXIN, NYIN, INPN, RMIS

IMPLICIT NONE
REAL(KIND=JPRB),INTENT(IN)      :: PBUFFIN(:,:)     !! default [mm/dt]
REAL(KIND=JPRB),INTENT(OUT)     :: PBUFFOUT(:,:)    !! m3/s
! SAVE for OMP
INTEGER(KIND=JPIM),SAVE  ::  ISEQ
INTEGER(KIND=JPIM),SAVE  ::  IXIN, IYIN, INPI  !! FOR OUTPUT
!$OMP THREADPRIVATE    (IXIN, IYIN, INPI)
!================================================
!*** 1. Sum valid runoff contributions for each catchment and convert units
! Missing input cells are skipped; INPA supplies spatial weights and DROFUNIT converts units.
!$OMP PARALLEL DO
DO ISEQ=1, NSEQMAX
  PBUFFOUT(ISEQ,1)=0._JPRB
  DO INPI=1, INPN
    IXIN=INPX(ISEQ,INPI)
    IYIN=INPY(ISEQ,INPI)
    IF( IXIN>0 )THEN
      IF( IXIN > NXIN .OR. IYIN > NYIN ) THEN
          ! Irregular catchments may extend beyond the runoff-data domain near map boundaries.
          ! Skip these contributions after logging; boundary areas are outside the target area of interest.
        WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_PUT/ROFF_INTERP]: input index out of range.'
        WRITE(LOGNAM,*) '  river cell, input layer, ix, iy:',ISEQ,INPI,IXIN,IYIN
        CYCLE
      ENDIF

      IF( PBUFFIN(IXIN,IYIN).NE.RMIS )THEN
        PBUFFOUT(ISEQ,1) = PBUFFOUT(ISEQ,1) + PBUFFIN(IXIN,IYIN) * INPA(ISEQ,INPI) / DROFUNIT   !! DTIN removed in v395
      ENDIF

    ENDIF
  END DO
END DO
!$OMP END PARALLEL DO
END SUBROUTINE ROFF_INTERP
!==========================================================
!+
!+
!==========================================================
SUBROUTINE CONV_RESOL(PBUFFIN,PBUFFOUT)
! Map equal-resolution runoff directly to river cells and convert it to discharge.
USE YOS_CMF_MAP,             ONLY: NSEQMAX, D2GRAREA
USE YOS_CMF_INPUT,           ONLY: RMIS
USE CMF_UTILS_MOD,           ONLY: mapD2vecD

IMPLICIT NONE

REAL(KIND=JPRB),INTENT(IN)      :: PBUFFIN(:,:)     !! default [mm/dt]
REAL(KIND=JPRB),INTENT(OUT)     :: PBUFFOUT(:,:)    !! m3/s

REAL(KIND=JPRB),ALLOCATABLE     :: D2TEMP(:,:)

INTEGER(KIND=JPIM),SAVE         ::  ISEQ
!================================================
!*** 1. Map runoff-grid values to river order and convert to discharge
! Valid values are scaled by catchment area and DROFUNIT; missing values contribute zero.
ALLOCATE(D2TEMP(NSEQMAX,1))
CALL mapD2vecD(PBUFFIN,D2TEMP)
!$OMP PARALLEL DO SIMD
DO ISEQ=1, NSEQMAX
  IF( D2TEMP(ISEQ,1).NE.RMIS )THEN
    PBUFFOUT(ISEQ,1) = D2TEMP(ISEQ,1) * D2GRAREA(ISEQ,1) / DROFUNIT
    PBUFFOUT(ISEQ,1) = MAX(PBUFFOUT(ISEQ,1), 0._JPRB)
  ELSE
    PBUFFOUT(ISEQ,1)=0._JPRB
  ENDIF
END DO
!$OMP END PARALLEL DO SIMD
END SUBROUTINE CONV_RESOL
!==========================================================

END SUBROUTINE CMF_FORCING_PUT
!####################################################################






!####################################################################
SUBROUTINE CMF_FORCING_END
#ifdef UseCDF_CMF
USE CMF_UTILS_MOD,         ONLY: NCERROR
USE NETCDF
#endif

IMPLICIT NONE
INTEGER(KIND=JPIM)              :: ISTREAM
!================================================
WRITE(LOGNAM,*) ""
WRITE(LOGNAM,*) "!---------------------!"
WRITE(LOGNAM,*) "CMF::FORCING_END: START"

#ifdef UseCDF_CMF
IF( ROFCDF%NCID>0 ) THEN
  CALL NCERROR(NF90_CLOSE(ROFCDF%NCID),'CLOSING :'//TRIM(ROFCDF%CNAME))
  ROFCDF%NCID=-1
ENDIF
#endif
DO ISTREAM=1,2
  IF( LBINOPEN(ISTREAM) ) THEN
    CLOSE(IBINUNIT(ISTREAM))
    LBINOPEN(ISTREAM)=.FALSE.
    IBINUNIT(ISTREAM)=0
    CBINNAME(ISTREAM)=' '
  ENDIF
ENDDO

WRITE(LOGNAM,*) "CMF::FORCING_END: END"
WRITE(LOGNAM,*) '!---------------------!'

END SUBROUTINE CMF_FORCING_END
!####################################################################

!####################################################################
PURE FUNCTION FORCING_UTIL_UPPER(CTEXT) RESULT(CUPPER)
! Convert lowercase ASCII letters to uppercase for configuration matching.
CHARACTER(LEN=*),INTENT(IN) :: CTEXT
CHARACTER(LEN=LEN(CTEXT))   :: CUPPER
INTEGER                     :: I,ICODE
!================================================
CUPPER=CTEXT
DO I=1,LEN(CTEXT)
  ICODE=IACHAR(CTEXT(I:I))
  IF( ICODE>=IACHAR('a') .AND. ICODE<=IACHAR('z') ) CUPPER(I:I)=ACHAR(ICODE-32)
ENDDO
END FUNCTION FORCING_UTIL_UPPER
!####################################################################

!####################################################################
SUBROUTINE FORCING_UTIL_RESOLVE_FILE(KMINNOW,CIFNAME,CFILETAG,KMINFILE,IRECORD)
! Resolve grouped filenames from the model calendar and calculate the 1-based record.
! Record 1 is 00:00 at the DAY/MON/YEAR period start; leap behavior follows LLEAPYR.
! IRECORD = (KMINNOW - KMINFILE) * 60 / DTIN + 1, requiring exact time alignment.
! MIN2DATE/DATE2MIN use the model calendar convention.
USE CMF_UTILS_MOD, ONLY: MIN2DATE,SPLITDATE,DATE2MIN
USE YOS_CMF_INPUT, ONLY: DTIN

IMPLICIT NONE
INTEGER(KIND=JPIM),INTENT(IN)  :: KMINNOW
INTEGER(KIND=JPIM),INTENT(OUT) :: KMINFILE,IRECORD
CHARACTER(LEN=*),INTENT(OUT)   :: CIFNAME
CHARACTER(LEN=*),INTENT(OUT)   :: CFILETAG
INTEGER(KIND=JPIM)              :: IDATE,IHHMM,IYEAR,IMON,IDAY,IPERIOD,IDTIN
INTEGER(KIND=JPIB)              :: IOFFSET
CHARACTER(LEN=8)                :: CDATE
!================================================
CDATE=' '
IDTIN=INT(DTIN,KIND=JPIM)
CALL MIN2DATE(KMINNOW,IDATE,IHHMM)
CALL SPLITDATE(IDATE,IYEAR,IMON,IDAY)
SELECT CASE(TRIM(CROFFREQ))
CASE('DAY')
  IPERIOD=IDATE
  WRITE(CDATE,'(I4.4,I2.2,I2.2)') IYEAR,IMON,IDAY
CASE('MON')
  IPERIOD=IYEAR*10000+IMON*100+1
  WRITE(CDATE,'(I4.4,I2.2)') IYEAR,IMON
CASE('YEAR')
  IPERIOD=IYEAR*10000+101
  WRITE(CDATE,'(I4.4)') IYEAR
CASE DEFAULT
  WRITE(LOGNAM,*) 'ERROR [FORCING_UTIL_RESOLVE_FILE]: unsupported CROFFREQ=',TRIM(CROFFREQ)
  STOP 9
END SELECT
KMINFILE=DATE2MIN(IPERIOD,0)
IOFFSET=(INT(KMINNOW,KIND=JPIB)-INT(KMINFILE,KIND=JPIB))*60_JPIB
IF( IOFFSET<0_JPIB .OR. MOD(IOFFSET,INT(IDTIN,KIND=JPIB))/=0_JPIB ) THEN
  WRITE(LOGNAM,*) 'ERROR [FORCING_UTIL_RESOLVE_FILE]: requested time is not aligned ', &
  & 'with the forcing record interval:',TRIM(CROFFREQ),KMINNOW,KMINFILE,DTIN
  STOP 9
ENDIF
IRECORD=INT(IOFFSET/INT(IDTIN,KIND=JPIB)+1_JPIB,KIND=JPIM)
CFILETAG=TRIM(CDATE)
CIFNAME=TRIM(CROFDIR)//'/'//TRIM(CROFPRE)//TRIM(CFILETAG)//TRIM(CROFSUF)
END SUBROUTINE FORCING_UTIL_RESOLVE_FILE
!####################################################################

!####################################################################
SUBROUTINE FORCING_UTIL_READ_BIN(ISTREAM,CNAME,IRECORD,KMINNOW,FIELD)
! Reuse each direct-access file until a grouped-file boundary is crossed.
USE YOS_CMF_INPUT, ONLY: NXIN,NYIN
USE CMF_UTILS_MOD, ONLY: INQUIRE_FID

IMPLICIT NONE
INTEGER(KIND=JPIM),INTENT(IN) :: ISTREAM,IRECORD,KMINNOW
CHARACTER(LEN=*),INTENT(IN)   :: CNAME
REAL(KIND=JPRM),INTENT(OUT)   :: FIELD(NXIN,NYIN)
INTEGER(KIND=JPIM)            :: IOPEN,IOS
CHARACTER(LEN=256)            :: CERR
!================================================
IF( .NOT. LBINOPEN(ISTREAM) .OR. TRIM(CBINNAME(ISTREAM))/=TRIM(CNAME) ) THEN
  IF( LBINOPEN(ISTREAM) ) CLOSE(IBINUNIT(ISTREAM))
  IOPEN=INQUIRE_FID()
  OPEN(IOPEN,FILE=TRIM(CNAME),FORM='UNFORMATTED',ACCESS='DIRECT', &
  & RECL=4*NXIN*NYIN,STATUS='OLD',IOSTAT=IOS,IOMSG=CERR)
  IF( IOS/=0 ) THEN
    WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_GET/CMF_FORCING_GET_BIN/FORCING_UTIL_READ_BIN]: ', &
    & 'cannot open file at model minute ',KMINNOW,': ',TRIM(CNAME),TRIM(CERR)
    STOP 9
  ENDIF
  IBINUNIT(ISTREAM)=IOPEN
  CBINNAME(ISTREAM)=TRIM(CNAME)
  LBINOPEN(ISTREAM)=.TRUE.
  WRITE(LOGNAM,*) '  FORCING_UTIL_READ_BIN: opened ',TRIM(CNAME)
ENDIF
READ(IBINUNIT(ISTREAM),REC=IRECORD,IOSTAT=IOS,IOMSG=CERR) FIELD
IF( IOS/=0 ) THEN
  WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_GET/CMF_FORCING_GET_BIN/FORCING_UTIL_READ_BIN]: ', &
  & 'read failed; model minute, file, record:',KMINNOW,TRIM(CNAME),IRECORD,TRIM(CERR)
  STOP 9
ENDIF
END SUBROUTINE FORCING_UTIL_READ_BIN
!####################################################################

END MODULE CMF_CTRL_FORCING_MOD
