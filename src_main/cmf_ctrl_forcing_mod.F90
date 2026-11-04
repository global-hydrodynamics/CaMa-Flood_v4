MODULE CMF_CTRL_FORCING_MOD
!==========================================================
!* PURPOSE: Manage CaMa-Flood forcing
!
!* CONTAINS:
! -- CMF_FORCING_NMLIST : Read setting from Namelist
! -- CMF_FORCING_INIT   : Initialize forcing data file
! -- CMF_FORCING_PUT    : Put forcing data (PBUFF) to CaMa-Flood
! -- CMF_FORCING_GET    : Read forcing data from file (save as "data buffer" PBUFF)
! -- CMF_FORCING_END    : Finalize forcing data file
!
!* SHARED UTILITIES:
! -- FORCING_UTIL_NML_HAS_SETTING : Detect an explicitly supplied legacy setting
! -- FORCING_UTIL_RESOLVE_FILE    : Resolve calendar file period and record index
! -- FORCING_UTIL_READ_BIN        : Reuse a direct-access Binary input file
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
USE cmf_cf_time_mod,         ONLY: cf_time_axis, cf_resolve_time_record
#endif
!============================
IMPLICIT NONE
SAVE
!*** NAMELIST/NFORCE/
! Forcing configulation / foprcing mapping table "input matrix"
LOGICAL                         :: LINPCDF             !! true : netCDF runoff forcing
LOGICAL                         :: LINPEND             !! true  for input    endian conversion
LOGICAL                         :: LINPDAY             !! legacy NetCDF daily/sequence selector
CHARACTER(LEN=16)               :: CROFFREQ            !! Runoff file grouping: DAY, MON, YEAR, or SEQ

LOGICAL                         :: LINTERP             !! true : runoff interpolation using input matrix
LOGICAL                         :: LITRPCDF            !! true : netCDF input matrix file 
CHARACTER(LEN=256)              :: CINPMAT             !! Input matrix filename
REAL(KIND=JPRB)                 :: DROFUNIT            !! runoff unit conversion ( InpUnit/DROFUNIT = m3/m2/s)
! Binary forcing (total runoff / surface runoff when LROSPLIT)
CHARACTER(LEN=256)              :: CROFDIR             !! Forcing: runoff directory
CHARACTER(LEN=256)              :: CROFPRE             !! Forcing: runoff prefix
CHARACTER(LEN=256)              :: CROFSUF             !! Forcing: runoff suffix
! 
CHARACTER(LEN=256)              :: CSUBDIR             !! Forcing: sub-surface runoff directory
CHARACTER(LEN=256)              :: CSUBPRE             !! Forcing: sub-surface runoff prefix
CHARACTER(LEN=256)              :: CSUBSUF             !! Forcing: sub-surface runoff suffix
! netCDF Forcing
CHARACTER(LEN=256)              :: CROFCDF             !! Netcdf forcing file file
CHARACTER(LEN=256)              :: CVNTIME             !! Netcdf VARNAME of time dimention
CHARACTER(LEN=256)              :: CVNROF              !! NetCDF VARNAME of runoff. Default "runoff"/
CHARACTER(LEN=256)              :: CVNSUB              !! NetCDF VARNAME of sub-surface runoff.

INTEGER(KIND=JPIM)              :: SYEARIN             !! first-record year; <=0 reads CF time metadata
INTEGER(KIND=JPIM)              :: SMONIN              !! first-record month for explicit legacy mode
INTEGER(KIND=JPIM)              :: SDAYIN              !! first-record day for explicit legacy mode
INTEGER(KIND=JPIM)              :: SHOURIN             !! first-record hour for explicit legacy mode

NAMELIST/NFORCE/   LINTERP, LINPEND, LINPDAY, LINPCDF, LITRPCDF, CROFFREQ, CINPMAT, DROFUNIT, &
                    CROFDIR,CROFPRE, CROFSUF, CSUBDIR,  CSUBPRE, CSUBSUF, &
                    CROFCDF,CVNTIME, CVNROF,  CVNSUB,  SYEARIN,  SMONIN,SDAYIN,SHOURIN

!* local variable
INTEGER(KIND=JPIM)              :: NCID        !! netCDF file     ID
INTEGER(KIND=JPIM)              :: NVARID(2)   !! netCDF variable ID
INTEGER(KIND=JPIM)              :: IBINUNIT(2)=0
LOGICAL                         :: LBINOPEN(2)=.FALSE.
LOGICAL                         :: LCROFFREQ_EXPLICIT=.FALSE.
CHARACTER(LEN=512)              :: CBINNAME(2)=' '

#ifdef UseCDF_CMF
TYPE TYPEROF
CHARACTER(LEN=256)              :: CNAME       !! netCDF file name
CHARACTER(LEN=256)              :: CVAR(3)     !! netCDF variable name
INTEGER(KIND=JPIM)              :: NCID        !! netCDF file     ID
INTEGER(KIND=JPIM)              :: NVARID(3)   !! netCDF variable ID
INTEGER(KIND=JPIM)              :: NSTART      !! Start date of netNDF (in KMIN)
INTEGER(KIND=JPIM)              :: NRECSTART   !! record corresponding to simulation start
LOGICAL                         :: LAUTOTIME    !! true: use the NetCDF CF time axis
END TYPE TYPEROF
TYPE(TYPEROF)                   :: ROFCDF      !! Derived type for Runoff input 
TYPE(cf_time_axis)              :: ROF_TIME_AXIS
#endif
CONTAINS
!####################################################################
PURE FUNCTION FORCING_UTIL_UPPER(CTEXT) RESULT(CUPPER)
CHARACTER(LEN=*),INTENT(IN) :: CTEXT
CHARACTER(LEN=LEN(CTEXT))   :: CUPPER
INTEGER                     :: I,ICODE
CUPPER=CTEXT
DO I=1,LEN(CTEXT)
  ICODE=IACHAR(CTEXT(I:I))
  IF( ICODE>=IACHAR('a') .AND. ICODE<=IACHAR('z') ) CUPPER(I:I)=ACHAR(ICODE-32)
ENDDO
END FUNCTION FORCING_UTIL_UPPER
!####################################################################

!####################################################################
LOGICAL FUNCTION FORCING_UTIL_NML_HAS_SETTING(CFILE,CSETTING)
! Distinguish an omitted legacy setting from an explicitly supplied value.
USE CMF_UTILS_MOD, ONLY: INQUIRE_FID
CHARACTER(LEN=*),INTENT(IN) :: CFILE,CSETTING
INTEGER(KIND=JPIM)           :: IUNIT,IOS,I,J,K,N
CHARACTER(LEN=2048)          :: CLINE,CLOWER
LOGICAL                      :: LNGROUP,LQUOTE
CHARACTER                    :: CQUOTE
FORCING_UTIL_NML_HAS_SETTING=.FALSE.
LNGROUP=.FALSE.
IUNIT=INQUIRE_FID()
OPEN(IUNIT,FILE=TRIM(CFILE),STATUS='OLD',ACTION='READ',IOSTAT=IOS)
IF( IOS/=0 ) RETURN
DO
  READ(IUNIT,'(A)',IOSTAT=IOS) CLINE
  IF( IOS/=0 ) EXIT
  CLOWER=FORCING_UTIL_UPPER(CLINE)
  LQUOTE=.FALSE.
  CQUOTE=' '
  DO I=1,LEN_TRIM(CLOWER)
    IF( LQUOTE ) THEN
      IF( CLOWER(I:I)==CQUOTE ) LQUOTE=.FALSE.
    ELSEIF( CLOWER(I:I)=="'" .OR. CLOWER(I:I)=='"' ) THEN
      LQUOTE=.TRUE.
      CQUOTE=CLOWER(I:I)
    ELSEIF( CLOWER(I:I)=='!' ) THEN
      CLOWER(I:)=' '
      EXIT
    ENDIF
  ENDDO
  IF( .NOT. LNGROUP ) THEN
    J=0
    LQUOTE=.FALSE.
    CQUOTE=' '
    K=1
    DO WHILE( K<=LEN_TRIM(CLOWER) )
      IF( LQUOTE ) THEN
        IF( CLOWER(K:K)==CQUOTE ) THEN
          IF( K<LEN_TRIM(CLOWER) ) THEN
            IF( CLOWER(K+1:K+1)==CQUOTE ) THEN
              K=K+2
              CYCLE
            ENDIF
          ENDIF
          LQUOTE=.FALSE.
        ENDIF
      ELSEIF( CLOWER(K:K)=="'" .OR. CLOWER(K:K)=='"' ) THEN
        LQUOTE=.TRUE.
        CQUOTE=CLOWER(K:K)
      ELSEIF( K+6<=LEN_TRIM(CLOWER) ) THEN
        IF( CLOWER(K:K+6)=='&NFORCE' .OR. CLOWER(K:K+6)=='$NFORCE' ) THEN
          J=K
          IF( K+7<=LEN_TRIM(CLOWER) ) THEN
            IF( INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_',CLOWER(K+7:K+7))/=0 ) J=0
          ENDIF
          IF( J>0 ) EXIT
        ENDIF
      ENDIF
      K=K+1
    ENDDO
    IF( J==0 ) CYCLE
    LNGROUP=.TRUE.
    CLOWER=CLOWER(J+7:)
  ENDIF
  N=LEN_TRIM(CLOWER)
  LQUOTE=.FALSE.
  CQUOTE=' '
  I=1
  DO WHILE( I<=N )
    IF( LQUOTE ) THEN
      IF( CLOWER(I:I)==CQUOTE ) THEN
        IF( I<N ) THEN
          IF( CLOWER(I+1:I+1)==CQUOTE ) THEN
            I=I+2
            CYCLE
          ENDIF
        ENDIF
        LQUOTE=.FALSE.
      ENDIF
      I=I+1
      CYCLE
    ELSEIF( CLOWER(I:I)=="'" .OR. CLOWER(I:I)=='"' ) THEN
      LQUOTE=.TRUE.
      CQUOTE=CLOWER(I:I)
      I=I+1
      CYCLE
    ENDIF
    IF( I+LEN_TRIM(CSETTING)-1>N ) EXIT
    IF( CLOWER(I:I+LEN_TRIM(CSETTING)-1)/=TRIM(FORCING_UTIL_UPPER(CSETTING)) ) THEN
      I=I+1
      CYCLE
    ENDIF
    IF( I>1 ) THEN
      IF( INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_',CLOWER(I-1:I-1))/=0 ) THEN
        I=I+1
        CYCLE
      ENDIF
    ENDIF
    J=I+LEN_TRIM(CSETTING)
    IF( J<=N ) THEN
      IF( INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_',CLOWER(J:J))/=0 ) THEN
        I=I+1
        CYCLE
      ENDIF
    ENDIF
    DO WHILE( J<=N )
      IF( CLOWER(J:J)/=' ' .AND. CLOWER(J:J)/=CHAR(9) ) EXIT
      J=J+1
    ENDDO
    IF( J<=N ) THEN
      IF( CLOWER(J:J)=='=' ) THEN
        FORCING_UTIL_NML_HAS_SETTING=.TRUE.
        CLOSE(IUNIT)
        RETURN
      ENDIF
    ENDIF
    I=I+1
  ENDDO
  IF( LNGROUP ) THEN
    ! Namelist terminators are normally on a line by themselves; accept an
    ! inline terminator only when it is outside a quoted string.
    LQUOTE=.FALSE.
    CQUOTE=' '
    K=1
    DO WHILE( K<=N )
      IF( LQUOTE ) THEN
        IF( CLOWER(K:K)==CQUOTE ) THEN
          IF( K<N ) THEN
            IF( CLOWER(K+1:K+1)==CQUOTE ) THEN
              K=K+2
              CYCLE
            ENDIF
          ENDIF
          LQUOTE=.FALSE.
        ENDIF
      ELSEIF( CLOWER(K:K)=="'" .OR. CLOWER(K:K)=='"' ) THEN
        LQUOTE=.TRUE.
        CQUOTE=CLOWER(K:K)
      ELSEIF( CLOWER(K:K)=='/' ) THEN
        CLOSE(IUNIT)
        RETURN
      ENDIF
      K=K+1
    ENDDO
  ENDIF
ENDDO
CLOSE(IUNIT)
END FUNCTION FORCING_UTIL_NML_HAS_SETTING
!####################################################################

!####################################################################
SUBROUTINE FORCING_UTIL_RESOLVE_FILE(KMINNOW,CIFNAME,CFILETAG,KMINFILE,IRECORD)
! Resolve calendar-grouped file names and exact record offsets for both formats.
! MIN2DATE/DATE2MIN and IMDAYS use the model's LLEAPYR calendar convention.
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
CDATE=' '
IDTIN=INT(DTIN,KIND=JPIM)
IF( DTIN<=0._JPRB .OR. REAL(IDTIN,KIND=JPRB)/=DTIN ) THEN
  WRITE(LOGNAM,*) 'ERROR [CMF::FORCING]: DAY/MON/YEAR requires a positive IFRQ_INP.'
  STOP 9
ENDIF
IF( MOD(86400,IDTIN)/=0 ) THEN
  WRITE(LOGNAM,*) 'ERROR [CMF::FORCING]: DAY/MON/YEAR requires a positive IFRQ_INP that divides 24 hours.'
  STOP 9
ENDIF
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
  WRITE(LOGNAM,*) 'ERROR [CMF::FORCING]: File-period resolver called for CROFFREQ=',TRIM(CROFFREQ)
  STOP 9
END SELECT
KMINFILE=DATE2MIN(IPERIOD,0)
IOFFSET=(INT(KMINNOW,KIND=JPIB)-INT(KMINFILE,KIND=JPIB))*60_JPIB
IF( IOFFSET<0_JPIB .OR. MOD(IOFFSET,INT(IDTIN,KIND=JPIB))/=0_JPIB ) THEN
  WRITE(LOGNAM,*) 'ERROR [CMF::FORCING]: requested time is not aligned with the forcing record interval:', &
  & TRIM(CROFFREQ),KMINNOW,KMINFILE,DTIN
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
IF( .NOT. LBINOPEN(ISTREAM) .OR. TRIM(CBINNAME(ISTREAM))/=TRIM(CNAME) ) THEN
  IF( LBINOPEN(ISTREAM) ) CLOSE(IBINUNIT(ISTREAM))
  IOPEN=INQUIRE_FID()
  OPEN(IOPEN,FILE=TRIM(CNAME),FORM='UNFORMATTED',ACCESS='DIRECT', &
  & RECL=4*NXIN*NYIN,STATUS='OLD',IOSTAT=IOS,IOMSG=CERR)
  IF( IOS/=0 ) THEN
    WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_GET_BIN]: cannot open file at model minute ', &
    & KMINNOW,': ',TRIM(CNAME),TRIM(CERR)
    STOP 9
  ENDIF
  IBINUNIT(ISTREAM)=IOPEN
  CBINNAME(ISTREAM)=TRIM(CNAME)
  LBINOPEN(ISTREAM)=.TRUE.
  WRITE(LOGNAM,*) 'Forcing file opened: ',TRIM(CNAME)
ENDIF
READ(IBINUNIT(ISTREAM),REC=IRECORD,IOSTAT=IOS,IOMSG=CERR) FIELD
IF( IOS/=0 ) THEN
  WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_GET_BIN]: read failed; model minute, file, record:', &
  & KMINNOW,TRIM(CNAME),IRECORD,TRIM(CERR)
  STOP 9
ENDIF
END SUBROUTINE FORCING_UTIL_READ_BIN
!####################################################################

!####################################################################
! -- CMF_FORCING_NMLIST : Read setting from Namelist
! -- CMF_FORCING_INIT   : Initialize forcing data file
! -- CMF_FORCING_PUT    : Put forcing data (PBUFF) to CaMa-Flood
! -- CMF_FORCING_GET    : Read forcing data from file (save as "data buffer" PBUFF)
! -- CMF_FORCING_END    : Finalize forcing data file
!
!####################################################################
SUBROUTINE CMF_FORCING_NMLIST
! Read forcing settings and update the shared model time origin when needed.
! -- Called from CMF_DRV_NMLIST
USE YOS_CMF_INPUT,      ONLY: CSETFILE,NSETFILE,LROSPLIT,DTIN
USE YOS_CMF_TIME,       ONLY: YYYY0,KMINSTAIN
USE CMF_UTILS_MOD,      ONLY: INQUIRE_FID,DATE2MIN,IMDAYS
IMPLICIT NONE
INTEGER(KIND=JPIM)              :: IYEARIN,IMONIN,IDAYIN
INTEGER(KIND=JPIM)              :: IDTIN
LOGICAL                         :: LLINPDAY_SET
CHARACTER(LEN=16)               :: CLEGACY_FREQ
!================================================
WRITE(LOGNAM,*) ""
WRITE(LOGNAM,*) "!---------------------!"

!*** 1. Open the namelist file
NSETFILE=INQUIRE_FID()
OPEN(NSETFILE,FILE=CSETFILE,STATUS="OLD")
WRITE(LOGNAM,*) "CMF::FORCING_NMLIST: namelist OPEN in unit: ", TRIM(CSETFILE), NSETFILE 

!*** 2. Set default forcing and input-matrix options
LINPCDF =.FALSE.
LINPEND =.FALSE.
LINPDAY =.FALSE.
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
  CVNROF="Qs"
  CVNSUB="Qsb"
ENDIF

SYEARIN=0                       !! <=0: derive the input record from NetCDF CF time metadata
SMONIN =0
SDAYIN =0
SHOURIN=0

!*** 3. Read settings and resolve legacy/new file-grouping configuration
REWIND(NSETFILE)
READ(NSETFILE,NML=NFORCE)
LLINPDAY_SET=FORCING_UTIL_NML_HAS_SETTING(CSETFILE,'LINPDAY')
LCROFFREQ_EXPLICIT=(LEN_TRIM(CROFFREQ)>0)

! Resolve the new grouping option independently from the runoff format.
IF( LEN_TRIM(CROFFREQ)==0 ) THEN
  IF( .NOT. LINPCDF ) THEN
    CROFFREQ='DAY'
  ELSEIF( LINPDAY ) THEN
    CROFFREQ='DAY'
  ELSE
    CROFFREQ='SEQ'
  ENDIF
ELSE
  CROFFREQ=TRIM(FORCING_UTIL_UPPER(ADJUSTL(CROFFREQ)))
  IF( TRIM(CROFFREQ)/='DAY' .AND. TRIM(CROFFREQ)/='MON' .AND. &
  &   TRIM(CROFFREQ)/='YEAR' .AND. TRIM(CROFFREQ)/='SEQ' ) THEN
    WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_NMLIST]: CROFFREQ must be DAY, MON, YEAR, or SEQ: ',TRIM(CROFFREQ)
    STOP 9
  ENDIF
  IF( LLINPDAY_SET ) THEN
    IF( LINPDAY ) THEN
      CLEGACY_FREQ='DAY'
    ELSE
      CLEGACY_FREQ='SEQ'
    ENDIF
    IF( TRIM(CROFFREQ)/=TRIM(CLEGACY_FREQ) ) THEN
      WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_NMLIST]: CROFFREQ conflicts with explicitly set LINPDAY.'
      WRITE(LOGNAM,*) '  CROFFREQ=',TRIM(CROFFREQ),' LINPDAY=',LINPDAY
      STOP 9
    ENDIF
  ENDIF
ENDIF

IF( TRIM(CROFFREQ)/='SEQ' ) THEN
  IDTIN=INT(DTIN,KIND=JPIM)
  IF( DTIN<=0._JPRB .OR. REAL(IDTIN,KIND=JPRB)/=DTIN ) THEN
    WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_NMLIST]: IFRQ_INP must define a positive integer-second interval.'
    STOP 9
  ENDIF
  IF( MOD(86400,IDTIN)/=0 ) THEN
    WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_NMLIST]: DAY/MON/YEAR requires IFRQ_INP to divide 24 hours.'
    STOP 9
  ENDIF
ENDIF
! Binary calendar-grouped files are indexed from model calendar boundaries;
! explicit first-record time applies only to Binary SEQ and NetCDF input.
IF( SYEARIN>0 .AND. (LINPCDF .OR. TRIM(CROFFREQ)=='SEQ') ) THEN
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
  IF( LINPCDF .AND. TRIM(CROFFREQ)/='SEQ' ) THEN
    IF( SHOURIN/=0 .OR. (TRIM(CROFFREQ)=='MON' .AND. SDAYIN/=1) .OR. &
    &   (TRIM(CROFFREQ)=='YEAR' .AND. (SMONIN/=1 .OR. SDAYIN/=1)) ) THEN
      WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_NMLIST]: grouped NetCDF SYEARIN must match its period start.'
      STOP 9
    ENDIF
  ENDIF
  YYYY0=MIN(YYYY0,SYEARIN)
  KMINSTAIN=DATE2MIN(SYEARIN*10000+SMONIN*100+SDAYIN,SHOURIN*100)
ELSEIF( TRIM(CROFFREQ)=='SEQ' .AND. .NOT. LINPCDF ) THEN
  WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_NMLIST]: Binary SEQ requires SYEARIN, SMONIN, SDAYIN, and SHOURIN.'
  STOP 9
ENDIF

WRITE(LOGNAM,*)     "=== NAMELIST, NFORCE ==="
WRITE(LOGNAM,*)     "LINPCDF:   ", LINPCDF
WRITE(LOGNAM,*)     "LINPDAY:   ", LINPDAY
WRITE(LOGNAM,*)     "CROFFREQ:  ", TRIM(CROFFREQ)
WRITE(LOGNAM,*)     "LINTERP:   ", LINTERP
WRITE(LOGNAM,*)     "LITRPCDF:  ", LITRPCDF
WRITE(LOGNAM,*)     "CINPMAT:   ", TRIM(CINPMAT)
WRITE(LOGNAM,*)     "LROSPLIT:  ", LROSPLIT
IF( .NOT. LINPCDF .OR. TRIM(CROFFREQ)/='SEQ' )THEN
  WRITE(LOGNAM,*)   "CROFDIR:   ", TRIM(CROFDIR)
  WRITE(LOGNAM,*)   "CROFPRE:   ", TRIM(CROFPRE)
  WRITE(LOGNAM,*)   "CROFSUF:   ", TRIM(CROFSUF)
ENDIF
IF( .not. LINPCDF )THEN !! plain binary
  IF( LROSPLIT )THEN
    WRITE(LOGNAM,*) "CSUBDIR:   ", TRIM(CSUBDIR)
    WRITE(LOGNAM,*) "CSUBPRE:   ", TRIM(CSUBPRE)
    WRITE(LOGNAM,*) "CSUBSUF:   ", TRIM(CSUBSUF)
  ENDIF
ELSE
  IF( TRIM(CROFFREQ)=='SEQ' )THEN
    WRITE(LOGNAM,*)   "CROFCDF:   ", TRIM(CROFCDF)
    IF ( SYEARIN > 0 ) THEN
      WRITE(LOGNAM,*) "SYEARIN,SMONIN,SDAYIN,SHOURIN ", SYEARIN,SMONIN,SDAYIN,SHOURIN
    ELSE
      WRITE(LOGNAM,*) "forcing start time: read from NetCDF CF time metadata"
    ENDIF
  ENDIF
  WRITE(LOGNAM,*)   "CVNTIME:   ", TRIM(CVNTIME)
  WRITE(LOGNAM,*)   "CVNROF:    ", TRIM(CVNROF)
  IF( LROSPLIT )THEN
    WRITE(LOGNAM,*) "CVNSUB:    ", TRIM(CVNSUB)
  ENDIF
ENDIF
IF( LINPEND )THEN
  WRITE(LOGNAM,*)   "LINPEND:   ", LINPEND
ENDIF

CLOSE(NSETFILE)

!*** 4. Adjust the shared time origin for explicitly dated forcing
IF( TRIM(CROFFREQ)=='SEQ' .AND. SYEARIN>0 ) THEN
  WRITE(LOGNAM,*) 'Explicit sequence forcing start KMIN:',KMINSTAIN
ENDIF

WRITE(LOGNAM,*) "CMF::FORCING_NMLIST: end" 

END SUBROUTINE CMF_FORCING_NMLIST
!####################################################################





!####################################################################
SUBROUTINE CMF_FORCING_INIT(LECMF2LAKEC)
! Initialize runoff and input-matrix sources selected by the namelist.
! -- called from "Main Program / Coupler"
USE YOS_CMF_INPUT,           ONLY: NXIN,NYIN,DTIN
USE YOS_CMF_TIME,            ONLY: KMINSTART,KMINEND,KMINSTAIN
USE CMF_UTILS_MOD,           ONLY: INQUIRE_FID
IMPLICIT NONE

INTEGER(KIND=JPIM),OPTIONAL,INTENT(IN) :: LECMF2LAKEC   !! Lake coupling: this is currently only used in ECMWF
INTEGER(KIND=JPIB)              :: IFILESIZE,IRECL,INREC,IREQ,INEED,IOFFSET
INTEGER(KIND=JPIM)              :: IOS
CHARACTER(LEN=512)              :: CSEQFILE

!================================================
WRITE(LOGNAM,*) ""
WRITE(LOGNAM,*) "!---------------------!"

WRITE(LOGNAM,*) "CMF::FORCING_INIT: " 
IF(PRESENT(LECMF2LAKEC)) WRITE(LOGNAM,*) "CMF::FORCING_INIT: LECMF2LAKEC"

IBINUNIT=0
LBINOPEN=.FALSE.
CBINNAME=' '
#ifdef UseCDF_CMF
ROFCDF%NCID=-1
#endif
IF( .NOT. LINPCDF .AND. TRIM(CROFFREQ)=='SEQ' ) THEN
  IF( DTIN<=0._JPRB .OR. REAL(INT(DTIN),KIND=JPRB)/=DTIN ) THEN
    WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_INIT]: Binary SEQ requires a positive integer-second DTIN.'
    STOP 9
  ENDIF
  CSEQFILE=TRIM(CROFDIR)//'/'//TRIM(CROFPRE)//TRIM(CROFSUF)
  IRECL=4_JPIB*INT(NXIN,KIND=JPIB)*INT(NYIN,KIND=JPIB)
  INQUIRE(FILE=TRIM(CSEQFILE),SIZE=IFILESIZE,IOSTAT=IOS)
  IF( IOS/=0 ) THEN
    WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_INIT]: missing or malformed Binary SEQ file: ',TRIM(CSEQFILE)
    STOP 9
  ENDIF
  IF( IRECL<=0_JPIB ) THEN
    WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_INIT]: invalid Binary SEQ record size:',IRECL
    STOP 9
  ENDIF
  IF( MOD(IFILESIZE,IRECL)/=0_JPIB ) THEN
    WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_INIT]: Binary SEQ file size is not a whole number of records: ', &
    & TRIM(CSEQFILE),IFILESIZE,IRECL
    STOP 9
  ENDIF
  INREC=IFILESIZE/IRECL
  IOFFSET=INT(KMINSTART-KMINSTAIN,KIND=JPIB)*60_JPIB
  IF( IOFFSET<0_JPIB .OR. MOD(IOFFSET,INT(DTIN,KIND=JPIB))/=0_JPIB ) THEN
    WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_INIT]: simulation start is before or unaligned with Binary SEQ data.'
    STOP 9
  ENDIF
  IREQ=IOFFSET/INT(DTIN,KIND=JPIB)+1_JPIB
  IF( MOD(INT(KMINEND-KMINSTART,KIND=JPIB)*60_JPIB,INT(DTIN,KIND=JPIB))/=0_JPIB ) THEN
    WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_INIT]: simulation end is not aligned with Binary SEQ records.'
    STOP 9
  ENDIF
  INEED=(INT(KMINEND-KMINSTART,KIND=JPIB)*60_JPIB+INT(DTIN,KIND=JPIB)-1_JPIB) &
  & /INT(DTIN,KIND=JPIB)
  IF( IREQ<1_JPIB .OR. IREQ+INEED-1_JPIB>INREC ) THEN
    WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_INIT]: requested run exceeds Binary SEQ records:', &
    & TRIM(CSEQFILE),IREQ,INEED,INREC
    STOP 9
  ENDIF
  WRITE(LOGNAM,*) 'Binary SEQ records validated:',TRIM(CSEQFILE),INREC
ENDIF

!*** 1. Initialize the NetCDF runoff source when selected
IF( LINPCDF ) THEN
#ifdef UseCDF_CMF
  WRITE(LOGNAM,*) "CMF::FORCING_INIT: Initialize runoff forcing file (only for netCDF)" 
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

WRITE(LOGNAM,*) "CMF::FORCING_INIT: end" 

CONTAINS
!==========================================================
!+ CMF_FORCING_INIT_CDF : open netCDF forcing
!+ CMF_INPMAT_INIT_CDF      :  open runoff interporlation matrix (inpmat)
!+ CMF_INPMAT_INIT_BIN      :  open runoff interporlation matrix (inpmat)
!==========================================================
#ifdef UseCDF_CMF
SUBROUTINE CMF_FORCING_INIT_CDF
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

  WRITE(LOGNAM,*) "CMF::FORCING_INIT_CDF:", TRIM(ROFCDF%CNAME), TRIM(ROFCDF%CVAR(1))
  
  !*** 2. Open the source and identify its data and time variables
  ISTATUS=NF90_OPEN(TRIM(ROFCDF%CNAME),NF90_NOWRITE,ROFCDF%NCID)
  IF( ISTATUS/=NF90_NOERR ) THEN
    WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_INIT_CDF]: cannot open SEQ file: ',TRIM(ROFCDF%CNAME)
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
  
  WRITE(LOGNAM,*) "CMF::FORCING_INIT_CDF: CNAME,NCID,VARID", TRIM(ROFCDF%CNAME),ROFCDF%NCID,ROFCDF%NVARID(1)
  
  !*** 3. Resolve the starting record and check time coverage
  ! CF time metadata selects the record at simulation start; explicit dates use legacy indexing.
  IF ( ROFCDF%LAUTOTIME ) THEN
    CALL MIN2DATE(KMINSTART,IDATE,IHHMM)
    CALL SPLITHOUR(IHHMM,IHOUR0,IMIN0)
    CALL cf_resolve_time_record(ROFCDF%NCID,TRIM(CVNTIME),IDATE,IHOUR0,IMIN0,LLEAPYR, &
                              & INT(DTIN,KIND=JPIM),NCDFSTP,ROF_TIME_AXIS,ROFCDF%NRECSTART,IERR,CMESSAGE)
    IF ( IERR/=0 ) THEN
      WRITE(LOGNAM,*) "Cannot resolve runoff NetCDF CF time: ",TRIM(CMESSAGE)
      STOP 9
    ENDIF
    KMINSTAIN=KMINSTART
    ROFCDF%NSTART=KMINSTART
    NREQUIRED=INT((INT(KMINEND-KMINSTART,KIND=JPIB)*60_JPIB+INT(DTIN,KIND=JPIB)-1_JPIB) &
                 & /INT(DTIN,KIND=JPIB),KIND=JPIM)
    IF ( ROFCDF%NRECSTART+NREQUIRED-1>NCDFSTP ) THEN
      WRITE(LOGNAM,*) "Run end later than forcing data",ROFCDF%NRECSTART,NREQUIRED,NCDFSTP
      STOP 9
    ENDIF
    WRITE(LOGNAM,*) "NetCDF CF time units: ",TRIM(ROF_TIME_AXIS%UNITS)
    WRITE(LOGNAM,*) "NetCDF CF calendar: ",TRIM(ROF_TIME_AXIS%CALENDAR)
    WRITE(LOGNAM,*) "NetCDF forcing interval [min]: ",ROF_TIME_AXIS%DT_MINUTES
    WRITE(LOGNAM,*) "NetCDF record at simulation start: ",ROFCDF%NRECSTART
  ELSE
    KMINSTAIN=DATE2MIN(SYEARIN*10000+SMONIN*100+SDAYIN,SHOURIN*100)
    ROFCDF%NSTART=KMINSTAIN
    IF ( KMINSTART .LT. KMINSTAIN ) THEN
      WRITE(LOGNAM,*) "Run start earlier than forcing data",TRIM(ROFCDF%CNAME),KMINSTART,KMINSTAIN
      STOP 9
    ENDIF
    KMINENDIN=KMINSTAIN+NCDFSTP*INT(DTIN/60,JPIM)
    IF ( KMINEND .GT. KMINENDIN ) THEN
      WRITE(LOGNAM,*) "Run end later than forcing data",TRIM(ROFCDF%CNAME),KMINEND,KMINENDIN
      STOP 9
    ENDIF
  ENDIF
ENDIF

END SUBROUTINE CMF_FORCING_INIT_CDF
#endif
!==========================================================
!+
!+
!+
!==========================================================
#ifdef UseCDF_CMF
SUBROUTINE CMF_INPMAT_INIT_CDF(LECMF2LAKEC)
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

! SAVE for OpenMP
INTEGER(KIND=JPIM),SAVE         :: IX,IY,ILEV
REAL(KIND=JPRB),SAVE            :: ZTMP
!$OMP THREADPRIVATE               (IY,ILEV,ZTMP)
!================================================
!*** 1. Allocate river-sequence input-matrix arrays
WRITE(LOGNAM,*) 'NX, NY, INPN =', NX, NY, INPN
ALLOCATE( INPX(NSEQMAX,INPN),INPY(NSEQMAX,INPN),INPA(NSEQMAX,INPN) )

!*** 2. Read forward input-matrix fields and map layers to river order
WRITE(LOGNAM,*) 'INPUT MATRIX netCDF', CINPMAT

CALL NCERROR (NF90_OPEN(CINPMAT,NF90_NOWRITE,NCID),'opening '//TRIM(CINPMAT) )

!** input matrix area
ALLOCATE( D2TMP(NX,NY,INPN) )
WRITE(LOGNAM,*)'INIT_MAP: inpa:',TRIM(CINPMAT)
CALL NCERROR ( NF90_INQ_VARID(NCID,'inpa',VARID),'getting id' )
CALL NCERROR ( NF90_GET_VAR(NCID,VARID,D2TMP,(/1,1,1/),(/NX,NY,INPN/)),'reading data' ) 
DO INPI=1, INPN
  CALL mapD2vecD(D2TMP(:,:,INPI:INPI),INPA(:,INPI:INPI))
END DO
DEALLOCATE( D2TMP )

!** input matrix IXIN
ALLOCATE( I2TMP(NX,NY,INPN) )

WRITE(LOGNAM,*)'INIT_MAP: inpx:',TRIM(CINPMAT)
CALL NCERROR ( NF90_INQ_VARID(NCID,'inpx',VARID),'getting id' )
CALL NCERROR ( NF90_GET_VAR(NCID,VARID,I2TMP,(/1,1,1/),(/NX,NY,INPN/)),'reading data' ) 
DO INPI=1, INPN
  CALL mapI2vecI(I2TMP(:,:,INPI:INPI),INPX(:,INPI:INPI))
END DO

!** input matrix IYIN
WRITE(LOGNAM,*)'INIT_MAP: inpy:',TRIM(CINPMAT)
CALL NCERROR ( NF90_INQ_VARID(NCID,'inpy',VARID),'getting id' )
CALL NCERROR ( NF90_GET_VAR(NCID,VARID,I2TMP,(/1,1,1/),(/NX,NY,INPN/)),'reading data' ) 
DO INPI=1, INPN
  CALL mapI2vecI(I2TMP(:,:,INPI:INPI),INPY(:,INPI:INPI))
END DO

DEALLOCATE( I2TMP )

!*** 3. Read optional inverse fields for lake-coupling workflows
! Normalize the inverse area weights so each target-grid cell's available contributions sum to one.
IF(PRESENT(LECMF2LAKEC) .AND. (LECMF2LAKEC .NE. 0)) THEN

  ISTATUS = NF90_INQ_VARID(NCID, 'levI', VARID)
  IF ( ISTATUS /= 0 ) THEN
    WRITE(LOGNAM,*) "Could not find levI variable in inpmat.nc: inverse interpolation not available"
    INPNI=-1  ! Not available 
  ELSE
    !* Find levels dimension  -- (assume "time ID" appears first. CF metadata conventions)
    CALL NCERROR( NF90_INQUIRE_VARIABLE(NCID,VARID,dimids=VDIMIDS),'getting levI dimensions ')
    CALL NCERROR( NF90_INQUIRE_DIMENSION(NCID,VDIMIDS(1),len=INPNI),'getting time len ') 
    WRITE(LOGNAM,*) 'Allocating INP*I: NXIN, NYIN, INPNI =', NXIN, NYIN, INPNI
    ALLOCATE( INPXI(NXIN,NYIN,INPNI),INPYI(NXIN,NYIN,INPNI),INPAI(NXIN,NYIN,INPNI) )
  
    WRITE(LOGNAM,*)'INIT_MAP: inpaI:',TRIM(CINPMAT)
    CALL NCERROR ( NF90_INQ_VARID(NCID,'inpaI',VARID),'getting id' )
    CALL NCERROR ( NF90_GET_VAR(NCID,VARID,INPAI,(/1,1,1/),(/NXIN,NYIN,INPNI/)),'reading data' ) 

    WRITE(LOGNAM,*)'INIT_MAP: inpx:',TRIM(CINPMAT)
    CALL NCERROR ( NF90_INQ_VARID(NCID,'inpxI',VARID),'getting id' )
    CALL NCERROR ( NF90_GET_VAR(NCID,VARID,INPXI,(/1,1,1/),(/NXIN,NYIN,INPNI/)),'reading data' ) 

    WRITE(LOGNAM,*)'INIT_MAP: inpy:',TRIM(CINPMAT)
    CALL NCERROR ( NF90_INQ_VARID(NCID,'inpyI',VARID),'getting id' )
    CALL NCERROR ( NF90_GET_VAR(NCID,VARID,INPYI,(/1,1,1/),(/NXIN,NYIN,INPNI/)),'reading data' )
  
    !! We normalize INPAI here as it is used to interpolate flood fraction (Input Area Inversed)
    WRITE(LOGNAM,*) 'INPAI normalization'
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
!+
!+
!+
!==========================================================
SUBROUTINE CMF_INPMAT_INIT_BIN
USE YOS_CMF_INPUT,           ONLY: TMPNAM, NX, NY, INPN
USE CMF_UTILS_MOD,           ONLY: INQUIRE_FID, CONV_END,  CONV_ENDI, mapR2vecD, mapI2vecI
USE YOS_CMF_MAP,             ONLY: NSEQMAX
IMPLICIT NONE
INTEGER(KIND=JPIM)              :: INPI
INTEGER(KIND=JPIM),ALLOCATABLE  :: I2TMP(:,:)
REAL(KIND=JPRM),ALLOCATABLE     :: R2TMP(:,:)
!================================================
!*** 1. Allocate river-sequence input-matrix arrays
WRITE(LOGNAM,*) 'NX, NY, INPN =', NX, NY, INPN
ALLOCATE( INPX(NSEQMAX,INPN),INPY(NSEQMAX,INPN),INPA(NSEQMAX,INPN) )

!*** 2. Read Binary records and map each layer to river order
WRITE(LOGNAM,*) 'INPUT MATRIX binary', CINPMAT

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

WRITE(LOGNAM,*) 'INPUT MATRIX done'

CLOSE(TMPNAM)
DEALLOCATE(I2TMP,R2TMP)

END SUBROUTINE CMF_INPMAT_INIT_BIN
!==========================================================

END SUBROUTINE CMF_FORCING_INIT
!####################################################################





!####################################################################
SUBROUTINE CMF_FORCING_GET(PBUFF)
USE YOS_CMF_INPUT,           ONLY: NXIN,NYIN,LROSPLIT, RMIS
USE CMF_UTILS_MOD,           ONLY: CMF_CheckNanB  !! check Udefined value
! Read forcing fields, then replace invalid or negative runoff values.
IMPLICIT NONE
REAL(KIND=JPRB),INTENT(INOUT)   :: PBUFF(:,:,:)

INTEGER(KIND=JPIM),SAVE         ::  IXIN, IYIN  !! FOR OUTPUT
!================================================
!*** 1. Read the configured Binary or NetCDF forcing source
IF( LINPCDF ) THEN
#ifdef UseCDF_CMF
  CALL CMF_FORCING_GET_CDF(PBUFF(:,:,:))
#endif
ELSE
  CALL CMF_FORCING_GET_BIN(PBUFF(:,:,:))
ENDIF 

!*** 2. Apply NaN replacement and the nonnegative bound to runoff fields
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

CONTAINS
!==========================================================
!+ CMF_FORCING_GET_BIN
!+ CMF_FORCING_GET_CDF
!==========================================================
SUBROUTINE CMF_FORCING_GET_BIN(PBUFF)
USE YOS_CMF_INPUT,           ONLY: NXIN,NYIN,LROSPLIT,DTIN
USE YOS_CMF_TIME,            ONLY: KMIN,KMINSTAIN
USE CMF_UTILS_MOD,           ONLY: CONV_END
IMPLICIT NONE
REAL(KIND=JPRB),INTENT(OUT) :: PBUFF(:,:,:)
INTEGER(KIND=JPIM)           :: IRECINP,KMINFILE
INTEGER(KIND=JPIB)            :: IRECOFFSET
REAL(KIND=JPRM)              :: R2TMP(NXIN,NYIN)
CHARACTER(LEN=512)           :: CIFNAME,CSUBNAME,CFILETAG

! Resolve the current record once; split runoff uses the same time and grouping.
IF( TRIM(CROFFREQ)=='SEQ' ) THEN
  CIFNAME=TRIM(CROFDIR)//'/'//TRIM(CROFPRE)//TRIM(CROFSUF)
  CFILETAG=' '
  KMINFILE=KMINSTAIN
  IF( KMIN<KMINFILE ) THEN
    WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_GET_BIN]: model time precedes Binary SEQ start:',KMIN,KMINFILE
    STOP 9
  ENDIF
  IF( DTIN<=0 ) THEN
    WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_GET_BIN]: DTIN must be positive for Binary SEQ.'
    STOP 9
  ENDIF
  IRECOFFSET=(INT(KMIN,KIND=JPIB)-INT(KMINFILE,KIND=JPIB))*60_JPIB
  IF( MOD(IRECOFFSET,INT(DTIN,KIND=JPIB))/=0_JPIB ) THEN
    WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_GET_BIN]: model time is not aligned with Binary SEQ records:',KMIN,DTIN
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
USE YOS_CMF_TIME,            ONLY: KMIN,KMINSTART,KMINSTAIN
USE YOS_CMF_INPUT,           ONLY: DTIN,NXIN,NYIN,LROSPLIT,LLEAPYR
USE CMF_UTILS_MOD,           ONLY: NCERROR,MIN2DATE,SPLITHOUR
USE NETCDF
IMPLICIT NONE
INTEGER(KIND=JPIM)           :: IRECINP,KMINFILE,IDATE,IHHMM,IHOUR0,IMIN0,NTIMEID,NCDFSTP
INTEGER(KIND=JPIM)           :: IOS,IERR
INTEGER(KIND=JPIB)            :: IOFFSET
CHARACTER(LEN=512)           :: CIFNAME,CFILETAG
CHARACTER(LEN=256)           :: CMESSAGE
REAL(KIND=JPRB),INTENT(OUT)  :: PBUFF(:,:,:)

IF( TRIM(CROFFREQ)=='SEQ' ) THEN
  CIFNAME=TRIM(ROFCDF%CNAME)
  IF( ROFCDF%LAUTOTIME ) THEN
    IOFFSET=(INT(KMIN,KIND=JPIB)-INT(KMINSTART,KIND=JPIB))*60_JPIB
    IF( DTIN<=0 ) THEN
      WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_GET_CDF]: DTIN must be positive for NetCDF SEQ.'
      STOP 9
    ENDIF
    IF( IOFFSET<0_JPIB .OR. MOD(IOFFSET,INT(DTIN,KIND=JPIB))/=0_JPIB ) THEN
      WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_GET_CDF]: unaligned SEQ time:',KMIN,DTIN
      STOP 9
    ENDIF
    IRECINP=ROFCDF%NRECSTART+INT(IOFFSET/INT(DTIN,KIND=JPIB),KIND=JPIM)
  ELSE
    IOFFSET=(INT(KMIN,KIND=JPIB)-INT(ROFCDF%NSTART,KIND=JPIB))*60_JPIB
    IF( DTIN<=0 ) THEN
      WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_GET_CDF]: DTIN must be positive for explicit NetCDF SEQ.'
      STOP 9
    ENDIF
    IF( IOFFSET<0_JPIB .OR. MOD(IOFFSET,INT(DTIN,KIND=JPIB))/=0_JPIB ) THEN
      WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_GET_CDF]: unaligned explicit SEQ time:',KMIN,DTIN
      STOP 9
    ENDIF
    IRECINP=INT(IOFFSET/INT(DTIN,KIND=JPIB)+1_JPIB,KIND=JPIM)
  ENDIF
ELSE
  IF( SYEARIN>0 .AND. KMIN<KMINSTAIN ) THEN
    WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_GET_CDF]: requested time precedes explicit grouped-file start:',KMIN,KMINSTAIN
    STOP 9
  ENDIF
  CALL FORCING_UTIL_RESOLVE_FILE(KMIN,CIFNAME,CFILETAG,KMINFILE,IRECINP)
  IF( ROFCDF%NCID<=0 .OR. TRIM(ROFCDF%CNAME)/=TRIM(CIFNAME) ) THEN
    IF( ROFCDF%NCID>0 ) CALL NCERROR(NF90_CLOSE(ROFCDF%NCID),'CLOSING :'//TRIM(ROFCDF%CNAME))
    ROFCDF%CNAME=TRIM(CIFNAME)
    ROFCDF%CVAR(1)=TRIM(CVNROF)
    ROFCDF%CVAR(2)=TRIM(CVNSUB)
    ROFCDF%NVARID(2)=-1
    IOS=NF90_OPEN(TRIM(ROFCDF%CNAME),NF90_NOWRITE,ROFCDF%NCID)
    IF( IOS/=NF90_NOERR ) THEN
      WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_GET_CDF]: cannot open file for ',KMIN,TRIM(ROFCDF%CNAME)
      CALL NCERROR(IOS,'OPENING :'//TRIM(ROFCDF%CNAME))
    ENDIF
    CALL NCERROR(NF90_INQ_VARID(ROFCDF%NCID,TRIM(ROFCDF%CVAR(1)),ROFCDF%NVARID(1)))
    IF( LROSPLIT ) CALL NCERROR(NF90_INQ_VARID(ROFCDF%NCID,TRIM(ROFCDF%CVAR(2)),ROFCDF%NVARID(2)))
    CALL NCERROR(NF90_INQ_DIMID(ROFCDF%NCID,TRIM(CVNTIME),NTIMEID),'GETTING TIME ID FORCING RUNOFF')
    CALL NCERROR(NF90_INQUIRE_DIMENSION(NCID=ROFCDF%NCID,DIMID=NTIMEID,LEN=NCDFSTP), &
    & 'GETTING TIME LENGTH FOR '//TRIM(ROFCDF%CNAME))
    IF( SYEARIN<=0 .AND. LCROFFREQ_EXPLICIT ) THEN
      CALL MIN2DATE(KMIN,IDATE,IHHMM)
      CALL SPLITHOUR(IHHMM,IHOUR0,IMIN0)
      CALL cf_resolve_time_record(ROFCDF%NCID,TRIM(CVNTIME),IDATE,IHOUR0,IMIN0,LLEAPYR, &
      & INT(DTIN,KIND=JPIM),NCDFSTP,ROF_TIME_AXIS,IRECINP,IERR,CMESSAGE)
      IF( IERR/=0 ) THEN
        WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_GET_CDF]: CF time validation failed for ',TRIM(ROFCDF%CNAME)
        WRITE(LOGNAM,*) '  requested minute, record, reason:',KMIN,IRECINP,TRIM(CMESSAGE)
        STOP 9
      ENDIF
      WRITE(LOGNAM,*) 'NetCDF forcing file opened:',TRIM(ROFCDF%CNAME), &
      & ' calendar=',TRIM(ROF_TIME_AXIS%CALENDAR),' interval[min]=',ROF_TIME_AXIS%DT_MINUTES
    ENDIF
    ROFCDF%NSTART=KMIN
    ROFCDF%NRECSTART=IRECINP
    WRITE(LOGNAM,*) 'NetCDF forcing file opened:',TRIM(ROFCDF%CNAME)
  ELSEIF( SYEARIN<=0 .AND. LCROFFREQ_EXPLICIT ) THEN
    IOFFSET=(INT(KMIN,KIND=JPIB)-INT(ROFCDF%NSTART,KIND=JPIB))*60_JPIB
    IF( DTIN<=0 ) THEN
      WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_GET_CDF]: DTIN must be positive for grouped NetCDF input.'
      STOP 9
    ENDIF
    IF( IOFFSET<0_JPIB .OR. MOD(IOFFSET,INT(DTIN,KIND=JPIB))/=0_JPIB ) THEN
      WRITE(LOGNAM,*) 'ERROR [CMF::FORCING_GET_CDF]: unaligned grouped-file time:',KMIN,DTIN
      STOP 9
    ENDIF
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
! -- called from "Main Program / Coupler" or CMF_DRV_ADVANCE
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
!============================
!*** 1. Gather catchment-grid flood fraction across MPI regions
CALL vecD2mapD(D2FLDFRC,D2MAPTMP)             !! MPI node data is gathered by vecP2mapR
#ifdef UseMPI_CMF
  CALL CMF_MPI_AllReduce_D2MAP(D2MAPTMP)
#endif

!*** 2. Interpolate the gathered field to the runoff grid
CALL INTERPI(D2MAPTMP,PBUFF(:,:,1))        !!  Inverse interpolation (CaMa grid -> input runoff grid)

CONTAINS
!==========================================================
!+ INTERPI
!==========================================================
SUBROUTINE INTERPI(PBUFFIN,PBUFFOUT)
! Interpolate a catchment-grid field to each runoff-grid cell using inverse weights.
USE YOS_CMF_INPUT,           ONLY: NXIN, NYIN, NX,NY
IMPLICIT NONE
REAL(KIND=JPRB),INTENT(IN)      :: PBUFFIN(:,:)     !! CaMa-Flood variable on catchment (NX*NY)
REAL(KIND=JPRB),INTENT(OUT)     :: PBUFFOUT(:,:)    !! output on target grid = input runoff grid (NXIN * NYIN)

INTEGER(KIND=JPIM)              :: IX,IY,INP,IXIN,IYIN
! ========================================================
!*** 1. Require inverse input-matrix data
IF ( INPNI == -1 ) THEN
  WRITE(LOGNAM,*) "INPNI==-1, no inverse interpolation possible"
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
! -- called from "Main Program / Coupler" or CMF_DRV_ADVANCE
USE YOS_CMF_INPUT,           ONLY: LROSPLIT,LWEVAP
USE YOS_CMF_PROG,            ONLY: D2RUNOFF,D2ROFSUB,D2WEVAP
IMPLICIT NONE 
! Declaration of arguments 
REAL(KIND=JPRB), INTENT(IN)     :: PBUFF(:,:,:)
!============================
!*** 1. Map total and optional subsurface runoff to CaMa-Flood catchments
! Use input-matrix weights when enabled; otherwise use the direct-resolution mapping.
IF (LINTERP) THEN ! mass conservation using "input matrix table (inpmat)"
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
    WRITE(LOGNAM,*)  "LWEVAP is true but evaporation not provide in input array for interpolation"
    WRITE(LOGNAM,*)  "CMF_FORCING_PUT(PBUFF), PBUFF should have 3 fields for interpolation "
    STOP 9
  ENDIF
ENDIF

CONTAINS
!==========================================================
!+ ROFF_INTERP : runoff interpolation with mass conservation using "input matrix table (inpmat)"
!+ CONV_RESOL : nearest point runoff interpolation
!==========================================================
SUBROUTINE ROFF_INTERP(PBUFFIN,PBUFFOUT)
! interporlate runoff using "input matrix"
USE YOS_CMF_MAP,             ONLY: NSEQMAX
USE YOS_CMF_INPUT,           ONLY: NXIN, NYIN, INPN, RMIS
IMPLICIT NONE
REAL(KIND=JPRB),INTENT(IN)      :: PBUFFIN(:,:)     !! default [mm/dt] 
REAL(KIND=JPRB),INTENT(OUT)     :: PBUFFOUT(:,:)    !! m3/s
! SAVE for OMP
INTEGER(KIND=JPIM),SAVE  ::  ISEQ
INTEGER(KIND=JPIM),SAVE  ::  IXIN, IYIN, INPI  !! FOR OUTPUT
!$OMP THREADPRIVATE    (IXIN, IYIN, INPI)
!============================
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
        WRITE(LOGNAM,*)  "error"
        WRITE(LOGNAM,*)  'XXX',ISEQ,INPI,IXIN,IYIN
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
!! use runoff data without any interporlation. map resolution & runoff resolution should be same
USE YOS_CMF_MAP,             ONLY: NSEQMAX, D2GRAREA
USE YOS_CMF_INPUT,           ONLY: RMIS
USE CMF_UTILS_MOD,           ONLY: mapD2vecD
IMPLICIT NONE

REAL(KIND=JPRB),INTENT(IN)      :: PBUFFIN(:,:)     !! default [mm/dt] 
REAL(KIND=JPRB),INTENT(OUT)     :: PBUFFOUT(:,:)    !! m3/s

REAL(KIND=JPRB),ALLOCATABLE     :: D2TEMP(:,:)

INTEGER(KIND=JPIM),SAVE         ::  ISEQ
! ================================================
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
WRITE(LOGNAM,*) "CMF::FORCING_END: Finalize forcing module"

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

WRITE(LOGNAM,*) "CMF::FORCING_END: end"

END SUBROUTINE CMF_FORCING_END
!####################################################################

END MODULE CMF_CTRL_FORCING_MOD
