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
USE PARKIND1,                ONLY: JPIM, JPRB, JPRM
USE YOS_CMF_INPUT,           ONLY: LOGNAM
!============================
IMPLICIT NONE
SAVE
!*** NAMELIST/NFORCE/
! Forcing configulation / foprcing mapping table "input matrix"
LOGICAL                         :: LINPCDF             !! true : netCDF runoff forcing
LOGICAL                         :: LINPEND             !! true  for input    endian conversion

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
CHARACTER(LEN=256)              :: CVNROF           !! NetCDF VARNAME of runoff. Default "runoff"/
CHARACTER(LEN=256)              :: CVNSUB           !! NetCDF VARNAME of sub-surface runoff.

INTEGER(KIND=JPIM)              :: SYEARIN             !! START YEAR IN NETCDF INPUT RUNOFF
INTEGER(KIND=JPIM)              :: SMONIN              !! START MONTH IN NETCDF INPUT RUNOFF
INTEGER(KIND=JPIM)              :: SDAYIN              !! START DAY IN NETCDF INPUT RUNOFF
INTEGER(KIND=JPIM)              :: SHOURIN             !! START HOUR IN NETCDF INPUT RUNOFF 

NAMELIST/NFORCE/   LINTERP, LINPEND, LINPCDF, LITRPCDF, CINPMAT, DROFUNIT, &
                    CROFDIR,CROFPRE, CROFSUF, CSUBDIR,  CSUBPRE, CSUBSUF, &
                    CROFCDF,CVNROF,  CVNSUB,  SYEARIN,  SMONIN,SDAYIN,SHOURIN

!* input file dimention (set by diminfo.txt)
INTEGER(KIND=JPIM)              :: NXIN                    !! NUMBER OF GRIDS IN HORIZONTAL
INTEGER(KIND=JPIM)              :: NYIN                    !! NUMBER OF GRIDS IN VERTICAL
INTEGER(KIND=JPIM)              :: INPN                    !! MAX INPUT NUMBER

!* local variable
INTEGER(KIND=JPIM)              :: NCID        !! netCDF file     ID
INTEGER(KIND=JPIM)              :: NVARID(2)   !! netCDF variable ID

#ifdef UseCDF
TYPE TYPEROF
CHARACTER(LEN=256)              :: CNAME       !! netCDF file name
CHARACTER(LEN=256)              :: CVAR(2)     !! netCDF variable name
INTEGER(KIND=JPIM)              :: NCID        !! netCDF file     ID
INTEGER(KIND=JPIM)              :: NVARID(2)   !! netCDF variable ID
INTEGER(KIND=JPIM)              :: NSTART      !! Start date of netNDF (in KMIN)
END TYPE TYPEROF
TYPE(TYPEROF)                   :: ROFCDF      !! Derived type for Runoff input 
#endif

! input matrix (converted from NX:NY*INPN to NSEQMAX*INPN)
INTEGER(KIND=JPIM),ALLOCATABLE  :: INPX(:,:)        !! INPUT GRID XIN
INTEGER(KIND=JPIM),ALLOCATABLE  :: INPY(:,:)        !! INPUT GRID YIN
REAL(KIND=JPRB),ALLOCATABLE     :: INPA(:,:)        !! INPUT AREA

CONTAINS
!####################################################################
! -- CMF_FORCING_NMLIST : Read setting from Namelist
! -- CMF_FORCING_INIT   : Initialize forcing data file
! -- CMF_FORCING_PUT    : Put forcing data (PBUFF) to CaMa-Flood
! -- CMF_FORCING_GET    : Read forcing data from file (save as "data buffer" PBUFF)
! -- CMF_FORCING_END    : Finalize forcing data file
!
!####################################################################
SUBROUTINE CMF_FORCING_NMLIST
! reed setting from namelist
! -- Called from CMF_DRV_NMLIST
USE YOS_CMF_INPUT,      ONLY: CSETFILE,NSETFILE,LROSPLIT
USE YOS_CMF_TIME,       ONLY: YYYY0
USE CMF_UTILS_MOD,      ONLY: INQUIRE_FID
IMPLICIT NONE
!================================================
WRITE(LOGNAM,*) ""
WRITE(LOGNAM,*) "!---------------------!"

!*** 1. open namelist
NSETFILE=INQUIRE_FID()
OPEN(NSETFILE,FILE=CSETFILE,STATUS="OLD")
WRITE(LOGNAM,*) "CMF::FORCING_NMLIST: namelist OPEN in unit: ", TRIM(CSETFILE), NSETFILE 

!*** 2. default value
LINPCDF=.FALSE.
LINPEND=.FALSE.
LINTERP=.FALSE.
LITRPCDF=.FALSE.
CINPMAT="NONE"
DROFUNIT=86400*1.D3             !! defaults mm/day -> m3/m2/s

CROFDIR="./runoff/"
CROFPRE="Roff____"           !! defaults runoff file name Roff____YYYYMMDD.one
CROFSUF=".one"

CSUBDIR="./runoff/"
CSUBPRE="Rsub____"           !! defaults runoff file name Rsub____YYYYMMDD.one
CSUBSUF=".one"

CROFCDF="NONE"
CVNROF="runoff"
CVNSUB="NONE"
IF( LROSPLIT )THEN
  CVNROF="Qs"
  CVNSUB="Qsb"
ENDIF

SYEARIN=0                       !! netCDF input file start date (set to 0 when not used)
SMONIN=0
SDAYIN=0
SHOURIN=0

!*** 3. read namelist
REWIND(NSETFILE)
READ(NSETFILE,NML=NFORCE)

WRITE(LOGNAM,*)     "=== NAMELIST, NFORCE ==="
WRITE(LOGNAM,*)     "LINPCDF:   ", LINPCDF
WRITE(LOGNAM,*)     "LINTERP:   ", LINTERP
WRITE(LOGNAM,*)     "LITRPCDF:  ", LITRPCDF
WRITE(LOGNAM,*)     "CINPMAT:   ", TRIM(CINPMAT)
WRITE(LOGNAM,*)     "LROSPLIT:  ", LROSPLIT
IF( .not. LINPCDF )THEN
  WRITE(LOGNAM,*)   "CROFDIR:   ", TRIM(CROFDIR)
  WRITE(LOGNAM,*)   "CROFPRE:   ", TRIM(CROFPRE)
  WRITE(LOGNAM,*)   "CROFSUF:   ", TRIM(CROFSUF)
  IF( LROSPLIT )THEN
    WRITE(LOGNAM,*) "CSUBDIR:   ", TRIM(CSUBDIR)
    WRITE(LOGNAM,*) "CSUBPRE:   ", TRIM(CSUBPRE)
    WRITE(LOGNAM,*) "CSUBSUF:   ", TRIM(CSUBSUF)
  ENDIF
ELSE
  WRITE(LOGNAM,*)   "CROFCDF:   ", TRIM(CROFCDF)
  WRITE(LOGNAM,*)   "CVNROF:    ", TRIM(CVNROF)
  IF( LROSPLIT )THEN
    WRITE(LOGNAM,*) "CVNSUB:    ", TRIM(CVNSUB)
  ENDIF
  WRITE(LOGNAM,*)   "SYEARIN,SMONIN,SDAYIN,SHOURIN ", SYEARIN,SMONIN,SDAYIN,SHOURIN
ENDIF
IF( LINPEND )THEN
  WRITE(LOGNAM,*)   "LINPEND:   ", LINPEND
ENDIF

CLOSE(NSETFILE)

!*** 4. modify base date (shared for KMIN)
IF( LINPCDF )THEN
  YYYY0=MIN(YYYY0,SYEARIN)
ENDIF

WRITE(LOGNAM,*) "CMF::FORCING_NMLIST: end" 

END SUBROUTINE CMF_FORCING_NMLIST
!####################################################################





!####################################################################
SUBROUTINE CMF_FORCING_INIT
! Initialize/open netcdf input 
! -- called from "Main Program / Coupler"
IMPLICIT NONE
!================================================
WRITE(LOGNAM,*) ""
WRITE(LOGNAM,*) "!---------------------!"

WRITE(LOGNAM,*) "CMF::FORCING_INIT: Initialize runoff forcing file (only for netCDF)" 
IF( LINPCDF ) THEN
#ifdef UseCDF
  CALL CMF_FORCING_INIT_CDF
#endif
ENDIF
IF( LINTERP ) THEN
  IF( LITRPCDF )THEN
#ifdef UseCDF
    CALL CMF_INPMAT_INIT_CDF
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
#ifdef UseCDF
SUBROUTINE CMF_FORCING_INIT_CDF
USE YOS_CMF_INPUT,           ONLY: LROSPLIT,  DTIN
USE YOS_CMF_TIME,            ONLY: KMINSTAIN, KMINSTART, KMINEND
USE CMF_UTILS_MOD,           ONLY: NCERROR,   DATE2MIN
USE NETCDF
IMPLICIT NONE
!* Local Variables 
INTEGER(KIND=JPIM)              :: NTIMEID,NCDFSTP
INTEGER(KIND=JPIM)              :: KMINENDIN
!================================================
!*** 1. calculate KMINSTAINP (start KMIN for forcing)
KMINSTAIN=DATE2MIN(SYEARIN*10000+SMONIN*100+SDAYIN,SHOURIN*100)

!*** 2. Initialize Type for Runoff CDF:
ROFCDF%CNAME=TRIM(CROFCDF)
ROFCDF%CVAR(1)=TRIM(CVNROF)
ROFCDF%CVAR(2)=TRIM(CVNSUB)
IF ( .not. LROSPLIT ) THEN
  ROFCDF%CVAR(2)="NONE"
  ROFCDF%NVARID(2)=-1
ENDIF
ROFCDF%NSTART=KMINSTAIN
WRITE(LOGNAM,*) "CMF::FORCING_INIT_CDF:", TRIM(ROFCDF%CNAME), TRIM(ROFCDF%CVAR(1))

!*** 3. Open netCDF ruoff file
CALL NCERROR( NF90_OPEN(TRIM(ROFCDF%CNAME),NF90_NOWRITE,ROFCDF%NCID),'OPENING :'//ROFCDF%CNAME )
CALL NCERROR( NF90_INQ_VARID(ROFCDF%NCID,TRIM(ROFCDF%CVAR(1)),ROFCDF%NVARID(1)) )

IF ( LROSPLIT ) THEN
  CALL NCERROR( NF90_INQ_VARID(ROFCDF%NCID,ROFCDF%CVAR(2),ROFCDF%NVARID(2)) )
ENDIF 
CALL NCERROR( NF90_INQ_DIMID(ROFCDF%NCID,'time',NTIMEID),'GETTING TIME ID FORCING RUNOFF')
CALL NCERROR( NF90_INQUIRE_DIMENSION(NCID=ROFCDF%NCID,DIMID=NTIMEID,LEN=NCDFSTP),'GETTING TIME LENGTH')

WRITE(LOGNAM,*) "CMF::FORCING_INIT_CDF: CNAME,NCID,VARID", TRIM(ROFCDF%CNAME),ROFCDF%NCID,ROFCDF%NVARID(1)

!*** 4. check runoff forcing time 
IF ( KMINSTART .LT. KMINSTAIN ) THEN 
  WRITE(LOGNAM,*) "Run start earlier than forcing data", TRIM(ROFCDF%CNAME), KMINSTART, KMINSTAIN
  STOP 9
ENDIF

KMINENDIN=KMINSTAIN + NCDFSTP*INT(DTIN/60,JPIM)
IF ( KMINEND .GT. KMINENDIN  ) THEN 
  WRITE(LOGNAM,*) "Run end later than forcing data", TRIM(ROFCDF%CNAME), KMINEND, KMINENDIN
  STOP 9
ENDIF

END SUBROUTINE CMF_FORCING_INIT_CDF
#endif
!==========================================================
!+
!+
!+
!==========================================================
#ifdef UseCDF
SUBROUTINE CMF_INPMAT_INIT_CDF
USE YOS_CMF_INPUT,           ONLY: NX, NY, INPN
USE YOS_CMF_MAP,             ONLY: NSEQMAX
USE CMF_UTILS_MOD,           ONLY: INQUIRE_FID, NCERROR, MAP2VECD, MAP2VECI
USE NETCDF
IMPLICIT NONE
INTEGER(KIND=JPIM)              :: INPI
INTEGER(KIND=JPIM)              :: VARID
INTEGER(KIND=JPIM),ALLOCATABLE  :: I2TMP(:,:,:)
REAL(KIND=JPRB),ALLOCATABLE     :: D2TMP(:,:,:)
!================================================
!*** 1. allocate input matrix variables
WRITE(LOGNAM,*) 'NX, NY, INPN =', NX, NY, INPN
ALLOCATE( INPX(NSEQMAX,INPN),INPY(NSEQMAX,INPN),INPA(NSEQMAX,INPN) )

!*** 2. Read Input Matrix
WRITE(LOGNAM,*) 'INPUT MATRIX netCDF', CINPMAT

CALL NCERROR (NF90_OPEN(CINPMAT,NF90_NOWRITE,NCID),'opening '//TRIM(CINPMAT) )

!** input matrix area
ALLOCATE( D2TMP(NX,NY,INPN) )
WRITE(LOGNAM,*)'INIT_MAP: inpa:',TRIM(CINPMAT)
CALL NCERROR ( NF90_INQ_VARID(NCID,'inpa',VARID),'getting id' )
CALL NCERROR ( NF90_GET_VAR(NCID,VARID,D2TMP,(/1,1,1/),(/NX,NY,INPN/)),'reading data' ) 
DO INPI=1, INPN
  CALL MAP2VECD(D2TMP(:,:,INPI:INPI),INPA(:,INPI:INPI))
END DO
DEALLOCATE( D2TMP )

!** input matrix IXIN
ALLOCATE( I2TMP(NX,NY,INPN) )

WRITE(LOGNAM,*)'INIT_MAP: inpx:',TRIM(CINPMAT)
CALL NCERROR ( NF90_INQ_VARID(NCID,'inpx',VARID),'getting id' )
CALL NCERROR ( NF90_GET_VAR(NCID,VARID,I2TMP,(/1,1,1/),(/NX,NY,INPN/)),'reading data' ) 
DO INPI=1, INPN
  CALL MAP2VECI(I2TMP(:,:,INPI:INPI),INPX(:,INPI:INPI))
END DO

!** input matrix IYIN
WRITE(LOGNAM,*)'INIT_MAP: inpy:',TRIM(CINPMAT)
CALL NCERROR ( NF90_INQ_VARID(NCID,'inpy',VARID),'getting id' )
CALL NCERROR ( NF90_GET_VAR(NCID,VARID,I2TMP,(/1,1,1/),(/NX,NY,INPN/)),'reading data' ) 
DO INPI=1, INPN
  CALL MAP2VECI(I2TMP(:,:,INPI:INPI),INPY(:,INPI:INPI))
END DO

DEALLOCATE( I2TMP )

END SUBROUTINE CMF_INPMAT_INIT_CDF
#endif
!==========================================================
!+
!+
!+
!==========================================================
SUBROUTINE CMF_INPMAT_INIT_BIN
USE YOS_CMF_INPUT,           ONLY: TMPNAM, NX, NY, INPN
USE CMF_UTILS_MOD,           ONLY: INQUIRE_FID, CONV_END,  CONV_ENDI, MAP2VEC, MAP2VECI
USE YOS_CMF_MAP,             ONLY: NSEQMAX
IMPLICIT NONE
INTEGER(KIND=JPIM)              :: INPI
INTEGER(KIND=JPIM),ALLOCATABLE  :: I2TMP(:,:)
REAL(KIND=JPRM),ALLOCATABLE     :: R2TMP(:,:)
!================================================
!*** 1. allocate input matrix variables
WRITE(LOGNAM,*) 'NX, NY, INPN =', NX, NY, INPN
ALLOCATE( INPX(NSEQMAX,INPN),INPY(NSEQMAX,INPN),INPA(NSEQMAX,INPN) )

!*** 2. Read Input Matrix
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
   CALL MAP2VECI(I2TMP,INPX(:,INPI:INPI))
  READ(TMPNAM,REC=  INPN+INPI) I2TMP
   CALL MAP2VECI(I2TMP,INPY(:,INPI:INPI))
  READ(TMPNAM,REC=2*INPN+INPI) R2TMP
   CALL MAP2VEC( R2TMP,INPA(:,INPI:INPI))
END DO

CLOSE(TMPNAM)
DEALLOCATE(I2TMP,R2TMP)

END SUBROUTINE CMF_INPMAT_INIT_BIN
!==========================================================

END SUBROUTINE CMF_FORCING_INIT
!####################################################################





!####################################################################
SUBROUTINE CMF_FORCING_GET(PBUFF)
! read runoff from file
IMPLICIT NONE
REAL(KIND=JPRB),INTENT(INOUT)   :: PBUFF(:,:,:)
!================================================
IF( LINPCDF ) THEN
#ifdef UseCDF
  CALL CMF_FORCING_GET_CDF(PBUFF(:,:,:))
#endif
ELSE
  CALL CMF_FORCING_GET_BIN(PBUFF(:,:,:))
ENDIF 

CONTAINS
!==========================================================
!+ CMF_FORCING_GET_BIN
!+ CMF_FORCING_GET_CDF
!==========================================================
SUBROUTINE CMF_FORCING_GET_BIN(PBUFF)
USE YOS_CMF_INPUT,           ONLY: TMPNAM,NXIN,NYIN,LROSPLIT,DTIN
USE YOS_CMF_TIME,            ONLY: IYYYY, IMM, IDD, IHOUR, IMIN
USE CMF_UTILS_MOD,           ONLY: CONV_END,INQUIRE_FID
IMPLICIT NONE
REAL(KIND=JPRB),INTENT(OUT)     :: PBUFF(:,:,:)
!* Local variables
INTEGER(KIND=JPIM)              :: IRECINP
INTEGER(KIND=JPIM)              :: ISEC
CHARACTER(LEN=256)              :: CIFNAME             !! INPUT FILE
CHARACTER(LEN=256)              :: CDATE               !!
REAL(KIND=JPRM)                 :: R2TMP(NXIN,NYIN)
!================================================
!*** 1. calculate IREC for sub-daily runoff
ISEC    = IHOUR*60*60+IMIN*60   !! current second in a day
IRECINP = int( ISEC/DTIN ) +1   !! runoff irec (sub-dairy runoff)

!*** 2. set file name
WRITE(CDATE,'(I4.4,I2.2,I2.2)') IYYYY,IMM,IDD
CIFNAME=TRIM(CROFDIR)//'/'//TRIM(CROFPRE)//TRIM(CDATE)//TRIM(CROFSUF)
WRITE(LOGNAM,*) "CMF::FORCING_GET_BIN:",TRIM(CIFNAME)

!*** 3. open & read runoff
TMPNAM=INQUIRE_FID()
OPEN(TMPNAM,FILE=CIFNAME,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4*NXIN*NYIN)
READ(TMPNAM,REC=IRECINP) R2TMP
CLOSE(TMPNAM)
WRITE(LOGNAM,*) "IRECINP:", IRECINP

!*** 4. copy runoff to PBUSS, endian conversion is needed
IF( LINPEND ) CALL CONV_END(R2TMP,NXIN,NYIN)
PBUFF(:,:,1)=R2TMP(:,:)

!*** for sub-surface runoff withe LROSPLIT
PBUFF(:,:,2)=0.D0  !! Plain Binary subsurface runoff to be added later
IF ( LROSPLIT ) THEN
  CIFNAME=TRIM(CSUBDIR)//'/'//TRIM(CSUBPRE)//TRIM(CDATE)//TRIM(CSUBSUF)
  WRITE(LOGNAM,*) "CMF::FORCING_GET_BIN: (sub-surface)",TRIM(CIFNAME)

  TMPNAM=INQUIRE_FID()
  OPEN(TMPNAM,FILE=CIFNAME,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4*NXIN*NYIN)
  READ(TMPNAM,REC=IRECINP) R2TMP
  CLOSE(TMPNAM)
  WRITE(LOGNAM,*) "IRECINP:", IRECINP

  IF( LINPEND ) CALL CONV_END(R2TMP,NXIN,NYIN)
  PBUFF(:,:,2)=R2TMP(:,:)
ENDIF

END SUBROUTINE CMF_FORCING_GET_BIN
! ================================================
!+
!+
!+
! ================================================
#ifdef UseCDF
SUBROUTINE CMF_FORCING_GET_CDF(PBUFF)
! Read forcing data from netcdf
! -- call from CMF_FORCING_GET
USE YOS_CMF_TIME,            ONLY: KMIN, IYYYYMMDD, IHHMM
USE YOS_CMF_INPUT,           ONLY: DTIN, NXIN, NYIN
USE CMF_UTILS_MOD,           ONLY: NCERROR
USE NETCDF
IMPLICIT NONE
!* Declaration of arguments 
REAL(KIND=JPRB),INTENT(OUT)     :: PBUFF(:,:,:)
!* Local variables
INTEGER(KIND=JPIM)              :: IRECINP
! ================================================
!*** 1. calculate irec
IRECINP=INT( (KMIN-ROFCDF%NSTART)*60_JPIM,JPIM ) / INT(DTIN,JPIM) + 1     !! (second from netcdf start time) / (input time step)

!*** 2. read runoff
CALL NCERROR( NF90_GET_VAR(ROFCDF%NCID,ROFCDF%NVARID(1),PBUFF(:,:,1),(/1,1,IRECINP/),(/NXIN,NYIN,1/)),'READING RUNOFF 1 ' )
IF ( ROFCDF%NVARID(2) .NE. -1 ) THEN
  CALL NCERROR( NF90_GET_VAR(ROFCDF%NCID,ROFCDF%NVARID(2),PBUFF(:,:,2),(/1,1,IRECINP/),(/NXIN,NYIN,1/)),'READING RUNOFF 2' )
ENDIF
WRITE(LOGNAM,*) "CMF::FORCING_GET_CDF: read runoff:",IYYYYMMDD,IHHMM,IRECINP

END SUBROUTINE CMF_FORCING_GET_CDF
#endif
! ================================================

END SUBROUTINE CMF_FORCING_GET
!####################################################################





!####################################################################
SUBROUTINE CMF_FORCING_PUT(PBUFF)
! interporlate with inpmat, then send runoff data to CaMa-Flood 
! -- called from "Main Program / Coupler" or CMF_DRV_ADVANCE
USE YOS_CMF_INPUT,           ONLY: LROSPLIT
USE YOS_CMF_PROG,            ONLY: D2RUNOFF,D2ROFSUB
IMPLICIT NONE 
! Declaration of arguments 
REAL(KIND=JPRB), INTENT(IN)     :: PBUFF(:,:,:)
!============================
! Runoff interpolation & unit conversion (mm/dt -> m3/sec)
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

CONTAINS
!==========================================================
!+ ROFF_INTERP : runoff interpolation with mass conservation using "input matrix table (inpmat)"
!+ CONV_RESOL : nearest point runoff interpolation
!==========================================================
SUBROUTINE ROFF_INTERP(PBUFFIN,PBUFFOUT)
! interporlate runoff using "input matrix"
USE YOS_CMF_MAP,             ONLY: NSEQALL
USE YOS_CMF_INPUT,           ONLY: NXIN, NYIN, INPN, RMIS
IMPLICIT NONE
REAL(KIND=JPRB),INTENT(IN)      :: PBUFFIN(:,:)     !! default [mm/dt] 
REAL(KIND=JPRB),INTENT(OUT)     :: PBUFFOUT(:,:)    !! m3/s
!$ SAVE
INTEGER(KIND=JPIM)  ::  ISEQ
INTEGER(KIND=JPIM)  ::  IXIN, IYIN, INPI  !! FOR OUTPUT
!$OMP THREADPRIVATE    (IXIN, IYIN, INPI)
!============================
!$OMP PARALLEL DO
DO ISEQ=1, NSEQALL
  PBUFFOUT(ISEQ,1)=0.D0
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
  PBUFFOUT(ISEQ,1)=MAX(PBUFFOUT(ISEQ,1), 0.D0)
END DO
!$OMP END PARALLEL DO
END SUBROUTINE ROFF_INTERP
!==========================================================
!+
!+
!==========================================================
SUBROUTINE CONV_RESOL(PBUFFIN,PBUFFOUT)
!! use runoff data without any interporlation. map resolution & runoff resolution should be same
USE YOS_CMF_MAP,             ONLY: NSEQALL, NSEQMAX, D2GRAREA
USE YOS_CMF_INPUT,           ONLY: RMIS
USE CMF_UTILS_MOD,           ONLY: MAP2VECD
IMPLICIT NONE

REAL(KIND=JPRB),INTENT(IN)      :: PBUFFIN(:,:)     !! default [mm/dt] 
REAL(KIND=JPRB),INTENT(OUT)     :: PBUFFOUT(:,:)    !! m3/s

REAL(KIND=JPRB),ALLOCATABLE     :: D2TEMP(:,:)

!$ SAVE
INTEGER(KIND=JPIM)  ::  ISEQ
! ================================================
ALLOCATE(D2TEMP(NSEQMAX,1))
CALL MAP2VECD(PBUFFIN,D2TEMP)
!$OMP PARALLEL DO
DO ISEQ=1, NSEQALL
  IF( D2TEMP(ISEQ,1).NE.RMIS )THEN
    PBUFFOUT(ISEQ,1) = D2TEMP(ISEQ,1) * D2GRAREA(ISEQ,1) / DROFUNIT
    PBUFFOUT(ISEQ,1) = MAX(PBUFFOUT(ISEQ,1), 0.D0)
  ELSE
    PBUFFOUT(ISEQ,1)=0.D0
  ENDIF
END DO
!$OMP END PARALLEL DO
END SUBROUTINE CONV_RESOL
!==========================================================

END SUBROUTINE CMF_FORCING_PUT
!####################################################################






!####################################################################
SUBROUTINE CMF_FORCING_END
#ifdef UseCDF
USE CMF_UTILS_MOD,         ONLY: NCERROR
USE NETCDF
#endif
IMPLICIT NONE
!================================================
WRITE(LOGNAM,*) ""
WRITE(LOGNAM,*) "!---------------------!"
WRITE(LOGNAM,*) "CMF::FORCING_END: Finalize forcing module"

!* Close Input netcdf 
IF( LINPCDF ) THEN
#ifdef UseCDF
  CALL NCERROR( NF90_CLOSE(ROFCDF%NCID))
  WRITE(LOGNAM,*) "input netCDF runoff closed:",ROFCDF%NCID
#endif
ENDIF 

WRITE(LOGNAM,*) "CMF::FORCING_END: end"

END SUBROUTINE CMF_FORCING_END
!####################################################################

END MODULE CMF_CTRL_FORCING_MOD
