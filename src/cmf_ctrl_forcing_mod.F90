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
USE PARKIND1,                ONLY: JPIM, JPRB, JPRM
USE YOS_CMF_INPUT,           ONLY: LOGNAM
USE YOS_CMF_MAP,             ONLY: INPX, INPY, INPA, INPXI, INPYI, INPAI, INPNI
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

!* local variable
INTEGER(KIND=JPIM)              :: NCID        !! netCDF file     ID
INTEGER(KIND=JPIM)              :: NVARID(2)   !! netCDF variable ID

#ifdef UseCDF_CMF
TYPE TYPEROF
CHARACTER(LEN=256)              :: CNAME       !! netCDF file name
CHARACTER(LEN=256)              :: CVAR(3)     !! netCDF variable name
INTEGER(KIND=JPIM)              :: NCID        !! netCDF file     ID
INTEGER(KIND=JPIM)              :: NVARID(3)   !! netCDF variable ID
INTEGER(KIND=JPIM)              :: NSTART      !! Start date of netNDF (in KMIN)
END TYPE TYPEROF
TYPE(TYPEROF)                   :: ROFCDF      !! Derived type for Runoff input 
#endif

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
LINPCDF =.FALSE.
LINPEND =.FALSE.
LINTERP =.FALSE.
LITRPCDF=.FALSE.
CINPMAT ="NONE"
DROFUNIT=86400*1000._JPRB             !! defaults mm/day -> m3/m2/s

CROFDIR="./runoff/"
CROFPRE="Roff____"           !! defaults runoff file name Roff____YYYYMMDD.one
CROFSUF=".one"

CSUBDIR="./runoff/"
CSUBPRE="Rsub____"           !! defaults runoff file name Rsub____YYYYMMDD.one
CSUBSUF=".one"

CROFCDF="NONE"
CVNROF ="runoff"
CVNSUB ="NONE"
IF( LROSPLIT )THEN
  CVNROF="Qs"
  CVNSUB="Qsb"
ENDIF

SYEARIN=0                       !! netCDF input file start date (set to 0 when not used)
SMONIN =0
SDAYIN =0
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
SUBROUTINE CMF_FORCING_INIT(LECMF2LAKEC)
! Initialize/open netcdf input 
! -- called from "Main Program / Coupler"
IMPLICIT NONE

INTEGER(KIND=JPIM),OPTIONAL,INTENT(IN) :: LECMF2LAKEC   !! Lake coupling: this is currently only used in ECMWF

!================================================
WRITE(LOGNAM,*) ""
WRITE(LOGNAM,*) "!---------------------!"

WRITE(LOGNAM,*) "CMF::FORCING_INIT: Initialize runoff forcing file (only for netCDF)" 
IF( LINPCDF ) THEN
#ifdef UseCDF_CMF
  CALL CMF_FORCING_INIT_CDF
#endif
ENDIF
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
USE YOS_CMF_INPUT,           ONLY: LROSPLIT,  LWEVAP,    DTIN
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
IF ( .NOT. LWEVAP ) THEN
  ROFCDF%CVAR(3)="NONE"
  ROFCDF%NVARID(3)=-1
ENDIF 

ROFCDF%NSTART=KMINSTAIN
WRITE(LOGNAM,*) "CMF::FORCING_INIT_CDF:", TRIM(ROFCDF%CNAME), TRIM(ROFCDF%CVAR(1))

!*** 3. Open netCDF ruoff file
CALL NCERROR( NF90_OPEN(TRIM(ROFCDF%CNAME),NF90_NOWRITE,ROFCDF%NCID),'OPENING :'//ROFCDF%CNAME )
CALL NCERROR( NF90_INQ_VARID(ROFCDF%NCID,TRIM(ROFCDF%CVAR(1)),ROFCDF%NVARID(1)) )

IF ( LROSPLIT ) THEN
  CALL NCERROR( NF90_INQ_VARID(ROFCDF%NCID,ROFCDF%CVAR(2),ROFCDF%NVARID(2)) )
ENDIF 
IF ( LWEVAP ) THEN
  CALL NCERROR( NF90_INQ_VARID(ROFCDF%NCID,ROFCDF%CVAR(3),ROFCDF%NVARID(3)) )
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

!================================================
!*** Check if inverse information is available  (only used in ECMWF/IFS v4.07)
IF(PRESENT(LECMF2LAKEC) .AND. (LECMF2LAKEC .NE. 0)) THEN

  ISTATUS = NF90_INQ_VARID(NCID, 'levI', VARID)
  IF ( ISTATUS /= 0 ) THEN
    WRITE(LOGNAM,*) "Could not find levI variable in inpmat.nc: inverse interpolation not available"
    INPNI=-1  ! Not available 
  ELSE
    !* Find levels dimension
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
   CALL mapI2vecI(I2TMP,INPX(:,INPI:INPI))
  READ(TMPNAM,REC=  INPN+INPI) I2TMP
   CALL mapI2vecI(I2TMP,INPY(:,INPI:INPI))
  READ(TMPNAM,REC=2*INPN+INPI) R2TMP
   CALL mapR2vecD( R2TMP,INPA(:,INPI:INPI))
END DO

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
! read runoff from file
IMPLICIT NONE
REAL(KIND=JPRB),INTENT(INOUT)   :: PBUFF(:,:,:)

INTEGER(KIND=JPIM),SAVE         ::  IXIN, IYIN  !! FOR OUTPUT
!$OMP THREADPRIVATE                (IXIN)
!================================================
IF( LINPCDF ) THEN
#ifdef UseCDF_CMF
  CALL CMF_FORCING_GET_CDF(PBUFF(:,:,:))
#endif
ELSE
  CALL CMF_FORCING_GET_BIN(PBUFF(:,:,:))
ENDIF 

!$OMP PARALLEL DO
DO IYIN=1,NYIN
  DO IXIN=1,NXIN
    IF( CMF_CheckNanB(PBUFF(IXIN,IYIN,1),0._JPRB) )THEN !! Check if PRUFINN(IX,IY) is NaN (Not-A-Number) ot not
      PBUFF(IXIN,IYIN,1)=RMIS 
    ENDIF
    PBUFF(IXIN,IYIN,1)=max(PBUFF(IXIN,IYIN,1),0._JPRB)    !! negative Runoff not assumed
  ENDDO
ENDDO
!$OMP END PARALLEL DO

IF ( LROSPLIT ) THEN
!$OMP PARALLEL DO
  DO IYIN=1,NYIN
    DO IXIN=1,NXIN
      IF( CMF_CheckNanB(PBUFF(IXIN,IYIN,2),0._JPRB) )THEN !! Check if PRUFINN(IX,IY) is NaN (Not-A-Number) ot not
        PBUFF(IXIN,IYIN,2)=RMIS 
      ENDIF
      PBUFF(IXIN,IYIN,2)=max(PBUFF(IXIN,IYIN,2),0._JPRB)    !! negative Runoff not assumed
    ENDDO
  ENDDO
!$OMP END PARALLEL DO
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
PBUFF(:,:,2)=0._JPRB  !! Plain Binary subsurface runoff to be added later
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
#ifdef UseCDF_CMF
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
SUBROUTINE CMF_FORCING_COM(PBUFF)
! interporlate with inpmatI (CaMa grid -> input runoff grid), then send calling Model 
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
CALL vecD2mapD(D2FLDFRC,D2MAPTMP)             !! MPI node data is gathered by vecP2mapR
#ifdef UseMPI_CMF
  CALL CMF_MPI_AllReduce_D2MAP(D2MAPTMP)
#endif

CALL INTERPI(D2MAPTMP,PBUFF(:,:,1))        !!  Inverse interpolation (CaMa grid -> input runoff grid)

CONTAINS
!==========================================================
!+ INTERPI
!==========================================================
SUBROUTINE INTERPI(PBUFFIN,PBUFFOUT)
! interporlate field using "input matrix inverse: from catchment to other grid"
USE YOS_CMF_INPUT,           ONLY: NXIN, NYIN, NX,NY
IMPLICIT NONE
REAL(KIND=JPRB),INTENT(IN)      :: PBUFFIN(:,:)     !! CaMa-Flood variable on catchment (NX*NY)
REAL(KIND=JPRB),INTENT(OUT)     :: PBUFFOUT(:,:)    !! output on target grid = input runoff grid (NXIN * NYIN)

INTEGER(KIND=JPIM)              :: IX,IY,INP,IXIN,IYIN
! ========================================================
IF ( INPNI == -1 ) THEN
  WRITE(LOGNAM,*) "INPNI==-1, no inverse interpolation possible"
  STOP 9
ENDIF
PBUFFOUT(:,:)=1._JPRB 

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
! interporlate with inpmat, then send runoff data to CaMa-Flood 
! -- called from "Main Program / Coupler" or CMF_DRV_ADVANCE
USE YOS_CMF_INPUT,           ONLY: LROSPLIT,LWEVAP
USE YOS_CMF_PROG,            ONLY: D2RUNOFF,D2ROFSUB,D2WEVAP
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
USE YOS_CMF_MAP,             ONLY: NSEQALL
USE YOS_CMF_INPUT,           ONLY: NXIN, NYIN, INPN, RMIS
IMPLICIT NONE
REAL(KIND=JPRB),INTENT(IN)      :: PBUFFIN(:,:)     !! default [mm/dt] 
REAL(KIND=JPRB),INTENT(OUT)     :: PBUFFOUT(:,:)    !! m3/s
! SAVE for OMP
INTEGER(KIND=JPIM),SAVE  ::  ISEQ
INTEGER(KIND=JPIM),SAVE  ::  IXIN, IYIN, INPI  !! FOR OUTPUT
!$OMP THREADPRIVATE    (IXIN, IYIN, INPI)
!============================
!$OMP PARALLEL DO
DO ISEQ=1, NSEQALL
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
USE YOS_CMF_MAP,             ONLY: NSEQALL, NSEQMAX, D2GRAREA
USE YOS_CMF_INPUT,           ONLY: RMIS
USE CMF_UTILS_MOD,           ONLY: mapD2vecD
IMPLICIT NONE

REAL(KIND=JPRB),INTENT(IN)      :: PBUFFIN(:,:)     !! default [mm/dt] 
REAL(KIND=JPRB),INTENT(OUT)     :: PBUFFOUT(:,:)    !! m3/s

REAL(KIND=JPRB),ALLOCATABLE     :: D2TEMP(:,:)

INTEGER(KIND=JPIM),SAVE         ::  ISEQ
! ================================================
ALLOCATE(D2TEMP(NSEQMAX,1))
CALL mapD2vecD(PBUFFIN,D2TEMP)
!$OMP PARALLEL DO
DO ISEQ=1, NSEQALL
  IF( D2TEMP(ISEQ,1).NE.RMIS )THEN
    PBUFFOUT(ISEQ,1) = D2TEMP(ISEQ,1) * D2GRAREA(ISEQ,1) / DROFUNIT
    PBUFFOUT(ISEQ,1) = MAX(PBUFFOUT(ISEQ,1), 0._JPRB)
  ELSE
    PBUFFOUT(ISEQ,1)=0._JPRB
  ENDIF
END DO
!$OMP END PARALLEL DO
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
!================================================
WRITE(LOGNAM,*) ""
WRITE(LOGNAM,*) "!---------------------!"
WRITE(LOGNAM,*) "CMF::FORCING_END: Finalize forcing module"

!* Close Input netcdf 
IF( LINPCDF ) THEN
#ifdef UseCDF_CMF
  CALL NCERROR( NF90_CLOSE(ROFCDF%NCID))
  WRITE(LOGNAM,*) "input netCDF runoff closed:",ROFCDF%NCID
#endif
ENDIF 

WRITE(LOGNAM,*) "CMF::FORCING_END: end"

END SUBROUTINE CMF_FORCING_END
!####################################################################

END MODULE CMF_CTRL_FORCING_MOD
