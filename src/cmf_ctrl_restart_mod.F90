MODULE CMF_CTRL_RESTART_MOD     !!!tentative version 7/21
!==========================================================
!* PURPOSE: Control CaMa-Flood restart
!
!* CONTAINS:
! -- CMF_RESTART_NMLIST : set restart configulation info from namelist
! -- CMF_RESTART_INIT   : Read  restart file
! -- CMF_RESTART_WRITE  : Write restart file
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
USE PARKIND1,                ONLY: JPIM, JPRB, JPRM, JPRD
USE YOS_CMF_INPUT,           ONLY: LOGNAM,  LSTOONLY, LDAMOUT, LLEVEE, LPTHOUT, LGDWDLY
USE YOS_CMF_INPUT,           ONLY: CSUFBIN, CSUFPTH,  CSUFCDF
IMPLICIT NONE
!============================
SAVE
!*** NAMELIST/NOUTPUT/ from inputnam
CHARACTER(LEN=256)              :: CRESTSTO               ! input restart file name
!
CHARACTER(LEN=256)              :: CRESTDIR               ! output restart file directory
CHARACTER(LEN=256)              :: CVNREST                ! output restart prefix
LOGICAL                         :: LRESTCDF               ! true: netCDF restart file
LOGICAL                         :: LRESTDBL               ! true: binary restart in double precision
INTEGER(KIND=JPIM)              :: IFRQ_RST               ! 0: only at last time, (1,2,3,...,24) hourly restart, 30: monthly restart
!
NAMELIST/NRESTART/ CRESTSTO,CRESTDIR,CVNREST,LRESTCDF,LRESTDBL,IFRQ_RST
!
CONTAINS 
!####################################################################
!! changed 12/12
! -- CMF_RESTART_NMLIST : set restart configulation info from namelist
! -- CMF_RESTART_INIT   : Read  restart file
! -- CMF_RESTART_WRITE  : Write restart file
!####################################################################
SUBROUTINE CMF_RESTART_NMLIST
! reed setting from namelist
! -- Called from CMF_DRV_NMLIST
USE YOS_CMF_INPUT,      ONLY: CSETFILE,NSETFILE
USE CMF_UTILS_MOD,      ONLY: INQUIRE_FID
IMPLICIT NONE
!================================================
WRITE(LOGNAM,*) ""
WRITE(LOGNAM,*) "!---------------------!"

!*** 1. open namelist
NSETFILE=INQUIRE_FID()
OPEN(NSETFILE,FILE=CSETFILE,STATUS="OLD")
WRITE(LOGNAM,*) "CMF::RESTART_NMLIST: namelist OPEN in unit: ", TRIM(CSETFILE), NSETFILE 

!*** 2. default value
CRESTSTO="restart"   ! input restart file name
!
CRESTDIR="./"        ! output restart file directory
CVNREST ="restart"   ! output restart file prefix
LRESTCDF=.FALSE.     ! true: netCDF restart file
LRESTDBL=.TRUE.      ! true: binary restart double precision
IFRQ_RST=0           ! 0: only end of simulation, [1,2,3,6,12,24] at selected hour, 30: monthly

!*** 3. read namelist
REWIND(NSETFILE)
READ(NSETFILE,NML=NRESTART)

WRITE(LOGNAM,*) "=== NAMELIST, NRESTART ==="
WRITE(LOGNAM,*) "CRESTSTO:  ", TRIM(CRESTSTO)
WRITE(LOGNAM,*) "CRESTDIR:  ", TRIM(CRESTDIR)
WRITE(LOGNAM,*) "CVNREST:   ", TRIM(CVNREST)
WRITE(LOGNAM,*) "LRESTCDF:  ", LRESTCDF
WRITE(LOGNAM,*) "LRESTDBL:  ", LRESTDBL
WRITE(LOGNAM,*) "IFRQ_RST:  ", IFRQ_RST

CLOSE(NSETFILE)

END SUBROUTINE CMF_RESTART_NMLIST
!####################################################################





!####################################################################
SUBROUTINE CMF_RESTART_INIT
! read restart file
! -- call from CMF_DRV_INIT
USE YOS_CMF_PROG,   ONLY: P2RIVSTO,    P2FLDSTO,    D2RIVOUT,    D2FLDOUT,    P2GDWSTO, &
                        & D2RIVOUT_PRE,D2FLDOUT_PRE,D2RIVDPH_PRE,D2FLDSTO_PRE,&
                        & D1PTHFLW,    D1PTHFLW_PRE, &
                        & P2DAMSTO,    P2LEVSTO      !!! added
IMPLICIT NONE
! ===========
P2RIVSTO(:,:)=0._JPRD
P2FLDSTO(:,:)=0._JPRD
D2RIVOUT(:,:)=0._JPRB
D2FLDOUT(:,:)=0._JPRB

D2RIVOUT_PRE(:,:)=0._JPRB
D2FLDOUT_PRE(:,:)=0._JPRB
D2RIVDPH_PRE(:,:)=0._JPRB
D2FLDSTO_PRE(:,:)=0._JPRB

IF( LPTHOUT )THEN
  D1PTHFLW(:,:)=0._JPRB
  D1PTHFLW_PRE(:,:)=0._JPRB
ENDIF
IF( LDAMOUT ) then
  P2DAMSTO(:,:)=0._JPRD   !!! added LDAMOUT
ENDIF
IF( LLEVEE ) then
  P2LEVSTO(:,:)=0._JPRD   !!! added LLEVEE
ENDIF
IF( LGDWDLY ) then
  P2GDWSTO(:,:)=0._JPRD
ENDIF

IF ( LRESTCDF ) THEN
  CALL READ_REST_CDF
ELSE
  CALL READ_REST_BIN
ENDIF

IF( LSTOONLY )THEN          !!  storage only restart
  D2FLDSTO_PRE(:,:)=P2FLDSTO(:,:)
ENDIF

CONTAINS
!==========================================================
!+ READ_REST_BIN
!+ READ_REST_CDF
!+
!==========================================================
SUBROUTINE READ_REST_BIN
USE YOS_CMF_INPUT,           ONLY: TMPNAM,  NX,NY
USE YOS_CMF_MAP,             ONLY: NSEQMAX,NPTHOUT, NPTHLEV
USE CMF_UTILS_MOD,           ONLY: INQUIRE_FID, mapR2vecD
IMPLICIT NONE
!*** LOCAL
INTEGER(KIND=JPIM)              :: RIREC
REAL(KIND=JPRD)                 :: P2VEC(NSEQMAX,1)
REAL(KIND=JPRM)                 :: R1PTH(NPTHOUT,NPTHLEV)
REAL(KIND=JPRD)                 :: P1PTH(NPTHOUT,NPTHLEV)
CHARACTER(LEN=256)              :: CFILE
!================================================
CFILE=TRIM(CRESTSTO)
WRITE(LOGNAM,*)'READ_REST: read restart binary: ', TRIM(CFILE)

TMPNAM=INQUIRE_FID()

IF( LRESTDBL )THEN
  OPEN(TMPNAM,FILE=CFILE,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=8*NX*NY)
ELSE
  OPEN(TMPNAM,FILE=CFILE,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4*NX*NY)
ENDIF

RIREC=0
CALL READ_BIN_MAP(P2VEC,TMPNAM,RIREC)
 P2RIVSTO=P2VEC
CALL READ_BIN_MAP(P2VEC,TMPNAM,RIREC)
 P2FLDSTO=P2VEC

!! additional restart data for optional schemes
IF ( .not. LSTOONLY )THEN           !! default restart with previous t-step outflw
  CALL READ_BIN_MAP(P2VEC,TMPNAM,RIREC)
   D2RIVOUT_PRE=P2VEC
  CALL READ_BIN_MAP(P2VEC,TMPNAM,RIREC)
   D2FLDOUT_PRE=P2VEC
  CALL READ_BIN_MAP(P2VEC,TMPNAM,RIREC)
   D2RIVDPH_PRE=P2VEC
  CALL READ_BIN_MAP(P2VEC,TMPNAM,RIREC)
   D2FLDSTO_PRE=P2VEC
ENDIF
IF ( LGDWDLY ) THEN
  CALL READ_BIN_MAP(P2VEC,TMPNAM,RIREC)
   P2GDWSTO=P2VEC
ENDIF
IF ( LDAMOUT ) THEN      !!! added LDAMOUT
  CALL READ_BIN_MAP(P2VEC,TMPNAM,RIREC)
   P2DAMSTO=P2VEC
ENDIF
IF ( LLEVEE ) THEN      !!! added LLEVEE
  CALL READ_BIN_MAP(P2VEC,TMPNAM,RIREC)
   P2LEVSTO=P2VEC
ENDIF
CLOSE(TMPNAM)

IF( LPTHOUT )THEN
  IF( .not. LSTOONLY )THEN
    CFILE=TRIM(CRESTSTO)//'.pth'
    WRITE(LOGNAM,*)'READ_REST: read restart binary: ', TRIM(CFILE)

    IF( LRESTDBL )THEN
      OPEN(TMPNAM,FILE=CFILE,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=8*NPTHOUT*NPTHLEV)
      READ(TMPNAM,REC=1) P1PTH
      CLOSE(TMPNAM)
      D1PTHFLW_PRE(:,:)=P1PTH(:,:)
    ELSE
      OPEN(TMPNAM,FILE=CFILE,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4*NPTHOUT*NPTHLEV)
      READ(TMPNAM,REC=1) R1PTH
      CLOSE(TMPNAM)
      D1PTHFLW_PRE(:,:)=R1PTH(:,:)
    ENDIF
  ELSE
    D1PTHFLW_PRE(:,:)=0._JPRB
  ENDIF
ENDIF

END SUBROUTINE READ_REST_BIN

SUBROUTINE READ_BIN_MAP(P2VAR,TNAM,IREC)
USE CMF_UTILS_MOD,      ONLY: mapP2vecP
USE YOS_CMF_INPUT,      ONLY: NX, NY
USE YOS_CMF_MAP,        ONLY: NSEQMAX
IMPLICIT NONE
REAL(KIND=JPRD)            :: P2VAR(NSEQMAX,1)
INTEGER(KIND=JPIM)         :: TNAM, IREC
!* local
REAL(KIND=JPRM)            :: R2TEMP(NX,NY)
REAL(KIND=JPRD)            :: P2TEMP(NX,NY)
!=================
IREC=IREC+1

!=== Double Precision Restart ===
IF( LRESTDBL )THEN
  READ(TNAM,REC=IREC) P2TEMP
!=== Single Precision Restart (convert to double precision once) ===
ELSE
  READ(TNAM,REC=IREC) R2TEMP
  P2TEMP=R2TEMP
ENDIF
CALL mapP2vecP(P2TEMP,P2VAR)

!=================
END SUBROUTINE READ_BIN_MAP
!======
!+
!+
!+
!==========================================================
SUBROUTINE READ_REST_CDF
#ifdef UseCDF_CMF
USE NETCDF
USE YOS_CMF_INPUT,    ONLY: NX, NY
USE YOS_CMF_MAP,      ONLY: NPTHOUT, NPTHLEV, PTH_UPST, PTH_DOWN
USE CMF_UTILS_MOD,    ONLY: NCERROR, mapP2vecP, mapP2vecD
IMPLICIT NONE
! local variables
INTEGER(KIND=JPIM)    ::  NCID,VARID
INTEGER(KIND=JPIM)    ::  IPTH
CHARACTER(LEN=256)    ::  CFILE
REAL(KIND=JPRD)       ::  P2TEMP(NX,NY), P1PTH(NPTHOUT,NPTHLEV)  !! NetCDF restart is in Double Precision
!================================================
CFILE=TRIM(CRESTSTO)
WRITE(LOGNAM,*)'READ_REST: read restart netcdf: ', TRIM(CFILE)

CALL NCERROR( NF90_OPEN(CFILE,NF90_NOWRITE,NCID), 'OPENING '//CFILE)

CALL NCERROR( NF90_INQ_VARID(NCID,'rivsto',VARID))
CALL NCERROR( NF90_GET_VAR(NCID,VARID,P2TEMP,(/1,1,1/),(/NX,NY,1/) ) )
CALL mapP2vecP(P2TEMP,P2RIVSTO)

CALL NCERROR( NF90_INQ_VARID(NCID,'fldsto',VARID))
CALL NCERROR( NF90_GET_VAR(NCID,VARID,P2TEMP,(/1,1,1/),(/NX,NY,1/) ) )
CALL mapP2vecP(P2TEMP,P2FLDSTO)


IF( .NOT. LSTOONLY )THEN
  CALL NCERROR( NF90_INQ_VARID(NCID,'rivout_pre',VARID))
  CALL NCERROR( NF90_GET_VAR(NCID,VARID,P2TEMP,(/1,1,1/),(/NX,NY,1/) ) )
  CALL mapP2vecD(P2TEMP,D2RIVOUT_PRE)
  D2RIVOUT=D2RIVOUT_PRE

  CALL NCERROR( NF90_INQ_VARID(NCID,'fldout_pre',VARID))
  CALL NCERROR( NF90_GET_VAR(NCID,VARID,P2TEMP,(/1,1,1/),(/NX,NY,1/) ) )
  CALL mapP2vecD(P2TEMP,D2FLDOUT_PRE)
  D2FLDOUT=D2FLDOUT_PRE

  CALL NCERROR( NF90_INQ_VARID(NCID,'rivdph_pre',VARID))
  CALL NCERROR( NF90_GET_VAR(NCID,VARID,P2TEMP,(/1,1,1/),(/NX,NY,1/) ) )
  CALL mapP2vecD(P2TEMP,D2RIVDPH_PRE)

  CALL NCERROR( NF90_INQ_VARID(NCID,'fldsto_pre',VARID))
  CALL NCERROR( NF90_GET_VAR(NCID,VARID,P2TEMP,(/1,1,1/),(/NX,NY,1/) ) )
  CALL mapP2vecD(P2TEMP,D2FLDSTO_PRE)
ENDIF

IF ( LGDWDLY ) THEN
  CALL NCERROR( NF90_INQ_VARID(NCID,'gdwsto',VARID))
  CALL NCERROR( NF90_GET_VAR(NCID,VARID,P2TEMP,(/1,1,1/),(/NX,NY,1/) ) )
  CALL mapP2vecP(P2TEMP,P2GDWSTO)
ENDIF

IF ( LDAMOUT ) THEN    !!! added
  CALL NCERROR( NF90_INQ_VARID(NCID,'damsto',VARID))
  CALL NCERROR( NF90_GET_VAR(NCID,VARID,P2TEMP,(/1,1,1/),(/NX,NY,1/) ) )
  CALL mapP2vecP(P2TEMP,P2DAMSTO)
ENDIF

IF ( LLEVEE ) THEN    !!! added
  CALL NCERROR( NF90_INQ_VARID(NCID,'levsto',VARID))
  CALL NCERROR( NF90_GET_VAR(NCID,VARID,P2TEMP,(/1,1,1/),(/NX,NY,1/) ) )
  CALL mapP2vecP(P2TEMP,P2LEVSTO)
ENDIF

IF ( LPTHOUT .AND. .NOT. LSTOONLY ) THEN
  CALL NCERROR( NF90_INQ_VARID(NCID,'pthflw_pre',VARID))
  CALL NCERROR( NF90_GET_VAR(NCID,VARID,P1PTH,(/1,1,1/),(/NPTHOUT,NPTHLEV,1/) ) )
  DO IPTH=1,NPTHOUT
    IF (PTH_UPST(IPTH)>0 .AND. PTH_DOWN(IPTH)>0 ) THEN
      D1PTHFLW_PRE(IPTH,:)=P1PTH(IPTH,:)
    ELSE
      D1PTHFLW_PRE(IPTH,:)=0._JPRB
    ENDIF
  END DO
ENDIF

CALL NCERROR( NF90_CLOSE(NCID) )

#endif
END SUBROUTINE READ_REST_CDF
!==========================================================

END SUBROUTINE CMF_RESTART_INIT
!####################################################################





!####################################################################
SUBROUTINE CMF_RESTART_WRITE
! write restart files
! -- called CMF_from DRV_ADVANCE
USE YOS_CMF_INPUT,      ONLY: TMPNAM, NX, NY
USE YOS_CMF_TIME,       ONLY: KSTEP,  NSTEPS, JYYYYMMDD, JHHMM, JDD, JHOUR, JMIN
USE YOS_CMF_MAP,        ONLY: NPTHOUT,     NPTHLEV
USE YOS_CMF_PROG,       ONLY: P2RIVSTO,    P2FLDSTO,    D2RIVOUT_PRE,D2FLDOUT_PRE, &
                            & D1PTHFLW_PRE,D2RIVDPH_PRE,D2FLDSTO_PRE,P2GDWSTO, &
                            & P2DAMSTO,    P2LEVSTO
USE CMF_UTILS_MOD,      ONLY: INQUIRE_FID
IMPLICIT NONE
!* local variable
INTEGER(KIND=JPIM)         :: IREST
!================================================
IREST=0

IF ( IFRQ_RST>=0 .and. KSTEP==NSTEPS )THEN         !! end of run
  IREST=1
ENDIF

IF ( IFRQ_RST>=1 .and. IFRQ_RST<=24 )THEN
  IF ( MOD(JHOUR,IFRQ_RST)==0 .and. JMIN==0 )THEN  !! at selected hour
    IREST=1
  ENDIF
ENDIF

IF ( IFRQ_RST==30 )THEN
  IF ( JDD==1 .and. JHOUR==0 .and. JMIN==0 )THEN  !! at start of month
    IREST=1
  ENDIF
ENDIF


IF( IREST==1 )THEN
  WRITE(LOGNAM,*) ""
  WRITE(LOGNAM,*) "!---------------------!"
  WRITE(LOGNAM,*) 'CMF::RESTART_WRITE: write time: ' , JYYYYMMDD, JHHMM


  IF( LRESTCDF )THEN
    CALL WRTE_REST_CDF  !! netCDF restart write
  ELSE
    CALL WRTE_REST_BIN
  ENDIF
END IF 

CONTAINS
!==========================================================
!+ WRTE_REST_BIN
!+ WRTE_REST_CDF
!==========================================================
SUBROUTINE WRTE_REST_BIN
USE YOS_CMF_TIME,       ONLY: JYYYYMMDD, JHOUR
USE YOS_CMF_MAP,        ONLY: REGIONTHIS, NSEQMAX
#ifdef UseMPI_CMF
USE CMF_CTRL_MPI_MOD,   ONLY: CMF_MPI_AllReduce_R1PTH, CMF_MPI_AllReduce_P1PTH
#endif
IMPLICIT NONE
! local variable 
INTEGER(KIND=JPIM)         :: RIREC
CHARACTER(LEN=256)         :: CFILE,CDATE
REAL(KIND=JPRD)            :: P2TMP(NSEQMAX,1)        !! use Real*8 for code simplicity
REAL(KIND=JPRD)            :: P1PTH(NPTHOUT,NPTHLEV) 
REAL(KIND=JPRM)            :: R1PTH(NPTHOUT,NPTHLEV) 
!================================================
!*** set file nam
WRITE(CDATE,'(I8.8,I2.2)') JYYYYMMDD,JHOUR
CFILE=TRIM(CRESTDIR)//TRIM(CVNREST)//TRIM(CDATE)//TRIM(CSUFBIN)
WRITE(LOGNAM,*) 'WRTE_REST_BIN: restart file:',CFILE

!*** write restart data (2D map)
TMPNAM=INQUIRE_FID()

IF( LRESTDBL )THEN
  IF ( REGIONTHIS==1 ) OPEN(TMPNAM,FILE=CFILE,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=8*NX*NY)
ELSE
  IF ( REGIONTHIS==1 ) OPEN(TMPNAM,FILE=CFILE,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4*NX*NY)
ENDIF

RIREC=0
  P2TMP=P2RIVSTO
   CALL WRTE_BIN_MAP(P2TMP,TMPNAM,RIREC)
  P2TMP=P2FLDSTO
   CALL WRTE_BIN_MAP(P2TMP,TMPNAM,RIREC)
!!================
!! additional restart data for optional schemes (only write required vars)
  IF ( .not. LSTOONLY )THEN           !! default restart with previous t-step outflw
    P2TMP=D2RIVOUT_PRE
     CALL WRTE_BIN_MAP(P2TMP,TMPNAM,RIREC)
    P2TMP=D2FLDOUT_PRE
     CALL WRTE_BIN_MAP(P2TMP,TMPNAM,RIREC)
    P2TMP=D2RIVDPH_PRE
     CALL WRTE_BIN_MAP(P2TMP,TMPNAM,RIREC)
    P2TMP=D2FLDSTO_PRE
     CALL WRTE_BIN_MAP(P2TMP,TMPNAM,RIREC)
  ENDIF

  IF ( LGDWDLY ) THEN
    P2TMP=P2GDWSTO
     CALL WRTE_BIN_MAP(P2TMP,TMPNAM,RIREC)
  ENDIF
  IF ( LDAMOUT ) THEN   !!! ADDED
    P2TMP=P2DAMSTO
     CALL WRTE_BIN_MAP(P2TMP,TMPNAM,RIREC)
  ENDIF
  IF ( LLEVEE ) THEN   !!! ADDED
    P2TMP=P2LEVSTO
     CALL WRTE_BIN_MAP(P2TMP,TMPNAM,RIREC)
  ENDIF

CLOSE(TMPNAM)

!*** write restart data (1D bifucation chanenl)
IF( LPTHOUT )THEN

  CFILE=TRIM(CRESTDIR)//TRIM(CVNREST)//TRIM(CDATE)//TRIM(CSUFBIN)//'.pth'
  WRITE(LOGNAM,*) 'WRTE_REST: WRITE RESTART BIN:',CFILE

  !! Double Precision Restart
  IF( LRESTDBL )THEN
    P1PTH(:,:)=D1PTHFLW_PRE(:,:)
#ifdef UseMPI_CMF
    CALL CMF_MPI_AllReduce_P1PTH(P1PTH)
#endif
    IF ( REGIONTHIS==1 )THEN
      OPEN(TMPNAM,FILE=CFILE,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=8*NPTHOUT*NPTHLEV)
      WRITE(TMPNAM,REC=1) P1PTH
      CLOSE(TMPNAM)
    ENDIF

  !! Single Precision Restart
  ELSE
    R1PTH(:,:)=D1PTHFLW_PRE(:,:)
#ifdef UseMPI_CMF
    CALL CMF_MPI_AllReduce_R1PTH(R1PTH)
#endif
    IF ( REGIONTHIS==1 )THEN
      OPEN(TMPNAM,FILE=CFILE,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4*NPTHOUT*NPTHLEV)
      WRITE(TMPNAM,REC=1) R1PTH
      CLOSE(TMPNAM)
    ENDIF
  ENDIF
ENDIF

END SUBROUTINE WRTE_REST_BIN
!=================
SUBROUTINE WRTE_BIN_MAP(P2VAR,TNAM,IREC)
USE CMF_UTILS_MOD,      ONLY: vecP2mapP,  vecP2mapR
USE YOS_CMF_MAP,        ONLY: REGIONTHIS, NSEQMAX
#ifdef UseMPI_CMF
USE CMF_CTRL_MPI_MOD,   ONLY: CMF_MPI_AllReduce_R2MAP, CMF_MPI_AllReduce_P2MAP
#endif
IMPLICIT NONE
REAL(KIND=JPRD)            :: P2VAR(NSEQMAX,1)  !! use Real*8 for code simplicity
INTEGER(KIND=JPIM)         :: TNAM,IREC
!* local
REAL(KIND=JPRM)            :: R2TEMP(NX,NY)
REAL(KIND=JPRD)            :: P2TEMP(NX,NY)
!=================
IREC=IREC+1

!! Double Precision Restart
IF( LRESTDBL )THEN
  CALL vecP2mapP(P2VAR,P2TEMP)  
#ifdef UseMPI_CMF
  CALL CMF_MPI_AllReduce_P2MAP(P2TEMP)
#endif

  IF ( REGIONTHIS==1 ) WRITE(TNAM,REC=IREC) P2TEMP

!! Single Precision Restart
ELSE
  CALL vecP2mapR(P2VAR,R2TEMP)  
#ifdef UseMPI_CMF
    CALL CMF_MPI_AllReduce_R2MAP(R2TEMP)
#endif
  IF ( REGIONTHIS==1 ) WRITE(TNAM,REC=IREC) R2TEMP
ENDIF
!=================
END SUBROUTINE WRTE_BIN_MAP
!==========================================================
!+
!+
!+
!==========================================================
SUBROUTINE WRTE_REST_CDF
#ifdef UseCDF_CMF
USE NETCDF
USE YOS_CMF_INPUT,      ONLY: DMIS
USE YOS_CMF_TIME,       ONLY: KMINNEXT, KMINSTART, ISYYYY,ISMM,ISDD, ISHOUR, ISMIN
USE YOS_CMF_TIME,       ONLY: JYYYYMMDD,JHOUR
USE YOS_CMF_MAP,        ONLY: D1LON,    D1LAT,     REGIONTHIS, NSEQMAX
USE CMF_UTILS_MOD,      ONLY: NCERROR,  vecP2mapP
#ifdef UseMPI_CMF
USE CMF_CTRL_MPI_MOD,   ONLY: CMF_MPI_AllReduce_P2MAP, CMF_MPI_AllReduce_P1PTH
#endif
IMPLICIT NONE
!* local variable
CHARACTER(LEN=256)         :: CFILE, CDATE, CTIME, CVAR
INTEGER(KIND=JPIM)         :: NCID,  VARID, LATID, LONID, TIMEID, JF, &
                              NPTHOUTID,    NPTHLEVID,    STATUS, IOUT
REAL(KIND=JPRB)            :: XTIME ! seconds since start of the run ! 
REAL(KIND=JPRD)            :: P2VEC(NSEQMAX,1), P2TEMP(NX,NY), P1PTH(NPTHOUT,NPTHLEV)
!================================================
!*** 1. set file name & tim
XTIME=REAL( (KMINNEXT-KMINSTART),JPRB) *60._JPRB
WRITE(CTIME,'(A14,I4.4,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2)') 'seconds since ',ISYYYY,'-',ISMM,'-',ISDD,' ',ISHOUR,":",ISMIN

WRITE(CDATE,'(I8.8,I2.2)') JYYYYMMDD,JHOUR
CFILE=TRIM(CRESTDIR)//TRIM(CVNREST)//TRIM(CDATE)//TRIM(CSUFCDF)
WRITE(LOGNAM,*) 'WRTE_REST:create RESTART NETCDF:',CFILE

!============================
!*** 2. create netCDF file
!! Note: all restart variables are saved as Float64.
IF( REGIONTHIS==1 )THEN   !! write restart only on master node

  CALL NCERROR( NF90_CREATE(CFILE,NF90_NETCDF4,NCID),'CREATING FILE:'//TRIM(CFILE) )
  
  !! dimensions 
  CALL NCERROR( NF90_DEF_DIM(NCID, 'time', NF90_UNLIMITED, TIMEID) )
  CALL NCERROR( NF90_DEF_DIM(NCID, 'lat', NY, LATID) )
  CALL NCERROR( NF90_DEF_DIM(NCID, 'lon', NX, LONID) )
  
  IF ( LPTHOUT ) THEN
    CALL NCERROR( NF90_DEF_DIM(NCID, 'NPTHOUT', NPTHOUT, NPTHOUTID) )
    CALL NCERROR( NF90_DEF_DIM(NCID, 'NPTHLEV', NPTHLEV, NPTHLEVID) )
  ENDIF 
  
  !! dimentions 
  CALL NCERROR( NF90_DEF_VAR(NCID, 'lat', NF90_FLOAT, (/LATID/), VARID) )
  CALL NCERROR( NF90_PUT_ATT(NCID, VARID, 'long_name','latitude') )
  CALL NCERROR( NF90_PUT_ATT(NCID, VARID, 'units','degrees_north') )
  
  CALL NCERROR( NF90_DEF_VAR(NCID, 'lon', NF90_FLOAT, (/LONID/), VARID) )
  CALL NCERROR( NF90_PUT_ATT(NCID, VARID, 'long_name','longitude') )
  CALL NCERROR( NF90_PUT_ATT(NCID, VARID, 'units','degrees_east') )
  
  CALL NCERROR( NF90_DEF_VAR(NCID, 'time', NF90_DOUBLE, (/TIMEID/), VARID) ) 
  CALL NCERROR( NF90_PUT_ATT(NCID, VARID, 'long_name','time') )
  CALL NCERROR( NF90_PUT_ATT(NCID, VARID, 'units',CTIME) )
  
  !! variables
  CALL NCERROR( NF90_DEF_VAR(NCID, 'rivsto', NF90_DOUBLE, (/LONID,LATID,TIMEID/), &
                             VARID,DEFLATE_LEVEL=6), 'Creating Variable')
  CALL NCERROR( NF90_PUT_ATT(NCID, VARID, 'long_name',"river storage" ) )
  CALL NCERROR( NF90_PUT_ATT(NCID, VARID, 'units',"m3") )
  CALL NCERROR( NF90_PUT_ATT(NCID, VARID, '_FillValue',REAL(DMIS,KIND=JPRD)),'in here?' )
  
   
  CALL NCERROR( NF90_DEF_VAR(NCID, 'fldsto', NF90_DOUBLE, (/LONID,LATID,TIMEID/), &
                             VARID,DEFLATE_LEVEL=6), 'Creating Variable')  
  CALL NCERROR( NF90_PUT_ATT(NCID, VARID, 'long_name',"flood plain storage" ) )
  CALL NCERROR( NF90_PUT_ATT(NCID, VARID, 'units',"m3") )
  CALL NCERROR( NF90_PUT_ATT(NCID, VARID, '_FillValue',REAL(DMIS,KIND=JPRD)) )
  
  IF ( .not. LSTOONLY )THEN           !! default restart with previous t-step outflw
    CALL NCERROR( NF90_DEF_VAR(NCID, 'rivout_pre', NF90_DOUBLE, (/LONID,LATID,TIMEID/),&
                               VARID,DEFLATE_LEVEL=6), 'Creating Variable')  
    CALL NCERROR( NF90_PUT_ATT(NCID, VARID, 'long_name',"river outflow prev" ) )
    CALL NCERROR( NF90_PUT_ATT(NCID, VARID, 'units',"m3/s") )
    CALL NCERROR( NF90_PUT_ATT(NCID, VARID, '_FillValue',REAL(DMIS,KIND=JPRD)) )
    
    CALL NCERROR( NF90_DEF_VAR(NCID, 'fldout_pre', NF90_DOUBLE, (/LONID,LATID,TIMEID/), &
                               VARID,DEFLATE_LEVEL=6), 'Creating Variable')  
    CALL NCERROR( NF90_PUT_ATT(NCID, VARID, 'long_name',"floodplain outflow prev" ) )
    CALL NCERROR( NF90_PUT_ATT(NCID, VARID, 'units',"m3/s") )
    CALL NCERROR( NF90_PUT_ATT(NCID, VARID, '_FillValue',REAL(DMIS,KIND=JPRD)) )
    
    CALL NCERROR( NF90_DEF_VAR(NCID, 'rivdph_pre', NF90_DOUBLE, (/LONID,LATID,TIMEID/), &
                               VARID,DEFLATE_LEVEL=6), 'Creating Variable')  
    CALL NCERROR( NF90_PUT_ATT(NCID, VARID, 'long_name',"river depth prev" ) )
    CALL NCERROR( NF90_PUT_ATT(NCID, VARID, 'units',"m") )
    CALL NCERROR( NF90_PUT_ATT(NCID, VARID, '_FillValue',REAL(DMIS,KIND=JPRD)) )
    
    CALL NCERROR( NF90_DEF_VAR(NCID, 'fldsto_pre', NF90_DOUBLE, (/LONID,LATID,TIMEID/), &
                               VARID,DEFLATE_LEVEL=6), 'Creating Variable')  
    CALL NCERROR( NF90_PUT_ATT(NCID, VARID, 'long_name',"floodplain storage prev" ) )
    CALL NCERROR( NF90_PUT_ATT(NCID, VARID, 'units',"m3") )
    CALL NCERROR( NF90_PUT_ATT(NCID, VARID, '_FillValue',REAL(DMIS,KIND=JPRD)) )
  
    !! optional variables
    IF ( LPTHOUT ) THEN
      CALL NCERROR( NF90_DEF_VAR(NCID, 'pthflw_pre', NF90_DOUBLE, (/NPTHOUTID,NPTHLEVID,TIMEID/),&
                                 VARID,DEFLATE_LEVEL=6) ) 
      CALL NCERROR( NF90_PUT_ATT(NCID, VARID, 'long_name',"bifurcation outflow pre" ) )
      CALL NCERROR( NF90_PUT_ATT(NCID, VARID, 'units',"m3/s") )
    ENDIF
  ENDIF
  
  IF ( LGDWDLY ) THEN
    CALL NCERROR( NF90_DEF_VAR(NCID, 'gdwsto', NF90_DOUBLE, (/LONID,LATID,TIMEID/), &
                             VARID,DEFLATE_LEVEL=6), 'Creating Variable gdwsto')  
    CALL NCERROR( NF90_PUT_ATT(NCID, VARID, 'long_name',"ground water storage" ) )
    CALL NCERROR( NF90_PUT_ATT(NCID, VARID, 'units',"m3") )
    CALL NCERROR( NF90_PUT_ATT(NCID, VARID, '_FillValue',REAL(DMIS,KIND=JPRD)) )
  ENDIF
  
  IF ( LDAMOUT ) THEN    !!! added
    CALL NCERROR( NF90_DEF_VAR(NCID, 'damsto', NF90_DOUBLE, (/LONID,LATID,TIMEID/), &
                             VARID,DEFLATE_LEVEL=6), 'Creating Variable dasmto')  
    CALL NCERROR( NF90_PUT_ATT(NCID, VARID, 'long_name',"dam reservoir storage" ) )
    CALL NCERROR( NF90_PUT_ATT(NCID, VARID, 'units',"m3") )
    CALL NCERROR( NF90_PUT_ATT(NCID, VARID, '_FillValue',REAL(DMIS,KIND=JPRD)) )
  ENDIF
  
  IF ( LLEVEE ) THEN    !!! added
    CALL NCERROR( NF90_DEF_VAR(NCID, 'levsto', NF90_DOUBLE, (/LONID,LATID,TIMEID/), &
                             VARID,DEFLATE_LEVEL=6), 'Creating Variable levsto')  
    CALL NCERROR( NF90_PUT_ATT(NCID, VARID, 'long_name',"storage exceeds levee protection" ) )
    CALL NCERROR( NF90_PUT_ATT(NCID, VARID, 'units',"m3") )
    CALL NCERROR( NF90_PUT_ATT(NCID, VARID, '_FillValue',REAL(DMIS,KIND=JPRD)) )
  ENDIF

  CALL NCERROR( NF90_ENDDEF(NCID) )
  !============================
  !*** 2. write data
  
  !! dimentions (time,lon,lat)
  CALL NCERROR( NF90_INQ_VARID(NCID,'time',VARID))
  CALL NCERROR( NF90_PUT_VAR(NCID,VARID,XTIME) )
  
  CALL NCERROR ( NF90_INQ_VARID(NCID,'lon',VARID),'getting id' )
  CALL NCERROR( NF90_PUT_VAR(NCID,VARID,D1LON))
  
  CALL NCERROR ( NF90_INQ_VARID(NCID,'lat',VARID),'getting id' )
  CALL NCERROR( NF90_PUT_VAR(NCID,VARID,D1LAT))

ENDIF  !! regionthis=1: definition

!! write restart variables (gather data in MPI mode)
DO JF=1,9
  IOUT=0
  SELECT CASE(JF)
    CASE (1)
      CVAR='rivsto'
      CALL vecP2mapP(P2RIVSTO,P2TEMP)
      IOUT=1
    CASE (2)
      CVAR='fldsto'
      CALL vecP2mapP(P2FLDSTO,P2TEMP)
      IOUT=1
    CASE (3)
      CVAR='rivout_pre'
      IF( .not. LSTOONLY ) THEN
        P2VEC(:,:)=D2RIVOUT_PRE(:,:)
        CALL vecP2mapP(P2VEC,P2TEMP)
        IOUT=1
      ENDIF
    CASE (4)
      CVAR='fldout_pre'
      IF( .not. LSTOONLY ) THEN
        P2VEC(:,:)=D2FLDOUT_PRE(:,:)
        CALL vecP2mapP(P2VEC,P2TEMP)
        IOUT=1
      ENDIF
   CASE (5)
      CVAR='rivdph_pre'
      IF( .not. LSTOONLY ) THEN
        P2VEC(:,:)=D2RIVDPH_PRE(:,:)
        CALL vecP2mapP(P2VEC,P2TEMP)
        IOUT=1
      ENDIF
    CASE (6)
      CVAR='fldsto_pre'
      IF( .not. LSTOONLY ) THEN
        P2VEC(:,:)=D2FLDSTO_PRE(:,:)
        CALL vecP2mapP(P2VEC,P2TEMP)
        IOUT=1
      ENDIF
    CASE (7)
      CVAR='gdwsto'
      IF( LGDWDLY ) THEN
        CALL vecP2mapP(P2GDWSTO,P2TEMP)
        IOUT=1
      ENDIF
    CASE (8)  !!! LDAMOUT
      CVAR='damsto'
      IF( LDAMOUT ) THEN
        CALL vecP2mapP(P2DAMSTO,P2TEMP)  !! P2DAMSTO only allocated for LDAMOUT
        IOUT=1
      ENDIF
    CASE (9)  !!! LLEVEE
      CVAR='levsto'
      IF( LLEVEE ) THEN
        CALL vecP2mapP(P2LEVSTO,P2TEMP)  !! P2DAMSTO only allocated for LDAMOUT
        IOUT=1
      ENDIF
  END SELECT

#ifdef UseMPI_CMF
  CALL CMF_MPI_AllReduce_P2MAP(P2TEMP)
#endif

  IF( IOUT==1 )THEN
    IF( REGIONTHIS==1 )THEN
      STATUS = NF90_INQ_VARID(NCID,TRIM(CVAR),VARID)  !! check VARID is defined above, write restart only when STATUS=0
      IF ( STATUS .EQ. 0 ) THEN
        CALL NCERROR( NF90_PUT_VAR(NCID,VARID,P2TEMP,(/1,1,1/),(/NX,NY,1/)) )
      ENDIF
    ENDIF
  ENDIF
ENDDO

IF ( LPTHOUT ) THEN
  IF ( .not. LSTOONLY )THEN
    P1PTH(:,:)=D1PTHFLW_PRE(:,:)  !! convert Float32 to Float64 (for Single Precison Use)
#ifdef UseMPI_CMF
    CALL CMF_MPI_AllReduce_P1PTH(P1PTH)
#endif
    IF( REGIONTHIS==1 )THEN
      CALL NCERROR( NF90_INQ_VARID(NCID,'pthflw_pre',VARID))
      CALL NCERROR( NF90_PUT_VAR(NCID,VARID,P1PTH,(/1,1,1/),(/NPTHOUT,NPTHLEV,1/)) )
    ENDIF
  ENDIF
ENDIF

IF( REGIONTHIS==1 )THEN
  CALL NCERROR( NF90_SYNC(NCID) )
  CALL NCERROR( NF90_CLOSE(NCID) )
ENDIF

WRITE(LOGNAM,*) 'WRTE_REST: WRITE RESTART NETCDF:',CFILE

#endif
END SUBROUTINE WRTE_REST_CDF
!==========================================================

END SUBROUTINE CMF_RESTART_WRITE
!####################################################################

END MODULE CMF_CTRL_RESTART_MOD
