MODULE CMF_CTRL_TRACER_MOD
!==========================================================
!* PURPOSE: CaMa-Flood tracer scheme (under development)
!
! (C) R. Hanazaki & D.Yamazaki (U-Tokyo)  Feb 2020
!
!* CONTAINS:
! -- CMF_DEM_NMLIST  : Read setting from namelist
! -- CMF_DAM_INIT    : Initialize dam data
! -- CMF_CALC_DAMOUT : Calculate inflow and outflow at dam
!
! Licensed under the Apache License, Version 2.0 (the "License");
!   You may not use this file except in compliance with the License.
!   You may obtain a copy of the License at: http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software distributed under the License is 
!  distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
! See the License for the specific language governing permissions and limitations under the License.
!==========================================================
USE PARKIND1,                ONLY: JPIM, JPRB, JPRM, JPRD
USE CMF_UTILS_MOD,           ONLY: INQUIRE_FID, CMF_CheckNanB
USE YOS_CMF_INPUT,           ONLY: LOGNAM,   LTRACE,  TMPNAM, NX, NY, NXIN, NYIN, INPN, RMIS
USE YOS_CMF_INPUT,           ONLY: IFRQ_OUT, CSUFBIN, CSUFVEC, LSPAMAT
USE YOS_CMF_MAP,             ONLY: NSEQMAX, NSEQALL, NSEQRIV, NPTHOUT
USE YOS_CMF_MAP,             ONLY: I1NEXT,  PTH_UPST,PTH_DOWN, I2MASK
USE YOS_CMF_MAP,             ONLY: I1UPST,   I1UPN,    I1P_OUT,  I1P_OUTN, I1P_INF, I1P_INFN
USE YOS_CMF_MAP,             ONLY: INPX, INPY, INPA
USE YOS_CMF_PROG,            ONLY: P2RIVSTO, P2FLDSTO
!============================
IMPLICIT NONE
SAVE
!*** NAMELIST/NAMTRACE/
INTEGER(KIND=JPIM)              :: NTRACE      !! number of tracer
CHARACTER(LEN=256)              :: CTRCNAM     !! tracer name
CHARACTER(LEN=256)              :: CTRCDIR     !! tracer input file directory
CHARACTER(LEN=256)              :: CTRCPRE     !! tracer file prefix
CHARACTER(LEN=256)              :: CTRCSUF     !! tracer file suffix

INTEGER(KIND=JPIM)              :: IFRQ_TRIN   !! tracer input frequency (hour)
REAL(KIND=JPRM)                 :: DTRCUNIT    !! tracer input unit conversion (DTRCUNIT=1 when input file is [(MASS)/m2/s]. )
LOGICAL                         :: LINPEND     !! true  for input    endian conversion

LOGICAL                         :: LTRCBIF     !! true  for consider bifurcation in tracer scheme

CHARACTER(LEN=256)              :: CRESTTRC         ! input restart file name
CHARACTER(LEN=256)              :: CRESTDIR         ! output restart file directory
CHARACTER(LEN=256)              :: CVNRSTTRC        ! output restart prefix
LOGICAL                         :: LRESTDBL         ! true: binary restart in double precision
INTEGER(KIND=JPIM)              :: IFRQ_RST         ! 0: only at last time, (1,2,3,...,24) hourly restart, 30: monthly restart

! output
CHARACTER(LEN=256)              :: COUTDIR           ! OUTPUT DIRECTORY
CHARACTER(LEN=256)              :: COUTTAG           ! Output Tag Name for each experiment
LOGICAL                         :: LOUTVEC           ! TRUE FOR VECTORIAL OUTPUT, FALSE FOR NX,NY OUTPUT


NAMELIST/NAMTRACE/   NTRACE, CTRCNAM, CTRCDIR, CTRCPRE,  CTRCSUF, IFRQ_TRIN, DTRCUNIT, LINPEND, LTRCBIF, &
                    &        CRESTTRC,CRESTDIR,CVNRSTTRC,LRESTDBL,IFRQ_RST,  COUTDIR, COUTTAG, LOUTVEC

!*** Local
REAL(KIND=JPRM)                 :: DTIN_TRC    !! tracer input frequency (Sec)
INTEGER(KIND=JPIM)              :: ITRACE      !! tracer id

!*** input
REAL(KIND=JPRB),ALLOCATABLE     :: TBUFF(:,:,:)       ! Buffer to store forcing tracer

!*** variables
REAL(KIND=JPRD),ALLOCATABLE     :: P2TRCSTO(:,:)       ! Tracer Storage

REAL(KIND=JPRB),ALLOCATABLE     :: D2TRCDNS(:,:)       ! Tracer Density
REAL(KIND=JPRB),ALLOCATABLE     :: D2TRCOUT(:,:)       ! Tracer Flux  (main channel)
REAL(KIND=JPRB),ALLOCATABLE     :: D2TRCINP(:,:)       ! Tracer input (interporlated to catchments)

REAL(KIND=JPRB),ALLOCATABLE     :: D1TRCPFLW(:,:)      ! Tracer Bifurcation Path Flux     (1D: NPTHALL)
REAL(KIND=JPRB),ALLOCATABLE     :: D2TRCPOUT(:,:)      ! Tracer Bifurcation Path Net Flux (2D: NSEQMAX)

!*** Average diagnostics for output
REAL(KIND=JPRB)                 :: NADD_out                    !! sum DT to calculate average
REAL(KIND=JPRB),ALLOCATABLE     :: D2TRCOUT_oAVG(:,:)   ! Tracer Flux (main channel)
REAL(KIND=JPRB),ALLOCATABLE     :: D2TRCDNS_oAVG(:,:)   ! Tracer Density
REAL(KIND=JPRB),ALLOCATABLE     :: D2TRCPOUT_oAVG(:,:)  ! Tracer Bifurcation Path Net Flux (2D: NSEQMAX)

!*** TYPE for tracer data (each tracer should have one data)
TYPE TTRACE
CHARACTER(LEN=256)              :: TRCNAME            ! tracer variable name
CHARACTER(LEN=256)              :: TRCPRE             ! input  file prefix
CHARACTER(LEN=256)              :: OUTFILE            ! output full path file name 
INTEGER(KIND=JPIM)              :: BINID              ! tracer binary output file ID
END TYPE TTRACE
TYPE(TTRACE),ALLOCATABLE        :: VTRACE(:)          ! tracer variable TYPE set

!================
!*** local variables for output
INTEGER(KIND=JPIM), PARAMETER   :: NVARS = 100
INTEGER(KIND=JPIM)              :: NVARSOUT
INTEGER(KIND=JPIM)              :: IRECOUT            ! Output file irec
!*** TYPE for output file    
TYPE TVAROUT
CHARACTER(LEN=256)              :: CVNAME             ! output variable name
CHARACTER(LEN=256)              :: CFILE              ! output full path file name 
INTEGER(KIND=JPIM)              :: BINID              ! output binary output file ID
END TYPE TVAROUT 
TYPE(TVAROUT),ALLOCATABLE       :: VAROUT(:)          ! output variable TYPE set


CONTAINS
!####################################################################
!* CONTAINS:
! -- CMF_TRACER_NMLIST      : Read setting from namelist
! -- CMF_TRACER_INIT        : Initialize tracer data
! -- CMF_TRACER_FORC_GET    : Read tracer forcing from file
! -- CMF_TRACER_FORC_INTERP : Interporlate tracer input to catchment
! -- CMF_TRACER_DENSITY     : Calculate tracer density
! -- CMF_TRACER_FLUX        : Calculate tracer flux and storage
!
!
!@@@@@@ TRACER Init @@@@@@
!####################################################################
SUBROUTINE CMF_TRACER_NMLIST
! reed setting from namelist
! -- Called from CMF_DRV_NMLIST
USE YOS_CMF_INPUT,      ONLY: CSETFILE,NSETFILE
IMPLICIT NONE
!================================================
WRITE(LOGNAM,*) ""
WRITE(LOGNAM,*) "!---------------------!"

!*** 1. open namelist
NSETFILE=INQUIRE_FID()
OPEN(NSETFILE,FILE=CSETFILE,STATUS="OLD")
WRITE(LOGNAM,*) "CMF::TRACER_NMLIST: namelist OPEN in unit: ", TRIM(CSETFILE), NSETFILE 

!*** 2. default value
NTRACE=2                        !! number of tracers

!! All tracer files must be in same directory, one file for one tracer input for each day:
!! File name should be $(CTRCDIR)/$(CTRCPRE)YYYYMMDD$(CTRCSUF). Example, inpdir/trace1_YYYYMMDD.bin
CTRCNAM="trace1,trace2"         !! tracer name
CTRCDIR="./"                    !! tracer input file directory
CTRCPRE="trace1_,trace2_"       !! tracer input file prefix
CTRCSUF=".bin"                  !! tracer input file suffix

IFRQ_TRIN=24                   !! tracer input DT (sec)
DTIN_TRC=86400                  !! tracer input DT (sec)
DTRCUNIT=1.0                    !! tracer unit conversion. (1 for per sec input. 60 for per min input)
LINPEND =.FALSE.

CRESTTRC="resttrc"   ! input restart file name
CRESTDIR="./"        ! output restart file directory
CVNRSTTRC="resttrc"   ! output restart file prefix
LRESTDBL=.TRUE.      ! true: binary restart double precision
IFRQ_RST=0           ! 0: only end of simulation, [1,2,3,6,12,24] at selected hour, 30: monthly

COUTDIR="./"
COUTTAG="_cmf"
LOUTVEC=.FALSE.

!*** 3. read namelist
REWIND(NSETFILE)
READ(NSETFILE,NML=NAMTRACE)

WRITE(LOGNAM,*)   "=== NAMELIST, NTRACER ==="
WRITE(LOGNAM,*)   "NTRACE:   " , NTRACE
WRITE(LOGNAM,*)   "CTRCNAM:  " , CTRCNAM
WRITE(LOGNAM,*)   "CTRCDIR:  " , CTRCDIR
WRITE(LOGNAM,*)   "CTRCPRE:  " , CTRCPRE
WRITE(LOGNAM,*)   "CTRCSUF:  " , CTRCSUF
WRITE(LOGNAM,*)   "IFRQ_TRIN: " , IFRQ_TRIN
WRITE(LOGNAM,*)   "DTRCUNIT: " , DTRCUNIT

WRITE(LOGNAM,*)   "CRESTTRC:  ", TRIM(CRESTTRC)
WRITE(LOGNAM,*)   "CRESTDIR:  ", TRIM(CRESTDIR)
WRITE(LOGNAM,*)   "CVNRSTTRC: ", TRIM(CVNRSTTRC)
WRITE(LOGNAM,*)   "LRESTDBL:  ", LRESTDBL
WRITE(LOGNAM,*)   "IFRQ_RST:  ", IFRQ_RST

WRITE(LOGNAM,*)   "COUTDIR:  ", TRIM(COUTDIR)
WRITE(LOGNAM,*)   "COUTTAG:  ", TRIM(COUTTAG)
WRITE(LOGNAM,*)   "LOUTVEC:  ", LOUTVEC

IF( LINPEND )THEN
  WRITE(LOGNAM,*)   "LINPEND:   ", LINPEND
ENDIF

DTIN_TRC=IFRQ_TRIN*60.*60.  !! hour to sec
WRITE(LOGNAM,*)   "DTIN_TRC: " , DTIN_TRC


CLOSE(NSETFILE)

WRITE(LOGNAM,*) "CMF::TRACER_NMLIST: end" 

END SUBROUTINE CMF_TRACER_NMLIST
!####################################################################
!+
!+
!+
!####################################################################
SUBROUTINE CMF_TRACER_INIT
! tracer variable initialization
IMPLICIT NONE

CHARACTER(LEN=256)              :: CTMP
INTEGER(KIND=JPIM)              :: J,J0
!####################################################################
WRITE(LOGNAM,*) ""
WRITE(LOGNAM,*) "!---------------------!"
WRITE(LOGNAM,*) "CMF::TRACER_INIT: initialize tracer" 

ALLOCATE(VTRACE(NTRACE))  !! tracer data variables
ALLOCATE(TBUFF(NXIN,NYIN,NTRACE))  !! tracer input buffer file

!==========
WRITE(LOGNAM,*) "  Check Tracer Names"
ITRACE=0
J0=1
DO J=1,LEN(TRIM(CTRCNAM))
  IF( (J>J0) .AND. (CTRCNAM(J:J) .EQ. ',') ) THEN
    CTMP=TRIM(ADJUSTL(CTRCNAM(J0:J-1)))
    IF (LEN(CTMP) > 0 ) THEN
      ITRACE=ITRACE+1
      VTRACE(ITRACE)%TRCNAME=CTMP
      WRITE(LOGNAM,*) ITRACE, trim(VTRACE(ITRACE)%TRCNAME)
    ENDIF
    J0=J+1
  ENDIF
ENDDO
! Last one 
IF ( J0 <= LEN(TRIM(CTRCNAM)) ) THEN
  J=LEN(TRIM(CTRCNAM))
  CTMP=TRIM(ADJUSTL(CTRCNAM(J0:J)))
  IF (LEN(CTMP) > 0 ) THEN
      ITRACE=ITRACE+1
      VTRACE(ITRACE)%TRCNAME=CTMP
      WRITE(LOGNAM,*) ITRACE, trim(VTRACE(ITRACE)%TRCNAME)
  ENDIF
ENDIF 
IF( ITRACE/=NTRACE )THEN
  WRITE(LOGNAM,*) 'ERROR: Tracer Name number do not match with NTRACE', trim(CTRCNAM), NTRACE
  stop
ENDIF

!==========
WRITE(LOGNAM,*) "  Check Tracer Input File Prefix"
ITRACE=0
J0=1
DO J=1,LEN(TRIM(CTRCPRE))
  IF( (J>J0) .AND. (CTRCPRE(J:J) .EQ. ',') ) THEN
    CTMP=TRIM(ADJUSTL(CTRCPRE(J0:J-1)))
    IF (LEN(CTMP) > 0 ) THEN
      ITRACE=ITRACE+1
      VTRACE(ITRACE)%TRCPRE=CTMP
      WRITE(LOGNAM,*) ITRACE, trim(VTRACE(ITRACE)%TRCPRE)
    ENDIF
    J0=J+1
  ENDIF
ENDDO
! Last one 
IF ( J0 <= LEN(TRIM(CTRCPRE)) ) THEN
  J=LEN(TRIM(CTRCPRE))
  CTMP=TRIM(ADJUSTL(CTRCPRE(J0:J)))
  IF (LEN(CTMP) > 0 ) THEN
      ITRACE=ITRACE+1
      VTRACE(ITRACE)%TRCPRE=CTMP
      WRITE(LOGNAM,*) ITRACE, trim(VTRACE(ITRACE)%TRCPRE)
  ENDIF
ENDIF 
IF( ITRACE/=NTRACE )THEN
  WRITE(LOGNAM,*) 'ERROR: Tracer Prefix number do not match with NTRACE', trim(CTRCNAM), NTRACE
  stop
ENDIF

!==========
WRITE(LOGNAM,*) "  Allocate Tracer Variables"
ALLOCATE( P2TRCSTO(NSEQMAX,NTRACE) )
ALLOCATE( D2TRCDNS(NSEQMAX,NTRACE) )
ALLOCATE( D2TRCOUT(NSEQMAX,NTRACE) )
ALLOCATE( D2TRCINP(NSEQMAX,NTRACE) )
P2TRCSTO(:,:)=0._JPRB
D2TRCDNS(:,:)=0._JPRB
D2TRCOUT(:,:)=0._JPRB
D2TRCINP(:,:)=0._JPRB

ALLOCATE( D1TRCPFLW(NPTHOUT,NTRACE) )
ALLOCATE( D2TRCPOUT(NSEQMAX,NTRACE) )
D1TRCPFLW(:,:)=0._JPRB
D2TRCPOUT(:,:)=0._JPRB

Nadd_out=0._JPRB
ALLOCATE( D2TRCDNS_oAVG(NSEQMAX,NTRACE) )
ALLOCATE( D2TRCOUT_oAVG(NSEQMAX,NTRACE) )
ALLOCATE( D2TRCPOUT_oAVG(NSEQMAX,NTRACE) )
D2TRCDNS_oAVG(:,:)=0._JPRB
D2TRCOUT_oAVG(:,:)=0._JPRB
D2TRCPOUT_oAVG(:,:)=0._JPRB


END SUBROUTINE CMF_TRACER_INIT
!####################################################################
!+
!+
!+
!####################################################################
SUBROUTINE CMF_TRACER_RESTART_INIT
! read restart file
USE CMF_UTILS_MOD,           ONLY: INQUIRE_FID, mapP2vecP
IMPLICIT NONE
!*** LOCAL
REAL(KIND=JPRM)                 :: R2TEMP(NX,NY)
REAL(KIND=JPRD)                 :: P2TEMP(NX,NY)
REAL(KIND=JPRD)                 :: P2VEC(NSEQMAX,1)
CHARACTER(LEN=256)              :: CFILE
!================================================
CFILE=TRIM(CRESTTRC)
WRITE(LOGNAM,*)'CMF::TRACER_RESTART_INIT: read restart binary: ', TRIM(CFILE)

TMPNAM=INQUIRE_FID()

IF( LRESTDBL )THEN
  OPEN(TMPNAM,FILE=CFILE,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=8*NX*NY)
  DO ITRACE=1, NTRACE
    READ(TMPNAM,REC=ITRACE) P2TEMP
    CALL mapP2vecP(P2TEMP,P2VEC)
    P2TRCSTO(:,ITRACE)=P2VEC(:,1)
  END DO
ELSE
  OPEN(TMPNAM,FILE=CFILE,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4*NX*NY)
  DO ITRACE=1, NTRACE
    READ(TMPNAM,REC=ITRACE) R2TEMP
    P2TEMP(:,:)=R2TEMP(:,:)
    CALL mapP2vecP(P2TEMP,P2VEC)
    P2TRCSTO(:,ITRACE)=P2VEC(:,1)
  END DO
ENDIF

END SUBROUTINE CMF_TRACER_RESTART_INIT
!####################################################################
!+
!+
!+
!####################################################################
SUBROUTINE CMF_TRACER_RESTART_WRITE
! write restart files
USE YOS_CMF_TIME,       ONLY: KSTEP,  NSTEPS, JYYYYMMDD, JHHMM, JDD, JHOUR, JMIN
USE CMF_UTILS_MOD,      ONLY: INQUIRE_FID, vecP2mapP, vecP2mapR
USE YOS_CMF_MAP,        ONLY: REGIONTHIS
#ifdef UseMPI_CMF
USE CMF_CTRL_MPI_MOD,   ONLY: CMF_MPI_AllReduce_P2MAP, CMF_MPI_AllReduce_R2MAP
#endif
IMPLICIT NONE
!* local variable
INTEGER(KIND=JPIM)         :: IREST
CHARACTER(LEN=256)         :: CFILE,CDATE
REAL(KIND=JPRD)            :: P2VAR(NSEQMAX,1)  !! use Real*8 for code simplicity
REAL(KIND=JPRM)            :: R2TEMP(NX,NY)
REAL(KIND=JPRD)            :: P2TEMP(NX,NY)
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
  WRITE(LOGNAM,*) 'CMF::TRACER_RESTART_WRITE: write time: ' , JYYYYMMDD, JHHMM

  !*** set file nam
  WRITE(CDATE,'(I8.8,I2.2)') JYYYYMMDD,JHOUR
  CFILE=TRIM(CRESTDIR)//TRIM(CVNRSTTRC)//TRIM(CDATE)//TRIM(CSUFBIN)
  WRITE(LOGNAM,*) 'TRACER_WRTE_REST_BIN: restart file:',CFILE

  !*** write restart data (2D map)
  TMPNAM=INQUIRE_FID()

  IF( LRESTDBL )THEN
    IF ( REGIONTHIS==1 ) OPEN(TMPNAM,FILE=CFILE,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=8*NX*NY)
  ELSE
    IF ( REGIONTHIS==1 ) OPEN(TMPNAM,FILE=CFILE,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4*NX*NY)
  ENDIF

  DO ITRACE=1, NTRACE
    !! Double Precision Restart
    P2VAR(:,1)=P2TRCSTO(:,ITRACE)
    IF( LRESTDBL )THEN
      CALL vecP2mapP(P2VAR,P2TEMP)  
#ifdef UseMPI_CMF
      CALL CMF_MPI_AllReduce_P2MAP(P2TEMP)
#endif
      IF ( REGIONTHIS==1 ) WRITE(TMPNAM,REC=ITRACE) P2TEMP

    !! Single Precision Restart
    ELSE
      CALL vecP2mapR(P2VAR,R2TEMP)  
#ifdef UseMPI_CMF
      CALL CMF_MPI_AllReduce_R2MAP(R2TEMP)
#endif
      IF ( REGIONTHIS==1 ) WRITE(TMPNAM,REC=ITRACE) R2TEMP
    ENDIF

  END DO
ENDIF

END SUBROUTINE CMF_TRACER_RESTART_WRITE
!####################################################################


!@@@@@@ TRACER Forcing Input @@@@@@
!####################################################################
SUBROUTINE CMF_TRACER_FORC_GET
! tracer forcing data
USE YOS_CMF_TIME,            ONLY: IYYYY, IMM, IDD, IHOUR, IMIN
USE CMF_UTILS_MOD,           ONLY: CONV_END
IMPLICIT NONE
!* Local variables
INTEGER(KIND=JPIM)              :: IRECINP
INTEGER(KIND=JPIM)              :: ISEC
CHARACTER(LEN=256)              :: CIFNAME             !! INPUT FILE
CHARACTER(LEN=256)              :: CDATE               !!
REAL(KIND=JPRM)                 :: R2TMP(NXIN,NYIN)
!####################################################################
!*** 1. calculate IREC for sub-daily runoff
ISEC    = IHOUR*60*60+IMIN*60   !! current second in a day
IRECINP = int( ISEC/DTIN_TRC ) +1   !! runoff irec (sub-dairy runoff)

DO ITRACE=1, NTRACE

  !*** 2. set file name
  WRITE(CDATE,'(I4.4,I2.2,I2.2)') IYYYY,IMM,IDD
  CIFNAME=TRIM(CTRCDIR)//'/'//TRIM(VTRACE(ITRACE)%TRCPRE)//TRIM(CDATE)//TRIM(CTRCSUF)
  WRITE(LOGNAM,*) "CMF::FORCING_GET_BIN:",TRIM(CIFNAME)

  !*** 3. open & read runoff
  TMPNAM=INQUIRE_FID()
  OPEN(TMPNAM,FILE=CIFNAME,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4*NXIN*NYIN)
  READ(TMPNAM,REC=IRECINP) R2TMP
  CLOSE(TMPNAM)
  WRITE(LOGNAM,*) "IRECINP:", IRECINP

  !*** 4. copy runoff to PBUSS, endian conversion is needed
  IF( LINPEND ) CALL CONV_END(R2TMP,NXIN,NYIN)
  TBUFF(:,:,ITRACE)=R2TMP(:,:)

END DO

END SUBROUTINE CMF_TRACER_FORC_GET
!####################################################################
!+
!+
!+
!####################################################################
SUBROUTINE CMF_TRACER_FORC_INTERP
! interporlate with inpmat, then send runoff data to CaMa-Flood 
! -- called from "Main Program / Coupler" or CMF_DRV_ADVANCE
IMPLICIT NONE
! SAVE for OMP
INTEGER(KIND=JPIM),SAVE  ::  ISEQ
INTEGER(KIND=JPIM),SAVE  ::  IXIN, IYIN, INPI  !! FOR OUTPUT
!$OMP THREADPRIVATE         (IXIN, IYIN, INPI)
!============================

DO ITRACE=1, NTRACE

  !$OMP PARALLEL DO
    DO ISEQ=1, NSEQMAX
      D2TRCINP(ISEQ,ITRACE)=0._JPRB
      DO INPI=1, INPN
        IXIN=INPX(ISEQ,INPI)
        IYIN=INPY(ISEQ,INPI)
        IF( IXIN>0 )THEN
          IF( IXIN > NXIN .OR. IYIN > NYIN ) THEN
            WRITE(LOGNAM,*)  "error"
            WRITE(LOGNAM,*)  'XXX',ISEQ,INPI,IXIN,IYIN
            CYCLE
          ENDIF
          IF( TBUFF(IXIN,IYIN,ITRACE).NE.RMIS )THEN
            D2TRCINP(ISEQ,ITRACE) = D2TRCINP(ISEQ,ITRACE) + TBUFF(IXIN,IYIN,ITRACE) * INPA(ISEQ,INPI) / DTRCUNIT
            !! assume tracer input file unit is [ MASS/m2/s ]. If it is not "per second", DTRCUNIT should be changed. (default is 1)
          ENDIF
          IF( CMF_CheckNanB(TBUFF(IXIN,IYIN,ITRACE),0._JPRB) ) D2TRCINP(ISEQ,ITRACE)=0._JPRB  !! treat NaN runoff input 
        ENDIF
      END DO
      D2TRCINP(ISEQ,ITRACE)=MAX(D2TRCINP(ISEQ,ITRACE), 0._JPRB)
    END DO
  !$OMP END PARALLEL DO

END DO

END SUBROUTINE CMF_TRACER_FORC_INTERP
!####################################################################



!@@@@@@ TRACER PHYSICS @@@@@@
!####################################################################
SUBROUTINE CMF_TRACER_DENSITY
IMPLICIT NONE
! SAVE for OpenMP
INTEGER(KIND=JPIM),SAVE    :: ISEQ
!============================

DO ITRACE=1, NTRACE
  !$OMP PARALLEL DO SIMD
  DO ISEQ=1, NSEQALL
    D2TRCDNS(ISEQ,ITRACE)=REAL( P2TRCSTO(ISEQ,ITRACE)/max( (P2RIVSTO(ISEQ,1)+P2FLDSTO(ISEQ,1)),1.D-6) ,KIND=JPRB)
  END DO
  !$OMP END PARALLEL DO SIMD
END DO

END SUBROUTINE CMF_TRACER_DENSITY
!####################################################################
!+
!+
!+
!####################################################################
SUBROUTINE CMF_TRACER_FLUX
! Calculate Tracer Physics
USE YOS_CMF_INPUT,      ONLY: DT
USE YOS_CMF_DIAG,       ONLY: D2OUTFLW_aAVG, D1PTHFLWSUM_aAVG

IMPLICIT NONE
REAL(KIND=JPRD)            :: P2STOOUT(NSEQMAX)                      !! total outflow from a grid     [m3]
REAL(KIND=JPRD)            :: P2TRCINF(NSEQMAX)                      !! 
REAL(KIND=JPRB)            :: D2RATE(NSEQMAX)                        !! outflow correction

! SAVE for OpenMP
INTEGER(KIND=JPIM),SAVE    :: ISEQ, IPTH, JSEQ, INUM, JPTH, ISEQP, JSEQP
!$OMP THREADPRIVATE                      (JSEQ, INUM, JPTH, ISEQP, JSEQP)
!============================

! ****** 1. calculate flux
D2TRCPOUT(:,:)=0


DO ITRACE=1, NTRACE
  P2TRCINF(:) = 0._JPRD
  P2STOOUT(:) = 0._JPRD
  D2RATE(:)   = 1._JPRB

  !$OMP PARALLEL DO
  DO ISEQ=1, NSEQALL
    JSEQ=I1NEXT(ISEQ)
    IF( D2OUTFLW_aAVG(ISEQ,1)>= 0 )THEN
      D2TRCOUT(ISEQ,ITRACE)=D2TRCDNS(ISEQ,ITRACE) * D2OUTFLW_aAVG(ISEQ,1)
    ELSE !! reverse flow
      IF( JSEQ>0 )THEN
        D2TRCOUT(ISEQ,ITRACE)=D2TRCDNS(JSEQ,ITRACE) * D2OUTFLW_aAVG(ISEQ,1)  !! use downstream density
      ELSE
        D2TRCOUT(ISEQ,ITRACE)=0._JPRB  !! from ocean (assume no flux)
      ENDIF
    ENDIF
  END DO
  !$OMP END PARALLEL DO

  IF( LTRCBIF ) THEN
    !$OMP PARALLEL DO
    DO IPTH=1, NPTHOUT  
      ISEQP=PTH_UPST(IPTH)
      JSEQP=PTH_DOWN(IPTH)
      IF (ISEQP<=0 .OR. JSEQP<=0 ) CYCLE  !! Avoid calculation outside of domain
    
      IF( D1PTHFLWSUM_aAVG(IPTH)>=0. )THEN
        D1TRCPFLW(IPTH,ITRACE)=D2TRCDNS(ISEQP,ITRACE) * D1PTHFLWSUM_aAVG(IPTH)
      ELSE !! reverse flow
        D1TRCPFLW(IPTH,ITRACE)=D2TRCDNS(JSEQP,ITRACE) * D1PTHFLWSUM_aAVG(IPTH)
      ENDIF
    END DO
    !$OMP END PARALLEL DO
  ENDIF

! ****** 2. calculate total outflow from each catchment
!! flux adjustment for mass balance
  !! for normal cells ---------
  !$OMP PARALLEL DO
  DO ISEQ=1, NSEQALL
    P2STOOUT(ISEQ) = max( D2TRCOUT(ISEQ,ITRACE),0._JPRB ) 
    IF( I1UPN(ISEQ)>0 )THEN
      DO INUM=1, I1UPN(ISEQ)
        JSEQ=I1UPST(ISEQ,INUM)
        P2STOOUT(ISEQ) = P2STOOUT(ISEQ) + max( -D2TRCOUT(JSEQ,ITRACE),0._JPRB ) 
      END DO
    ENDIF
    P2STOOUT(ISEQ) = P2STOOUT(ISEQ) *DT
  END DO
  !$OMP END PARALLEL DO

  !! for bifurcation channels ------------
  IF( LTRCBIF )THEN
  !$OMP PARALLEL DO  !! No OMP Atomic for bit-identical simulation (set in Mkinclude)
    DO ISEQ=1, NSEQALL
      IF( I1P_OUTN(ISEQ)>0 )THEN
        DO INUM=1, I1P_OUTN(ISEQ)
          JPTH=I1P_OUT(ISEQ,INUM)
          P2STOOUT(ISEQ) = P2STOOUT(ISEQ) + max(  D1TRCPFLW(JPTH,ITRACE),0._JPRB )*DT
        END DO
      ENDIF
  
      IF( I1P_INFN(ISEQ)>0 )THEN
        DO INUM=1, I1P_INFN(ISEQ)
          JPTH=I1P_INF(ISEQ,INUM)
          P2STOOUT(ISEQ) = P2STOOUT(ISEQ) + max( -D1TRCPFLW(JPTH,ITRACE),0._JPRB )*DT
        END DO
      ENDIF
    END DO
  !$OMP END PARALLEL DO
  ENDIF

  !! calculate modification rate
  !$OMP PARALLEL DO SIMD
  DO ISEQ=1, NSEQALL
    IF ( P2STOOUT(ISEQ) > 1.E-8_JPRB ) THEN
      D2RATE(ISEQ) = min( REAL(P2TRCSTO(ISEQ,ITRACE) * P2STOOUT(ISEQ)**(-1.), KIND=JPRB), 1._JPRB )
    ENDIF
  END DO
  !$OMP END PARALLEL DO SIMD

  !============================
  !****** 3. modify outflow

  !! normal pixels------
  !$OMP PARALLEL DO SIMD  !! No OMP Atomic for bit-identical simulation (set in Mkinclude)
  DO ISEQ=1, NSEQRIV ! for normal pixels
    JSEQ=I1NEXT(ISEQ)
    IF( D2TRCOUT(ISEQ,ITRACE) >= 0._JPRB )THEN
      D2TRCOUT(ISEQ,ITRACE) = D2TRCOUT(ISEQ,ITRACE)*D2RATE(ISEQ)
    ELSE
      D2TRCOUT(ISEQ,ITRACE) = D2TRCOUT(ISEQ,ITRACE)*D2RATE(JSEQ)
    ENDIF
  END DO
  !$OMP END PARALLEL DO SIMD

  !! river mouth-----------------
  !$OMP PARALLEL DO SIMD
  DO ISEQ=NSEQRIV+1, NSEQALL
    D2TRCOUT(ISEQ,ITRACE) = D2TRCOUT(ISEQ,ITRACE)*D2RATE(ISEQ)
  END DO
  !$OMP END PARALLEL DO SIMD

  !$OMP PARALLEL DO
  DO ISEQ=1, NSEQALL ! for normal pixels
    IF( I1UPN(ISEQ)>0 )THEN
      DO INUM=1, I1UPN(ISEQ)
        JSEQ=I1UPST(ISEQ,INUM)
        P2TRCINF(ISEQ) = P2TRCINF(ISEQ) + D2TRCOUT(JSEQ,ITRACE)   !! total inflow to a grid (from upstream)
      END DO
    ENDIF
  END DO
  !$OMP END PARALLEL DO

  !! bifurcation channels --------
  IF( LTRCBIF )THEN
    !$OMP PARALLEL DO 
    DO IPTH=1, NPTHOUT  
      ISEQP=PTH_UPST(IPTH)
      JSEQP=PTH_DOWN(IPTH)
      !! Avoid calculation outside of domain
      IF (ISEQP<=0 .OR. JSEQP<=0 ) CYCLE
      IF (I2MASK(ISEQP,1)>0 .OR. I2MASK(JSEQP,1)>0 ) CYCLE  !! I2MASK is for 1: kinemacit 2: dam  no bifurcation
      
      IF( D1TRCPFLW(IPTH,ITRACE) >= 0._JPRB )THEN
        D1TRCPFLW(IPTH,ITRACE)  = D1TRCPFLW(IPTH,ITRACE)*D2RATE(ISEQP)
      ELSE
        D1TRCPFLW(IPTH,ITRACE)  = D1TRCPFLW(IPTH,ITRACE)*D2RATE(JSEQP)  !! reverse flow
      ENDIF
    END DO
    !$OMP END PARALLEL DO

    !$OMP PARALLEL DO
    DO ISEQ=1, NSEQALL
      IF( I1P_OUTN(ISEQ)>0 )THEN
        DO INUM=1, I1P_OUTN(ISEQ)
          JPTH=I1P_OUT(ISEQ,INUM)
          D2TRCPOUT(ISEQ,ITRACE) = D2TRCPOUT(ISEQ,ITRACE) + D1TRCPFLW(JPTH,ITRACE)
        END DO
      ENDIF
  
      IF( I1P_INFN(ISEQ)>0 )THEN
        DO INUM=1, I1P_INFN(ISEQ)
          JPTH=I1P_INF(ISEQ,INUM)
          D2TRCPOUT(ISEQ,ITRACE) = D2TRCPOUT(ISEQ,ITRACE) - D1TRCPFLW(JPTH,ITRACE)
        END DO
      ENDIF
    END DO
    !$OMP END PARALLEL DO 
  ENDIF

  !============================
  !*** 4. calculate next step storage

  !$OMP PARALLEL DO SIMD
  DO ISEQ=1, NSEQALL
    P2TRCSTO(ISEQ,ITRACE) = P2TRCSTO(ISEQ,ITRACE) + P2TRCINF(ISEQ)*DT - D2TRCOUT(ISEQ,ITRACE)*DT
    P2TRCSTO(ISEQ,ITRACE) = P2TRCSTO(ISEQ,ITRACE) - D2TRCPOUT(ISEQ,ITRACE)*DT
    P2TRCSTO(ISEQ,ITRACE) = P2TRCSTO(ISEQ,ITRACE) + D2TRCINP(ISEQ,ITRACE) *DT
    P2TRCSTO(ISEQ,ITRACE) = max ( P2TRCSTO(ISEQ,ITRACE), 0._JPRD )
  END DO
  !$OMP END PARALLEL DO SIMD

END DO

CALL CMF_TRACER_DIAG_AVEADD

END SUBROUTINE CMF_TRACER_FLUX
!####################################################################



!@@@@@@ TRACER Output @@@@@@
!####################################################################
SUBROUTINE CMF_TRACER_OUTPUT_INIT
! Initialize tracer output module (create/open files)
! -- Called from CMF_DRV_INIT
USE YOS_CMF_TIME,            ONLY: ISYYYY, ISMM,   ISDD,   ISHOUR, ISMIN
USE YOS_CMF_MAP,             ONLY: REGIONTHIS
USE CMF_UTILS_MOD,           ONLY: INQUIRE_FID
IMPLICIT NONE
!* Local variables 
CHARACTER(LEN=256)              :: CTIME
INTEGER(KIND=JPIM)              :: JF
CHARACTER(LEN=256)              :: CVNAMES(NVARS)
!================================================
WRITE(LOGNAM,*) ""
WRITE(LOGNAM,*) "!---------------------!"

WRITE(LOGNAM,*) "CMF::TRACER_OUTPUT_INIT: set variable names"
!! 
NVARSOUT=0
DO ITRACE=1, NTRACE
  NVARSOUT=NVARSOUT+1
  CVNAMES(NVARSOUT)=TRIM(VTRACE(ITRACE)%TRCNAME)//'_sto'    !! storage

  NVARSOUT=NVARSOUT+1
  CVNAMES(NVARSOUT)=TRIM(VTRACE(ITRACE)%TRCNAME)//'_out'    !! flux (main channel)

  NVARSOUT=NVARSOUT+1
  CVNAMES(NVARSOUT)=TRIM(VTRACE(ITRACE)%TRCNAME)//'_dns'    !  density

  IF( LTRCBIF )THEN
    NVARSOUT=NVARSOUT+1
    CVNAMES(NVARSOUT)=TRIM(VTRACE(ITRACE)%TRCNAME)//'_bifout'  !! net flux (bifurcation channel out-in)
  ENDIF

END DO

ALLOCATE(VAROUT(NVARSOUT))
WRITE(CTIME,'(A14,I4.4,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2)') 'seconds since ',ISYYYY,'-',ISMM,'-',ISDD,' ',ISHOUR,":",ISMIN

!* Loop on variables and create files 
DO JF=1,NVARSOUT
  WRITE(LOGNAM,*) "Creating output for variable:", TRIM( CVNAMES(JF) )
  VAROUT(JF)%CVNAME=CVNAMES(JF)
  VAROUT(JF)%BINID=INQUIRE_FID()

  IF( LOUTVEC )THEN   !!  1D land only output
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
END DO

IRECOUT=0  ! Initialize Output record to 1 (shared in netcdf & binary)

END SUBROUTINE CMF_TRACER_OUTPUT_INIT
!####################################################################
!+
!+
!+
!####################################################################
SUBROUTINE CMF_TRACER_OUTPUT_WRITE
!======
USE CMF_UTILS_MOD,      ONLY: vecD2mapR
! save results to output files
! -- Called either from "MAIN/Coupler" or CMF_DRV_ADVANCE
USE YOS_CMF_MAP,        ONLY: REGIONTHIS
USE YOS_CMF_TIME,       ONLY: JYYYYMMDD, JHHMM, JHOUR, JMIN
#ifdef UseMPI_CMF
USE CMF_CTRL_MPI_MOD,   ONLY: CMF_MPI_AllReduce_P2MAP, CMF_MPI_AllReduce_R2MAP
#endif

IMPLICIT NONE
INTEGER(KIND=JPIM)          :: JF
!*** LOCAL
REAL(KIND=JPRM)             :: R2OUT(NX,NY)
REAL(KIND=JPRM)             :: R2COPY(NSEQMAX,1)
REAL(KIND=JPRB)             :: D2COPY(NSEQMAX,1)        !! Dammy Array for Float64/32 switch
!================================================
!*** 0. check date:hour with output frequency
IF ( MOD(JHOUR,IFRQ_OUT)==0 .and. JMIN==0 ) THEN             ! JHOUR: end of time step , NFPPH: output frequency (hour)

  CALL CMF_TRACER_DIAG_GETAVE

  !*** 1. update IREC & calc average variable
  IRECOUT=IRECOUT+1 
  WRITE(LOGNAM,*) 'CMF::TRACER_OUTPUT_WRITE: write at time: ', JYYYYMMDD, JHHMM, IRECOUT

  !*** 2. check variable name & allocate data to pointer DVEC
  JF=0
  DO ITRACE=1, NTRACE
    !! storage ======  
    JF=JF+1
    D2COPY(:,1)=REAL(P2TRCSTO(:,ITRACE),KIND=JPRB) !! convert Double to Single precision when using SinglePrecisionMode 

    IF( LOUTVEC )THEN
      R2COPY(:,1)=REAL(D2COPY(:,1),KIND=JPRM)
      WRITE(VAROUT(JF)%BINID,REC=IRECOUT) R2COPY         !! 1D vector (optional)
    ELSE
      !! convert 1Dvector to 2Dmap
      CALL vecD2mapR(D2COPY,R2OUT)             !! MPI node data is gathered by vecP2mapR
#ifdef UseMPI_CMF
      CALL CMF_MPI_AllReduce_R2MAP(R2OUT)
#endif
      IF ( REGIONTHIS==1 ) WRITE(VAROUT(JF)%BINID,REC=IRECOUT) R2OUT         !! 2D map
    ENDIF

    !! flux =======
    JF=JF+1
    IF( LOUTVEC )THEN
      R2COPY(:,1)=REAL(D2TRCOUT_oAVG(:,ITRACE),KIND=JPRM)
      WRITE(VAROUT(JF)%BINID,REC=IRECOUT) R2COPY         !! 1D vector (optional)
    ELSE
      !! convert 1Dvector to 2Dmap
      CALL vecD2mapR(D2TRCOUT_oAVG(:,ITRACE),R2OUT)             !! MPI node data is gathered by vecP2mapR
#ifdef UseMPI_CMF
      CALL CMF_MPI_AllReduce_R2MAP(R2OUT)
#endif
      IF ( REGIONTHIS==1 ) WRITE(VAROUT(JF)%BINID,REC=IRECOUT) R2OUT         !! 2D map
    ENDIF

    !! flux =======
    JF=JF+1
    IF( LOUTVEC )THEN
      R2COPY(:,1)=REAL(D2TRCDNS_oAVG(:,ITRACE),KIND=JPRM)
      WRITE(VAROUT(JF)%BINID,REC=IRECOUT) R2COPY         !! 1D vector (optional)
    ELSE
      !! convert 1Dvector to 2Dmap
      CALL vecD2mapR(D2TRCDNS_oAVG(:,ITRACE),R2OUT)             !! MPI node data is gathered by vecP2mapR
#ifdef UseMPI_CMF
      CALL CMF_MPI_AllReduce_R2MAP(R2OUT)
#endif
      IF ( REGIONTHIS==1 ) WRITE(VAROUT(JF)%BINID,REC=IRECOUT) R2OUT         !! 2D map
    ENDIF

    !! bifurcation net outflow =======
    IF( LTRCBIF )THEN 
      JF=JF+1
      IF( LOUTVEC )THEN
        R2COPY(:,1)=REAL(D2TRCPOUT_oAVG(:,ITRACE),KIND=JPRM)
        WRITE(VAROUT(JF)%BINID,REC=IRECOUT) R2COPY         !! 1D vector (optional)
      ELSE
        !! convert 1Dvector to 2Dmap
        CALL vecD2mapR(D2TRCPOUT_oAVG(:,ITRACE),R2OUT)             !! MPI node data is gathered by vecP2mapR
#ifdef UseMPI_CMF
        CALL CMF_MPI_AllReduce_R2MAP(R2OUT)
#endif
        IF ( REGIONTHIS==1 ) WRITE(VAROUT(JF)%BINID,REC=IRECOUT) R2OUT         !! 2D map
      ENDIF
    ENDIF
  END DO

  WRITE(LOGNAM,*) 'CMF::TRACER_OUTPUT_WRITE: end'

ENDIF

CALL CMF_TRACER_DIAG_RESET

END SUBROUTINE CMF_TRACER_OUTPUT_WRITE
!####################################################################
!+
!+
!+
!####################################################################
SUBROUTINE CMF_TRACER_OUTPUT_END
! Finalize output module (close files)
! -- Called from CMF_DRV_END
USE YOS_CMF_MAP,             ONLY: REGIONTHIS
IMPLICIT NONE
! Local variables
INTEGER(KIND=JPIM)              :: JF
!================================================
WRITE(LOGNAM,*) ""
WRITE(LOGNAM,*) "!---------------------!"
WRITE(LOGNAM,*) "CMF::TRACER_OUTPUT_END: finalize output module"

IF( REGIONTHIS==1 )THEN
  DO JF=1,NVARSOUT
    CLOSE(VAROUT(JF)%BINID)
    WRITE(LOGNAM,*) "Output binary output unit closed:",VAROUT(JF)%BINID
  ENDDO
ENDIF

END SUBROUTINE CMF_TRACER_OUTPUT_END
!####################################################################




! @@@@@@ Tracer Diagnose (for output)
!####################################################################
SUBROUTINE CMF_TRACER_DIAG_RESET
USE YOS_CMF_TIME,       ONLY: JYYYYMMDD, JHHMM
IMPLICIT NONE
!================================================
WRITE(LOGNAM,*) "CMF::DIAG_AVERAGE: reset", JYYYYMMDD, JHHMM
NADD_out=0
D2TRCOUT_oAVG(:,:) = 0._JPRB
D2TRCDNS_oAVG(:,:) = 0._JPRB
D2TRCPOUT_oAVG(:,:)= 0._JPRB
END SUBROUTINE CMF_TRACER_DIAG_RESET
!####################################################################
!
!####################################################################
SUBROUTINE CMF_TRACER_DIAG_AVEADD
USE YOS_CMF_INPUT,      ONLY: DT
IMPLICIT NONE
!====================
NADD_out=NADD_out+DT
D2TRCOUT_oAVG(:,:)  = D2TRCOUT_oAVG(:,:)  + D2TRCOUT(:,:) *DT
D2TRCDNS_oAVG(:,:)  = D2TRCDNS_oAVG(:,:)  + D2TRCDNS(:,:) *DT
D2TRCPOUT_oAVG(:,:) = D2TRCPOUT_oAVG(:,:) + D2TRCPOUT(:,:)*DT
END SUBROUTINE CMF_TRACER_DIAG_AVEADD
!####################################################################
!
!####################################################################
SUBROUTINE CMF_TRACER_DIAG_GETAVE
IMPLICIT NONE
!====================
D2TRCOUT_oAVG(:,:)  = D2TRCOUT_oAVG(:,:)  / REAL(NADD_out,KIND=JPRB)
D2TRCDNS_oAVG(:,:)  = D2TRCDNS_oAVG(:,:)  / REAL(NADD_out,KIND=JPRB)
D2TRCPOUT_oAVG(:,:) = D2TRCPOUT_oAVG(:,:) / REAL(NADD_out,KIND=JPRB)
END SUBROUTINE CMF_TRACER_DIAG_GETAVE
!####################################################################

END MODULE CMF_CTRL_TRACER_MOD
