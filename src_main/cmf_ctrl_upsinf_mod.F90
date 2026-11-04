MODULE CMF_CTRL_UPSINF_MOD
!==========================================================
!* PURPOSE: CaMa-Flood upstream inflow scheme
!
! (C) D.Yamazaki (U-Tokyo) March 2026
!
!* CONTAINS:
! -- CMF_
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
USE CMF_UTILS_MOD,           ONLY: INQUIRE_FID
USE YOS_CMF_INPUT,           ONLY: LOGNAM, LUPSINF
USE YOS_CMF_MAP,             ONLY: I2VECTOR, i1next
USE YOS_CMF_PROG,            ONLY: D2UPSINF
USE YOS_CMF_TIME,            ONLY: IYYYY, IMM, IDD, IHOUR, IMIN
!============================
IMPLICIT NONE
SAVE
!*** NAMELIST/NUPSINF/
CHARACTER(LEN=256)              :: CINFCSV     !! Upstream Inflow CSV file
INTEGER(KIND=JPIM)              :: IFRQ_INF    !! Upstream Inflow frequency [hour]
NAMELIST/NUPSINF/   CINFCSV, IFRQ_INF

!*** upsinf parameters
INTEGER(KIND=JPIM)              :: NINFFILE
INTEGER(KIND=JPIM)              :: IINF, NINF  !! number of inflow points

INTEGER(KIND=JPIM),ALLOCATABLE  :: INF_X(:)    !! Inflow X
INTEGER(KIND=JPIM),ALLOCATABLE  :: INF_Y(:)    !! Inflow Y
INTEGER(KIND=JPIM),ALLOCATABLE  :: INF_SEQ(:)  !! Inflow SEQ
REAL(KIND=JPRB),ALLOCATABLE     :: INF_Q(:)    !! Inflow Q

CONTAINS
!####################################################################
!* CONTAINS:
! -- CMF_
!####################################################################
SUBROUTINE CMF_UPSINF_NMLIST
! reed setting from namelist
! -- Called from CMF_DRV_NMLIST
USE YOS_CMF_INPUT,      ONLY: CSETFILE,NSETFILE,LUPSINF
IMPLICIT NONE
!================================================
WRITE(LOGNAM,*) ""
WRITE(LOGNAM,*) "!---------------------!"

!*** 1. open namelist
NSETFILE=INQUIRE_FID()
OPEN(NSETFILE,FILE=CSETFILE,STATUS="OLD")
WRITE(LOGNAM,*) "CMF::UPSINF_NMLIST: namelist OPEN in unit: ", TRIM(CSETFILE), NSETFILE 

!*** 2. default value
CINFCSV="./mekong_inflow.csv"
IFRQ_INF=1

!*** 3. read namelist
REWIND(NSETFILE)
READ(NSETFILE,NML=NUPSINF)

IF( LUPSINF )THEN
  WRITE(LOGNAM,*)   "=== NAMELIST, NUPSINF ==="
  WRITE(LOGNAM,*)   "CINFCSV:   " , CINFCSV
  WRITE(LOGNAM,*)   "IFRQ_INF: " , IFRQ_INF
ENDIF

CLOSE(NSETFILE)

WRITE(LOGNAM,*) "CMF::UPSINF_NMLIST: end" 

END SUBROUTINE CMF_UPSINF_NMLIST
!####################################################################
!
!
!
!####################################################################
SUBROUTINE CMF_UPSINF_INIT
! reed setting from CINFCSV
IMPLICIT NONE
INTEGER(KIND=JPIM)         :: IOS
CHARACTER(LEN=1024)        :: LINE
INTEGER(KIND=JPIM)         :: I

CHARACTER(LEN=256)         :: BUF 
INTEGER(KIND=JPIM)         :: IX, IY, ISEQ
!####################################################################
WRITE(LOGNAM,*) ""
WRITE(LOGNAM,*) "!---------------------!"
WRITE(LOGNAM,*) "CMF::UPSINF_INIT: initialize upstream inflow", trim(CINFCSV)

!==========
NINFFILE=INQUIRE_FID()
OPEN(NINFFILE,FILE=CINFCSV,STATUS="OLD", IOSTAT=IOS)

READ(NINFFILE,'(A)',IOSTAT=IOS) LINE 
IF (IOS/=0) STOP 'ERROR reading header line'

LINE = ADJUSTL(TRIM(LINE))
NINF=0
DO i = 1, LEN_TRIM(line)
  IF (line(i:i) == ',') NINF = NINF + 1
END DO
WRITE(LOGNAM,*) "CMF::UPSINF_INIT: number of inflow points:", NINF 

allocate(INF_X(NINF),INF_Y(NINF),INF_SEQ(NINF),INF_Q(NINF))

READ(NINFFILE,*) BUF, INF_X(1:NINF)
READ(NINFFILE,*) BUF, INF_Y(1:NINF)
READ(NINFFILE,*)        !! skip lat
READ(NINFFILE,*)        !! skip lon
READ(NINFFILE,*)        !! skip uparea

DO IINF=1, NINF
  IX=INF_X(IINF)
  IY=INF_Y(IINF)
  ISEQ=I2VECTOR(IX,IY)
  INF_SEQ(IINF)=ISEQ
  WRITE(LOGNAM,*) 'InfPoint', IINF, IX, IY, ISEQ, i1next(ISEQ)
END DO

END SUBROUTINE CMF_UPSINF_INIT
!####################################################################
!
!
!
!####################################################################
SUBROUTINE CMF_UPSINF_UPDATE
! update inflow data
IMPLICIT NONE
INTEGER(KIND=JPIM)         :: IOS, ISEQ
CHARACTER(LEN=256)         :: CINFTIME
INTEGER(KIND=JPIM)         :: InYYYY, InMM, InDD, InHOUR, InMIN
!####################################################################
IF( IMIN==0 .and. MOD(IHOUR,IFRQ_INF)==0 )THEN
  DO
    READ(NINFFILE,*,IOSTAT=IOS) CINFTIME, INF_Q(1:NINF)
    READ(CINFTIME,'(i4,x,i2,x,i2,x,i2,x,i2)') InYYYY, InMM, InDD, InHOUR, InMIN
    IF( InYYYY*10000+InMM*100+InDD < IYYYY*10000+IMM*100+IDD ) CYCLE  !! skip until current time
    IF( InHour*100 + InMIN         < IHour*100 + IMIN        ) CYCLE
    EXIT
  END DO
  WRITE(LOGNAM,*) "CMF::UPSINF_UPSATE", InYYYY, InMM, InDD, InHOUR, InMIN, INF_Q(1)
  
  DO IINF=1, NINF
    ISEQ=INF_SEQ(IINF)
    D2UPSINF(ISEQ,1)=INF_Q(IINF)
  END DO
ENDIF

END SUBROUTINE CMF_UPSINF_UPDATE
!####################################################################
!
!
!
!####################################################################
SUBROUTINE CMF_UPSINF_END
! close file
!####################################################################
CLOSE(NINFFILE)

END SUBROUTINE CMF_UPSINF_END
!####################################################################
!
!####################################################################
END MODULE CMF_CTRL_UPSINF_MOD
