MODULE CMF_CTRL_LEVEE_MOD
!==========================================================
!* PURPOSE: CaMa-Flood flood stage with levee scheme
!
! (C) Y. Tanaka & D.Yamazaki (U-Tokyo)  Mar 2022
!
!* CONTAINS:
! -- CMF_LEV_NMLIST      : Read setting from namelist
! -- CMF_LEV_INIT        : Initialize levee scheme
! -- CMF_CALC_FLDSTG_LEV : Calculate flood stage considering levee
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
USE YOS_CMF_INPUT,           ONLY: LOGNAM, IMIS
!============================
IMPLICIT NONE
SAVE
!*** NAMELIST/NLEVEE/
!*** Levee Map
CHARACTER(LEN=256)             ::  CLEVHGT         !! LEVEE HEIGHT from RIVER
CHARACTER(LEN=256)             ::  CLEVFRC         !! Unprotected fraction. Relative Levee distance from RIVER

NAMELIST/NLEVEE/  CLEVHGT, CLEVFRC

!*** Levee Parameters from map
REAL(KIND=JPRB),ALLOCATABLE    ::  B2LEVHGT(:,:)        !! LEVEE HEIGHT [M] (levee croen elevation above elevtn.bin-river elevation)
REAL(KIND=JPRB),ALLOCATABLE    ::  B2LEVFRC(:,:)        !! Unprotected fraction = RELATIVE DISTANCE between LEVEE and RIVER [0-1].
                                                        !!  0 = just aside channel, 1 = edge of catchment

!*** Levee stage parameter (calculated)
REAL(KIND=JPRB),ALLOCATABLE    ::  B2BASHGT(:,:)        !! LEVEE Base height [M] (levee base elevation above elevtn.bin-river elev)
REAL(KIND=JPRB),ALLOCATABLE    ::  B2LEVDST(:,:)        !! Absolute DISTANCE between LEVEE and RIVER [0-1]. 
                                                        !! 0 = just aside channel, 1 = edge of catchment

REAL(KIND=JPRB),ALLOCATABLE    ::  B2LEVBASSTO(:,:)  !! MAXIMUM STORAGE under LEVEE BASE [M3]
REAL(KIND=JPRB),ALLOCATABLE    ::  B2LEVTOPSTO(:,:)  !! MAXIMUM STORAGE at LEVEE TOP [M3] (only river side)
REAL(KIND=JPRB),ALLOCATABLE    ::  B2LEVFILSTO(:,:) !! MAXIMUM STORAGE at LEVEE TOP [M3] (both river & protected side are filled)

CONTAINS
!####################################################################
!* CONTAINS:
! -- CMF_LEVEE_NMLIST         : Read setting from namelist
! -- CMF_LEVEE_INIT           : Initialize dam data
! -- CMF_LEVEE_FLDSTG         : Calculate inflow and outflow at dam
! -- CMF_LEVEE_OPT_PTHOUT     : Bifurcation scheme with levee consideration
!####################################################################
SUBROUTINE CMF_LEVEE_NMLIST
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
WRITE(LOGNAM,*) "CMF::LEVEE_NMLIST: namelist OPEN in unit: ", TRIM(CSETFILE), NSETFILE 

!*** 2. default value
CLEVHGT   ="NONE"
CLEVFRC   ="NONE"

!*** 3. read namelist
REWIND(NSETFILE)
READ(NSETFILE,NML=NLEVEE)

WRITE(LOGNAM,*)   "=== NAMELIST, NLEVEE ==="
WRITE(LOGNAM,*)   "CLEVHGT   : ", CLEVHGT   
WRITE(LOGNAM,*)   "CLEVFRC   : ", CLEVFRC   

CLOSE(NSETFILE)

WRITE(LOGNAM,*) "CMF::LEVEE_NMLIST: end" 

END SUBROUTINE CMF_LEVEE_NMLIST
!####################################################################





!####################################################################
SUBROUTINE CMF_LEVEE_INIT
USE YOS_CMF_INPUT,      ONLY: TMPNAM, NX, NY, NLFP
USE YOS_CMF_MAP,        ONLY: NSEQALL, NSEQMAX
USE YOS_CMF_MAP,        ONLY: B2GRAREA, B2RIVLEN, B2RIVWTH, B2FLDHGT, &
                             & B2FLDGRD, B2RIVSTOMAX, B2FLDSTOMAX, BFRCINC
USE CMF_UTILS_MOD,      ONLY: MAPR2VECB, INQUIRE_FID
!
IMPLICIT NONE
!* local variables
REAL(KIND=JPRM)            ::  R2TEMP(NX,NY)
! SAVE for OpenMP
INTEGER(KIND=JPIM),SAVE    ::  ISEQ, I, ILEV
REAL(KIND=JPRD),SAVE       ::  DSTONOW,DSTOPRE,DHGTPRE,DWTHINC,DWTHPRE,DWTHNOW,DHGTNOW,DHGTDIF
!$OMP THREADPRIVATE    (I,ILEV,DSTONOW,DSTOPRE,DHGTPRE,DWTHINC,DWTHPRE,DWTHNOW,DHGTNOW,DHGTDIF)
!####################################################################
WRITE(LOGNAM,*) ""
WRITE(LOGNAM,*) "!---------------------!"
WRITE(LOGNAM,*) "CMF::LEVEE_INIT: initialize levee"

!********************
! [1] Read Levee Parameter Map
WRITE(LOGNAM,*) "CMF::LEVEE_INIT: read levee parameter files"

ALLOCATE( B2LEVHGT(NSEQMAX,1) )
ALLOCATE( B2LEVFRC(NSEQMAX,1) )
B2LEVHGT(:,:)   =0._JPRB
B2LEVFRC(:,:)   =0._JPRB

TMPNAM=INQUIRE_FID()

WRITE(LOGNAM,*)'INIT_LEVEE: levee crown height : ',TRIM(CLEVHGT)
OPEN(TMPNAM,FILE=CLEVHGT,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4*NX*NY)
READ(TMPNAM,REC=1) R2TEMP(:,:)
CALL MAPR2VECB(R2TEMP,B2LEVHGT)
CLOSE(TMPNAM)

WRITE(LOGNAM,*)'INIT_LEVEE: distance from levee to river : ',TRIM(CLEVFRC)
OPEN(TMPNAM,FILE=CLEVFRC,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4*NX*NY)
READ(TMPNAM,REC=1) R2TEMP(:,:)
CALL MAPR2VECB(R2TEMP,B2LEVFRC)
CLOSE(TMPNAM)

!*******************************
! [2] Calculate Levee Stage Parameter
WRITE(LOGNAM,*) "CMF::LEVEE_INIT: flood stage parameters considering levee"

ALLOCATE( B2BASHGT(NSEQMAX,1) )
ALLOCATE( B2LEVDST(NSEQMAX,1) )

ALLOCATE( B2LEVBASSTO(NSEQMAX,1) )
ALLOCATE( B2LEVTOPSTO(NSEQMAX,1) )
ALLOCATE( B2LEVFILSTO(NSEQMAX,1) )

B2FLDSTOMAX(:,:,:) = 0._JPRB   !! max floodplain  storage  at each layer
B2FLDGRD(:,:,:)    = 0._JPRB   !! floodplain topo gradient of each layer
BFRCINC=dble(NLFP)**(-1.)   !! fration of each layer

B2LEVBASSTO(:,:)= 0._JPRB      !! storage at levee base     (levee protection start)
B2LEVTOPSTO(:,:)= 0._JPRB      !! storage at levee top      (levee protection end)
B2LEVFILSTO(:,:)= 0._JPRB      !! storage when levee filled (protected-side depth reach levee top)

!$OMP PARALLEL DO
DO ISEQ=1, NSEQALL
  IF( B2LEVHGT(ISEQ,1)<=0._JPRB )THEN
    B2LEVHGT(ISEQ,1)=0._JPRB
    B2LEVFRC(ISEQ,1)=1._JPRB   !! If no levee, all area is unprotected/
  ENDIF
  B2LEVFRC(ISEQ,1)=MAX(0._JPRB,MIN(1._JPRB,B2LEVFRC(ISEQ,1)))
END DO
!$OMP END PARALLEL DO

!$OMP PARALLEL DO
DO ISEQ=1, NSEQALL
! calculate floodplain parameters (without levee, same as SET_FLDSTG)
  DSTOPRE = B2RIVSTOMAX(ISEQ,1)
  DHGTPRE = 0._JPRB
  DWTHINC = B2GRAREA(ISEQ,1) * B2RIVLEN(ISEQ,1)**(-1.) * BFRCINC  !! width increlment for each layer
  DO I=1, NLFP
    DSTONOW = B2RIVLEN(ISEQ,1) * ( B2RIVWTH(ISEQ,1) + DWTHINC*(DBLE(I)-0.5) ) * (B2FLDHGT(ISEQ,1,I)-DHGTPRE)  !! storage increment
    B2FLDSTOMAX(ISEQ,1,I) = DSTOPRE + DSTONOW
    B2FLDGRD(ISEQ,1,I) = (B2FLDHGT(ISEQ,1,I)-DHGTPRE) * DWTHINC**(-1.)
    DSTOPRE = B2FLDSTOMAX(ISEQ,1,I)
    DHGTPRE = B2FLDHGT(ISEQ,1,I)
  END DO

! Levee parameters calculation
  IF( B2LEVHGT(ISEQ,1) == 0._JPRB )THEN ! Grid without levee, treat everything as unprotected
    B2BASHGT(ISEQ,1) = 1.E18
    B2LEVDST(ISEQ,1) = 1.E18
    B2LEVBASSTO(ISEQ,1) = 1.E18
    B2LEVTOPSTO(ISEQ,1) = 1.E18
    B2LEVFILSTO(ISEQ,1) = 1.E18
  ELSE  !! levee exist
    !!*********
    !! [1] levee base storage & levee top storage (water only in river side)

    DSTOPRE = B2RIVSTOMAX(ISEQ,1)
    DHGTPRE = 0._JPRB
    DWTHPRE = 0._JPRB
    B2LEVDST(ISEQ,1) = B2LEVFRC(ISEQ,1) * DWTHINC*NLFP !! distance from channel to levee [m]

    ILEV=INT( B2LEVFRC(ISEQ,1)*NLFP )+1 !! which layer levee exist
    IF( ILEV>=2 )THEN
      DSTOPRE = B2FLDSTOMAX(ISEQ,1,ILEV-1)
      DHGTPRE = B2FLDHGT(ISEQ,1,ILEV-1)
      DWTHPRE = DWTHINC * (ILEV-1)
    ENDIF

    IF( ILEV<=NLFP )THEN
      !! levee in floodplain layer ILEV
      DWTHNOW = B2LEVDST(ISEQ,1) - DWTHPRE
      DHGTNOW = DWTHNOW * B2FLDGRD(ISEQ,1,ILEV) !! levee height above lower floodplain profile point
      B2BASHGT(ISEQ,1) = DHGTNOW + DHGTPRE
      B2LEVHGT(ISEQ,1) = max( B2LEVHGT(ISEQ,1), B2BASHGT(ISEQ,1) ) !! levee height >= base height

      DSTONOW = ( DWTHNOW*0.5 + DWTHPRE + B2RIVWTH(ISEQ,1) ) * DHGTNOW * B2RIVLEN(ISEQ,1) 
      B2LEVBASSTO(ISEQ,1) = DSTOPRE + DSTONOW

      DHGTDIF = B2LEVHGT(ISEQ,1) - B2BASHGT(ISEQ,1)
      B2LEVTOPSTO(ISEQ,1) = B2LEVBASSTO(ISEQ,1) + ( B2LEVDST(ISEQ,1)+B2RIVWTH(ISEQ,1) ) * DHGTDIF * B2RIVLEN(ISEQ,1)
    ELSE
      !! levee on the floodplain edge (ILEV=NLEV+1)
      B2BASHGT(ISEQ,1) = DHGTPRE
      B2LEVHGT(ISEQ,1) = max( B2LEVHGT(ISEQ,1), B2BASHGT(ISEQ,1) ) !! levee height >= base height

      B2LEVBASSTO(ISEQ,1) = DSTOPRE

      DHGTDIF = B2LEVHGT(ISEQ,1) - B2BASHGT(ISEQ,1)
      B2LEVTOPSTO(ISEQ,1) = B2LEVBASSTO(ISEQ,1) + ( B2LEVDST(ISEQ,1)+B2RIVWTH(ISEQ,1) ) * DHGTDIF * B2RIVLEN(ISEQ,1)
    ENDIF

    !!*********
    !! [2] levee fill storage (water in both river side & protected side)
    I=1
    DSTOPRE = B2RIVSTOMAX(ISEQ,1)
    DWTHPRE = B2RIVWTH(ISEQ,1)
    DHGTPRE = 0._JPRB

    !! check which layer levee top belongs
    DO WHILE( B2LEVHGT(ISEQ,1) > B2FLDHGT(ISEQ,1,I) .AND. I<=NLFP )
      DSTOPRE = B2FLDSTOMAX(ISEQ,1,I)
      DWTHPRE = DWTHPRE + DWTHINC
      DHGTPRE = B2FLDHGT(ISEQ,1,I)
      I=I+1
      IF( I>NLFP ) EXIT
    END DO

    !! calculate levee fill volume
    IF( I<=NLFP )THEN 
      !! levee top height collesponds to layer I
      DHGTNOW = B2LEVHGT(ISEQ,1) - DHGTPRE
      DWTHNOW = DHGTNOW * B2FLDGRD(ISEQ,1,I)**(-1.)

      DSTONOW = ( DWTHNOW*0.5 + DWTHPRE ) * DHGTNOW * B2RIVLEN(ISEQ,1)
      B2LEVFILSTO(ISEQ,1) = DSTOPRE + DSTONOW
    ELSE
      !! levee higher than catchment boundary height
      DHGTNOW = B2LEVHGT(ISEQ,1) - DHGTPRE
      DSTONOW = DWTHPRE * DHGTNOW * B2RIVLEN(ISEQ,1)
      B2LEVFILSTO(ISEQ,1) = DSTOPRE + DSTONOW
    ENDIF
  ENDIF

END DO
!$OMP END PARALLEL DO

END SUBROUTINE CMF_LEVEE_INIT
!####################################################################




!####################################################################
SUBROUTINE CMF_LEVEE_FLDSTG
! ================================================
! calculate river and floodplain staging considering levee
! ================================================
USE YOS_CMF_INPUT  ,ONLY: NLFP
USE YOS_CMF_MAP    ,ONLY: NSEQALL
USE YOS_CMF_MAP    ,ONLY: B2GRAREA, B2RIVLEN, B2RIVWTH, B2RIVELV, B2RIVSTOMAX, B2FLDSTOMAX, B2FLDGRD, BFRCINC, B2FLDHGT
USE YOS_CMF_PROG   ,ONLY: D2RIVSTO, D2FLDSTO
USE YOS_CMF_DIAG   ,ONLY: B2RIVDPH, B2FLDDPH, B2FLDFRC, B2FLBARE, B2SFCELV
USE YOS_CMF_DIAG   ,ONLY: DGLBSTOPRE2, DGLBSTONEW2, DGLBRIVSTO, DGLBFLDSTO, DGLBLEVSTO, DGLBFLBARE

!! levee specific data
USE YOS_CMF_PROG   ,ONLY: D2LEVSTO  !! flood storage in protected side (D2FLDSTO for storage betwen river & levee)
USE YOS_CMF_DIAG   ,ONLY: B2LEVDPH  !! flood depth in protected side   (B2FLDDPH for water depth betwen river & levee)
IMPLICIT NONE

!*** LOCAL
! Save for OpenMP
INTEGER(KIND=JPIM),SAVE ::  ISEQ, I, ILEV
REAL(KIND=JPRD),SAVE    ::  BSTOALL, DSTONOW, DSTOPRE, DWTHNOW, DWTHPRE, DDPHPRE, DDPHNOW, DWTHINC, BSTOADD
!$OMP THREADPRIVATE (I,ILEV,BSTOALL, DSTONOW, DSTOPRE, DWTHNOW, DWTHPRE, DDPHPRE, DDPHNOW, DWTHINC, BSTOADD)
!!==============================
DGLBSTOPRE2=0._JPRD
DGLBSTONEW2=0._JPRD
DGLBRIVSTO =0._JPRD
DGLBFLDSTO =0._JPRD
DGLBLEVSTO =0._JPRD
DGLBFLBARE =0._JPRD

!$OMP PARALLEL DO REDUCTION(+:DGLBSTOPRE2,DGLBSTONEW2,DGLBRIVSTO,DGLBFLDSTO,DGLBLEVSTO,DGLBFLBARE)
DO ISEQ=1, NSEQALL
!
  BSTOALL = D2RIVSTO(ISEQ,1) + D2FLDSTO(ISEQ,1) + D2LEVSTO(ISEQ,1)
  DWTHINC = B2GRAREA(ISEQ,1) * B2RIVLEN(ISEQ,1)**(-1.) * BFRCINC    !! width of each layer [m]
  IF( BSTOALL > B2RIVSTOMAX(ISEQ,1) )THEN
    !**********
    ! [Case-1] Water surface is under levee base (all water is between river-levee)
    IF( BSTOALL < B2LEVBASSTO(ISEQ,1) )THEN 
      I=1
      DSTOPRE = B2RIVSTOMAX(ISEQ,1)
      DWTHPRE = B2RIVWTH(ISEQ,1)
      DDPHPRE = 0._JPRB

      ! which layer current water level is
      DO WHILE( BSTOALL > B2FLDSTOMAX(ISEQ,1,I) .AND. I<=NLFP )
        DSTOPRE = B2FLDSTOMAX(ISEQ,1,I)
        DWTHPRE = DWTHPRE + DWTHINC
        DDPHPRE = DDPHPRE + B2FLDGRD(ISEQ,1,I) * DWTHINC
        I=I+1
        IF( I>NLFP ) EXIT
      END DO

      ! water depth at unprotected area
      IF( I<=NLFP )THEN
        DSTONOW =  BSTOALL - DSTOPRE
        DWTHNOW = -DWTHPRE + ( DWTHPRE**2. + 2. * DSTONOW * B2RIVLEN(ISEQ,1)**(-1.) * B2FLDGRD(ISEQ,1,I)**(-1.) )**0.5
        B2FLDDPH(ISEQ,1) = DDPHPRE + B2FLDGRD(ISEQ,1,I) * DWTHNOW
      ELSE
        DSTONOW = BSTOALL - DSTOPRE
        DWTHNOW = 0._JPRB
        B2FLDDPH(ISEQ,1) = DDPHPRE + DSTONOW * DWTHPRE**(-1.) * B2RIVLEN(ISEQ,1)**(-1.)
      ENDIF

      D2RIVSTO(ISEQ,1) = B2RIVSTOMAX(ISEQ,1) + B2RIVLEN(ISEQ,1) * B2RIVWTH(ISEQ,1) * B2FLDDPH(ISEQ,1)
      B2RIVDPH(ISEQ,1) = D2RIVSTO(ISEQ,1) * B2RIVLEN(ISEQ,1)**(-1.) * B2RIVWTH(ISEQ,1)**(-1.)
!
      D2FLDSTO(ISEQ,1) = BSTOALL - D2RIVSTO(ISEQ,1)
      D2FLDSTO(ISEQ,1) = MAX( D2FLDSTO(ISEQ,1), 0._JPRD )
      B2FLDFRC(ISEQ,1) = (-B2RIVWTH(ISEQ,1) + DWTHPRE + DWTHNOW ) * (DWTHINC*NLFP)**(-1.)
      B2FLDFRC(ISEQ,1) = MAX( B2FLDFRC(ISEQ,1),0._JPRB )
      B2FLDFRC(ISEQ,1) = MIN( B2FLDFRC(ISEQ,1),1._JPRB )
      B2FLBARE(ISEQ,1) = B2GRAREA(ISEQ,1)*B2FLDFRC(ISEQ,1)
!
      D2LEVSTO(ISEQ,1) = 0._JPRD  !! no flooding in protected area
      B2LEVDPH(ISEQ,1) = 0._JPRB

    !**********
    ! [Case-2]  River-side water surface is under levee crown (water only in river side)
    ELSEIF( BSTOALL < B2LEVTOPSTO(ISEQ,1) )THEN 

      DSTONOW = BSTOALL - B2LEVBASSTO(ISEQ,1)
      DWTHNOW = B2LEVDST(ISEQ,1) + B2RIVWTH(ISEQ,1)
      B2FLDDPH(ISEQ,1) = B2BASHGT(ISEQ,1) + DSTONOW * DWTHNOW**(-1.) * B2RIVLEN(ISEQ,1)**(-1.)

      D2RIVSTO(ISEQ,1) = B2RIVSTOMAX(ISEQ,1) + B2RIVLEN(ISEQ,1) * B2RIVWTH(ISEQ,1) * B2FLDDPH(ISEQ,1)
      B2RIVDPH(ISEQ,1) = D2RIVSTO(ISEQ,1) * B2RIVLEN(ISEQ,1)**(-1.) * B2RIVWTH(ISEQ,1)**(-1.)
  !
      D2FLDSTO(ISEQ,1) = BSTOALL - D2RIVSTO(ISEQ,1)
      D2FLDSTO(ISEQ,1) = MAX( D2FLDSTO(ISEQ,1), 0._JPRD )
      B2FLDFRC(ISEQ,1) = B2LEVFRC(ISEQ,1)
      B2FLBARE(ISEQ,1) = B2GRAREA(ISEQ,1)*B2FLDFRC(ISEQ,1)
  ! 
      D2LEVSTO(ISEQ,1) = 0._JPRD  !! no flooding in protected area
      B2LEVDPH(ISEQ,1) = 0._JPRB

    !**********
    ! [Case-3] River side is full, protected side is under levee crown height (Water both in river side & protected side)
    ELSEIF( BSTOALL < B2LEVFILSTO(ISEQ,1) )THEN 
      ! river side stage = levee height
      B2FLDDPH(ISEQ,1) = B2LEVHGT(ISEQ,1)
      D2RIVSTO(ISEQ,1) = B2RIVSTOMAX(ISEQ,1) + B2RIVLEN(ISEQ,1) * B2RIVWTH(ISEQ,1) * B2FLDDPH(ISEQ,1)
      B2RIVDPH(ISEQ,1) = D2RIVSTO(ISEQ,1) * B2RIVLEN(ISEQ,1)**(-1.) * B2RIVWTH(ISEQ,1)**(-1.)

      D2FLDSTO(ISEQ,1) = B2LEVTOPSTO(ISEQ,1) - D2RIVSTO(ISEQ,1)
      D2FLDSTO(ISEQ,1) = MAX( D2FLDSTO(ISEQ,1), 0._JPRD )

      !! protected side storate calculation
      D2LEVSTO(ISEQ,1) = BSTOALL - D2RIVSTO(ISEQ,1) - D2FLDSTO(ISEQ,1)
      D2LEVSTO(ISEQ,1) = MAX( D2LEVSTO(ISEQ,1), 0._JPRD )

      !!****
      !! protected side stage calculation
      ILEV=INT( B2LEVFRC(ISEQ,1)*NLFP )+1 !! levee relative distance -> floodplain layer with levee
      DSTOPRE = B2LEVTOPSTO(ISEQ,1)
      DWTHPRE = 0._JPRB
      DDPHPRE = 0._JPRB
      !! which layer current water level is
      I=ILEV
      DO WHILE( I<=NLFP )
        BSTOADD = ( B2LEVDST(ISEQ,1)+B2RIVWTH(ISEQ,1) ) * ( B2LEVHGT(ISEQ,1)-B2FLDHGT(ISEQ,1,I) ) * B2RIVLEN(ISEQ,1) 
        IF( BSTOALL < B2FLDSTOMAX(ISEQ,1,I) + BSTOADD ) EXIT
        DSTOPRE = B2FLDSTOMAX(ISEQ,1,I) + BSTOADD
        DWTHPRE = DWTHINC*I - B2LEVDST(ISEQ,1)
        DDPHPRE = B2FLDHGT(ISEQ,1,I) - B2BASHGT(ISEQ,1)
        I=I+1
        IF( I>NLFP ) EXIT
      END DO

      IF( I<=NLFP )THEN
        DSTONOW = BSTOALL - DSTOPRE
        DWTHNOW = -DWTHPRE + ( DWTHPRE**2. + 2. * DSTONOW*B2RIVLEN(ISEQ,1)**(-1.) * B2FLDGRD(ISEQ,1,I)**(-1.) )**0.5
        DDPHNOW = DWTHNOW * B2FLDGRD(ISEQ,1,I)
        B2LEVDPH(ISEQ,1) = B2BASHGT(ISEQ,1) + DDPHPRE + DDPHNOW

        B2FLDFRC(ISEQ,1) = ( DWTHPRE + B2LEVDST(ISEQ,1) ) * (DWTHINC*NLFP)**(-1.)
        B2FLDFRC(ISEQ,1) = MAX( B2FLDFRC(ISEQ,1),0._JPRB)
        B2FLDFRC(ISEQ,1) = MIN( B2FLDFRC(ISEQ,1),1._JPRB)
        B2FLBARE(ISEQ,1) = B2GRAREA(ISEQ,1)*B2FLDFRC(ISEQ,1)
      ELSE
        DSTONOW = BSTOALL - DSTOPRE
        DDPHNOW = DSTONOW * DWTHPRE**(-1.) * B2RIVLEN(ISEQ,1)**(-1.)
        B2LEVDPH(ISEQ,1) = B2BASHGT(ISEQ,1) + DDPHPRE + DDPHNOW

        B2FLDFRC(ISEQ,1) = 1._JPRB
        B2FLBARE(ISEQ,1) = B2GRAREA(ISEQ,1)*B2FLDFRC(ISEQ,1)
      ENDIF

    !**********
    ! [Case-4] Water level above levee crown (Both river side and protected side exceed levee crown height)
    ELSE 
      I=1
      DSTOPRE = B2RIVSTOMAX(ISEQ,1)
      DWTHPRE = B2RIVWTH(ISEQ,1)
      DDPHPRE = 0._JPRB
      DO WHILE( BSTOALL > B2FLDSTOMAX(ISEQ,1,I) .AND. I<=NLFP)
        DSTOPRE = B2FLDSTOMAX(ISEQ,1,I)
        DWTHPRE = DWTHPRE + DWTHINC
        DDPHPRE = DDPHPRE + B2FLDGRD(ISEQ,1,I) * DWTHINC
        I=I+1
        IF( I>NLFP ) EXIT
      END DO

      IF( I<=NLFP )THEN
        DSTONOW =  BSTOALL - DSTOPRE
        DWTHNOW = -DWTHPRE + ( DWTHPRE**2. + 2. * DSTONOW * B2RIVLEN(ISEQ,1)**(-1.) * B2FLDGRD(ISEQ,1,I)**(-1.) )**0.5
        B2FLDDPH(ISEQ,1) = DDPHPRE + B2FLDGRD(ISEQ,1,I) * DWTHNOW
      ELSE
        DSTONOW = BSTOALL - DSTOPRE
        DWTHNOW = 0._JPRB
        B2FLDDPH(ISEQ,1) = DDPHPRE + DSTONOW * DWTHPRE**(-1.) * B2RIVLEN(ISEQ,1)**(-1.)
      ENDIF

      B2FLDFRC(ISEQ,1) = (-B2RIVWTH(ISEQ,1) + DWTHPRE + DWTHNOW ) * (DWTHINC*NLFP)**(-1.)
      B2FLBARE(ISEQ,1) = B2GRAREA(ISEQ,1)*B2FLDFRC(ISEQ,1)

      !! river channel storage
      D2RIVSTO(ISEQ,1) = B2RIVSTOMAX(ISEQ,1) + B2RIVLEN(ISEQ,1) * B2RIVWTH(ISEQ,1) * B2FLDDPH(ISEQ,1)
      B2RIVDPH(ISEQ,1) = D2RIVSTO(ISEQ,1) * B2RIVLEN(ISEQ,1)**(-1.) * B2RIVWTH(ISEQ,1)**(-1.)
!
      BSTOADD = ( B2FLDDPH(ISEQ,1)-B2LEVHGT(ISEQ,1) ) * (B2LEVDST(ISEQ,1)+B2RIVWTH(ISEQ,1)) * B2RIVLEN(ISEQ,1)
      D2FLDSTO(ISEQ,1) = B2LEVTOPSTO(ISEQ,1) + BSTOADD - D2RIVSTO(ISEQ,1)
      D2FLDSTO(ISEQ,1) = MAX( D2FLDSTO(ISEQ,1), 0._JPRD )

      D2LEVSTO(ISEQ,1) = BSTOALL - D2RIVSTO(ISEQ,1) - D2FLDSTO(ISEQ,1)
      D2LEVSTO(ISEQ,1) = MAX( D2LEVSTO(ISEQ,1), 0._JPRD )
      B2LEVDPH(ISEQ,1) = B2FLDDPH(ISEQ,1)
    ENDIF

  ! [Case-0] Water only in river channel
  ELSE
    D2RIVSTO(ISEQ,1) = BSTOALL
    B2RIVDPH(ISEQ,1) = BSTOALL * B2RIVLEN(ISEQ,1)**(-1.) * B2RIVWTH(ISEQ,1)**(-1.)
    B2RIVDPH(ISEQ,1) = MAX( B2RIVDPH(ISEQ,1), 0._JPRB )
    D2FLDSTO(ISEQ,1) = 0._JPRD
    B2FLDDPH(ISEQ,1) = 0._JPRB
    B2FLDFRC(ISEQ,1) = 0._JPRB
    B2FLBARE(ISEQ,1) = 0._JPRB
    D2LEVSTO(ISEQ,1) = 0._JPRD
    B2LEVDPH(ISEQ,1) = 0._JPRB
  ENDIF
  B2SFCELV(ISEQ,1)     = B2RIVELV(ISEQ,1) + B2RIVDPH(ISEQ,1)

  DGLBSTOPRE2     = DGLBSTOPRE2 + BSTOALL
  DGLBSTONEW2     = DGLBSTONEW2 + D2RIVSTO(ISEQ,1) + D2FLDSTO(ISEQ,1) + D2LEVSTO(ISEQ,1)
  DGLBRIVSTO      = DGLBRIVSTO  + D2RIVSTO(ISEQ,1)
  DGLBFLDSTO      = DGLBFLDSTO  + D2FLDSTO(ISEQ,1)
  DGLBLEVSTO      = DGLBLEVSTO  + D2LEVSTO(ISEQ,1)
  DGLBFLBARE      = DGLBFLBARE  + B2FLBARE(ISEQ,1)
END DO
!$OMP END PARALLEL DO

END SUBROUTINE CMF_LEVEE_FLDSTG
!####################################################################




!####################################################################
SUBROUTINE CMF_LEVEE_OPT_PTHOUT
! realistic bifurcation considering levee
USE PARKIND1,           ONLY: JPIM, JPRB
USE YOS_CMF_INPUT,      ONLY: DT, PGRV, BMIS
USE YOS_CMF_MAP,        ONLY: NSEQALL, NSEQMAX, NPTHOUT, NPTHLEV, PTH_UPST, PTH_DOWN, PTH_DST, &
                            & PTH_ELV, PTH_WTH, PTH_MAN, I2MASK
USE YOS_CMF_MAP,        ONLY: B2ELEVTN, B2RIVELV
USE YOS_CMF_PROG,       ONLY: D2RIVSTO, D2FLDSTO, B1PTHFLW, B2RIVOUT, B2FLBOUT
USE YOS_CMF_PROG,       ONLY: B1PTHFLW_PRE, B2RIVDPH_PRE
USE YOS_CMF_DIAG,       ONLY: B2PTHOUT, B2PTHINF, B2RIVINF, B2LEVDPH, B2FLDINF, B2SFCELV
IMPLICIT NONE
!*** Local
REAL(KIND=JPRB)    ::  B2SFCELV_LEV(NSEQMAX,1)                  !! water surface elev protected [m]

REAL(KIND=JPRB)    ::  B2SFCELV_PRE(NSEQMAX,1)                  !! water surface elev (t-1) [m] (for stable calculation)
REAL(KIND=JPRB)    ::  B2RATE(NSEQMAX,1)                        !! outflow correction

! SAVE for OpenMP
INTEGER(KIND=JPIM),SAVE ::  IPTH, ILEV, ISEQ, ISEQP, JSEQP
REAL(KIND=JPRB),SAVE    ::  BSLOPE, BFLW, BOUT_PRE, BFLW_PRE, BFLW_IMP, BSTO_TMP
!$OMP THREADPRIVATE         (BSLOPE, BFLW, BOUT_PRE, BFLW_PRE, BFLW_IMP, BSTO_TMP, ILEV, ISEQP, JSEQP)
!================================================
!$OMP PARALLEL DO
DO ISEQ=1, NSEQALL
  IF( B2LEVFRC(ISEQ,1)<1.0 )THEN
    B2SFCELV_LEV(ISEQ,1) = B2ELEVTN(ISEQ,1)+B2LEVDPH(ISEQ,1)  !! levee exist, calculate pthout based on levee protected depth
  ELSE
    B2SFCELV_LEV(ISEQ,1) = B2SFCELV(ISEQ,1)
  ENDIF

  B2SFCELV_PRE(ISEQ,1) = B2RIVELV(ISEQ,1)+B2RIVDPH_PRE(ISEQ,1)
  B2PTHOUT(ISEQ,1) = 0._JPRB
  B2PTHINF(ISEQ,1) = 0._JPRB
  B2RATE(ISEQ,1)   =-999._JPRB
END DO
!$OMP END PARALLEL DO

B1PTHFLW(:,:) = BMIS
!$OMP PARALLEL DO
DO IPTH=1, NPTHOUT  
  ISEQP=PTH_UPST(IPTH)
  JSEQP=PTH_DOWN(IPTH)
  !! Avoid calculation outside of domain
  IF (ISEQP == 0 .OR. JSEQP== 0 ) CYCLE
  IF (I2MASK(ISEQP,1) == 1 .OR. I2MASK(JSEQP,1) == 1 ) CYCLE  !! I2MASK is for kinematic-inertial mixed flow scheme. 

!! [1] for channel bifurcation, use river surface elevation  
  BSLOPE  = (B2SFCELV(ISEQP,1)-B2SFCELV(JSEQP,1)) * PTH_DST(IPTH)**(-1.)
  BSLOPE = max(-0.005_JPRB,min(0.005_JPRB,BSLOPE))                                    !! v390 stabilization

  ILEV=1 !! for river channek
    BFLW = MAX(B2SFCELV(ISEQP,1),B2SFCELV(JSEQP,1)) - PTH_ELV(IPTH,ILEV) 
    BFLW = MAX(BFLW,0._JPRB)

    BFLW_PRE = MAX(B2SFCELV_PRE(ISEQP,1),B2SFCELV_PRE(JSEQP,1)) - PTH_ELV(IPTH,ILEV)
    BFLW_PRE = MAX(BFLW_PRE,0._JPRB)

    BFLW_IMP = (BFLW*BFLW_PRE)**0.5                                       !! semi implicit flow depth
    IF( BFLW_IMP<=0._JPRB ) BFLW_IMP=BFLW

    IF( BFLW_IMP>1.E-5 )THEN                         !! local inertial equation, see [Bates et al., 2010, J.Hydrol.]
      BOUT_PRE = B1PTHFLW_PRE(IPTH,ILEV) * PTH_WTH(IPTH,ILEV)**(-1.)           !! outflow (t-1) [m2/s] (unit width)
      B1PTHFLW(IPTH,ILEV) = PTH_WTH(IPTH,ILEV) * ( BOUT_PRE + PGRV*DT*BFLW_IMP*BSLOPE ) &
                         * ( 1. + PGRV*DT*PTH_MAN(ILEV)**2. * abs(BOUT_PRE)*BFLW_IMP**(-7./3.) )**(-1.)
    ELSE
      B1PTHFLW(IPTH,ILEV) = 0._JPRB
    ENDIF

!! [1] for overland bifurcation, use levee protected surface elevation
  IF( NPTHLEV<=1 ) CYCLE

  BSLOPE  = (B2SFCELV_LEV(ISEQP,1)-B2SFCELV_LEV(JSEQP,1)) * PTH_DST(IPTH)**(-1.)
  BSLOPE = max(-0.005_JPRB,min(0.005_JPRB,BSLOPE))      

  DO ILEV=2, NPTHLEV
    BFLW = MAX(B2SFCELV_LEV(ISEQP,1),B2SFCELV_LEV(JSEQP,1)) - PTH_ELV(IPTH,ILEV) 
    BFLW = MAX(BFLW,0._JPRB)

    BFLW_IMP=BFLW  !! do not consider implicit flow depth for overland bifurcation
    IF( BFLW_IMP>1.E-5 )THEN                         !! local inertial equation, see [Bates et al., 2010, J.Hydrol.]
      BOUT_PRE = B1PTHFLW_PRE(IPTH,ILEV) * PTH_WTH(IPTH,ILEV)**(-1.)            !! outflow (t-1) [m2/s] (unit width)
      B1PTHFLW(IPTH,ILEV) = PTH_WTH(IPTH,ILEV) * ( BOUT_PRE + PGRV*DT*BFLW_IMP*BSLOPE ) &
                         * ( 1. + PGRV*DT*PTH_MAN(ILEV)**2. * abs(BOUT_PRE)*BFLW_IMP**(-7./3.) )**(-1.)
    ELSE
      B1PTHFLW(IPTH,ILEV) = 0._JPRB
    ENDIF
  END DO
END DO
!$OMP END PARALLEL DO

#ifndef NoAtom_CMF
!$OMP PARALLEL DO  !! No OMP Atomic for bit-identical simulation (set in Mkinclude)
#endif
DO IPTH=1, NPTHOUT  
  ISEQP=PTH_UPST(IPTH)
  JSEQP=PTH_DOWN(IPTH)
  !! Avoid calculation outside of domain
  IF (ISEQP == 0 .OR. JSEQP== 0 ) CYCLE
  IF (I2MASK(ISEQP,1) == 1 .OR. I2MASK(JSEQP,1) == 1 ) CYCLE

  DO ILEV=1, NPTHLEV
    IF( B1PTHFLW(IPTH,ILEV) >= 0._JPRB )THEN                                  !! total outflow from each grid
!$OMP ATOMIC
      B2PTHOUT(ISEQP,1) = B2PTHOUT(ISEQP,1) + B1PTHFLW(IPTH,ILEV)
    ELSE
!$OMP ATOMIC
      B2PTHOUT(JSEQP,1) = B2PTHOUT(JSEQP,1) - B1PTHFLW(IPTH,ILEV)
    ENDIF
  END DO
END DO
#ifndef NoAtom_CMF
!$OMP END PARALLEL DO  !! No OMP Atomic for bit-identical simulation (set in Mkinclude)
#endif

!$OMP PARALLEL DO                                              !! calculate total outflow from a grid
DO ISEQ=1, NSEQALL
  IF( B2PTHOUT(ISEQ,1) > 1.E-10 )THEN
    BSTO_TMP = ( D2RIVSTO(ISEQ,1)+D2FLDSTO(ISEQ,1) ) &
                  - B2RIVOUT(ISEQ,1)*DT + B2RIVINF(ISEQ,1)*DT - B2FLBOUT(ISEQ,1)*DT + B2FLDINF(ISEQ,1)*DT
    B2RATE(ISEQ,1) = MIN( BSTO_TMP * (B2PTHOUT(ISEQ,1)*DT)**(-1.), 1._JPRB )
  ELSE
    B2RATE(ISEQ,1) = 1._JPRB
  ENDIF
  B2PTHOUT(ISEQ,1) = B2PTHOUT(ISEQ,1) * B2RATE(ISEQ,1)
END DO
!$OMP END PARALLEL DO

#ifndef NoAtom_CMF
!$OMP PARALLEL DO  !! No OMP Atomic for bit-identical simulation (set in Mkinclude)
#endif
DO IPTH=1, NPTHOUT
  ISEQP=PTH_UPST(IPTH)
  JSEQP=PTH_DOWN(IPTH)
  !! Avoid calculation outside of domain
  IF (ISEQP == 0 .OR. JSEQP== 0 ) CYCLE
  IF (I2MASK(ISEQP,1) == 1 .OR. I2MASK(JSEQP,1) == 1 ) CYCLE
  
  DO ILEV=1, NPTHLEV
    IF( B1PTHFLW(IPTH,ILEV) >= 0._JPRB )THEN
      B1PTHFLW(IPTH,ILEV) = B1PTHFLW(IPTH,ILEV)*B2RATE(ISEQP,1)
!$OMP ATOMIC
      B2PTHINF(JSEQP,1) = B2PTHINF(JSEQP,1) + B1PTHFLW(IPTH,ILEV)             !! total inflow [m3/s] (from upstream)
    ELSE
      B1PTHFLW(IPTH,ILEV) = B1PTHFLW(IPTH,ILEV)*B2RATE(JSEQP,1)
!$OMP ATOMIC
      B2PTHINF(ISEQP,1) = B2PTHINF(ISEQP,1) - B1PTHFLW(IPTH,ILEV)             !! total inflow [m3/s] (from upstream)
    ENDIF
    B1PTHFLW_PRE(IPTH,ILEV)=B1PTHFLW(IPTH,ILEV)
  END DO
END DO
#ifndef NoAtom_CMF
!$OMP END PARALLEL DO  !! No OMP Atomic for bit-identical simulation (set in Mkinclude)
#endif

END SUBROUTINE CMF_LEVEE_OPT_PTHOUT
!################################################################

END MODULE CMF_CTRL_LEVEE_MOD
