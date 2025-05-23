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
REAL(KIND=JPRB),ALLOCATABLE    ::  D2LEVHGT(:,:)        !! LEVEE HEIGHT [M] (levee croen elevation above elevtn.bin-river elevation)
REAL(KIND=JPRB),ALLOCATABLE    ::  D2LEVFRC(:,:)        !! Unprotected fraction = RELATIVE DISTANCE between LEVEE and RIVER [0-1].
                                                        !!  0 = just aside channel, 1 = edge of catchment

!*** Levee stage parameter (calculated)
REAL(KIND=JPRB),ALLOCATABLE    ::  D2BASHGT(:,:)        !! LEVEE Base height [M] (levee base elevation above elevtn.bin-river elev)
REAL(KIND=JPRB),ALLOCATABLE    ::  D2LEVDST(:,:)        !! Absolute DISTANCE between LEVEE and RIVER [0-1]. 
                                                        !! 0 = just aside channel, 1 = edge of catchment

REAL(KIND=JPRB),ALLOCATABLE    ::  D2LEVBASSTO(:,:)  !! MAXIMUM STORAGE under LEVEE BASE [M3]
REAL(KIND=JPRB),ALLOCATABLE    ::  D2LEVTOPSTO(:,:)  !! MAXIMUM STORAGE at LEVEE TOP [M3] (only river side)
REAL(KIND=JPRB),ALLOCATABLE    ::  D2LEVFILSTO(:,:) !! MAXIMUM STORAGE at LEVEE TOP [M3] (both river & protected side are filled)

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
USE YOS_CMF_MAP,        ONLY: NSEQMAX, NSEQALL
USE YOS_CMF_MAP,        ONLY: D2GRAREA, D2RIVLEN, D2RIVWTH, D2FLDHGT, &
                             & D2FLDGRD, D2RIVSTOMAX, D2FLDSTOMAX, DFRCINC
USE CMF_UTILS_MOD,      ONLY: mapR2vecD, INQUIRE_FID
!
IMPLICIT NONE
!* local variables
REAL(KIND=JPRM)            ::  R2TEMP(NX,NY)
! SAVE for OpenMP
INTEGER(KIND=JPIM),SAVE    ::  ISEQ, I, ILEV
REAL(KIND=JPRB),SAVE       ::  DSTO_add,DSTO_fil,DHGTPRE,DWTH_inc,DWTH_fil,DWTH_add,DHGTNOW,DHGTDIF
!$OMP THREADPRIVATE    (I,ILEV,DSTO_add,DSTO_fil,DHGTPRE,DWTH_inc,DWTH_fil,DWTH_add,DHGTNOW,DHGTDIF)
!####################################################################
WRITE(LOGNAM,*) ""
WRITE(LOGNAM,*) "!---------------------!"
WRITE(LOGNAM,*) "CMF::LEVEE_INIT: initialize levee"

!********************
! [1] Read Levee Parameter Map
WRITE(LOGNAM,*) "CMF::LEVEE_INIT: read levee parameter files"

ALLOCATE( D2LEVHGT(NSEQMAX,1) )
ALLOCATE( D2LEVFRC(NSEQMAX,1) )
D2LEVHGT(:,:)   =0._JPRB
D2LEVFRC(:,:)   =0._JPRB

TMPNAM=INQUIRE_FID()

WRITE(LOGNAM,*)'INIT_LEVEE: levee crown height : ',TRIM(CLEVHGT)
OPEN(TMPNAM,FILE=CLEVHGT,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4*NX*NY)
READ(TMPNAM,REC=1) R2TEMP(:,:)
CALL mapR2vecD(R2TEMP,D2LEVHGT)
CLOSE(TMPNAM)

WRITE(LOGNAM,*)'INIT_LEVEE: distance from levee to river : ',TRIM(CLEVFRC)
OPEN(TMPNAM,FILE=CLEVFRC,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4*NX*NY)
READ(TMPNAM,REC=1) R2TEMP(:,:)
CALL mapR2vecD(R2TEMP,D2LEVFRC)
CLOSE(TMPNAM)

!*******************************
! [2] Calculate Levee Stage Parameter
WRITE(LOGNAM,*) "CMF::LEVEE_INIT: flood stage parameters considering levee"

ALLOCATE( D2BASHGT(NSEQMAX,1) )
ALLOCATE( D2LEVDST(NSEQMAX,1) )

ALLOCATE( D2LEVBASSTO(NSEQMAX,1) )
ALLOCATE( D2LEVTOPSTO(NSEQMAX,1) )
ALLOCATE( D2LEVFILSTO(NSEQMAX,1) )

D2FLDSTOMAX(:,:,:) = 0._JPRD   !! max floodplain  storage  at each layer
D2FLDGRD(:,:,:)    = 0._JPRB   !! floodplain topo gradient of each layer
DFRCINC=REAL(NLFP,KIND=JPRB)**(-1._JPRB)   !! fration of each layer

D2LEVBASSTO(:,:)= 0._JPRB      !! storage at levee base     (levee protection start)
D2LEVTOPSTO(:,:)= 0._JPRB      !! storage at levee top      (levee protection end)
D2LEVFILSTO(:,:)= 0._JPRB      !! storage when levee filled (protected-side depth reach levee top)

!$OMP PARALLEL DO SIMD
DO ISEQ=1, NSEQALL
  IF( D2LEVHGT(ISEQ,1)<=0._JPRB )THEN
    D2LEVHGT(ISEQ,1)=0._JPRB
    D2LEVFRC(ISEQ,1)=1._JPRB   !! If no levee, all area is unprotected/
  ENDIF
  D2LEVFRC(ISEQ,1)=MAX(0._JPRB,MIN(1._JPRB,D2LEVFRC(ISEQ,1)))
END DO
!$OMP END PARALLEL DO SIMD

!$OMP PARALLEL DO
DO ISEQ=1, NSEQALL
! calculate floodplain parameters (without levee, same as SET_FLDSTG)
  DSTO_fil = D2RIVSTOMAX(ISEQ,1)
  DHGTPRE = 0._JPRB
  DWTH_inc = D2GRAREA(ISEQ,1) * D2RIVLEN(ISEQ,1)**(-1.) * DFRCINC  !! width increlment for each layer
  DO I=1, NLFP
    DSTO_add = D2RIVLEN(ISEQ,1) * ( D2RIVWTH(ISEQ,1) + DWTH_inc*(REAL(I,KIND=JPRB)-0.5) ) * (D2FLDHGT(ISEQ,1,I)-DHGTPRE)  
    D2FLDSTOMAX(ISEQ,1,I) = DSTO_fil + DSTO_add
    D2FLDGRD(ISEQ,1,I) = (D2FLDHGT(ISEQ,1,I)-DHGTPRE) * DWTH_inc**(-1.)
    DSTO_fil = D2FLDSTOMAX(ISEQ,1,I)
    DHGTPRE = D2FLDHGT(ISEQ,1,I)
  END DO

! Levee parameters calculation
  IF( D2LEVHGT(ISEQ,1) == 0._JPRB )THEN ! Grid without levee, treat everything as unprotected
    D2BASHGT(ISEQ,1) = 1.E18_JPRB
    D2LEVDST(ISEQ,1) = 1.E18_JPRB
    D2LEVBASSTO(ISEQ,1) = 1.E18_JPRB
    D2LEVTOPSTO(ISEQ,1) = 1.E18_JPRB
    D2LEVFILSTO(ISEQ,1) = 1.E18_JPRB
  ELSE  !! levee exist
    !!*********
    !! [1] levee base storage & levee top storage (water only in river side)

    DSTO_fil = D2RIVSTOMAX(ISEQ,1)
    DHGTPRE = 0._JPRB
    DWTH_fil = 0._JPRB
    D2LEVDST(ISEQ,1) = D2LEVFRC(ISEQ,1) * DWTH_inc*NLFP !! distance from channel to levee [m]

    ILEV=INT( D2LEVFRC(ISEQ,1)*NLFP )+1 !! which layer levee exist
    IF( ILEV>=2 )THEN
      DSTO_fil = D2FLDSTOMAX(ISEQ,1,ILEV-1)
      DHGTPRE = D2FLDHGT(ISEQ,1,ILEV-1)
      DWTH_fil = DWTH_inc * (ILEV-1)
    ENDIF

    IF( ILEV<=NLFP )THEN
      !! levee in floodplain layer ILEV
      DWTH_add = D2LEVDST(ISEQ,1) - DWTH_fil
      DHGTNOW = DWTH_add * D2FLDGRD(ISEQ,1,ILEV) !! levee height above lower floodplain profile point
      D2BASHGT(ISEQ,1) = DHGTNOW + DHGTPRE
      D2LEVHGT(ISEQ,1) = max( D2LEVHGT(ISEQ,1), D2BASHGT(ISEQ,1) ) !! levee height >= base height

      DSTO_add = ( DWTH_add*0.5 + DWTH_fil + D2RIVWTH(ISEQ,1) ) * DHGTNOW * D2RIVLEN(ISEQ,1) 
      D2LEVBASSTO(ISEQ,1) = DSTO_fil + DSTO_add

      DHGTDIF = D2LEVHGT(ISEQ,1) - D2BASHGT(ISEQ,1)
      D2LEVTOPSTO(ISEQ,1) = D2LEVBASSTO(ISEQ,1) + ( D2LEVDST(ISEQ,1)+D2RIVWTH(ISEQ,1) ) * DHGTDIF * D2RIVLEN(ISEQ,1)
    ELSE
      !! levee on the floodplain edge (ILEV=NLEV+1)
      D2BASHGT(ISEQ,1) = DHGTPRE
      D2LEVHGT(ISEQ,1) = max( D2LEVHGT(ISEQ,1), D2BASHGT(ISEQ,1) ) !! levee height >= base height

      D2LEVBASSTO(ISEQ,1) = DSTO_fil

      DHGTDIF = D2LEVHGT(ISEQ,1) - D2BASHGT(ISEQ,1)
      D2LEVTOPSTO(ISEQ,1) = D2LEVBASSTO(ISEQ,1) + ( D2LEVDST(ISEQ,1)+D2RIVWTH(ISEQ,1) ) * DHGTDIF * D2RIVLEN(ISEQ,1)
    ENDIF

    !!*********
    !! [2] levee fill storage (water in both river side & protected side)
    I=1
    DSTO_fil = D2RIVSTOMAX(ISEQ,1)
    DWTH_fil = D2RIVWTH(ISEQ,1)
    DHGTPRE = 0._JPRB

    !! check which layer levee top belongs
    DO WHILE( D2LEVHGT(ISEQ,1) > D2FLDHGT(ISEQ,1,I) .AND. I<=NLFP )
      DSTO_fil = D2FLDSTOMAX(ISEQ,1,I)
      DWTH_fil = DWTH_fil + DWTH_inc
      DHGTPRE = D2FLDHGT(ISEQ,1,I)
      I=I+1
      IF( I>NLFP ) EXIT
    END DO

    !! calculate levee fill volume
    IF( I<=NLFP )THEN 
      !! levee top height collesponds to layer I
      DHGTNOW = D2LEVHGT(ISEQ,1) - DHGTPRE
      DWTH_add = DHGTNOW / D2FLDGRD(ISEQ,1,I)

      DSTO_add = ( DWTH_add*0.5_JPRB + DWTH_fil ) * DHGTNOW * D2RIVLEN(ISEQ,1)
      D2LEVFILSTO(ISEQ,1) = DSTO_fil + DSTO_add
    ELSE
      !! levee higher than catchment boundary height
      DHGTNOW = D2LEVHGT(ISEQ,1) - DHGTPRE
      DSTO_add = DWTH_fil * DHGTNOW * D2RIVLEN(ISEQ,1)
      D2LEVFILSTO(ISEQ,1) = DSTO_fil + DSTO_add
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
USE YOS_CMF_MAP    ,ONLY: D2GRAREA, D2RIVLEN, D2RIVWTH, D2RIVELV, D2RIVSTOMAX, D2FLDSTOMAX, D2FLDGRD, DFRCINC, D2FLDHGT
USE YOS_CMF_PROG   ,ONLY: P2RIVSTO, P2FLDSTO
USE YOS_CMF_DIAG   ,ONLY: D2RIVDPH, D2FLDDPH, D2FLDFRC, D2FLDARE, D2SFCELV, D2STORGE
USE YOS_CMF_DIAG   ,ONLY: P0GLBSTOPRE2, P0GLBSTONEW2, P0GLBRIVSTO, P0GLBFLDSTO, P0GLBLEVSTO, P0GLBFLDARE

!! levee specific data
USE YOS_CMF_PROG   ,ONLY: P2LEVSTO  !! flood storage in protected side (P2FLDSTO for storage betwen river & levee)
USE YOS_CMF_DIAG   ,ONLY: D2LEVDPH  !! flood depth in protected side   (D2FLDDPH for water depth betwen river & levee)
IMPLICIT NONE

!*** LOCAL
! Save for OpenMP
INTEGER(KIND=JPIM),SAVE ::  ISEQ, I, ILEV
REAL(KIND=JPRD),SAVE    ::  PSTOALL
REAL(KIND=JPRB),SAVE    ::  DSTOALL,DSTO_add,DSTO_fil,DWTH_add,DWTH_fil,DDPH_fil,DDPH_add,DWTH_inc
!$OMP THREADPRIVATE (I,ILEV,DSTOALL,DSTO_add,DSTO_fil,DWTH_add,DWTH_fil,DDPH_fil,DDPH_add,DWTH_inc,PSTOALL)
!!==============================
P0GLBSTOPRE2=0._JPRD
P0GLBSTONEW2=0._JPRD
P0GLBRIVSTO =0._JPRD
P0GLBFLDSTO =0._JPRD
P0GLBLEVSTO =0._JPRD
P0GLBFLDARE =0._JPRD

!$OMP PARALLEL DO REDUCTION(+:P0GLBSTOPRE2)
DO ISEQ=1, NSEQALL
!
  PSTOALL = P2RIVSTO(ISEQ,1) + P2FLDSTO(ISEQ,1) + P2LEVSTO(ISEQ,1)
  DSTOALL = REAL( PSTOALL, KIND=JPRB)
  P0GLBSTOPRE2     = P0GLBSTOPRE2 + PSTOALL

  DWTH_inc = D2GRAREA(ISEQ,1) / D2RIVLEN(ISEQ,1) * DFRCINC    !! width of each layer [m]
  IF( DSTOALL > D2RIVSTOMAX(ISEQ,1) )THEN
    !**********
    ! [Case-1] Water surface is under levee base (all water is between river-levee)
    IF( DSTOALL < D2LEVBASSTO(ISEQ,1) )THEN 
      I=1
      DSTO_fil = D2RIVSTOMAX(ISEQ,1)
      DWTH_fil = D2RIVWTH(ISEQ,1)
      DDPH_fil = 0._JPRB

      ! which layer current water level is
      DO WHILE( DSTOALL > D2FLDSTOMAX(ISEQ,1,I) .AND. I<=NLFP )
        DSTO_fil = D2FLDSTOMAX(ISEQ,1,I)
        DWTH_fil = DWTH_fil + DWTH_inc
        DDPH_fil = DDPH_fil + D2FLDGRD(ISEQ,1,I) * DWTH_inc
        I=I+1
        IF( I>NLFP ) EXIT
      END DO

      ! water depth at unprotected area
      IF( I<=NLFP )THEN
        DSTO_add =  DSTOALL - DSTO_fil
        DWTH_add = -DWTH_fil + ( DWTH_fil**2._JPRB + 2._JPRB*DSTO_add / D2RIVLEN(ISEQ,1) / D2FLDGRD(ISEQ,1,I) )**0.5_JPRB
        D2FLDDPH(ISEQ,1) = DDPH_fil + D2FLDGRD(ISEQ,1,I) * DWTH_add
      ELSE
        DSTO_add = DSTOALL - DSTO_fil
        DWTH_add = 0._JPRB
        D2FLDDPH(ISEQ,1) = DDPH_fil + DSTO_add / DWTH_fil / D2RIVLEN(ISEQ,1)
      ENDIF

      P2RIVSTO(ISEQ,1) = D2RIVSTOMAX(ISEQ,1) + D2RIVLEN(ISEQ,1) * D2RIVWTH(ISEQ,1) * D2FLDDPH(ISEQ,1)
      D2RIVDPH(ISEQ,1) = REAL(P2RIVSTO(ISEQ,1),KIND=JPRB) / D2RIVLEN(ISEQ,1) / D2RIVWTH(ISEQ,1)
!
      P2FLDSTO(ISEQ,1) = PSTOALL - P2RIVSTO(ISEQ,1)
      P2FLDSTO(ISEQ,1) = MAX( P2FLDSTO(ISEQ,1), 0._JPRD )
      D2FLDFRC(ISEQ,1) = (-D2RIVWTH(ISEQ,1) + DWTH_fil + DWTH_add ) / (DWTH_inc*NLFP)
      D2FLDFRC(ISEQ,1) = MAX( D2FLDFRC(ISEQ,1),0._JPRB )
      D2FLDFRC(ISEQ,1) = MIN( D2FLDFRC(ISEQ,1),1._JPRB )
      D2FLDARE(ISEQ,1) = D2GRAREA(ISEQ,1)*D2FLDFRC(ISEQ,1)
!
      P2LEVSTO(ISEQ,1) = 0._JPRD  !! no flooding in protected area
      D2LEVDPH(ISEQ,1) = 0._JPRB

    !**********
    ! [Case-2]  River-side water surface is under levee crown (water only in river side)
    ELSEIF( DSTOALL < D2LEVTOPSTO(ISEQ,1) )THEN 

      DSTO_add = DSTOALL - D2LEVBASSTO(ISEQ,1)
      DWTH_add = D2LEVDST(ISEQ,1) + D2RIVWTH(ISEQ,1)
      D2FLDDPH(ISEQ,1) = D2BASHGT(ISEQ,1) + DSTO_add / DWTH_add / D2RIVLEN(ISEQ,1)

      P2RIVSTO(ISEQ,1) = D2RIVSTOMAX(ISEQ,1) + D2RIVLEN(ISEQ,1) * D2RIVWTH(ISEQ,1) * D2FLDDPH(ISEQ,1)
      D2RIVDPH(ISEQ,1) = REAL(P2RIVSTO(ISEQ,1),KIND=JPRB) / D2RIVLEN(ISEQ,1) / D2RIVWTH(ISEQ,1)
  !
      P2FLDSTO(ISEQ,1) = PSTOALL - P2RIVSTO(ISEQ,1)
      P2FLDSTO(ISEQ,1) = MAX( P2FLDSTO(ISEQ,1), 0._JPRD )
      D2FLDFRC(ISEQ,1) = D2LEVFRC(ISEQ,1)
      D2FLDARE(ISEQ,1) = D2GRAREA(ISEQ,1)*D2FLDFRC(ISEQ,1)
  ! 
      P2LEVSTO(ISEQ,1) = 0._JPRD  !! no flooding in protected area
      D2LEVDPH(ISEQ,1) = 0._JPRB

    !**********
    ! [Case-3] River side is full, protected side is under levee crown height (Water both in river side & protected side)
    ELSEIF( DSTOALL < D2LEVFILSTO(ISEQ,1) )THEN 
      ! river side stage = levee height
      D2FLDDPH(ISEQ,1) = D2LEVHGT(ISEQ,1)
      P2RIVSTO(ISEQ,1) = D2RIVSTOMAX(ISEQ,1) + D2RIVLEN(ISEQ,1) * D2RIVWTH(ISEQ,1) * D2FLDDPH(ISEQ,1)
      D2RIVDPH(ISEQ,1) = REAL(P2RIVSTO(ISEQ,1),KIND=JPRB) / D2RIVLEN(ISEQ,1) / D2RIVWTH(ISEQ,1)

      P2FLDSTO(ISEQ,1) = D2LEVTOPSTO(ISEQ,1) - P2RIVSTO(ISEQ,1)
      P2FLDSTO(ISEQ,1) = MAX( P2FLDSTO(ISEQ,1), 0._JPRD )

      !! protected side storate calculation
      P2LEVSTO(ISEQ,1) = PSTOALL - P2RIVSTO(ISEQ,1) - P2FLDSTO(ISEQ,1)
      P2LEVSTO(ISEQ,1) = MAX( P2LEVSTO(ISEQ,1), 0._JPRD )

      !!****
      !! protected side stage calculation
      ILEV=INT( D2LEVFRC(ISEQ,1)*NLFP )+1 !! levee relative distance -> floodplain layer with levee
      DSTO_fil = D2LEVTOPSTO(ISEQ,1)
      DWTH_fil = 0._JPRB
      DDPH_fil = 0._JPRB
      !! which layer current water level is
      I=ILEV
      DO WHILE( I<=NLFP )
        DSTO_add = ( D2LEVDST(ISEQ,1)+D2RIVWTH(ISEQ,1) ) * ( D2LEVHGT(ISEQ,1)-D2FLDHGT(ISEQ,1,I) ) * D2RIVLEN(ISEQ,1) 
        IF( DSTOALL < D2FLDSTOMAX(ISEQ,1,I) + DSTO_add ) EXIT
        DSTO_fil = D2FLDSTOMAX(ISEQ,1,I) + DSTO_add
        DWTH_fil = DWTH_inc*I - D2LEVDST(ISEQ,1)
        DDPH_fil = D2FLDHGT(ISEQ,1,I) - D2BASHGT(ISEQ,1)
        I=I+1
        IF( I>NLFP ) EXIT
      END DO

      IF( I<=NLFP )THEN
        DSTO_add = DSTOALL - DSTO_fil
        DWTH_add = -DWTH_fil + ( DWTH_fil**2._JPRB + 2._JPRB / DSTO_add*D2RIVLEN(ISEQ,1) / D2FLDGRD(ISEQ,1,I) )**0.5_JPRB
        DDPH_add = DWTH_add * D2FLDGRD(ISEQ,1,I)
        D2LEVDPH(ISEQ,1) = D2BASHGT(ISEQ,1) + DDPH_fil + DDPH_add

        D2FLDFRC(ISEQ,1) = ( DWTH_fil + D2LEVDST(ISEQ,1) ) / (DWTH_inc*NLFP)
        D2FLDFRC(ISEQ,1) = MAX( D2FLDFRC(ISEQ,1),0._JPRB)
        D2FLDFRC(ISEQ,1) = MIN( D2FLDFRC(ISEQ,1),1._JPRB)
        D2FLDARE(ISEQ,1) = D2GRAREA(ISEQ,1)*D2FLDFRC(ISEQ,1)
      ELSE
        DSTO_add = DSTOALL - DSTO_fil
        DDPH_add = DSTO_add / DWTH_fil / D2RIVLEN(ISEQ,1)
        D2LEVDPH(ISEQ,1) = D2BASHGT(ISEQ,1) + DDPH_fil + DDPH_add

        D2FLDFRC(ISEQ,1) = 1._JPRB
        D2FLDARE(ISEQ,1) = D2GRAREA(ISEQ,1)*D2FLDFRC(ISEQ,1)
      ENDIF

    !**********
    ! [Case-4] Water level above levee crown (Both river side and protected side exceed levee crown height)
    ELSE 
      I=1
      DSTO_fil = D2RIVSTOMAX(ISEQ,1)
      DWTH_fil = D2RIVWTH(ISEQ,1)
      DDPH_fil = 0._JPRB
      DO WHILE( DSTOALL > D2FLDSTOMAX(ISEQ,1,I) .AND. I<=NLFP)
        DSTO_fil = D2FLDSTOMAX(ISEQ,1,I)
        DWTH_fil = DWTH_fil + DWTH_inc
        DDPH_fil = DDPH_fil + D2FLDGRD(ISEQ,1,I) * DWTH_inc
        I=I+1
        IF( I>NLFP ) EXIT
      END DO

      IF( I<=NLFP )THEN
        DSTO_add =  DSTOALL - DSTO_fil
        DWTH_add = -DWTH_fil + ( DWTH_fil**2._JPRB + 2._JPRB*DSTO_add / D2RIVLEN(ISEQ,1) / D2FLDGRD(ISEQ,1,I) )**0.5_JPRB
        D2FLDDPH(ISEQ,1) = DDPH_fil + D2FLDGRD(ISEQ,1,I) * DWTH_add
      ELSE
        DSTO_add = DSTOALL - DSTO_fil
        DWTH_add = 0._JPRB
        D2FLDDPH(ISEQ,1) = DDPH_fil + DSTO_add / DWTH_fil / D2RIVLEN(ISEQ,1)
      ENDIF

      D2FLDFRC(ISEQ,1) = (-D2RIVWTH(ISEQ,1) + DWTH_fil + DWTH_add ) / (DWTH_inc*NLFP)
      D2FLDARE(ISEQ,1) = D2GRAREA(ISEQ,1)*D2FLDFRC(ISEQ,1)

      !! river channel storage
      P2RIVSTO(ISEQ,1) = D2RIVSTOMAX(ISEQ,1) + D2RIVLEN(ISEQ,1) * D2RIVWTH(ISEQ,1) * D2FLDDPH(ISEQ,1)
      D2RIVDPH(ISEQ,1) = REAL(P2RIVSTO(ISEQ,1),KIND=JPRB) / D2RIVLEN(ISEQ,1) / D2RIVWTH(ISEQ,1)
!
      DSTO_add = ( D2FLDDPH(ISEQ,1)-D2LEVHGT(ISEQ,1) ) * (D2LEVDST(ISEQ,1)+D2RIVWTH(ISEQ,1)) * D2RIVLEN(ISEQ,1)
      P2FLDSTO(ISEQ,1) = D2LEVTOPSTO(ISEQ,1) + DSTO_add - P2RIVSTO(ISEQ,1)
      P2FLDSTO(ISEQ,1) = MAX( P2FLDSTO(ISEQ,1), 0._JPRD )

      P2LEVSTO(ISEQ,1) = PSTOALL - P2RIVSTO(ISEQ,1) - P2FLDSTO(ISEQ,1)
      P2LEVSTO(ISEQ,1) = MAX( P2LEVSTO(ISEQ,1), 0._JPRD )
      D2LEVDPH(ISEQ,1) = D2FLDDPH(ISEQ,1)
    ENDIF
  ! [Case-0] Water only in river channel
  ELSE
    P2RIVSTO(ISEQ,1) = PSTOALL
    D2RIVDPH(ISEQ,1) = DSTOALL / D2RIVLEN(ISEQ,1) / D2RIVWTH(ISEQ,1)
    D2RIVDPH(ISEQ,1) = MAX( D2RIVDPH(ISEQ,1), 0._JPRB )
    P2FLDSTO(ISEQ,1) = 0._JPRD
    D2FLDDPH(ISEQ,1) = 0._JPRB
    D2FLDFRC(ISEQ,1) = 0._JPRB
    D2FLDARE(ISEQ,1) = 0._JPRB
    P2LEVSTO(ISEQ,1) = 0._JPRD
    D2LEVDPH(ISEQ,1) = 0._JPRB
  ENDIF
END DO
!$OMP END PARALLEL DO

!$OMP PARALLEL DO SIMD REDUCTION(+:P0GLBSTONEW2,P0GLBRIVSTO,P0GLBFLDSTO,P0GLBLEVSTO,P0GLBFLDARE)
DO ISEQ=1, NSEQALL
  D2SFCELV(ISEQ,1)     = D2RIVELV(ISEQ,1) + D2RIVDPH(ISEQ,1)
  D2STORGE(ISEQ,1) =REAL(P2RIVSTO(ISEQ,1) + P2FLDSTO(ISEQ,1)+ P2LEVSTO(ISEQ,1) ,KIND=JPRB)

  P0GLBSTONEW2     = P0GLBSTONEW2 + P2RIVSTO(ISEQ,1) + P2FLDSTO(ISEQ,1) + P2LEVSTO(ISEQ,1)
  P0GLBRIVSTO      = P0GLBRIVSTO  + P2RIVSTO(ISEQ,1)
  P0GLBFLDSTO      = P0GLBFLDSTO  + P2FLDSTO(ISEQ,1)
  P0GLBLEVSTO      = P0GLBLEVSTO  + P2LEVSTO(ISEQ,1)
  P0GLBFLDARE      = P0GLBFLDARE  + D2FLDARE(ISEQ,1)
END DO
!$OMP END PARALLEL DO SIMD

END SUBROUTINE CMF_LEVEE_FLDSTG
!####################################################################




!####################################################################
SUBROUTINE CMF_LEVEE_OPT_PTHOUT
! realistic bifurcation considering levee
USE PARKIND1,           ONLY: JPIM, JPRB, JPRD
USE YOS_CMF_INPUT,      ONLY: DT, PGRV
USE YOS_CMF_MAP,        ONLY: NSEQMAX, NSEQALL, NPTHOUT, NPTHLEV, PTH_UPST, PTH_DOWN, PTH_DST, &
                            & PTH_ELV, PTH_WTH, PTH_MAN, I2MASK
USE YOS_CMF_MAP,        ONLY: D2ELEVTN, D2RIVELV
USE YOS_CMF_PROG,       ONLY: D1PTHFLW, D1PTHFLW_PRE, D2RIVDPH_PRE
USE YOS_CMF_DIAG,       ONLY: D2LEVDPH, D2SFCELV, D2STORGE, D1PTHFLWSUM
IMPLICIT NONE
!*** Local
REAL(KIND=JPRB)    ::  D2SFCELV_LEV(NSEQMAX,1)                  !! water surface elev protected [m]
REAL(KIND=JPRB)    ::  D2SFCELV_PRE(NSEQMAX,1)                  !! water surface elev (t-1) [m] (for stable calculation)

! SAVE for OpenMP
INTEGER(KIND=JPIM),SAVE ::  IPTH, ILEV, ISEQ, ISEQP, JSEQP
REAL(KIND=JPRB),SAVE    ::  DSLP, DFLW, DOUT_pr, DFLW_pr, DFLW_im, RATE
!$OMP THREADPRIVATE        (DSLP, DFLW, DOUT_pr, DFLW_pr, DFLW_im, RATE, ILEV, ISEQP, JSEQP)
!================================================
!$OMP PARALLEL DO SIMD
DO ISEQ=1, NSEQALL
  IF( D2LEVFRC(ISEQ,1)<1.0_JPRB )THEN
    D2SFCELV_LEV(ISEQ,1) = D2ELEVTN(ISEQ,1)+D2LEVDPH(ISEQ,1)  !! levee exist, calculate pthout based on levee protected depth
  ELSE
    D2SFCELV_LEV(ISEQ,1) = D2SFCELV(ISEQ,1)
  ENDIF
  D2SFCELV_PRE(ISEQ,1) = D2RIVELV(ISEQ,1)+D2RIVDPH_PRE(ISEQ,1)
END DO
!$OMP END PARALLEL DO SIMD

D1PTHFLW(:,:) = 0._JPRB
!$OMP PARALLEL DO
DO IPTH=1, NPTHOUT  
  ISEQP=PTH_UPST(IPTH)
  JSEQP=PTH_DOWN(IPTH)
  !! Avoid calculation outside of domain
  IF (ISEQP<=0 .OR. JSEQP<=0 ) CYCLE
  IF (I2MASK(ISEQP,1) == 1 .OR. I2MASK(JSEQP,1) == 1 ) CYCLE  !! I2MASK is for kinematic-inertial mixed flow scheme. 

!! [1] for channel bifurcation, use river surface elevation  
  DSLP  = (D2SFCELV(ISEQP,1)-D2SFCELV(JSEQP,1)) / PTH_DST(IPTH)
  DSLP = max(-0.005_JPRB,min(0.005_JPRB,DSLP))                                    !! v390 stabilization

  ILEV=1 !! for river channek
    DFLW = MAX(D2SFCELV(ISEQP,1),D2SFCELV(JSEQP,1)) - PTH_ELV(IPTH,ILEV) 
    DFLW = MAX(DFLW,0._JPRB)

    DFLW_pr = MAX(D2SFCELV_PRE(ISEQP,1),D2SFCELV_PRE(JSEQP,1)) - PTH_ELV(IPTH,ILEV)
    DFLW_pr = MAX(DFLW_pr,0._JPRB)

    DFLW_im = (DFLW*DFLW_pr)**0.5                                       !! semi implicit flow depth
    IF( DFLW_im<=0._JPRB ) DFLW_im=DFLW

    IF( DFLW_im>1.E-5_JPRB )THEN                         !! local inertial equation, see [Bates et al., 2010, J.Hydrol.]
      DOUT_pr = D1PTHFLW_PRE(IPTH,ILEV) / PTH_WTH(IPTH,ILEV)      !! outflow (t-1) [m2/s] (unit width)
      D1PTHFLW(IPTH,ILEV) = PTH_WTH(IPTH,ILEV) * ( DOUT_pr + PGRV*DT*DFLW_im*DSLP ) &
             / ( 1._JPRB + PGRV*DT*PTH_MAN(ILEV)**2._JPRB * abs(DOUT_pr)*DFLW_im**(-7._JPRB/3._JPRB) )
    ELSE
      D1PTHFLW(IPTH,ILEV) = 0._JPRB
    ENDIF

!! [1] for overland bifurcation, use protected-side water surface elevation
  IF( NPTHLEV<=1 ) CYCLE

  DSLP  = (D2SFCELV_LEV(ISEQP,1)-D2SFCELV_LEV(JSEQP,1)) / PTH_DST(IPTH)
  DSLP = max(-0.005_JPRB,min(0.005_JPRB,DSLP))      

  DO ILEV=2, NPTHLEV
    DFLW = MAX(D2SFCELV_LEV(ISEQP,1),D2SFCELV_LEV(JSEQP,1)) - PTH_ELV(IPTH,ILEV) 
    DFLW = MAX(DFLW,0._JPRB)

    DFLW_im=DFLW  !! do not consider implicit flow depth for overland bifurcation
    IF( DFLW_im>1.E-5_JPRB )THEN                         !! local inertial equation, see [Bates et al., 2010, J.Hydrol.]
      DOUT_pr = D1PTHFLW_PRE(IPTH,ILEV) * PTH_WTH(IPTH,ILEV)**(-1.)            !! outflow (t-1) [m2/s] (unit width)
      D1PTHFLW(IPTH,ILEV) = PTH_WTH(IPTH,ILEV) * ( DOUT_pr + PGRV*DT*DFLW_im*DSLP ) &
             / ( 1._JPRB + PGRV*DT*PTH_MAN(ILEV)**2._JPRB * abs(DOUT_pr)*DFLW_im**(-7._JPRB/3._JPRB) )
    ELSE
      D1PTHFLW(IPTH,ILEV) = 0._JPRB
    ENDIF
  END DO

  D1PTHFLWSUM(IPTH)=0._JPRB
END DO
!$OMP END PARALLEL DO

DO ILEV=1, NPTHLEV
  !$OMP PARALLEL DO SIMD
  DO IPTH=1, NPTHOUT
    D1PTHFLWSUM(IPTH)=D1PTHFLWSUM(IPTH)+D1PTHFLW(IPTH,ILEV)  !! bifurcation height layer summation
  END DO
  !$OMP END PARALLEL DO SIMD
END DO

!! Storage change limitter (to prevent sudden increase of upstream water level) (v423)
!$OMP PARALLEL DO
DO IPTH=1, NPTHOUT  
  ISEQP=PTH_UPST(IPTH)
  JSEQP=PTH_DOWN(IPTH)
  IF( D1PTHFLWSUM(IPTH)/=0._JPRB )THEN
    RATE= 0.05_JPRB*min(D2STORGE(ISEQP,1),D2STORGE(JSEQP,1)) / abs(D1PTHFLWSUM(IPTH)*DT)  !! flow limit: 5% storage for stability
    RATE= min(RATE, 1.0_JPRB )
    D1PTHFLW(IPTH,:) =D1PTHFLW(IPTH,:) *RATE
    D1PTHFLWSUM(IPTH)=D1PTHFLWSUM(IPTH)*RATE
  ENDIF
END DO
!$OMP END PARALLEL DO

END SUBROUTINE CMF_LEVEE_OPT_PTHOUT
!################################################################

END MODULE CMF_CTRL_LEVEE_MOD
