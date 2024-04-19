MODULE CMF_CTRL_DAMOUT_MOD
!==========================================================
!* PURPOSE: CaMa-Flood reservoir operation scheme (under development)
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
USE CMF_UTILS_MOD,           ONLY: INQUIRE_FID
USE YOS_CMF_INPUT,           ONLY: LOGNAM, IMIS, LDAMOUT, LPTHOUT, LRESTART
USE YOS_CMF_INPUT,           ONLY: NX, NY, DT
USE YOS_CMF_MAP,             ONLY: I2VECTOR, I1NEXT, NSEQALL, NSEQRIV, NSEQMAX
USE YOS_CMF_MAP,             ONLY: NPTHOUT,  NPTHLEV, PTH_UPST, PTH_DOWN, PTH_ELV, I2MASK!! bifurcation pass
USE YOS_CMF_PROG,            ONLY: D2RIVOUT, D2FLDOUT, P2RIVSTO, P2FLDSTO, P2DAMSTO, P2DAMINF, D2RUNOFF
USE YOS_CMF_DIAG,            ONLY: D2RIVINF, D2FLDINF
USE YOS_CMF_TIME,            ONLY: IYYYYMMDD,ISYYYY
!============================
IMPLICIT NONE
SAVE
!*** NAMELIST/NDAM/
CHARACTER(LEN=256)              :: CDAMFILE    !! dam paramter file
LOGICAL                         :: LDAMTXT     !! true: dam inflow-outflw txt output
LOGICAL                         :: LDAMH22     !! true: Use Hanazaki 2022 scheme
LOGICAL                         :: LDAMYBY     !! true: Use Year-By-Year dam activation
LOGICAL                         :: LiVnorm     !! true: initialize dam storage with Normal Volume
NAMELIST/NDAMOUT/   CDAMFILE, LDAMTXT, LDAMH22, LDAMYBY, LiVnorm

!*** dam parameters
INTEGER(KIND=JPIM)              :: IDAM, NDAM  !! number of dams
INTEGER(KIND=JPIM)              :: NDAMX       !! exclude dams

INTEGER(KIND=JPIM),ALLOCATABLE  :: DamID(:) !! Dam ID
CHARACTER(LEN=256),ALLOCATABLE  :: DamName(:)  !! 
INTEGER(KIND=JPIM),ALLOCATABLE  :: DamIX(:), DamIY(:)  !! IX,IY of dam grid
REAL(KIND=JPRB),ALLOCATABLE     :: DamLon(:), DamLat(:)  !! longitude, latitude of dam body
REAL(KIND=JPRB),ALLOCATABLE     :: upreal(:)   !! observed drainage area of reservoir
REAL(KIND=JPRB),ALLOCATABLE     :: R_VolUpa(:) !! ratio: flood storage capacity / drainage area
REAL(KIND=JPRB),ALLOCATABLE     :: Qf(:), Qn(:) !! Qf: flood discharge, Qn: normal discharge
INTEGER(KIND=JPIM),ALLOCATABLE  :: DamYear(:)  !! Dam activation year
INTEGER(KIND=JPIM),ALLOCATABLE  :: DamStat(:)  !! Dam Status, 2=old, 1=new, -1=not_yet, IMIS=out_of_domain

REAL(KIND=JPRB),ALLOCATABLE     :: EmeVol(:)   !! storage volume to start emergency operation
REAL(KIND=JPRB),ALLOCATABLE     :: FldVol(:)   !! flood control volume: exclusive for flood control
REAL(KIND=JPRB),ALLOCATABLE     :: ConVol(:)   !! conservative volume: mainly for water supply
REAL(KIND=JPRB),ALLOCATABLE     :: NorVol(:)   !! normal storage volume: impoundment

! internal dam param for stability
REAL(KIND=JPRB),ALLOCATABLE     :: AdjVol(:)   !! Dam storage for stabilization 
REAL(KIND=JPRB),ALLOCATABLE     :: Qa(:)       !! Dam outflow for stabilization


!*** dam map
INTEGER(KIND=JPIM),ALLOCATABLE  :: DamSeq(:)   !! coresponding ISEQ of each dam
INTEGER(KIND=JPIM),ALLOCATABLE  :: I1DAM(:)    !! dam map: 1=dam, 10=upstream of dam, 11: dam grid & downstream is also dam, 0=other

CONTAINS
!####################################################################
!* CONTAINS:
! -- CMF_DEMOUT_NMLIST  : Read setting from namelist
! -- CMF_DAMOUT_INIT    : Initialize dam data
! -- CMF_DAMOUT_CALC    : Calculate inflow and outflow at dam
! -- CMF_DAMOUT_WATBAL  : Calculate water balance at dam
! -- CMF_DAMOUT_WRITE   : Write dam-related variables in text file
!####################################################################
SUBROUTINE CMF_DAMOUT_NMLIST
! reed setting from namelist
! -- Called from CMF_DRV_NMLIST
USE YOS_CMF_INPUT,      ONLY: CSETFILE,NSETFILE,LDAMOUT
IMPLICIT NONE
!================================================
WRITE(LOGNAM,*) ""
WRITE(LOGNAM,*) "!---------------------!"

!*** 1. open namelist
NSETFILE=INQUIRE_FID()
OPEN(NSETFILE,FILE=CSETFILE,STATUS="OLD")
WRITE(LOGNAM,*) "CMF::DAMOUT_NMLIST: namelist OPEN in unit: ", TRIM(CSETFILE), NSETFILE 

!*** 2. default value
CDAMFILE="./dam_params.csv"
LDAMTXT=.TRUE.
LDAMH22=.FALSE.
LDAMYBY=.FALSE.
LiVnorm=.FALSE.

!*** 3. read namelist
REWIND(NSETFILE)
READ(NSETFILE,NML=NDAMOUT)

IF( LDAMOUT )THEN
  WRITE(LOGNAM,*)   "=== NAMELIST, NDAMOUT ==="
  WRITE(LOGNAM,*)   "CDAMFILE: " , CDAMFILE
  WRITE(LOGNAM,*)   "LDAMTXT:  " , LDAMTXT
  WRITE(LOGNAM,*)   "LDAMH22:  " , LDAMH22
  WRITE(LOGNAM,*)   "LDAMYBY:  " , LDAMYBY
  WRITE(LOGNAM,*)   "LiVnorm: " , LiVnorm
ENDIF

CLOSE(NSETFILE)

WRITE(LOGNAM,*) "CMF::DAMOUT_NMLIST: end" 

END SUBROUTINE CMF_DAMOUT_NMLIST
!####################################################################





!####################################################################
SUBROUTINE CMF_DAMOUT_INIT
! reed setting from CDAMFILE
IMPLICIT NONE
INTEGER(KIND=JPIM)         :: NDAMFILE
INTEGER(KIND=JPIM)         :: ISEQ, JSEQ
INTEGER(KIND=JPIM)         :: IX, IY
REAL(KIND=JPRB)            :: FldVol_mcm, ConVol_mcm, TotVol_mcm !! from file in Million Cubic Metter
REAL(KIND=JPRB)            :: Qsto, Vyr

INTEGER(KIND=JPIM)         :: IPTH, ILEV, ISEQP, JSEQP
!####################################################################
WRITE(LOGNAM,*) ""
WRITE(LOGNAM,*) "!---------------------!"
WRITE(LOGNAM,*) "CMF::DAMOUT_INIT: initialize dam", trim(CDAMFILE) 

!==========
NDAMFILE=INQUIRE_FID()
OPEN(NDAMFILE,FILE=CDAMFILE,STATUS="OLD")
READ(NDAMFILE,*) NDAM
READ(NDAMFILE,*)        !! skip header

WRITE(LOGNAM,*) "CMF::DAMOUT_INIT: number of dams", NDAM

!! === ALLOCATE ===
!! from CDAMFILE
ALLOCATE(DamID(NDAM),DamName(NDAM))
ALLOCATE(DamIX(NDAM),DamIY(NDAM),DamLon(NDAM),DamLat(NDAM))
ALLOCATE(upreal(NDAM))
ALLOCATE(Qf(NDAM),Qn(NDAM))
ALLOCATE(DamYear(NDAM),DamStat(NDAM))

!! calculate from CDAMFILE
ALLOCATE(DamSeq(NDAM))
ALLOCATE(FldVol(NDAM),ConVol(NDAM),EmeVol(NDAM),NorVol(NDAM))

!! for outflw stability
ALLOCATE(AdjVol(NDAM),Qa(NDAM))

!! H22scheme parameter (FldVol/Upreal)
ALLOCATE(R_VolUpa(NDAM))

!! dam map, dam variable
ALLOCATE(I1DAM(NSEQMAX))
!! =================
DamSeq(:) =IMIS
DamStat(:)=IMIS
I1DAM(:)=0
NDAMX=0
!! read dam parameters
DO IDAM = 1, NDAM
  IF( LDAMYBY) THEN
    READ(NDAMFILE,*) DamID(IDAM), DamName(IDAM), DamLat(IDAM), DamLon(IDAM), upreal(IDAM), &
     DamIX(IDAM), DamIY(IDAM), FldVol_mcm, ConVol_mcm, TotVol_mcm, Qn(IDAM), Qf(IDAM), DamYear(IDAM)
  ELSE
    READ(NDAMFILE,*) DamID(IDAM), DamName(IDAM), DamLat(IDAM), DamLon(IDAM), upreal(IDAM), &
     DamIX(IDAM), DamIY(IDAM), FldVol_mcm, ConVol_mcm, TotVol_mcm, Qn(IDAM), Qf(IDAM)
  ENDIF

  !! storage parameter --- from Million Cubic Meter to m3
  FldVol(IDAM) = FldVol_mcm * 1.E6                  ! Flood control storage capacity: exclusive for flood control
  ConVol(IDAM) = ConVol_mcm * 1.E6

  EmeVol(IDAM) = ConVol(IDAM) + FldVol(IDAM) * 0.95     ! storage to start emergency operation

  IX=DamIX(IDAM)
  IY=DamIY(IDAM)
  IF (IX<=0 .or. IX > NX .or. IY<=0 .or. IY > NY ) cycle

  ISEQ=I2VECTOR(IX,IY)
  IF( I1NEXT(ISEQ)==-9999 .or. ISEQ<=0 ) cycle
  NDAMX=NDAMX+1

  DamSeq(IDAM) =ISEQ
  DamStat(IDAM)=2

  I1DAM(ISEQ)=1
  I2MASK(ISEQ,1)=2   !! reservoir grid. skipped for adaptive time step

  IF( LDAMH22 )THEN    !! Hanazaki 2022 scheme 
    NorVol(IDAM)   = ConVol(IDAM) * 0.5    ! normal storage
    R_VolUpa(NDAM) = FldVol(IDAM) * 1.E-6 / upreal(IDAM)

  ELSE  !! Yamazaki&Funato scheme (paper in prep)
    Vyr =Qn(IDAM)*(365.*24.*60.*60.)                    !! Annual inflow -> assume dry period inflow is 1/8 of annual flow 
    Qsto=(ConVol(IDAM)*0.7+Vyr/4.)/(180.*24.*60.*60.)   !! possible mean outflow in dry period (6month, ConVol*0.7 + Inflow)
    Qn(IDAM)=min(Qn(IDAM),Qsto)*1.5                     !! Outflow at normal volume (*1.5 is parameter to decide outflw balance)

    AdjVol(IDAM)=ConVol(IDAM)  + FldVol(IDAM)*0.1       !! AdjVol is for outflow stability (result is not so sensitive)
    Qa(IDAM)=( Qn(IDAM)+Qf(IDAM) )*0.5                  !! Qa is also for stability
  ENDIF

  !! Year-by-Year scheme. If dam is not yet constructed
  IF( LDAMYBY )THEN
    IF( ISYYYY==DamYear(IDAM) )THEN
      DamStat(IDAM)=1   !! new this year
    ELSEIF( ISYYYY<DamYear(IDAM) .and. DamYear(IDAM)>0 )THEN
      DamStat(IDAM)=-1  !! not yet activated
      I1DAM(ISEQ)=-1
      FldVol(IDAM)=0._JPRB
      ConVol(IDAM)=0._JPRB
    ENDIF
  ENDIF

END DO
CLOSE(NDAMFILE)

WRITE(LOGNAM,*) "CMF::DAMOUT_INIT: allocated dams:", NDAMX 
!==========

!! mark upstream of dam grid, for applying kinematic wave routine to suppress storage buffer effect.
DO ISEQ=1, NSEQALL
  IF( I1DAM(ISEQ)<=0 .and. I1NEXT(ISEQ)>0 )THEN !! if target is non-dam grid
    JSEQ=I1NEXT(ISEQ)
    IF( I1DAM(JSEQ)==1 .or. I1DAM(JSEQ)==11 )THEN !! if downstream is dam
      I1DAM(ISEQ)=10            !! mark upstream of dam grid by "10"
      I2MASK(ISEQ,1)=1   !! reservoir upstream grid. skipped for adaptive time step
    ENDIF
  ENDIF

  IF( I1DAM(ISEQ)==1 .and. I1NEXT(ISEQ)>0 )THEN !! if target is dam grid
    JSEQ=I1NEXT(ISEQ)
    IF( I1DAM(JSEQ)==1 .or. I1DAM(JSEQ)==11 )THEN !! if downstream is dam
      I1DAM(ISEQ)=11            !! mark upstream of dam grid by "11"
      I2MASK(ISEQ,1)=2   !! reservoir grid (cascading). skipped for adaptive time step
    ENDIF
  ENDIF
END DO

!! Initialize dam storage
IF( .not. LRESTART )THEN  !! Initialize without restart data
  P2DAMSTO(:,1)=0._JPRD
  DO IDAM=1, NDAM
    IF( DamStat(IDAM)==IMIS )CYCLE !
    ISEQ=DamSeq(IDAM)
    IF( DamStat(IDAM)==-1 )THEN !! Dam not yet constructed
      P2DAMSTO(ISEQ,1)= P2RIVSTO(ISEQ,1)+P2FLDSTO(ISEQ,1)
    ELSE
      P2DAMSTO(ISEQ,1)=P2RIVSTO(ISEQ,1)+P2FLDSTO(ISEQ,1)
      IF( P2DAMSTO(ISEQ,1)<ConVol(IDAM) )THEN  !! If Normal Volume > initial storage, replace
        P2DAMSTO(ISEQ,1)=ConVol(IDAM)
        P2RIVSTO(ISEQ,1)=ConVol(IDAM)  
        P2FLDSTO(ISEQ,1)=0._JPRD
      ENDIF
    ENDIF
  END DO
ELSE       !! if from restart file
  IF( LDAMYBY )THEN   !! for restart with year-by-year option, set damsto for newly constructed dam 
    DO IDAM=1, NDAM
      IF( DamStat(IDAM)==1 )THEN !! Dam newly activated from this year
        ISEQ=DamSeq(IDAM)
        P2DAMSTO(ISEQ,1)=P2RIVSTO(ISEQ,1)+P2FLDSTO(ISEQ,1)
        IF( LiVnorm .and. P2DAMSTO(ISEQ,1)<ConVol(IDAM) )THEN  !! If Initialize Vnormal option & Vnor>Riv+Fld sto, replace
          P2DAMSTO(ISEQ,1)=ConVol(IDAM)
          P2RIVSTO(ISEQ,1)=ConVol(IDAM)  
          P2FLDSTO(ISEQ,1)=0._JPRD
        ENDIF
      ENDIF
    END DO
  ENDIF
ENDIF

!! Initialize dam inflow
DO ISEQ=1, NSEQALL
  P2DAMINF(ISEQ,1)=0._JPRD
END DO

!! Stop bifurcation at dam & dam-upstream grids
IF( LPTHOUT )THEN
  DO IPTH=1, NPTHOUT
    ISEQP=PTH_UPST(IPTH)
    JSEQP=PTH_DOWN(IPTH)
    IF( ISEQP<=0 .or. JSEQP<=0) CYCLE
    IF( I1DAM(ISEQP)>0 .or. I1DAM(JSEQP)>0 )THEN
      DO ILEV=1, NPTHLEV
        PTH_ELV(IPTH,ILEV)=1.E20  !! no bifurcation
      END DO
    ENDIF
  END DO
ENDIF

END SUBROUTINE CMF_DAMOUT_INIT
!####################################################################





!####################################################################
SUBROUTINE CMF_DAMOUT_CALC
! local
IMPLICIT NONE
! SAVE for OMP
INTEGER(KIND=JPIM),SAVE    :: ISEQD
!** dam variables
REAL(KIND=JPRB),SAVE       :: DamVol
REAL(KIND=JPRB),SAVE       :: DamInflow
REAL(KIND=JPRB),SAVE       :: DamOutflw           !! Total outflw 
REAL(KIND=JPRB),SAVE       :: DamOutTmp           !! Total outflw 
!$OMP THREADPRIVATE    (ISEQD,DamVol,DamInflow,DamOutflw,DamOutTmp)
!====================
!CONTAINS
!+ UPDATE_INFLOW: replace dam upstream with kinamatic wave, calculate inflow to dam
!+ MODIFY_OUTFLW: modify outflw to avoid negative storage
!+
!==========================================================

!* (1) Replace discharge in upstream grids with kinematic outflow
!     to avoid storage buffer effect (Shin et al., 2019, WRR)
! ------  rivout at upstream grids of dam, rivinf to dam grids are updated.
CALL UPDATE_INFLOW


!* (2) Reservoir Operation
!====================================
!     -- compare DamVol against storage level (NorVol, ConVol, EmeVol) & DamInflow against Qf
!$OMP PARALLEL DO
DO IDAM=1, NDAM
  IF( DamStat(IDAM)<=0 ) CYCLE  !! no calculation for dams not activated

  !! *** 2a update dam volume and inflow -----------------------------------
  ISEQD=DamSeq(IDAM)
  DamVol    = P2DAMSTO(ISEQD,1)    
  DamInflow = P2DAMINF(ISEQD,1)

  !! *** 2b Reservoir Operation          ------------------------------
  !===========================
  IF( LDAMH22 )THEN !! Hanazaki 2022 scheme
    !! case1: Water
    IF( DamVol <= NorVol(IDAM) )THEN
      DamOutflw = Qn(IDAM) * (DamVol / ConVol(IDAM) )
    !! case2: water supply
    ELSEIF( NorVol(IDAM)<DamVol .and. DamVol<=ConVol(IDAM) )THEN
      IF( Qf(IDAM)<=DamInflow )THEN
        DamOutflw = Qn(IDAM)*0.5 +   (DamVol-NorVol(IDAM))/( ConVol(IDAM)-NorVol(IDAM))      * (Qf(IDAM) - Qn(IDAM))
      ELSE
        DamOutflw = Qn(IDAM)*0.5 + (((DamVol-NorVol(IDAM))/( EmeVol(IDAM)-NorVol(IDAM)))**2) * (Qf(IDAM) - Qn(IDAM))
      ENDIF  
    !! case3: flood control
    ELSEIF( ConVol(IDAM)<DamVol .and. DamVol<EmeVol(IDAM) ) THEN
      IF( Qf(IDAM) <= DamInflow ) THEN
        DamOutflw = Qf(IDAM) + max((1. - R_VolUpa(IDAM)/0.2),0._JPRB) &
          * (DamVol-ConVol(IDAM))/(EmeVol(IDAM)-ConVol(IDAM)) * (DamInflow-Qf(IDAM))
      !! pre- and after flood control
      ELSE
        DamOutflw = Qn(IDAM)*0.5 + (((DamVol-NorVol(IDAM))/(EmeVol(IDAM)-NorVol(IDAM)))**2)* (Qf(IDAM) - Qn(IDAM))
      ENDIF
    !! case4: emergency operation
    ELSE
      DamOutflw = max(DamInflow, Qf(IDAM))
    ENDIF

  !===========================
  ELSE  !! (not LDAMH22) improved reservoir operation 'Yamazaki & Funato'

    !! Case 1: water use
    IF( DamVol<=ConVol(IDAM) )THEN
      DamOutflw = Qn(IDAM) * (DamVol/ConVol(IDAM))**0.5
    !! case 2: water excess (just avobe ConVol, for outflow stability)
    ELSEIF( DamVol>ConVol(IDAM) .and. DamVol<=AdjVol(IDAM) ) THEN
      DamOutflw = Qn(IDAM) + ( (DamVol-ConVol(IDAM)) / (AdjVol(IDAM)-ConVol(IDAM)) )**3.0 * (Qa(IDAM) - Qn(IDAM))
    !! case 3: water excess
    ELSEIF( DamVol>AdjVol(IDAM) .and. DamVol<=EmeVol(IDAM) ) THEN
      !! (flood period)
      IF( DamInflow >= Qf(IDAM) ) THEN
        !!figure left side No.2
        DamOutflw = Qn(IDAM) + ( (DamVol-ConVol(IDAM)) / (EmeVol(IDAM)-ConVol(IDAM)) )**1.0 * (DamInflow- Qn(IDAM))
        DamOutTmp = Qa(IDAM) + ( (DamVol-AdjVol(IDAM)) / (EmeVol(IDAM)-AdjVol(IDAM)) )**0.1 * (Qf(IDAM) - Qa(IDAM))
        DamOutflw = max( DamOutflw,DamOutTmp )
      !! (non-flood period)
      ELSE
        DamOutflw = Qa(IDAM) + ( (DamVol-AdjVol(IDAM)) / (EmeVol(IDAM)-AdjVol(IDAM)) )**0.1 * (Qf(IDAM) - Qa(IDAM))
      ENDIF
    !! case 4: emergency operation(no.1)
    ELSEIF( DamVol>EmeVol(IDAM) )THEN
      !! (flood period)
      IF( DamInflow >= Qf(IDAM) ) THEN
        DamOutflw = DamInflow
      !! (non-flood period)
      ELSE
        DamOutflw = Qf(IDAM)
      ENDIF
    ENDIF
  ENDIF

  !! *** 2c flow limitter
  DamOutflw = min( DamOutflw, DamVol/DT, real(P2RIVSTO(ISEQD,1)+P2FLDSTO(ISEQD,1),JPRB)/DT )
  DamOutflw = max( DamOutflw, 0._JPRB )

  !! update CaMa variables  (treat all outflow as RIVOUT in dam grid, no fldout)
  D2RIVOUT(ISEQD,1) = DamOutflw
  D2FLDOUT(ISEQD,1) = 0._JPRB
END DO
!$OMP END PARALLEL DO
!====================================

CONTAINS
!==========================================================
!+ UPDATE_INFLOW: replace dam upstream with kinamatic wave, calculate inflow to dam
!+ MODIFY_OUTFLW: modify outflw to avoid negative storage
!+
!==========================================================
SUBROUTINE UPDATE_INFLOW
USE YOS_CMF_INPUT,      ONLY: PMINSLP, PMANFLD
USE YOS_CMF_MAP,        ONLY: D2RIVLEN, D2RIVMAN, D2ELEVTN, D2NXTDST, D2RIVWTH
USE YOS_CMF_PROG,       ONLY: D2RIVOUT_PRE, D2FLDOUT_PRE
USE YOS_CMF_DIAG,       ONLY: D2RIVDPH, D2RIVVEL, D2FLDDPH
IMPLICIT NONE
! SAVE for OpenMP
INTEGER(KIND=JPIM),SAVE    :: ISEQ, JSEQ
REAL(KIND=JPRB),SAVE       :: DSLOPE,DAREA,DVEL,DSLOPE_F,DARE_F,DVEL_F
!$OMP THREADPRIVATE     (JSEQ,DSLOPE,DAREA,DVEL,DSLOPE_F,DARE_F,DVEL_F)
!============================

!*** 1a. reset outflw & dam inflow
!$OMP PARALLEL DO
DO ISEQ=1, NSEQALL
  IF( I1DAM(ISEQ)>0 )THEN  !! if dam grid or upstream of dam, reset variables
    D2RIVOUT(ISEQ,1) = 0._JPRB
    D2FLDOUT(ISEQ,1) = 0._JPRB
    P2DAMINF(ISEQ,1) = 0._JPRD
  ENDIF
END DO
!$OMP END PARALLEL DO

!*** 1b. calculate dam inflow, using previous tstep discharge
#ifndef NoAtom_CMF
!$OMP PARALLEL DO  !! No OMP Atomic for bit-identical simulation (set in Mkinclude)
#endif
DO ISEQ=1, NSEQALL
  IF( I1DAM(ISEQ)==10 .or. I1DAM(ISEQ)==11 )THEN  !! if dam grid or upstream of dam
    JSEQ=I1NEXT(ISEQ)
#ifndef NoAtom_CMF
!$OMP ATOMIC
#endif
    P2DAMINF(JSEQ,1) = P2DAMINF(JSEQ,1) + D2RIVOUT_PRE(ISEQ,1) + D2FLDOUT_PRE(ISEQ,1) 
  ENDIF
END DO
#ifndef NoAtom_CMF
!$OMP END PARALLEL DO
#endif

!*** 1c. discharge for upstream grids of dams
!$OMP PARALLEL DO  !! No OMP Atomic for bit-identical simulation (set in Mkinclude)
DO ISEQ=1, NSEQRIV
  IF( I1DAM(ISEQ)==10 )THEN  !! if downstream is DAM
    JSEQ   = I1NEXT(ISEQ)
    ! === river flow
    DSLOPE = (D2ELEVTN(ISEQ,1)-D2ELEVTN(JSEQ,1)) * D2NXTDST(ISEQ,1)**(-1.)
    DSLOPE = max(DSLOPE,PMINSLP)

    DVEL   = D2RIVMAN(ISEQ,1)**(-1.) * DSLOPE**0.5 * D2RIVDPH(ISEQ,1)**(2./3.)
    DAREA  = D2RIVWTH(ISEQ,1) * D2RIVDPH(ISEQ,1)

    D2RIVVEL(ISEQ,1) = DVEL
    D2RIVOUT(ISEQ,1) = DAREA * DVEL
    D2RIVOUT(ISEQ,1) = MIN( D2RIVOUT(ISEQ,1), real(P2RIVSTO(ISEQ,1),JPRB)/DT )
    !=== floodplain flow
    DSLOPE_F = min( 0.005_JPRB,DSLOPE )    !! set min [instead of using weirequation for efficiency]
    DVEL_F   = PMANFLD**(-1.) * DSLOPE_F**0.5 * D2FLDDPH(ISEQ,1)**(2./3.)
    DARE_F   = P2FLDSTO(ISEQ,1) * D2RIVLEN(ISEQ,1)**(-1.)
    DARE_F   = MAX( DARE_F - D2FLDDPH(ISEQ,1)*D2RIVWTH(ISEQ,1), 0._JPRB )   !!remove above river channel     area

    D2FLDOUT(ISEQ,1) = DARE_F * DVEL_F
    D2FLDOUT(ISEQ,1) = MIN(  D2FLDOUT(ISEQ,1)*1._JPRD, P2FLDSTO(ISEQ,1)/DT )
  ENDIF
END DO
!$OMP END PARALLEL DO

END SUBROUTINE UPDATE_INFLOW
!==========================================================
END SUBROUTINE CMF_DAMOUT_CALC
!####################################################################
!
!
!
!####################################################################
SUBROUTINE CMF_DAMOUT_WATBAL
IMPLICIT NONE
! SAVE for OMP
INTEGER(KIND=JPIM),SAVE    :: ISEQD
!*** water balance
REAL(KIND=JPRB),SAVE       :: DamInflow
REAL(KIND=JPRB),SAVE       :: DamOutflw           !! Total outflw 
REAL(KIND=JPRD),SAVE       :: GlbDAMSTO, GlbDAMSTONXT, GlbDAMINF, GlbDAMOUT, DamMiss
!$OMP THREADPRIVATE    (ISEQD,DamInflow,DamOutflw)
! ==========================================
!* 4) update reservoir storage and check water DamMiss --------------------------
GlbDAMSTO    = 0._JPRB
GlbDAMSTONXT = 0._JPRB
GlbDAMINF    = 0._JPRB
GlbDAMOUT    = 0._JPRB

!$OMP PARALLEL DO REDUCTION(+:GlbDAMSTO, GlbDAMSTONXT, GlbDAMINF, GlbDAMOUT)
DO IDAM=1, NDAM
  IF( DamStat(IDAM)==IMIS ) CYCLE  !! do not calculate for dams outside of calculation domain
  ISEQD = DamSeq(IDAM)

  DamInflow = D2RIVINF(ISEQD,1) + D2FLDINF(ISEQD,1) + D2RUNOFF(ISEQD,1)
  DamOutflw = D2RIVOUT(ISEQD,1) + D2FLDOUT(ISEQD,1)
!!P2DAMINF(ISEQD,1)=DamInflow   !! if water balance needs to be checked in the output file, P2DAMINF should be updated.

  GlbDAMSTO = GlbDAMSTO + P2DAMSTO(ISEQD,1)
  GlbDAMINF = GlbDAMINF + DamInflow*DT
  GlbDAMOUT = GlbDAMOUT + DamOutflw*DT

  P2DAMSTO(ISEQD,1) = P2DAMSTO(ISEQD,1) + DamInflow * DT - DamOutflw * DT

  GlbDAMSTONXT = GlbDAMSTONXT + P2DAMSTO(ISEQD,1)
END DO
!$OMP END PARALLEL DO

DamMiss = GlbDAMSTO-GlbDAMSTONXT+GlbDAMINF-GlbDAMOUT
WRITE(LOGNAM,*) "CMF::DAM_CALC: DamMiss at all dams:", DamMiss*1.D-9

END SUBROUTINE CMF_DAMOUT_WATBAL
!####################################################################
!
!
!
!####################################################################
SUBROUTINE CMF_DAMOUT_WRTE
! local
CHARACTER(len=36)          :: WriteTXT(NDAMX), WriteTXT2(NDAMX)
REAL(KIND=JPRB)            :: DDamInf, DDamOut
! File IO
INTEGER(KIND=JPIM),SAVE    :: ISEQD, JDAM
INTEGER(KIND=JPIM),SAVE    :: LOGDAM
CHARACTER(len=4),SAVE      :: cYYYY
CHARACTER(len=256),SAVE    :: CLEN, CFMT
CHARACTER(len=256),SAVE    :: DAMTXT
LOGICAL,SAVE               :: IsOpen
DATA IsOpen       /.FALSE./
! ==========================================

IF( LDAMTXT )THEN

  IF( .not. IsOpen )THEN
    IsOpen=.TRUE.
    WRITE(CYYYY,'(i4.4)') ISYYYY
    DAMTXT='./damtxt-'//trim(cYYYY)//'.txt'

    LOGDAM=INQUIRE_FID()
    OPEN(LOGDAM,FILE=DAMTXT,FORM='formatted')

    WRITE(CLEN,'(i0)') NDAMX
    CFMT="(i10,"//TRIM(CLEN)//"(a36))"

    JDAM=0
    DO IDAM=1, NDAM
      IF( DamStat(IDAM)==IMIS )CYCLE
      JDAM=JDAM+1
      IF( DamStat(IDAM)==-1 ) THEN   !! dam not activated yet
        WRITE(WriteTxt(JDAM), '(i12,2f12.2)') DamID(IDAM), -9., -9.
        WRITE(WriteTxt2(JDAM),'(3f12.2)') upreal(IDAM),   Qf(IDAM), Qn(IDAM)
      ELSE
        WRITE(WriteTxt(JDAM), '(i12,2f12.2)') DamID(IDAM), (FldVol(IDAM)+ConVol(IDAM))*1.E-9, ConVol(IDAM)*1.E-9
        WRITE(WriteTxt2(JDAM),'(3f12.2)') upreal(IDAM),   Qf(IDAM), Qn(IDAM)
      ENDIF
    END DO

    WRITE(LOGDAM,CFMT) NDAMX, (WriteTXT(JDAM) ,JDAM=1, NDAMX)

    CFMT="(a10,"//TRIM(CLEN)//"(a36))"
    WRITE(LOGDAM,CFMT)  "Date", (WriteTXT2(JDAM),JDAM=1, NDAMX)
  ENDIF

  JDAM=0
  DO IDAM=1, NDAM
    IF( DamStat(IDAM)==IMIS ) CYCLE
    JDAM=JDAM+1
    ISEQD=DamSeq(IDAM)
    DDamInf=P2DAMINF(ISEQD,1)
    DDamOut=D2RIVOUT(ISEQD,1) + D2FLDOUT(ISEQD,1)
    WRITE(WriteTxt(JDAM), '(3f12.2)') P2DAMSTO(ISEQD,1)*1.E-9, DDamInf, DDamOut
  END DO

  CFMT="(i10,"//TRIM(CLEN)//"(a36))"  
  WRITE(LOGDAM,CFMT) IYYYYMMDD, (WriteTXT(JDAM),JDAM=1, NDAMX)

ENDIF

END SUBROUTINE CMF_DAMOUT_WRTE
!####################################################################


END MODULE CMF_CTRL_DAMOUT_MOD
