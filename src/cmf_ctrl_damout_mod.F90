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
USE PARKIND1,                ONLY: JPIM, JPRB, JPRM
USE YOS_CMF_INPUT,           ONLY: LOGNAM
!============================
IMPLICIT NONE
SAVE
!*** NAMELIST/NDAM/
CHARACTER(LEN=256)              :: CDAMFILE    !! dam paramter file
NAMELIST/NDAMOUT/   CDAMFILE

!*** dam parameters
INTEGER(KIND=JPIM)              :: IDAM, NDAM  !! number of dams
INTEGER(KIND=JPIM),ALLOCATABLE  :: GRanD_ID(:) !! GRanD ID
CHARACTER(LEN=256),ALLOCATABLE  :: DamName(:)  !! 
INTEGER(KIND=JPIM),ALLOCATABLE  :: DamIX(:), DamIY(:)  !! IX,IY of dam grid
REAL(KIND=JPRB),ALLOCATABLE     :: DamLon(:), DamLat(:)  !! longitude, latitude of dam body
REAL(KIND=JPRB),ALLOCATABLE     :: upreal(:)   !! observed drainage area of reservoir
REAL(KIND=JPRB),ALLOCATABLE     :: R_VolUpa(:) !! ratio: flood storage capacity / drainage area
REAL(KIND=JPRB),ALLOCATABLE     :: Qf(:), Qn(:) !! Qf: flood discharge, Qn: normal discharge

REAL(KIND=JPRB),ALLOCATABLE     :: EmeVol(:)   !! storage volume to start emergency operation
REAL(KIND=JPRB),ALLOCATABLE     :: FldVol(:)   !! flood control volume: exclusive for flood control
REAL(KIND=JPRB),ALLOCATABLE     :: ConVol(:)   !! conservative volume: mainly for water supply
REAL(KIND=JPRB),ALLOCATABLE     :: NorVol(:)   !! normal storage volume: impoundment

!*** dam map
INTEGER(KIND=JPIM),ALLOCATABLE  :: DamSeq(:)   !! coresponding ISEQ of each dam
INTEGER(KIND=JPIM),ALLOCATABLE  :: I1DAM(:)    !! dam map: 1=dam, 10=upstream of dam, 11: dam grid & downstream is also dam, 0=other

CONTAINS
!####################################################################
!* CONTAINS:
! -- CMF_DEMOUT_NMLIST  : Read setting from namelist
! -- CMF_DAMOUT_INIT    : Initialize dam data
! -- CMF_DAMOUT_CALC    : Calculate inflow and outflow at dam
!####################################################################
SUBROUTINE CMF_DAMOUT_NMLIST
! reed setting from namelist
! -- Called from CMF_DRV_NMLIST
USE YOS_CMF_INPUT,      ONLY: CSETFILE,NSETFILE,LDAMOUT
USE CMF_UTILS_MOD,      ONLY: INQUIRE_FID
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

!*** 3. read namelist
REWIND(NSETFILE)
READ(NSETFILE,NML=NDAMOUT)

IF( LDAMOUT )THEN
  WRITE(LOGNAM,*)   "=== NAMELIST, NDAMOUT ==="
  WRITE(LOGNAM,*)   "CDAMFILE: ", CDAMFILE
ENDIF

CLOSE(NSETFILE)

WRITE(LOGNAM,*) "CMF::DAMOUT_NMLIST: end" 

END SUBROUTINE CMF_DAMOUT_NMLIST
!####################################################################





!####################################################################
SUBROUTINE CMF_DAMOUT_INIT
USE CMF_UTILS_MOD,      ONLY: INQUIRE_FID
USE YOS_CMF_INPUT,      ONLY: NX, NY, LRESTART, LPTHOUT
USE YOS_CMF_MAP,        ONLY: I2VECTOR, I1NEXT, NSEQALL, NSEQMAX
USE YOS_CMF_PROG,       ONLY: D2DAMSTO, D2DAMINF
USE YOS_CMF_MAP,        ONLY: NPTHOUT, NPTHLEV, PTH_UPST, PTH_DOWN, PTH_ELV !! bifurcation pass

! reed setting from CDAMFILE
IMPLICIT NONE
INTEGER(KIND=JPIM)         :: NDAMFILE
INTEGER(KIND=JPIM)         :: ISEQ, JSEQ
INTEGER(KIND=JPIM)         :: IX, IY
REAL(KIND=JPRB)            :: FldVol_mcm, ConVol_mcm, TotVol_mcm !! from file in Million Cubic Metter

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
ALLOCATE(GRanD_ID(NDAM),DamName(NDAM))
ALLOCATE(DamIX(NDAM),DamIY(NDAM),DamLon(NDAM),DamLat(NDAM))
ALLOCATE(upreal(NDAM))
ALLOCATE(R_VolUpa(NDAM))
ALLOCATE(Qf(NDAM),Qn(NDAM))

!! calculate from CDAMFILE
ALLOCATE(DamSeq(NDAM))
ALLOCATE(FldVol(NDAM),ConVol(NDAM),EmeVol(NDAM),NorVol(NDAM))

!! dam map, dam variable
ALLOCATE(I1DAM(NSEQMAX))
!! =================
DamSeq(:)=-9999
I1DAM(:)=0
!! read dam parameters
DO IDAM = 1, NDAM
  READ(NDAMFILE,*) GRanD_ID(IDAM), DamName(IDAM), DamLon(IDAM), DamLat(IDAM), upreal(IDAM), &
   DamIX(IDAM), DamIY(IDAM), FldVol_mcm, ConVol_mcm, TotVol_mcm, R_VolUpa(IDAM), Qn(IDAM), Qf(IDAM)

  !! storage parameter --- from Million Cubic Meter to m3
  FldVol(IDAM) = FldVol_mcm * 1.D6                  ! Flood control storage capacity: exclusive for flood control
  ConVol(IDAM) = ConVol_mcm * 1.D6
  EmeVol(IDAM) = ConVol(IDAM) + FldVol(IDAM) * 0.8     ! storage to start emergency operation
  NorVol(IDAM) = ConVol(IDAM) * 0.5    ! normal storage

  IX=DamIX(IDAM)
  IY=DamIY(IDAM)
  if (IX<=0 .or. IX > NX .or. IY<=0 .or. IY > NY ) cycle

  ISEQ=I2VECTOR(IX,IY)
  DamSeq(IDAM)=ISEQ
  I1DAM(ISEQ)=1
END DO
CLOSE(NDAMFILE)
!==========

!! mark upstream of dam grid, for applying kinematic wave routine to suppress storage buffer effect.
DO ISEQ=1, NSEQALL
  IF( I1DAM(ISEQ)==0 .and. I1NEXT(ISEQ)>0 )THEN !! if target is non-dam grid
    JSEQ=I1NEXT(ISEQ)
    IF( I1DAM(JSEQ)==1 .or. I1DAM(JSEQ)==11 )THEN !! if downstream is dam
      I1DAM(ISEQ)=10            !! mark upstream of dam grid by "10"
    ENDIF
  ENDIF

  IF( I1DAM(ISEQ)==1 .and. I1NEXT(ISEQ)>0 )THEN !! if target is dam grid
    JSEQ=I1NEXT(ISEQ)
    IF( I1DAM(JSEQ)==1 .or. I1DAM(JSEQ)==11 )THEN !! if downstream is dam
      I1DAM(ISEQ)=11            !! mark upstream of dam grid by "11"
    ENDIF
  ENDIF
END DO

!! Initialize dam storage
IF( .not. LRESTART )THEN
  D2DAMSTO(:,1)=0.D0
  DO IDAM=1, NDAM
    IF( DamSeq(IDAM)>0 )THEN
      ISEQ=DamSeq(IDAM)
      D2DAMSTO(ISEQ,1)=NorVol(IDAM)  !! set initial storage to Normal Storage Volume
    ENDIF
  END DO
ENDIF

!! Initialize dam inflow
DO ISEQ=1, NSEQALL
  D2DAMINF(ISEQ,1)=0.D0
END DO

!! Stop bifurcation at dam & dam-upstream grids
IF( LPTHOUT )THEN
  DO IPTH=1, NPTHOUT
    ISEQP=PTH_UPST(IPTH)
    JSEQP=PTH_DOWN(IPTH)
    IF( I1DAM(ISEQP)>0 .or. I1DAM(JSEQP)>0 )THEN
      DO ILEV=1, NPTHLEV
        PTH_ELV(IPTH,ILEV)=1.D20  !! no bifurcation
      END DO
    ENDIF
  END DO
ENDIF

END SUBROUTINE CMF_DAMOUT_INIT
!####################################################################





!####################################################################
SUBROUTINE CMF_DAMOUT_CALC
USE YOS_CMF_INPUT,      ONLY: DT
USE YOS_CMF_MAP,        ONLY: I1NEXT,   NSEQALL,  NSEQRIV
USE YOS_CMF_PROG,       ONLY: D2RIVOUT, D2FLDOUT, D2RIVSTO, D2FLDSTO
USE YOS_CMF_PROG,       ONLY: D2DAMSTO, D2DAMINF, D2RUNOFF    !! 
USE YOS_CMF_DIAG,       ONLY: D2RIVINF, D2FLDINF
! local
IMPLICIT NONE
!$ SAVE
INTEGER(KIND=JPIM)         :: ISEQD
!** dam variables
REAL(KIND=JPRB)            :: DamVol
REAL(KIND=JPRB)            :: DamInflow
REAL(KIND=JPRB)            :: DamOutflw           !! Total outflw 
!*** water balance
REAL(KIND=JPRB)            :: GlbDAMSTO, GlbDAMSTONXT, GlbDAMINF, GlbDAMOUT, DamMiss

!$OMP THREADPRIVATE    (ISEQD,DamVol,DamInflow,DamOutflw)
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
!     -- compare DamVol against storage level (NorVol, ConVol, EmeVol) & DamInflow against Qf
!$OMP PARALLEL DO
DO IDAM=1, NDAM
  IF( DamSeq(IDAM)<=0 ) CYCLE
  ISEQD=DamSeq(IDAM)

  !! *** 2a update dam volume and inflow -----------------------------------
  DamVol    = D2DAMSTO(ISEQD,1)    
  DamInflow = D2DAMINF(ISEQD,1)

  !! *** 2b Reservoir Operation          ------------------------------
  !! case1: impoundment
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
      DamOutflw = Qf(IDAM) + max((1.D0-R_VolUpa(IDAM)/0.2),0.D0) &
        * (DamVol-ConVol(IDAM))/(EmeVol(IDAM)-ConVol(IDAM)) * (DamInflow-Qf(IDAM))
    !! pre- and after flood control
    ELSE
      DamOutflw = Qn(IDAM)*0.5 + (((DamVol-NorVol(IDAM))/(EmeVol(IDAM)-NorVol(IDAM)))**2)* (Qf(IDAM) - Qn(IDAM))
    ENDIF
  !! case4: emergency operation
  ELSE
    DamOutflw = max(DamInflow, Qf(IDAM))
  ENDIF

  !! *** 2c flow limitter
  DamOutflw = min( DamOutflw, DamVol/DT, ( D2RIVSTO(ISEQD,1)+D2FLDSTO(ISEQD,1) )/DT )
  DamOutflw = max( DamOutflw, 0.D0 )

  !! update CaMa variables  (treat all outflow as RIVOUT in dam grid, no fldout)
  D2RIVOUT(ISEQD,1) = DamOutflw
  D2FLDOUT(ISEQD,1) = 0.D0
END DO
!$OMP END PARALLEL DO

!* 3) modify outflow to suppless negative discharge, update RIVOUT,FLDOUT,RIVINF,FLDINF
CALL MODIFY_OUTFLW


!* 4) update reservoir storage and check water DamMiss --------------------------
GlbDAMSTO    = 0.D0
GlbDAMSTONXT = 0.D0
GlbDAMINF    = 0.D0
GlbDAMOUT    = 0.D0

!$OMP PARALLEL DO REDUCTION(+:GlbDAMSTO, GlbDAMSTONXT, GlbDAMINF, GlbDAMOUT)
DO IDAM=1, NDAM
  IF( DamSeq(IDAM)<=0 ) CYCLE
  ISEQD = DamSeq(IDAM)

  DamInflow = D2RIVINF(ISEQD,1) + D2FLDINF(ISEQD,1) + D2RUNOFF(ISEQD,1)
  DamOutflw = D2RIVOUT(ISEQD,1) + D2FLDOUT(ISEQD,1)
!!D2DAMINF(ISEQD,1)=DamInflow   !! if water balance needs to be checked in the output file, D2DAMINF should be updated.

  GlbDAMSTO = GlbDAMSTO + D2DAMSTO(ISEQD,1)
  GlbDAMINF = GlbDAMINF + DamInflow*DT
  GlbDAMOUT = GlbDAMOUT + DamOutflw*DT

  D2DAMSTO(ISEQD,1) = D2DAMSTO(ISEQD,1) + DamInflow * DT - DamOutflw * DT

  GlbDAMSTONXT = GlbDAMSTONXT + D2DAMSTO(ISEQD,1)
END DO
!$OMP END PARALLEL DO

DamMiss = GlbDAMSTO-GlbDAMSTONXT+GlbDAMINF-GlbDAMOUT
WRITE(LOGNAM,*) "CMF::DAM_CALC: DamMiss at all dams:", DamMiss*1.D-9


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
!$ SAVE
INTEGER                    :: ISEQ, JSEQ
REAL(KIND=JPRB)            :: DSLOPE,DAREA,DVEL,DSLOPE_F,DARE_F,DVEL_F
!$OMP THREADPRIVATE     (JSEQ,DSLOPE,DAREA,DVEL,DSLOPE_F,DARE_F,DVEL_F)
!============================

!*** 1a. reset outflw & dam inflow
!$OMP PARALLEL DO
DO ISEQ=1, NSEQALL
  IF( I1DAM(ISEQ)>0 )THEN  !! if dam grid or upstream of dam, reset variables
    D2RIVOUT(ISEQ,1) = 0.D0
    D2FLDOUT(ISEQ,1) = 0.D0
    D2DAMINF(ISEQ,1) = 0.D0
  ENDIF
END DO
!$OMP END PARALLEL DO

!*** 1b. calculate dam inflow, using previous tstep discharge
#ifndef NoAtom
!$OMP PARALLEL DO  !! No OMP Atomic for bit-identical simulation (set in Mkinclude)
#endif
DO ISEQ=1, NSEQALL
  IF( I1DAM(ISEQ)==10 .or. I1DAM(ISEQ)==11 )THEN  !! if dam grid or upstream of dam
    JSEQ=I1NEXT(ISEQ)
!$OMP ATOMIC
    D2DAMINF(JSEQ,1) = D2DAMINF(JSEQ,1) + D2RIVOUT_PRE(ISEQ,1) + D2FLDOUT_PRE(ISEQ,1) 
  ENDIF
END DO
#ifndef NoAtom
!$OMP END PARALLEL DO
#endif

!*** 1c. discharge for upstream grids of dams
!$OMP PARALLEL DO  !! No OMP Atomic for bit-identical simulation (set in Mkinclude)
DO ISEQ=1, NSEQRIV
  IF( I1DAM(ISEQ)==10 )THEN  !! if downstream is DAM
    JSEQ   = I1NEXT(ISEQ)
    ! === river flow
    DSLOPE = (D2ELEVTN(ISEQ,1)-D2ELEVTN(JSEQ,1)) * D2NXTDST(ISEQ,1)**(-1.D0)
    DSLOPE = max(DSLOPE,PMINSLP)

    DVEL   = D2RIVMAN(ISEQ,1)**(-1.D0) * DSLOPE**0.5D0 * D2RIVDPH(ISEQ,1)**(2D0/3.D0)
    DAREA  = D2RIVWTH(ISEQ,1) * D2RIVDPH(ISEQ,1)

    D2RIVVEL(ISEQ,1) = DVEL
    D2RIVOUT(ISEQ,1) = DAREA * DVEL
    D2RIVOUT(ISEQ,1) = MIN(  D2RIVOUT(ISEQ,1), D2RIVSTO(ISEQ,1)/DT )
    !=== floodplain flow
    DSLOPE_F = min( 0.005D0,DSLOPE )    !! set min [instead of using weirequation for efficiency]
    DVEL_F   = PMANFLD**(-1.D0) * DSLOPE_F**0.5D0 * D2FLDDPH(ISEQ,1)**(2.D0/3D0)
    DARE_F   = D2FLDSTO(ISEQ,1) * D2RIVLEN(ISEQ,1)**(-1.D0)
    DARE_F   = MAX( DARE_F - D2FLDDPH(ISEQ,1)*D2RIVWTH(ISEQ,1), 0.D0 )   !!remove above river channel     area

    D2FLDOUT(ISEQ,1) = DARE_F * DVEL_F
    D2FLDOUT(ISEQ,1) = MIN(  D2FLDOUT(ISEQ,1), D2FLDSTO(ISEQ,1)/DT )
  ENDIF
END DO
!$OMP END PARALLEL DO

END SUBROUTINE UPDATE_INFLOW
!==========================================================
!+
!+
!+
!==========================================================
SUBROUTINE MODIFY_OUTFLW
! modify outflow in order to avoid negative storage
USE YOS_CMF_MAP,        ONLY: NSEQMAX
IMPLICIT NONE
!$ SAVE
REAL(KIND=JPRB)            :: D2STOOUT(NSEQMAX,1)                      !! total outflow from a grid     [m3]
REAL(KIND=JPRB)            :: D2RATE(NSEQMAX,1)                        !! outflow correction

INTEGER                    :: ISEQ, JSEQ
REAL(KIND=JPRB)            :: OUT_R1, OUT_R2, OUT_F1, OUT_F2, DIUP, DIDW
!$OMP THREADPRIVATE     (JSEQ,OUT_R1, OUT_R2, OUT_F1, OUT_F2, DIUP, DIDW)
!================================================
  
!*** 1. initialize & calculate D2STOOUT for normal cells

!$OMP PARALLEL DO
DO ISEQ=1, NSEQALL
  D2RIVINF(ISEQ,1) = 0.D0
  D2FLDINF(ISEQ,1) = 0.D0
  D2STOOUT(ISEQ,1) = 0.D0
  D2RATE(ISEQ,1) = 1._JPRB
END DO
!$OMP END PARALLEL DO

!! for normal cells ---------
#ifndef NoAtom
!$OMP PARALLEL DO
#endif
DO ISEQ=1, NSEQRIV                                                    !! for normalcells
  JSEQ=I1NEXT(ISEQ) ! next cell's pixel
  OUT_R1 = max(  D2RIVOUT(ISEQ,1),0.D0 )
  OUT_R2 = max( -D2RIVOUT(ISEQ,1),0.D0 )
  OUT_F1 = max(  D2FLDOUT(ISEQ,1),0.D0 )
  OUT_F2 = max( -D2FLDOUT(ISEQ,1),0.D0 )
  DIUP=(OUT_R1+OUT_F1)*DT
  DIDW=(OUT_R2+OUT_F2)*DT
!$OMP ATOMIC
  D2STOOUT(ISEQ,1) = D2STOOUT(ISEQ,1) + DIUP 
!$OMP ATOMIC
  D2STOOUT(JSEQ,1) = D2STOOUT(JSEQ,1) + DIDW 
END DO
#ifndef NoAtom
!$OMP END PARALLEL DO
#endif

!! for river mouth grids ------------
!$OMP PARALLEL DO
DO ISEQ=NSEQRIV+1, NSEQALL
  OUT_R1 = max( D2RIVOUT(ISEQ,1), 0.D0 )
  OUT_F1 = max( D2FLDOUT(ISEQ,1), 0.D0 )
  D2STOOUT(ISEQ,1) = D2STOOUT(ISEQ,1) + OUT_R1*DT + OUT_F1*DT
END DO
!$OMP END PARALLEL DO

!============================
!*** 2. modify outflow

!$OMP PARALLEL DO
DO ISEQ=1, NSEQALL
  IF ( D2STOOUT(ISEQ,1) > 1.D-8 ) THEN
    D2RATE(ISEQ,1) = min( (D2RIVSTO(ISEQ,1)+D2FLDSTO(ISEQ,1)) * D2STOOUT(ISEQ,1)**(-1.D0), 1.D0 )
  ENDIF
END DO
!$OMP END PARALLEL DO

!! normal pixels------
#ifndef NoAtom
!$OMP PARALLEL DO  !! No OMP Atomic for bit-identical simulation (set in Mkinclude)
#endif
DO ISEQ=1, NSEQRIV ! for normal pixels
  JSEQ=I1NEXT(ISEQ)
  IF( D2RIVOUT(ISEQ,1) >= 0.D0 )THEN
    D2RIVOUT(ISEQ,1) = D2RIVOUT(ISEQ,1)*D2RATE(ISEQ,1)
    D2FLDOUT(ISEQ,1) = D2FLDOUT(ISEQ,1)*D2RATE(ISEQ,1)
  ELSE
    D2RIVOUT(ISEQ,1) = D2RIVOUT(ISEQ,1)*D2RATE(JSEQ,1)
    D2FLDOUT(ISEQ,1) = D2FLDOUT(ISEQ,1)*D2RATE(JSEQ,1)
  ENDIF
!$OMP ATOMIC
  D2RIVINF(JSEQ,1) = D2RIVINF(JSEQ,1) + D2RIVOUT(ISEQ,1)             !! total inflow to a grid (from upstream)
!$OMP ATOMIC
  D2FLDINF(JSEQ,1) = D2FLDINF(JSEQ,1) + D2FLDOUT(ISEQ,1)
END DO
#ifndef NoAtom
!$OMP END PARALLEL DO
#endif

!! river mouth-----------------
!$OMP PARALLEL DO
DO ISEQ=NSEQRIV+1, NSEQALL
  D2RIVOUT(ISEQ,1) = D2RIVOUT(ISEQ,1)*D2RATE(ISEQ,1)
  D2FLDOUT(ISEQ,1) = D2FLDOUT(ISEQ,1)*D2RATE(ISEQ,1)
END DO
!$OMP END PARALLEL DO

END SUBROUTINE MODIFY_OUTFLW
!==========================================================

END SUBROUTINE CMF_DAMOUT_CALC
!####################################################################

END MODULE CMF_CTRL_DAMOUT_MOD
