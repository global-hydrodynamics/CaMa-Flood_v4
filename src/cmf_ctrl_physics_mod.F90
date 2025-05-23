MODULE CMF_CTRL_PHYSICS_MOD
!==========================================================
!* PURPOSE: call CaMa-Flood physics
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
CONTAINS 
!####################################################################
! -- CMF_PHYSICS_ADVANCE
! -- CMF_PHYSICS_FLDSTG
! --
!####################################################################
SUBROUTINE CMF_PHYSICS_ADVANCE
USE PARKIND1,              ONLY: JPIM,   JPRB,    JPRD,      JPRM
USE YOS_CMF_INPUT,         ONLY: LOGNAM, DT,      LADPSTP
USE YOS_CMF_INPUT,         ONLY: LKINE,  LSLPMIX, LFLDOUT,   LPTHOUT,   LDAMOUT, LLEVEE, LOUTINS, LSPAMAT
USE YOS_CMF_PROG,          ONLY: D2FLDOUT, D2FLDOUT_PRE
!
USE CMF_CALC_OUTFLW_MOD,   ONLY: CMF_CALC_OUTFLW, CMF_CALC_INFLOW, CMF_CALC_INFLOW_LSPAMAT
USE CMF_CALC_PTHOUT_MOD,   ONLY: CMF_CALC_PTHOUT
USE CMF_CALC_STONXT_MOD,   ONLY: CMF_CALC_STONXT
USE CMF_CALC_DIAG_MOD,     ONLY: CMF_DIAG_RESET_ADPSTP, CMF_DIAG_AVEMAX_ADPSTP, CMF_DIAG_GETAVE_ADPSTP
! optional
USE CMF_OPT_OUTFLW_MOD,    ONLY: CMF_CALC_OUTFLW_KINEMIX, CMF_CALC_OUTFLW_KINE,CMF_CALC_OUTINS
USE CMF_CTRL_DAMOUT_MOD,   ONLY: CMF_DAMOUT_CALC, CMF_DAMOUT_WATBAL, CMF_DAMOUT_WRTE
USE CMF_CTRL_LEVEE_MOD,    ONLY: CMF_LEVEE_OPT_PTHOUT
#ifdef ILS
USE YOS_CMF_ICI,           ONLY: LLAKEIN
USE CMF_CALC_LAKEIN_MOD,   ONLY: CMF_CALC_LAKEIN, CMF_LAKEIN_AVE
#endif

IMPLICIT NONE
!! LOCAL
INTEGER(KIND=JPIM)            ::  IT, NT
REAL(KIND=JPRB)               ::  DT_DEF
!================================================
DT_DEF=DT

!=== 0. calculate river and floodplain stage (for DT calc & )
CALL CMF_PHYSICS_FLDSTG

NT=1
IF( LADPSTP )THEN    ! adoptive time step
  CALL CALC_ADPSTP
ENDIF

CALL CMF_DIAG_RESET_ADPSTP  !! average & max calculation: reset

!! ==========
DO IT=1, NT

!=== 1. Calculate river discharge 
  IF ( LKINE ) THEN
    CALL CMF_CALC_OUTFLW_KINE       !!  OPTION: kinematic
  ELSEIF( LSLPMIX ) THEN
    CALL CMF_CALC_OUTFLW_KINEMIX    !!  OPTION: mix local-inertial & kinematic based on slope
  ELSE
    CALL CMF_CALC_OUTFLW            !!  Default: Local inertial
  ENDIF

  IF( .not. LFLDOUT )THEN
    D2FLDOUT(:,:)=0._JPRB    !! OPTION: no high-water channel flow
    D2FLDOUT_PRE(:,:)=0._JPRB
  ENDIF

! --- v4.12: damout before pthout for water buget error
  IF ( LDAMOUT ) THEN
    CALL CMF_DAMOUT_CALC            !! reservoir operation
  ENDIF

! --- Bifurcation channel flow
  IF( LPTHOUT )THEN
    IF( LLEVEE )THEN
      CALL CMF_LEVEE_OPT_PTHOUT            !! bifurcation channel flow
    ELSE
      CALL CMF_CALC_PTHOUT            !! bifurcation channel flow
    ENDIF
  ENDIF

! --- Water budget adjustment and calculate inflow
  IF( LSPAMAT )THEN
    CALL CMF_CALC_INFLOW_LSPAMAT
  ELSE
    CALL CMF_CALC_INFLOW
  ENDIF

  IF ( LDAMOUT ) THEN
    CALL CMF_DAMOUT_WATBAL            !! reservoir operation
  ENDIF

! --- save value for next tstet
  CALL CALC_VARS_PRE

!=== 2.  Calculate the storage in the next time step in FTCS diff. eq.
  CALL CMF_CALC_STONXT

!=== option for ILS coupling
#ifdef ILS
  IF( LLAKEIN )THEN
    CALL CMF_CALC_LAKEIN            !! calculate lake inflow for river-lake coupling
  ENDIF
#endif

!=== 3. calculate river and floodplain staging
  CALL CMF_PHYSICS_FLDSTG

!=== 4.  write water balance monitoring to IOFILE
  CALL CALC_WATBAL(IT)


!=== 5. calculate averages, maximum
  CALL CMF_DIAG_AVEMAX_ADPSTP

!=== option for ILS coupling
#ifdef ILS
  IF( LLAKEIN )THEN
    CALL CMF_LAKEIN_AVE
  ENDIF
#endif

END DO
DT=DT_DEF   !! reset DT

CALL CMF_DIAG_GETAVE_ADPSTP   !! average & max calculation: finalize

! --- Optional: calculate instantaneous discharge (only at the end of outer time step)
IF ( LOUTINS ) THEN
  CALL CMF_CALC_OUTINS 
ENDIF


CONTAINS
!==========================================================
!+ CALC_ADPSTP
!+ CALC_WATBAL(IT)
!+ CALC_VARS_PRE
!==========================================================
SUBROUTINE CALC_ADPSTP
USE YOS_CMF_INPUT,      ONLY: PGRV, PDSTMTH, PCADP
USE YOS_CMF_MAP,        ONLY: D2NXTDST
USE YOS_CMF_MAP,        ONLY: NSEQALL,NSEQRIV,I2MASK
USE YOS_CMF_DIAG,       ONLY: D2RIVDPH
#ifdef UseMPI_CMF
USE CMF_CTRL_MPI_MOD,   ONLY: CMF_MPI_ADPSTP
#endif
IMPLICIT NONE
! MPI setting
! SAVE for OpenMP
INTEGER(KIND=JPIM),SAVE         :: ISEQ
REAL(KIND=JPRB),SAVE            :: DT_MIN
REAL(KIND=JPRB),SAVE            :: DDPH, DDST
!$OMP THREADPRIVATE               (DDPH,DDST)
!================================================

DT_MIN=DT_DEF
!$OMP PARALLEL DO SIMD REDUCTION(MIN:DT_MIN)  !! time step for river channels
DO ISEQ=1, NSEQRIV
  IF( I2MASK(ISEQ,1)==0 )THEN
    DDPH=MAX(D2RIVDPH(ISEQ,1),0.01_JPRB )
    DDST=D2NXTDST(ISEQ,1)
    DT_MIN=min( DT_MIN, PCADP*DDST * (PGRV*DDPH)**(-0.5) )
  ENDIF
END DO
!$OMP END PARALLEL DO SIMD

!$OMP PARALLEL DO SIMD REDUCTION(MIN:DT_MIN)
DO ISEQ=NSEQRIV+1, NSEQALL
  IF( I2MASK(ISEQ,1)==0 )THEN
    DDPH=MAX(D2RIVDPH(ISEQ,1),0.01_JPRB )
    DDST=PDSTMTH
    DT_MIN=min( DT_MIN, PCADP*DDST * (PGRV*DDPH)**(-0.5) )
  ENDIF
END DO
!$OMP END PARALLEL DO SIMD

!*** MPI: use same DT in all node
#ifdef UseMPI_CMF
CALL CMF_MPI_ADPSTP(DT_MIN)
#endif
!*********************************

NT=INT( DT_DEF * DT_MIN**(-1.) -0.01 )+1
DT=DT_DEF * REAL(NT)**(-1.)

IF( NT>=2 ) WRITE(LOGNAM,'(A15,I4,3F10.2)') "ADPSTP: NT=",NT, DT_DEF, DT_MIN, DT

END SUBROUTINE CALC_ADPSTP
!==========================================================
!+
!+
!+
!==========================================================
SUBROUTINE CALC_WATBAL(IT)
USE YOS_CMF_TIME,            ONLY: KMIN
USE YOS_CMF_DIAG,            ONLY: P0GLBSTOPRE, P0GLBSTONXT, P0GLBSTONEW,P0GLBRIVINF,P0GLBRIVOUT !! dischrge calculation
USE YOS_CMF_DIAG,            ONLY: P0GLBSTOPRE2,P0GLBSTONEW2,P0GLBRIVSTO,P0GLBFLDSTO,P0GLBFLDARE
USE CMF_UTILS_MOD,           ONLY: MIN2DATE,SPLITDATE,SPLITHOUR
IMPLICIT NONE
INTEGER(KIND=JPIM),INTENT(IN)   :: IT        !! step in adaptive time loop
!*** LOCAL
REAL(KIND=JPRD)                 :: DERROR  !! water ballance error1 (discharge calculation)   [m3]
REAL(KIND=JPRD)                 :: DERROR2 !! water ballance error2 (flood stage calculation) [m3]

!*** local physics time
INTEGER(KIND=JPIM)              :: PKMIN
INTEGER(KIND=JPIM)              :: PYEAR, PMON, PDAY, PHOUR, PMIN
INTEGER(KIND=JPIM)              :: PYYYYMMDD, PHHMM
!*** PARAMETER
REAL(KIND=JPRD)                 ::  DORD
PARAMETER                          (DORD=1.D-9)
! ================================================
PKMIN=INT ( KMIN + IT*DT/60._JPRB )
CALL MIN2DATE(PKMIN,PYYYYMMDD,PHHMM)
CALL SPLITDATE(PYYYYMMDD,PYEAR,PMON,PDAY)
CALL SPLITHOUR(PHHMM,PHOUR,PMIN)

! poisitive error when water appears from somewhere, negative error when water is lost to somewhere
DERROR   = - (P0GLBSTOPRE  - P0GLBSTONXT  + P0GLBRIVINF - P0GLBRIVOUT )  !! flux  calc budget error
DERROR2  = - (P0GLBSTOPRE2 - P0GLBSTONEW2 )                            !! stage calc budget error
WRITE(LOGNAM,'(I4.4,4(A1,I2.2),I6,a6,3F12.3,G12.3,2x,2F12.3,a6, 2F12.3,G12.3,3F12.3)') &
  PYEAR, '/', PMON, '/', PDAY, '_', PHOUR, ':', PMIN, IT, ' flx: ', &
  P0GLBSTOPRE*DORD, P0GLBSTONXT*DORD, P0GLBSTONEW*DORD ,DERROR*DORD,    P0GLBRIVINF*DORD, P0GLBRIVOUT*DORD, ' stg: ', &
  P0GLBSTOPRE2*DORD,P0GLBSTONEW2*DORD,DERROR2*DORD,    P0GLBRIVSTO*DORD,P0GLBFLDSTO*DORD, P0GLBFLDARE*DORD

END SUBROUTINE CALC_WATBAL
!==========================================================
!+
!+
!+
!==========================================================
SUBROUTINE CALC_VARS_PRE
USE YOS_CMF_MAP,        ONLY: NSEQMAX
USE YOS_CMF_PROG,       ONLY: D2RIVOUT,     D2FLDOUT,     P2FLDSTO
USE YOS_CMF_PROG,       ONLY: D2RIVOUT_PRE, D2FLDOUT_PRE, D2FLDSTO_PRE, D2RIVDPH_PRE
USE YOS_CMF_PROG,       ONLY: D1PTHFLW, D1PTHFLW_PRE
USE YOS_CMF_DIAG,       ONLY: D2RIVDPH
IMPLICIT NONE
! SAVE for OpenMP
INTEGER(KIND=JPIM),SAVE         :: ISEQ
! ================================================
!$OMP PARALLEL DO SIMD
DO ISEQ=1, NSEQMAX
  D2RIVOUT_PRE(ISEQ,1)=D2RIVOUT(ISEQ,1)                              !! save outflow (t)
  D2RIVDPH_PRE(ISEQ,1)=D2RIVDPH(ISEQ,1)                              !! save depth   (t)
  D2FLDOUT_PRE(ISEQ,1)=D2FLDOUT(ISEQ,1)                              !! save outflow (t)
  D2FLDSTO_PRE(ISEQ,1)=REAL(P2FLDSTO(ISEQ,1),KIND=JPRB)
END DO
!$OMP END PARALLEL DO SIMD

IF( LPTHOUT )THEN
  D1PTHFLW_PRE(:,:)=D1PTHFLW(:,:)
ENDIF

END SUBROUTINE CALC_VARS_PRE
!==========================================================

END SUBROUTINE CMF_PHYSICS_ADVANCE
!###############################################################





!###############################################################
SUBROUTINE CMF_PHYSICS_FLDSTG
! flood stage scheme selecter
USE YOS_CMF_INPUT,      ONLY: LLEVEE, LSTG_ES
USE CMF_CALC_FLDSTG_MOD,ONLY: CMF_CALC_FLDSTG_DEF, CMF_OPT_FLDSTG_ES
USE CMF_CTRL_LEVEE_MOD, ONLY: CMF_LEVEE_FLDSTG
IMPLICIT NONE

IF( LLEVEE )THEN
  CALL CMF_LEVEE_FLDSTG  !! levee floodstage (Vector processor option not available)
ELSE
  IF( LSTG_ES )THEN
    CALL CMF_OPT_FLDSTG_ES  !! Alternative subroutine optimized for vector processor
  ELSE 
    CALL CMF_CALC_FLDSTG_DEF     !! Default
  ENDIF
ENDIF

END SUBROUTINE CMF_PHYSICS_FLDSTG
!###############################################################

END MODULE CMF_CTRL_PHYSICS_MOD
