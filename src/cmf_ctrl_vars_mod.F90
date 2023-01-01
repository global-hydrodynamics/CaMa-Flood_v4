MODULE CMF_CTRL_VARS_MOD
!==========================================================
!* PURPOSE: Manage prognostic/diagnostic variables in CaMa-Flood
!
!* CONTAINS:
! -- CMF_PROG_INIT      : Initialize Prognostic variables (include restart data handling)
! -- CMF_DIAG_INIT      : Initialize Diagnostic variables
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
USE PARKIND1,                ONLY: JPIM, JPRM, JPRB, JPRD
USE YOS_CMF_INPUT,           ONLY: LOGNAM, LPTHOUT, LDAMOUT, LLEVEE, LWEVAP, LOUTINS, LGDWDLY
IMPLICIT NONE
CONTAINS 
!####################################################################
! -- CMF_PROG_INIT      : Initialize Prognostic variables (include restart data handling)
! -- CMF_DIAG_INIT      : Initialize Diagnostic variables
!
!####################################################################
SUBROUTINE CMF_PROG_INIT
USE YOS_CMF_MAP,             ONLY: NSEQMAX, NPTHOUT, NPTHLEV
USE YOS_CMF_PROG,            ONLY: B2RUNOFF,     B2ROFSUB,     &
                                 & D2RIVSTO,     D2FLDSTO,     B2RIVOUT,     B2FLDOUT,     &
                                 & B2RIVOUT_PRE, B2FLDOUT_PRE, B2RIVDPH_PRE, B2FLDSTO_PRE, &
                                 & B1PTHFLW,     B1PTHFLW_PRE, B2GDWSTO,     B2GDWRTN,     &
                                 & D2DAMSTO,     B2DAMINF,     D2LEVSTO ,    B2WEVAP,      &   !! optional
                                 & D2JPRD,       B2JPRB,       R2JPRM
IMPLICIT NONE
!================================================
WRITE(LOGNAM,*) ""
WRITE(LOGNAM,*) "!---------------------!"

WRITE(LOGNAM,*) "CMF::PROG_INIT: prognostic variable initialization"

!*** 1. ALLOCATE 
!ND2PROG=10                       !! # of standard prognostic variables 
!IF ( LDAMOUT ) ND2PROG=ND2PROG+1 !! dam   variables are added (D2DAMSTO, B2DAMINF)
!IF ( LWEVAP  ) ND2PROG=ND2PROG+1 !! input evapolation (B2WEVAPs)

!! Always Double Precision
!ND2PROG_DBL=2                       !! # of standard prognostic variables 
!IF ( LDAMOUT ) ND2PROG_DBL=ND2PROG_DBL+1 !! dam   variables are added (D2DAMSTO, B2DAMINF)
!IF ( LLEVEE  ) ND2PROG_DBL=ND2PROG_DBL+1 !! levee variables are added (D2LEVSTO)

! runoff input
ALLOCATE( B2RUNOFF(NSEQMAX,1)     )
ALLOCATE( B2ROFSUB(NSEQMAX,1)     )

! river+floodplain storage
ALLOCATE( D2RIVSTO(NSEQMAX,1)     )
ALLOCATE( D2FLDSTO(NSEQMAX,1)     )

! discharge calculation
ALLOCATE( B2RIVOUT(NSEQMAX,1)     )
ALLOCATE( B2FLDOUT(NSEQMAX,1)     )
ALLOCATE( B2RIVOUT_PRE(NSEQMAX,1)     )
ALLOCATE( B2FLDOUT_PRE(NSEQMAX,1)     )
ALLOCATE( B2RIVDPH_PRE(NSEQMAX,1)     )
ALLOCATE( B2FLDSTO_PRE(NSEQMAX,1)     )

B2RUNOFF(:,:)=0._JPRB
B2ROFSUB(:,:)=0._JPRB

D2RIVSTO(:,:)=0._JPRD
D2FLDSTO(:,:)=0._JPRD

B2RIVOUT(:,:)=0._JPRB
B2FLDOUT(:,:)=0._JPRB
B2RIVOUT_PRE(:,:)=0._JPRB
B2FLDOUT_PRE(:,:)=0._JPRB
B2RIVDPH_PRE(:,:)=0._JPRB
B2FLDSTO_PRE(:,:)=0._JPRB

IF( LPTHOUT ) THEN  !! additional prognostics for bifurcation scheme
  ALLOCATE( B1PTHFLW(NPTHOUT,NPTHLEV)     )
  ALLOCATE( B1PTHFLW_PRE(NPTHOUT,NPTHLEV) )
  B1PTHFLW(:,:)=0._JPRB
  B1PTHFLW_PRE(:,:)=0._JPRB
ENDIF
IF( LDAMOUT ) THEN  !! additional prognostics for reservoir operation
  ALLOCATE( D2DAMSTO(NSEQMAX,1)     )
  ALLOCATE( B2DAMINF(NSEQMAX,1)     )
  D2DAMSTO(:,:)=0._JPRD
  B2DAMINF(:,:)=0._JPRB
ENDIF
IF( LLEVEE ) THEN  !! additional prognostics for LLEVEE
  ALLOCATE( D2LEVSTO(NSEQMAX,1)     )
  D2LEVSTO(:,:)=0._JPRD
ENDIF

!! Used in ECMWF
IF( LWEVAP ) THEN  !! additional prognostics for LLEVEE
  ALLOCATE( B2WEVAP(NSEQMAX,1)     )
  B2WEVAP(:,:)=0._JPRB
ENDIF

!! keep these variables even when LGDWDLY is not used.
ALLOCATE( B2GDWSTO(NSEQMAX,1)     )
ALLOCATE( B2GDWRTN(NSEQMAX,1)     )
B2GDWSTO(:,:)=0._JPRD
B2GDWRTN(:,:)=0._JPRB

!! dammy variable for data handling
ALLOCATE( D2JPRD(NSEQMAX,1)) !! always Float64
ALLOCATE( B2JPRB(NSEQMAX,1)) !! Float64/32 switch 
ALLOCATE( R2JPRM(NSEQMAX,1)) !! always Float32
D2JPRD(:,:)=0._JPRD
B2JPRB(:,:)=0._JPRB
R2JPRM(:,:)=0._JPRM

!============================
!***  2. set initial water surface elevation to sea surface level
WRITE(LOGNAM,*) 'PROG_INIT: fill channels below downstream boundary'
CALL STORAGE_SEA_SURFACE


WRITE(LOGNAM,*) "CMF::PROG_INIT: end"

CONTAINS
!==========================================================
!+ STORAGE_SEA_SURFACE: set initial storage, assuming water surface not lower than downstream sea surface elevation
!+
!+
! ==================================================
SUBROUTINE STORAGE_SEA_SURFACE
! set initial storage, assuming water surface not lower than downstream sea surface elevation
USE YOS_CMF_MAP,  ONLY: NSEQRIV,  NSEQALL,  I1NEXT
USE YOS_CMF_MAP,  ONLY: B2DWNELV, B2RIVELV,B2RIVHGT,B2RIVWTH,B2RIVLEN
IMPLICIT NONE
! local variables
INTEGER(KIND=JPIM)   :: ISEQ, JSEQ
!
REAL(KIND=JPRB),SAVE :: BSEAELV, BDPH
!$OMP THREADPRIVATE    (BSEAELV, BDPH)
!!=================
! For River Mouth Grid
!$OMP PARALLEL DO
DO ISEQ=NSEQRIV+1,NSEQALL
  BSEAELV=B2DWNELV(ISEQ,1) !! downstream boundary elevation

  !! set initial water level to sea level if river bed is lower than sea level
  BDPH=MAX( BSEAELV-B2RIVELV(ISEQ,1),0._JPRB )
  BDPH=MIN( BDPH,B2RIVHGT(ISEQ,1) )
  D2RIVSTO(ISEQ,1)=BDPH*B2RIVLEN(ISEQ,1)*B2RIVWTH(ISEQ,1)
  B2RIVDPH_PRE(ISEQ,1)=BDPH
END DO
!$OMP END PARALLEL DO

!! For Usual River Grid (from downstream to upstream). OMP cannot be applied
DO ISEQ=NSEQRIV,1, -1
  JSEQ=I1NEXT(ISEQ)
  BSEAELV=B2RIVELV(JSEQ,1)+B2RIVDPH_PRE(JSEQ,1)

  !! set initial water level to sea level if river bed is lower than sea level
  BDPH=MAX( BSEAELV-B2RIVELV(ISEQ,1),0._JPRB )
  BDPH=MIN( BDPH,B2RIVHGT(ISEQ,1) )

  D2RIVSTO(ISEQ,1)=BDPH*B2RIVLEN(ISEQ,1)*B2RIVWTH(ISEQ,1)
  B2RIVDPH_PRE(ISEQ,1)=BDPH
END DO

  
! old version before v4.02 (too slow)
!DO ISEQ=1, NSEQALL
!  JSEQ=ISEQ
!  DO WHILE( I1NEXT(JSEQ)>0 )
!    KSEQ=JSEQ
!    JSEQ=I1NEXT(KSEQ)
!  END DO
!
!  BSEAELV=B2DWNELV(JSEQ,1) !! downstream boundary elevation
!  !! set initial water level to sea level if river bed is lower than sea level
!  BDPH=MAX( BSEAELV-B2RIVELV(ISEQ,1),0._JPRB )
!  BDPH=MIN( BDPH,B2RIVHGT(ISEQ,1) )
!  D2RIVSTO(ISEQ,1)=BDPH*B2RIVLEN(ISEQ,1)*B2RIVWTH(ISEQ,1)
!END DO
    
END SUBROUTINE STORAGE_SEA_SURFACE
! ==================================================

END SUBROUTINE CMF_PROG_INIT
!####################################################################






!####################################################################
SUBROUTINE CMF_DIAG_INIT

USE YOS_CMF_MAP,        ONLY: NSEQMAX,NPTHOUT,NPTHLEV
USE YOS_CMF_PROG,       ONLY: B2JPRB
USE YOS_CMF_DIAG,       ONLY: N2DIAG, B2DIAG, &
                            &   B2RIVINF, B2RIVDPH, B2RIVVEL, B2FLDINF, B2FLDDPH, B2FLDFRC, B2FLDARE, &
                            &   B2PTHOUT, B2PTHINF, B2SFCELV, B2OUTFLW, B2STORGE, B2OUTINS, B2LEVDPH, &
                            &   B2WEVAPEX
USE YOS_CMF_DIAG,       ONLY: N2DIAG_AVG, B2DIAG_AVG, NADD, &
                            &   B2RIVOUT_AVG, B2FLDOUT_AVG, B2OUTFLW_AVG, B2RIVVEL_AVG, B2PTHOUT_AVG, &
                            &   B2GDWRTN_AVG, B2RUNOFF_AVG, B2ROFSUB_AVG, B1PTHFLW_AVG, B2WEVAPEX_AVG,&
                            &   B2DAMINF_AVG
USE YOS_CMF_DIAG,       ONLY: N2DIAG_MAX, B2DIAG_MAX, &
                            &   B2STORGE_MAX, B2OUTFLW_MAX, B2RIVDPH_MAX
IMPLICIT NONE
!*** LOCAL
INTEGER(KIND=JPIM),SAVE         :: IND
!================================================
WRITE(LOGNAM,*) ""
WRITE(LOGNAM,*) "!---------------------!"

WRITE(LOGNAM,*) "CMF::DIAG_INIT: initialize diagnostic variables"

!*** 1. snapshot 2D diagnostics
N2DIAG=12
IF ( LLEVEE  ) N2DIAG=N2DIAG+1 !! levee variables are added     (D2LEVSTO )
IF ( LWEVAP  ) N2DIAG=N2DIAG+1 !! evapolation added             (B2WEVAPEX) 
IF ( LOUTINS ) N2DIAG=N2DIAG+1 !! instantaneous discharge added (B2OUTINS )

ALLOCATE(B2DIAG(NSEQMAX,1,N2DIAG))
B2DIAG(:,:,:) = 0._JPRB
B2RIVINF => B2DIAG(:,:,1)
B2RIVDPH => B2DIAG(:,:,2)
B2RIVVEL => B2DIAG(:,:,3)
B2FLDINF => B2DIAG(:,:,4)
B2FLDDPH => B2DIAG(:,:,5)
B2FLDFRC => B2DIAG(:,:,6)
B2FLDARE => B2DIAG(:,:,7)
B2PTHOUT => B2DIAG(:,:,8)
B2PTHINF => B2DIAG(:,:,9)
B2SFCELV => B2DIAG(:,:,10)
B2OUTFLW => B2DIAG(:,:,11)
B2STORGE => B2DIAG(:,:,12)

IND=12
IF ( LLEVEE  )THEN
  IND=IND+1
  B2LEVDPH => B2DIAG(:,:,IND)
ELSE
  B2LEVDPH => B2JPRB(:,:)
ENDIF
IF ( LWEVAP  )THEN
  IND=IND+1
  B2WEVAPEX => B2DIAG(:,:,IND)
ELSE
  B2WEVAPEX => B2JPRB(:,:)
ENDIF
IF ( LOUTINS  )THEN
  IND=IND+1
  B2OUTINS => B2DIAG(:,:,IND)
ELSE
  B2OUTINS => B2JPRB(:,:)
ENDIF

!============================
!*** 2a. time-average 2D diagnostics
N2DIAG_AVG=8
IF ( LDAMOUT ) N2DIAG_AVG=N2DIAG_AVG+1    !!! B2DAMINF_AVG is added

ALLOCATE(B2DIAG_AVG(NSEQMAX,1,N2DIAG_AVG))
B2DIAG_AVG(:,:,:) = 0._JPRB 
B2RIVOUT_AVG => B2DIAG_AVG(:,:,1)
B2FLDOUT_AVG => B2DIAG_AVG(:,:,2)
B2OUTFLW_AVG => B2DIAG_AVG(:,:,3)
B2RIVVEL_AVG => B2DIAG_AVG(:,:,4)
B2PTHOUT_AVG => B2DIAG_AVG(:,:,5)

B2GDWRTN_AVG => B2DIAG_AVG(:,:,6)
B2RUNOFF_AVG => B2DIAG_AVG(:,:,7)
B2ROFSUB_AVG => B2DIAG_AVG(:,:,8)

IND=8
IF ( LDAMOUT ) THEN
  IND=IND+1
  B2DAMINF_AVG => B2DIAG_AVG(:,:,IND)
ELSE
  B2DAMINF_AVG => B2JPRB(:,:)
ENDIF
IF ( LWEVAP ) THEN
  IND=IND+1
  B2WEVAPEX_AVG => B2DIAG_AVG(:,:,IND)
ELSE
  B2WEVAPEX_AVG => B2JPRB(:,:)
ENDIF

NADD=0

!*** 2b time-average 1D Diagnostics (bifurcation channel)
ALLOCATE(B1PTHFLW_AVG(NPTHOUT,NPTHLEV))
B1PTHFLW_AVG(:,:) = 0._JPRB 

!============================
!*** 3. Maximum 2D Diagnostics 
N2DIAG_MAX=3

ALLOCATE(B2DIAG_MAX(NSEQMAX,1,N2DIAG_MAX))
B2DIAG_MAX(:,:,:) = 0._JPRB 
B2STORGE_MAX => B2DIAG_MAX(:,:,1)
B2OUTFLW_MAX => B2DIAG_MAX(:,:,2)
B2RIVDPH_MAX => B2DIAG_MAX(:,:,3)

WRITE(LOGNAM,*) "CMF::DIAG_INIT: end"

END SUBROUTINE CMF_DIAG_INIT
!####################################################################

END MODULE CMF_CTRL_VARS_MOD
