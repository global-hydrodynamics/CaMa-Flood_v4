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
USE YOS_CMF_PROG,            ONLY: D2RUNOFF,     D2ROFSUB,     &
                                 & P2RIVSTO,     P2FLDSTO,     D2RIVOUT,     D2FLDOUT,     &
                                 & D2RIVOUT_PRE, D2FLDOUT_PRE, D2RIVDPH_PRE, D2FLDSTO_PRE, &
                                 & D1PTHFLW,     D1PTHFLW_PRE, P2GDWSTO,     D2GDWRTN,     &
                                 & P2DAMSTO,     P2DAMINF,     P2LEVSTO ,    D2WEVAP,      &   !! optional
                                 & D2DAMMY,      D2COPY
IMPLICIT NONE
!================================================
WRITE(LOGNAM,*) ""
WRITE(LOGNAM,*) "!---------------------!"

WRITE(LOGNAM,*) "CMF::PROG_INIT: prognostic variable initialization"

!*** 1. ALLOCATE 
! runoff input
ALLOCATE( D2RUNOFF(NSEQMAX,1)     )
ALLOCATE( D2ROFSUB(NSEQMAX,1)     )

! river+floodplain storage
ALLOCATE( P2RIVSTO(NSEQMAX,1)     )
ALLOCATE( P2FLDSTO(NSEQMAX,1)     )

! discharge calculation
ALLOCATE( D2RIVOUT(NSEQMAX,1)     )
ALLOCATE( D2FLDOUT(NSEQMAX,1)     )
ALLOCATE( D2RIVOUT_PRE(NSEQMAX,1)     )
ALLOCATE( D2FLDOUT_PRE(NSEQMAX,1)     )
ALLOCATE( D2RIVDPH_PRE(NSEQMAX,1)     )
ALLOCATE( D2FLDSTO_PRE(NSEQMAX,1)     )

D2RUNOFF(:,:)=0._JPRB
D2ROFSUB(:,:)=0._JPRB

P2RIVSTO(:,:)=0._JPRD
P2FLDSTO(:,:)=0._JPRD

D2RIVOUT(:,:)=0._JPRB
D2FLDOUT(:,:)=0._JPRB
D2RIVOUT_PRE(:,:)=0._JPRB
D2FLDOUT_PRE(:,:)=0._JPRB
D2RIVDPH_PRE(:,:)=0._JPRB
D2FLDSTO_PRE(:,:)=0._JPRB

IF( LPTHOUT ) THEN  !! additional prognostics for bifurcation scheme
  ALLOCATE( D1PTHFLW(NPTHOUT,NPTHLEV)     )
  ALLOCATE( D1PTHFLW_PRE(NPTHOUT,NPTHLEV) )
  D1PTHFLW(:,:)=0._JPRB
  D1PTHFLW_PRE(:,:)=0._JPRB
ENDIF
IF( LDAMOUT ) THEN  !! additional prognostics for reservoir operation
  ALLOCATE( P2DAMSTO(NSEQMAX,1)     )
  ALLOCATE( P2DAMINF(NSEQMAX,1)     )
  P2DAMSTO(:,:)=0._JPRD
  P2DAMINF(:,:)=0._JPRD
ENDIF
IF( LLEVEE ) THEN  !! additional prognostics for LLEVEE
  ALLOCATE( P2LEVSTO(NSEQMAX,1)     )
  P2LEVSTO(:,:)=0._JPRD
ENDIF

!! Used in ECMWF
IF( LWEVAP ) THEN  !! additional prognostics for LLEVEE
  ALLOCATE( D2WEVAP(NSEQMAX,1)     )
  D2WEVAP(:,:)=0._JPRB
ENDIF

!! keep these variables even when LGDWDLY is not used.
ALLOCATE( P2GDWSTO(NSEQMAX,1)     )
ALLOCATE( D2GDWRTN(NSEQMAX,1)     )
P2GDWSTO(:,:)=0._JPRD
D2GDWRTN(:,:)=0._JPRB

!! dammy variable for data handling
ALLOCATE( D2DAMMY(NSEQMAX,1)) !! Float64/32 switch (Dammy for unused var)
ALLOCATE( D2COPY(NSEQMAX,1))  !! Float64/32 switch (Dammy for output)
D2DAMMY(:,:)=0._JPRB
D2COPY(:,:) =0._JPRB

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
USE YOS_CMF_MAP,  ONLY: D2DWNELV, D2RIVELV,D2RIVHGT,D2RIVWTH,D2RIVLEN,P2RIVSTOMAX
IMPLICIT NONE
! local variables
INTEGER(KIND=JPIM)   :: ISEQ, JSEQ
!
REAL(KIND=JPRB),SAVE :: DSEAELV, DDPH
!$OMP THREADPRIVATE    (DSEAELV, DDPH)
!!=================
! For River Mouth Grid
!$OMP PARALLEL DO
DO ISEQ=NSEQRIV+1,NSEQALL
  DSEAELV=D2DWNELV(ISEQ,1) !! downstream boundary elevation

  !! set initial water level to sea level if river bed is lower than sea level
  DDPH=MAX( DSEAELV-D2RIVELV(ISEQ,1),0._JPRB )
  DDPH=MIN( DDPH,D2RIVHGT(ISEQ,1) )
  P2RIVSTO(ISEQ,1)=DDPH*D2RIVLEN(ISEQ,1)*D2RIVWTH(ISEQ,1)
  P2RIVSTO(ISEQ,1)=MIN( P2RIVSTO(ISEQ,1),P2RIVSTOMAX(ISEQ,1) )
  D2RIVDPH_PRE(ISEQ,1)=DDPH
END DO
!$OMP END PARALLEL DO

!! For Usual River Grid (from downstream to upstream). OMP cannot be applied
DO ISEQ=NSEQRIV,1, -1
  JSEQ=I1NEXT(ISEQ)
  DSEAELV=D2RIVELV(JSEQ,1)+D2RIVDPH_PRE(JSEQ,1)

  !! set initial water level to sea level if river bed is lower than sea level
  DDPH=MAX( DSEAELV-D2RIVELV(ISEQ,1),0._JPRB )
  DDPH=MIN( DDPH,D2RIVHGT(ISEQ,1) )

  P2RIVSTO(ISEQ,1)=DDPH*D2RIVLEN(ISEQ,1)*D2RIVWTH(ISEQ,1)
  P2RIVSTO(ISEQ,1)=MIN(  P2RIVSTO(ISEQ,1),P2RIVSTOMAX(ISEQ,1) )
  D2RIVDPH_PRE(ISEQ,1)=DDPH
END DO

  
! old version before v4.02 (too slow)
!DO ISEQ=1, NSEQALL
!  JSEQ=ISEQ
!  DO WHILE( I1NEXT(JSEQ)>0 )
!    KSEQ=JSEQ
!    JSEQ=I1NEXT(KSEQ)
!  END DO
!
!  DSEAELV=D2DWNELV(JSEQ,1) !! downstream boundary elevation
!  !! set initial water level to sea level if river bed is lower than sea level
!  DDPH=MAX( DSEAELV-D2RIVELV(ISEQ,1),0._JPRB )
!  DDPH=MIN( DDPH,D2RIVHGT(ISEQ,1) )
!  P2RIVSTO(ISEQ,1)=DDPH*D2RIVLEN(ISEQ,1)*D2RIVWTH(ISEQ,1)
!END DO
    
END SUBROUTINE STORAGE_SEA_SURFACE
! ==================================================

END SUBROUTINE CMF_PROG_INIT
!####################################################################






!####################################################################
SUBROUTINE CMF_DIAG_INIT

USE YOS_CMF_MAP,        ONLY: NSEQMAX,NPTHOUT,NPTHLEV
USE YOS_CMF_DIAG,       ONLY: D2RIVINF, D2RIVDPH, D2RIVVEL, D2FLDINF, D2FLDDPH, D2FLDFRC, D2FLDARE, &
                            & D2PTHOUT, D2PTHINF, D2SFCELV, D2OUTFLW, D2STORGE, D2OUTINS, D2LEVDPH, &
                            & D2WEVAPEX
USE YOS_CMF_DIAG,       ONLY: D2RIVOUT_AVG, D2FLDOUT_AVG, D2OUTFLW_AVG, D2RIVVEL_AVG, D2PTHOUT_AVG, &
                            & D2GDWRTN_AVG, D2RUNOFF_AVG, D2ROFSUB_AVG, D1PTHFLW_AVG, D2WEVAPEX_AVG,&
                            & D2DAMINF_AVG, NADD
USE YOS_CMF_DIAG,       ONLY: D2STORGE_MAX, D2OUTFLW_MAX, D2RIVDPH_MAX
IMPLICIT NONE
!================================================
WRITE(LOGNAM,*) ""
WRITE(LOGNAM,*) "!---------------------!"

WRITE(LOGNAM,*) "CMF::DIAG_INIT: initialize diagnostic variables"

!*** 1. snapshot 2D diagnostics
ALLOCATE(D2RIVINF(NSEQMAX,1))
ALLOCATE(D2RIVDPH(NSEQMAX,1))
ALLOCATE(D2RIVVEL(NSEQMAX,1))
ALLOCATE(D2FLDINF(NSEQMAX,1))
ALLOCATE(D2FLDDPH(NSEQMAX,1))
ALLOCATE(D2FLDFRC(NSEQMAX,1))
ALLOCATE(D2FLDARE(NSEQMAX,1))
ALLOCATE(D2PTHOUT(NSEQMAX,1))
ALLOCATE(D2PTHINF(NSEQMAX,1))
ALLOCATE(D2SFCELV(NSEQMAX,1))
ALLOCATE(D2OUTFLW(NSEQMAX,1))
ALLOCATE(D2STORGE(NSEQMAX,1))
D2RIVINF(:,:)=0._JPRB
D2RIVDPH(:,:)=0._JPRB
D2RIVVEL(:,:)=0._JPRB
D2FLDINF(:,:)=0._JPRB
D2FLDDPH(:,:)=0._JPRB
D2FLDFRC(:,:)=0._JPRB
D2FLDARE(:,:)=0._JPRB
D2PTHOUT(:,:)=0._JPRB
D2PTHINF(:,:)=0._JPRB
D2SFCELV(:,:)=0._JPRB
D2OUTFLW(:,:)=0._JPRB
D2STORGE(:,:)=0._JPRB

IF ( LLEVEE  )THEN
  ALLOCATE(D2LEVDPH(NSEQMAX,1))
  D2LEVDPH(:,:)=0._JPRB
ENDIF
IF ( LWEVAP  )THEN
  ALLOCATE(D2WEVAPEX(NSEQMAX,1))
  D2WEVAPEX(:,:)=0._JPRB
ENDIF
IF ( LOUTINS  )THEN
  ALLOCATE(D2OUTINS (NSEQMAX,1))
  D2OUTINS (:,:)=0._JPRB
ENDIF

!============================
!*** 2a. time-average 2D diagnostics

ALLOCATE(D2RIVOUT_AVG(NSEQMAX,1))
ALLOCATE(D2FLDOUT_AVG(NSEQMAX,1))
ALLOCATE(D2OUTFLW_AVG(NSEQMAX,1))
ALLOCATE(D2RIVVEL_AVG(NSEQMAX,1))
ALLOCATE(D2PTHOUT_AVG(NSEQMAX,1))
ALLOCATE(D2GDWRTN_AVG(NSEQMAX,1))
ALLOCATE(D2RUNOFF_AVG(NSEQMAX,1))
ALLOCATE(D2ROFSUB_AVG(NSEQMAX,1))
D2RIVOUT_AVG(:,:)=0._JPRB
D2FLDOUT_AVG(:,:)=0._JPRB
D2OUTFLW_AVG(:,:)=0._JPRB
D2RIVVEL_AVG(:,:)=0._JPRB
D2PTHOUT_AVG(:,:)=0._JPRB
D2GDWRTN_AVG(:,:)=0._JPRB
D2RUNOFF_AVG(:,:)=0._JPRB
D2ROFSUB_AVG(:,:)=0._JPRB

IF ( LDAMOUT ) THEN
  ALLOCATE(D2DAMINF_AVG(NSEQMAX,1))
  D2DAMINF_AVG(:,:)=0._JPRB
ENDIF
IF ( LWEVAP ) THEN
  ALLOCATE(D2WEVAPEX_AVG(NSEQMAX,1))
  D2WEVAPEX_AVG(:,:)=0._JPRB
ENDIF

NADD=0

!*** 2b time-average 1D Diagnostics (bifurcation channel)
ALLOCATE(D1PTHFLW_AVG(NPTHOUT,NPTHLEV))
D1PTHFLW_AVG(:,:) = 0._JPRB 

!============================
!*** 3. Maximum 2D Diagnostics 

ALLOCATE(D2STORGE_MAX(NSEQMAX,1))
ALLOCATE(D2OUTFLW_MAX(NSEQMAX,1))
ALLOCATE(D2RIVDPH_MAX(NSEQMAX,1))
D2STORGE_MAX(:,:)=0._JPRB
D2OUTFLW_MAX(:,:)=0._JPRB
D2RIVDPH_MAX(:,:)=0._JPRB

WRITE(LOGNAM,*) "CMF::DIAG_INIT: end"

END SUBROUTINE CMF_DIAG_INIT
!####################################################################

END MODULE CMF_CTRL_VARS_MOD
