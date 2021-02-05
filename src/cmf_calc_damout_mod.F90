MODULE CMF_CALC_DAMOUT_MOD
!==========================================================
!* PURPOSE: CaMa-Flood reservoir operation scheme (under development)
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
! -- CMF_CALC_DAMOUT
! -- 1. replace outflow in 1-grid upstream grid of dams to kinematic discharge
! -- 2. reservoir operation
! -- 3. modify outflow
! -- 4. calculate reservoir storage

!####################################################################
SUBROUTINE CMF_CALC_DAMOUT
USE PARKIND1,           ONLY: JPIM, JPRB
USE YOS_CMF_MAP,        ONLY: I1NEXT,   I2VECTOR
USE YOS_CMF_PROG,       ONLY: D2RIVOUT, D2FLDOUT, D2RIVSTO, D2FLDSTO, &
                              & D2DAMSTO, d2daminf, d2runoff
USE YOS_CMF_DIAG,       ONLY: D2RIVINF, D2FLDINF
USE YOS_CMF_INPUT,      ONLY: DT, LOGNAM, NX, NY, lrestart

IMPLICIT NONE

!*** Local
INTEGER(KIND=JPIM)         :: iseq, jseq

!*** files
integer(kind=jpim)         :: damcsv, dam, ndams
character(len=256)         :: damfile
character(len=256)         :: dammapfile
integer(kind=jpim)         :: ios

!*** dam parameters
integer(kind=jpim)         :: grandid, ix, iy
real(kind=jprb)            :: lon, lat, upreal, fldsto_mcm, consto_mcm, totalsto_mcm, fldstouparea, Qf, Qn
real(kind=jprb)            :: emesto, fldsto, consto, norsto
character(len=256)         :: damname

!** dam variables
real(kind=jprb)            :: dinflow, d2rivout_pre, d2fldout_pre, stopre
REAL(KIND=JPRB)            :: doutflw     !! Total outflw 
REAL(KIND=JPRB)            :: drivred, Dfldred     !! river flow reduction, floodplain flow reduction
real(kind=jprb), allocatable, save     :: infmax(:), stomax(:)

integer(kind=jpim), save   :: initialization

!*** water balance
real(kind=jprb)            :: glbdamstopre, glbdamstonxt, glbdaminf, glbdamout, missing


!=====================================================
!! replace discharge in upstream grids with kinematic outflow

dammapfile = "../../etc/reservoir_operation/sample_data/updam_glb_15min.bin"  !!!!!!
call CMF_CALC_OUTFLW_KINE_UP_DAM


!=====================================================
!! read dam parameter file

damfile = '../../etc/reservoir_operation/sample_data/dam_params_glb_15min_ERA5Land.csv' 
ndams = 2169    !!!!number of dams

if (.not. allocated(infmax) ) allocate(infmax(ndams))
if (.not. allocated(stomax) ) allocate(stomax(ndams))

open(damcsv, file=trim(damfile), iostat=ios)
read(damcsv,*)   !skip header


!=====================================================
!! dam loop

do dam = 1, ndams
    read(damcsv,*) grandid, damname, lon, lat, upreal, ix, iy, fldsto_mcm, consto_mcm, totalsto_mcm, fldstouparea, Qn, Qf

    if (ix > NX .or. iy > NY ) cycle

    iseq = i2vector(ix,iy)
    jseq = i1next(iseq)

    !! storage parameter ---------------------------
    fldsto = fldsto_mcm * 1000000   ! from Million Cubic Meter to m3
    consto = consto_mcm * 1000000
    emesto = consto + fldsto * 0.8     ! storage to start emergency operation
    norsto = consto * 0.5    ! normal storage

    !! dam variable -----------------------------------
    dinflow = d2rivinf(iseq,1) + d2fldinf(iseq,1) + d2runoff(iseq,1)   !!!! inflow calculation
    d2daminf(iseq,1) = dinflow

    d2rivout_pre = d2rivout(iseq,1)
    d2fldout_pre = d2fldout(iseq,1)
    stopre = d2damsto(iseq,1)    

    !! initial dam storage ---------------------
    if ( initialization == 0 .and. .not. lrestart) then
        stopre = norsto
        d2damsto(iseq,1) = norsto
        if ( dam==1 ) then
            write(lognam,*) "read dam file:", trim(damfile)
            write(lognam,*) "d2damsto is initialized"
        endif
    endif

    
    !! dam operation ------------------------------

    !! impoundment
    if ( stopre <= norsto ) then
        doutflw = Qn * (stopre / consto)
    
    !! water supply
    else if ( norsto < stopre .and. stopre <= consto) then

        if ( Qf <= dinflow ) then
            doutflw = Qn*0.5 + (stopre-norsto)/(consto-norsto) * (Qf-Qn)

        else
            doutflw = Qn*0.5 + (((stopre-norsto)/(emesto-norsto))**2)* (Qf - Qn)

        endif
    
    !! flood control
    else if ( consto < stopre .and. stopre < emesto ) then

        if ( Qf <= dinflow ) then
            doutflw = Qf + max((1.0-fldstouparea/0.2),0.0) * (stopre-consto)/(emesto-consto) * (dinflow-Qf)
        
        !! pre- and after flood control
        else
            doutflw = Qn*0.5 + (((stopre-norsto)/(emesto-norsto))**2)* (Qf - Qn)

        endif

    !! emergency operation
    else
        doutflw = max(dinflow, Qf)

    endif


    doutflw = min( doutflw, stopre / DT, ( D2RIVSTO(ISEQ,1) + D2FLDSTO(ISEQ,1) ) / DT )

    if ( doutflw < 0) then
        doutflw = 0
    endif

    d2rivout(iseq,1) = doutflw
    d2fldout(iseq,1) = 0
    
    drivred = d2rivout_pre - doutflw
    dfldred = d2fldout_pre
    
    d2rivinf(jseq,1) = d2rivinf(jseq,1) - drivred
    d2fldinf(jseq,1) = d2fldinf(jseq,1) - dfldred

enddo

close(damcsv)

if ( initialization == 0 ) then
    initialization = initialization + 1
endif


call cmf_modify_outflw


!! update reservoir storage and check water missing --------------------------

glbdamstopre = 0.D0
glbdamstonxt = 0.D0
glbdaminf = 0.D0
glbdamout = 0.D0

open(damcsv, file=trim(damfile), iostat=ios)
read(damcsv,*)   !skip header
do dam = 1, ndams
  
  read(damcsv,*) grandid, damname, lon, lat, upreal, ix, iy, fldsto_mcm, consto_mcm, totalsto_mcm, fldstouparea, Qn, Qf
  
  if (ix > NX .or. iy > NY ) cycle
  iseq = i2vector(ix,iy)

  dinflow = d2rivinf(iseq,1) + d2fldinf(iseq,1) + d2runoff(iseq,1)
  d2daminf(iseq,1) = dinflow
  doutflw = d2rivout(iseq,1) + d2fldout(iseq,1)
    
  glbdamstopre = glbdamstopre + d2damsto(iseq,1)
  glbdaminf = glbdaminf + d2daminf(iseq,1)*dt
  glbdamout = glbdamout + doutflw*dt

  d2damsto(iseq,1) = d2damsto(iseq,1) + dinflow * dt - doutflw * dt

  glbdamstonxt = glbdamstonxt + d2damsto(iseq,1)

enddo
close(damcsv)

missing = glbdamstopre-glbdamstonxt+glbdaminf-glbdamout
write(lognam,*) "missing at all dams:", missing*1.D-9

!========================================================

contains

!####################################################################
SUBROUTINE CMF_CALC_OUTFLW_KINE_UP_DAM
! Calculate discharge, mix kinematic & local inertial, depending on slope
! only in upstream grids of dams
USE PARKIND1,           ONLY: JPIM, JPRB
USE YOS_CMF_INPUT,      ONLY: DT,       LBITSAFE, PMANFLD,  PMINSLP, nx, ny, lognam
USE YOS_CMF_MAP,        ONLY: I1NEXT,   NSEQRIV, i2vector
USE YOS_CMF_MAP,        ONLY: D2RIVELV, D2ELEVTN, D2NXTDST, D2RIVWTH
USE YOS_CMF_MAP,        ONLY: D2RIVLEN, D2RIVMAN
USE YOS_CMF_PROG,       ONLY: D2RIVSTO, D2RIVOUT, D2FLDSTO, D2FLDOUT
USE YOS_CMF_DIAG,       ONLY: D2RIVDPH, D2RIVVEL, D2RIVINF, D2FLDDPH,D2FLDINF, D2SFCELV
IMPLICIT NONE
!$ SAVE
INTEGER(KIND=JPIM)   ::  ISEQ, JSEQ
REAL(KIND=JPRB)      ::  DSLOPE,   DAREA , DVEL,   DSLOPE_F, DARE_F, DVEL_F

integer(kind=jpim), allocatable, save     :: updamseqx(:), updamseqy(:)
integer(kind=jpim), allocatable     :: updammap(:,:)
integer(kind=jpim), save   :: initialization2, nsequpdam
integer(kind=jpim)   ::  tmpnamdam, iy, ix, updamseq

!$OMP THREADPRIVATE     (ix, iy, iseq, JSEQ, DSLOPE,   DAREA , DVEL,   DSLOPE_F, DARE_F, DVEL_F)

!================================================
!*** 0. read dam upstream grids map and convert 2D map into 1D sequence

if ( initialization2 == 0 ) then

    write(lognam,*) "start reading updammap"

    allocate ( updammap(nx,ny) )
    allocate ( updamseqx(nseqriv) )
    allocate ( updamseqy(nseqriv) )

    open(tmpnamdam, file=trim(dammapfile), form='unformatted', access='direct', recl=4*NX*NY)
    write(lognam,*) "updammap successfully opened:", trim(dammapfile)
    read(tmpnamdam, rec=1) updammap(:,:)
    write(lognam,*) "updammap successfully read   max=", maxval(updammap)
    close(tmpnamdam)

    nsequpdam = 0
    updamseq = 0 
    do iy=1, ny
        do ix=1, nx
            if ( updammap(ix,iy) == 10 ) then
                updamseq = updamseq+1
                updamseqx(updamseq) = ix
                updamseqy(updamseq) = iy
            endif
        end do
    end do

    nsequpdam = updamseq
    write(lognam,*) ""
    write(lognam,*) "NSEQRIV=", nseqriv, "nsequpdam=", nsequpdam
    write(lognam,*) ""

    deallocate(updammap)
    write(lognam,*) "end of part 0"

    initialization2 = initialization2 + 1
  
endif


!===========================    
!*** 1. calculate surface water elevation, reset inflow

!$OMP PARALLEL DO
DO updamseq=1, nsequpdam
  ix = updamseqx(updamseq)
  iy = updamseqy(updamseq)
  iseq = i2vector(ix,iy)

  D2SFCELV(ISEQ,1)     = D2RIVELV(ISEQ,1) + D2RIVDPH(ISEQ,1)

  jseq = i1next(iseq)
  D2RIVINF(jseq,1) = 0.D0
  D2FLDINF(jseq,1) = 0.D0
  d2rivout(iseq,1) = 0.D0
  d2fldout(iseq,1) = 0.D0

END DO
!$OMP END PARALLEL DO


!============================
!*** 2. discharge for upstream grids of dams

!$OMP PARALLEL DO
DO updamseq=1, nsequpdam
  ix = updamseqx(updamseq)
  iy = updamseqy(updamseq)
  iseq = i2vector(ix,iy)

  if ( iseq <= nseqriv ) then
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

  endif

END DO
!$OMP END PARALLEL DO


!============================
!*** 4. update D2STOOUT and modify outflow to avoid negative storage
 
call cmf_modify_outflw


END SUBROUTINE CMF_CALC_OUTFLW_KINE_UP_DAM
!#######################################################################


!####################################################################
SUBROUTINE CMF_MODIFY_OUTFLW
  ! modify outflow in order to avoid negative storage
  USE PARKIND1,           ONLY: JPIM, JPRB
  USE YOS_CMF_INPUT,      ONLY: DT,       LBITSAFE, lognam
  USE YOS_CMF_MAP,        ONLY: I1NEXT,   NSEQALL,  NSEQRIV, nseqmax
  USE YOS_CMF_PROG,       ONLY: D2RIVSTO, D2RIVOUT, D2FLDSTO, D2FLDOUT
  use yos_cmf_prog,       only: d2rivout_pre, d2rivdph_pre, d2fldout_pre, d2fldsto_pre
  USE YOS_CMF_DIAG,       ONLY: D2RIVDPH, D2RIVINF, D2FLDDPH,D2FLDINF
  IMPLICIT NONE
  !$ SAVE
  INTEGER(KIND=JPIM)   ::  ISEQ, JSEQ
  
  REAL(KIND=JPRB)            :: OUT_R1, OUT_R2, OUT_F1, OUT_F2, DIUP, DIDW
  REAL(KIND=JPRB)            :: D2STOOUT(NSEQMAX,1)                      !! total outflow from a grid     [m3]
  REAL(KIND=JPRB)            :: D2RATE(NSEQMAX,1)                        !! outflow correction

  
  !$OMP THREADPRIVATE     (iseq, JSEQ)
  !$OMP THREADPRIVATE    (OUT_R1, OUT_R2, OUT_F1, OUT_F2, DIUP, DIDW)
  
!================================================
  

!============================
!*** 1. initialize & calculate D2STOOUT for normal cells

!$OMP PARALLEL DO
do iseq=1, nseqall
    D2RIVINF(ISEQ,1) = 0.D0
    D2FLDINF(ISEQ,1) = 0.D0
    D2STOOUT(ISEQ,1) = 0.D0
    D2RATE(ISEQ,1) = 1._JPRB
end do
!$OMP END PARALLEL DO

!! for normal cells ---------
IF( LBITSAFE )THEN  !! Avoid OMP ATOMIC for bit identical simulation
  DO ISEQ=1, NSEQRIV                                                    !! for normalcells

    JSEQ=I1NEXT(ISEQ) ! next cell's pixel
    OUT_R1 = max(  D2RIVOUT(ISEQ,1),0.D0 )
    OUT_R2 = max( -D2RIVOUT(ISEQ,1),0.D0 )
    OUT_F1 = max(  D2FLDOUT(ISEQ,1),0.D0 )
    OUT_F2 = max( -D2FLDOUT(ISEQ,1),0.D0 )
    DIUP=(OUT_R1+OUT_F1)*DT
    DIDW=(OUT_R2+OUT_F2)*DT
    D2STOOUT(ISEQ,1) = D2STOOUT(ISEQ,1) + DIUP
    D2STOOUT(JSEQ,1) = D2STOOUT(JSEQ,1) + DIDW
  END DO
ELSE
!$OMP PARALLEL DO
  DO ISEQ=1, NSEQRIV                                                    !! for normalcells

    D2RIVINF(ISEQ,1) = 0.D0
    D2FLDINF(ISEQ,1) = 0.D0
    D2STOOUT(ISEQ,1) = 0.D0
    D2RATE(ISEQ,1) = 1._JPRB

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
!$OMP END PARALLEL DO
ENDIF


!! for river mouth grids ------------
!$OMP PARALLEL DO
do iseq=nseqriv+1, nseqall
    out_r1 = max( d2rivout(iseq,1), 0.D0 )
    out_f1 = max( d2fldout(iseq,1), 0.D0 )
    d2stoout(iseq,1) = d2stoout(iseq,1) + out_r1*dt + out_f1*dt
end do
!$OMP END PARALLEL DO


!============================
!*** 2. modify outflow

!$OMP PARALLEL DO
do iseq=1, nseqall
    IF ( D2STOOUT(ISEQ,1) > 1.D-8 ) THEN
      D2RATE(ISEQ,1)   = min( (D2RIVSTO(ISEQ,1)+D2FLDSTO(ISEQ,1)) * D2STOOUT(ISEQ,1)**(-1.D0), 1.D0 )
    ENDIF
end do
!$OMP END PARALLEL DO


!! normal pixels------
IF( LBITSAFE )THEN  !! Aboid OMP ATOMIC for bit identical simulation
  DO ISEQ=1, NSEQRIV ! for normal pixels
    JSEQ=I1NEXT(ISEQ)
    IF( D2RIVOUT(ISEQ,1) >= 0.D0 )THEN
      D2RIVOUT(ISEQ,1) = D2RIVOUT(ISEQ,1)*D2RATE(ISEQ,1)
      D2FLDOUT(ISEQ,1) = D2FLDOUT(ISEQ,1)*D2RATE(ISEQ,1)
    ELSE
      D2RIVOUT(ISEQ,1) = D2RIVOUT(ISEQ,1)*D2RATE(JSEQ,1)
      D2FLDOUT(ISEQ,1) = D2FLDOUT(ISEQ,1)*D2RATE(JSEQ,1)
    ENDIF
    D2RIVINF(JSEQ,1) = D2RIVINF(JSEQ,1) + D2RIVOUT(ISEQ,1)             !! total inflow to a grid (from upstream)
    D2FLDINF(JSEQ,1) = D2FLDINF(JSEQ,1) + D2FLDOUT(ISEQ,1)

    D2RIVOUT_PRE(ISEQ,1)=D2RIVOUT(ISEQ,1)                              !! save outflow (t)
    D2RIVDPH_PRE(ISEQ,1)=D2RIVDPH(ISEQ,1)                              !! save depth   (t)
    D2FLDOUT_PRE(ISEQ,1)=D2FLDOUT(ISEQ,1)                              !! save outflow (t)
    D2FLDSTO_PRE(ISEQ,1)=D2FLDSTO(ISEQ,1)
  END DO
ELSE
!$OMP PARALLEL DO
  DO ISEQ=1, NSEQRIV ! for normal pixels

    IF ( D2STOOUT(ISEQ,1) > 1.D-8 ) THEN
      D2RATE(ISEQ,1)   = min( (D2RIVSTO(ISEQ,1)+D2FLDSTO(ISEQ,1)) * D2STOOUT(ISEQ,1)**(-1.D0), 1.D0 )
    ENDIF

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

    D2RIVOUT_PRE(ISEQ,1)=D2RIVOUT(ISEQ,1)                              !! save outflow (t)
    D2RIVDPH_PRE(ISEQ,1)=D2RIVDPH(ISEQ,1)                              !! save depth   (t)
    D2FLDOUT_PRE(ISEQ,1)=D2FLDOUT(ISEQ,1)                              !! save outflow (t)
    D2FLDSTO_PRE(ISEQ,1)=D2FLDSTO(ISEQ,1)
  END DO
!$OMP END PARALLEL DO
ENDIF

!! river mouth-----------------
!$OMP PARALLEL DO
do iseq=nseqriv+1, nseqall
    d2rivout(iseq,1) = d2rivout(iseq,1)*d2rate(iseq,1)
    d2fldout(iseq,1) = d2fldout(iseq,1)*d2rate(iseq,1)
end do
!$OMP END PARALLEL DO


D2RIVOUT_PRE(:,1)=D2RIVOUT(:,1)                              !! save outflow (t)
D2RIVDPH_PRE(:,1)=D2RIVDPH(:,1)                              !! save depth   (t)
D2FLDOUT_PRE(:,1)=D2FLDOUT(:,1)                              !! save outflow (t)
D2FLDSTO_PRE(:,1)=D2FLDSTO(:,1)


END SUBROUTINE CMF_MODIFY_OUTFLW
!#######################################################################


END SUBROUTINE CMF_CALC_DAMOUT
!####################################################################

END MODULE CMF_CALC_DAMOUT_MOD
