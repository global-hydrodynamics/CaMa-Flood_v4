MODULE CMF_CTRL_MAPS_MOD
!==========================================================
!* PURPOSE: Control CaMa-Flood map/topography data
!
!* CONTAINS:
! -- CMF_MAPS_NMLIST   : configuration from namelist
! -- CMF_RIVMAP_INIT  : read & set river network map 
! -- CMF_TOPO_INIT    : read & set topography
!
! (C) D.Yamazaki & E. Dutra  (U-Tokyo/FCUL)  Nov 2026
!
! Licensed under the Apache License, Version 2.0 (the "License");
!   You may not use this file except in compliance with the License.
!   You may obtain a copy of the License at: http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software distributed under the License is 
!  distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
! See the License for the specific language governing permissions and limitations under the License.
!==========================================================
! shared variables in module
USE PARKIND1,                ONLY: JPIM, JPRB, JPRD, JPRM
USE YOS_CMF_INPUT,           ONLY: LOGNAM
IMPLICIT NONE

SAVE
!*** Map input settings (registered in NAMELIST/NMAP/)
!*** NAMELIST/NMAP/: Individual map files (Binary or NetCDF)
CHARACTER(LEN=256)              :: CNEXTXY         !! downstream cell indices (nextx, nexty)
CHARACTER(LEN=256)              :: CGRAREA         !! upstream catchment area
CHARACTER(LEN=256)              :: CELEVTN         !! riverbank elevation
CHARACTER(LEN=256)              :: CNXTDST         !! distance to downstream cell
CHARACTER(LEN=256)              :: CRIVLEN         !! river channel length
CHARACTER(LEN=256)              :: CFLDHGT         !! floodplain elevation profile
!* River channel parameters
CHARACTER(LEN=256)              :: CRIVWTH         !! river channel width
CHARACTER(LEN=256)              :: CRIVHGT         !! river channel depth
CHARACTER(LEN=256)              :: CRIVMAN         !! Manning's roughness coefficient
!* Optional map inputs
CHARACTER(LEN=256)              :: CPTHOUT         !! bifurcation channel parameters (text)
CHARACTER(LEN=256)              :: CGDWDLY         !! groundwater delay parameter
CHARACTER(LEN=256)              :: CMEANSL         !! mean sea level elevation
!* MPI domain decomposition
CHARACTER(LEN=256)              :: CMPIREG         !! MPI region ID for each grid cell
!* Legacy consolidated NetCDF map inputs
LOGICAL                         :: LMAPCDF         !! true: legacy NetCDF; false: individual files
CHARACTER(LEN=256)              :: CRIVCLINC       !! consolidated river network and topography
CHARACTER(LEN=256)              :: CRIVPARNC       !! consolidated channel and optional parameters
CHARACTER(LEN=256)              :: CMEANSLNC       !! mean sea level (legacy NetCDF)
CHARACTER(LEN=256)              :: CMPIREGNC       !! MPI region map (legacy NetCDF)

!*** Register map input variables to NAMELIST/NMAP/
NAMELIST/NMAP/     CNEXTXY,  CGRAREA,  CELEVTN,  CNXTDST, CRIVLEN, CFLDHGT, &
                   CRIVWTH,  CRIVHGT,  CRIVMAN,  CPTHOUT, CGDWDLY, CMEANSL, &
                   CMPIREG,  LMAPCDF,  CRIVCLINC,CRIVPARNC,CMEANSLNC,CMPIREGNC


CONTAINS
!####################################################################
! Principal model procedures
! -- CMF_MAPS_NMLIST : Read map input settings from the namelist
! -- CMF_RIVMAP_INIT : Initialize the river network and computation sequence
! -- CMF_TOPO_INIT   : Initialize topography and derived map parameters
!
! Shared module-level utilities
! -- MAP_UTIL_INPUT_ERROR : Report a fatal Map input error
! -- MAP_UTIL_INPUT_KIND  : Identify an individual Map file format
! -- MAP_UTIL_READ_REAL2D : Read an individual real-valued Binary/NetCDF raster
! -- MAP_UTIL_READ_INT2D  : Read an individual integer-valued Binary/NetCDF raster
! -- MAP_UTIL_NCERROR     : Report a contextual NetCDF error and stop (UseCDF_CMF)
!####################################################################
SUBROUTINE CMF_MAPS_NMLIST
! Read Map input settings from the model namelist.
! -- Called from CMF_DRV_NMLIST
USE YOS_CMF_INPUT,      ONLY: CSETFILE,NSETFILE,LMEANSL,LGDWDLY
USE CMF_UTILS_MOD,      ONLY: INQUIRE_FID
IMPLICIT NONE
!================================================
!*** 1. open namelist
WRITE(LOGNAM,*) ""
WRITE(LOGNAM,*) "!---------------------!"
WRITE(LOGNAM,*) 'CMF::MAPS_NMLIST: START read Map input settings'

NSETFILE=INQUIRE_FID()
OPEN(NSETFILE,FILE=CSETFILE,STATUS="OLD")
WRITE(LOGNAM,*) "CMF::MAPS_NMLIST: namelist OPEN in unit: ", TRIM(CSETFILE), NSETFILE

!*** 2. default value
CNEXTXY="./nextxy.bin"
CGRAREA="./ctmare.bin"
CELEVTN="./elevtn.bin"
CNXTDST="./nxtdst.bin"
CRIVLEN="./rivlen.bin"
CFLDHGT="./fldhgt.bin"

CRIVWTH="./rivwth.bin"
CRIVHGT="./rivhgt.bin"
CRIVMAN="./rivman.bin"

CPTHOUT="./bifprm.txt"
CGDWDLY="NONE"
CMEANSL="NONE"

CMPIREG="NONE"

LMAPCDF=.FALSE.
CRIVCLINC="NONE"
CRIVPARNC="NONE"
CMEANSLNC="NONE"
CMPIREGNC="NONE"

!*** 3. read namelist
REWIND(NSETFILE)
READ(NSETFILE,NML=NMAP)

#ifndef UseCDF_CMF
IF ( LMAPCDF ) THEN
  WRITE(LOGNAM,*) 'ERROR [CMF::MAPS_NMLIST]: LMAPCDF=.TRUE. requests legacy consolidated NetCDF Map input, but this executable was built without UseCDF_CMF.'
  STOP 9
ENDIF
#endif

WRITE(LOGNAM,*)     "CMF::MAPS_NMLIST: NMAP settings"
WRITE(LOGNAM,*)     "  LMAPCDF:   ", LMAPCDF
IF( LMAPCDF )THEN
  WRITE(LOGNAM,*)   "  CRIVCLINC: ", TRIM(CRIVCLINC)
  WRITE(LOGNAM,*)   "  CRIVPARNC: ", TRIM(CRIVPARNC)
  IF( LMEANSL ) THEN
    WRITE(LOGNAM,*) "  CMEANSLNC: ", TRIM(CMEANSLNC)
  ENDIF
#ifdef UseMPI_CMF
    WRITE(LOGNAM,*) "  CMPIREGNC:   ", TRIM(CMPIREGNC)
#endif
ELSE
  WRITE(LOGNAM,*)   "  CNEXTXY:   ", TRIM(CNEXTXY)
  WRITE(LOGNAM,*)   "  CGRAREA:   ", TRIM(CGRAREA)
  WRITE(LOGNAM,*)   "  CELEVTN:   ", TRIM(CELEVTN)
  WRITE(LOGNAM,*)   "  CNXTDST:   ", TRIM(CNXTDST)
  WRITE(LOGNAM,*)   "  CRIVLEN:   ", TRIM(CRIVLEN)
  WRITE(LOGNAM,*)   "  CFLDHGT:   ", TRIM(CFLDHGT)

  WRITE(LOGNAM,*)   "  CRIVWTH:   ", TRIM(CRIVWTH)
  WRITE(LOGNAM,*)   "  CRIVHGT:   ", TRIM(CRIVHGT)
  WRITE(LOGNAM,*)   "  CRIVMAN:   ", TRIM(CRIVMAN)

  WRITE(LOGNAM,*)   "  CPTHOUT:   ", TRIM(CPTHOUT)
  IF( LGDWDLY )THEN
    WRITE(LOGNAM,*) "  CGDWDLY:    ",TRIM(CGDWDLY)
  ENDIF
  IF( LMEANSL )THEN
    WRITE(LOGNAM,*) "  CMEANSL:   ", TRIM(CMEANSL)
  ENDIF
#ifdef UseMPI_CMF
    WRITE(LOGNAM,*) "  CMPIREG:   ", TRIM(CMPIREG)
#endif
ENDIF

CLOSE(NSETFILE)

WRITE(LOGNAM,*) 'CMF::MAPS_NMLIST: END'
WRITE(LOGNAM,*) "!---------------------!"

END SUBROUTINE CMF_MAPS_NMLIST
!####################################################################





!####################################################################
SUBROUTINE CMF_RIVMAP_INIT
! Initialize the river network, partition, computation sequence, and bifurcations.
!
! Internal subroutines:
!+ READ_MAP_FILES : Read the individual river-network map and derive coordinates
!+ READ_MAP_CDF   : Read the legacy consolidated NetCDF river-network map
!+ CALC_REGION    : Assign cells to computational regions
!+ CALC_1D_SEQ    : Construct the upstream-to-downstream computation sequence
!+ READ_BIFPARAM  : Read bifurcation pathways and build per-cell pathway lists
USE YOS_CMF_INPUT,      ONLY: TMPNAM, NX,NY,NLFP, LPTHOUT
USE YOS_CMF_MAP,        ONLY: I2NEXTX,I2NEXTY, I2REGION, REGIONALL,REGIONTHIS, &
                            & I1SEQX, I1SEQY,  I1NEXT,  I2VECTOR, D1LON,    D1LAT,      &
                            & NSEQRIV,NSEQALL, NSEQMAX, NPTHOUT,  NPTHLEV
USE YOS_CMF_MAP,        ONLY: I1UPST, I1UPN, I1P_OUT, I1P_OUTN, I1P_INF, I1P_INFN
USE CMF_UTILS_MOD,      ONLY: INQUIRE_FID
IMPLICIT NONE
!================================================
WRITE(LOGNAM,*) ""
WRITE(LOGNAM,*) "!---------------------!"
WRITE(LOGNAM,*) 'CMF::RIVMAP_INIT: START river network initialization'

! *** 1. ALLOCATE ARRAYS
ALLOCATE( I2NEXTX(NX,NY) )
ALLOCATE( I2NEXTY(NX,NY) )
ALLOCATE( I2REGION(NX,NY) )
ALLOCATE( D1LON(NX) )
ALLOCATE( D1LAT(NY) )

!============================
!*** 2a. read river network map
WRITE(LOGNAM,*) 'CMF::RIVMAP_INIT: read nextXY & set lat lon'
IF( LMAPCDF )THEN
#ifdef UseCDF_CMF
  CALL READ_MAP_CDF
#else
  WRITE(LOGNAM,*) 'ERROR [CMF::RIVMAP_INIT]: legacy consolidated NetCDF Map input requires a UseCDF_CMF build.'
  STOP 9
#endif
ELSE
  CALL READ_MAP_FILES
ENDIF

!*** 2b. calculate river sequence & regions
WRITE(LOGNAM,*) 'CMF::RIVMAP_INIT: calc region'
CALL CALC_REGION

!============================
!*** 3. conversion 2D map -> 1D vector
WRITE(LOGNAM,*) 'CMF::RIVMAP_INIT: calculate 1d river sequence'

CALL CALC_1D_SEQ                                  !! 2D map to 1D vector conversion. for faster calculation

WRITE(LOGNAM,*) 'CMF::RIVMAP_INIT: NSEQRIV=',NSEQRIV
WRITE(LOGNAM,*) 'CMF::RIVMAP_INIT: NSEQALL=',NSEQALL
WRITE(LOGNAM,*) 'CMF::RIVMAP_INIT: NSEQMAX=',NSEQMAX

!*** 3c. Write Map Data                                       !! used for combining mpi distributed output into one map
IF( REGIONTHIS==1 )THEN
  TMPNAM=INQUIRE_FID()
  OPEN(TMPNAM,FILE='./mapdata.txt',FORM='FORMATTED')
  WRITE(TMPNAM,*) 'NX',        NX
  WRITE(TMPNAM,*) 'NY',        NY
  WRITE(TMPNAM,*) 'NLFP',      NLFP
  WRITE(TMPNAM,*) 'REGIONALL', REGIONALL
  WRITE(TMPNAM,*) 'NSEQMAX',   NSEQMAX
  CLOSE(TMPNAM)
ENDIF

!============================
!*** 4.  bifurcation channel parameters
NPTHOUT=1  !! tentative number
NPTHLEV=1
IF( LPTHOUT )THEN
  WRITE(LOGNAM,*) 'CMF::RIVMAP_INIT: read bifurcation channel setting'
  CALL READ_BIFPARAM
ENDIF

DEALLOCATE( I2NEXTX,I2NEXTY,I2REGION )

WRITE(LOGNAM,*) 'CMF::RIVMAP_INIT: END'
WRITE(LOGNAM,*) "!---------------------!"

CONTAINS
!==========================================================
!+ READ_MAP_FILES : Read the individual river-network map and derive coordinates
!+ READ_MAP_CDF   : Read the legacy consolidated NetCDF river-network map
!+ CALC_REGION    : Assign cells to computational regions
!+ CALC_1D_SEQ    : Construct the upstream-to-downstream computation sequence
!+ READ_BIFPARAM  : Read bifurcation pathways and build per-cell pathway lists
!==========================================================
SUBROUTINE READ_MAP_FILES
! Read river connectivity from an individual Binary or NetCDF file.
USE YOS_CMF_INPUT,      ONLY: WEST,EAST,NORTH,SOUTH
IMPLICIT NONE
!* local variables
INTEGER(KIND=JPIM),SAVE    :: IX,IY
!==========================================================
WRITE(LOGNAM,*) '  READ_MAP_FILES: read nextxy from ',TRIM(CNEXTXY)
CALL MAP_UTIL_READ_INT2D(CNEXTXY,'nextx',I2NEXTX,1,'CMF::RIVMAP_INIT')
CALL MAP_UTIL_READ_INT2D(CNEXTXY,'nexty',I2NEXTY,2,'CMF::RIVMAP_INIT')

! Derive cell-center coordinates for regular geographic grids.
! Skip non-geographic maps based on their coordinate bounds.
IF( WEST>=-180._JPRB .and. EAST<=360._JPRB .and. &
    SOUTH>=-90._JPRB .and. NORTH<=90._JPRB )THEN
  !$OMP PARALLEL DO
  DO IX=1,NX
    D1LON(IX)=WEST +(REAL(IX,KIND=JPRB)-0.5_JPRB)*(EAST-WEST)  /REAL(NX,KIND=JPRB)
  ENDDO
  !$OMP END PARALLEL DO
  !$OMP PARALLEL DO
  DO IY=1,NY
    D1LAT(IY)=NORTH-(REAL(IY,KIND=JPRB)-0.5_JPRB)*(NORTH-SOUTH)/REAL(NY,KIND=JPRB)
  ENDDO
  !$OMP END PARALLEL DO
ENDIF

END SUBROUTINE READ_MAP_FILES
!==========================================================
!+
!+
!+
!==========================================================
#ifdef UseCDF_CMF
SUBROUTINE READ_MAP_CDF
! Read connectivity and coordinates from the legacy consolidated NetCDF file.
USE NETCDF
IMPLICIT NONE
!* local variables
INTEGER(KIND=JPIM)              :: NCID,VARID
!================================================
WRITE(LOGNAM,*)'  READ_MAP_CDF: read nextxy from ', TRIM(CRIVCLINC)

CALL MAP_UTIL_NCERROR (NF90_OPEN(CRIVCLINC,NF90_NOWRITE,NCID),'opening '//TRIM(CRIVCLINC), &
                       'CMF::RIVMAP_INIT/READ_MAP_CDF')

!*** next xy
CALL MAP_UTIL_NCERROR ( NF90_INQ_VARID(NCID,'nextx',VARID),'getting id', 'CMF::RIVMAP_INIT/READ_MAP_CDF' )
CALL MAP_UTIL_NCERROR ( NF90_GET_VAR(NCID,VARID,I2NEXTX),'reading nextx', 'CMF::RIVMAP_INIT/READ_MAP_CDF' )

CALL MAP_UTIL_NCERROR ( NF90_INQ_VARID(NCID,'nexty',VARID),'getting id', 'CMF::RIVMAP_INIT/READ_MAP_CDF' )
CALL MAP_UTIL_NCERROR ( NF90_GET_VAR(NCID,VARID,I2NEXTY),'reading nexty', 'CMF::RIVMAP_INIT/READ_MAP_CDF' )

!*** lat, lon
CALL MAP_UTIL_NCERROR ( NF90_INQ_VARID(NCID,'lat',VARID),'getting id', 'CMF::RIVMAP_INIT/READ_MAP_CDF' )
CALL MAP_UTIL_NCERROR ( NF90_GET_VAR(NCID,VARID,D1LAT),'reading lat', 'CMF::RIVMAP_INIT/READ_MAP_CDF' )

CALL MAP_UTIL_NCERROR ( NF90_INQ_VARID(NCID,'lon',VARID),'getting id', 'CMF::RIVMAP_INIT/READ_MAP_CDF' )
CALL MAP_UTIL_NCERROR ( NF90_GET_VAR(NCID,VARID,D1LON),'reading lon', 'CMF::RIVMAP_INIT/READ_MAP_CDF' )

CALL MAP_UTIL_NCERROR( NF90_CLOSE(NCID),'closing '//TRIM(CRIVCLINC), 'CMF::RIVMAP_INIT/READ_MAP_CDF')

END SUBROUTINE READ_MAP_CDF
#endif
!==========================================================
!+
!+
!+
!==========================================================
SUBROUTINE CALC_REGION
! Assign map cells to the active computational regions and count their sizes.
USE YOS_CMF_INPUT,           ONLY: IMIS
#ifdef UseCDF_CMF
USE NETCDF
#endif
IMPLICIT NONE
!* local variables
INTEGER(KIND=JPIM),ALLOCATABLE  :: REGIONGRID(:)
!
INTEGER(KIND=JPIM),SAVE         :: IX,IY
INTEGER(KIND=JPIM),SAVE         :: IREGION
INTEGER(KIND=JPIM),SAVE         :: NSEQMAX_MPI
#ifdef UseMPI_CMF
#ifdef UseCDF_CMF
INTEGER(KIND=JPIM)              :: NCID
INTEGER(KIND=JPIM)              :: VARID
#endif
#endif
!$OMP THREADPRIVATE               (IX)
!================================================
WRITE(LOGNAM,*) '  CALC_REGION: assign region codes'

!*** when MPI is not used. Treat all grid as region=1
REGIONALL=1
I2REGION(:,:)=IMIS
!$OMP PARALLEL DO
DO IY=1, NY
  DO IX=1, NX
    IF( I2NEXTX(IX,IY)/=IMIS ) THEN
      I2REGION(IX,IY)=1
    ENDIF
  END DO
END DO
!$OMP END PARALLEL DO

!*** read MPI region map
!! Use MPI: read MPI region map, allocate regions to MPI nodes
#ifdef UseMPI_CMF
  IF ( LMAPCDF ) THEN
#ifdef UseCDF_CMF
    CALL MAP_UTIL_NCERROR (NF90_OPEN(CMPIREGNC,NF90_NOWRITE,NCID),'opening '//TRIM(CMPIREGNC), &
                           'CMF::RIVMAP_INIT/CALC_REGION')
    CALL MAP_UTIL_NCERROR (NF90_INQ_VARID(NCID, 'mpireg',VARID),'getting mpireg id', &
                           'CMF::RIVMAP_INIT/CALC_REGION')
    CALL MAP_UTIL_NCERROR (NF90_GET_VAR(NCID,VARID,I2REGION),'reading mpireg', &
                           'CMF::RIVMAP_INIT/CALC_REGION')
    CALL MAP_UTIL_NCERROR (NF90_CLOSE(NCID),'closing '//TRIM(CMPIREGNC), 'CMF::RIVMAP_INIT/CALC_REGION')
#else
    WRITE(LOGNAM,*) 'ERROR [CMF::RIVMAP_INIT/CALC_REGION]: legacy consolidated NetCDF MPI map requires a UseCDF_CMF build.'
    STOP 9
#endif
  ELSE
    CALL MAP_UTIL_READ_INT2D(CMPIREG,'mpireg',I2REGION,CONTEXT='CMF::RIVMAP_INIT')
  ENDIF

  REGIONALL=1
!$OMP PARALLEL DO REDUCTION(max:REGIONALL)
  DO IY=1, NY
    DO IX=1, NX
      REGIONALL=MAX( REGIONALL, I2REGION(IX,IY) )
    END DO
  END DO
!$OMP END PARALLEL DO
#endif


WRITE(LOGNAM,*)'  CALC_REGION: count cells in each region'
ALLOCATE(REGIONGRID(REGIONALL))
REGIONGRID(:)=0
!! OMP reduction operation for array might not be available in some environment
DO IY=1, NY
  DO IX=1, NX
    IF( I2REGION(IX,IY)>0 ) THEN
      IREGION=I2REGION(IX,IY)
      REGIONGRID(IREGION)=REGIONGRID(IREGION)+1
    ENDIF
  END DO
END DO

NSEQMAX=0
NSEQMAX_MPI=0
DO IREGION=1, REGIONALL
  IF( IREGION==REGIONTHIS ) NSEQMAX=REGIONGRID(IREGION)
  NSEQMAX_MPI=MAX(NSEQMAX,REGIONGRID(IREGION))  !! maximum nseqmax among all MPI region
END DO

WRITE(LOGNAM,*) '  CALC_REGION: REGIONALL= ', REGIONALL
WRITE(LOGNAM,*) '  CALC_REGION: NSEQMAX='   , NSEQMAX
WRITE(LOGNAM,*) '  CALC_REGION: NSEQMAX_MPI=', NSEQMAX_MPI

END SUBROUTINE CALC_REGION
!==========================================================
!+
!+
!+
!==========================================================
SUBROUTINE CALC_1D_SEQ
! Build the river-cell sequence used to process upstream cells before downstream cells.
USE YOS_CMF_INPUT,           ONLY: IMIS
IMPLICIT NONE
!* local variables
INTEGER(KIND=JPIM)              :: IX,IY,JX,JY,ISEQ,JSEQ,ISEQ1,ISEQ2,AGAIN
INTEGER(KIND=JPIM)              :: UPNMAX

INTEGER(KIND=JPIM),ALLOCATABLE  :: NUPST(:,:), UPNOW(:,:)
!================================================
WRITE(LOGNAM,*) '  CALC_1D_SEQ: convert 2D map to 1D sequence'

ALLOCATE( NUPST(NX,NY) )
ALLOCATE( UPNOW(NX,NY) )

ALLOCATE( I1SEQX(NSEQMAX) )
ALLOCATE( I1SEQY(NSEQMAX) )
ALLOCATE( I1NEXT(NSEQMAX) )
ALLOCATE( I2VECTOR(NX,NY) )
I1SEQX(:)=0
I1SEQY(:)=0
I1NEXT(:)=0
I2VECTOR(:,:)=0

!*** 1. Count incoming river links at each downstream cell.
NUPST(:,:)=0
UPNOW(:,:)=0
UPNMAX=0
DO IY=1, NY
  DO IX=1, NX
    IF( I2NEXTX(IX,IY).GT.0 .and. I2REGION(IX,IY)==REGIONTHIS )THEN
      JX=I2NEXTX(IX,IY)
      JY=I2NEXTY(IX,IY)
      NUPST(JX,JY)=NUPST(JX,JY)+1
      UPNMAX=max(UPNMAX,NUPST(JX,JY))
    ENDIF
  END DO
END DO

!*** 2. Start the sequence with cells that have no upstream river links.
ISEQ=0
DO IY=1, NY
  DO IX=1, NX
    IF( I2NEXTX(IX,IY).GT.0 .and. I2REGION(IX,IY)==REGIONTHIS )THEN
      IF( NUPST(IX,IY)==UPNOW(IX,IY) )THEN
        ISEQ=ISEQ+1
        I1SEQX(ISEQ)=IX
        I1SEQY(ISEQ)=IY
        I2VECTOR(IX,IY)=ISEQ
      ENDIF
    ENDIF
  END DO
END DO
ISEQ1=1
ISEQ2=ISEQ

AGAIN=1
!*** 3. Add downstream cells after all their upstream cells are sequenced.
DO WHILE( AGAIN==1 )
  AGAIN=0
  JSEQ=ISEQ2
  DO ISEQ=ISEQ1, ISEQ2
    IX=I1SEQX(ISEQ)
    IY=I1SEQY(ISEQ)
    JX=I2NEXTX(IX,IY)
    JY=I2NEXTY(IX,IY)
    UPNOW(JX,JY)=UPNOW(JX,JY)+1
    IF( UPNOW(JX,JY)==NUPST(JX,JY) .and. I2NEXTX(JX,JY)>0 )THEN !! if all upstream calculated, register to 1D sequence
      JSEQ=JSEQ+1
      I1SEQX(JSEQ)=JX
      I1SEQY(JSEQ)=JY
      I2VECTOR(JX,JY)=JSEQ
      AGAIN=1
    ENDIF
  END DO
  ISEQ1=ISEQ2+1
  ISEQ2=JSEQ
END DO
NSEQRIV=JSEQ    !! END OF RIVER-LINK GRID

ISEQ=NSEQRIV
!*** 4. Append river-mouth cells after the river-link cells.
DO IY=1, NY
  DO IX=1, NX
    IF( I2NEXTX(IX,IY).LT.0 .AND. I2NEXTX(IX,IY).NE.IMIS .AND. I2REGION(IX,IY)==REGIONTHIS )THEN
      ISEQ=ISEQ+1
      I1SEQX(ISEQ)=IX
      I1SEQY(ISEQ)=IY
      I2VECTOR(IX,IY)=ISEQ
    ENDIF
  END DO
END DO
NSEQALL=ISEQ    !! END OF RIVER-MOUTH GRID

!*** 5. Convert downstream map coordinates to sequence indices.
DO ISEQ=1, NSEQALL
  IX=I1SEQX(ISEQ)
  IY=I1SEQY(ISEQ)
  IF( I2NEXTX(IX,IY)>0 )THEN
    JX=I2NEXTX(IX,IY)
    JY=I2NEXTY(IX,IY)
    I1NEXT(ISEQ)=I2VECTOR(JX,JY)
  ELSE
    I1NEXT(ISEQ)=I2NEXTX(IX,IY)
  ENDIF
END DO

DEALLOCATE(NUPST,UPNOW)

WRITE(LOGNAM,*) '  CALC_1D_SEQ: NSEQRIV='   , NSEQRIV
WRITE(LOGNAM,*) '  CALC_1D_SEQ: NSEQALL='   , NSEQALL

!*** 6. Build the inverse upstream-cell list for each river cell.
!!IF( LSPAMAT )THEN
  WRITE(LOGNAM,*) '  CALC_1D_SEQ: upstream matrix size', UPNMAX
  ALLOCATE(I1UPST(NSEQMAX,UPNMAX),I1UPN(NSEQMAX))
  I1UPST(:,:)=-9999
  I1UPN(:)=0
  DO ISEQ=1, NSEQRIV
    JSEQ=I1NEXT(ISEQ)
    I1UPN(JSEQ)=I1UPN(JSEQ)+1
    I1UPST(JSEQ,I1UPN(JSEQ))=ISEQ
  END DO
!!ENDIF

END SUBROUTINE CALC_1D_SEQ
!==========================================================
!+
!+
!+
!==========================================================
SUBROUTINE READ_BIFPARAM
! Read bifurcation pathways and build their per-cell input and output lists.
USE YOS_CMF_INPUT,      ONLY: PMANRIV, PMANFLD
USE YOS_CMF_MAP,        ONLY: NPTHOUT, NPTHLEV, PTH_UPST, PTH_DOWN,&
                            & PTH_DST, PTH_ELV, PTH_WTH,  PTH_MAN, NSEQMAX
USE CMF_UTILS_MOD,      ONLY: INQUIRE_FID
IMPLICIT NONE
!* local variables
INTEGER(KIND=JPIM)         :: IX,IY, JX,JY, ISEQ, JSEQ
INTEGER(KIND=JPIM)         :: IPTH,  ILEV,  NPTHOUT1
REAL(KIND=JPRB)            :: PELV,  PWTH,  PDPH
INTEGER(KIND=JPIM)         :: ONMAX,  INMAX
INTEGER(KIND=JPIM)         :: OCOUNT(NSEQMAX),  ICOUNT(NSEQMAX)
!================================================
WRITE(LOGNAM,*)"  READ_BIFPARAM: read bifurcation parameters from ", TRIM(CPTHOUT)

!*** 1. Read the number of pathways and their layer count.
TMPNAM=INQUIRE_FID()
OPEN(TMPNAM,FILE=CPTHOUT,FORM='FORMATTED')
READ(TMPNAM,*) NPTHOUT,NPTHLEV

WRITE(LOGNAM,*) "  READ_BIFPARAM: channel dimensions", NPTHOUT, NPTHLEV

ALLOCATE( PTH_UPST(NPTHOUT) )
ALLOCATE( PTH_DOWN(NPTHOUT) )
ALLOCATE( PTH_DST(NPTHOUT)  )
ALLOCATE( PTH_ELV(NPTHOUT,NPTHLEV) )
ALLOCATE( PTH_WTH(NPTHOUT,NPTHLEV) )
ALLOCATE( PTH_MAN(NPTHLEV)  )

OCOUNT(:)=0
ICOUNT(:)=0

!*** 2. Read each pathway and derive its level profile.
NPTHOUT1=0
DO IPTH=1, NPTHOUT
  READ(TMPNAM,*) IX, IY, JX, JY, PTH_DST(IPTH), PELV, PDPH, (PTH_WTH(IPTH,ILEV),ILEV=1,NPTHLEV)
  PTH_UPST(IPTH)=I2VECTOR(IX,IY)
  PTH_DOWN(IPTH)=I2VECTOR(JX,JY)
  IF (PTH_UPST(IPTH) > 0 .AND. PTH_DOWN(IPTH) > 0) THEN  ! both upst and down grids are within domain
    NPTHOUT1=NPTHOUT1+1
    ISEQ=PTH_UPST(IPTH)
    JSEQ=PTH_DOWN(IPTH)
    OCOUNT(ISEQ)=OCOUNT(ISEQ)+1
    ICOUNT(JSEQ)=ICOUNT(JSEQ)+1
  ENDIF
  DO ILEV=1, NPTHLEV
    IF( ILEV==1 )THEN            !!ILEV=1: water channel bifurcation. consider bifurcation channel depth
      PWTH=PTH_WTH(IPTH,ILEV)
      IF( PWTH>0 )then
        PTH_ELV(IPTH,ILEV)=PELV - PDPH
      ELSE
        PTH_ELV(IPTH,ILEV)=1.E20
      ENDIF
    ELSE
      PWTH=PTH_WTH(IPTH,ILEV)
      IF( PWTH>0 )then
        PTH_ELV(IPTH,ILEV)=PELV + ILEV - 2.0    !! ILEV=2: bank top level 
      ELSE
        PTH_ELV(IPTH,ILEV)=1.E20
      ENDIF
    ENDIF
  END DO
END DO
CLOSE(TMPNAM)

!*** 3. Set river/floodplain roughness and count connected pathways per cell.
DO ILEV=1, NPTHLEV
  IF( ILEV==1 )THEN
    PTH_MAN(ILEV)=PMANRIV
  ELSE
    PTH_MAN(ILEV)=PMANFLD
  ENDIF
END DO

IF (NPTHOUT /= NPTHOUT1) THEN
  WRITE(LOGNAM,*) 'WARNING [CMF::RIVMAP_INIT/READ_BIFPARAM]: bifurcation channels outside the active domain. Valid:', NPTHOUT1
ENDIF

!=====
!!IF( LSPAMAT )THEN
  ONMAX=0
  INMAX=0
  DO ISEQ=1, NSEQMAX
    ONMAX=max(ONMAX,OCOUNT(ISEQ))
    INMAX=max(INMAX,ICOUNT(ISEQ))
  END DO
  WRITE(LOGNAM,*) "  READ_BIFPARAM: maximum pathways per cell (out, in):", ONMAX, INMAX
  
  ALLOCATE(I1P_OUT(NSEQMAX,ONMAX))
  ALLOCATE(I1P_INF(NSEQMAX,INMAX))
  ALLOCATE(I1P_OUTN(NSEQMAX))
  ALLOCATE(I1P_INFN(NSEQMAX))
  I1P_OUT(:,:)=0
  I1P_INF(:,:)=0
  I1P_OUTN(:)=0
  I1P_INFN(:)=0

  DO IPTH=1, NPTHOUT
    IF (PTH_UPST(IPTH) > 0 .AND. PTH_DOWN(IPTH) > 0) THEN  ! both upst and down grids are within domain
      ISEQ=PTH_UPST(IPTH)
      JSEQ=PTH_DOWN(IPTH)
  
      I1P_OUTN(ISEQ)=I1P_OUTN(ISEQ)+1
      I1P_OUT(ISEQ,I1P_OUTN(ISEQ))=IPTH
  
      I1P_INFN(JSEQ)=I1P_INFN(JSEQ)+1
      I1P_INF(JSEQ,I1P_INFN(JSEQ))=IPTH
    ENDIF
  END DO
!!ENDIF

END SUBROUTINE READ_BIFPARAM
!==========================================================


END SUBROUTINE CMF_RIVMAP_INIT
!####################################################################





!####################################################################
SUBROUTINE CMF_TOPO_INIT
! Read topography and initialize derived channel and floodplain parameters.
!
! Internal subroutines:
!+ READ_TOPO_BIN : Read individual Binary/NetCDF maps and vectorize their values
!+ READ_TOPO_CDF : Read maps from the legacy consolidated NetCDF files
!+ SET_FLDSTG    : Calculate floodplain storage and geometry by layer
!+ SET_SLOPEMIX  : Read and apply the legacy slope-mixing mask
USE YOS_CMF_INPUT,  ONLY: NX,NY,NLFP,  &
                        & LFPLAIN,  LMEANSL,  LGDWDLY,  LSLPMIX, LSLOPEMOUTH
USE YOS_CMF_MAP,    ONLY: D2NXTDST, D2GRAREA, D2ELEVTN, D2RIVLEN, &
                        & D2RIVWTH, D2RIVHGT, D2FLDHGT, D2RIVELV, &
                        & D2FLDGRD, D2RIVMAN, D2RIVSTOMAX, D2FLDSTOMAX,  &
                        & DFRCINC,  NSEQMAX,  D2MEANSL, D2DWNELV, &
                            & D2GDWDLY, I2MASK
IMPLICIT NONE
!================================================
WRITE(LOGNAM,*) ""
WRITE(LOGNAM,*) "!---------------------!"

WRITE(LOGNAM,*) 'CMF::TOPO_INIT: START topography map initialization'

! *** 1. ALLOCATE ARRAYS
ALLOCATE( D2GRAREA(NSEQMAX,1) )
ALLOCATE( D2ELEVTN(NSEQMAX,1) )
ALLOCATE( D2NXTDST(NSEQMAX,1) )
ALLOCATE( D2RIVLEN(NSEQMAX,1) )
ALLOCATE( D2RIVWTH(NSEQMAX,1) )
ALLOCATE( D2RIVHGT(NSEQMAX,1) )
ALLOCATE( D2FLDHGT(NSEQMAX,1,NLFP) )
ALLOCATE( D2RIVMAN(NSEQMAX,1) )
ALLOCATE( D2MEANSL(NSEQMAX,1) )
ALLOCATE( D2DWNELV(NSEQMAX,1) )
ALLOCATE( D2GDWDLY(NSEQMAX,1) )
ALLOCATE( I2MASK(NSEQMAX,1) )

D2GRAREA(:,:)  =0._JPRB
D2ELEVTN(:,:)  =0._JPRB
D2NXTDST(:,:)  =0._JPRB
D2RIVLEN(:,:)  =0._JPRB
D2RIVWTH(:,:)  =0._JPRB
D2RIVHGT(:,:)  =0._JPRB
D2FLDHGT(:,:,:)=0._JPRB
D2RIVMAN(:,:)  =0._JPRB
D2MEANSL(:,:)  =0._JPRB
D2DWNELV(:,:)  =0._JPRB
D2GDWDLY(:,:)  =0._JPRB
I2MASK(:,:)    =0._JPIM     !! mask for calculation (IFS slopemix: Kinemacti Wave for Mask=1; Reservoir: dam=2, dam upstream=1)

!============================
! *** 2. Read topo map
WRITE(LOGNAM,*) 'CMF::TOPO_INIT: read topography maps'
! These ECMWF-specific options require the legacy consolidated NetCDF path.
IF ( .NOT. LMAPCDF .AND. LSLOPEMOUTH ) THEN
  CALL MAP_UTIL_INPUT_ERROR('LMAPCDF','LSLOPEMOUTH requires legacy consolidated NetCDF Map input; individual-file input is not supported', &
                            'CMF::TOPO_INIT')
ENDIF
IF ( .NOT. LMAPCDF .AND. LSLPMIX ) THEN
  CALL MAP_UTIL_INPUT_ERROR('LMAPCDF','LSLPMIX requires legacy consolidated NetCDF Map input; individual-file input is not supported', &
                            'CMF::TOPO_INIT')
ENDIF
IF ( .not. LMAPCDF ) THEN
  CALL READ_TOPO_BIN
ELSE
  CALL READ_TOPO_CDF
ENDIF

I2MASK(:,:)=0_JPIM
IF ( LSLPMIX ) THEN
  CALL SET_SLOPEMIX
ENDIF

!============================
! *** 3a. Calc Channel Parameters
WRITE(LOGNAM,*) 'CMF::TOPO_INIT: calc river channel parameters'

ALLOCATE(D2RIVSTOMAX(NSEQMAX,1))
ALLOCATE(D2RIVELV(NSEQMAX,1))

IF ( LFPLAIN ) THEN
  D2RIVSTOMAX(:,:) = D2RIVLEN(:,:) * D2RIVWTH(:,:) * D2RIVHGT(:,:)
ELSE
  WRITE(LOGNAM,*) 'CMF::TOPO_INIT: no floodplain (rivstomax=1.D18)'
  D2RIVSTOMAX(:,:) = 1.E18
ENDIF
D2RIVELV(:,:) = D2ELEVTN(:,:) - D2RIVHGT(:,:)

!*** 3b. Calc Channel Parameters
WRITE(LOGNAM,*) 'CMF::TOPO_INIT: calc floodplain parameters'

ALLOCATE(D2FLDSTOMAX(NSEQMAX,1,NLFP))
ALLOCATE(D2FLDGRD(NSEQMAX,1,NLFP))
CALL SET_FLDSTG

!*** 3c. Calc downstream boundary
WRITE(LOGNAM,*) 'CMF::TOPO_INIT: calc downstream boundary elevation'
D2DWNELV(:,:)=D2ELEVTN(:,:)
IF( LMEANSL ) THEN
  D2DWNELV(:,:)=D2ELEVTN(:,:)+D2MEANSL(:,:)
ENDIF

WRITE(LOGNAM,*) 'CMF::TOPO_INIT: END'
WRITE(LOGNAM,*) "!---------------------!"

CONTAINS
!==========================================================
!+ READ_TOPO_BIN : Read individual Binary/NetCDF maps and vectorize their values
!+ READ_TOPO_CDF : Read maps from the legacy consolidated NetCDF files
!+ SET_FLDSTG    : Calculate floodplain storage and geometry by layer
!+ SET_SLOPEMIX  : Read and apply the legacy slope-mixing mask
!==========================================================
SUBROUTINE READ_TOPO_BIN
! Read individual map rasters and convert them to the model river-cell order.
USE CMF_UTILS_MOD,       ONLY: mapR2vecD
IMPLICIT NONE
!* local variables
INTEGER(KIND=JPIM)          :: ILFP
REAL(KIND=JPRM),ALLOCATABLE :: R2TEMP(:,:)
REAL(KIND=JPRB),ALLOCATABLE :: D2TEMP(:,:)
!================================================
ALLOCATE(R2TEMP(NX,NY))
ALLOCATE(D2TEMP(NSEQMAX,1))

! Each raster is read on the geographic grid, then mapped to model river order.
WRITE(LOGNAM,*)'  READ_TOPO_BIN: unit-catchment area: ',TRIM(CGRAREA)
CALL MAP_UTIL_READ_REAL2D(CGRAREA,'ctmare',R2TEMP,CONTEXT='CMF::TOPO_INIT')
CALL mapR2vecD(R2TEMP,D2GRAREA)

WRITE(LOGNAM,*)'  READ_TOPO_BIN: ground elevation: ',TRIM(CELEVTN)
CALL MAP_UTIL_READ_REAL2D(CELEVTN,'elevtn',R2TEMP,CONTEXT='CMF::TOPO_INIT')
CALL mapR2vecD(R2TEMP,D2ELEVTN)

WRITE(LOGNAM,*)'  READ_TOPO_BIN: downstream distance: ',TRIM(CNXTDST)
CALL MAP_UTIL_READ_REAL2D(CNXTDST,'nxtdst',R2TEMP,CONTEXT='CMF::TOPO_INIT')
CALL mapR2vecD(R2TEMP,D2NXTDST)

WRITE(LOGNAM,*)'  READ_TOPO_BIN: river channel length: ',TRIM(CRIVLEN)
CALL MAP_UTIL_READ_REAL2D(CRIVLEN,'rivlen',R2TEMP,CONTEXT='CMF::TOPO_INIT')
CALL mapR2vecD(R2TEMP,D2RIVLEN)

WRITE(LOGNAM,*)'  READ_TOPO_BIN: floodplain elevation profile: ',TRIM(CFLDHGT)
DO ILFP=1,NLFP
  CALL MAP_UTIL_READ_REAL2D(CFLDHGT,'fldhgt',R2TEMP,ILFP,'CMF::TOPO_INIT')
  CALL mapR2vecD(R2TEMP,D2TEMP)
  D2FLDHGT(:,:,ILFP)= D2TEMP(:,:)
ENDDO

!*** river channel / groundwater parameters)

WRITE(LOGNAM,*)'  READ_TOPO_BIN: river channel depth: ',TRIM(CRIVHGT)
CALL MAP_UTIL_READ_REAL2D(CRIVHGT,'rivhgt',R2TEMP,CONTEXT='CMF::TOPO_INIT')
CALL mapR2vecD(R2TEMP,D2RIVHGT)

WRITE(LOGNAM,*)'  READ_TOPO_BIN: river channel width: ',TRIM(CRIVWTH)
CALL MAP_UTIL_READ_REAL2D(CRIVWTH,'rivwth',R2TEMP,CONTEXT='CMF::TOPO_INIT')
CALL mapR2vecD(R2TEMP,D2RIVWTH)

WRITE(LOGNAM,*)'  READ_TOPO_BIN: river Manning coefficient: ',TRIM(CRIVMAN)
CALL MAP_UTIL_READ_REAL2D(CRIVMAN,'rivman',R2TEMP,CONTEXT='CMF::TOPO_INIT')
CALL mapR2vecD(R2TEMP,D2RIVMAN)

IF( LGDWDLY )THEN
  WRITE(LOGNAM,*)'  READ_TOPO_BIN: groundwater delay parameter: ',TRIM(CGDWDLY)
  CALL MAP_UTIL_READ_REAL2D(CGDWDLY,'gdwdly',R2TEMP,CONTEXT='CMF::TOPO_INIT')
  CALL mapR2vecD(R2TEMP,D2GDWDLY)
ENDIF

! ==========

IF( LMEANSL ) THEN
  WRITE(LOGNAM, *)'  READ_TOPO_BIN: mean sea level: ', TRIM(CMEANSL)
  CALL MAP_UTIL_READ_REAL2D(CMEANSL,'meansl',R2TEMP,CONTEXT='CMF::TOPO_INIT')
  CALL mapR2vecD(R2TEMP, D2MEANSL)
ENDIF

DEALLOCATE(R2TEMP)
DEALLOCATE(D2TEMP)

END SUBROUTINE READ_TOPO_BIN
!==========================================================
!+
!+
!+
!==========================================================
SUBROUTINE READ_TOPO_CDF
! Read topography and channel parameters from legacy consolidated NetCDF files.
#ifdef UseCDF_CMF
USE NETCDF 
USE CMF_UTILS_MOD,            ONLY: mapR2vecD
USE YOS_CMF_MAP,              ONLY: D2ELEVSLOPE     !! only used in ECMWF
IMPLICIT NONE
!* local variables
INTEGER(KIND=JPIM)               :: NCID,VARID,STATUS
INTEGER(KIND=JPIM)               :: ILEV
REAL(KIND=JPRM),ALLOCATABLE      :: R2TEMP(:,:)
REAL(KIND=JPRB),ALLOCATABLE      :: D2TEMP(:,:)
!================================================
ALLOCATE(R2TEMP(NX,NY))
ALLOCATE(D2TEMP(NSEQMAX,1))

! The legacy consolidated files are opened separately by variable group.
!! CLIM FILE
CALL MAP_UTIL_NCERROR (NF90_OPEN(CRIVCLINC,NF90_NOWRITE,NCID),'opening '//TRIM(CRIVCLINC), &
                       'CMF::TOPO_INIT/READ_TOPO_CDF')

WRITE(LOGNAM,*)'  READ_TOPO_CDF: ctmare: ',TRIM(CRIVCLINC)
CALL MAP_UTIL_NCERROR ( NF90_INQ_VARID(NCID,'ctmare',VARID),'getting ctmare id', 'CMF::TOPO_INIT/READ_TOPO_CDF' )
CALL MAP_UTIL_NCERROR ( NF90_GET_VAR(NCID,VARID,R2TEMP),'reading ctmare', 'CMF::TOPO_INIT/READ_TOPO_CDF' )
CALL mapR2vecD(R2TEMP,D2GRAREA)

WRITE(LOGNAM,*)'  READ_TOPO_CDF: elevtn: ',TRIM(CRIVCLINC)
CALL MAP_UTIL_NCERROR ( NF90_INQ_VARID(NCID,'elevtn',VARID),'getting elevtn id', 'CMF::TOPO_INIT/READ_TOPO_CDF' )
CALL MAP_UTIL_NCERROR ( NF90_GET_VAR(NCID,VARID,R2TEMP),'reading elevtn', 'CMF::TOPO_INIT/READ_TOPO_CDF' )
CALL mapR2vecD(R2TEMP,D2ELEVTN)

WRITE(LOGNAM,*)'  READ_TOPO_CDF: nxtdst: ',TRIM(CRIVCLINC)
CALL MAP_UTIL_NCERROR ( NF90_INQ_VARID(NCID,'nxtdst',VARID),'getting nxtdst id', 'CMF::TOPO_INIT/READ_TOPO_CDF' )
CALL MAP_UTIL_NCERROR ( NF90_GET_VAR(NCID,VARID,R2TEMP),'reading nxtdst', 'CMF::TOPO_INIT/READ_TOPO_CDF' )
CALL mapR2vecD(R2TEMP,D2NXTDST)

WRITE(LOGNAM,*)'  READ_TOPO_CDF: rivlen: ',TRIM(CRIVCLINC)
CALL MAP_UTIL_NCERROR ( NF90_INQ_VARID(NCID,'rivlen',VARID),'getting rivlen id', 'CMF::TOPO_INIT/READ_TOPO_CDF' )
CALL MAP_UTIL_NCERROR ( NF90_GET_VAR(NCID,VARID,R2TEMP),'reading rivlen', 'CMF::TOPO_INIT/READ_TOPO_CDF' )
CALL mapR2vecD(R2TEMP,D2RIVLEN)

WRITE(LOGNAM,*)'  READ_TOPO_CDF: fldhgt: ',TRIM(CRIVCLINC)
CALL MAP_UTIL_NCERROR ( NF90_INQ_VARID(NCID,'fldhgt',VARID),'getting fldhgt id', 'CMF::TOPO_INIT/READ_TOPO_CDF' )
DO ILEV=1,NLFP
  CALL MAP_UTIL_NCERROR ( NF90_GET_VAR(NCID,VARID,R2TEMP,(/1,1,ILEV/),(/NX,NY,1/)), &
                          'reading fldhgt layer', 'CMF::TOPO_INIT/READ_TOPO_CDF' )
  CALL mapR2vecD(R2TEMP,D2TEMP)
  D2FLDHGT(:,:,ILEV)=D2TEMP(:,:)
ENDDO

CALL MAP_UTIL_NCERROR( NF90_CLOSE(NCID),'closing '//TRIM(CRIVCLINC), 'CMF::TOPO_INIT/READ_TOPO_CDF')

!!========== 
!! PAR FILE (river channel / groundwater parameters)
CALL MAP_UTIL_NCERROR (NF90_OPEN(CRIVPARNC,NF90_NOWRITE,NCID),'opening '//TRIM(CRIVPARNC), &
                       'CMF::TOPO_INIT/READ_TOPO_CDF')

WRITE(LOGNAM,*)'  READ_TOPO_CDF: rivwth: ',TRIM(CRIVPARNC)
CALL MAP_UTIL_NCERROR ( NF90_INQ_VARID(NCID,'rivwth',VARID),'getting rivwth id', 'CMF::TOPO_INIT/READ_TOPO_CDF' )
CALL MAP_UTIL_NCERROR ( NF90_GET_VAR(NCID,VARID,R2TEMP),'reading rivwth', 'CMF::TOPO_INIT/READ_TOPO_CDF' )
CALL mapR2vecD(R2TEMP,D2RIVWTH)

WRITE(LOGNAM,*)'  READ_TOPO_CDF: rivhgt: ',TRIM(CRIVPARNC)
CALL MAP_UTIL_NCERROR ( NF90_INQ_VARID(NCID,'rivhgt',VARID),'getting rivhgt id', 'CMF::TOPO_INIT/READ_TOPO_CDF' )
CALL MAP_UTIL_NCERROR ( NF90_GET_VAR(NCID,VARID,R2TEMP),'reading rivhgt', 'CMF::TOPO_INIT/READ_TOPO_CDF' )
CALL mapR2vecD(R2TEMP,D2RIVHGT)

WRITE(LOGNAM,*)'  READ_TOPO_CDF: rivman: ',TRIM(CRIVPARNC)
CALL MAP_UTIL_NCERROR ( NF90_INQ_VARID(NCID,'rivman',VARID),'getting rivman id', 'CMF::TOPO_INIT/READ_TOPO_CDF' )
CALL MAP_UTIL_NCERROR ( NF90_GET_VAR(NCID,VARID,R2TEMP),'reading rivman', 'CMF::TOPO_INIT/READ_TOPO_CDF' )
CALL mapR2vecD(R2TEMP,D2RIVMAN)

IF ( LGDWDLY ) THEN
  WRITE(LOGNAM,*)'  READ_TOPO_CDF: gdwdly: ',TRIM(CRIVPARNC)
  STATUS = NF90_INQ_VARID(NCID,'gdwdly',VARID)
  IF (STATUS /= 0 ) THEN
    WRITE(LOGNAM,*)'WARNING [CMF::TOPO_INIT/READ_TOPO_CDF]: gdwdly is not present; setting to zero'
    R2TEMP(:,:) = 0._JPRB
  ELSE
    CALL MAP_UTIL_NCERROR ( NF90_GET_VAR(NCID,VARID,R2TEMP),'reading gdwdly', 'CMF::TOPO_INIT/READ_TOPO_CDF' )
  ENDIF 
  CALL mapR2vecD(R2TEMP,D2GDWDLY)
ENDIF

IF ( LSLOPEMOUTH ) THEN
  ALLOCATE( D2ELEVSLOPE(NSEQMAX,1) )
  WRITE(LOGNAM,*)'  READ_TOPO_CDF: elevslope: ',TRIM(CRIVPARNC)
  STATUS = NF90_INQ_VARID(NCID,'elevslope',VARID)
  IF (STATUS /= 0 ) THEN
    WRITE(LOGNAM,*)'ERROR [CMF::TOPO_INIT/READ_TOPO_CDF]: elevslope is not present; stopping'
    STOP 9
  ELSE
    CALL MAP_UTIL_NCERROR ( NF90_GET_VAR(NCID,VARID,R2TEMP),'reading elevslope', 'CMF::TOPO_INIT/READ_TOPO_CDF' )
  ENDIF
  CALL mapR2vecD(R2TEMP,D2ELEVSLOPE)
ENDIF

CALL MAP_UTIL_NCERROR( NF90_CLOSE(NCID),'closing '//TRIM(CRIVPARNC), 'CMF::TOPO_INIT/READ_TOPO_CDF')

!!========== 
!! MEAN SEA LEVEL FILE
IF( LMEANSL ) THEN
  CALL MAP_UTIL_NCERROR (NF90_OPEN(CMEANSLNC,NF90_NOWRITE,NCID),'opening '//TRIM(CMEANSLNC), &
                         'CMF::TOPO_INIT/READ_TOPO_CDF')
  WRITE(LOGNAM,*)'  READ_TOPO_CDF: meansl: ',TRIM(CMEANSLNC)
  CALL MAP_UTIL_NCERROR ( NF90_INQ_VARID(NCID,'meansl',VARID),'getting meansl id', 'CMF::TOPO_INIT/READ_TOPO_CDF' )
  CALL MAP_UTIL_NCERROR ( NF90_GET_VAR(NCID,VARID,R2TEMP),'reading meansl', 'CMF::TOPO_INIT/READ_TOPO_CDF' )
  CALL mapR2vecD ( R2TEMP,D2MEANSL  )
  CALL MAP_UTIL_NCERROR ( NF90_CLOSE(NCID),'closing '//TRIM(CMEANSLNC), 'CMF::TOPO_INIT/READ_TOPO_CDF' )
ENDIF 

DEALLOCATE(R2TEMP)
DEALLOCATE(D2TEMP)
#endif
END SUBROUTINE READ_TOPO_CDF
!==========================================================
!+
!+
!+
!==========================================================
SUBROUTINE SET_FLDSTG
! Derive floodplain storage capacity and geometry for each floodplain layer.
IMPLICIT NONE
!* local variables
INTEGER(KIND=JPIM),SAVE  ::  ISEQ, I
REAL(KIND=JPRB),SAVE     ::  DSTONOW
REAL(KIND=JPRB),SAVE     ::  DSTOPRE
REAL(KIND=JPRB),SAVE     ::  DHGTPRE
REAL(KIND=JPRB),SAVE     ::  DWTHINC
!$OMP THREADPRIVATE               (I,DSTONOW,DSTOPRE,DHGTPRE,DWTHINC)
!================================================
D2FLDSTOMAX(:,:,:) = 0._JPRD
D2FLDGRD(:,:,:)    = 0._JPRB
DFRCINC=REAL(NLFP,KIND=JPRB)**(-1.)
! Build cumulative floodplain storage and the area-width increment for each layer.
!
!$OMP PARALLEL DO
DO ISEQ=1, NSEQMAX
  DSTOPRE = D2RIVSTOMAX(ISEQ,1)
  DHGTPRE = 0._JPRB
  DWTHINC = D2GRAREA(ISEQ,1) * D2RIVLEN(ISEQ,1)**(-1.) * DFRCINC
  DO I=1, NLFP
    DSTONOW = D2RIVLEN(ISEQ,1) * ( D2RIVWTH(ISEQ,1) + DWTHINC*(REAL(I,KIND=JPRB)-0.5) ) * (D2FLDHGT(ISEQ,1,I)-DHGTPRE)
    D2FLDSTOMAX(ISEQ,1,I) = DSTOPRE + DSTONOW
    D2FLDGRD(ISEQ,1,I) = (D2FLDHGT(ISEQ,1,I)-DHGTPRE) * DWTHINC**(-1.)
    DSTOPRE = D2FLDSTOMAX(ISEQ,1,I)
    DHGTPRE = D2FLDHGT(ISEQ,1,I)
  END DO
END DO
!$OMP END PARALLEL DO

!
END SUBROUTINE SET_FLDSTG
!==========================================================
!+
!+
!+
!==========================================================
SUBROUTINE SET_SLOPEMIX    !! only used in IFS0
! Read the legacy slope-mixing mask and map it to river-cell order.
#ifdef UseCDF_CMF
USE NETCDF 
USE CMF_UTILS_MOD,           ONLY: mapI2vecI

IMPLICIT NONE
INTEGER(KIND=JPIM),ALLOCATABLE  :: I2TEMP(:,:)
INTEGER(KIND=JPIM)              :: ISEQ, I0, I1
INTEGER(KIND=JPIM)              :: NCID,VARID,STATUS

  ! mask_slope is available only in the legacy consolidated parameter file.
  ALLOCATE(I2TEMP(NX,NY))
  CALL MAP_UTIL_NCERROR (NF90_OPEN(CRIVPARNC,NF90_NOWRITE,NCID),'opening '//TRIM(CRIVPARNC), &
                         'CMF::TOPO_INIT/SET_SLOPEMIX')
  WRITE(LOGNAM,*)'  SET_SLOPEMIX: mask_slope: ',TRIM(CRIVPARNC)
  STATUS =  NF90_INQ_VARID(NCID,'mask_slope',VARID)
  IF (STATUS /= 0 ) THEN
    WRITE(LOGNAM,*)'ERROR [CMF::TOPO_INIT/SET_SLOPEMIX]: mask_slope is missing; LSLPMIX must be FALSE'
    CALL MAP_UTIL_NCERROR (NF90_CLOSE(NCID),'closing '//TRIM(CRIVPARNC), 'CMF::TOPO_INIT/SET_SLOPEMIX')
    STOP 9
  ENDIF 
  CALL MAP_UTIL_NCERROR ( NF90_GET_VAR(NCID,VARID,I2TEMP),'reading mask_slope', 'CMF::TOPO_INIT/SET_SLOPEMIX' )
  CALL MAP_UTIL_NCERROR (NF90_CLOSE(NCID),'closing '//TRIM(CRIVPARNC), 'CMF::TOPO_INIT/SET_SLOPEMIX')
  CALL mapI2vecI(I2TEMP,I2MASK)
  I0=0
  I1=0
  DO ISEQ=1,NSEQMAX
    IF (I2MASK(ISEQ,1) == 1 ) THEN  !! kinematic wave applied
      I1=I1+1
    ENDIF
    IF (I2MASK(ISEQ,1) == 0 ) THEN 
      I0=I0+1
    ENDIF
  ENDDO
  WRITE(LOGNAM,*)'  SET_SLOPEMIX: sum(mask==0), sum(mask==1)',I0,I1
  IF ( I0+I1 .NE. NSEQMAX ) THEN 
     WRITE(LOGNAM,*)'ERROR [CMF::TOPO_INIT/SET_SLOPEMIX]: mask==0 + mask==1 does not match NSEQMAX'
     STOP 9
  ENDIF 

  DEALLOCATE(I2TEMP)
#endif
END SUBROUTINE SET_SLOPEMIX

END SUBROUTINE CMF_TOPO_INIT
!####################################################################


!####################################################################
SUBROUTINE MAP_UTIL_INPUT_ERROR(CFILE, MESSAGE, CONTEXT)
! Write a Map input diagnostic with its caller context and stop execution.
IMPLICIT NONE
CHARACTER(LEN=*),INTENT(IN) :: CFILE
CHARACTER(LEN=*),INTENT(IN) :: MESSAGE
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: CONTEXT
CHARACTER(LEN=256) :: CWHERE

CWHERE='CMF_CTRL_MAPS_MOD/MAP_UTIL_INPUT_ERROR'
IF (PRESENT(CONTEXT)) CWHERE=CONTEXT
WRITE(LOGNAM,*) 'ERROR ['//TRIM(CWHERE)//']: Map input '//TRIM(CFILE)//': '//TRIM(MESSAGE)
STOP 9
END SUBROUTINE MAP_UTIL_INPUT_ERROR
!####################################################################

#ifdef UseCDF_CMF
!####################################################################
SUBROUTINE MAP_UTIL_NCERROR(STATUS, MESSAGE, CONTEXT)
! Write a contextual NetCDF diagnostic and preserve the NCERROR stop status.
USE NETCDF, ONLY: NF90_NOERR,NF90_STRERROR
IMPLICIT NONE
INTEGER(KIND=JPIM),INTENT(IN) :: STATUS
CHARACTER(LEN=*),INTENT(IN) :: MESSAGE,CONTEXT

IF (STATUS /= NF90_NOERR) THEN
  WRITE(LOGNAM,*) 'ERROR ['//TRIM(CONTEXT)//']: '//TRIM(NF90_STRERROR(STATUS))//' '//TRIM(MESSAGE)
  STOP 10
ENDIF
END SUBROUTINE MAP_UTIL_NCERROR
#endif
!####################################################################


!####################################################################
INTEGER(KIND=JPIM) FUNCTION MAP_UTIL_INPUT_KIND(CFILE, CONTEXT)
! Return the individual Map format code selected from the filename extension.
IMPLICIT NONE
CHARACTER(LEN=*),INTENT(IN) :: CFILE
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: CONTEXT
INTEGER(KIND=JPIM) :: NFILE

NFILE=LEN_TRIM(CFILE)
IF (NFILE >= 3) THEN
  IF (CFILE(NFILE-2:NFILE) == '.nc') THEN
    MAP_UTIL_INPUT_KIND=2
    RETURN
  ENDIF
ENDIF
IF (NFILE >= 4) THEN
  IF (CFILE(NFILE-3:NFILE) == '.bin') THEN
    MAP_UTIL_INPUT_KIND=1
    RETURN
  ENDIF
ENDIF
IF (PRESENT(CONTEXT)) THEN
  CALL MAP_UTIL_INPUT_ERROR(CFILE,'unsupported extension; expected .bin or .nc', &
                            TRIM(CONTEXT)//'/MAP_UTIL_INPUT_KIND')
ELSE
  CALL MAP_UTIL_INPUT_ERROR(CFILE,'unsupported extension; expected .bin or .nc')
ENDIF
MAP_UTIL_INPUT_KIND=0
END FUNCTION MAP_UTIL_INPUT_KIND
!####################################################################


!####################################################################
SUBROUTINE MAP_UTIL_READ_REAL2D(CFILE,VARNAME,R2DATA,IRECORD,CONTEXT)
! Read a real-valued raster from an individual Binary or NetCDF Map file.
USE YOS_CMF_INPUT, ONLY: NX,NY,LMAPEND
#ifdef UseCDF_CMF
USE YOS_CMF_INPUT, ONLY: NLFP
#endif
USE CMF_UTILS_MOD, ONLY: INQUIRE_FID,CONV_END
#ifdef UseCDF_CMF
USE NETCDF
#endif
IMPLICIT NONE
CHARACTER(LEN=*),INTENT(IN) :: CFILE,VARNAME
REAL(KIND=JPRM),INTENT(OUT) :: R2DATA(:,:)
INTEGER(KIND=JPIM),INTENT(IN),OPTIONAL :: IRECORD
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: CONTEXT
INTEGER(KIND=JPIM) :: IUNIT,IREC,IOS,FILEKIND
CHARACTER(LEN=256) :: CWHERE
#ifdef UseCDF_CMF
INTEGER(KIND=JPIM) :: NCID,VARID,STATUS,NDIMS,VARTYPE,DIMIDS(NF90_MAX_VAR_DIMS),DIMLEN
INTEGER(KIND=JPIM) :: IDIM,START3(3),COUNT3(3)
#endif

! Binary files use one direct-access record per map layer; NetCDF rasters
! use (NX,NY), or (NX,NY,NLFP) for a layered variable. The caller vectorizes.
CWHERE='CMF_CTRL_MAPS_MOD/MAP_UTIL_READ_REAL2D'
IF (PRESENT(CONTEXT)) CWHERE=TRIM(CONTEXT)//'/MAP_UTIL_READ_REAL2D'
IF (SIZE(R2DATA,1) /= NX .OR. SIZE(R2DATA,2) /= NY) THEN
  CALL MAP_UTIL_INPUT_ERROR(CFILE,'internal destination array dimensions do not match NX,NY',TRIM(CWHERE))
ENDIF
WRITE(LOGNAM,*) '  MAP_UTIL_READ_REAL2D: variable ',TRIM(VARNAME)
IREC=1
IF (PRESENT(IRECORD)) IREC=IRECORD
FILEKIND=MAP_UTIL_INPUT_KIND(CFILE,CWHERE)

IF (FILEKIND == 1) THEN
  IUNIT=INQUIRE_FID()
  OPEN(IUNIT,FILE=TRIM(CFILE),FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4*NX*NY,IOSTAT=IOS)
  IF (IOS /= 0) CALL MAP_UTIL_INPUT_ERROR(CFILE,'cannot open Binary map',TRIM(CWHERE))
  READ(IUNIT,REC=IREC,IOSTAT=IOS) R2DATA
  IF (IOS /= 0) CALL MAP_UTIL_INPUT_ERROR(CFILE,'cannot read requested Binary record',TRIM(CWHERE))
  CLOSE(IUNIT)
  IF (LMAPEND) CALL CONV_END(R2DATA,NX,NY)
  RETURN
ENDIF

#ifdef UseCDF_CMF
STATUS=NF90_OPEN(TRIM(CFILE),NF90_NOWRITE,NCID)
IF (STATUS /= NF90_NOERR) CALL MAP_UTIL_INPUT_ERROR(CFILE,'cannot open NetCDF map: '//TRIM(NF90_STRERROR(STATUS)),TRIM(CWHERE))
STATUS=NF90_INQ_VARID(NCID,TRIM(VARNAME),VARID)
IF (STATUS /= NF90_NOERR) THEN
  CALL MAP_UTIL_INPUT_ERROR(CFILE,'NetCDF variable '//TRIM(VARNAME)//' is missing',TRIM(CWHERE))
ENDIF
STATUS=NF90_INQUIRE_VARIABLE(NCID,VARID,NDIMS=NDIMS,XTYPE=VARTYPE,DIMIDS=DIMIDS)
IF (STATUS /= NF90_NOERR) CALL MAP_UTIL_INPUT_ERROR(CFILE,'cannot inspect NetCDF variable '//TRIM(VARNAME),TRIM(CWHERE))
IF (PRESENT(IRECORD)) THEN
  IF (NDIMS /= 3) CALL MAP_UTIL_INPUT_ERROR(CFILE,'variable '//TRIM(VARNAME)//' must have dimensions NX,NY,NLFP',TRIM(CWHERE))
ELSE
  IF (NDIMS /= 2) CALL MAP_UTIL_INPUT_ERROR(CFILE,'variable '//TRIM(VARNAME)//' must have dimensions NX,NY',TRIM(CWHERE))
ENDIF
DO IDIM=1,NDIMS
  STATUS=NF90_INQUIRE_DIMENSION(NCID,DIMIDS(IDIM),LEN=DIMLEN)
  IF (STATUS /= NF90_NOERR) CALL MAP_UTIL_INPUT_ERROR(CFILE,'cannot inspect a NetCDF dimension',TRIM(CWHERE))
  IF (IDIM == 1 .AND. DIMLEN /= NX) CALL MAP_UTIL_INPUT_ERROR(CFILE,'NetCDF first dimension does not equal NX',TRIM(CWHERE))
  IF (IDIM == 2 .AND. DIMLEN /= NY) CALL MAP_UTIL_INPUT_ERROR(CFILE,'NetCDF second dimension does not equal NY',TRIM(CWHERE))
  IF (IDIM == 3 .AND. DIMLEN /= NLFP) CALL MAP_UTIL_INPUT_ERROR(CFILE,'NetCDF third dimension does not equal NLFP',TRIM(CWHERE))
ENDDO
IF (VARTYPE /= NF90_FLOAT) CALL MAP_UTIL_INPUT_ERROR(CFILE,'NetCDF variable '//TRIM(VARNAME)//' must be float32',TRIM(CWHERE))
IF (PRESENT(IRECORD)) THEN
  IF (IRECORD < 1 .OR. IRECORD > NLFP) CALL MAP_UTIL_INPUT_ERROR(CFILE,'requested layer is outside 1..NLFP',TRIM(CWHERE))
  START3=(/1,1,IRECORD/)
  COUNT3=(/NX,NY,1/)
  CALL MAP_UTIL_NCERROR(NF90_GET_VAR(NCID,VARID,R2DATA,START3,COUNT3), &
                        'reading '//TRIM(VARNAME)//' from '//TRIM(CFILE),TRIM(CWHERE))
ELSE
  CALL MAP_UTIL_NCERROR(NF90_GET_VAR(NCID,VARID,R2DATA), &
                        'reading '//TRIM(VARNAME)//' from '//TRIM(CFILE),TRIM(CWHERE))
ENDIF
CALL MAP_UTIL_NCERROR(NF90_CLOSE(NCID),'closing '//TRIM(CFILE),TRIM(CWHERE))
#else
CALL MAP_UTIL_INPUT_ERROR(CFILE,'NetCDF input requires a UseCDF_CMF build',TRIM(CWHERE))
#endif

END SUBROUTINE MAP_UTIL_READ_REAL2D
!####################################################################


!####################################################################
SUBROUTINE MAP_UTIL_READ_INT2D(CFILE,VARNAME,I2DATA,IRECORD,CONTEXT)
! Read an integer-valued raster from an individual Binary or NetCDF Map file.
USE YOS_CMF_INPUT, ONLY: NX,NY,LMAPEND
USE CMF_UTILS_MOD, ONLY: INQUIRE_FID,CONV_ENDI
#ifdef UseCDF_CMF
USE NETCDF
#endif
IMPLICIT NONE
CHARACTER(LEN=*),INTENT(IN) :: CFILE,VARNAME
INTEGER(KIND=JPIM),INTENT(OUT) :: I2DATA(:,:)
INTEGER(KIND=JPIM),INTENT(IN),OPTIONAL :: IRECORD
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: CONTEXT
INTEGER(KIND=JPIM) :: IUNIT,IREC,IOS,FILEKIND
CHARACTER(LEN=256) :: CWHERE
#ifdef UseCDF_CMF
INTEGER(KIND=JPIM) :: NCID,VARID,STATUS,NDIMS,VARTYPE,DIMIDS(NF90_MAX_VAR_DIMS),DIMLEN
INTEGER(KIND=JPIM) :: IDIM
#endif

! IRECORD selects a Binary record; NetCDF network components are separate
! variables (nextx and nexty) in two-dimensional arrays.
CWHERE='CMF_CTRL_MAPS_MOD/MAP_UTIL_READ_INT2D'
IF (PRESENT(CONTEXT)) CWHERE=TRIM(CONTEXT)//'/MAP_UTIL_READ_INT2D'
IF (SIZE(I2DATA,1) /= NX .OR. SIZE(I2DATA,2) /= NY) THEN
  CALL MAP_UTIL_INPUT_ERROR(CFILE,'internal destination array dimensions do not match NX,NY',TRIM(CWHERE))
ENDIF
WRITE(LOGNAM,*) '  MAP_UTIL_READ_INT2D: variable ',TRIM(VARNAME)
IREC=1
IF (PRESENT(IRECORD)) IREC=IRECORD
FILEKIND=MAP_UTIL_INPUT_KIND(CFILE,CWHERE)

IF (FILEKIND == 1) THEN
  IUNIT=INQUIRE_FID()
  OPEN(IUNIT,FILE=TRIM(CFILE),FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4*NX*NY,IOSTAT=IOS)
  IF (IOS /= 0) CALL MAP_UTIL_INPUT_ERROR(CFILE,'cannot open Binary map',TRIM(CWHERE))
  READ(IUNIT,REC=IREC,IOSTAT=IOS) I2DATA
  IF (IOS /= 0) CALL MAP_UTIL_INPUT_ERROR(CFILE,'cannot read requested Binary record',TRIM(CWHERE))
  CLOSE(IUNIT)
  IF (LMAPEND) CALL CONV_ENDI(I2DATA,NX,NY)
  RETURN
ENDIF

#ifdef UseCDF_CMF
STATUS=NF90_OPEN(TRIM(CFILE),NF90_NOWRITE,NCID)
IF (STATUS /= NF90_NOERR) CALL MAP_UTIL_INPUT_ERROR(CFILE,'cannot open NetCDF map: '//TRIM(NF90_STRERROR(STATUS)),TRIM(CWHERE))
STATUS=NF90_INQ_VARID(NCID,TRIM(VARNAME),VARID)
IF (STATUS /= NF90_NOERR) THEN
  CALL MAP_UTIL_INPUT_ERROR(CFILE,'NetCDF variable '//TRIM(VARNAME)//' is missing',TRIM(CWHERE))
ENDIF
STATUS=NF90_INQUIRE_VARIABLE(NCID,VARID,NDIMS=NDIMS,XTYPE=VARTYPE,DIMIDS=DIMIDS)
IF (STATUS /= NF90_NOERR) CALL MAP_UTIL_INPUT_ERROR(CFILE,'cannot inspect NetCDF variable '//TRIM(VARNAME),TRIM(CWHERE))
IF (NDIMS /= 2) CALL MAP_UTIL_INPUT_ERROR(CFILE,'variable '//TRIM(VARNAME)//' must have dimensions NX,NY',TRIM(CWHERE))
DO IDIM=1,2
  STATUS=NF90_INQUIRE_DIMENSION(NCID,DIMIDS(IDIM),LEN=DIMLEN)
  IF (STATUS /= NF90_NOERR) CALL MAP_UTIL_INPUT_ERROR(CFILE,'cannot inspect a NetCDF dimension',TRIM(CWHERE))
  IF (IDIM == 1 .AND. DIMLEN /= NX) CALL MAP_UTIL_INPUT_ERROR(CFILE,'NetCDF first dimension does not equal NX',TRIM(CWHERE))
  IF (IDIM == 2 .AND. DIMLEN /= NY) CALL MAP_UTIL_INPUT_ERROR(CFILE,'NetCDF second dimension does not equal NY',TRIM(CWHERE))
ENDDO
IF (VARTYPE /= NF90_INT) CALL MAP_UTIL_INPUT_ERROR(CFILE,'NetCDF variable '//TRIM(VARNAME)//' must be int32',TRIM(CWHERE))
CALL MAP_UTIL_NCERROR(NF90_GET_VAR(NCID,VARID,I2DATA), &
                      'reading '//TRIM(VARNAME)//' from '//TRIM(CFILE),TRIM(CWHERE))
CALL MAP_UTIL_NCERROR(NF90_CLOSE(NCID),'closing '//TRIM(CFILE),TRIM(CWHERE))
#else
CALL MAP_UTIL_INPUT_ERROR(CFILE,'NetCDF input requires a UseCDF_CMF build',TRIM(CWHERE))
#endif

END SUBROUTINE MAP_UTIL_READ_INT2D
!####################################################################


END MODULE CMF_CTRL_MAPS_MOD
