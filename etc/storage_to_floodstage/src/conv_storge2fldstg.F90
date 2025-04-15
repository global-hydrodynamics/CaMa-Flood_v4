      program conv_storge2fldstg
! ===============================================
! Diagnose Flood Stage (Depth, Flood Area, Storage in river and floodplain) from Total Storage
! Currently only available for plain binary data. 
!   Apr 2025, by Dai Yamazaki
! ===============================================
      implicit none
! Real Type Definition
      INTEGER, PARAMETER :: JPRM = SELECTED_REAL_KIND(6,37)  !! 4 byte float
      INTEGER, PARAMETER :: JPRB = SELECTED_REAL_KIND(6,37)  !! JPRB is switchable (4 byte in Single Precision Mode)
      !INTEGER, PARAMETER :: JPRD = SELECTED_REAL_KIND(13,300) 
      INTEGER, PARAMETER :: JPRD = SELECTED_REAL_KIND(6,37) 

! CaMa-Flood parameters       
      integer                        ::  nX, nY                      !! grid number (river network map)
      integer                        ::  I, ILEV, NLFP                          !! floodplain layers
      real                           ::  gsize                         !! grid size [deg]
      integer                        ::  ISEQ, NSEQALL, NSEQMAX
      integer                        ::  IREC

      character*256                  ::  CSETFILE
      INTEGER                        ::  NSETFILE, TMPNAM, LOGNAM

!*** NAMELIST/NMAP/ from inputnam
      !* river map and topography
      CHARACTER(LEN=256)             :: CPARAMS         !! map parameter
      CHARACTER(LEN=256)             :: CNEXTXY         !! river network nextxy
      CHARACTER(LEN=256)             :: CGRAREA         !! catchment area
      CHARACTER(LEN=256)             :: CELEVTN         !! bank top elevation
      CHARACTER(LEN=256)             :: CRIVLEN         !! river channel length
      CHARACTER(LEN=256)             :: CFLDHGT         !! floodplain elevation profile
      CHARACTER(LEN=256)             :: CRIVWTH         !! channel width
      CHARACTER(LEN=256)             :: CRIVHGT         !! channel depth
      !* levee scheme
      LOGICAL                        :: LLEVEE          !! true for using levee scheme
      CHARACTER(LEN=256)             :: CLEVFRC         !! levee unprotected fraction [0-1]
      CHARACTER(LEN=256)             :: CLEVHGT         !! levee height [m]
      !* input storage file
      INTEGER                        :: NREC            !! number of input data record
      CHARACTER(LEN=256)             :: CSTORGE         !! (input) simulated storage 
      !* output variable files
      CHARACTER(LEN=256)             :: COUTDIR         !! (output) directory to save output data
      CHARACTER(LEN=256)             :: CVARSOUT        !! (output) Comma-separated list of output variables to save 
      
      NAMELIST/NMAP/     CPARAMS,  CNEXTXY,  CGRAREA,  CELEVTN,  CRIVLEN, CFLDHGT, CRIVWTH,  CRIVHGT, &
                         LLEVEE,   CLEVFRC,  CLEVHGT, &
                         NREC,     CSTORGE,  COUTDIr, CVARSOUT

!*** river network map input
      ! River Map
      INTEGER,ALLOCATABLE           ::  I2NEXTX(:,:)       !! POINT DOWNSTREAM HORIZONTAL
      REAL,ALLOCATABLE              ::  D2GRAREA(:,:)      !! GRID AREA [M2]
      REAL,ALLOCATABLE              ::  D2ELEVTN(:,:)      !! ELEVATION [M]
      REAL,ALLOCATABLE              ::  D2RIVLEN(:,:)      !! RIVER LENGTH [M]
      REAL,ALLOCATABLE              ::  D2RIVWTH(:,:)      !! RIVER WIDTH [M]
      REAL,ALLOCATABLE              ::  D2RIVHGT(:,:)      !! RIVER HEIGHT [M]
      REAL,ALLOCATABLE              ::  D2FLDHGT(:,:,:)    !! FLOODPLAIN HEIGHT [M]
      ! Levee parameter map
      REAL,ALLOCATABLE              ::  D2LEVFRC(:,:)      !! levee unprotected fraction (or relative distance of levee from river centerline)
      REAL,ALLOCATABLE              ::  D2LEVHGT(:,:)      !! levee height [m]

!*** Internal topo parameters
      ! Floodplain topo paramters (Internally-calculated)
      REAL,ALLOCATABLE              ::  D2RIVELV(:,:)      !! elevation of river bed [m3]
      REAL,ALLOCATABLE              ::  D2RIVSTOMAX(:,:)   !! maximum river storage [m3]
      REAL,ALLOCATABLE              ::  D2FLDSTOMAX(:,:,:) !! MAXIMUM FLOODPLAIN STORAGE [M3]
      REAL,ALLOCATABLE              ::  D2FLDGRD(:,:,:)    !! FLOODPLAIN GRADIENT

      ! Levee stage parameter (internally-calculated calculated)
      REAL,ALLOCATABLE              ::  D2BASHGT(:,:)        !! LEVEE Base height [M] (levee base elevation above elevtn.bin-river elev)
      REAL,ALLOCATABLE              ::  D2LEVDST(:,:)        !! Absolute DISTANCE between LEVEE and RIVER [0-1]. 
                                                      !! 0 = just aside channel, 1 = edge of catchment
      REAL,ALLOCATABLE              ::  D2LEVBASSTO(:,:)     !! MAXIMUM STORAGE under LEVEE BASE [M3]
      REAL,ALLOCATABLE              ::  D2LEVTOPSTO(:,:)     !! MAXIMUM STORAGE at LEVEE TOP [M3] (only river side)
      REAL,ALLOCATABLE              ::  D2LEVFILSTO(:,:)     !! MAXIMUM STORAGE at LEVEE TOP [M3] (both river & protected side are filled)

      ! internal variables for calculation
      REAL                          ::  DFRCINC                             !! FLOODPLAIN FRACTION INCREMENT [-] (1/NLFP)
      REAL                          ::  DDPHNOW, DDPHPRE                  
      REAL                          ::  DWTHNOW, DWTHPRE, DWTHINC         
      REAL                          ::  DSTONOW, DSTOPRE, DSTOALL, DSTOADD
      REAL                          ::  DHGTNOW, DHGTPRE, DHGTDIF

!*** Input total storage
      REAL,ALLOCATABLE              ::  D2STORGE(:,:)        !! Total Water Storage [m3] (input)

!*** Output flood stage variables (diagnosed)
      REAL,ALLOCATABLE              ::  D2RIVSTO(:,:)        !! river storage [m3]
      REAL,ALLOCATABLE              ::  D2RIVDPH(:,:)        !! river depth [m]
      REAL,ALLOCATABLE              ::  D2FLDSTO(:,:)        !! floodplain storage [m3]
      REAL,ALLOCATABLE              ::  D2FLDDPH(:,:)        !! floodplain depth [m]
      REAL,ALLOCATABLE              ::  D2FLDFRC(:,:)        !! flooded fraction [ratio: 0-1]
      REAL,ALLOCATABLE              ::  D2FLDARE(:,:)        !! flooded area [m]
      REAL,ALLOCATABLE              ::  D2SFCELV(:,:)        !! water surface elevation [m]
      ! levee scheme variables
      REAL,ALLOCATABLE              ::  D2LEVSTO(:,:)        !! flood storage in protected side 
      REAL,ALLOCATABLE              ::  D2LEVDPH(:,:)        !! water level   in protected side 

!*** Output Files
      INTEGER                         :: NVARS               ! temporal output var number
      PARAMETER                         (NVARS=100)
      INTEGER                         :: NVARSOUT            ! actual   output var number
      CHARACTER(LEN=256)              :: CTMP
      INTEGER                         :: JF,J,J0
      CHARACTER(LEN=256)              :: CVNAMES(NVARS)      ! variable name
      INTEGER                         :: ID_VAROUT(NVARS)    ! file access ID
      CHARACTER(LEN=256)              :: COUTFILE            ! output file name
! ===============================================
print *, ""
print *, "=== conv_storge2fldstg =========="
! Default setting (no need to change)
      ! File IO number
      NSETFILE=111
      TMPNAM=12
      LOGNAM=6   !! standard output 6
      
! Default namelist values
      CSETFILE='./input.nam'
      CPARAMS="./map/params.txt"
      CNEXTXY="./map/nextxy.bin"
      CGRAREA="./map/ctmare.bin"
      CELEVTN="./map/elevtn.bin"
      CRIVLEN="./map/rivlen.bin"
      CFLDHGT="./map/fldhgt.bin"
      CRIVWTH="./map/rivwth.bin"
      CRIVHGT="./map/rivhgt.bin"
      
      LLEVEE =.false.
      CLEVFRC="./map/levfrc.bin"
      CLEVHGT="./map/levhgt.bin"
      
      NREC=1
      CSTORGE="./out/storge.bin"
      COUTDIR="./output/"
      CVARSOUT='rivsto,rivdph,fldsto,flddph,fldfec,fldare,sfcelv'
      !CVARSOUT='rivsto,rivdph,fldsto,flddph,fldfec,fldare,sfcelv,levsto,levdph'

!! Read Namelist & Parameter File
      print *, 'Read Namelist', trim(CSETFILE)
      OPEN(NSETFILE,FILE=CSETFILE,STATUS="OLD")
      REWIND(NSETFILE)
      READ(NSETFILE,NML=NMAP)
      CLOSE(NSETFILE)

      print *, 'Read Map Parameter', trim(CPARAMS)
      open(11,file=CPARAMS,form='formatted')
      read(11,*) nX
      read(11,*) nY
      read(11,*) NLFP
      read(11,*) gsize
      close(11)

      NSEQALL=nX*nY
      NSEQMAX=NSEQALL
      print *, 'input storage data', trim(CSTORGE)
      print *, 'nX, nY, nseqall',  nX, nY, nseqall
      print *, 'NREC=', NREC

      ! allocate map variables
      ALLOCATE( I2NEXTX(NSEQMAX,1) )
      ALLOCATE( D2GRAREA(NSEQMAX,1) )
      ALLOCATE( D2ELEVTN(NSEQMAX,1) )
      ALLOCATE( D2RIVLEN(NSEQMAX,1) )
      ALLOCATE( D2RIVWTH(NSEQMAX,1) )
      ALLOCATE( D2RIVHGT(NSEQMAX,1) )
      ALLOCATE( D2FLDHGT(NSEQMAX,1,NLFP) )

      ! internal parameter
      ALLOCATE( D2RIVELV(NSEQMAX,1) )
      ALLOCATE( D2RIVSTOMAX(NSEQMAX,1) )
      ALLOCATE( D2FLDSTOMAX(NSEQMAX,1,NLFP) )
      ALLOCATE( D2FLDGRD(NSEQMAX,1,NLFP) )

      !! levee scheme
      IF( LLEVEE )THEN
        !! levee input parameter map
        ALLOCATE( D2LEVFRC(NSEQMAX,1) )
        ALLOCATE( D2LEVHGT(NSEQMAX,1) )
        !! internal parameter 
        ALLOCATE( D2BASHGT(NSEQMAX,1) )
        ALLOCATE( D2LEVDST(NSEQMAX,1) )
        ALLOCATE( D2LEVBASSTO(NSEQMAX,1) )
        ALLOCATE( D2LEVTOPSTO(NSEQMAX,1) )
        ALLOCATE( D2LEVFILSTO(NSEQMAX,1) )
      ENDIF
!================================
print *, '====================='
print *, 'read map files'

      WRITE(LOGNAM,*)'TOPO_INIT: river network : ',TRIM(CNEXTXY)
      OPEN(TMPNAM,FILE=CNEXTXY,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4*NX*NY)
      READ(TMPNAM,REC=1) I2NEXTX
      CLOSE(TMPNAM)
      
      WRITE(LOGNAM,*)'TOPO_INIT: unit-catchment area : ',TRIM(CGRAREA) 
      OPEN(TMPNAM,FILE=CGRAREA,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4*NX*NY)
      READ(TMPNAM,REC=1) D2GRAREA
      CLOSE(TMPNAM)
      
      WRITE(LOGNAM,*)'TOPO_INIT: ground elevation : ',TRIM(CELEVTN)
      OPEN(TMPNAM,FILE=CELEVTN,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4*NX*NY)
      READ(TMPNAM,REC=1) D2ELEVTN
      CLOSE(TMPNAM)
      
      WRITE(LOGNAM,*)'TOPO_INIT: river channel length : ',TRIM(CRIVLEN)
      OPEN(TMPNAM,FILE=CRIVLEN,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4*NX*NY)
      READ(TMPNAM,REC=1) D2RIVLEN
      CLOSE(TMPNAM)
      
      WRITE(LOGNAM,*)'TOPO_INIT: floodplain elevation profile : ',TRIM(CFLDHGT)
      OPEN(TMPNAM,FILE=TRIM(CFLDHGT),FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4*NX*NY)
      DO ILEV=1,NLFP
        READ(TMPNAM,REC=ILEV) D2FLDHGT(:,:,ILEV)
      ENDDO
      CLOSE(TMPNAM)
      
      WRITE(LOGNAM,*)'TOPO_INIT: river channel depth : ',TRIM(CRIVHGT)
      OPEN(TMPNAM,FILE=CRIVHGT,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4*NX*NY)
      READ(TMPNAM,REC=1) D2RIVHGT
      CLOSE(TMPNAM)
      
      WRITE(LOGNAM,*)'TOPO_INIT: river channel width : ',TRIM(CRIVWTH)
      OPEN(TMPNAM,FILE=CRIVWTH,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4*NX*NY)
      READ(TMPNAM,REC=1) D2RIVWTH
      CLOSE(TMPNAM)
      
      IF( LLEVEE )THEN
        WRITE(LOGNAM,*)'TOPO_INIT: levee fraction : ',TRIM(CLEVFRC)
        OPEN(TMPNAM,FILE=CLEVFRC,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4*NX*NY)
        READ(TMPNAM,REC=1) D2LEVFRC
        CLOSE(TMPNAM)
        
        WRITE(LOGNAM,*)'TOPO_INIT: levee height : ',TRIM(CLEVHGT)
        OPEN(TMPNAM,FILE=CLEVHGT,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4*NX*NY)
        READ(TMPNAM,REC=1) D2LEVHGT
        CLOSE(TMPNAM)
      
        DO ISEQ=1, NSEQALL
          IF( I2NEXTX(ISEQ,1)<=-999 ) CYCLE
          !! exception treatment: check NaN 
          IF( D2LEVFRC(ISEQ,1)*0.0 /= 0.0 ) D2LEVFRC(ISEQ,1)=1.0 !! no levee
          IF( D2LEVHGT(ISEQ,1)*0.0 /= 0.0 ) D2LEVHGT(ISEQ,1)=1.0 !! no levee
        END DO
      ENDIF

!================================
print *, '====================='
print *, 'Calculate Floodplain Topography Parameters'

      D2RIVELV(:,:)      =-9999.
      D2RIVSTOMAX(:,:)   =-9999.
      D2FLDSTOMAX(:,:,:) =-9999.
      D2FLDGRD(:,:,:)    =-9999.
      
      DFRCINC=NLFP**(-1.)


      DO ISEQ=1, NSEQALL
        IF( I2NEXTX(ISEQ,1)<=-999 ) CYCLE
      
        D2RIVELV(ISEQ,1)    = D2ELEVTN(ISEQ,1) - D2RIVHGT(ISEQ,1)
        D2RIVSTOMAX(ISEQ,1) = D2RIVLEN(ISEQ,1) * D2RIVWTH(ISEQ,1) * D2RIVHGT(ISEQ,1)
        
        DSTOPRE = D2RIVSTOMAX(ISEQ,1)
        DHGTPRE = 0.
        DWTHINC = D2GRAREA(ISEQ,1) * D2RIVLEN(ISEQ,1)**(-1.) * DFRCINC
        DO I=1, NLFP
          DSTONOW = D2RIVLEN(ISEQ,1) * ( D2RIVWTH(ISEQ,1) + DWTHINC*(REAL(I)-0.5) ) * (D2FLDHGT(ISEQ,1,I)-DHGTPRE)
          D2FLDSTOMAX(ISEQ,1,I) = DSTOPRE + DSTONOW
          D2FLDGRD(ISEQ,1,I) = (D2FLDHGT(ISEQ,1,I)-DHGTPRE) * DWTHINC**(-1.)
          DSTOPRE = D2FLDSTOMAX(ISEQ,1,I)
          DHGTPRE = D2FLDHGT(ISEQ,1,I)
        END DO
      END DO
      
      !! update topo parameter for LEVEE case
      IF( LLEVEE ) CALL CALC_LEVEE_PARAM


!================================
print *, '====================='
print *, 'Set output files'
      ! Check output variab
      NVARSOUT=0
      J0=1
      DO J=1,LEN(TRIM(CVARSOUT))
        IF( (J>J0) .AND. (CVARSOUT(J:J) .EQ. ',') ) THEN
          CTMP=TRIM(ADJUSTL(CVARSOUT(J0:J-1)))
          IF (LEN(CTMP) > 0 ) THEN
            NVARSOUT=NVARSOUT+1
            CVNAMES(NVARSOUT)=CTMP
          ENDIF
          J0=J+1
        ENDIF
      ENDDO
      ! Last one 
      IF ( J0 <= LEN(TRIM(CVARSOUT)) ) THEN
        J=LEN(TRIM(CVARSOUT))
        CTMP=TRIM(ADJUSTL(CVARSOUT(J0:J)))
        IF (LEN(CTMP) > 0 ) THEN
           NVARSOUT=NVARSOUT+1
           CVNAMES(NVARSOUT)=CTMP
        ENDIF
      ENDIF 
      print *, NVARSOUT, TRIM(CVARSOUT)

      !* Loop on variables and create files 
      DO JF=1,NVARSOUT
        WRITE(LOGNAM,*) "Creating output for variable:", TRIM( CVNAMES(JF) )
        ID_VAROUT(JF)=200+JF
        COUTFILE=trim(COUTDIR)//'/'//trim(CVNAMES(JF))//".bin"
        print *, trim(COUTFILE)
        OPEN(ID_VAROUT(JF),FILE=COUTFILE,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4*NX*NY)
      END DO

!================================
! allocate diagnostic variable
      ALLOCATE( D2STORGE(NSEQMAX,1) )
      ALLOCATE( D2RIVSTO(NSEQMAX,1) )
      ALLOCATE( D2RIVDPH(NSEQMAX,1) )
      ALLOCATE( D2FLDSTO(NSEQMAX,1) )
      ALLOCATE( D2FLDDPH(NSEQMAX,1) )
      ALLOCATE( D2FLDFRC(NSEQMAX,1) )
      ALLOCATE( D2FLDARE(NSEQMAX,1) )
      ALLOCATE( D2SFCELV(NSEQMAX,1) )
      
      IF( LLEVEE )THEN
        ALLOCATE( D2LEVSTO(NSEQMAX,1) )
        ALLOCATE( D2LEVDPH(NSEQMAX,1) )
      ENDIF

!================================
print *, 'Main calculation: convert storage to flood stage'
      WRITE(LOGNAM,*)'Input Storage File: ',TRIM(CSTORGE)
      OPEN(31,FILE=CSTORGE,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4*NX*NY)

      do irec=1, nrec
        !! read input storage
        read(31,rec=irec) D2STORGE
      
        !! diagnose flood stage
        IF( LLEVEE) THEN
          CALL CALC_FLDSTG_LEVEE
        ELSE
          call CALC_FLDSTG
        ENDIF
      
        !! save output data
        call WRITE_OUTVAR
      end do
      CLOSE(31)



      CONTAINS
!===============================================
      SUBROUTINE CALC_FLDSTG
      
      D2RIVSTO(:,:)=-9999
      D2RIVDPH(:,:)=-9999
      
      D2FLDSTO(:,:)=-9999
      D2FLDDPH(:,:)=-9999
      D2FLDFRC(:,:)=-9999
      D2FLDARE(:,:)=-9999
      
      D2SFCELV(:,:)=-9999
      
      DO ISEQ=1, NSEQALL
        IF( I2NEXTX(ISEQ,1)<=-999 ) cycle
      
        DSTOALL = D2STORGE(ISEQ,1)
        IF( DSTOALL<1.E-20 ) DSTOALL=0
      
        IF( DSTOALL > D2RIVSTOMAX(ISEQ,1) )THEN
          I=1
          DSTOPRE = D2RIVSTOMAX(ISEQ,1)
          DWTHPRE = D2RIVWTH(ISEQ,1)
          DDPHPRE = 0.
          DWTHINC = D2GRAREA(ISEQ,1) * D2RIVLEN(ISEQ,1)**(-1.) * DFRCINC
          DO WHILE( DSTOALL > D2FLDSTOMAX(ISEQ,1,I) .AND. I<=NLFP)
            DSTOPRE = D2FLDSTOMAX(ISEQ,1,I)
            DWTHPRE = DWTHPRE + DWTHINC
            DDPHPRE = DDPHPRE + D2FLDGRD(ISEQ,1,I) * DWTHINC
            I=I+1
            IF( I>NLFP ) EXIT
          END DO
          IF( I>NLFP )THEN
            DSTONOW = DSTOALL - DSTOPRE
            DWTHNOW = 0.
            D2FLDDPH(ISEQ,1) = DDPHPRE + DSTONOW * DWTHPRE**(-1.) * D2RIVLEN(ISEQ,1)**(-1.)
          ELSE
            DSTONOW =  DSTOALL - DSTOPRE
            DWTHNOW = -DWTHPRE + &
      &      ( DWTHPRE**2. + 2. * DSTONOW * D2RIVLEN(ISEQ,1)**(-1.) * D2FLDGRD(ISEQ,1,I)**(-1.) )**0.5
            D2FLDDPH(ISEQ,1) = DDPHPRE + D2FLDGRD(ISEQ,1,I) * DWTHNOW
          ENDIF
          D2RIVSTO(ISEQ,1) = D2RIVSTOMAX(ISEQ,1) + D2RIVLEN(ISEQ,1) * D2RIVWTH(ISEQ,1) * D2FLDDPH(ISEQ,1)
          D2RIVSTO(ISEQ,1) = MIN(D2RIVSTO(ISEQ,1),DSTOALL)
      
          D2RIVDPH(ISEQ,1) = D2RIVSTO(ISEQ,1) * D2RIVLEN(ISEQ,1)**(-1.) * D2RIVWTH(ISEQ,1)**(-1.)
      !
          D2FLDSTO(ISEQ,1) = DSTOALL - D2RIVSTO(ISEQ,1)
          D2FLDSTO(ISEQ,1) = MAX( D2FLDSTO(ISEQ,1), 0. )
          D2FLDFRC(ISEQ,1) = (-D2RIVWTH(ISEQ,1) + DWTHPRE + DWTHNOW ) * (DWTHINC*NLFP)**(-1.)  !! bugfix 191113, (10._JPRB -> NLFP)
          D2FLDFRC(ISEQ,1) = MAX( D2FLDFRC(ISEQ,1),0.)
          D2FLDFRC(ISEQ,1) = MIN( D2FLDFRC(ISEQ,1),1.)
          D2FLDARE(ISEQ,1) = D2GRAREA(ISEQ,1)*D2FLDFRC(ISEQ,1)
        ELSE
          D2RIVSTO(ISEQ,1) = DSTOALL
          D2RIVDPH(ISEQ,1) = DSTOALL * D2RIVLEN(ISEQ,1)**(-1.) * D2RIVWTH(ISEQ,1)**(-1.)
          D2RIVDPH(ISEQ,1) = MAX( D2RIVDPH(ISEQ,1), 0. )
          D2FLDSTO(ISEQ,1) = 0.
          D2FLDDPH(ISEQ,1) = 0.
          D2FLDFRC(ISEQ,1) = 0.
          D2FLDARE(ISEQ,1) = 0.
        ENDIF
        D2SFCELV(ISEQ,1)     = D2RIVELV(ISEQ,1) + D2RIVDPH(ISEQ,1)
      END DO
      
      END SUBROUTINE CALC_FLDSTG
!===========================================
!+
!+
!+
!===========================================
      SUBROUTINE CALC_LEVEE_PARAM
print *, 'CALC_LEVEE_PARAM: Calculate LEVEE Topography Parameters'

      D2FLDSTOMAX(:,:,:) = 0.   !! max floodplain  storage  at each layer
      D2FLDGRD(:,:,:)    = 0.   !! floodplain topo gradient of each layer
      DFRCINC=NLFP**(-1.)       !! fration of each layer
      
      D2LEVBASSTO(:,:)= 0.      !! storage at levee base     (levee protection start)
      D2LEVTOPSTO(:,:)= 0.      !! storage at levee top      (levee protection end)
      D2LEVFILSTO(:,:)= 0.      !! storage when levee filled (protected-side depth reach levee top)
      
      DO ISEQ=1, NSEQALL
        IF( I2NEXTX(ISEQ,1)<=-999 )CYCLE
      
        IF( D2LEVHGT(ISEQ,1)<=0. )THEN
          D2LEVHGT(ISEQ,1)=0.
          D2LEVFRC(ISEQ,1)=1.   !! If no levee, all area is unprotected/
        ENDIF
        D2LEVFRC(ISEQ,1)=MAX(0.,MIN(1.,D2LEVFRC(ISEQ,1)))
      END DO

      DO ISEQ=1, NSEQALL
        IF( I2NEXTX(ISEQ,1)<=-999 )CYCLE
      
      ! calculate floodplain parameters (without levee, same as SET_FLDSTG)
        DSTOPRE = D2RIVSTOMAX(ISEQ,1)
        DHGTPRE = 0.
        DWTHINC = D2GRAREA(ISEQ,1) * D2RIVLEN(ISEQ,1)**(-1.) * DFRCINC  !! width increlment for each layer
        DO I=1, NLFP
          DSTONOW = D2RIVLEN(ISEQ,1) * ( D2RIVWTH(ISEQ,1) + DWTHINC*(REAL(I)-0.5) ) * (D2FLDHGT(ISEQ,1,I)-DHGTPRE)  !! storage increment
          D2FLDSTOMAX(ISEQ,1,I) = DSTOPRE + DSTONOW
          D2FLDGRD(ISEQ,1,I) = (D2FLDHGT(ISEQ,1,I)-DHGTPRE) * DWTHINC**(-1.)
          DSTOPRE = D2FLDSTOMAX(ISEQ,1,I)
          DHGTPRE = D2FLDHGT(ISEQ,1,I)
        END DO
      
      ! Levee parameters calculation
        IF( D2LEVHGT(ISEQ,1) == 0. )THEN ! Grid without levee, treat everything as unprotected
          D2BASHGT(ISEQ,1) = 1.E18
          D2LEVDST(ISEQ,1) = 1.E18
          D2LEVBASSTO(ISEQ,1) = 1.E18
          D2LEVTOPSTO(ISEQ,1) = 1.E18
          D2LEVFILSTO(ISEQ,1) = 1.E18
        ELSE  !! levee exist
          !!*********
          !! [1] levee base storage & levee top storage (water only in river side)
      
          DSTOPRE = D2RIVSTOMAX(ISEQ,1)
          DHGTPRE = 0.
          DWTHPRE = 0.
          D2LEVDST(ISEQ,1) = D2LEVFRC(ISEQ,1) * DWTHINC*NLFP !! distance from channel to levee [m]
      
          ILEV=INT( D2LEVFRC(ISEQ,1)*NLFP )+1 !! which layer levee exist
          IF( ILEV>=2 )THEN
            DSTOPRE = D2FLDSTOMAX(ISEQ,1,ILEV-1)
            DHGTPRE = D2FLDHGT(ISEQ,1,ILEV-1)
            DWTHPRE = DWTHINC * (ILEV-1)
          ENDIF
      
          IF( ILEV<=NLFP )THEN
            !! levee in floodplain layer ILEV
            DWTHNOW = D2LEVDST(ISEQ,1) - DWTHPRE
            DHGTNOW = DWTHNOW * D2FLDGRD(ISEQ,1,ILEV) !! levee height above lower floodplain profile point
            D2BASHGT(ISEQ,1) = DHGTNOW + DHGTPRE
            D2LEVHGT(ISEQ,1) = max( D2LEVHGT(ISEQ,1), D2BASHGT(ISEQ,1) ) !! levee height >= base height
      
            DSTONOW = ( DWTHNOW*0.5 + DWTHPRE + D2RIVWTH(ISEQ,1) ) * DHGTNOW * D2RIVLEN(ISEQ,1) 
            D2LEVBASSTO(ISEQ,1) = DSTOPRE + DSTONOW
      
            DHGTDIF = D2LEVHGT(ISEQ,1) - D2BASHGT(ISEQ,1)
            D2LEVTOPSTO(ISEQ,1) = D2LEVBASSTO(ISEQ,1) + ( D2LEVDST(ISEQ,1)+D2RIVWTH(ISEQ,1) ) * DHGTDIF * D2RIVLEN(ISEQ,1)
          ELSE
            !! levee on the floodplain edge (ILEV=NLEV+1)
            D2BASHGT(ISEQ,1) = DHGTPRE
            D2LEVHGT(ISEQ,1) = max( D2LEVHGT(ISEQ,1), D2BASHGT(ISEQ,1) ) !! levee height >= base height
      
            D2LEVBASSTO(ISEQ,1) = DSTOPRE
      
            DHGTDIF = D2LEVHGT(ISEQ,1) - D2BASHGT(ISEQ,1)
            D2LEVTOPSTO(ISEQ,1) = D2LEVBASSTO(ISEQ,1) + ( D2LEVDST(ISEQ,1)+D2RIVWTH(ISEQ,1) ) * DHGTDIF * D2RIVLEN(ISEQ,1)
          ENDIF
      
          !!*********
          !! [2] levee fill storage (water in both river side & protected side)
          I=1
          DSTOPRE = D2RIVSTOMAX(ISEQ,1)
          DWTHPRE = D2RIVWTH(ISEQ,1)
          DHGTPRE = 0.
      
          !! check which layer levee top belongs
          DO WHILE( D2LEVHGT(ISEQ,1) > D2FLDHGT(ISEQ,1,I) .AND. I<=NLFP )
            DSTOPRE = D2FLDSTOMAX(ISEQ,1,I)
            DWTHPRE = DWTHPRE + DWTHINC
            DHGTPRE = D2FLDHGT(ISEQ,1,I)
            I=I+1
            IF( I>NLFP ) EXIT
          END DO
      
          !! calculate levee fill volume
          IF( I<=NLFP )THEN 
            !! levee top height collesponds to layer I
            DHGTNOW = D2LEVHGT(ISEQ,1) - DHGTPRE
            DWTHNOW = DHGTNOW * D2FLDGRD(ISEQ,1,I)**(-1.)
      
            DSTONOW = ( DWTHNOW*0.5 + DWTHPRE ) * DHGTNOW * D2RIVLEN(ISEQ,1)
            D2LEVFILSTO(ISEQ,1) = DSTOPRE + DSTONOW
          ELSE
            !! levee higher than catchment boundary height
            DHGTNOW = D2LEVHGT(ISEQ,1) - DHGTPRE
            DSTONOW = DWTHPRE * DHGTNOW * D2RIVLEN(ISEQ,1)
            D2LEVFILSTO(ISEQ,1) = DSTOPRE + DSTONOW
          ENDIF
        ENDIF
      
      END DO
      
      END SUBROUTINE CALC_LEVEE_PARAM
!=====================================
!+
!+
!+
!=====================================
SUBROUTINE CALC_FLDSTG_LEVEE

      D2RIVSTO(:,:)=-9999
      D2RIVDPH(:,:)=-9999
      
      D2FLDSTO(:,:)=-9999
      D2FLDDPH(:,:)=-9999
      D2FLDFRC(:,:)=-9999
      D2FLDARE(:,:)=-9999
      
      D2SFCELV(:,:)=-9999
      D2LEVSTO(:,:)=-9999
      D2LEVDPH(:,:)=-9999

      DO ISEQ=1, NSEQALL
        IF( I2NEXTX(ISEQ,1)<=-999 ) CYCLE
      !
        DSTOALL = D2STORGE(ISEQ,1)
        IF( DSTOALL<1.E-20 ) DSTOALL=0
      
        DWTHINC = D2GRAREA(ISEQ,1) * D2RIVLEN(ISEQ,1)**(-1.) * DFRCINC    !! width of each layer [m]
        IF( DSTOALL > D2RIVSTOMAX(ISEQ,1) )THEN
          !**********
          ! [Case-1] Water surface is under levee base (all water is between river-levee)
          IF( DSTOALL < D2LEVBASSTO(ISEQ,1) )THEN 
            I=1
            DSTOPRE = D2RIVSTOMAX(ISEQ,1)
            DWTHPRE = D2RIVWTH(ISEQ,1)
            DDPHPRE = 0.
      
            ! which layer current water level is
            DO WHILE( DSTOALL > D2FLDSTOMAX(ISEQ,1,I) .AND. I<=NLFP )
              DSTOPRE = D2FLDSTOMAX(ISEQ,1,I)
              DWTHPRE = DWTHPRE + DWTHINC
              DDPHPRE = DDPHPRE + D2FLDGRD(ISEQ,1,I) * DWTHINC
              I=I+1
              IF( I>NLFP ) EXIT
            END DO
      
            ! water depth at unprotected area
            IF( I<=NLFP )THEN
              DSTONOW =  DSTOALL - DSTOPRE
              DWTHNOW = -DWTHPRE + ( DWTHPRE**2. + 2. * DSTONOW * D2RIVLEN(ISEQ,1)**(-1.) * D2FLDGRD(ISEQ,1,I)**(-1.) )**0.5
              D2FLDDPH(ISEQ,1) = DDPHPRE + D2FLDGRD(ISEQ,1,I) * DWTHNOW
            ELSE
              DSTONOW = DSTOALL - DSTOPRE
              DWTHNOW = 0.
              D2FLDDPH(ISEQ,1) = DDPHPRE + DSTONOW * DWTHPRE**(-1.) * D2RIVLEN(ISEQ,1)**(-1.)
            ENDIF
      
            D2RIVSTO(ISEQ,1) = D2RIVSTOMAX(ISEQ,1) + D2RIVLEN(ISEQ,1) * D2RIVWTH(ISEQ,1) * D2FLDDPH(ISEQ,1)
            D2RIVDPH(ISEQ,1) = D2RIVSTO(ISEQ,1) * D2RIVLEN(ISEQ,1)**(-1.) * D2RIVWTH(ISEQ,1)**(-1.)
      !
            D2FLDSTO(ISEQ,1) = DSTOALL - D2RIVSTO(ISEQ,1)
            D2FLDSTO(ISEQ,1) = MAX( D2FLDSTO(ISEQ,1), 0. )
            D2FLDFRC(ISEQ,1) = (-D2RIVWTH(ISEQ,1) + DWTHPRE + DWTHNOW ) * (DWTHINC*NLFP)**(-1.)
            D2FLDFRC(ISEQ,1) = MAX( D2FLDFRC(ISEQ,1),0. )
            D2FLDFRC(ISEQ,1) = MIN( D2FLDFRC(ISEQ,1),1. )
            D2FLDARE(ISEQ,1) = D2GRAREA(ISEQ,1)*D2FLDFRC(ISEQ,1)
      !
            D2LEVSTO(ISEQ,1) = 0.  !! no flooding in protected area
            D2LEVDPH(ISEQ,1) = 0.
      
          !**********
          ! [Case-2]  River-side water surface is under levee crown (water only in river side)
          ELSEIF( DSTOALL < D2LEVTOPSTO(ISEQ,1) )THEN 
      
            DSTONOW = DSTOALL - D2LEVBASSTO(ISEQ,1)
            DWTHNOW = D2LEVDST(ISEQ,1) + D2RIVWTH(ISEQ,1)
            D2FLDDPH(ISEQ,1) = D2BASHGT(ISEQ,1) + DSTONOW * DWTHNOW**(-1.) * D2RIVLEN(ISEQ,1)**(-1.)
      
            D2RIVSTO(ISEQ,1) = D2RIVSTOMAX(ISEQ,1) + D2RIVLEN(ISEQ,1) * D2RIVWTH(ISEQ,1) * D2FLDDPH(ISEQ,1)
            D2RIVDPH(ISEQ,1) = D2RIVSTO(ISEQ,1) * D2RIVLEN(ISEQ,1)**(-1.) * D2RIVWTH(ISEQ,1)**(-1.)
        !
            D2FLDSTO(ISEQ,1) = DSTOALL - D2RIVSTO(ISEQ,1)
            D2FLDSTO(ISEQ,1) = MAX( D2FLDSTO(ISEQ,1), 0. )
            D2FLDFRC(ISEQ,1) = D2LEVFRC(ISEQ,1)
            D2FLDARE(ISEQ,1) = D2GRAREA(ISEQ,1)*D2FLDFRC(ISEQ,1)
        ! 
            D2LEVSTO(ISEQ,1) = 0.  !! no flooding in protected area
            D2LEVDPH(ISEQ,1) = 0.
      
          !**********
          ! [Case-3] River side is full, protected side is under levee crown height (Water both in river side & protected side)
          ELSEIF( DSTOALL < D2LEVFILSTO(ISEQ,1) )THEN 
            ! river side stage = levee height
            D2FLDDPH(ISEQ,1) = D2LEVHGT(ISEQ,1)
            D2RIVSTO(ISEQ,1) = D2RIVSTOMAX(ISEQ,1) + D2RIVLEN(ISEQ,1) * D2RIVWTH(ISEQ,1) * D2FLDDPH(ISEQ,1)
            D2RIVDPH(ISEQ,1) = D2RIVSTO(ISEQ,1) * D2RIVLEN(ISEQ,1)**(-1.) * D2RIVWTH(ISEQ,1)**(-1.)
      
            D2FLDSTO(ISEQ,1) = D2LEVTOPSTO(ISEQ,1) - D2RIVSTO(ISEQ,1)
            D2FLDSTO(ISEQ,1) = MAX( D2FLDSTO(ISEQ,1), 0. )
      
            !! protected side storate calculation
            D2LEVSTO(ISEQ,1) = DSTOALL - D2RIVSTO(ISEQ,1) - D2FLDSTO(ISEQ,1)
            D2LEVSTO(ISEQ,1) = MAX( D2LEVSTO(ISEQ,1), 0. )
      
            !!****
            !! protected side stage calculation
            ILEV=INT( D2LEVFRC(ISEQ,1)*NLFP )+1 !! levee relative distance -> floodplain layer with levee
            DSTOPRE = D2LEVTOPSTO(ISEQ,1)
            DWTHPRE = 0.
            DDPHPRE = 0.
            !! which layer current water level is
            I=ILEV
            DO WHILE( I<=NLFP )
              DSTOADD = ( D2LEVDST(ISEQ,1)+D2RIVWTH(ISEQ,1) ) * ( D2LEVHGT(ISEQ,1)-D2FLDHGT(ISEQ,1,I) ) * D2RIVLEN(ISEQ,1) 
              IF( DSTOALL < D2FLDSTOMAX(ISEQ,1,I) + DSTOADD ) EXIT
              DSTOPRE = D2FLDSTOMAX(ISEQ,1,I) + DSTOADD
              DWTHPRE = DWTHINC*I - D2LEVDST(ISEQ,1)
              DDPHPRE = D2FLDHGT(ISEQ,1,I) - D2BASHGT(ISEQ,1)
              I=I+1
              IF( I>NLFP ) EXIT
            END DO
      
            IF( I<=NLFP )THEN
              DSTONOW = DSTOALL - DSTOPRE
              DWTHNOW = -DWTHPRE + ( DWTHPRE**2. + 2. * DSTONOW*D2RIVLEN(ISEQ,1)**(-1.) * D2FLDGRD(ISEQ,1,I)**(-1.) )**0.5
              DDPHNOW = DWTHNOW * D2FLDGRD(ISEQ,1,I)
              D2LEVDPH(ISEQ,1) = D2BASHGT(ISEQ,1) + DDPHPRE + DDPHNOW
      
              D2FLDFRC(ISEQ,1) = ( DWTHPRE + D2LEVDST(ISEQ,1) ) * (DWTHINC*NLFP)**(-1.)
              D2FLDFRC(ISEQ,1) = MAX( D2FLDFRC(ISEQ,1),0.)
              D2FLDFRC(ISEQ,1) = MIN( D2FLDFRC(ISEQ,1),1.)
              D2FLDARE(ISEQ,1) = D2GRAREA(ISEQ,1)*D2FLDFRC(ISEQ,1)
            ELSE
              DSTONOW = DSTOALL - DSTOPRE
              DDPHNOW = DSTONOW * DWTHPRE**(-1.) * D2RIVLEN(ISEQ,1)**(-1.)
              D2LEVDPH(ISEQ,1) = D2BASHGT(ISEQ,1) + DDPHPRE + DDPHNOW
      
              D2FLDFRC(ISEQ,1) = 1.
              D2FLDARE(ISEQ,1) = D2GRAREA(ISEQ,1)*D2FLDFRC(ISEQ,1)
            ENDIF
      
          !**********
          ! [Case-4] Water level above levee crown (Both river side and protected side exceed levee crown height)
          ELSE 
            I=1
            DSTOPRE = D2RIVSTOMAX(ISEQ,1)
            DWTHPRE = D2RIVWTH(ISEQ,1)
            DDPHPRE = 0.
            DO WHILE( DSTOALL > D2FLDSTOMAX(ISEQ,1,I) .AND. I<=NLFP)
              DSTOPRE = D2FLDSTOMAX(ISEQ,1,I)
              DWTHPRE = DWTHPRE + DWTHINC
              DDPHPRE = DDPHPRE + D2FLDGRD(ISEQ,1,I) * DWTHINC
              I=I+1
              IF( I>NLFP ) EXIT
            END DO
      
            IF( I<=NLFP )THEN
              DSTONOW =  DSTOALL - DSTOPRE
              DWTHNOW = -DWTHPRE + ( DWTHPRE**2. + 2. * DSTONOW * D2RIVLEN(ISEQ,1)**(-1.) * D2FLDGRD(ISEQ,1,I)**(-1.) )**0.5
              D2FLDDPH(ISEQ,1) = DDPHPRE + D2FLDGRD(ISEQ,1,I) * DWTHNOW
            ELSE
              DSTONOW = DSTOALL - DSTOPRE
              DWTHNOW = 0.
              D2FLDDPH(ISEQ,1) = DDPHPRE + DSTONOW * DWTHPRE**(-1.) * D2RIVLEN(ISEQ,1)**(-1.)
            ENDIF
      
            D2FLDFRC(ISEQ,1) = (-D2RIVWTH(ISEQ,1) + DWTHPRE + DWTHNOW ) * (DWTHINC*NLFP)**(-1.)
            D2FLDARE(ISEQ,1) = D2GRAREA(ISEQ,1)*D2FLDFRC(ISEQ,1)
      
            !! river channel storage
            D2RIVSTO(ISEQ,1) = D2RIVSTOMAX(ISEQ,1) + D2RIVLEN(ISEQ,1) * D2RIVWTH(ISEQ,1) * D2FLDDPH(ISEQ,1)
            D2RIVDPH(ISEQ,1) = D2RIVSTO(ISEQ,1) * D2RIVLEN(ISEQ,1)**(-1.) * D2RIVWTH(ISEQ,1)**(-1.)
      !
            DSTOADD = ( D2FLDDPH(ISEQ,1)-D2LEVHGT(ISEQ,1) ) * (D2LEVDST(ISEQ,1)+D2RIVWTH(ISEQ,1)) * D2RIVLEN(ISEQ,1)
            D2FLDSTO(ISEQ,1) = D2LEVTOPSTO(ISEQ,1) + DSTOADD - D2RIVSTO(ISEQ,1)
            D2FLDSTO(ISEQ,1) = MAX( D2FLDSTO(ISEQ,1), 0. )
      
            D2LEVSTO(ISEQ,1) = DSTOALL - D2RIVSTO(ISEQ,1) - D2FLDSTO(ISEQ,1)
            D2LEVSTO(ISEQ,1) = MAX( D2LEVSTO(ISEQ,1), 0. )
            D2LEVDPH(ISEQ,1) = D2FLDDPH(ISEQ,1)
          ENDIF
      
        ! [Case-0] Water only in river channel
        ELSE
          D2RIVSTO(ISEQ,1) = DSTOALL
          D2RIVDPH(ISEQ,1) = DSTOALL * D2RIVLEN(ISEQ,1)**(-1.) * D2RIVWTH(ISEQ,1)**(-1.)
          D2RIVDPH(ISEQ,1) = MAX( D2RIVDPH(ISEQ,1), 0. )
          D2FLDSTO(ISEQ,1) = 0.
          D2FLDDPH(ISEQ,1) = 0.
          D2FLDFRC(ISEQ,1) = 0.
          D2FLDARE(ISEQ,1) = 0.
          D2LEVSTO(ISEQ,1) = 0.
          D2LEVDPH(ISEQ,1) = 0.
        ENDIF
        D2SFCELV(ISEQ,1)     = D2RIVELV(ISEQ,1) + D2RIVDPH(ISEQ,1)
      
      END DO
      !$OMP END PARALLEL DO
      
      END SUBROUTINE CALC_FLDSTG_LEVEE
!=====================================
!+
!+
!+
!=====================================
      SUBROUTINE WRITE_OUTVAR

      !! write output variables
      DO JF=1,NVARSOUT
        ID_VAROUT(JF)=200+JF
        SELECT CASE (CVNAMES(JF))
          CASE ('rivsto')
            write(ID_VAROUT(JF),rec=irec) D2RIVSTO
          CASE ('rivdph')
            write(ID_VAROUT(JF),rec=irec) D2RIVDPH
          CASE ('fldsto')
            write(ID_VAROUT(JF),rec=irec) D2FLDSTO
          CASE ('flddph')
            write(ID_VAROUT(JF),rec=irec) D2FLDDPH
          CASE ('fldare')
            write(ID_VAROUT(JF),rec=irec) D2FLDARE
          CASE ('fldfrc')
            write(ID_VAROUT(JF),rec=irec) D2FLDFRC
          CASE ('sfcelv')
            write(ID_VAROUT(JF),rec=irec) D2SFCELV
          CASE ('levsto')
            write(ID_VAROUT(JF),rec=irec) D2LEVSTO
          CASE ('levdph')
            write(ID_VAROUT(JF),rec=irec) D2LEVDPH
        END SELECT
      END DO
      
      END SUBROUTINE WRITE_OUTVAR
!======================================================
      end program conv_storge2fldstg

