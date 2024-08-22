      program set_bifparam
! ================================================
! original file: bifori.txt
! bifucation parameter (bifurcation channle depth = bifdph) is added by analyzing river network
! (if needed, number of bifurcation layers can be modified.)
! -- 10 layer exist in original. Reduced to 5 by s01-*sh script to reduce computation cost
! ================================================
#ifdef UseCDF_CMF
USE NETCDF
#endif
      implicit none
! ===================
! calculation type
      character*256       ::  buf
      character*256       ::  diminfo                 !! dimention info file
      data                    diminfo /'./diminfo_test-1deg.txt'/


! river network map parameters
      integer             ::  ix, iy, jx, jy
      integer             ::  nx, ny          !! river map grid number
! river netwrok map
      integer,allocatable ::  nextx(:,:)      !! downstream x
      integer,allocatable ::  nexty(:,:)      !! downstream y
      real                ::  west, east, north, south
! variable
      integer,allocatable ::  basin(:,:)      !! 

      real,allocatable    ::  rivhgt(:,:)
      real,allocatable    ::  bifdph(:,:), bifwth(:,:)     !! channel depth / width [m]
! file
      character*256       ::  cnextxy, crivhgt, cbifway, cbifprm, cbifdph, cbasin
      integer             ::  ios

      integer             ::  ipth, npth, npth_new
      integer             ::  ilev, nlev, nlev_new

      real                ::  len, elv, dph, lon, lat
      real,allocatable    ::  wth(:)

      real                ::  dph0
      integer             ::  isData, ibsn, jbsn
!
! Undefined Values
      integer             ::  imis                !! integer undefined value
      real                ::  rmis                !! real    undefined value
      parameter              (imis = -9999)
      parameter              (rmis = 1.e+20)

      character*256          ::  cfmt, clen
! ================================================
print *, 'set_bifparam: read parameters from arguments'
      call getarg(1,crivhgt)
      call getarg(2,cbifprm)
      call getarg(3,buf)
       read(buf,*) nlev_new
      call getarg(4,buf)
       if( buf/='' ) read(buf,'(a128)') diminfo

      open(11,file=diminfo,form='formatted')
      read(11,*) nx
      read(11,*) ny
      read(11,*) 
      read(11,*) 
      read(11,*) 
      read(11,*) 
      read(11,*) 
      read(11,*) west
      read(11,*) east
      read(11,*) north
      read(11,*) south
      close(11)

! ==============================

      print *, nx, ny

      allocate(nextx(nx,ny),nexty(nx,ny))
      allocate(rivhgt(nx,ny),bifdph(nx,ny),bifwth(nx,ny))
      allocate(basin(nx,ny))

! ===================
print *, 'set_bifparam: read nextxy.bin'
      cnextxy='./nextxy.bin'
      open(11,file=cnextxy,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
      read(11,rec=1) nextx
      read(11,rec=2) nexty
      close(11)

      open(13,file=crivhgt,form='unformatted',access='direct',recl=4*nx*ny)
      read(13,rec=1) rivhgt
      close(13)

      cbasin='./basin.bin'
      open(11,file=cbasin,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
      read(11,rec=1) basin
      close(11)      

! =============================
print *, 'set_bifparam: read bifori.txt'

      bifdph(:,:)=-9999
      bifwth(:,:)=-9999

      cbifway='./bifori.txt'
      open(12,file=cbifway,form='formatted')
      read(12,*) npth, nlev

      print *, 'npth_ori, nlev_ori:', npth, nlev

      allocate(wth(nlev))
      npth_new=0
      do ipth=1, npth
        read(12,*) ix, iy, jx, jy, len, elv, (wth(ilev),ilev=1,nlev), lat, lon

!************************************
        !! if no bifurcation below nlev_new, excliude. (original: 1 water + 10 overland layers)
        isData=0
        do ilev=1, nlev_new
          if( wth(ilev)>0 ) isData=1
        end do
!************************************

        if( isData==1 )then
          npth_new=npth_new+1
        endif


      end do
      close(12)
      print *, 'npth_new, nlev_new:', npth_new, nlev_new

! ===============================


      open(21,file=cbifprm,form='formatted')
      write(21,'(2i8,a)') npth_new, nlev_new, &
      '  npath_new, nlev_new, (ix,iy), (jx,jy), length, elevtn, depth, (width1, width2, ... wodth_nlev), (lat,lon), (basins)'

      cbifway='./bifori.txt'
      open(12,file=cbifway,form='formatted')
      read(12,*) npth, nlev

      do ipth=1, npth
        read(12,*) ix, iy, jx, jy, len, elv, (wth(ilev),ilev=1,nlev), lat, lon

!************************************
        !! if no bifurcation below nlev_new, excliude. (original: 1 water + 10 overland layers)
        isData=0
        do ilev=1, nlev_new
          if( wth(ilev)>0 ) isData=1
        end do
!************************************


        if( isData==1 )then
          if( wth(1)<=0 )then
            dph=-9999
            wth(1)=0.0
          else                           !! if channel connectivity exist, set depth following bifurcation channel width.
            dph=log10(wth(1))*2.5-4.0
            dph=max(0.5,dph)
            dph0=max(rivhgt(ix,iy),rivhgt(jx,jy))  !! modify is nearby channel depth is bigger
            dph=min(dph,dph0)
  
            bifdph(ix,iy)=dph
            bifwth(ix,iy)=wth(1)
          endif
  
          ibsn=basin(ix,iy)
          jbsn=basin(jx,jy)
          if( ibsn > jbsn ) then
            jbsn=basin(ix,iy)
            ibsn=basin(jx,jy)
          endif

          write(clen,'(i2)') 3+nlev_new
          cfmt='(4i8,'//trim(clen)//'f12.2,2f10.3,2i8)'
          write(21,cfmt) ix, iy, jx, jy, len, elv, dph, &
                            (wth(ilev),ilev=1,nlev_new), lat, lon, ibsn, jbsn
        endif
      end do
      close(12)
      close(21)

! ------
! 'set_bifparam: save 2d map of bifucation depth and width (just for checking, not used in simulation)'
      cbifdph='./bifdph.bin'
      open(13,file=cbifdph,form='unformatted',access='direct',recl=4*nx*ny)
      write(13,rec=1) bifdph
      write(13,rec=2) bifwth
      close(13)


! === Finish =====================================



#ifdef UseCDF_CMF
      CONTAINS
!!================================================
      SUBROUTINE NCERROR(STATUS,STRING)
      USE NETCDF
      IMPLICIT NONE
      INTEGER,INTENT(IN) :: STATUS
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: STRING

! ======
      IF ( STATUS /= 0 ) THEN
        PRINT*, TRIM(NF90_STRERROR(STATUS))
        IF( PRESENT(STRING) ) PRINT*,TRIM(STRING)
        PRINT*,'PROGRAM STOP ! '
        STOP 10
      ENDIF
      END SUBROUTINE NCERROR
#endif
!!================================================

      end program set_bifparam
