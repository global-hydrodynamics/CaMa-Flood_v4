! Shared v5 map-tool copy of map/src/src_param/set_bifparam.F90.
! Filter original bifurcation paths to the requested number of layers, estimate
! channel depth, and write bifprm.csv. The independent converter creates
! bifprm.nc from that same finalized table. With optional "check_map", also write
! the two-record
! check-only raster check_bif_dphwth.bin (depth, then width). This program does
! not modify basin.bin.
      program param_bif_set_params
! ================================================
! original file: bifori.txt
! Bifurcation-channel depth (BIFDPH) is estimated from the river network.
! (if needed, number of bifurcation layers can be modified.)
! The source table can contain up to 10 width layers; the workflow may retain fewer.
! ================================================
      use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
      implicit none
! ===================
! Command-line options and input mode
      character*256            :: buf
      character*256            :: diminfo  !! input-dimension descriptor filename
      data                    diminfo /'./inpdim_test-glb_1deg.txt'/

! River-network map parameters
      integer                  :: ix, iy  !! x/y grid index
      integer                  :: jx, jy  !! target x/y grid index
      integer                  :: nx, ny  !! x/y grid dimensions

! River-network maps
      integer, allocatable      :: nextx(:,:)  !! downstream x grid index
      integer, allocatable      :: nexty(:,:)  !! downstream y grid index
      real                     :: west, east, north, south  !! map bounds [deg]

! Working arrays
      integer, allocatable      :: basin(:,:)  !! basin identifier
      real, allocatable         :: map_lon(:,:)  !! unit-catchment outlet longitude [degrees_east]
      real, allocatable         :: map_lat(:,:)  !! unit-catchment outlet latitude [degrees_north]

      real, allocatable         :: rivhgt(:,:)  !! channel depth/height [m]
      real, allocatable         :: bifdph(:,:)  !! bifurcation depth [m]
      real, allocatable         :: bifwth(:,:)  !! bifurcation width [m]

! Input and output filenames
      character*256            :: cnextxy
      character*256            :: crivhgt
      character*256            :: cbifway
      character*256            :: cbifprm
      character*256            :: cbasin

! Input status and optional diagnostic-output control.
      integer                  :: ios  !! I/O status code
      logical                  :: write_check_map

! Bifurcation-table dimensions and path counters.
      integer                  :: ipth  !! bifurcation-path index
      integer                  :: npth  !! bifurcation-path count
      integer                  :: npth_new  !! number of paths retained after the layer/width filter
      integer                  :: pth_id  !! row index in the finalized CSV
      integer                  :: ilev  !! bifurcation-layer index
      integer                  :: nlev  !! layer count
      integer                  :: nlev_new  !! requested/retained bifurcation width-layer count

! Physical attributes read from each bifurcation-table row.
      real                     :: len  !! pathway length [m]
      real                     :: elv  !! pathway elevation [m]
      real                     :: dph  !! bifurcation depth [m]
      real                     :: lon, lat  !! longitude/latitude [deg]
      real, allocatable         :: wth(:)  !! bifurcation width [m]

! Pathway filter and basin identifiers.
      real                     :: dph0  !! maximum adjacent channel depth used to cap bifurcation depth [m]
      integer                  :: isData  !! 1 when a retained path has any positive width
      integer                  :: ibsn  !! basin identifier
      integer                  :: jbsn  !! basin identifier
!
! Missing-value sentinels
      integer                  :: imis  !! integer undefined value
      real                     :: rmis  !! real    undefined value
      parameter              (imis = -9999)
      parameter              (rmis = 1.e+20)

! ================================================
print *, 'set_bifparam: read parameters from arguments'

! Arguments: river-depth binary, output parameter-table filename, number of
! bifurcation layers to retain, optional dimension-info text filename, and an
! optional "check_map" token to request the diagnostic map output.
! The requested layer count must be 1..10. If it exceeds the layer count in
! bifori.txt, it is clamped to the available count below. Path endpoint checks
! remain limited to the existing algorithm.
      call getarg(1,crivhgt)
      call getarg(2,cbifprm)
      call getarg(3,buf)
       read(buf,*,iostat=ios) nlev_new
       if( ios/=0 )then
         print *, 'ERROR: BIFLAYER must be an integer from 1 to 10'
         stop 2
       endif
       if( nlev_new<1 .or. nlev_new>10 )then
         print *, 'ERROR: BIFLAYER must be an integer from 1 to 10'
         stop 2
       endif
      call getarg(4,buf)
       if( buf/='' ) read(buf,'(a128)') diminfo
      write_check_map=.false.
      call getarg(5,buf)
      if( trim(adjustl(buf))=='check_map' )then
        write_check_map=.true.
      elseif( len_trim(buf)>0 )then
        print *, 'ERROR: optional fifth argument must be check_map'
        stop 1
      endif

! ================================================
! [1] Read map configuration and river-network inputs
! ================================================
! INPDIM supplies the base-grid dimensions and bounds; all subsequent direct
! binary records are sized as NX*NY four-byte values.
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
      allocate(map_lon(nx,ny),map_lat(nx,ny))

! ===================
print *, 'set_bifparam: read nextxy.bin'
! nextxy.bin contains two direct-access records: downstream X, then downstream Y.
      cnextxy='./nextxy.bin'
      open(11,file=cnextxy,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
      read(11,rec=1) nextx
      read(11,rec=2) nexty
      close(11)

! Read the channel depth and basin ID at each endpoint of a bifurcation path.
! The depth map limits the estimated bifurcation-channel depth below.
      open(13,file=crivhgt,form='unformatted',access='direct',recl=4*nx*ny)
      read(13,rec=1) rivhgt
      close(13)

      cbasin='./basin.bin'
      open(11,file=cbasin,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
      read(11,rec=1) basin
      close(11)      

! lonlat.bin contains unit-catchment outlet longitude in record 1 and latitude
! in record 2. Use those outlet coordinates rather than regular cell centers.
      open(11,file='./lonlat.bin',form='unformatted',access='direct', &
           recl=4*nx*ny,status='old',iostat=ios)
      if( ios/=0 )then
        print *, 'ERROR: cannot open lonlat.bin'
        stop 2
      endif
      read(11,rec=1,iostat=ios) map_lon
      if( ios/=0 )then
        print *, 'ERROR: cannot read longitude record from lonlat.bin'
        stop 2
      endif
      read(11,rec=2,iostat=ios) map_lat
      if( ios/=0 )then
        print *, 'ERROR: cannot read latitude record from lonlat.bin'
        stop 2
      endif
      close(11)

! =============================
print *, 'set_bifparam: read bifori.txt'

! Initialize diagnostic rasters to the legacy missing value. Cells not used by
! a retained river bifurcation remain missing in both records.
      bifdph(:,:)=-9999
      bifwth(:,:)=-9999

      cbifway='./bifori.txt'
      open(12,file=cbifway,form='formatted')
      read(12,*) npth, nlev

      print *, 'npth_ori, nlev_ori:', npth, nlev

! A request for more layers than bifori.txt contains cannot be honored. Warn
! and use all available layers so subsequent width-array accesses stay in range.
      if( nlev_new>nlev )then
        print *, 'WARNING: requested BIFLAYER exceeds bifori.txt layer count; using available layers:', nlev
        nlev_new=nlev
      endif

! ================================================
! [2] Read and filter the bifurcation pathway table
! ================================================
! bifori.txt starts with path count and layer count. Each path row stores its
! endpoint indices, length/elevation, widths by layer, and latitude/longitude.
      allocate(wth(nlev))
      npth_new=0
      do ipth=1, npth
        read(12,*) ix, iy, jx, jy, len, elv, (wth(ilev),ilev=1,nlev), lat, lon

!************************************
        !! Keep a path only if at least one requested-layer width is positive.
        !! The first pass counts rows so CSV generation follows the existing filter.
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


! Write a named CSV in the existing path order. Assign pth_id only after the
! BIFLAYER filter has selected each final data row.
      open(21,file=cbifprm,form='formatted')
      write(21,'(a)',advance='no') 'pth_id,ix,iy,jx,jy,length,elevtn,depth'
      do ilev=1,nlev_new
        write(21,'(a)',advance='no') ',width_'
        write(21,'(i0)',advance='no') ilev
      enddo
      write(21,'(a)') ',start_lat,start_lon,end_lat,end_lon,ibsn,jbsn'

      cbifway='./bifori.txt'
      open(12,file=cbifway,form='formatted')
      read(12,*) npth, nlev

      pth_id=0
      do ipth=1, npth
        read(12,*) ix, iy, jx, jy, len, elv, (wth(ilev),ilev=1,nlev), lat, lon

!************************************
        !! Apply the same requested-layer filter as in the counting pass.
        isData=0
        do ilev=1, nlev_new
          if( wth(ilev)>0 ) isData=1
        end do
!************************************


        if( isData==1 )then
          if( min(ix,iy,jx,jy)<1 .or. ix>nx .or. jx>nx .or. iy>ny .or. jy>ny )then
            print *, 'ERROR: retained bifurcation path has endpoint outside map dimensions:', ix,iy,jx,jy
            stop 2
          endif
          if( .not.ieee_is_finite(map_lon(jx,jy)) .or. .not.ieee_is_finite(map_lat(jx,jy)) .or. &
              map_lon(jx,jy)<-9990.0 .or. map_lat(jx,jy)<-9990.0 .or. &
              abs(map_lon(jx,jy))>360.0 .or. abs(map_lat(jx,jy))>90.0 )then
            print *, 'ERROR: retained bifurcation path has missing/invalid outlet coordinates:', jx,jy
            stop 2
          endif
          pth_id=pth_id+1
          if( wth(1)<=0 )then
            dph=-9999
            wth(1)=0.0
          else                           !! if channel connectivity exist, set depth following bifurcation channel width.
! ================================================
! [3] Estimate pathway depths and write the bifprm.csv row
! ================================================
! Estimate river-channel bifurcation depth from layer-1 width, apply the
! 0.5 m lower bound, and cap it at the deeper endpoint's existing channel depth.
            dph=log10(wth(1))*2.5-4.0
            dph=max(0.5,dph)
            dph0=max(rivhgt(ix,iy),rivhgt(jx,jy))  !! modify is nearby channel depth is bigger
            dph=min(dph,dph0)
  
            bifdph(ix,iy)=dph
            bifwth(ix,iy)=wth(1)
          endif
  
! Store the two endpoint basin IDs in ascending order in the text table.
          ibsn=basin(ix,iy)
          jbsn=basin(jx,jy)
          if( ibsn > jbsn ) then
            jbsn=basin(ix,iy)
            ibsn=basin(jx,jy)
          endif

          write(21,'(i0,4(",",i0))',advance='no') pth_id,ix,iy,jx,jy
          write(21,'(",",f0.2)',advance='no') len
          write(21,'(",",f0.2)',advance='no') elv
          write(21,'(",",f0.2)',advance='no') dph
          do ilev=1,nlev_new
            write(21,'(",",f0.2)',advance='no') wth(ilev)
          enddo
          write(21,'(",",f0.3)',advance='no') lat
          write(21,'(",",f0.3)',advance='no') lon
          write(21,'(",",es24.16e3)',advance='no') map_lat(jx,jy)
          write(21,'(",",es24.16e3)',advance='no') map_lon(jx,jy)
          write(21,'(",",i0,",",i0)') ibsn,jbsn
        endif
      end do
      close(12)
      close(21)

! ------
! ================================================
! [4] Write the optional inspection raster
! ================================================
! The optional check map has two direct-access float32 records: bifurcation
! depth first and width second. It is for inspection, not simulation input.
      if( write_check_map )then
        print *, 'set_bifparam: write check-only map check_bif_dphwth.bin'
        open(13,file='./check_bif_dphwth.bin',form='unformatted', &
             access='direct',recl=4*nx*ny)
        write(13,rec=1) bifdph
        write(13,rec=2) bifwth
        close(13)
      else
        print *, 'set_bifparam: no check map requested; diagnostic output skipped'
      endif


! === Finish =====================================



!!================================================

      end program param_bif_set_params
