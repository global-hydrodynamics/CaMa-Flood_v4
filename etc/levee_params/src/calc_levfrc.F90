      program downscale_flddph
! ===============================================
      implicit none
! CaMa-Flood parameters       
      character*256            ::  param                         !! river map parameters
      integer                  ::  iXX, iYY
      integer                  ::  nXX, nYY                      !! grid number (river network map)
      integer                  ::  nflp                          !! floodplain layers
      real*8                   ::  gsize                         !! grid size [deg]
      real*8                   ::  west, east, north, south      !! domain (river network map)

! High-res map parameters                                        !! (from location.txt)
      character*256            ::  list_loc
      integer                  ::  narea                         !! area ID
      character*256            ::  area                          !! area code
      integer                  ::  ix, iy
      integer                  ::  nx, ny                        !! grid number (hires data)
      real*8                   ::  csize                         !! size of pixel [deg]
      real*8                   ::  lon_ori                       !! west  edge
      real*8                   ::  lon_end                       !! east  edge
      real*8                   ::  lat_ori                       !! north edge
      real*8                   ::  lat_end                       !! south edge

! low res map & data (15min, 6min, etc)
      real,allocatable         ::  uparea(:,:)                   !! drainage area [m2]
      real,allocatable         ::  ctmare(:,:)                   !! catchment area  [m2]
      real,allocatable         ::  rivlen(:,:)                   !! mainstem length [m2]
      real,allocatable         ::  rivwth(:,:)                   !! river width [m]

      real,allocatable         ::  glon(:,:)                     !! catchment outlet lon
      real,allocatable         ::  glat(:,:)                     !! catchment outlet lat


! high-res map & data (default: 1min hires map for global map)
      integer*2,allocatable    ::  catmXX(:,:), catmYY(:,:)      !! catchment (iXX,iYY) of pixel (ix,iy)
      real,allocatable         ::  flddif(:,:)                   !! height above channel [m]
      real,allocatable         ::  area1(:,:)                    !! river width (as permanent water mask)
      real,allocatable         ::  lon(:), lat(:)

! from levee list file
      real                     ::  lat0, lon0, area0, dist0
      integer                  ::  class0                        !! levee pixel type (1:lake, 3-4 River, 5: Ice)

      real                     ::  lsize   !! levee list resolution
      real                     ::  rcell   !! ratio of high-res data and levlist resolution (default global: 1min/3sec=20)

! levee mapping parameter
      real                     ::  thrs_levrat  !! threshold for "mainstem protected rate" to built levee in CaMa-Flood (default 0.2)
      real                     ::  thrs_levnum   !! threshold for "minimum number of levee pixels" to built levee in CaMa-Flood 

! calc
      real,allocatable         ::  mainpx(:,:)   !! mainstem pixel number in each catchment
      real,allocatable         ::  numlev(:,:)   !! number of levee data on mainstem pixel
      real,allocatable         ::  numlev2(:,:)  !! number of levee data on mainstem pixel (lake+ice excluded)
      real,allocatable         ::  levrat(:,:)   !! levee protection ratio of mainstem pixel (if thrs_levrat, assumed to have levee in CaMa-flood) 
      real,allocatable         ::  avedst(:,:)   !! average levee distance

      real,allocatable         ::  levfrc(:,:)   !! levee unprotected fracton (or relative distance from river to levee)
      real,allocatable         ::  levhgt(:,:)   !! levee height (tentatively set to infinite value for initial simulation)
! puxel count
      integer                  ::  gland  !! number of land grid
      integer                  ::  glev   !! number of grid with levee
      integer                  ::  gshort !! number of grid with short levee (excluded)
      integer                  ::  gwide  !! number of grid with wide  levee (larger than cell size)
      integer                  ::  gnarr  !! number of grid with narrow levee (smaller than river width)
!
      character*256            ::  mapdir
      parameter                   (mapdir='./map/')              !! map directory (please make a symbolic link)
      character*256            ::  fuparea, fctmare, frivlen, frivwth, flonlat  !! low res files
      character*256            ::  flevfrc, flevhgt_inf

      character*256            ::  rfile, wfile
      character*256            ::  levlist     !! input levee list file

      character*16             ::  hires       !! resolution of high res data for analysis
      character*16             ::  listres     !! resolution of levee list file
      character*256            ::  buf
      integer                  ::  ios
! ===============================================
      call getarg(1,hires)      !! high-res map dir
      call getarg(2,levlist)    !! levee distance list file
      call getarg(3,listres)    !! levee distance list file
      call getarg(4,frivwth)    !! river width map

! read low res map parameter
      param=trim(mapdir)//'params.txt'
      open(11,file=param,form='formatted')
      read(11,*) nXX
      read(11,*) nYY
      read(11,*) nflp
      read(11,*) gsize
      read(11,*) west
      read(11,*) east
      read(11,*) south
      read(11,*) north
      close(11)

! set high-res pixel size
      if( trim(hires)=='15sec' )then
        csize=1./240.
      elseif( trim(hires)=='30sec' )then
        csize=1./120.
      elseif( trim(hires)=='1min' )then
        csize=1./60.
      else
        stop
      endif

! set levee list resolution
      if( trim(listres)=='3sec' )then
        lsize=1./1200.
      elseif( trim(listres)=='1min' )then
        lsize=1./3600.
      else
        stop
      endif
! 

      rcell=csize/lsize     !! ratio of levee list data resolution to high-res pixel (default global: 1min/3sec = 20)
      print *, "high-res, list-res, rcell=", csize, lsize, rcell

! ==========
! allocate variables
      allocate(uparea(nXX,nYY),ctmare(nXX,nYY),rivlen(nXX,nYY),rivwth(nXX,nYY),glat(nXX,nYY),glon(nXX,nYY))

      fuparea=trim(mapdir)//'uparea.bin'
      fctmare=trim(mapdir)//'ctmare.bin'
      frivlen=trim(mapdir)//'rivlen.bin'
      flonlat=trim(mapdir)//'lonlat.bin'

      open(11, file=fuparea, form='unformatted', access='direct', recl=4*nXX*nYY)
      read(11,rec=1) uparea
      close(11)
      do iYY=1, nYY
        do iXX=1, nXX
          if( uparea(iXX,iYY)>0 )then
            uparea(iXX,iYY)=uparea(iXX,iYY)*1.e-6   !! m2 -> km2
          endif
        end do
      end do

      open(11, file=fctmare, form='unformatted', access='direct', recl=4*nXX*nYY)
      read(11,rec=1) ctmare
      close(11)

      open(11, file=frivlen, form='unformatted', access='direct', recl=4*nXX*nYY)
      read(11,rec=1) rivlen
      close(11)

      open(11, file=frivwth, form='unformatted', access='direct', recl=4*nXX*nYY)
      read(11,rec=1) rivwth
      close(11)

      open(11, file=flonlat, form='unformatted', access='direct', recl=4*nXX*nYY)
      read(11,rec=1) glon
      read(11,rec=2) glat
      close(11)

!===== open hires files
! read list of high-res files (location.txt)
      list_loc=trim(mapdir)//trim(hires)//'/location.txt'
      open(11,file=list_loc,form='formatted')
      read(11,*) narea
      read(11,*)

      read(11,*) buf, area, lon_ori, lon_end, lat_end, lat_ori, nx, ny, csize
      allocate(catmXX(nx,ny),catmYY(nx,ny))
      allocate(flddif(nx,ny),area1(nx,ny))
      allocate(lon(nx),lat(ny))

      print *, 'high-res file list', trim(list_loc)
      print *, lon_ori, lon_end, lat_end, lat_ori, nx, ny, csize

      rfile=trim(mapdir)//trim(hires)//'/'//trim(area)//'.catmxy.bin'
      print *, trim(rfile)
      open(21,file=rfile,form='unformatted',access='direct',recl=2*nx*ny,status='old',iostat=ios)
       if( ios/=0 ) print *, 'no data: ', trim(rfile)
      read(21,rec=1) catmXX
      read(21,rec=2) catmYY
      close(21)

      rfile=trim(mapdir)//trim(hires)//'/'//trim(area)//'.flddif.bin'
      open(21,file=rfile,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
      read(21,rec=1) flddif
      close(21)

      rfile=trim(mapdir)//trim(hires)//'/'//trim(area)//'.uparea.bin'
      open(21,file=rfile,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
      read(21,rec=1) area1
      close(21)
  
      do ix=1, nx
        lon(ix)=lon_ori+(real(ix)-0.5)*csize
        if( lon(ix)>=180. ) lon(ix)=lon(ix)-360.
        if( lon(ix)<-180. ) lon(ix)=lon(ix)+360.
      end do
      do iy=1, ny
        lat(iy) =lat_ori-(real(iy)-0.5)*csize
      end do

! prepare variables
      allocate(mainpx(nXX,nYY),numlev(nXX,nYY),numlev2(nXX,nYY),levrat(nXX,nYY),avedst(nXX,nYY))
      allocate(levfrc(nXX,nYY),levhgt(nXX,nYY))
      mainpx(:,:)  =-9999
      numlev(:,:) =-9999
      numlev2(:,:)=-9999
      levrat(:,:)=-9999
      avedst(:,:)   =-9999

      levfrc(:,:) =-9999
      levhgt(:,:) =-9999

      do iYY=1, nYY
        do iXX=1, nXX
          if( uparea(iXX,iYY)>0 ) then
            mainpx(iXX,iYY)  =0
            numlev(iXX,iYY) =0
            numlev2(iXX,iYY)=0
            levrat(iXX,iYY)=0
            avedst(iXX,iYY)   =0
          endif
        end do
      end do

! define mainstem pixels in each catchment
      do iy=1, ny
        do ix=1, nx
          iXX=catmXX(ix,iy)
          iYY=catmYY(ix,iy)
          if( iXX>0 .and. iYY>0 )then
            if( area1(ix,iy)>0.5*uparea(iXX,iYY) )then    !! define mainstem as "pixels with uparea > 0.5 of outlet value"
              mainpx(iXX,iYY)=mainpx(iXX,iYY)+1
            endif
          endif
        end do
      end do

!! ===== read levee list file (text, csv)

      print *, 'READ: ', trim(levlist)
      open(31,file=levlist,form='formatted')
 1000 continue
      read(31,*,end=1090) lat0, lon0, area0, dist0, class0   !! lat, lon, drainage area (km2), distance to levee, water class (lake/river/ice)
      ix=int( (lon0-lon_ori)/csize )+1  !! calculate corresponding high-res pixel from lat,lon
      iy=int( (lat_ori-lat0)/csize )+1
      if( ix>0 .and. ix<=nx .and. iy>0 .and. iy<=ny )then
        iXX=catmXX(ix,iy)               !! get corresponding unit-catchment
        iYY=catmYY(ix,iy)
        if( iXX>0 .and. iYY>0 )then
          if( area1(ix,iy)>0.5*uparea(iXX,iYY) )then   !! if levee piel is on mainstem
            numlev(iXX,iYY)=numlev(iXX,iYY)+1          !! mainpx number of levee pixels on mainstem
            if( class0==0 .or. class0==3 .or. class0==4 )then
              numlev2(iXX,iYY)=numlev2(iXX,iYY)+1      !! mainpx number of levee pixels (exluce lake and ice)
              avedst(iXX,iYY)   =avedst(iXX,iYY)   +dist0  
            endif
          endif
        endif
      endif
      goto 1000
 1090 continue
      close(31)

! ========
      print *, 'calculate levee statistics in unit-catchment' 
      do iYY=1, nYY
        do iXX=1, nXX
          if( uparea(iXX,iYY)>0 )then
            !! calculate ratio of "pixels with levee" in "mainstem pixels"
            !! note: levee list is on 3sec pixel while "mainpx" is for 1min pixel (global case). Thus, adjustment by "rcell" is needed.
            levrat(iXX,iYY)=numlev2(iXX,iYY) / (rcell*mainpx(iXX,iYY))
            levrat(iXX,iYY)=min(1.0,levrat(iXX,iYY))

            !! if levee pixel exist:
            if( numlev2(iXX,iYY)>0 )then
              avedst(iXX,iYY)=avedst(iXX,iYY)/numlev2(iXX,iYY)          !! calculate avedst levee distance
              avedst(iXX,iYY)=max(avedst(iXX,iYY),rivwth(iXX,iYY)*0.5)  !! levee distance should not be smaller than half of river width
            endif
          endif
        end do
      end do

      print *, 'calculate levee unprotected fraction'
      thrs_levrat=0.2                           !! minimum levee-protected mainstem rate     (default 20% of mainstem)
      thrs_levnum =min(10.,gsize/lsize*thrs_levrat)   !! minimum number of levee data in catchment
      print *, '-- threshold for minimum levee-protected mainstem rate',     thrs_levrat
      print *, '-- threshold for minimum number of levee data in catchment', thrs_levnum
      do iYY=1, nYY
        do iXX=1, nXX
          if( uparea(iXX,iYY)>0 )then
            levfrc(iXX,iYY)=1.0       !! default fraction is 1.0 (all catchment is not protected)

            !! judge whether building levee in CaMa-Flood
            if( levrat(iXX,iYY)>thrs_levrat .and. numlev2(iXX,iYY)>thrs_levnum )then
              levfrc(iXX,iYY)=avedst(iXX,iYY)*2. / ( ctmare(iXX,iYY)/rivlen(iXX,iYY) )
              levfrc(iXX,iYY)=min(1.0,levfrc(iXX,iYY)) !! modify wider than catchment width
            else
              levfrc(iXX,iYY)=-9999  !! no levee catchment
            endif
          endif
        end do
      end do

      levhgt(:,:)=-9999
      do iYY=1, nYY
        do iXX=1, nXX
          if( uparea(iXX,iYY)>0 )then
            levhgt(iXX,iYY)=0.0
            if( levfrc(iXX,iYY)>0 )then !! levee exist
              levhgt(iXX,iYY)=100.0       !! assume infinite levee height
            endif
          endif
        end do
      end do

      !! save tentative data for quality check
      open(41, file='./tmp/tmp.bin', form='unformatted', access='direct', recl=4*nXX*nYY)
      write(41,rec=1) mainpx
      write(41,rec=2) numlev
      write(41,rec=3) numlev2
      write(41,rec=4) levrat
      write(41,rec=5) avedst
      write(41,rec=6) levfrc
      close(41)

      flevfrc=trim(mapdir)//'levfrc.bin'
      print *, 'SAVE levee fraction: ', trim(flevfrc)
      open(42, file=flevfrc, form='unformatted', access='direct', recl=4*nXX*nYY)
      write(42,rec=1) levfrc
      close(42)

      flevhgt_inf=trim(mapdir)//'levhgt_sim00.bin'
      print *, 'SAVE levee infinite height: ', trim(flevhgt_inf)
      open(43, file=flevhgt_inf, form='unformatted', access='direct', recl=4*nXX*nYY)
      write(43,rec=1) levhgt
      close(43)

! =============

      print *, 'statistics'
      gland =0
      glev  =0
      gshort=0
      gwide =0
      gnarr =0

      do iYY=1, nYY
        do iXX=1, nXX
          if( uparea(iXX,iYY)>0 )then
            gland=gland+1
            if( levrat(iXX,iYY)>thrs_levrat .and. numlev2(iXX,iYY)>thrs_levnum )then
              glev=glev+1
              if( avedst(iXX,iYY)*2. > (ctmare(iXX,iYY)/rivlen(iXX,iYY)) )then  !! wider than catchment width
                gwide=gwide+1
              endif
              if( avedst(iXX,iYY)==rivwth(iXX,iYY)*0.5) then
                gnarr=gnarr+1
              endif
            else
              if( numlev2(iXX,iYY)>0 )then  !! levee exist but short
                gshort=gshort+1
              endif
            endif
          endif
        end do
      end do

      print *, '-- gland ',  gland 
      print *, '-- glev  ',  glev  
      print *, '-- gshort',  gshort
      print *, '-- gwide ',  gwide 
      print *, '-- gnarr ',  gnarr 

      wfile='./tmp/levfrc_stat.txt'
      open(51,file=wfile,form='formatted')
      do iYY=1, nYY
        do iXX=1, nXX
          if( uparea(iXX,iYY)>0 .and. numlev2(iXX,iYY)>0 )then
            write(51,'(5f15.3)') glat(iXX,iYY), glon(iXX,iYY), uparea(iXX,iYY), levfrc(iXX,iYY), levrat(iXX,iYY)
          endif
        end do
      end do
      close(51)

      end program downscale_flddph


