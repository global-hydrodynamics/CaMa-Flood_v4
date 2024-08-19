      program allocate_gauge
! ===============================================
! to allocate river flow gauge on CaMa-Flood river map.
!   input: gauge list, allocated on MERIT Hydro or J-FlwDir.
!          1st-4th column should be (ID, Lat, Lon, Uparea)
! ===============================================
      implicit none
! index TRIP
      integer              ::  iXX, iYY, jXX, jYY
      integer              ::  nXX, nYY                        !! x-y matrix GLOBAL
      real                 ::  gsize, csize                    !! grid size [degree]
      real                 ::  west,  north,  east,  south
      real                 ::  west2, north2, east2, south2
      character*16         ::  buf
      character*16         ::  hires                           !! hires map directory (default: 1min global / 15sec Japan)
      integer              ::  ios, inum
! index 1min
      integer              ::  ix, iy, jx, jy, kx, ky, dx, dy
      integer              ::  nx, ny                          !! high-resolution map coordinate

      integer              ::  ix0, iy0                        !! allocated station xy
      integer              ::  jx0, jy0                        !! catchment outlet of the allocated gauge
! input
      real,allocatable     ::  uparea(:,:)                    !! drainage area (GRID base)
      real,allocatable     ::  elevtn(:,:)                    !! elevation
      integer,allocatable  ::  nextXX(:,:), nextYY(:,:)       !! next grid X

      real,allocatable     ::  maxupa(:,:)                    !! maximum drainage area of upstream grids 
      integer,allocatable  ::  upstXX(:,:,:), upstYY(:,:,:)   !! list of upstream grid
      integer,allocatable  ::  outx(:,:), outy(:,:)           !! high-res (ix,iy) coordinate of outlet pixel

      real,allocatable     ::  glon(:), glat(:)

      integer*2,allocatable::  dwx1m(:,:), dwy1m(:,:)         !! downstream pixel (jx,jy) of each high-res pixel (ix,iy) 
      integer*2,allocatable::  ctx1m(:,:), cty1m(:,:)         !! corresponding unit-catchment (iXX,iYY) of each high-res pixel
      real,allocatable     ::  upa1m(:,:)                     !! high-res upstream area 
      real,allocatable     ::  elv1m(:,:)                     !! high-res elevtn
      real,allocatable     ::  lon1m(:), lat1m(:)             !! high-res pixel lat lon

      integer*8            ::  id
      real                 ::  lat0, lon0, lat, lon, area0, area
      real                 ::  err, err0, err1, err2, dd
      real                 ::  rate, rate0

      integer              ::  type         !! 1: allocated on the catchment mainstem. 2: on catchment tributary, 3: small stream
 
      real                 ::  dst
      real                 ::  dst_outlt                      !! distance to outlet pixel [km]
      real                 ::  dst_upst                       !! distance from upstream outlet pixel [km]

      real                 ::  elv_outlt                      !! elevation of outlet pixel [m]
      real                 ::  elv_gauge                      !! elevation of gauge  pixel [m]
      real                 ::  elv_upst                       !! elevation of upstream outlet pixel [m]

      integer              ::  nn

      integer              ::  i_ups, j_ups, n_ups            !! upstream grid 

      integer              ::  iXX0, iYY0, jXX0, jYY0
! file
      character*128        ::  fparam
      character*128        ::  rfile1, gaugelist, hilist
      character*128        ::  wfile1
! ===============================================
      print *, 'Allocate gauge on CaMa-Flood river network'

      print *, 'USAGE'
      print *, '% ./allocate_lelve_gauge GaugeListFile'
      call getarg(1,gaugelist)

      print *, 'Check Hires Map'
      hires='1min'
      hilist='../'//trim(hires)//'/location.txt'
      open(11, file=hilist, form='formatted', status='old',iostat=ios)
      if( ios==0 ) then
        read(11,*) inum
        close(11)
        if( inum>1 ) ios=1
      endif

      if( ios/=0 )then  !! 1min map not found (case of Japan)
        hires='15sec'
        hilist='../'//trim(hires)//'/location.txt'
        open(11, file=hilist, form='formatted', status='old',iostat=ios)
        if( ios==0 ) then
          read(11,*) inum
          close(11)
          if( inum>1 ) ios=1
        endif
      endif

      if( ios/=0 )then
        print *, '  ERROR no high resolution data available'
        stop
      else
        print *, '  hires map= ', trim(hires)
      endif

      !! read CaMa-Flood map parameter
      fparam='../params.txt'
      open(11,file=fparam,form='formatted')
      read(11,*) nXX
      read(11,*) nYY
      read(11,*) 
      read(11,*) gsize
      read(11,*) west
      read(11,*) east
      read(11,*) south
      read(11,*) north
      close(11)

! ==========
      !! read CaMa-Flood map files
      allocate(uparea(nXX,nYY),elevtn(nXX,nYY))
      allocate(nextXX(nXX,nYY),nextYY(nXX,nYY))

      rfile1='../uparea.bin'
      open(11, file=rfile1, form='unformatted', access='direct', recl=4*nXX*nYY,status='old',iostat=ios)
      read(11,rec=1) uparea
      close(11)

      rfile1='../elevtn.bin'
      open(11, file=rfile1, form='unformatted', access='direct', recl=4*nXX*nYY,status='old',iostat=ios)
      read(11,rec=1) elevtn
      close(11)

      rfile1='../nextxy.bin'
      open(11, file=rfile1, form='unformatted', access='direct', recl=4*nXX*nYY,status='old',iostat=ios)
      read(11,rec=1) nextXX
      read(11,rec=2) nextYY
      close(11)

      do iYY=1, nYY
        do iXX=1, nXX
          if( uparea(iXX,iYY)>0 ) uparea(iXX,iYY)=uparea(iXX,iYY)*1.e-6   !! convert m2 --> km2
        end do
      end do

      allocate(glon(nXX),glat(nYY))
      do iYY=1, nYY
        glat(iYY)= north-gsize*(iYY-0.5)
      end do
      do iXX=1, nXX
        glon(iXX)=  west+gsize*(iXX-0.5)
      end do

! =====================
      !! read high-res map data
      hilist='../'//trim(hires)//'/location.txt'
      open(11, file=hilist, form='formatted')
      read(11,*)
      read(11,*)
      read(11,*) buf, buf, west2, east2, south2, north2, nx, ny
      close(11)
      csize=dble(east2-west2)/dble(nx)

      print *, ' nx,ny,csize=', nx, ny, csize

      allocate(upa1m(nx,ny),ctx1m(nx,ny),cty1m(nx,ny),dwx1m(nx,ny),dwy1m(nx,ny),elv1m(nx,ny))

      rfile1='../'//trim(hires)//'/'//trim(hires)//'.uparea.bin'
      open(11, file=rfile1, form='unformatted', access='direct', recl=4*nx*ny,status='old',iostat=ios)
      read(11,rec=1) upa1m
      close(11)

      rfile1='../'//trim(hires)//'/'//trim(hires)//'.catmxy.bin'
      open(11, file=rfile1, form='unformatted', access='direct', recl=2*nx*ny,status='old',iostat=ios)
      read(11,rec=1) ctx1m
      read(11,rec=2) cty1m
      close(11)

      rfile1='../'//trim(hires)//'/'//trim(hires)//'.downxy.bin'
      open(11, file=rfile1, form='unformatted', access='direct', recl=2*nx*ny,status='old',iostat=ios)
      read(11,rec=1) dwx1m
      read(11,rec=2) dwy1m
      close(11)

      rfile1='../'//trim(hires)//'/'//trim(hires)//'.elevtn.bin'
      open(11, file=rfile1, form='unformatted', access='direct', recl=4*nx*ny,status='old',iostat=ios)
      read(11,rec=1) elv1m
      close(11)

      allocate(lon1m(nx),lat1m(ny))
      do iy=1, ny
        lat1m(iy)= north2-(iy-0.5)*csize
      end do
      do ix=1, nx
        lon1m(ix)=  west2+(ix-0.5)*csize
      end do

! ===============================================
print *, 'check upstream grid'

      n_ups=8  !! number of upstream grid considered
      allocate(upstXX(nXX,nYY,n_ups),upstYY(nXX,nYY,n_ups),maxupa(nXX,nYY))

      upstXX(:,:,:)=-9999
      upstYY(:,:,:)=-9999
      do i_ups=1, n_ups
        maxupa(:,:)=0
        do iYY=1, nYY
          do iXX=1, nXX
            if( nextXX(iXX,iYY)>0 ) then
              jXX=nextXX(iXX,iYY)
              jYY=nextYY(iXX,iYY)
              if( i_ups>=2 )then
                do j_ups=1, i_ups-1
                  if( iXX==upstXX(jXX,jYY,j_ups).and.iYY==upstYY(jXX,jYY,j_ups) )then
                    goto 2000    !! already registered, skip
                  endif
                end do
              endif
              if( uparea(iXX,iYY)>maxupa(jXX,jYY) )then  !! register grid from larger uparea
                maxupa(jXX,jYY)=uparea(iXX,iYY)
                upstXX(jXX,jYY,i_ups)=iXX
                upstYY(jXX,jYY,i_ups)=iYY
              endif
            endif
   2000     continue
          end do
        end do
      end do

!===
print *, 'calc outlet pixel location of each unit catchment'
      allocate(outx(nXX,nYY),outy(nXX,nYY))
      outx(:,:)=-9999
      outy(:,:)=-9999

      maxupa(:,:)=0
      do iy=1, ny
        do ix=1, nx
          if( ctx1m(ix,iy)>0 )then
            iXX=ctx1m(ix,iy)
            iYY=cty1m(ix,iy)
            if( dwx1m(ix,iy)<=-900 )then !! river mouth
              outx(iXX,iYY)=ix
              outy(iXX,iYY)=iy
            else
              call nextxy(ix,iy,jx,jy)
              if( jx>=1 .and. jx<=nx .and. jy>=1 .and. jy<=ny )then
                if( ctx1m(jx,jy)/=ctx1m(ix,iy) .or. cty1m(jx,jy)/=cty1m(ix,iy) )then
                  if( outx(iXX,iYY)/=-9999 )then
                    !!print *, 'Multiple Outlet', iXX,iYY
                    if( upa1m(jx,jy)>maxupa(iXX,iYY) )then    !! mid resolution river map might have two outlet, due to resampling limitation
                      maxupa(iXX,iYY)=upa1m(jx,jy)
                      outx(iXX,iYY)=ix
                      outy(iXX,iYY)=iy
                    endif
                  else
                    outx(iXX,iYY)=ix
                    outy(iXX,iYY)=iy
                  endif
                endif
              endif
            endif
          endif
        end do
      end do

! ===============================================
      open(11, file=gaugelist, form='formatted')
      read(11,*)

      wfile1='./gauge_alloc.txt'
      open(21, file=wfile1, form='formatted')
      write(21,'(a,a)') '        ID       lat       lon        area    Type     ix1   iy1     ix2   iy2',&
                        '  elv_outlet   elv_gauge    elv_upst  dst_outlet    dst_upst     upa_outlet'

 1000 continue
      read(11,*,end=1090) id, lat0, lon0, area0  !! read from gauge list
      if( lon0<west .or. lon0>east .or. lat0<south .or. lat0>north ) goto 1000 !! out of domain

      ix=int( (lon0 -west2)/csize )+1
      iy=int( (north2-lat0)/csize )+1

      err0 =1.e20
      err1 =1.e20
      rate0=1.e20

      ix0=-9999
      iy0=-9999

!!    search high-res uparea and find the best fit pixel
      nn=3
      do dy=-nn, nn
        do dx=-nn, nn
          jx=ix+dx
          jy=iy+dy

          if( east-west==360 )then
            if( jx<=0 ) jx=jx+nx
            if( jx>nx ) jx=jx-nx
          endif

          if( jx<=0 .or. jx>nx .or. jy<=0 .or. jy>ny )cycle

          if( jy>0 .and. jy<=ny )then
            if( upa1m(jx,jy)>area0*0.05 )then
              err=(upa1m(jx,jy)-area0)/area0     !! relative uparea error

              err2=err                           !! error considering location difference
              dd=(  (jy-iy)**2.+(jy-iy)**2. )**0.5
              if( err>0 ) err2=err+0.02*dd
              if( err<0 ) err2=err-0.02*dd

              !! calculate error rate separately for overestimation and underestimation
              if( err2>=0 )then
                rate=(1+err2)        !! 1.x times as large
              elseif( err2>-1 .and. err2<0 )then
                rate=1./(1+err2)     !! 1.x times as small
                rate=min(rate,1000.)
              else
                rate=1000
              endif

              if( rate<rate0 )then   !! register as corresponding pixel 
                err0 =err2
                err1 =err
                rate0=rate
                ix0  =jx
                iy0  =jy
                area =upa1m(ix0,iy0)
                lon  =west2 +(ix0-0.5)*csize
                lat  =north2-(iy0-0.5)*csize
              endif
            endif
          endif
        end do
      end do

      !! if gauge cannot be allocated on CaMa-Flood map
      if( err0==1.e20 .or. area0<=0 ) then
        write(21,'(i10,2f10.3,f12.1, i8,2(i8,i6), 5f12.2,f15.2)')      id, lat0, lon0, area0, &
                  -9, -999, -999, -999, -999,   -999.0,-999.0,-999.0, -999.0, -999.0, -999.0
        goto 1000
      endif

!===================
      !! find best grid for the gauge
      iXX0=ctx1m(ix0,iy0)   !! unit-catchment iXX,iYY
      iYY0=cty1m(ix0,iy0)
      if( iXX0<0 .or. iYY0<0 )then   !! if allocated pixel is not considered in CaMa-Flood (e.g. coastal small basin), skip
        write(21,'(i10,2f10.3,f12.1, i8,2(i8,i6), 5f12.2,f15.2)')      id, lat0, lon0, area0, &
                  -9, -999, -999, -999, -999,   -999.0,-999.0,-999.0, -999.0, -999.0, -999.0
        goto 1000
      endif

      !! outlet pixel
      jx0=outx(iXX0,iYY0)
      jy0=outy(iXX0,iYY0)
      elv_outlt=elevtn(iXX0,iYY0)                !! catchment outlet elevation
      elv_gauge=elv1m(ix0,iy0)                   !! elevation difference (gauge location - outlet) 

      !! calculate distance [km] between gauge and outlet
      ix=ix0
      iy=iy0
      dst=0
      do while( ix/=jx0 .or. iy/=jy0 )
        call nextxy(ix,iy,jx,jy)
        dst=dst+rgetlen(lon1m(ix),lat1m(iy),lon1m(jx),lat1m(jy))
        ix=jx
        iy=jy
        if( ctx1m(ix,iy)/=iXX0 .or. cty1m(ix,iy)/=iyy0 ) exit
      end do
      dst_outlt=dst

      !! which stream the gauge belongs to. 1: mainstem, 2: triburaty, 3: other small stream
      type=0
      do i_ups=1, n_ups  !! check upstream grids
        jXX=upstXX(iXX0,iYY0,i_ups)
        jYY=upstYY(iXX0,iYY0,i_ups)
        if( jXX<=0 ) cycle !! no upstream

        ix=outx(jXX,jYY)  !! check outlet pixel of upstream grid
        iy=outy(jXX,jYY)
        call nextxy(ix,iy,jx,jy)
        dst=rgetlen(lon1m(ix),lat1m(iy),lon1m(jx),lat1m(jy))

        !!follow river from upstream outlet, check the gauge exist in downstream or not
        do while( ctx1m(jx,jy)==iXX0 .and. cty1m(jx,jy)==iYY0 .and. dwx1m(jx,jy)>-900 )
          if( jx==ix0 .and. jy==iy0 )then
            !! upstream grid is found
            if( i_ups==1 ) then
              type=1  !! gauge exists on the mainstem of catchment
            else
              type=2  !! gauge exists on the tributary
            endif
            jXX0=ctx1m(ix,iy)  
            jYY0=cty1m(ix,iy)
            elv_upst=elevtn(jXX0,jYY0)
            dst_upst=dst
            goto 3200         !! already decided. 
          else
            call nextxy(jx,jy,kx,ky)
            dst=dst+rgetlen(lon1m(ix),lat1m(iy),lon1m(jx),lat1m(jy))
            jx=kx
            jy=ky
          endif
        end do
      end do
 3200 continue

      if( type==0 )then  !! gauge exists on small stream
        type=3
        jXX0=-999
        jYY0=-999
        elv_upst=-999.0
        dst_upst=-999.0
      endif



      write(21,'(i10,2f10.3,f12.1, i8,2(i8,i6), 5f12.2,f15.2)')      id, lat0, lon0, area0, &
        type, iXX0,iYY0,jXX0,jYY0, elv_outlt,elv_gauge,elv_upst, dst_outlt,dst_upst, uparea(iXX0,iYY0)

      print *, id, lat0,lon0,area0, '-->', glat(iYY0),glon(iXX0), dst_outlt

      goto 1000
 1090 continue

      close(11)
      close(21)

! ====================

CONTAINS
      subroutine nextxy(ix,iy,jx,jy)
        integer :: ix,iy,jx,jy

        jx=ix+dwx1m(ix,iy)
        jy=iy+dwy1m(ix,iy)
        if( jx<=0 ) jx=jx+nx
        if( jx>nx ) jx=jx-nx

        return
      end subroutine nextxy



      real function rgetlen(rlon1, rlat1, rlon2, rlat2)
! ================================================
! to   get the length (km) between (rlon1, rlat1) to (rlon2, rlat2)
! by   nhanasaki
! on   1st Nov 2003
! at   IIS,UT
!
!     see page 643 of Rika-Nenpyo (2000)
!     at the final calculation, earth is assumed to be a sphere
! ================================================
      implicit none
      real                ::  rpi                !! Pi
      double precision    ::  de2                !! eccentricity powered by 2
      double precision    ::  da                 !! the radius of the earth
!
      real                ::  rlon1              !! longitude of the origin
      real                ::  rlon2              !! longitude of the destination
      real                ::  rlat1              !! latitude of the origin
      real                ::  rlat2              !! latitude of the destination
      double precision    ::  dsinlat1           !! sin(lat1)
      double precision    ::  dsinlon1           !! sin(lon1)
      double precision    ::  dcoslat1           !! cos(lat1)
      double precision    ::  dcoslon1           !! cos(lon1)
      double precision    ::  dsinlat2           !! sin(lat2) 
      double precision    ::  dsinlon2           !! sin(lon2)
      double precision    ::  dcoslat2           !! cos(lat2)
      double precision    ::  dcoslon2           !! cos(lon2)
      double precision    ::  dh1                !! hegiht of the origin
      double precision    ::  dn1                !! intermediate val of calculation
      double precision    ::  dx1                !! X coordinate of the origin
      double precision    ::  dy1                !! Y coordinate of the origin
      double precision    ::  dz1                !! Z coordinate of the origin
      double precision    ::  dh2                !! height of the destination
      double precision    ::  dn2                !! intermediate val of calculation
      double precision    ::  dx2                !! X coordinate of the destination
      double precision    ::  dy2                !! Y coordinate of the destination
      double precision    ::  dz2                !! Z coordinate of the destination
!
      double precision    ::  dlen               !! length between origin and destination
      double precision    ::  drad               !! half of the angle
! parameters
      data             da/6378137.0/
      data             de2/0.006694470/
      data             rpi/3.141592/      
! ================================================
! (lon1,lat1) --> (x1,y1,z1)
! ================================================
      dh1=0
      dh2=0

      dsinlat1 = dble(sin(rlat1 * rpi/180))
      dsinlon1 = dble(sin(rlon1 * rpi/180))
      dcoslat1 = dble(cos(rlat1 * rpi/180))
      dcoslon1 = dble(cos(rlon1 * rpi/180))
!
      dn1 = da/(sqrt(1.0-de2*dsinlat1*dsinlat1))
      dx1 = (dn1+dh1)*dcoslat1*dcoslon1
      dy1 = (dn1+dh1)*dcoslat1*dsinlon1
      dz1 = (dn1*(1-de2)+dh1)*dsinlat1
! ================================================
! (lon2,lat2) --> (x2,y2,z2)
! ================================================
      dsinlat2 = dble(sin(rlat2 * rpi/180))
      dsinlon2 = dble(sin(rlon2 * rpi/180))
      dcoslat2 = dble(cos(rlat2 * rpi/180))
      dcoslon2 = dble(cos(rlon2 * rpi/180))
!
      dn2 = da/(sqrt(1.0-de2*dsinlat2*dsinlat2))
      dx2 = (dn2+dh2)*dcoslat2*dcoslon2
      dy2 = (dn2+dh2)*dcoslat2*dsinlon2
      dz2 = (dn2*(1-de2)+dh2)*dsinlat2      
! ================================================
! Calculate length
! ================================================
      dlen=sqrt((dx1-dx2)**2+(dy1-dy2)**2+(dz1-dz2)**2)
      drad=dble(asin(real(dlen/2/da)))
      rgetlen=real(drad*2*da)

      rgetlen=rgetlen*0.001  !! from meter to km
!
      return
      end function rgetlen



      end program  allocate_gauge

