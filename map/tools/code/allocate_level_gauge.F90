      program allocate_level_gauge
! v5 interface: allocate_level_gauge GaugeListFile HIRES_TAG
! Map arrays are loaded by allocation_map_io; classification, river-path
! distance, and output behavior are retained from the legacy v4 program.
! ===============================================
! to allocate river flow gauge on CaMa-Flood river map.
!   input: gauge list, allocated on MERIT Hydro or J-FlwDir.
!          1st-4th column should be (ID, Lat, Lon, Uparea)
! ===============================================
      use allocation_map_io, only: read_allocation_base, read_allocation_hires
      implicit none
! index TRIP
      integer              ::  iXX, iYY, jXX, jYY
      integer              ::  nXX, nYY                        !! x-y matrix GLOBAL
      real                 ::  gsize, csize                    !! grid size [degree]
      real                 ::  west,  north,  east,  south       !! base-map bounds [deg]
      real                 ::  west2, north2, east2, south2      !! selected high-resolution map bounds [deg]
      character*16         ::  hires                           !! selected high-resolution map tag
! index 1min
      integer              ::  ix, iy, jx, jy, kx, ky, dx, dy
      integer              ::  nx, ny                          !! high-resolution map coordinate

      integer              ::  ix0, iy0                        !! allocated station xy
      integer              ::  jx0, jy0                        !! catchment outlet of the allocated gauge
! input
      real,allocatable     ::  uparea(:,:)                    !! base-map upstream area [km2 after conversion]
      real,allocatable     ::  elevtn(:,:)                    !! base-map elevation [m]
      integer,allocatable  ::  nextXX(:,:), nextYY(:,:)       !! downstream base-map cell indices

      real,allocatable     ::  maxupa(:,:)                    !! largest upstream area encountered per downstream cell [km2]
      integer,allocatable  ::  upstXX(:,:,:), upstYY(:,:,:)   !! upstream cells ranked by drainage area
      integer,allocatable  ::  outx(:,:), outy(:,:)           !! high-res (ix,iy) coordinate of outlet pixel

      real,allocatable     ::  glon(:), glat(:)              !! base-map cell-center longitude/latitude [deg]

      integer*2,allocatable::  dwx1m(:,:), dwy1m(:,:)         !! downstream pixel (jx,jy) of each high-res pixel (ix,iy)
      integer*2,allocatable::  ctx1m(:,:), cty1m(:,:)         !! corresponding unit-catchment (iXX,iYY) of each high-res pixel
      real,allocatable     ::  upa1m(:,:)                     !! high-resolution upstream area [km2]
      real,allocatable     ::  elv1m(:,:)                     !! high-resolution elevation [m]
      real,allocatable     ::  lon1m(:), lat1m(:)             !! high-res pixel lat lon

      integer*8            ::  id                              !! external gauge identifier
      real                 ::  lat0, lon0, lat, lon             !! input and selected-point coordinates [deg]
      real                 ::  area0, area                      !! reported and candidate drainage area [km2]
      real                 ::  err, err0, err1, err2, dd       !! area mismatch and adjusted candidate score
      real                 ::  rate, rate0                      !! symmetric area-error score and best score

      integer              ::  type         !! 1: mainstem; 2: tributary; 3: small stream

      real                 ::  dst                              !! accumulated river-path distance [km]
      real                 ::  dst_outlt                      !! distance to outlet pixel [km]
      real                 ::  dst_upst                       !! distance from upstream outlet pixel [km]

      real                 ::  elv_outlt                      !! elevation of outlet pixel [m]
      real                 ::  elv_gauge                      !! elevation of gauge  pixel [m]
      real                 ::  elv_upst                       !! elevation of upstream outlet pixel [m]

      integer              ::  nn                               !! half-width of high-resolution candidate search window

      integer              ::  i_ups, j_ups, n_ups            !! ranked upstream-candidate indices and list size

      integer              ::  iXX0, iYY0, jXX0, jYY0
! file
      character*128        ::  gaugelist
      character*128        ::  wfile1
! ===============================================
      print *, 'Allocate gauge on CaMa-Flood river network'

      print *, 'USAGE'
      print *, '% ./allocate_level_gauge GaugeListFile HIRES_TAG'
      call getarg(1,gaugelist)

      call getarg(2,hires)
      if( len_trim(hires)==0 )then
        print *, 'ERROR: provide HIRES_TAG (for example 1min or 15sec)'
        error stop 1
      endif
      print *, 'Read selected high-resolution map: ', trim(hires)

      call read_allocation_base('..',nXX,nYY,gsize,west,east,south,north, &
                                uparea,nextXX,nextYY,elevtn=elevtn)
      do iYY=1,nYY
        do iXX=1,nXX
          if( uparea(iXX,iYY)>0 ) uparea(iXX,iYY)=uparea(iXX,iYY)*1.e-6
        enddo
      enddo
      allocate(glon(nXX),glat(nYY))
      do iYY=1,nYY
        glat(iYY)=north-gsize*(iYY-0.5)
      enddo
      do iXX=1,nXX
        glon(iXX)=west+gsize*(iXX-0.5)
      enddo

      call read_allocation_hires('..',trim(hires),nx,ny,west2,east2,south2,north2, &
                                 upa1m,ctx1m,cty1m,dwx1m,dwy1m,elevtn=elv1m)
      csize=dble(east2-west2)/dble(nx)
      print *, ' nx,ny,csize=', nx,ny,csize
      allocate(lon1m(nx),lat1m(ny))
      do iy=1,ny
        lat1m(iy)=north2-(iy-0.5)*csize
      enddo
      do ix=1,nx
        lon1m(ix)=west2+(ix-0.5)*csize
      enddo

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
                    if( upa1m(jx,jy)>maxupa(iXX,iYY) )then    !! resampling can give a cell multiple outlets
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

!! Search a 7x7 pixel window. Candidates must exceed 5% of the reported area;
!! the score combines relative area mismatch with a distance penalty.
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
              dd=(  (jx-ix)**2.+(jy-iy)**2. )**0.5
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
            dst=dst+rgetlen(lon1m(jx),lat1m(jy),lon1m(kx),lat1m(ky))
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



      end program  allocate_level_gauge
