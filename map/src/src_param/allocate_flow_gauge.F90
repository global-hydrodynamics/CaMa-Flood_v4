      program allocate_gauge
! ===============================================
! to allocate river flow gauge on CaMa-Flood river map.
!   input: gauge list, allocated on MERIT Hydro or J-FlwDir.
!          1st-4th column should be (ID, Lat, Lon, Uparea)
! ===============================================
      implicit none
!
      character*16         ::  mode   !! multi  = up to 2 corresponding grid
                                      !! single = only  1 corresponding grid (primary for dam allocation)

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
! input
      real,allocatable     ::  uparea(:,:)                    !! drainage area (GRID base)
      integer,allocatable  ::  nextXX(:,:), nextYY(:,:)       !! next grid X

      real,allocatable     ::  maxupa(:,:)                    !! maximum drainage area of upstream grids 
      integer,allocatable  ::  upstXX(:,:,:), upstYY(:,:,:)   !! list of upstream grid
      integer,allocatable  ::  outx(:,:), outy(:,:)           !! high-res (ix,iy) coordinate of outlet pixel

      real,allocatable     ::  glon(:), glat(:)

      integer*2,allocatable::  dwx1m(:,:), dwy1m(:,:)         !! downstream pixel (jx,jy) of each high-res pixel (ix,iy) 
      integer*2,allocatable::  ctx1m(:,:), cty1m(:,:)         !! corresponding unit-catchment (iXX,iYY) of each high-res pixel
      real,allocatable     ::  upa1m(:,:)                     !! high-res upstream area 
      real,allocatable     ::  lon1m(:), lat1m(:)             !! high-res pixel lat lon

      integer*8            ::  id
      real                 ::  lat0, lon0, lat, lon, area0, area
      real                 ::  err, err0, err1, err2, dd, diff
      real                 ::  rate, rate0
      integer              ::  nn

      integer              ::  i_ups, j_ups, n_ups            !! upstream grid 
      integer              ::  snum

      integer              ::  iXX0, iYY0, jXX0, jYY0
      integer              ::  isFound

      integer              ::  staX(2), staY(2)
      real                 ::  staA(2), upa_sum

! file
      character*128        ::  fparam
      character*128        ::  rfile1, gaugelist, hilist
      character*128        ::  wfile1
! ===============================================
      print *, 'Allocate gauge on CaMa-Flood river network'

      print *, 'USAGE'
      print *, '% ./allocate_flow_gauge GaugeListFile (multi/single)'
      call getarg(1,gaugelist)
      call getarg(2,mode)

      if( trim(mode)=='multi' )then
        mode='multi'
      elseif( trim(mode)=='single' )then
        mode='single'
      else
        mode='multi'
      endif

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
      allocate(uparea(nXX,nYY))
      allocate(nextXX(nXX,nYY),nextYY(nXX,nYY))

      rfile1='../uparea.bin'
      open(11, file=rfile1, form='unformatted', access='direct', recl=4*nXX*nYY,status='old',iostat=ios)
      read(11,rec=1) uparea
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

      allocate(upa1m(nx,ny),ctx1m(nx,ny),cty1m(nx,ny),dwx1m(nx,ny),dwy1m(nx,ny))

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
      write(21,'(a,a)') '        ID       lat       lon  area_Input   area_CaMa       error        diff', &
                '    Type     ix1   iy1     ix2   iy2       area1       area2'

 1000 continue
      read(11,*,end=1090) id, lat0, lon0, area0  !! read from gauge list
      if( lon0<west .or. lon0>east .or. lat0<south .or. lat0>north ) goto 1000 !! out of domain

      ix=int( (lon0 -west2)/csize )+1
      iy=int( (north2-lat0)/csize )+1

      err0 =1.e20
      err1 =1.e20
      rate0=1.e20

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
                kx   =jx
                ky   =jy
                area =upa1m(kx,ky)
                lon  =west2 +(kx-0.5)*csize
                lat  =north2-(ky-0.5)*csize
              endif
            endif
          endif
        end do
      end do

      !! if gauge cannot be allocated on CaMa-Flood map
      if( err0==1.e20 .or. area0<=0 ) then
        write(21,'(i10,2f10.3,4f12.2,  i8,2(i8,i6),2f12.1)') id, lat0, lon0, area0, -999.0, -999.0, -999.0,&
                  -9, -999, -999, -999, -999, -999.0, -999.0
        goto 1000
      endif

!===================
      !! find best grid for the gauge
      ix0=kx  !! GRDC allocated on 1min
      iy0=ky
      iXX0=ctx1m(ix0,iy0)   !! unit-catchment iXX,iYY
      iYY0=cty1m(ix0,iy0)

      if( iXX0<0 .or. iYY0<0 )then   !! if allocated pixel is not considered in CaMa-Flood (e.g. coastal small basin), skip
        write(21,'(i10,2f10.3,4f12.2,  i8,2(i8,i6),2f12.1)') id, lat0, lon0, area0, -999.0, -999.0, -999.0,&
                  -9, -999, -999, -999, -999, -999.0, -999.0
        goto 1000
      endif

      staX(1)=iXX0  !! primary corresponding grid
      staY(1)=iYY0
      staA(1)=uparea(iXX0,iYY0)

      staX(2)=-999  !! secondary corresponding grid
      staY(2)=-999
      staA(2)=-999

      jXX0=iXX0
      jYY0=iYY0
      area=uparea(jXX0,jYY0)
      diff=area-area0
      err1=diff/area0        !! relative error of uparea (outlet uparea - gauge uparea)
      isFound=0

! =============
      !! if uparea of outlet is >5% larger than gauge's uparea, consider using upstream grids as alternative corresponding grid
      snum=0       !! when upstream is used to represent gauge, count number of upstream
      upa_sum=0    !! summation of upstream uparea
      if( abs(err1)>0.05 )then 

        do i_ups=1, n_ups  !! check upstream grids
          jXX=upstXX(iXX0,iYY0,i_ups)
          jYY=upstYY(iXX0,iYY0,i_ups)
          if( jXX<=0 ) goto 3100

          ix=outx(jXX,jYY)  !! check outlet pixel of upstream grid
          iy=outy(jXX,jYY)
          call nextxy(ix,iy,jx,jy)
          isFound=0

          !!follow river from upstream outlet, check the gauge exist in downstream or not
          do while( ctx1m(jx,jy)==iXX0 .and. cty1m(jx,jy)==iYY0 .and. dwx1m(jx,jy)>-900 )
            if( jx==ix0 .and. jy==iy0 )then
              isFound=1
              exit
            else
              call nextxy(jx,jy,kx,ky)
              jx=kx
              jy=ky
            endif
          end do

          !! if gauge exist in downsream of the upstream outlet
          if( isFound==1 )then
            jXX=ctx1m(ix,iy)
            jYY=cty1m(ix,iy)
            if( uparea(jXX,jYY)<area0*0.1 ) goto 3200   !! upstream grid uparea is 1-order smaller than gauge uparea --> small tributary: skip

            area=upa_sum+uparea(jXX,jYY)    !! consider multiple upstream grids
            diff=area-area0
            err2=diff/area0

            if( abs(err2)<abs(err1) )then   !! if error of "(sum of) upstream uparea" is smaller than using downstream uparea, 
              err1=err2                     !!  use upstream to represent gauge location
              snum=snum+1
              staX(snum)=jXX
              staY(snum)=jYY
              staA(snum)=uparea(jXX,jYY)
              upa_sum=area

              if( snum==2       ) goto 3200   !! maximum 2 upstream grids  to represent gauges
              if( trim(mode)=='single' .and. snum==1 ) goto 3200  !! single corresponding grid mode
              if( abs(err1)<0.1 ) goto 3200   !! if upstream error is smaller than 10%, stop searching corresponding grid
            endif
          endif
 3100     continue
        end do
      endif
 3200 continue

      if( snum==0 )then
        upa_sum=uparea(iXX0,iYY0)   !! if no upstream corresponding grid, use downstream uparea as CaMa uparea
      endif
      diff=upa_sum-area0  !! difference: CaMa uparea - gauge uparea
      err1=diff/area0     !! relative error

      if( snum==0 .and. err1>1 )then !! gauge is on small tributary, and no corresponding CaMa grid
        write(21,'(i10,2f10.3,4f12.2,  i8,2(i8,i6),2f12.1)') id, lat0, lon0, area0, -999.0, -999.0, -999.0,&
                  -9, -999, -999, -999, -999, -999.0, -999.0
        goto 1000
      endif

      write(21,'(i10,2f10.3,4f12.2,  i8,2(i8,i6),2f12.1)') id, lat0, lon0, area0, upa_sum, err1, diff,&
              snum, staX(1), staY(1), staX(2), staY(2), StaA(1), StaA(2)

      iXX=staX(1)
      iYY=staY(1)
      if( iXX>0 )then
        print '(i8,3f12.2,a,3f12.2)', id, lat0, lon0, area0, '  -->', glat(iYY),glon(iXX),uparea(iXX,iYY)
      endif

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



      end program  allocate_gauge

