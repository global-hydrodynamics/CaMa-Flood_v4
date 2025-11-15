      program allocate_dam
! ===============================================
! to set various maps
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
      integer              ::  ios, inum, ifile
! index 1min
      integer              ::  ix, iy, jx, jy, kx, ky, dx, dy
      integer              ::  nx, ny                          !! high-resolution map coordinate

      integer              ::  ix0, iy0                    !! allocated station xy
! input
      real,allocatable     ::  uparea(:,:)                 !! drainage  area 
      real,allocatable     ::  ctmare(:,:)                 !! catchment area 
      integer,allocatable  ::  nextXX(:,:), nextYY(:,:)   !! next grid X

      real,allocatable     ::  maxupa(:,:)                    !! maximum drainage area of upstream grids 
      integer,allocatable  ::  upstXX(:,:,:), upstYY(:,:,:)   !! list of upstream grid
      integer,allocatable  ::  outx(:,:), outy(:,:)           !! high-res (ix,iy) coordinate of outlet pixel

      real,allocatable     ::  glon(:), glat(:)

      integer*2,allocatable::  dwx1m(:,:), dwy1m(:,:)         !! downstream pixel (jx,jy) of each high-res pixel (ix,iy) 
      integer*2,allocatable::  ctx1m(:,:), cty1m(:,:)         !! corresponding unit-catchment (iXX,iYY) of each high-res pixel
      real,allocatable     ::  upa1m(:,:)                     !! high-res upstream area 
      real,allocatable     ::  lon1m(:), lat1m(:)             !! high-res pixel lat lon

      integer              ::  id
      character*32         ::  damname, rivname
      integer              ::  year
      real                 ::  lat0, lon0, lat, lon, area0, area, cap_mcm
      real                 ::  err, err0, err1, err2, dd, diff
      real                 ::  rate, rate0
      integer              ::  nn

      integer              ::  ista, jsta, nsta
      integer              ::  snum

      integer              ::  iXX0, iYY0, jXX0, jYY0
      integer              ::  isFound

      integer              ::  staX(3), staY(3)
      real                 ::  staA(3), area_cmf

! file
      character*128        ::  fparam
      character*128        ::  rfile1, damlist, hilist
      character*128        ::  wfile1, wfile2,  wfile3
! ===============================================
      print *, 'Allocate dam on CaMa-Flood river network'

      print *, 'USAGE'
      print *, '% ./allocate_dam DamListFile'
      call getarg(1,damlist)

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
      allocate(uparea(nXX,nYY),ctmare(nXX,nYY))
      allocate(nextXX(nXX,nYY),nextYY(nXX,nYY))

      rfile1='../uparea.bin'
      open(11, file=rfile1, form='unformatted', access='direct', recl=4*nXX*nYY,status='old',iostat=ios)
      read(11,rec=1) uparea
      close(11)

      rfile1='../ctmare.bin'
      open(11, file=rfile1, form='unformatted', access='direct', recl=4*nXX*nYY,status='old',iostat=ios)
      read(11,rec=1) ctmare
      close(11)

      rfile1='../nextxy.bin'
      open(11, file=rfile1, form='unformatted', access='direct', recl=4*nXX*nYY,status='old',iostat=ios)
      read(11,rec=1) nextXX
      read(11,rec=2) nextYY
      close(11)

      do iYY=1, nYY
        do iXX=1, nXX
          if( uparea(iXX,iYY)>0 ) uparea(iXX,iYY)=uparea(iXX,iYY)*1.e-6
          if( ctmare(iXX,iYY)>0 ) ctmare(iXX,iYY)=ctmare(iXX,iYY)*1.e-6
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

      nsta=8  !! number of upstream grid considered
      allocate(upstXX(nXX,nYY,nsta),upstYY(nXX,nYY,nsta),maxupa(nXX,nYY))

      upstXX(:,:,:)=-9999
      upstYY(:,:,:)=-9999
      do ista=1, nsta
        maxupa(:,:)=0
        do iYY=1, nYY
          do iXX=1, nXX
            if( nextXX(iXX,iYY)>0 ) then
              jXX=nextXX(iXX,iYY)
              jYY=nextYY(iXX,iYY)
              if( ista>=2 )then
                do jsta=1, ista-1
                  if( iXX==upstXX(jXX,jYY,jsta).and.iYY==upstYY(jXX,jYY,jsta) )then
                    goto 2000
                  endif
                end do
              endif
              if( uparea(iXX,iYY)>maxupa(jXX,jYY) )then
                maxupa(jXX,jYY)=uparea(iXX,iYY)
                upstXX(jXX,jYY,ista)=iXX
                upstYY(jXX,jYY,ista)=iYY
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
      open(11, file=damlist, form='formatted')
      read(11,*)

      wfile1='./dam_alloc_river.txt'
      open(21, file=wfile1, form='formatted')
      write(21,'(a,a)') '        ID       lat       lon   area_CaMa  area_Input       error        diff', &
                        '      ix      iy     cap_mcm  year  damname                         rivname'

      wfile2='./dam_alloc_small.txt'
      open(22, file=wfile2, form='formatted')
      write(22,'(a,a)') '        ID       lat       lon   area_CaMa  area_Input       error        diff', &
                        '      ix      iy     cap_mcm  year  damname                         rivname'

      wfile3='./dam_alloc_error.txt'
      open(23, file=wfile3, form='formatted')
      write(23,'(a,a)') '        ID       lat       lon   area_CaMa  area_Input       error        diff', &
                        '      ix      iy     cap_mcm  year  damname                         rivname'



 1000 continue
      read(11,*,end=1090) id, lat0,lon0,area0, damname, rivname, cap_mcm, year

      if( lon0<west .or. lon0>east .or. lat0<south .or. lat0>north ) goto 1000
      if( cap_mcm<0 ) goto 1000

      ix=int( (lon0 -west2)/csize )+1
      iy=int( (north2-lat0)/csize )+1

      err0=1.e20
      err1=1.e20
      rate0=1.e20
      nn=3

!!    search high-res uparea and find the best fit
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
              err=(upa1m(jx,jy)-area0)/area0      !! relative uparea error

              err2=err                            !! error considering location difference
              dd=(  (jx-ix)**2.+(jy-iy)**2. )**0.5
              if( err>0 ) err2=err+0.02*dd
              if( err<0 ) err2=err-0.02*dd

              !! calculate error rate separately for overestimation and underestimation
              if( err2>=0 )then
                rate=(1+err2)
              elseif( err2>-1 .and. err2<0 )then
                rate=1./(1+err2)
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
        write(23,'(i10,2f10.3,4f12.1, 2i8,f12.1,i6,2x,a30,2x,a30)') id, lat0,lon0, -999. ,area0, -999., -999.,&
                -999, -999, cap_mcm, year, trim(damname), trim(rivname)
        goto 1000
      endif

!===================
      !! find best grid for the gauge
      ix0=kx  !! GRDC allocated on 1min
      iy0=ky
      iXX0=ctx1m(ix0,iy0)
      iYY0=cty1m(ix0,iy0)

      if( iXX0<0 .or. iYY0<0 )then   !! if allocated pixel is not considered in CaMa-Flood (e.g. coastal small basin), skip
        write(23,'(i10,2f10.3,4f12.1, 2i8,f12.1,i6,2x,a30,2x,a30)') id, lat0,lon0, -999. ,area0, -999., -999.,&
                -999, -999, cap_mcm, year, trim(damname), trim(rivname)
        goto 1000
      endif

      !! when dam is very small and cannot be allocated on river network (treat as subgrid dam, receive runoff)
      if( area0<ctmare(iXX0,iYY0)*0.3 )then
        staX(1)=iXX
        staY(1)=iYY
        staA(1)=-888
        area_cmf=-888
        diff=-888
        err1=-8
        goto 3300
      endif

        !! when dam is very small and cannot be allocated on river network
        if( area0<ctmare(iXX0,iYY0)*0.3 )then

        endif

      staX(1)=iXX0
      staY(1)=iYY0
      staA(1)=uparea(iXX0,iYY0)

      jXX0=iXX0
      jYY0=iYY0
      area=uparea(jXX0,jYY0)
      diff=area-area0
      err1=diff/area0
      isFound=0

! ======
      snum=0    !! when upstream is used to represent gauge, count number of upstream
      area_cmf=0
      if( abs(err1)>0.2 )then    !! uparea error is large. consider using upstream grid

        do ista=1, nsta  !! check upstream grids
          jXX=upstXX(iXX0,iYY0,ista)
          jYY=upstYY(iXX0,iYY0,ista)
          if( jXX<=0 ) goto 3100

          ix=outx(jXX,jYY)  !! outlet pixel of upstream grid
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
            if( uparea(jXX,jYY)<area0*0.1 ) goto 3200

            area=uparea(jXX,jYY)
            diff=area-area0
            err2=diff/area0

            if( abs(err2)<abs(err1) )then
              err1=err2
              snum=1
              staX(snum)=jXX
              staY(snum)=jYY
              staA(snum)=uparea(jXX,jYY)
              area_cmf=area

              if( abs(err1)<0.1 ) goto 3200
            endif
          endif
 3100     continue
        end do
      endif
 3200 continue

      if( snum==0 )then
        area_cmf=uparea(iXX0,iYY0)   !! if no upstream corresponding grid, use downstream uparea as CaMa uparea
      endif
      diff=area_cmf-area0  !! difference: CaMa uparea - gauge uparea
      err1=diff/area0      !! relative error

      !! when dam is very small and cannot be allocated on river network
      if( err1>1 )then
        staX(1)=iXX
        staY(1)=iYY
        staA(1)=-888
        area_cmf=-888
        diff=-888
        err1=-8
      endif

 3300 continue

      ifile=21
      if( area_cmf==-888 ) ifile=22  !! too small dam to allocate on river map (sub-grid dams)
      if( area_cmf==-999 ) ifile=23  !! dams which cannot be allocated 

      write(ifile,'(i10,2f10.3,4f12.1, 2i8,f12.1,i6,2x,a30,2x,a30)') id, lat0,lon0, area_cmf,area0, err1,diff,&
                staX(1), staY(1), cap_mcm, year, damname, rivname

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



      end program  allocate_dam

