      program allocate_dam
! ===============================================
! to set various maps
! v5 interface: allocate_dam DamListFile HIRES_TAG
! Map arrays are loaded by allocation_map_io; area-based classification,
! matching logic, and output records follow the legacy v4 implementation.
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
      integer              ::  ifile
! index 1min
      integer              ::  ix, iy, jx, jy, kx, ky, dx, dy
      integer              ::  nx, ny                          !! high-resolution map coordinate

      integer              ::  ix0, iy0                    !! allocated station xy
! input
      real,allocatable     ::  uparea(:,:)                 !! base-map upstream area [km2 after conversion]
      real,allocatable     ::  ctmare(:,:)                 !! unit-catchment area [km2 after conversion]
      integer,allocatable  ::  nextXX(:,:), nextYY(:,:)   !! downstream base-map cell indices

      real,allocatable     ::  maxupa(:,:)                    !! largest upstream area encountered per downstream cell [km2]
      integer,allocatable  ::  upstXX(:,:,:), upstYY(:,:,:)   !! upstream cells ranked by drainage area
      integer,allocatable  ::  outx(:,:), outy(:,:)           !! high-res (ix,iy) coordinate of outlet pixel

      real,allocatable     ::  glon(:), glat(:)              !! base-map cell-center longitude/latitude [deg]

      integer*2,allocatable::  dwx1m(:,:), dwy1m(:,:)         !! downstream pixel (jx,jy) of each high-res pixel (ix,iy)
      integer*2,allocatable::  ctx1m(:,:), cty1m(:,:)         !! corresponding unit-catchment (iXX,iYY) of each high-res pixel
      real,allocatable     ::  upa1m(:,:)                     !! high-resolution upstream area [km2]
      real,allocatable     ::  lon1m(:), lat1m(:)             !! high-res pixel lat lon

      integer              ::  id                             !! external dam identifier
      character*32         ::  damname, rivname
      integer              ::  year                           !! dam construction year from input table
      real                 ::  lat0, lon0, lat, lon            !! input and selected-point coordinates [deg]
      real                 ::  area0, area                     !! reported and candidate drainage area [km2]
      real                 ::  cap_mcm                         !! reservoir capacity [million m3]
      real                 ::  err, err0, err1, err2, dd, diff !! area mismatch and adjusted candidate score
      real                 ::  rate, rate0                     !! symmetric area-error score and best score
      integer              ::  nn                              !! half-width of high-resolution candidate search window

      integer              ::  ista, jsta, nsta
      integer              ::  snum

      integer              ::  iXX0, iYY0, jXX0, jYY0
      integer              ::  isFound

      integer              ::  staX(3), staY(3)                !! selected CaMa map cells for allocation output
      real                 ::  staA(3), area_cmf                !! selected areas and reported CaMa drainage area [km2]

! file
      character*128        ::  damlist
      character*128        ::  wfile1, wfile2,  wfile3
! ===============================================
      print *, 'Allocate dam on CaMa-Flood river network'

      print *, 'USAGE'
      print *, '% ./allocate_dam DamListFile HIRES_TAG'
      call getarg(1,damlist)

      call getarg(2,hires)
      if( len_trim(hires)==0 )then
        print *, 'ERROR: provide HIRES_TAG (for example 1min or 15sec)'
        error stop 1
      endif
      print *, 'Read selected high-resolution map: ', trim(hires)

      call read_allocation_base('..',nXX,nYY,gsize,west,east,south,north, &
                                uparea,nextXX,nextYY,ctmare=ctmare)
      do iYY=1,nYY
        do iXX=1,nXX
          if( uparea(iXX,iYY)>0 ) uparea(iXX,iYY)=uparea(iXX,iYY)*1.e-6
          if( ctmare(iXX,iYY)>0 ) ctmare(iXX,iYY)=ctmare(iXX,iYY)*1.e-6
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
                                 upa1m,ctx1m,cty1m,dwx1m,dwy1m)
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

!! Search a 7x7 pixel window. Candidates must exceed 5% of the reported area;
!! the score combines relative area mismatch with a distance penalty.
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
      !! find best river-network grid for the dam
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
        staX(1)=iXX0
        staY(1)=iYY0
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
      snum=0    !! count upstream cells used to represent the dam drainage area
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

          !! follow the river downstream to check whether the dam lies on this path
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

          !! if the dam lies downstream of the upstream outlet
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
        staX(1)=iXX0
        staY(1)=iYY0
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
