      program get_ele
! ===============================================
      implicit none
! CaMa-Flood parameters       
      character*256            ::  param                         !! river map parameters
      integer                  ::  nXX, nYY                      !! grid number (river network map)
      integer                  ::  nflp                          !! floodplain layers
      real*8                   ::  gsize                         !! grid size [deg]
      real*8                   ::  west, east, north, south      !! domain (river network map)

! HydroSHEDS parameters                                        !! (from location.txt)
      integer                  ::  i, narea               !! area ID
      character*256            ::  area                          !! area code
      integer                  ::  ix, iy, jx, jy, dx, dy                        
      integer                  ::  nx, ny                        !! grid number (hires data)
      real*8                   ::  csize                         !! size of pixel [deg]
      real*8                   ::  lon_ori                       !! west  edge
      real*8                   ::  lon_end                       !! east  edge
      real*8                   ::  lat_ori                       !! north edge
      real*8                   ::  lat_end                       !! south edge

      real*8                   ::  west2, east2, north2, south2      !! output domain 
      integer                  ::  mx, my

      real,allocatable         ::  out(:,:)
      integer                  ::  ngrid, mx2, my2
      real                     ::  num, maxthrs, minthrs

      character*256            ::  list_loc
!
      integer*4,allocatable    ::  nextXX(:,:)                    !! downstream (jXX,jYY)
      real,allocatable         ::  hand(:,:)                     !! height above channel [m]
      real,allocatable         ::  lon(:), lat(:)

      real,allocatable         ::  ele(:,:)                   !! flood depth [m] (coarse resolution)
!
      character*256            ::  mapdir, hires
      parameter                   (mapdir='./map/')              !! map directory (please make a symbolic link)
      character*256            ::  fnextxy, rfile, wfile
      character*256            ::  buf
      integer                  ::  ios
! ===============================================
! downscale target domain
      call getarg(1,buf)
        read(buf,*) west2
      call getarg(2,buf)
        read(buf,*) east2
      call getarg(3,buf)
        read(buf,*) south2
      call getarg(4,buf)
        read(buf,*) north2

      call getarg(5,hires)       !! downscale resolution
      call getarg(6,buf)
        read(buf,*) ngrid
    
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

! CaMa-Flood simulation domain
      east =west +real(nXX)*gsize
      south=north-real(nXX)*gsize

      if( trim(hires)=='1sec' )then
        csize=1./3600.
      elseif( trim(hires)=='3sec' )then
        csize=1./1200.
      elseif( trim(hires)=='5sec' )then
        csize=1./720.
      elseif( trim(hires)=='15sec' )then
        csize=1./240.
      elseif( trim(hires)=='30sec' )then
        csize=1./120.
      elseif( trim(hires)=='1min' )then
        csize=1./60.
      else
        stop
      endif

      minthrs=-1
      maxthrs=3

! calculate number of cell size
      mx=nint( (east2 -west2 )/csize )   !! downscale domain x*y
      my=nint( (north2-south2)/csize )

      if( mx*my>525000000 )then
        print *, 'downscale domain too large: mx*my*4>integer limit'
        stop
      endif

      print *, 'slp domain:', west2, east2, south2, north2, mx, my

! ==========

      allocate(nextXX(nXX,nYY))
      allocate(ele(mx,my))
      ele(:,:)=-9999

! ===============================================
      fnextxy=trim(mapdir)//'nextxy.bin'

      open(11, file=fnextxy, form='unformatted', access='direct', recl=4*nXX*nYY)
      read(11,rec=1) nextXX
      close(11)


! open hires files
      list_loc=trim(mapdir)//trim(hires)//'/location.txt'
      open(11,file=list_loc,form='formatted')
      read(11,*) narea
      read(11,*)

      do i=1, narea
        read(11,*) buf, area, lon_ori, lon_end, lat_end, lat_ori, nx, ny, csize

        if( lon_end<west2 .or. lon_ori>east2 .or.lat_ori<south2 .or. lat_end>north2 ) goto 1090  !! out of domain

        allocate(hand(nx,ny))
        allocate(lon(nx),lat(ny))
  
        !rfile=trim(mapdir)//trim(hires)//'/'//trim(area)//'.hand.bin'
        rfile=trim(mapdir)//trim(hires)//'/'//trim(area)//'.elevtn.bin'
        print *, rfile
        open(21,file=rfile,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
        if( ios==0 )then
          read(21,rec=1) hand
          close(21)
        else
          print *, '*******************'
          print *, 'no data: ', rfile
          stop
        endif
  
        do ix=1, nx
          lon(ix)=lon_ori+(real(ix)-0.5)*csize
          if( lon(ix)>=180. ) lon(ix)=lon(ix)-360.
          if( lon(ix)<-180. ) lon(ix)=lon(ix)+360.
        end do
        do iy=1, ny
          lat(iy) =lat_ori-(real(iy)-0.5)*csize
        end do

        do iy=1, ny
          do ix=1, nx
            if( lon(ix)>west2 .and. lon(ix)<east2 .and. lat(iy)>south2 .and. lat(iy)<north2 )then
              jx=int( (lon(ix)-west2 )/csize )+1
              jy=int( (north2-lat(iy))/csize )+1
              ele(jx,jy)=hand(ix,iy)
            endif
          end do
        end do

        deallocate(hand)
        deallocate(lon,lat)

 1090   continue
      enddo

      mx2=mx/ngrid
      my2=my/ngrid
      allocate(out(mx2,my2))
      out(:,:)=0

      do iy=1, my2
        do ix=1, mx2
          num=0
          do dx=1, ngrid
            do dy=1, ngrid
              jx=dx+ngrid*(ix-1)
              jy=dy+ngrid*(iy-1)
              out(ix,iy)=out(ix,iy)+ele(jx,jy)
              num=num+1
            end do
          end do
          out(ix,iy)=out(ix,iy)/num
        end do
      end do

      wfile='ele.bin'
      open(11, file=wfile, form='unformatted', access='direct', recl=4*mx2*my2)
      write(11,rec=1) out
      close(11)


      end program get_ele

