      program proj_flood
! ===============================================
      implicit none
! CaMa-Flood parameters       
      character*256            ::  param                         !! river map parameters
      integer                  ::  iXX, iYY
      integer                  ::  nXX, nYY                      !! grid number (river network map)
      integer                  ::  nflp                          !! floodplain layers
      real*8                   ::  gsize                         !! grid size [deg]
      real*8                   ::  west, east, north, south      !! domain (river network map)

! HydroSHEDS parameters                                        !! (from location.txt)
      integer                  ::  i, narea               !! area ID
      character*256            ::  area                          !! area code
      integer                  ::  ix, iy, jx, jy                        
      integer                  ::  nx, ny                        !! grid number (hires data)
      real*8                   ::  csize                         !! size of pixel [deg]
      real*8                   ::  lon_ori                       !! west  edge
      real*8                   ::  lon_end                       !! east  edge
      real*8                   ::  lat_ori                       !! north edge
      real*8                   ::  lat_end                       !! south edge

      real*8                   ::  west2, east2, north2, south2      !! output domain 
      integer                  ::  mx, my

      character*256            ::  list_loc
!
      integer*4,allocatable    ::  nextXX(:,:)                    !! downstream (jXX,jYY)
      integer*2,allocatable    ::  catmXX(:,:), catmYY(:,:)        !! catchment (iXX,iYY) of pixel (ix,iy)
      real,allocatable         ::  flddif(:,:), rivwth(:,:)      !! height above channel [m]
      real,allocatable         ::  lon(:), lat(:)

      !real,allocatable         ::  flddph(:,:)                   !! flood depth [m] (coarse resolution)
      real,allocatable         ::  flddph(:,:,:)                   !! flood depth [m] (coarse resolution)
      real,allocatable         ::  fldday(:,:,:)                   !! inundation period [day] (coarse resolution)
!
      real,allocatable         ::  flood(:,:)                    !! downscaled flood depth [m]
!
      character*256            ::  mapdir, hires
      parameter                   (mapdir='./map/')              !! map directory (please make a symbolic link)
      character*256            ::  fnextxy, fflood, rfile
      character*256            ::  ffldday
      integer                  ::  itrec, ntrec
      character*256            ::  buf
      integer                  ::  ios
! Parameters for inundation period
      real                     ::  diffdph, intdph


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
      call getarg(6,ffldday)    !! depth-duration file
      call getarg(7,fflood)     !! output file
      call getarg(8,buf)     !! interval for depth-duration calculation
        read(buf,*) intdph
      call getarg(9,buf)        !! downscale file total record number
      if( buf/='' )then
        read(buf,*) ntrec
      else
        ntrec=1
      endif

      print *, trim(ffldday), '->', trim(fflood)


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

      if( trim(hires)=='3sec' )then
        csize=1./1200.
      elseif( trim(hires)=='15sec' )then
        csize=1./240.
      elseif( trim(hires)=='30sec' )then
        csize=1./120.
      elseif( trim(hires)=='1min' )then
        csize=1./60.
      endif

! calculate number of cell size
      mx=nint( (east2 -west2 )/csize )   !! downscale domain x*y
      my=nint( (north2-south2)/csize )

      !if( mx*my>525000000 )then
      !  print *, 'downscale domain too large: mx*my*4>integer limit'
      !  stop
      !endif

      print *, 'domain:', west2, east2, south2, north2, mx, my

! ==========

      !allocate(nextXX(nXX,nYY),flddph(nXX,nYY))
      allocate(nextXX(nXX,nYY))
      allocate(flddph(nXX,nYY,ntrec),fldday(nXX,nYY,ntrec))
      allocate(flood(mx,my))
      flood(:,:)=-9999

! ===============================================
      fnextxy=trim(mapdir)//'nextxy.bin'

      open(11, file=fnextxy, form='unformatted', access='direct', recl=4*nXX*nYY)
      read(11,rec=1) nextXX
      close(11)

      open(12, file=ffldday, form='unformatted', access='direct', recl=4*nXX*nYY)
      do itrec = 1, ntrec
        read(12,rec=itrec) fldday(:, :,itrec)
      end do
      close(12)

! open hires files
      list_loc=trim(mapdir)//trim(hires)//'/location.txt'
      open(11,file=list_loc,form='formatted')
      read(11,*) narea
      read(11,*)

      do i=1, narea
        read(11,*) buf, area, lon_ori, lon_end, lat_end, lat_ori, nx, ny, csize

        if( lon_end<west2 .or. lon_ori>east2 .or.lat_ori<south2 .or. lat_end>north2 ) goto 1090  !! out of domain
        allocate(catmXX(nx,ny),catmYY(nx,ny),flddif(nx,ny),rivwth(nx,ny))
        allocate(lon(nx),lat(ny))
  
        rfile=trim(mapdir)//trim(hires)//'/'//trim(area)//'.catmxy.bin'
        print *, trim(rfile)
        open(21,file=rfile,form='unformatted',access='direct',recl=2*nx*ny,status='old',iostat=ios)
        if( ios==0 )then
          read(21,rec=1) catmXX
          read(21,rec=2) catmYY
          close(21)
        else
          print *, '*******************'
          print *, 'no data: ', trim(rfile)
          stop
        endif
  
        rfile=trim(mapdir)//trim(hires)//'/'//trim(area)//'.flddif.bin'
        open(21,file=rfile,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
        if( ios==0 )then
          read(21,rec=1) flddif
          close(21)
        else
          print *, '*******************'
          print *, 'no data: ', trim(rfile)
          stop
        endif

        rfile=trim(mapdir)//trim(hires)//'/'//trim(area)//'.rivwth.bin'
        open(21,file=rfile,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
        if( ios==0 )then
          read(21,rec=1) rivwth
          close(21)
        else
          print *, '*******************'
          print *, 'no data: ', trim(rfile)
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

              if( catmXX(ix,iy)>0 )then
                iXX=catmXX(ix,iy)
                iYY=catmYY(ix,iy)
                !diffdph = max(maxval(flddph(iXX,iYY,:)), 0.0)
                !diffdph = diffdph - max(flddif(ix,iy),0.0)
                diffdph = flddif(ix,iy)
                if (diffdph >= 0.0) then
                  itrec = min(int(diffdph/intdph) + 1, ntrec)
                  flood(jx,jy)=fldday(iXX,iYY,itrec)
                else
                  flood(jx,jy)=0.0
                endif

                if( rivwth(ix,iy)/=-9999 .and. rivwth(ix,iy)/=0 )then !! permanent water
                  flood(jx,jy)=max(999.0,flood(jx,jy))
                endif

              endif
            endif
          end do
        end do

        deallocate(catmXX,catmYY,flddif,rivwth)
        deallocate(lon,lat)

 1090   continue
      enddo

      print *, trim(fflood)
      !open(11, file=fflood, form='unformatted', access='direct', recl=4*mx*my)
      !write(11,rec=1) flood
      !close(11)
      open(11, file=fflood, form='unformatted', access='direct', recl=4*mx)
      do iy = 1, my
        write(11,rec=iy) flood(:,iy)
      enddo
      close(11)

      end program proj_flood

