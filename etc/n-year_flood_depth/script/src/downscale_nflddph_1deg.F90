      program dnflddph
! ===============================================
      implicit none
! CaMa-Flood parameters       
      character*64             ::  param                         !! river map parameters
      integer                  ::  iXX, iYY
      integer                  ::  nXX, nYY                      !! grid number (river network map)
      integer                  ::  nflp                          !! floodplain layers
      real                     ::  gsize                         !! grid size [deg]
      real                     ::  west, east, north, south      !! domain (river network map)

! Hires hydrography parameters                                   !! (from location.txt)
      integer                  ::  i, narea               !! area ID
      character*15             ::  area                          !! area code
      integer                  ::  ix, iy, jx, jy                        
      integer                  ::  nx, ny                        !! grid number (hires data)
      real                     ::  csize                         !! size of pixel [deg]
      real                     ::  lon_ori                       !! west  edge
      real                     ::  lon_end                       !! east  edge
      real                     ::  lat_ori                       !! north edge
      real                     ::  lat_end                       !! south edge

      real                     ::  west2, east2, north2, south2      !! output domain 
      integer                  ::  mx, my

      character*64             ::  list_loc
!
      integer*4,allocatable    ::  nextXX(:,:)                    !! downstream (jXX,jYY)
      integer*2,allocatable    ::  catmXX(:,:), catmYY(:,:)        !! catchment (iXX,iYY) of pixel (ix,iy)
      real,allocatable         ::  flddif(:,:), rivwth(:,:)      !! height above channel [m]
      real,allocatable         ::  lon(:), lat(:)

      real,allocatable         ::  flddph(:,:)                   !! flood depth [m] (coarse resolution)
!
      real,allocatable         ::  flood(:,:), floodt(:,:)       !! downscaled flood depth [m]
      integer*1,allocatable    ::  floodm(:,:), floodmt(:,:)     !! downscaled flood mask
!
      character*64             ::  outdir, mapdir, hires, rp
      character*128            ::  fnextxy, fflood, ffloodm
      character*256            :: rfile
      character*128            ::  fflddph
!      integer                  ::  trec
      character*128            ::  buf
      integer                  ::  ios

      integer*2                ::  ilat, ilon
      integer                  ::  oy, ox
      character*7              ::  fname
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
      call getarg(6,fflddph)    !! coarce file
      call getarg(7,rp)         !! return period
      call getarg(8,outdir)     !! output directory
!      call getarg(8,buf)        !! downscale file record number
!      if( buf/='' )then
!        read(buf,*) trec
!      else
!        trec=1
!      endif

      mapdir=trim(outdir)//'/map/'
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
      mx=int( (east2 -west2 )/csize +0.1 )   !! downscale domain x*y
      my=int( (north2-south2)/csize +0.1 )

      if( mx*my>525000000 )then
        print *, 'downscale domain too large: mx*my*4>integer limit'
        stop
      endif

      print *, 'domain:', west2, east2, south2, north2, mx, my

! ==========

      allocate(nextXX(nXX,nYY),flddph(nXX,nYY))
      allocate(flood(mx,my),floodm(mx,my))
      flood(:,:)=-9999
      floodm(:,:)=-9

! ===============================================
      fnextxy=trim(mapdir)//'nextxy.bin'

      open(11, file=fnextxy, form='unformatted', access='direct', recl=4*nXX*nYY)
      read(11,rec=1) nextXX
      close(11)

      open(12, file=fflddph, form='unformatted', access='direct', recl=4*nXX*nYY)
      read(12,rec=1) flddph
      close(12)
!      do iYY=1, nYY
!        do iXX=1, nXX
!          if( flddph(iXX,iYY)==1.e20 )then
!            flddph(iXX,iYY)=-9999
!          endif
!        end do
!      end do

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
        print *, rfile
        open(21,file=rfile,form='unformatted',access='direct',recl=2*nx*ny,status='old',iostat=ios)
        if( ios==0 )then
          read(21,rec=1) catmXX
          read(21,rec=2) catmYY
          close(21)
        else
          print *, '*******************'
          print *, 'no data: ', rfile
          stop
        endif
  
        rfile=trim(mapdir)//trim(hires)//'/'//trim(area)//'.flddif.bin'
        open(21,file=rfile,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
        if( ios==0 )then
          read(21,rec=1) flddif
          close(21)
        else
          print *, '*******************'
          print *, 'no data: ', rfile
          stop
        endif

        rfile=trim(mapdir)//trim(hires)//'/'//trim(area)//'.rivwth.bin'
        open(21,file=rfile,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
        if( ios==0 )then
          read(21,rec=1) rivwth
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

              if( catmXX(ix,iy)>0 )then
                flood(jx,jy)=0
                floodm(jx,jy)=0
                iXX=catmXX(ix,iy)
                iYY=catmYY(ix,iy)
                if( flddph(iXX,iYY)>flddif(ix,iy) )then
                  flood(jx,jy)=flddph(iXX,iYY)-flddif(ix,iy)
                  floodm(jx,jy)=1
                endif

                if( rivwth(ix,iy)/=-9999 .and. rivwth(ix,iy)/=0 )then !! permanent water
                  flood(jx,jy)=max(0.1,flood(jx,jy))
                  floodm(jx,jy)=2
                endif

              endif
            endif
          end do
        end do

        deallocate(catmXX,catmYY,flddif,rivwth)
        deallocate(lon,lat)

 1090   continue
      enddo

      do ilat=int(north2)-1, int(south2), -1
        do ilon=int(west2), int(east2)-1

          allocate(floodt(1200,1200),floodmt(1200,1200))
          floodt(:,:)=-9999
          floodmt(:,:)=-9

          CALL SET_NAME(ilat,ilon,fname)
          print *, fname

          do oy=1, 1200
            do ox=1, 1200
              floodt(ox,oy)=flood( (ilon+127)*1200+ox, (51-ilat)*1200+oy )
              floodmt(ox,oy)=floodm( (ilon+127)*1200+ox, (51-ilat)*1200+oy )
            end do
          end do

          rfile=trim(outdir)//'/HazardMap_1deg/'//fname//'_rp'//trim(rp)//'_'//trim(hires)//'.bin'
          ! print *, fflood
          open(11, file=rfile, form='unformatted', access='direct', recl=4*1200*1200)
          write(11,rec=1) floodt
          close(11)

          rfile=trim(outdir)//'/HazardMap_1deg/'//fname//'_rp'//trim(rp)//'_'//trim(hires)//'_mask.bin'
          ! print *, ffloodm
          open(11, file=rfile, form='unformatted', access='direct', recl=1*1200*1200)
          write(11,rec=1) floodmt
          close(11)

          deallocate(floodt,floodmt)
        end do
      end do

! ===============================================
CONTAINS
! ===============================================
! ===============================================
  SUBROUTINE SET_NAME(lat,lon,fname)
! ===============================================
    implicit none
  !
    integer*2       ::  lon, lat

    character*1     ::  ew, sn
    character*2     ::  clat
    character*3     ::  clon
    character*7     ::  fname
! ===============================================
      if( lat<0 )then
        sn='s'
        write(clat,'(i2.2)') int(-lat)
      else
        sn='n'
        write(clat,'(i2.2)')  int(lat)
      endif

      if( lon<0 )then
        ew='w'
        write(clon,'(i3.3)') int(-lon)
      else
        ew='e'
        write(clon,'(i3.3)')  int(lon)
      endif

      fname=sn//clat//ew//clon

  END SUBROUTINE SET_NAME


      end program dnflddph

