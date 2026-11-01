      program cut_domain
! ==================================================
      implicit none
!
      character*256          ::  region_info
      parameter                 (region_info='./region_info.txt')
      character*256          ::  global_dir
      real*8                 ::  west, east, north, south
      real*8                 ::  west2, east2, north2, south2

      character*256          ::  global_param, cut_param
      real*8                 ::  lon_ori, lat_ori, lon_end, lat_end
      real*8                 ::  glon, glat, d1, d2

      character*256          ::  region_dir, region_param
      parameter                 (region_dir='../')
      parameter                 (region_param='../params.txt')
!
      integer                ::  ix, iy, jx, jy, kx, ky
      integer                ::  nx, ny, mx, my, dx, dy
      real*8                 ::  gsize
      integer                ::  iflp, nflp                  !! number of floodplain layer
!
      character*256          ::  fnextxy0, fdownxy0, felevtn0, ffldhgt0, fctmare0, fgrdare0, fuparea0
      character*256          ::  flonlat0, fnxtdst0, frivlen0, fwidth0
      character*256          ::  fnextxy,  fdownxy,  felevtn,  ffldhgt,  fctmare,  fgrdare,  fuparea
      character*256          ::  flonlat,  fnxtdst,  frivlen,  fwidth
      character*256          ::  flsmask

      integer,allocatable    ::  nextx0(:,:),  nexty0(:,:)      !!  global maps
      integer,allocatable    ::  downx0(:,:),  downy0(:,:)      !!  global maps
      real,allocatable       ::  elevtn0(:,:), fldhgt0(:,:,:)
      real,allocatable       ::  ctmare0(:,:), uparea0(:,:), grdare0(:,:)
      real,allocatable       ::  lon0(:,:)   , lat0(:,:)
      real,allocatable       ::  nxtdst0(:,:), rivlen0(:,:)
      real,allocatable       ::  width0(:,:)

      integer,allocatable    ::  nextx(:,:),  nexty(:,:)        !!  regional maps
      integer,allocatable    ::  downx(:,:),  downy(:,:)        !!  regional maps
      real,allocatable       ::  elevtn(:,:), fldhgt(:,:,:)
      real,allocatable       ::  ctmare(:,:), uparea(:,:),   grdare(:,:)
      real,allocatable       ::  lon(:,:),    lat(:,:)
      real,allocatable       ::  nxtdst(:,:), rivlen(:,:)
      real,allocatable       ::  width(:,:)
      integer,allocatable    ::  lsmask(:,:)

      real,allocatable       ::  tmp0(:,:), tmp(:,:)            !! for fldhgt I/O

      integer,allocatable    ::  check(:,:)
      integer                ::  icheck

! ==================================================
      open(11,file=region_info,form='formatted')
      read(11,'(a)') global_dir       !!  global map directory
      read(11,*    ) west               !! regional map domain
      read(11,*    ) east
      read(11,*    ) south
      read(11,*    ) north
      close(11)

      global_param=trim(global_dir)//'/params.txt'

      open(11,file=global_param,form='formatted')
      read(11,*) nx                  !! global map nx*ny
      read(11,*) ny
      read(11,*) nflp
      read(11,*) gsize
      read(11,*) lon_ori             !! global map west
      read(11,*) lon_end
      read(11,*) lat_end
      read(11,*) lat_ori             !! global map north
      close(11)

      allocate(nextx0(nx,ny),nexty0(nx,ny),downx0(nx,ny),downy0(nx,ny))
      allocate(elevtn0(nx,ny),fldhgt0(nx,ny,nflp),ctmare0(nx,ny),grdare0(nx,ny),uparea0(nx,ny))
      allocate(lon0(nx,ny),  lat0(nx,ny),  nxtdst0(nx,ny),rivlen0(nx,ny), width0(nx,ny))

      print *, 'input:  ', west, east, north, south
      west2=west
      east2=east
      north2=north
      south2=south

      !! find optimum west, east, north, south, if the regionalized grid is offset from the original grid
      d1=1.e20
      d2=1.e20
      do ix=1, nx
        glon=lon_ori+dble(ix-1)*gsize
        if( west2>=glon-gsize*0.1 .and. west2< glon+gsize*1.1 .and. abs(west2-glon)<d1 )then
          west=glon
          d1=abs(west2-glon)
        endif
        if( east2> glon-gsize*0.1 .and. east2<=glon+gsize*1.1 .and. abs(east2-glon-gsize)<d2 )then
          east=glon+gsize
          d2=abs(east2-glon-gsize)
        endif
      end do

      d1=1.e20
      d2=1.e20
      do iy=1, ny
        glat=lat_ori-dble(iy-1)*gsize
        if( north2> glat-gsize*1.1 .and. north2<=glat+gsize*0.1 .and. abs(north2-glat)<d1 )then
          north=glat
          d1=abs(north2-glat)
        endif
        if( south2>=glat-gsize*1.1 .and. south2< glat+gsize*0.1 .and. abs(south2-glat+gsize)<d2 )then
          south=glat-gsize
          d2=abs(south2-glat+gsize)
        endif
      end do

      !! if differemce ios very small, use the original value
      if( abs(west- west2 )<0.001 ) west =west2
      if( abs(east- east2 )<0.001 ) east =east2
      if( abs(north-north2)<0.001 ) north=north2
      if( abs(south-south2)<0.001 ) south=south2

      print *, 'output: ', west, east, north, south

      mx=nint( dble(east-west)    /dble(gsize) )
      my=nint( dble(north-south)  /dble(gsize) )
      dx=nint( dble(west-lon_ori) /dble(gsize) )
      dy=nint( dble(lat_ori-north)/dble(gsize) )

      cut_param='./dim_change.txt'
      open(11,file=cut_param,form='formatted')
      write(11,'(a)') trim(global_dir)
      write(11,*) nx
      write(11,*) ny
      write(11,*) gsize
      write(11,*) lon_ori
      write(11,*) lon_end
      write(11,*) lat_end
      write(11,*) lat_ori
      write(11,*) '../'
      write(11,*) mx
      write(11,*) my
      write(11,*) dx
      write(11,*) dy
      write(11,*) west
      write(11,*) east
      write(11,*) south
      write(11,*) north
      close(11)

      open(11,file=region_param,form='formatted')
      write(11,'(i12  ,a35)') mx,    'grid number (east-west)'
      write(11,'(i12  ,a35)') my,    'grid number (north-south)'
      write(11,'(i12  ,a35)') nflp,  'floodplain layers'
      write(11,'(f12.8,a35)') gsize, 'grid size  [deg]'
      write(11,'(f12.3,a35)') west,  'west  edge [deg]'
      write(11,'(f12.3,a35)') east,  'east  edge [deg]'
      write(11,'(f12.3,a35)') south, 'south  edge [deg]'
      write(11,'(f12.3,a35)') north, 'north edge [deg]'
      close(11)

      write(6,'(i12  ,a35)') mx,    'grid number (east-west)'
      write(6,'(i12  ,a35)') my,    'grid number (north-south)'
      write(6,'(i12  ,a35)') nflp,  'floodplain layers'
      write(6,'(f12.8,a35)') gsize, 'grid size  [deg]'
      write(6,'(f12.3,a35)') west,  'west  edge [deg]'
      write(6,'(f12.3,a35)') east,  'east  edge [deg]'
      write(6,'(f12.3,a35)') south, 'south edge [deg]'
      write(6,'(f12.3,a35)') north, 'north edge [deg]'

      allocate(nextx(mx,my),nexty(mx,my),downx(mx,my),downy(mx,my))
      allocate(elevtn(mx,my),fldhgt(mx,my,nflp),ctmare(mx,my),grdare(mx,my),uparea(mx,my))
      allocate(lon(mx,my),  lat(mx,my),  nxtdst(mx,my),rivlen(mx,my), width(mx,my))
      allocate(lsmask(mx,my))

      allocate(tmp0(nx,ny), tmp(mx,my))

      fnextxy0=trim(global_dir)//'nextxy.bin'
      fdownxy0=trim(global_dir)//'downxy.bin'
      felevtn0=trim(global_dir)//'elevtn.bin'
      ffldhgt0=trim(global_dir)//'fldhgt.bin'
      fctmare0=trim(global_dir)//'ctmare.bin'
      fgrdare0=trim(global_dir)//'grdare.bin'
      fuparea0=trim(global_dir)//'uparea.bin'
      flonlat0=trim(global_dir)//'lonlat.bin'
      fnxtdst0=trim(global_dir)//'nxtdst.bin'
      frivlen0=trim(global_dir)//'rivlen.bin'
      fwidth0=trim(global_dir)//'width.bin'

print *, 'read global maps'
      print *, trim(fnextxy0)
      open(11,file=fnextxy0,form='unformatted',access='direct',recl=4*nx*ny)
      read(11,rec=1) nextx0
      read(11,rec=2) nexty0
      close(11)

      print *, trim(fdownxy0)
      open(11,file=fdownxy0,form='unformatted',access='direct',recl=4*nx*ny)
      read(11,rec=1) downx0
      read(11,rec=2) downy0
      close(11)


      print *, trim(felevtn0)
      open(11,file=felevtn0,form='unformatted',access='direct',recl=4*nx*ny)
      read(11,rec=1) elevtn0
      close(11)

      print *, trim(ffldhgt0)
      open(11,file=ffldhgt0,form='unformatted',access='direct',recl=4*nx*ny)
      do iflp=1, nflp
        read(11,rec=iflp) tmp0
        fldhgt0(:,:,iflp)=tmp0(:,:)
      end do
      close(11)

      print *, trim(fctmare0)
      open(11,file=fctmare0,form='unformatted',access='direct',recl=4*nx*ny)
      read(11,rec=1) ctmare0
      close(11)

      print *, trim(fgrdare0)
      open(11,file=fgrdare0,form='unformatted',access='direct',recl=4*nx*ny)
      read(11,rec=1) grdare0
      close(11)

      print *, trim(fuparea0)
      open(11,file=fuparea0,form='unformatted',access='direct',recl=4*nx*ny)
      read(11,rec=1) uparea0
      close(11)

      print *, trim(flonlat0)
      open(11,file=flonlat0,form='unformatted',access='direct',recl=4*nx*ny)
      read(11,rec=1) lon0
      read(11,rec=2) lat0
      close(11)

      print *, trim(fnxtdst0)
      open(11,file=fnxtdst0,form='unformatted',access='direct',recl=4*nx*ny)
      read(11,rec=1) nxtdst0
      close(11)

      print *, trim(frivlen0)
      open(11,file=frivlen0,form='unformatted',access='direct',recl=4*nx*ny)
      read(11,rec=1) rivlen0
      close(11)

      print *, trim(fwidth0)
      open(11,file=fwidth0,form='unformatted',access='direct',recl=4*nx*ny)
      read(11,rec=1) width0
      close(11)

print *, 'cut domain'

      nextx(:,:)=-9999
      nexty(:,:)=-9999
      elevtn(:,:)=-9999.
      fldhgt(:,:,:)=-9999.
      ctmare(:,:)=-9999
      grdare(:,:)=-9999
      uparea(:,:)=-9999
      lon(:,:)=-9999
      lat(:,:)=-9999
      nxtdst(:,:)=-9999
      rivlen(:,:)=-9999
      width(:,:)=-9999
      lsmask(:,:)=0

      do iy=1, my
        do ix=1, mx
          jx=ix+dx
          jy=iy+dy

          if( nextx0(jx,jy)>0 )then
            nextx(ix,iy)=nextx0(jx,jy)-dx
            nexty(ix,iy)=nexty0(jx,jy)-dy
            downx(ix,iy)=downx0(jx,jy)
            downy(ix,iy)=downy0(jx,jy)
            kx=nextx(ix,iy)
            ky=nexty(ix,iy)
            if( kx<1 .or. kx>mx .or. ky<1 .or. ky>my )then  !! if downstream is outside the domain
              nextx(ix,iy)=-10
              nexty(ix,iy)=-10
              downx(ix,iy)=-1000
              downy(ix,iy)=-1000
            endif
          elseif( nextx0(jx,jy)/=-9999 )then     !! if river mouth
            nextx(ix,iy)=nextx0(jx,jy)
            nexty(ix,iy)=nexty0(jx,jy)
            downx(ix,iy)=downx0(jx,jy)
            downy(ix,iy)=downy0(jx,jy)
          endif

          if( nextx(ix,iy)/=-9999 )then
            elevtn(ix,iy)  =elevtn0(jx,jy)
            fldhgt(ix,iy,:)=fldhgt0(jx,jy,:)
            ctmare(ix,iy)  =ctmare0(jx,jy)
            grdare(ix,iy)  =grdare0(jx,jy)
            uparea(ix,iy)  =uparea0(jx,jy)
            lon(ix,iy)     =lon0(jx,jy)
            lat(ix,iy)     =lat0(jx,jy)
            nxtdst(ix,iy)  =nxtdst0(jx,jy)
            rivlen(ix,iy)  =rivlen0(jx,jy)
            width(ix,iy)  =width0(jx,jy)
            lsmask(ix,iy) =1
          endif
        end do
      end do

print *, 'write reagional maps'

      fnextxy='../nextxy.bin'
      fdownxy='../downxy.bin'
      felevtn='../elevtn.bin'
      ffldhgt='../fldhgt.bin'
      fctmare='../ctmare.bin'
      fgrdare='../grdare.bin'
      fuparea='../uparea.bin'
      flonlat='../lonlat.bin'
      fnxtdst='../nxtdst.bin'
      frivlen='../rivlen.bin'
      fwidth ='../width.bin'
      flsmask='../lsmask.bin'

      print *, trim(fnextxy)
      open(21,file=fnextxy,form='unformatted',access='direct',recl=4*mx*my)
      write(21,rec=1) nextx
      write(21,rec=2) nexty
      close(21)

      print *, trim(fdownxy)
      open(21,file=fdownxy,form='unformatted',access='direct',recl=4*mx*my)
      write(21,rec=1) downx
      write(21,rec=2) downy
      close(21)

      print *, trim(felevtn)
      open(21,file=felevtn,form='unformatted',access='direct',recl=4*mx*my)
      write(21,rec=1) elevtn
      close(21)

      print *, trim(ffldhgt)
      open(21,file=ffldhgt,form='unformatted',access='direct',recl=4*mx*my)
      do iflp=1, nflp
        tmp(:,:)=fldhgt(:,:,iflp)
        write(21,rec=iflp) tmp
      end do
      close(21)

      print *, trim(fctmare)
      open(21,file=fctmare,form='unformatted',access='direct',recl=4*mx*my)
      write(21,rec=1) ctmare
      close(21)

      print *, trim(fgrdare)
      open(21,file=fgrdare,form='unformatted',access='direct',recl=4*mx*my)
      write(21,rec=1) grdare
      close(21)

      print *, trim(fuparea)
      open(21,file=fuparea,form='unformatted',access='direct',recl=4*mx*my)
      write(21,rec=1) uparea
      close(21)

      print *, trim(flonlat)
      open(21,file=flonlat,form='unformatted',access='direct',recl=4*mx*my)
      write(21,rec=1) lon
      write(21,rec=2) lat
      close(21)

      print *, trim(fnxtdst)
      open(21,file=fnxtdst,form='unformatted',access='direct',recl=4*mx*my)
      write(21,rec=1) nxtdst
      close(21)

      print *, trim(frivlen)
      open(21,file=frivlen,form='unformatted',access='direct',recl=4*mx*my)
      write(21,rec=1) rivlen
      close(21)

      print *, trim(fwidth)
      open(21,file=fwidth,form='unformatted',access='direct',recl=4*mx*my)
      write(21,rec=1) width
      close(21)

      print *, trim(flsmask)
      open(21,file=flsmask,form='unformatted',access='direct',recl=4*mx*my)
      write(21,rec=1) lsmask
      close(21)

! mask out-of-domain rivers
      allocate(check(mx,my))

      check(:,:)=-9
      do iy=1, my
        do ix=1, mx
          if( nextx(ix,iy)/=-9999 ) check(ix,iy)=0
          if( nextx(ix,iy)==-9 .or. nextx(ix,iy)==-10 ) check(ix,iy)=10
        end do
      end do

      do iy=1, my
        do ix=1, mx
          if( nextx(ix,iy)/=-9999 )then
            if( ix<=2 .or. ix>=mx-1 .or. iy<=2 .or. iy>=my-1 )then
              jx=ix
              jy=iy
              do while( check(jx,jy)==0 .and. nextx(jx,jy)>0 )
                check(jx,jy)=20
                kx=nextx(jx,jy)
                ky=nexty(jx,jy)
                jx=kx
                jy=ky
              end do
              check(jx,jy)=20
            endif
          endif
        end do
      end do

      do iy=1, my
        do ix=1, mx
          if( nextx(ix,iy)>0 .and. check(ix,iy)==0 )then
            jx=ix
            jy=iy
            do while( check(jx,jy)==0 )
              kx=nextx(jx,jy)
              ky=nexty(jx,jy)
              jx=kx
              jy=ky
            end do
            icheck=check(jx,jy)

            jx=ix
            jy=iy
            do while( check(jx,jy)==0 )
              check(jx,jy)=icheck
              kx=nextx(jx,jy)
              ky=nexty(jx,jy)
              jx=kx
              jy=ky
            end do
            icheck=check(jx,jy)
          endif
        end do
      end do

      do iy=1, my
        do ix=1, mx
          if( check(ix,iy)==20 )then
            nextx(ix,iy)=-9999
            nexty(ix,iy)=-9999
          endif
        end do
      end do

      fnextxy=trim(region_dir)//'nextxy_noedge.bin'
      print *, trim(fnextxy)
      open(21,file=fnextxy,form='unformatted',access='direct',recl=4*mx*my)
      write(21,rec=1) nextx
      write(21,rec=2) nexty
      close(21)

! ==================================================
      end program cut_domain
