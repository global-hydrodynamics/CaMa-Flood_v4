      program downscale_flddph
! ===============================================
      implicit none
! CaMa-Flood parameters       
      character*256            ::  param                         !! river map parameters
      integer                  ::  iXX, iYY, jXX, jYY
      integer                  ::  nXX, nYY                      !! grid number (river network map)
      integer                  ::  nflp                          !! floodplain layers
      real*8                   ::  gsize                         !! grid size [deg]
      real*8                   ::  west, east, north, south      !! domain (river network map)

! High-res map parameters                                          !! (from location.txt)
      character*256            ::  list_loc

      integer                  ::  i, narea                      !! area ID
      character*256            ::  area                          !! area code
      integer                  ::  ix, iy, jx, jy, kx, ky
      integer                  ::  ix0, iy0
      integer                  ::  nx, ny                        !! grid number (hires data)
      real*8                   ::  csize                         !! size of pixel [deg]
      real*8                   ::  lon_ori                       !! west  edge
      real*8                   ::  lon_end                       !! east  edge
      real*8                   ::  lat_ori                       !! north edge
      real*8                   ::  lat_end                       !! south edge

! output domain
      real*8                   ::  west2, east2, north2, south2      !! output domain 
      integer                  ::  lx, ly, dx, dy

      real*8                   ::  west3, east3, north3, south3      !! output domain with buffer
      integer                  ::  mx, my

! low res map & data
      integer*4,allocatable    ::  nextXX(:,:), nextYY(:,:)      !! downstream (jXX,jYY)
      real,allocatable         ::  uparea(:,:)                   !! drainage area [m2]

      real,allocatable         ::  flddph(:,:)                   !! input flood depth [m] (coarse resolution)

      real,allocatable         ::  maxupa(:,:)                   !! max upstream uparea [m2] to decide mainstem
      real,allocatable         ::  modify(:,:)                   !! tributary modified flag

! high-res map & data
! -- input (lx*ly)
      integer*1,allocatable    ::  flwdir(:,:)                         !! Flow Direction (3sec only)
      integer*2,allocatable    ::  downX_in(:,:),  downY_in(:,:)       !! downstream increment (dx,dy): jx=ix+dx, jy=iy+dy
      integer*2,allocatable    ::  catmXX_in(:,:), catmYY_in(:,:)      !! catchment (iXX,iYY) of pixel (ix,iy)
      real,allocatable         ::  flddif_in(:,:)                      !! height above channel [m]
      real,allocatable         ::  rivwth_in(:,:)                      !! river width (as permanent water mask)
      real,allocatable         ::  lon_in(:),      lat_in(:)
! -- merge for domain with bugger
      integer*2,allocatable    ::  downX(:,:),  downY(:,:)       !! downstream increment (dx,dy): jx=ix+dx, jy=iy+dy
      integer*2,allocatable    ::  catmXX(:,:), catmYY(:,:)      !! catchment (iXX,iYY) of pixel (ix,iy)
      real,allocatable         ::  flddif(:,:)                   !! height above channel [m]
      real,allocatable         ::  rivwth(:,:)                   !! river width (as permanent water mask)
      real,allocatable         ::  lon(:), lat(:)

! calc
      real,allocatable         ::  flood(:,:)                    !! downscaled flood depth [m]
      real,allocatable         ::  check(:,:)                    !! mainstem, tributary, outlet flag
      real,allocatable         ::  output(:,:)                   !! output data (lx,ly)

      real                     ::  val, dph1, dph2, ddph, len    !! for calculation
!
      character*256            ::  mapdir
      parameter                   (mapdir='./map/')              !! map directory (please make a symbolic link)
      character*256            ::  fnextxy
      character*256            ::  fuparea
      character*256            ::  rfile

      character*256            ::  fflddph        !! input flood depth map (low res)
      character*256            ::  fflood         !! output downscaled flood depth map (high res)
      character*256            ::  hires          !! target resolution tag (1min, 15sec, 3sec, etc9)

      integer                  ::  trec           !! rec length
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

      call getarg(5,hires)      !! downscale resolution
      call getarg(6,fflddph)    !! downscale file
      call getarg(7,fflood)     !! output file
      call getarg(8,buf)        !! downscale file record number
      if( buf/='' )then
        read(buf,*) trec
      else
        trec=1
      endif

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

! calculate number of cell size (index2 is downscaling domain, index 3 is domain with 5grid buffer)
      lx=nint( (east2 -west2 )/csize +0.001 )
      ly=nint( (north2-south2)/csize +0.001 )

      west3  = west2  - gsize*5
      east3  = east2  + gsize*5
      south3 = south2 - gsize*5
      north3 = north2 + gsize*5
      mx=nint( (east3 -west3 )/csize +0.001 )   !! downscale domain with 5 grid buffer
      my=nint( (north3-south3)/csize +0.001 )

      dx=int(gsize*5. / csize + 0.01)  !! pixel shift for buffer zone
      dy=int(gsize*5. / csize + 0.01)


      if( mx*my>525000000 )then
        print *, 'downscale domain too large: mx*my*4>integer limit'
        stop
      endif

      print *, 'domain:',     west2, east2, south2, north2, lx, ly
      print *, 'domain+buf:', west3, east3, south3, north3, mx, my

! ==========
! allocate variables
      allocate(nextXX(nXX,nYY),nextYY(nXX,nYY),uparea(nXX,nYY))
      allocate(flddph(nXX,nYY))
      allocate(maxupa(nXX,nYY),modify(nXX,nYY))

      allocate(flood(mx,my),check(mx,my))
      flood(:,:)=-9999
      check(:,:)=0
! ===============================================
! real low-res map
      fnextxy=trim(mapdir)//'nextxy.bin'
      fuparea=trim(mapdir)//'uparea.bin'

      open(11, file=fnextxy, form='unformatted', access='direct', recl=4*nXX*nYY)
      read(11,rec=1) nextXX
      read(11,rec=2) nextYY
      close(11)

      open(11, file=fuparea, form='unformatted', access='direct', recl=4*nXX*nYY)
      read(11,rec=1) uparea
      close(11)
      do iYY=1, nYY
        do iXX=1, nXX
          if( nextXX(iXX,iYY)/=-9999 )then
            uparea(iXX,iYY)=uparea(iXX,iYY)*1.e-6
          endif
        end do
      end do

      !! calculate max upstream grid area to decide mainstem
      maxupa(:,:)=0
      do iYY=1, nYY
        do iXX=1, nXX
          if( nextXX(iXX,iYY)>0 )then
            jXX=nextXX(iXX,iYY)
            jYY=nextYY(iXX,iYY)
            maxupa(jXX,jYY)=max(maxupa(jXX,jYY),uparea(iXX,iYY))
          endif
        end do
      end do

      open(12, file=fflddph, form='unformatted', access='direct', recl=4*nXX*nYY)
      read(12,rec=trec) flddph
      close(12)
      do iYY=1, nYY
        do iXX=1, nXX
          if( flddph(iXX,iYY)==1.e20 )then   !! treat undef value
            flddph(iXX,iYY)=-9999
          endif
        end do
      end do

!====================
! hires files
      allocate(downX(mx,my), downY(mx,my),catmXX(mx,my),catmYY(mx,my),flddif(mx,my),rivwth(mx,my))
      allocate(lon(mx),lat(my))
      downx(:,:)=-9999
      downy(:,:)=-9999
      catmXX(:,:)=-9999
      catmYY(:,:)=-9999
      flddif(:,:)=-9999
      rivwth(:,:)=-9999

      do ix=1, mx
        lon(ix) = west3 + (real(ix)-0.5)*csize
      end do
      do iy=1, my
        lat(iy) = north3 -(real(iy)-0.5)*csize
      end do

!===== open hires files and merge
! read list of high-res files (location.txt)
      list_loc=trim(mapdir)//trim(hires)//'/location.txt'
      open(11,file=list_loc,form='formatted')
      read(11,*) narea
      read(11,*)

! read each high-res file in the list
      do i=1, narea
        read(11,*) buf, area, lon_ori, lon_end, lat_end, lat_ori, nx, ny, csize
        if( lon_end<west3 .or. lon_ori>east3 .or.lat_ori<south3 .or. lat_end>north3 ) cycle !! out of domain

        allocate(downX_in(nx,ny), downY_in(nx,ny), catmXX_in(nx,ny),catmYY_in(nx,ny))
        allocate(flddif_in(nx,ny),rivwth_in(nx,ny))
        allocate(lon_in(nx),lat_in(ny))
  
        rfile=trim(mapdir)//trim(hires)//'/'//trim(area)//'.catmxy.bin'
        print *, rfile
        open(21,file=rfile,form='unformatted',access='direct',recl=2*nx*ny,status='old',iostat=ios)
        if( ios==0 )then
          read(21,rec=1) catmXX_in
          read(21,rec=2) catmYY_in
          close(21)
        else
          print *, '*******************'
          print *, 'no data: ', rfile
          stop
        endif

        rfile=trim(mapdir)//trim(hires)//'/'//trim(area)//'.downxy.bin'
        open(21,file=rfile,form='unformatted',access='direct',recl=2*nx*ny,status='old',iostat=ios)
        if( ios==0 )then
          read(21,rec=1) downx_in
          read(21,rec=2) downy_in
          close(21)
        else   !! use flwdir for glb 3sec and jpn 1sec
          rfile=trim(mapdir)//trim(hires)//'/'//trim(area)//'.flwdir.bin'
          print *, rfile
          open(21,file=rfile,form='unformatted',access='direct',recl=1*nx*ny,status='old',iostat=ios)
          if( ios==0 )then
            allocate(flwdir(nx,ny))    
            read(21,rec=1) flwdir
            close(21)
            ! convert flwdir to downxy
            do iy=1, ny
              do ix=1, nx
                if( flwdir(ix,iy)==1 )then
                  downx_in(ix,iy)=0
                  downy_in(ix,iy)=-1
                elseif( flwdir(ix,iy)==2 )then
                  downx_in(ix,iy)=1
                  downy_in(ix,iy)=-1
                elseif( flwdir(ix,iy)==3 )then
                  downx_in(ix,iy)=1
                  downy_in(ix,iy)=0
                elseif( flwdir(ix,iy)==4 )then
                  downx_in(ix,iy)=1
                  downy_in(ix,iy)=1
                elseif( flwdir(ix,iy)==5 )then
                  downx_in(ix,iy)=0
                  downy_in(ix,iy)=1
                elseif( flwdir(ix,iy)==6 )then
                  downx_in(ix,iy)=-1
                  downy_in(ix,iy)=1
                elseif( flwdir(ix,iy)==7 )then
                  downx_in(ix,iy)=-1
                  downy_in(ix,iy)=0
                elseif( flwdir(ix,iy)==8 )then
                  downx_in(ix,iy)=-1
                  downy_in(ix,iy)=-1
                elseif( flwdir(ix,iy)==0 )then
                  downx_in(ix,iy)=-999
                  downy_in(ix,iy)=-999
                elseif( flwdir(ix,iy)==-1 )then
                  downx_in(ix,iy)=-1000
                  downy_in(ix,iy)=-1000
                else   !! if( flwdir(ix,iy)==-9 )then
                  downx_in(ix,iy)=-9999
                  downy_in(ix,iy)=-9999
                endif
              end do
            end do
            deallocate(flwdir)
          else
            print *, '*******************'
            print *, 'no data: ', rfile
            stop
          endif
        endif
  
        rfile=trim(mapdir)//trim(hires)//'/'//trim(area)//'.flddif.bin'
        open(21,file=rfile,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
        if( ios==0 )then
          read(21,rec=1) flddif_in
          close(21)
        else
          print *, '*******************'
          print *, 'no data: ', rfile
          stop
        endif

        rfile=trim(mapdir)//trim(hires)//'/'//trim(area)//'.rivwth.bin'
        open(21,file=rfile,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
        if( ios==0 )then
          read(21,rec=1) rivwth_in
          close(21)
        else
          print *, '*******************'
          print *, 'no data: ', rfile
          stop
        endif
  
        do ix=1, nx
          lon_in(ix)=lon_ori+(real(ix)-0.5)*csize
          if( lon_in(ix)>=180. ) lon_in(ix)=lon_in(ix)-360.
          if( lon_in(ix)<-180. ) lon_in(ix)=lon_in(ix)+360.
        end do
        do iy=1, ny
          lat_in(iy) =lat_ori-(real(iy)-0.5)*csize
        end do

        !! copy tiled high-res data (lx*ly) to merged map (mx*my)
        do iy=1, ny
          do ix=1, nx
            if( lon_in(ix)>west3 .and. lon_in(ix)<east3 .and. lat_in(iy)>south3 .and. lat_in(iy)<north3 )then
              ix0=int( (lon_in(ix)-west3 )/csize +0.001 )+1
              iy0=int( (north3-lat_in(iy))/csize +0.001 )+1

              downx(ix0,iy0)  = downx_in(ix,iy) 
              downy(ix0,iy0)  = downy_in(ix,iy) 
              catmXX(ix0,iy0) = catmXX_in(ix,iy)
              catmYY(ix0,iy0) = catmYY_in(ix,iy)
              flddif(ix0,iy0) = flddif_in(ix,iy)
              rivwth(ix0,iy0) = rivwth_in(ix,iy)
            endif
          end do
        end do

        deallocate(downx_in,downy_in,catmXX_in,catmYY_in,flddif_in,rivwth_in)
        deallocate(lon_in,lat_in)
      enddo
!! high-res file merging end
!=======================================================
!! downscale flood depth
      ! use flood depth of outlet pixel, and downscale it using floodplain relative height (fldhgt)
      do iy=1, my
        do ix=1, mx
          if( catmXX(ix,iy)>0 )then
            flood(ix,iy)=0
            iXX=catmXX(ix,iy)
            iYY=catmYY(ix,iy)
            if( flddph(iXX,iYY)>flddif(ix,iy) )then
              flood(ix,iy)=flddph(iXX,iYY)-flddif(ix,iy)
            endif
          endif
        end do
      end do

      !! calc mainstem and tributary
      do iy=1, my
        do ix=1, mx
          if( catmXX(ix,iy)>0 )then
            iXX=catmXX(ix,iy)     !! current catchment (iXX,iYY)
            iYY=catmYY(ix,iy)
            if( nextXX(iXX,iYY)<=0 )cycle
            jXX=nextXX(iXX,iYY)   !! downstream catchment (jXX,jYY)
            jYY=nextYY(iXX,iYY)

            if( downx(ix,iy)<-900 ) cycle
            jx=ix+downx(ix,iy)    !! downstream pixel (jx,jy)
            jy=iy+downy(ix,iy)
            if( jx<=0 .or. jx>mx .or. jy<=0 .or. jy>my ) cycle

            if( catmXX(jx,jy)==jXX .and. catmYY(jx,jy)==jYY )then !! if outlet pixel
              check(ix,iy)=6.  !! mark outlet check=6

              val=1            !! mark as tributary check=1
              if( uparea(iXX,iYY)==maxupa(jXX,jYY) ) val=3. !! mark as mainstem check=3

              !! mark downstream mainstem or triburaty
              do while( catmXX(jx,jy)==jXX .and. catmYY(jx,jy)==jYY )
                check(jx,jy)=max(check(jx,jy),val)
                if( downx(jx,jy)<-900 ) exit
                kx=jx+downx(jx,jy)
                ky=jy+downy(jx,jy)
                if( kx<=0 .or. kx>mx .or. ky<=0 .or. ky>my )then !! out of domain
                  check(jx,jy)=-1
                  exit
                endif
                jx=kx
                jy=ky
              end do
            endif
          endif
        end do
      end do

      !! increase flood depth of downstream tributary (if upstream has flooded)
      modify(:,:)=0
      do iy=1, my
        do ix=1, mx
          if( catmXX(ix,iy)>0 )then
            iXX=catmXX(ix,iy)
            iYY=catmYY(ix,iy)
            if( nextXX(iXX,iYY)<=0 )cycle
            jXX=nextXX(iXX,iYY)
            jYY=nextYY(iXX,iYY)
            if( downx(ix,iy)<-900 ) cycle

            jx=ix+downx(ix,iy)
            jy=iy+downy(ix,iy)
            if( jx<=0 .or. jx>mx .or. jy<=0 .or. jy>my ) cycle

            if( catmXX(jx,jy)==jXX .and. catmYY(jx,jy)==jYY )then !! outlet pixel
              if( check(jx,jy)>1 ) cycle !! mainstem=3 do not need treatment
              dph1=flood(ix,iy)          !! depth of upstream outlet
              len=1                      !! length of modifying section

              do while( check(jx,jy)==1 )    !! follow downstream until mainstem
                if( downx(jx,jy)<-900 ) exit
                kx=jx+downx(jx,jy)
                ky=jy+downy(jx,jy)
                jx=kx
                jy=ky
                len=len+1
              end do
              if( check(jx,jy)==-1 ) cycle

              dph2=flood(jx,jy)          !! downstream mainstem depth
              ddph=(dph1-dph2)/len       !! depth decrease considering tributary length
              ddph=max(ddph,0.)          !! (do not increase depth when downstream flddph is larger)

              jx=ix+downx(ix,iy)
              jy=iy+downy(ix,iy)
              len=1
              do while( check(jx,jy)==1 )
                if( dph1-ddph*len > flood(jx,jy))then   !! modify downstream tributary flood depth
                  modify(jXX,jYY)=1
                  flood(jx,jy)=max(flood(jx,jy),dph1-ddph*len)
                endif
                if( downx(jx,jy)<-900 ) exit
                kx=jx+downx(jx,jy)
                ky=jy+downy(jx,jy)
                jx=kx
                jy=ky
                len=len+1
              end do
            endif

          endif
        end do
      end do

      !! consider backwater in downstream tributary
      do iy=1, my
        do ix=1, mx
          if( catmXX(ix,iy)>0 )then
            iXX=catmXX(ix,iy)
            iYY=catmYY(ix,iy)
            if( modify(iXX,iYY)==0 ) cycle     !! calculate only for catchment with flddph modification
            if( check(ix,iy)/=0    ) cycle
            if( downx(ix,iy)<-900  ) cycle

            jx=ix+downx(ix,iy)
            jy=iy+downy(ix,iy)
            if( jx<=0 .or. jx>mx .or. jy<=0 .or. jy>my ) cycle

            do while( check(jx,jy)==0 )      !! check all pixels in the catchment (not yet marked)
              if( downx(jx,jy)<-900 ) exit
              kx=jx+downx(jx,jy)
              ky=jy+downy(jx,jy)
              if( kx<=0 .or. kx>mx .or. ky<=0 .or. ky>my )then !! out of domain
                check(jx,jy)=-1
                exit
              endif
              jx=kx
              jy=ky
            end do
            if( check(jx,jy)==-1 ) exit
            dph1=flddif(jx,jy)+flood(jx,jy)  !! downstream surface elevation

            jx=ix
            jy=iy
            do while( check(jx,jy)==0 )
              if( flddif(jx,jy)+flood(jx,jy)<dph1 )then  !! if local surface elv < downstream surface elv
                flood(jx,jy)=dph1-flddif(jx,jy)          !! increase flood depth of the pixel
              endif
              check(jx,jy)=10
              if( downx(jx,jy)<-900 ) exit
              kx=jx+downx(jx,jy)
              ky=jy+downy(jx,jy)
              if( kx<=0 .or. kx>mx .or. ky<=0 .or. ky>my )then !! out of domain
                check(jx,jy)=-1
                exit
              endif
              jx=kx
              jy=ky
            end do
          endif
        end do
      end do


      !! add permanent water using rivwth file
      do iy=1, my
        do ix=1, mx
          if( catmXX(ix,iy)>0 )then
            if( rivwth(ix,iy)/=-9999 .and. rivwth(ix,iy)/=0 )then !! permanent water
              flood(ix,iy)=max(0.1,flood(ix,iy))
            endif
          endif
        end do
      end do

!==================
! save downscaled flood depth
      allocate(output(lx,ly))
      output(:,:)=flood(dx+1:dx+lx,dx+1:dy+ly)

      print *, fflood
      open(11, file=fflood, form='unformatted', access='direct', recl=4*lx*ly)
      write(11,rec=1) output
      close(11)

      end program downscale_flddph


