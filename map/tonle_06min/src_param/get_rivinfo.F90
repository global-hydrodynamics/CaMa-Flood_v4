      program calc_rivwth
! ================================================
      implicit none
! ===================
! calculation type
      character*256       ::  buf


! river network map parameters
      integer             ::  ix, iy, jx, jy, kx, ky
      integer             ::  nx, ny, mx, my          !! river map grid number
      real                ::  lat, lon
! river netwrok map
      integer,allocatable ::  nextx(:,:)      !! downstream x
      integer,allocatable ::  nexty(:,:)      !! downstream y

      integer,allocatable ::  basin(:,:)      !! 
      integer,allocatable ::  upgrid(:,:)     !! 

      real,allocatable    ::  uparea(:,:)     !! 
      real,allocatable    ::  ctmare(:,:)     !! 
      real,allocatable    ::  elevtn(:,:)     !! 
      real,allocatable    ::  outlon(:,:)     !! 
      real,allocatable    ::  outlat(:,:)     !! 

      integer*2,allocatable:: catmx(:,:), catmy(:,:)

      real                ::  glon, glat


      real                ::  west, east, north, south
      real                ::  gsize, csize

      integer             ::  isHires

! file
      character*256       ::  finp, fparam, floc
      character*256       ::  type
      character*256       ::  tag
      integer             ::  ios

! ================================================
! read parameters from arguments
      call getarg(1,type)
      if( trim(type)=='xy' )then
        call getarg(2,buf)
        read(buf,*) ix
        call getarg(3,buf)
        read(buf,*) iy
      elseif( trim(type)=='latlon' )then
        call getarg(2,buf)
        read(buf,*) lat
        call getarg(3,buf)
        read(buf,*) lon
      else
        print *, '######################################################'
        print *, 'USAGE(1) Input IX  and IY :   ./get_rivinfo xy $IX  $IY    '
        print *, '    (Use Fortran IX,IY coordinate)'
        print *, ''
        print *, 'USAGE(2) Input lat and lon:   ./get_rivinfo latlon $LAT $LON'
        print *, '######################################################'
        stop
      endif

      fparam='../params.txt'
      open(11,file=fparam,form='formatted')
      read(11,*) nx
      read(11,*) ny
      read(11,*) 
      read(11,*) gsize
      read(11,*) west
      read(11,*) east
      read(11,*) south
      read(11,*) north
      close(11)

      print *, 'Map Domain W-E-S-N:', west, east, south, north
      print *, 'Map Resolution: ', gsize
      print *, 'Map NX,NY: ',  nx, ny

! ==============================

      allocate(nextx(nx,ny), nexty(nx,ny))
      allocate(basin(nx,ny), upgrid(nx,ny),uparea(nx,ny),ctmare(nx,ny),elevtn(nx,ny))
      allocate(outlon(nx,ny),outlat(nx,ny))

! ===================

      finp='../nextxy.bin'
      open(11,file=finp,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
      read(11,rec=1) nextx
      read(11,rec=2) nexty
      close(11)

      finp='../basin.bin'
      open(11,file=finp,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
      read(11,rec=1) basin
      close(11)

      finp='../upgrid.bin'
      open(11,file=finp,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
      read(11,rec=1) upgrid
      close(11)

      finp='../uparea.bin'
      open(11,file=finp,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
      read(11,rec=1) uparea
      close(11)

      finp='../ctmare.bin'
      open(11,file=finp,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
      read(11,rec=1) ctmare
      close(11)

      finp='../elevtn.bin'
      open(11,file=finp,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
      read(11,rec=1) elevtn
      close(11)

      finp='../lonlat.bin'
      open(11,file=finp,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
      read(11,rec=1) outlon
      read(11,rec=2) outlat
      close(11)

      if( trim(type)=='latlon' )then

        isHires=0

        !! check 15sec hires map availability
        floc='../15sec/location.txt'
        open(11,file=floc,form='formatted',status='old',iostat=ios)
        if( ios==0 )then
          read(11,*)
          read(11,*)
          read(11,*) buf, tag, buf, buf, buf, buf, mx, my, csize
          close(11)
          if( trim(tag)=='15sec' )then
            print *, 'USE 15sec hires map'
            isHires=15

            allocate(catmx(mx,my),catmy(mx,my))

            finp='../15sec/15sec.catmxy.bin'
            open(11,file=finp,form='unformatted',access='direct',recl=2*mx*my,status='old',iostat=ios)
            read(11,rec=1) catmx
            read(11,rec=2) catmy
            close(11)

            jx=int( (lon-west) /csize )+1
            jy=int( (north-lat)/csize )+1
            if( jx<=0 .or. jx>mx .or. jy<=0 .or. jy>my )then
              print *, 'ix,iy cannot be defined'
              stop
            endif
            ix=catmx(jx,jy)
            iy=catmy(jx,jy)
            if( ix<=0 .or. ix>nx .or. ix<=0 .or. iy>ny )then
              print *, 'ix,iy cannot be defined'
              stop
            endif
          endif
        endif

        if( isHires/=15 )then
        !! check 1min hires map availability
          floc='../1min/location.txt'
          open(11,file=floc,form='formatted',status='old',iostat=ios)
          if( ios==0 )then
            read(11,*)
            read(11,*)
            read(11,*) buf, tag, buf, buf, buf, buf, mx, my, csize
            close(11)
            if( trim(tag)=='1min' )then
              print *, 'USE 1min hires map'
              isHires=60
  
              allocate(catmx(mx,my),catmy(mx,my))
  
              finp='../1min/1min.catmxy.bin'
              open(11,file=finp,form='unformatted',access='direct',recl=2*mx*my,status='old',iostat=ios)
              read(11,rec=1) catmx
              read(11,rec=2) catmy
              close(11)
  
              jx=int( (lon-west) /csize )+1
              jy=int( (north-lat)/csize )+1
              if( jx<=0 .or. jx>mx .or. jy<=0 .or. jy>my )then
                print *, 'ix,iy cannot be defined'
                stop
              endif
              ix=catmx(jx,jy)
              iy=catmy(jx,jy)
              if( ix<=0 .or. ix>nx .or. ix<=0 .or. iy>ny )then
                print *, 'ix,iy cannot be defined'
                stop
              endif
            endif
          endif
        endif

        if( isHires==0 )then
          ix=int( (lon-west) /gsize )+1
          iy=int( (north-lat)/gsize )+1
        endif
      endif

      if( trim(type)=='xy' )then
        lon=west +gsize*(ix-0.5)
        lat=north-gsize*(iy-0.5)
        print *, 'LAT LON: ', lat, lon
      endif

      glon=west +gsize*(ix-0.5)
      glat=north-gsize*(iy-0.5)

      if( nextx(ix,iy)==-9999 )then
        print *, 'NOT LAND GRID'
        stop
      endif

      print *, '######################################################'
      print *, ''
      print *, 'Point Information'

      print *, 'Fortran IX,IY: ', ix,  iy
      print *, 'Python  IX,IY: ', ix-1,  iy-1
      print *, ''

      print *, 'Lat, Lon  (Point):            ', lat, lon
      print *, 'Lat, Lon  (Grid Center):      ', glat, glon
      print *, 'Lat, Lon  (Catchment Outler): ', outlat(ix,iy), outlon(ix,iy)
      print *, ''

      print *, 'River Basin ID:      ', basin(ix,iy)
      print *, 'Drainage Area [km2]: ',  uparea(ix,iy)*1.e-6
      print *, 'Upstream Grids:      ', upgrid(ix,iy)
      print *, ''

      print *, 'Elevation (Catchment Outler) [m]: ', elevtn(ix,iy)
      print *, 'Unit Catchment Area [km2]:        ', ctmare(ix,iy)*1.e-6
      print *, ''

      if( nextx(ix,iy)<0 .and. nextx(ix,iy)/=-9999 )then
        print *, 'RIVER MOUTH GRID'
      else
        jx=nextx(ix,iy)
        jy=nexty(ix,iy)
        print *, 'Downstream IX,IY,AREA: ', jx, jy, uparea(jx,jy)*1.e-6
      endif

      do jy=1, ny
        do jx=1, nx
          if( nextx(jx,jy)>0 )then
            kx=nextx(jx,jy)
            ky=nexty(jx,jy)
            if( ix==kx .and. iy==ky)then
              print *, 'Upstream   IX,IY,AREA: ', jx, jy, uparea(jx,jy)*1.e-6
            endif
          endif
        end do
      end do
  

!!================================================

      end program calc_rivwth
