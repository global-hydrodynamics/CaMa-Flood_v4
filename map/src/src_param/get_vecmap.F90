      program calc_rivwth
! ================================================
      implicit none
! ===================
! calculation type
      character*256       ::  buf
      real                ::  west0, east0, south0, north0

! river network map parameters
      integer             ::  ix, iy, jx, jy
      integer             ::  nx, ny          !! river map grid number
! river netwrok map
      integer,allocatable ::  nextx(:,:)      !! downstream x
      integer,allocatable ::  nexty(:,:)      !! downstream y

      real,allocatable    ::  uparea(:,:)     !! 
      real,allocatable    ::  elevtn(:,:)     !! 
      real,allocatable    ::  outlon(:,:)     !! 
      real,allocatable    ::  outlat(:,:)     !! 
      real,allocatable    ::  glon(:), glat(:)

      real                ::  west, east, north, south
      real                ::  gsize

      real                ::  olon, olat, dlon, dlat
      integer             ::  isDomain

! file
      character*256       ::  finp, fparam
      integer             ::  ios

! ================================================
! read parameters from arguments
      call getarg(1,buf)
      if( trim(buf)=='' )then
        print *, '######################################################'
        print *, 'USAGE(1) Input WEST, EAST, SOUTH, NORTH:   ./get_vecmap $WEST $EAST $SOUTH $NORTH '
        print *, '######################################################'
        stop
      endif

      call getarg(1,buf)
       read(buf,*) west0
      call getarg(2,buf)
       read(buf,*) east0
      call getarg(3,buf)
       read(buf,*) south0
      call getarg(4,buf)
       read(buf,*) north0

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
! ==============================

      allocate(nextx(nx,ny), nexty(nx,ny))
      allocate(uparea(nx,ny),elevtn(nx,ny))
      allocate(outlon(nx,ny),outlat(nx,ny))
      allocate(glon(nx), glat(ny))

! ===================

      finp='../nextxy.bin'
      open(11,file=finp,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
      read(11,rec=1) nextx
      read(11,rec=2) nexty
      close(11)

      finp='../uparea.bin'
      open(11,file=finp,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
      read(11,rec=1) uparea
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

      do ix=1, nx
        glon(ix)=west +gsize*(ix-0.5)
      end do
      do iy=1, ny
        glat(iy)=north-gsize*(iy-0.5)
      end do

!===========
      print *, 'ix(Fort), iy(Fort),   ori-lat, ori-lon,   dst-lat, dst-lon,   uparea,   elevtn'

      do iy=1, ny
        do ix=1, nx
          if( nextx(ix,iy)>0 )then
            jx=nextx(ix,iy)
            jy=nexty(ix,iy)
            olon=outlon(ix,iy)
            olat=outlat(ix,iy)
            dlon=outlon(jx,jy)
            dlat=outlat(jx,jy)

            isDomain=0
            if( olon>=west0 .and. olon<=east0 .and. olat>=south0 .and. olat<=north0 ) isDomain=1
            if( dlon>=west0 .and. dlon<=east0 .and. dlat>=south0 .and. dlat<=north0 ) isDomain=1

            if( isDomain==1 )then
              print '(2i8,2f12.3,2x,2f12.3,f12.1,f8.1)', ix, iy, &
                olat, olon, dlat, dlon, uparea(ix,iy)*1.e-6, elevtn(ix,iy)
            endif

          elseif( nextx(ix,iy)==-9 .or. nextx(ix,iy)==-10 )then
            olon=outlon(ix,iy)
            olat=outlat(ix,iy)
            dlon=-999
            dlat=-999
            isDomain=0
            if( olon>=west0 .and. olon<=east0 .and. olat>=south0 .and. olat<=north0 ) isDomain=1

            if( isDomain==1 )then
              print '(2i8,2f12.3,2x,2f12.3,f12.1,f8.1)', ix, iy, &
                olat, olon, dlat, dlon, uparea(ix,iy)*1.e-6, elevtn(ix,iy)
            endif
          endif
        end do
      end do

!!================================================

      end program calc_rivwth
