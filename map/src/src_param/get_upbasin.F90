      program calc_rivwth
! ================================================
      implicit none
! ===================
! calculation type
      character*256       ::  buf
! river network map parameters
      integer             ::  ix, iy, jx, jy, kx, ky
      integer             ::  nx, ny                  !! river map grid number
! river netwrok map
      integer,allocatable ::  nextx(:,:)      !! downstream x
      integer,allocatable ::  nexty(:,:)      !! downstream y
      integer,allocatable ::  upgrid(:,:)    
      real,allocatable    ::  uparea(:,:)

      integer,allocatable ::  basin(:,:)      !! downstream y


      integer             ::  ix0, iy0, ibasin


      real                ::  west, east, north, south
      real                ::  gsize

! file
      character*256       ::  finp, fparam, fout
      integer             ::  ios

! ================================================
! read parameters from arguments
      print *, '######################################################'
      print *, 'USAGE: Input IX, IY, OutFile :   ./get_upbasin $IX  $IY $OUTFILE'
      print *, '    (Use Fortran IX,IY coordinate)'
      print *, 'OUTPUT: upbasin.bin (upstream basin of [IX,IY])'
      print *, '######################################################'

      call getarg(1,buf)
      read(buf,*) ix0
      call getarg(2,buf)
      read(buf,*) iy0
      call getarg(3,fout)
      if( trim(fout)=='' ) fout='../upbasin.bin'


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

      allocate(nextx(nx,ny), nexty(nx,ny), upgrid(nx,ny),uparea(nx,ny))
      allocate(basin(nx,ny))

! ===================

      finp='../nextxy.bin'
      open(11,file=finp,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
      read(11,rec=1) nextx
      read(11,rec=2) nexty
      close(11)

      finp='../upgrid.bin'
      open(11,file=finp,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
      read(11,rec=1) upgrid
      close(11)

      finp='../uparea.bin'
      open(11,file=finp,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
      read(11,rec=1) uparea
      close(11)

      if( nextx(ix0,iy0)==-9999 )then
        print *, 'error: [ix,iy] is not land'
        stop
      else
        print *, 'Upstream Grid #     of [ix,iy]:', upgrid(ix0,iy0)
        print *, 'Upstream Area [km2] of [ix,iy]:', uparea(ix0,iy0)*1.e-6
      endif

      basin(:,:)=-9999
      do iy=1, ny
        do ix=1, nx
          if( nextx(ix,iy)/=-9999 )then
            if( nextx(ix,iy)<=0 ) basin(ix,iy)=0
          endif
        end do
      end do
      basin(ix0,iy0)=1

      do iy=1, ny
        do ix=1, nx
          if( nextx(ix,iy)/=-9999 .and. basin(ix,iy)==-9999 )then
            jx=ix
            jy=iy
            do while( basin(jx,jy)==-9999 )
              kx=nextx(jx,jy)
              ky=nexty(jx,jy)
              jx=kx
              jy=ky
            end do
            ibasin=basin(jx,jy)

            jx=ix
            jy=iy
            do while( basin(jx,jy)==-9999 )
              basin(jx,jy)=ibasin
              kx=nextx(jx,jy)
              ky=nexty(jx,jy)
              jx=kx
              jy=ky
            end do

          endif
        end do
      end do

  
      open(11,file=fout,form='unformatted',access='direct',recl=4*nx*ny)
      write(11,rec=1) basin
      close(11)

!!================================================

      end program calc_rivwth
