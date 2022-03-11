      program calc_levparam
! ================================================
! calculate test levee parameters
! ================================================
      implicit none
! ===================
! calculation type
      character*256       ::  buf             !! dimention info file

      character*256       ::  params             !! dimention info file
      data                    params /'./map/params.txt'/

! river network map parameters
      integer             ::  ix, iy
      integer             ::  nx, ny          !! river map grid number
      integer             ::  nlfp            !! floodplain layer
! river netwrok map
      integer,allocatable ::  nextx(:,:)      !! downstream x
      integer,allocatable ::  nexty(:,:)      !! downstream y
      real,allocatable    ::  lon(:), lat(:)  !! longitude, latitude [deg]
      real                ::  west, east, north, south, gsize
! variable
      real,allocatable    ::  outflw(:,:)     !! discharge      [m3/s]
      real,allocatable    ::  rivdph(:,:)     !! river channel depth [m]
      real,allocatable    ::  fldfrc(:,:)     !! flood fraction [0-1]
      real,allocatable    ::  fldare(:,:)     !! flood fraction [m2]
      real,allocatable    ::  flddph(:,:)     !! unprotected depth
      real,allocatable    ::  levdph(:,:)     !! protected depth

      real,allocatable    ::  outflw0(:,:)     !! discharge      [m3/s]
      real,allocatable    ::  rivdph0(:,:)     !! river channel depth [m]
      real,allocatable    ::  fldfrc0(:,:)     !! flood fraction [0-1]
      real,allocatable    ::  fldare0(:,:)     !! flood area [m2]
      real,allocatable    ::  flddph0(:,:)     !! unprotected depth

      real,allocatable    ::  rivd_max(:,:)     !! river channel depth [m]
      real,allocatable    ::  rivd_ave(:,:)     !! river channel depth [m]
      real,allocatable    ::  rivd_max0(:,:)     !! river channel depth [m]
      real,allocatable    ::  rivd_ave0(:,:)     !! river channel depth [m]

      real,allocatable    ::  ffrc_max(:,:)     !! flood fraction [m]
      real,allocatable    ::  ffrc_ave(:,:)     !! flood fraction [m]
      real,allocatable    ::  ffrc_max0(:,:)     !! flood fraction  [m]
      real,allocatable    ::  ffrc_ave0(:,:)     !! flood fraction  [m]

      real,allocatable    ::  fdph_m(:,:)     !! flood depth   [m]
      real,allocatable    ::  ldph_m(:,:)     !! protectdd depth  [m]
      real,allocatable    ::  fdph_m0(:,:)     !! flood depth   [m]
      real,allocatable    ::  ffrc_m(:,:)     !! flood depth   [m]
      real,allocatable    ::  ffrc_m0(:,:)     !! flood depth   [m]


      integer             ::  iday, nday
      integer             ::  idate
      real                ::  fare, fare0, diff_max
! file
      character*256       ::  rfile, wfile
      integer             ::  ios
! Undefined Values
      integer             ::  imis                !! integer undefined value
      real                ::  rmis                !! real    undefined value
      parameter              (imis = -9999)
      parameter              (rmis = 1.e+20)
! ================================================
print *, 'calc_levparam :read parameters from arguments'

      open(11,file=params,form='formatted')
      read(11,*) nx
      read(11,*) ny
      read(11,*) nlfp
      read(11,*) gsize
      read(11,*) west
      read(11,*) east
      read(11,*) south
      read(11,*) north
      close(11)

! ==============================
      print *, nx, ny, nlfp

      allocate(nextx(nx,ny),nexty(nx,ny))
      allocate(outflw(nx,ny), rivdph(nx,ny), fldfrc(nx,ny), fldare(nx,ny), flddph(nx,ny),levdph(nx,ny))
      allocate(outflw0(nx,ny),rivdph0(nx,ny),fldfrc0(nx,ny),fldare0(nx,ny),flddph0(nx,ny))

      allocate(rivd_max(nx,ny), rivd_ave(nx,ny))
      allocate(rivd_max0(nx,ny),rivd_ave0(nx,ny))

      allocate(ffrc_max(nx,ny), ffrc_ave(nx,ny))
      allocate(ffrc_max0(nx,ny),ffrc_ave0(nx,ny))

      allocate(fdph_m(nx,ny),ldph_m(nx,ny),fdph_m0(nx,ny))


      allocate(lon(nx),lat(ny)) 
      gsize=(east-west)/real(nx)
      do ix=1,nx
        lon(ix)=west+(real(ix)-0.5)*gsize
      enddo
      do iy=1,ny
        lat(iy)=north-(real(iy)-0.5)*gsize
      enddo

! ===================
print *, 'calc_levparam: read nextxy.bin'
      rfile='./map/nextxy.bin'
      open(11,file=rfile,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
      read(11,rec=1) nextx
      read(11,rec=2) nexty
      close(11)

! =============================
print *, 'read daily data'

      rivd_max(:,:)=0
      rivd_ave(:,:)=0
      rivd_max0(:,:)=0
      rivd_ave0(:,:)=0

      diff_max=0
      nday=365
      do iday=1, 365
!!  print *, iday
        rfile='./out_lev/rivdph2001.bin'
        open(11,file=rfile,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
        read(11,rec=iday) rivdph
        close(11)

        rfile='./out_ori/rivdph2001.bin'
        open(11,file=rfile,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
        read(11,rec=iday) rivdph0
        close(11)

        rfile='./out_lev/fldfrc2001.bin'
        open(11,file=rfile,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
        read(11,rec=iday) fldfrc
        close(11)

        rfile='./out_ori/fldfrc2001.bin'
        open(11,file=rfile,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
        read(11,rec=iday) fldfrc0
        close(11)

!!
        rfile='./out_lev/fldare2001.bin'
        open(11,file=rfile,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
        read(11,rec=iday) fldare
        close(11)

        rfile='./out_ori/fldare2001.bin'
        open(11,file=rfile,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
        read(11,rec=iday) fldare0
        close(11)

!====

        rfile='./out_lev/flddph2001.bin'
        open(11,file=rfile,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
        read(11,rec=iday) flddph
        close(11)

        rfile='./out_ori/flddph2001.bin'
        open(11,file=rfile,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
        read(11,rec=iday) flddph0
        close(11)

        rfile='./out_lev/levdph2001.bin'
        open(11,file=rfile,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
        read(11,rec=iday) levdph
        close(11)

        rivd_max =max(rivd_max, rivdph)
        rivd_max0=max(rivd_max0,rivdph0)
        rivd_ave =rivd_ave +rivdph
        rivd_ave0=rivd_ave0+rivdph0

        ffrc_max =max(ffrc_max, fldfrc)
        ffrc_max0=max(ffrc_max0,fldfrc0)
        ffrc_ave =ffrc_ave +fldfrc
        ffrc_ave0=ffrc_ave0+fldfrc0

        fare=0
        fare0=0
        do iy=1, ny
          do ix=1, nx
            if( lon(ix)>-93 .and. lon(ix)<-89 .and. lat(iy)>29 .and. lat(iy)<35 )then
              if( nextx(ix,iy)/=-9999 )then
                fare =fare +fldare(ix,iy)
                fare0=fare0+fldare0(ix,iy)
              endif
            endif
          end do
        end do

print *, iday, fare, fare0, diff_max

        if( fare0-fare > diff_max )then
          diff_max=fare0-fare
          idate=iday
          fdph_m =flddph
          ldph_m =levdph
          fdph_m0=flddph0
          ffrc_m =fldfrc
          ffrc_m0=fldfrc0
        endif

      end do

      rivd_ave =rivd_ave  / real(nday)
      rivd_ave0=rivd_ave0 / real(nday)

      ffrc_ave =ffrc_ave  / real(nday)
      ffrc_ave0=ffrc_ave0 / real(nday)

      print *, idate

      wfile='./data/analysis.bin'
      open(11,file=wfile,form='unformatted',access='direct',recl=4*nx*ny)
      write(11,rec=1) rivd_max
      write(11,rec=2) rivd_max0
      write(11,rec=3) rivd_ave
      write(11,rec=4) rivd_ave0

      write(11,rec=5) ffrc_max
      write(11,rec=6) ffrc_max0
      write(11,rec=7) ffrc_ave
      write(11,rec=8) ffrc_ave0
      close(11)


      wfile='./data/missi.bin'
      open(11,file=wfile,form='unformatted',access='direct',recl=4*nx*ny)
      write(11,rec=1) fdph_m
      write(11,rec=2) ldph_m
      write(11,rec=3) fdph_m0
      write(11,rec=4) ffrc_m
      write(11,rec=5) ffrc_m0
      close(11)

      end program calc_levparam
