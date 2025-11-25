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
      integer             ::  ilev, nlfp            !! floodplain layer
! river netwrok map
      integer,allocatable ::  nextx(:,:)      !! downstream x
      integer,allocatable ::  nexty(:,:)      !! downstream y
      real,allocatable    ::  lon(:), lat(:)  !! longitude, latitude [deg]
      real                ::  west, east, north, south, gsize
! variable
      real,allocatable    ::  uparea(:,:)     !! drainage area     [m2] -> [km2]
      real,allocatable    ::  outclm(:,:)     !! mean discharge      [m3/s]
      real,allocatable    ::  fldhgt(:,:,:)   !! floodplain elevation profile [m]
      integer,allocatable ::  class(:,:)    !! class: 
      real,allocatable    ::  grdare(:,:)     !! drainage area     [m2] 
      real,allocatable    ::  rivlen(:,:)     !! drainage area     [m2] 

      real,allocatable    ::  levfrc(:,:)     !! unprocted fraction (relative levee distance, 0-1)
      real,allocatable    ::  levhgt(:,:)     !! levee top  height [m] (above river channel elevtn.bin)
      real,allocatable    ::  levbas(:,:)     !! levee base height [m] (above river channel elevtn.bin)
      real,allocatable    ::  levdst(:,:)     !! levee distance from river [m] (assume uniform & one-side cross section)


      real                ::  elv0, elv1, dst0, dst1

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
      allocate(uparea(nx,ny),fldhgt(nx,ny,nlfp),class(nx,ny),outclm(nx,ny))
      allocate(grdare(nx,ny),rivlen(nx,ny))

      allocate(levfrc(nx,ny),levhgt(nx,ny),levbas(nx,ny),levdst(nx,ny))

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

      rfile='./map/uparea.bin'
      open(13,file=rfile,form='unformatted',access='direct',recl=4*nx*ny)
      read(13,rec=1) uparea
      close(13)

      rfile='./map/fldhgt.bin'
      open(13,file=rfile,form='unformatted',access='direct',recl=4*nx*ny)
      do ilev=1, nlfp
        read(13,rec=ilev) fldhgt(:,:,ilev)
      end do
      close(13)

      rfile='./map/class.bin'
      open(13,file=rfile,form='unformatted',access='direct',recl=4*nx*ny)
      read(13,rec=1) class
      close(13)

      rfile='./map/outclm.bin'
      open(13,file=rfile,form='unformatted',access='direct',recl=4*nx*ny)
      read(13,rec=1) outclm
      close(13)

!! to calculate levee distance
      rfile='./map/grdare.bin'
      open(13,file=rfile,form='unformatted',access='direct',recl=4*nx*ny)
      read(13,rec=1) grdare
      close(13)
      rfile='./map/rivlen.bin'
      open(13,file=rfile,form='unformatted',access='direct',recl=4*nx*ny)
      read(13,rec=1) rivlen
      close(13)


      uparea(:,:)=uparea(:,:)*1.e-6

! =============================
print *, 'calc_levparam: width & depth calculation'
      do iy=1, ny
        do ix=1, nx
          if( uparea(ix,iy)>1000 .or. nextx(ix,iy)==-9 )then !! large river or coast grid
            levfrc(ix,iy)=0.05
            levhgt(ix,iy)=log10(outclm(ix,iy))*2
            levhgt(ix,iy)=max(2.0,levhgt(ix,iy))

            if( class(ix,iy)==1 ) then !! no levee over large lake
              if( fldhgt(ix,iy,1)<0.5 ) then
                levfrc(ix,iy)=1.0 !! all area not protected
                levhgt(ix,iy)=0
              endif
            endif

            if( outclm(ix,iy)<1 ) then !! no levee in very dry area
              levfrc(ix,iy)=1.0 !! all area not protected
              levhgt(ix,iy)=0
            endif

          ! no levee
          elseif( uparea(ix,iy)>0 )then
            levfrc(ix,iy)=1.0 !! all area not protected
            levhgt(ix,iy)=0
          else
            levfrc(ix,iy)=-9999
            levhgt(ix,iy)=-9999
          endif
        end do
      end do

print *, 'levee height adjustment considering floodplain profile'
      do iy=1, ny
        do ix=1, nx
          !! [1] levee exist
          if( levhgt(ix,iy)>0 )then
            ilev=int( levfrc(ix,iy)*nlfp ) +1
            !! (1a) levee within floodplain
            if( ilev<=nlfp )then
              if( ilev==1 )then
                elv0=0.
              else
                elv0=fldhgt(ix,iy,ilev-1)
              endif
              elv1=fldhgt(ix,iy,ilev)
              dst0=levfrc(ix,iy)*nlfp - (ilev-1)
              dst1=ilev - levfrc(ix,iy)*nlfp

              levbas(ix,iy)=dst1*elv0 + dst0*elv1
              if( levhgt(ix,iy)<levbas(ix,iy) ) then
                levhgt(ix,iy)=0.0
                levfrc(ix,iy)=1.0
                levbas(ix,iy)=-99
              endif

            !! (1b) levee at the edge of floodplain (ilev>nlfp)
            else
              levbas(ix,iy)=fldhgt(ix,iy,nlfp)
              if( levhgt(ix,iy)<levbas(ix,iy) ) then
                levhgt(ix,iy)=0.0
                levfrc(ix,iy)=1.0
                levbas(ix,iy)=-99
              endif
            endif

          !! [2] no levee
          elseif( nextx(ix,iy)/=-9999 )then
            levbas(ix,iy)=-99  !! no basehgt defined

          !! [3] ocean grid
          else
            levbas(ix,iy)=-9999  !! no basehgt defined
          endif
        end do
      end do

      do iy=1, ny
        do ix=1, nx
          if( levfrc(ix,iy)>=0 )then !! large river or coast grid
            levdst(ix,iy)=grdare(ix,iy)/rivlen(ix,iy)*levfrc(ix,iy)
          elseif( nextx(ix,iy)/=-9999 )then
            levdst(ix,iy)=-99 !! no levee
          else
            levdst(ix,iy)=-9999
          endif
        end do
      end do

! =============================
print *, 'calc_levparam :write levee parameter file'

      wfile='./map/levfrc.bin'
      open(21,file=wfile,form='unformatted',access='direct',recl=4*nx*ny)
      write(21,rec=1) levfrc
      close(21)

      wfile='./map/levhgt.bin'
      open(22,file=wfile,form='unformatted',access='direct',recl=4*nx*ny)
      write(22,rec=1) levhgt
      close(22)

      wfile='./map/levbas.bin'
      open(22,file=wfile,form='unformatted',access='direct',recl=4*nx*ny)
      write(22,rec=1) levbas
      close(22)

      wfile='./map/levdst.bin'
      open(22,file=wfile,form='unformatted',access='direct',recl=4*nx*ny)
      write(22,rec=1) levdst
      close(22)

      end program calc_levparam
