      program calc_rivwth
! ================================================
! fuse power-law width (rivwth.bin) and satellite width (width.bin)
! to create width parameter data for simulation (rivwth_gwdlr.bin)
! ================================================
      implicit none
! ===================
! river network map parameters
      integer             ::  ix, iy
      integer             ::  nx, ny          !! river map grid number
! river netwrok map
      integer,allocatable ::  nextx(:,:)      !! downstream x
      integer,allocatable ::  nexty(:,:)      !! downstream y
      real,allocatable    ::  lon(:), lat(:)  !! longitude, latitude [deg]

! variable
      real,allocatable    ::  rivwth(:,:)     !! channel width (empirical) [m]
      real,allocatable    ::  gwdlr(:,:)      !! channel width (GWD-LR)    [m]
! file
      character*256       ::  diminfo
!
      character*256       ::  cnextxy, crivwth, cwidth, cgwdlr
      parameter              (crivwth='./rivwth.bin')
      parameter              (cwidth='./width.bin')
      parameter              (cgwdlr='./rivwth_gwdlr.bin')

      integer             ::  ios
! Undefined Values
      integer             ::  imis                !! integer undefined value
      real                ::  rmis                !! real    undefined value
      parameter              (imis = -9999)
      parameter              (rmis = 1.e+20)
! ================================================
      call getarg(1,diminfo)

      open(11,file=diminfo,form='formatted')
      read(11,*    ) nx
      read(11,*    ) ny
print *, nx, ny

      allocate(nextx(nx,ny),nexty(nx,ny))
      allocate(rivwth(nx,ny),gwdlr(nx,ny))
      allocate(lon(nx),lat(ny))

      cnextxy='./nextxy.bin'
      open(11,file=cnextxy,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
      read(11,rec=1) nextx
      read(11,rec=2) nexty
      close(11)

print *, trim(crivwth)
      open(13,file=crivwth,form='unformatted',access='direct',recl=4*nx*ny)
      read(13,rec=1) rivwth
      close(13)

print *, trim(cwidth)
      open(13,file=cwidth,form='unformatted',access='direct',recl=4*nx*ny)
      read(13,rec=1) gwdlr
      close(13)


! ********************************************************************
! please modify this part to calibrate river width and height parameters
! ********************************************************************
      do iy=1, ny
        do ix=1, nx
          if( nextx(ix,iy)/=imis )then
            if( gwdlr(ix,iy)<50 )then
              gwdlr(ix,iy)=max(gwdlr(ix,iy),rivwth(ix,iy))
            elseif( gwdlr(ix,iy)<rivwth(ix,iy)*0.5 )then   !!! if satellite width is very small compated to power-law width, modify
              gwdlr(ix,iy)=rivwth(ix,iy)*0.5
            else
              if( gwdlr(ix,iy)>rivwth(ix,iy)*5.0 )then    !! if satellite width is very large, mnodify
                gwdlr(ix,iy)=rivwth(ix,iy)*5.0
              endif
              if( gwdlr(ix,iy)>10000. )then   !! set max value for river width
                gwdlr(ix,iy)=10000.
              endif
            endif
          else
            gwdlr(ix,iy)=-9999
          endif
        end do
      end do
! ********************************************************************
print *, trim(cgwdlr)
      open(21,file=cgwdlr,form='unformatted',access='direct',recl=4*nx*ny)
      write(21,rec=1) gwdlr
      close(21)


      end program calc_rivwth


