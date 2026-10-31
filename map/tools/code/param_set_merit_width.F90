      program param_set_merit_width
! ================================================
! fuse power-law width (rivwth.bin) and MERIT Hydro width (width.bin)
! to create the alternate width product (rivwth_merit.bin)
! Processing outline:
!   1. Read the map dimensions from DIMINFO and topology from NEXTXY.
!   2. Read power-law river widths (RIVWTH) and MERIT Hydro widths (WIDTH).
!   3. On river cells, retain MERIT widths with existing lower/upper
!      adjustments relative to the power-law width; mark other cells missing.
!   4. Write the result as a single-record float32 map in RIVWTH_MERIT.
! Run from the map directory; all filenames except DIMINFO are fixed below.
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
      real,allocatable    ::  meritwth(:,:)      !! MERIT Hydro channel width [m]
! file
      character*256       ::  diminfo
!
      character*256       ::  cnextxy, crivwth, cwidth, cmerit
      parameter              (crivwth='./rivwth.bin')
      parameter              (cwidth='./width.bin')
      parameter              (cmerit='./rivwth_merit.bin')

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
      allocate(rivwth(nx,ny),meritwth(nx,ny))
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
      read(13,rec=1) meritwth
      close(13)


! Apply the inherited width-merging rules to river cells:
!   - MERIT width below 50 m: use the larger of MERIT and power-law width.
!   - Otherwise, constrain it to 0.5-5 times the power-law width and cap at
!     10,000 m. Non-river cells receive the -9999 missing-value sentinel.
! These thresholds are part of the existing algorithm; this routine does
! not recalibrate channel-height parameters.
! ********************************************************************
      do iy=1, ny
        do ix=1, nx
          if( nextx(ix,iy)/=imis )then
            if( meritwth(ix,iy)<50 )then
              meritwth(ix,iy)=max(meritwth(ix,iy),rivwth(ix,iy))
            elseif( meritwth(ix,iy)<rivwth(ix,iy)*0.5 )then   !!! if MERIT width is very small compated to power-law width, modify
              meritwth(ix,iy)=rivwth(ix,iy)*0.5
            else
              if( meritwth(ix,iy)>rivwth(ix,iy)*5.0 )then    !! if MERIT width is very large, mnodify
                meritwth(ix,iy)=rivwth(ix,iy)*5.0
              endif
              if( meritwth(ix,iy)>10000. )then   !! set max value for river width
                meritwth(ix,iy)=10000.
              endif
            endif
          else
            meritwth(ix,iy)=-9999
          endif
        end do
      end do
! ********************************************************************
print *, trim(cmerit)
      open(21,file=cmerit,form='unformatted',access='direct',recl=4*nx*ny)
      write(21,rec=1) meritwth
      close(21)


      end program param_set_merit_width


