!===========================
! Code to allocate GRanD reservoir on CaMa-Flood river map
! -- input:  GRanD reservoir list      (inp/damlist.csv)
! -- output: tentative allocation file (filename = aug1)
! 
! written by Risa Hanazaki, 2021Mar, Modified by D.Yamazaki 2022 Mar
!===================================================================
program main
  implicit none
  integer                ::  ndams     !! number of dams in GRanD data
  integer                ::  idam
! Dam Parameter
  integer                ::  grandid   !! GRanD ID
  character*256          ::  damname   !! Dam Name
  real                   ::  upreal    !! upstream area from GRanD
  real                   ::  totalsto  !! total storage from GRanD
  real                   ::  damlat, damlon  !! dam location from GRanD

! CaMa map parameters
  integer                ::  nx, ny
  real                   ::  gsize, west, east, south, north
  integer                ::  isGlobal

  integer                ::  ix, iy, jx, jy, dx, dy
  integer                ::  NN        !! buffer for search dam location

! File I/O
  character*256          ::  grandf, mapdir, outf, fparam, floc, finp
  character*256          ::  fmt, buf
  integer                ::  ios

! CaMa-Flood Map Data
  integer,allocatable    ::  nextx(:,:)      !! downstream x
  integer,allocatable    ::  nexty(:,:)      !! downstream y
  integer,allocatable    ::  basin(:,:)      !! basin ID
  integer,allocatable    ::  upgrid(:,:)     !! upstream grid number
  real,allocatable       ::  uparea(:,:)     !! upstream area
  real,allocatable       ::  ctmare(:,:)     !! unit-catchment area
  real,allocatable       ::  elevtn(:,:)     !! elevation
  real,allocatable       ::  outlon(:,:)     !! outlet-pixel lon
  real,allocatable       ::  outlat(:,:)     !! outlet pixel lat

! CaMa-Flood high-res map
  integer                ::  MMX, MMY          !! hires pixel size
  integer*2,allocatable  ::  catmx(:,:)      !! unit-catchment IX
  integer*2,allocatable  ::  catmy(:,:)      !! unit-catchment IY
  real                   ::  csize           !! hires pixel size
  integer                ::  isHires         !! hires data availability: 15=15sec,  60=1min,  0=no hires data
  character*256          ::  tag             !! hires data tag

  !-------------------------------------------------------------------
  !! linked input list & map
  grandf='./inp/damlist.csv'   !! GRaND CSV damlist
  outf  ='./damloc_tmp.txt'    !! temporal dam allocation data
  mapdir='./inp/map/'          !! CaMa-Flood map data to allocate dam

print *, '[1] Read Map Parameters'
  open(31, file=outf, form='formatted', action='write')
  write(31,'(a)') "damid   damname   damlon   damlat   ix   iy   upreal   uparea_cama   totalsto_mcm"

  open(21, file=grandf, form='formatted', action='read')
  read(21,*) ndams   !! read number of dams to allocate
  read(21,*)         !! skip header

  print *, "INPUT DAM LIST:  ", TRIM(grandf)
  print *, "Number of Dams:  ", ndams
  print *, "CaMa Map Dir:    ", TRIM(mapdir)
  print *, "Output Filename: ", TRIM(outf)
  print *, " "

  fparam=TRIM(mapdir)//'/params.txt'
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

  isGlobal=0
  if( east-west>=360 ) isGlobal=1
  
  print *, 'Map Domain W-E-S-N: ', west, east, south, north
  print *, 'Map Resolution    : ', gsize
  print *, 'Map NX,NY         : ', nx, ny
  print *, "--------------------------"

  ! ------------------------------------------------------------
print *, "[2] Read CaMa-Flood Map Files"

  allocate(nextx(nx,ny), nexty(nx,ny))
  allocate(basin(nx,ny), upgrid(nx,ny),uparea(nx,ny),ctmare(nx,ny),elevtn(nx,ny))
  allocate(outlon(nx,ny),outlat(nx,ny))

  finp=TRIM(mapdir)//'/nextxy.bin'
  open(11,file=finp,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
  read(11,rec=1) nextx
  read(11,rec=2) nexty
  close(11)

  finp=TRIM(mapdir)//'/basin.bin'
  open(11,file=finp,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
  read(11,rec=1) basin
  close(11)

  finp=TRIM(mapdir)//'/upgrid.bin'
  open(11,file=finp,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
  read(11,rec=1) upgrid
  close(11)

  finp=TRIM(mapdir)//'/uparea.bin'
  open(11,file=finp,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
  read(11,rec=1) uparea
  close(11)

  finp=TRIM(mapdir)//'/ctmare.bin'
  open(11,file=finp,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
  read(11,rec=1) ctmare
  close(11)

  finp=TRIM(mapdir)//'/elevtn.bin'
  open(11,file=finp,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
  read(11,rec=1) elevtn
  close(11)

  finp=TRIM(mapdir)//'/lonlat.bin'
  open(11,file=finp,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
  read(11,rec=1) outlon
  read(11,rec=2) outlat
  close(11)

print *, "++ Read Hires Map"

  isHires=0

  !! check 15sec hires map availability
  if( isHires==0 )then
    floc=TRIM(mapdir)//'/15sec/location.txt'
    open(11,file=floc,form='formatted',status='old',iostat=ios)
    if( ios==0 )then
      read(11,*)
      read(11,*)
      read(11,*) buf, tag, buf, buf, buf, buf, MMX, MMY, csize
      close(11)
      if( trim(tag)=='15sec' )then
        isHires=15
        allocate(catmx(MMX,MMY),catmy(MMX,MMY))
        finp=TRIM(mapdir)//'/15sec/15sec.catmxy.bin'
        print *, trim(finp)
        open(11,file=finp,form='unformatted',access='direct',recl=2*MMX*MMY,status='old',iostat=ios)
        read(11,rec=1) catmx
        read(11,rec=2) catmy
        close(11)
      endif
    endif
  endif

  if( isHires==0 )then
    floc=TRIM(mapdir)//'/1min/location.txt'
    open(11,file=floc,form='formatted',status='old',iostat=ios)
    if( ios==0 )then
      read(11,*)
      read(11,*)
      read(11,*) buf, tag, buf, buf, buf, buf, MMX, MMY, csize
      close(11)
      if( trim(tag)=='1min' )then
        isHires=60
        allocate(catmx(MMX,MMY),catmy(MMX,MMY))
        finp=TRIM(mapdir)//'/1min/1min.catmxy.bin'
        print *, trim(finp)
        open(11,file=finp,form='unformatted',access='direct',recl=2*MMX*MMY,status='old',iostat=ios)
        read(11,rec=1) catmx
        read(11,rec=2) catmy
        close(11)
      endif
    endif
  endif

  print *, 'Hires Map=', isHires

  !------------------------------------------------------------
print *, "[3] Read Dam data and allocate"

  do idam=1, ndams, 1
    read(21,*) grandid, damname, damlon, damlat, totalsto, upreal

    if( isGlobal==0 )then
      if( damlon<west .or. damlon>east .or. damlat<south .or. damlat>north ) cycle
    endif

    print *, "grandid:", grandid, TRIM(damname)
    print *, "  lon:", damlon, " lat:", damlat, " uparea:", upreal, " totalsto:", totalsto

    ix=0
    iy=0
    call calc_ixiy(damlon, damlat, ix, iy)

    ! if dam cannot be allocated, shift dam latlon and allocate
    if ( ix<0 .or. iy<0 ) then
      do NN=1, 4  !! allow 4 grid shift at most
        do dx=-NN, NN
          do dy=-NN, NN
            if ( dx==NN .or. dy==NN ) then
              damlon = damlon + gsize*dy
              damlat = damlat + gsize*dx
              call calc_ixiy(damlon, damlat, ix, iy)
              if ( ix>0 .and. iy>0) exit
            endif
          end do
        end do
      end do
    endif

    if ( ix < 0 .or. iy < 0 ) then
      print *, grandid, damlon, damlat, ix, iy, upreal, "undefined"

      fmt="(i12,x,a80,x,2f12.3,x,2i8,x,3f15.3)"
      write(31,fmt) grandid, damname, damlon, damlat, ix, iy, upreal, -9999.0, totalsto
      print *, "---------------------------"
      print *, ""

    else if ( ix > 0 .and. iy > 0) then
      !print *, grandid, damlon, damlat, ix, iy, upreal, uparea(ix,iy)*1e-6
      fmt="(i12,x,a80,x,2f12.3,x,2i8,x,3f15.3)"
      write(31,fmt) grandid, damname, damlon, damlat, ix, iy, upreal, uparea(ix,iy)*1e-6, totalsto

      print *, 'Fortran IX,IY: ', ix, iy
      !print *, 'Python  IX,IY: ', ix-1,  iy-1
      print *, ''

      print *, 'damlat, damlon  (Point):            ', damlat, damlon
      print *, 'Drainage Area [km2]: ', uparea(ix,iy)*1.e-6
      print *, 'Upstream Grids:      ', upgrid(ix,iy)

      if( nextx(ix,iy)<0 .and. nextx(ix,iy)/=-9999 )then
        print *, 'RIVER MOUTH GRID'
      else
        jx=nextx(ix,iy)
        jy=nexty(ix,iy)
        print *, 'Downstream IX,IY,AREA: ', jx, jy, uparea(jx,jy)*1.e-6
      endif

      print *, "---------------------------"
      print *, ""
    end if

  end do

  deallocate(nextx, nexty)
  deallocate(basin, upgrid, uparea,ctmare,elevtn)
  deallocate(outlon,outlat)

  close(21)
  close(31)
CONTAINS
!! =====================
!! calcudamlate (ix,iy) coordinate on CaMa river map, for input location (damlat,damlon)
!! =====================
subroutine calc_ixiy(damlon0, damlat0, ix0, iy0)
  real, intent(in)           ::  damlat0, damlon0
  integer, intent(out)       ::  ix0, iy0
! local
  integer                    ::  JJX, JJY !! high resolution pixel xy
  !----------------------------------------------------
  !! if hires data exist
  if( isHires>0 )then
    JJX=int( (damlon0 -west)/csize )+1
    JJY=int( (north-damlat0)/csize )+1
    if( JJX<=0 .or. JJX>MMX .or. JJY<=0 .or. JJY>MMY )then
      ix0=-99
      iy0=-99
    else
      ix0=catmx(JJX,JJY)
      iy0=catmy(JJX,JJY)
    endif
    if( ix0<=0 .or. ix0>nx .or. ix0<=0 .or. iy0>ny )then
      print *, 'ix,iy cannot be defined'
      ix0=-99
      iy0=-99
    endif
  elseif( isHires==0 )then
    ix0=int( (damlon0 -west)/gsize )+1
    iy0=int( (north-damlat0)/gsize )+1
  endif

  if ( ix0>0 .and. iy0>0) then
    !print *, "nextx(ix,iy):", nextx(ix,iy)
    if( nextx(ix0,iy0)==-9999 )then
      print *, 'NOT LAND GRID'
      ix0= -99
      iy0= -99
    end if
  end if
end subroutine calc_ixiy
!! END CONTAIN

end program main
