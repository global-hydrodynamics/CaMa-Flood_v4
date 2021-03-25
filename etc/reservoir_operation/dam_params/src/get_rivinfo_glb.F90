module convert_latlon
  implicit none

  contains

    subroutine calc_ixiy(lon, lat, nx, ny, mapdir, west, east, north, south, ix, iy, nextx)
      ! calculation type
      character*256       ::  buf

      ! river network map parameters
      integer, intent(out)       ::  ix, iy
      integer             ::  jx, jy, kx, ky, mx,my
      integer, intent(in)        ::  nx, ny        !! river map grid number
      real, intent(in)    ::  lat, lon
      ! river netwrok map
      integer*2,allocatable:: catmx(:,:), catmy(:,:)
      real                ::  glon, glat
      real, intent(in)    ::  west, east, north, south
      real                ::  gsize, csize

      integer             ::  isHires

      ! file
      character*256       ::  finp, fparam, floc
      character*256       ::  tag
      integer             ::  ios

      character*128, intent(in)       ::  mapdir  !!!
  
      integer, intent(in) ::  nextx(nx, ny)      !! downstream 
      
      !----------------------------------------------------

        isHires=0
        print *, ''

        !! check 15sec hires map availability
        floc=TRIM(mapdir)//'/15sec/location.txt'
        open(11,file=floc,form='formatted',status='old',iostat=ios)
        if( ios==0 )then
          read(11,*)
          read(11,*)
          read(11,*) buf, tag, buf, buf, buf, buf, mx, my, csize
          close(11)
          if( trim(tag)=='15sec' )then
            !print *, 'USE 15sec hires map'
            isHires=15

            allocate(catmx(mx,my),catmy(mx,my))

            finp=TRIM(mapdir)//'/15sec/15sec.catmxy.bin'
            open(11,file=finp,form='unformatted',access='direct',recl=2*mx*my,status='old',iostat=ios)
            read(11,rec=1) catmx
            read(11,rec=2) catmy
            close(11)

            jx=int( (lon-west) /csize )+1
            jy=int( (north-lat)/csize )+1
            if( jx<=0 .or. jx>mx .or. jy<=0 .or. jy>my )then
              print *, 'ix,iy cannot be defined'
              ix=-99
              iy=-99
              !stop
            endif
            ix=catmx(jx,jy)
            iy=catmy(jx,jy)
            if( ix<=0 .or. ix>nx .or. ix<=0 .or. iy>ny )then
              print *, 'ix,iy cannot be defined'
              ix=-99
              iy=-99
              !stop
            endif
          endif
        endif

        if( isHires/=15 )then
        !! check 1min hires map availability
          floc=TRIM(mapdir)//'/1min/location.txt'
          open(11,file=floc,form='formatted',status='old',iostat=ios)
          if( ios==0 )then
            read(11,*)
            read(11,*)
            read(11,*) buf, tag, buf, buf, buf, buf, mx, my, csize
            close(11)
            if( trim(tag)=='1min' )then
              !print *, 'USE 1min hires map'
              isHires=60
  
              allocate(catmx(mx,my),catmy(mx,my))
  
              finp=TRIM(mapdir)//'/1min/1min.catmxy.bin'
              open(11,file=finp,form='unformatted',access='direct',recl=2*mx*my,status='old',iostat=ios)
              read(11,rec=1) catmx
              read(11,rec=2) catmy
              close(11)
  
              jx=int( (lon-west) /csize )+1
              jy=int( (north-lat)/csize )+1
              ! print *, jx, jy, mx, my
              if( jx<=0 .or. jx>mx .or. jy<=0 .or. jy>my )then
                print *, 'ix,iy cannot be defined'
                ix=-99
                iy=-99
                !stop
              endif
              ix=catmx(jx,jy)
              iy=catmy(jx,jy)
              ! print *, ix, iy
              if( ix<=0 .or. ix>nx .or. ix<=0 .or. iy>ny )then
                print *, 'ix,iy cannot be defined'
                ix=-99
                iy=-99
                !stop
              endif
            endif
          endif
        endif

        deallocate(catmx, catmy)


        if( isHires==0 )then
          ix=int( (lon-west) /gsize )+1
          iy=int( (north-lat)/gsize )+1
        endif


        glon=west +gsize*(ix-0.5)
        glat=north-gsize*(iy-0.5)


        if ( ix>0 .and. iy>0) then
          !print *, "nextx(ix,iy):", nextx(ix,iy)

          if( nextx(ix,iy)==-9999 )then
            print *, 'NOT LAND GRID'
            ix = -99
            iy = -99
            !stop
          end if
        end if

    end subroutine calc_ixiy
      
end module convert_latlon



!===================================================================

program main

    use convert_latlon

    implicit none

    integer          :: ndams, grand, flag, nx, ny, dam, grandid, ix, iy, dx, dy, is, ios, jx, jy, cnt
    real             :: gsize, west, east, south, north, upreal, lat, lon, totalsto
    character*256    :: buf, grandf, mapdir, outf, fparam, finp, grandid_c, catchskm, londd, latdd, fmt, damname, capmcm
    integer,allocatable ::  nextx(:,:)      !! downstream x
    integer,allocatable ::  nexty(:,:)      !! downstream y
    integer,allocatable ::  basin(:,:)      !! 
    integer,allocatable ::  upgrid(:,:)     !! 
    real,allocatable    ::  uparea(:,:)     !! 
    real,allocatable    ::  ctmare(:,:)     !! 
    real,allocatable    ::  elevtn(:,:)     !! 
    real,allocatable    ::  outlon(:,:)     !! 
    real,allocatable    ::  outlat(:,:)     !! 

    !-------------------------------------------------------------------


    call getarg(1, buf)
    read(buf,*) ndams
    call getarg(2,outf)

    !! link
    grandf='./inp/damfile'
    mapdir='./inp/map/'

    open(150, file=outf, form='formatted', action='write')
    fmt="(a,x,a,x,a,x,a,x,a,x,a,x,a,x,a,x,a)"
    write(150,fmt) "damid", "damname", "lon", "lat", "ix", "iy", "upreal", "uparea_cama", "totalsto_mcm"

    if (is/=0) stop 'cannot open file!!!'
    open(grand, file=grandf, form='formatted', action='read')
    read(grand,*)

    print *, TRIM(grandf)
    print *, ndams
    print *, TRIM(mapdir)
    print *, TRIM(outf)
    print *, " "

    flag=0

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
  
    print *, 'Map Domain W-E-S-N:', west, east, south, north
    print *, 'Map Resolution: ', gsize
    print *, 'Map NX,NY: ',  nx, ny
    print *, "--------------------------"

    ! ------------------------------------------------------------

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

    print *, ""

    !------------------------------------------------------------

    do dam=1, ndams, 1

      read(grand,*) grandid_c,damname,londd,latdd,capmcm,catchskm
      grandid_c=TRIM(grandid_c)
      londd=TRIM(londd)
      latdd=TRIM(latdd)
      catchskm=TRIM(catchskm)
      damname=TRIM(damname)
      read(grandid_c,*) grandid
      read(londd,*) lon
      read(latdd,*) lat
      read(catchskm,*) upreal
      read(capmcm,*) totalsto
      print *, "grandid:", grandid, " damname:", TRIM(damname), " uparea:", upreal
      print *, " lon:", lon, " lat:", lat, " totalsto:", totalsto

      ix=0
      iy=0

      call calc_ixiy(lon, lat, nx, ny, mapdir, west, east, north, south, ix, iy, nextx)

      if ( ix<0 .or. iy<0 ) then
        do cnt=1, 4, 1
          do dx = -1, 1, 1
            do dy = -1, 1, 1
              if ( dx == 0 .and.dy == 0) cycle
              lon = lon + 0.25*cnt*dy
              lat = lat + 0.25*cnt*dx
              call calc_ixiy(lon, lat, nx, ny, mapdir, west, east, north, south, ix, iy, nextx)
              if ( ix>0 .and. iy>0) exit
            end do
          end do
        end do
      endif

      
      if ( ix < 0 .or. iy < 0 ) then
        print *, grandid, lon, lat, ix, iy, upreal, "undefined"
        fmt="(i,x,a,x,f,x,f,x,i,x,i,x,f,x,i,x,f)"
        write(150,fmt) grandid, damname, lon, lat, ix, iy, upreal, -9999, totalsto
        print *, "---------------------------"
        print *, ""

      else if ( ix > 0 .and. iy > 0) then
        !print *, grandid, lon, lat, ix, iy, upreal, uparea(ix,iy)*1e-6
        fmt="(i,x,a,x,f,x,f,x,i,x,i,x,f,x,f,x,f)"
        write(150,fmt) grandid, damname, lon, lat, ix, iy, upreal, uparea(ix,iy)*1e-6, totalsto

        !print *, ''
        !print *, 'Point Information'

        print *, 'Fortran IX,IY: ', ix,  iy
        !print *, 'Python  IX,IY: ', ix-1,  iy-1
        print *, ''

        print *, 'Lat, Lon  (Point):            ', lat, lon
        !print *, 'Lat, Lon  (Catchment Outler): ', outlat(ix,iy), outlon(ix,iy)
        !print *, ''

        ! print *, 'River Basin ID:      ', basin(ix,iy)
        print *, 'Drainage Area [km2]: ',  uparea(ix,iy)*1.e-6
        print *, 'Upstream Grids:      ', upgrid(ix,iy)
        !print *, ''

        ! print *, 'Elevation (Catchment Outler) [m]: ', elevtn(ix,iy)
        ! print *, 'Unit Catchment Area [km2]:        ', ctmare(ix,iy)*1.e-6
        !print *, ''

        if( nextx(ix,iy)<0 .and. nextx(ix,iy)/=-9999 )then
          print *, 'RIVER MOUTH GRID'
        else
          jx=nextx(ix,iy)
          jy=nexty(ix,iy)
          print *, 'Downstream IX,IY,AREA: ', jx, jy, uparea(jx,jy)*1.e-6
        endif

        !print *, ""
        print *, "---------------------------"
        print *, ""
      end if

    end do

    deallocate(nextx, nexty)
    deallocate(basin, upgrid, uparea,ctmare,elevtn)
    deallocate(outlon,outlat)

    close(grand)
    close(150)




end program main