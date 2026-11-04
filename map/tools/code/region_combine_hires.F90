      program COMBINE_HIRES
! ===============================================
      implicit none

! Map geometry, region offsets, and selected high-resolution resolution.
      character*256            :: hires_tag

      character*256            :: global_dir  !! global map directory
      integer                  :: nXX, nYY  !! x/y map dimensions
      real*8                   :: lon_ori, lat_ori  !! west and north edge of global map
      real*8                   :: lon_end, lat_end  !! east and south edge of global map
      real*8                   :: csize  !! high-resolution pixel size [deg]
      integer                  :: cnum                   !! high-resolution pixels per degree

      character*256            :: region_param
      parameter                 (region_param='../params.txt')
      integer                  :: mXX, mYY  !! regional x/y dimensions
      integer                  :: dXX, dYY  !! river map domain shift from global map
      real*8                   :: gsize  !! map grid size [deg]
      real*8                   :: west, east, north, south  !! map bounds [deg]

      integer                  :: ix, iy  !! x/y grid index
      integer                  :: nx, ny  !! x/y grid dimensions
!
      character*256            :: list_loc
      character*256            :: area  !! area code
      integer                  :: i  !! area ID
      integer                  :: narea  !! area ID

      integer                  :: ix0, iy0  !! source high-resolution pixel indices
      integer                  :: nx0  !! source-area x dimension
      integer                  :: ny0  !! source-area y dimension
      real*8                   :: west0, north0, east0, south0  !! map bounds [deg]

      integer                  :: isTile                 !! whether the regional extent is processed in tiles
      integer                  :: ilon                   !! tile longitude index
      integer                  :: ilat                   !! tile latitude index
      integer                  :: nlon                   !! number of global tiles along longitude
      integer                  :: nlat                   !! number of global tiles along latitude
      character*7              :: CCname                 !! current source-area or output-tile name
      integer                  :: iloc                   !! output location-table row index
      integer                  :: nloc                   !! count of source tiles with required catmxy input
      real                     :: mwin                   !! geographic tile width [deg]

! regional hires map
      integer*2, allocatable    :: catmXX(:,:), catmYY(:,:)  !! high-resolution pixel to regional catchment x/y indices
      integer*2, allocatable    :: downx(:,:), downy(:,:)  !! relative x/y offsets to downstream high-resolution pixel
      integer*1, allocatable    :: catmZZ(:,:)  !! floodplain profile band per pixel
      integer*1, allocatable    :: catmZ100(:,:)  !! floodplain rank (1-100) per pixel
      integer*1, allocatable    :: flwdir(:,:)  !! high-resolution flow-direction code
      real, allocatable         :: flddif(:,:)  !! elevation difference from reference channel pixel [m]
      real, allocatable         :: grdare(:,:)  !! represented land area per pixel [km2]
      real, allocatable         :: elevtn(:,:)  !! elevation [m]
      real, allocatable         :: uparea(:,:)  !! upstream contributing area per pixel [km2]
      real, allocatable         :: rivwth(:,:)  !! channel width [m]
      real, allocatable         :: hand(:,:)  !! height above nearest drainage [m]
      integer*1, allocatable    :: visual(:,:)  !! categorical visualization code

! Source-area arrays are read with the same layer layout before being placed
! into the regional grid above.
      integer*2, allocatable    :: catmXX0(:,:)
      integer*2, allocatable    :: catmYY0(:,:)
      integer*2, allocatable    :: downx0(:,:)
      integer*2, allocatable    :: downy0(:,:)
      integer*1, allocatable    :: catmZZ0(:,:)
      integer*1, allocatable    :: catmZ100_0(:,:)
      integer*1, allocatable    :: flwdir0(:,:)
      real, allocatable         :: flddif0(:,:)
      real, allocatable         :: grdare0(:,:)
      real, allocatable         :: elevtn0(:,:)
      real, allocatable         :: uparea0(:,:)
      real, allocatable         :: rivwth0(:,:)
      real, allocatable         :: hand0(:,:)
      integer*1, allocatable    :: visual0(:,:)

      integer                  :: isDownXY               !! whether to write the downxy output layer
      integer                  :: isFdir                 !! whether to write the flow-direction output layer

      real, allocatable         :: lon0(:), lat0(:)      !! source high-resolution pixel-center coordinates [deg]

! files
      character*256            :: rfile1
      character*256            :: wfile1
      character*256            :: fcheck
      character*256            :: hires
      character*256            :: out_hdir
      integer                  :: ios  !! I/O status code
      character*256            :: buf
! ===============================================
      call getarg(1,hires_tag)

! ================================================
! [1] Read regional geometry and select the high-resolution grid
! ================================================
! Combine source high-resolution map areas for tag (for example, 1min)
! into the selected regional extent; available layer files are read by area.
! Load the regional crop metadata and use the requested high-resolution tag
! to locate source layers and define the output pixel size and dimensions.
      rfile1='./dim_change.txt'
      open(11,file=rfile1,form='formatted')
      read(11,'(a)') global_dir
      read(11,*    ) nXX
      read(11,*    ) nYY
      read(11,*    ) gsize
      read(11,*    ) lon_ori
      read(11,*    ) lon_end
      read(11,*    ) lat_end
      read(11,*    ) lat_ori
      read(11,*    ) 
      read(11,*    ) mXX
      read(11,*    ) mYY
      read(11,*    ) dXX
      read(11,*    ) dYY
      read(11,*    ) west
      read(11,*    ) east
      read(11,*    ) south
      read(11,*    ) north
      close(11)

      hires=trim(global_dir)//'/'//trim(hires_tag)//'/'
      out_hdir='../'//trim(hires_tag)//'/'

      if( trim(hires_tag)=="1min" )then
        cnum=60
        csize=1./dble(cnum)
      elseif( trim(hires_tag)=="30sec" )then
        cnum=120
        csize=1./dble(cnum)
      elseif( trim(hires_tag)=="15sec" )then
        cnum=240
        csize=1./dble(cnum)
      elseif( trim(hires_tag)=="5sec" )then
        cnum=720
        csize=1./dble(cnum)
      elseif( trim(hires_tag)=="3sec" )then
        cnum=1200
        csize=1./dble(cnum)
      elseif( trim(hires_tag)=="1sec" )then
        cnum=3600
        csize=1./dble(cnum)
      else
        stop
      endif

      nx=int( (east-west)  *cnum )      !!  hires output nx*ny
      ny=int( (north-south)*cnum )

! Large high-resolution extents are processed as tiles (except 1min).
      isTile=0
      if( trim(hires_tag)/='1min' )then
        if( nx>16000 .or. ny>16000)then
          isTile=1
        endif
      endif

      print '(a8,4f8.2)', 'W E N S ', west, east, north, south
      print '(a16,4i8)' , 'nx ny dXX dYY ', nx, ny, dXX, dYY


! ================================================
! [2] Combine intersecting source areas into one regional grid
! ================================================
! For a non-tiled map, read each intersecting source area, place its pixels
! into the regional arrays, and write the regional layers and location.txt.
      isDownXY=0
      isFdir=0
      if( isTile==0 )then

        CCname=trim(hires_tag)

        allocate(catmXX(nx,ny),catmYY(nx,ny),catmZZ(nx,ny),flddif(nx,ny),grdare(nx,ny),catmZ100(nx,ny))
        allocate(flwdir(nx,ny),hand(nx,ny),elevtn(nx,ny),uparea(nx,ny),rivwth(nx,ny),visual(nx,ny),downx(nx,ny),downy(nx,ny))

        catmXX(:,:)=-9999
        catmYY(:,:)=-9999
        catmZZ(:,:)=-9
        catmZ100(:,:)=-9
        flwdir(:,:)=-9
        flddif(:,:)=-9999
        grdare(:,:)=-9999
        elevtn(:,:)=-9999
        hand(:,:)  =-9999
        rivwth(:,:)=-9999
        uparea(:,:)=-9999
        visual(:,:)=-9
        downx(:,:) =-9999
        downy(:,:) =-9999

        list_loc=trim(hires)//'/location.txt'
        open(11,file=list_loc,form='formatted')
        read(11,*) narea
        read(11,*)

! Read and combine the requested source layers from each listed high-resolution area.
        do i=1, narea
          read(11,*) buf, area, west0, east0, south0, north0, nx0, ny0, buf

          if( west0>east .or. east0<west .or. north0<south .or. south0>north )then
            print *, '      out of domain: ', trim(area)
            goto 2000
          endif

          allocate(catmXX0(nx0,ny0),catmYY0(nx0,ny0),catmZZ0(nx0,ny0),flddif0(nx0,ny0),grdare0(nx0,ny0),catmZ100_0(nx0,ny0))
          allocate(flwdir0(nx0,ny0),hand0(nx0,ny0),elevtn0(nx0,ny0),uparea0(nx0,ny0),rivwth0(nx0,ny0),visual0(nx0,ny0))
          allocate(downx0(nx0,ny0),downy0(nx0,ny0))
          allocate(lon0(nx0),lat0(ny0))

! Initialize optional source layers to their missing values for this area.
          catmZZ0=-9
          catmZ100_0=-9
          flwdir0=-9
          flddif0=-9999.
          grdare0=-9999.
          hand0=-9999.
          elevtn0=-9999.
          uparea0=-9999.
          rivwth0=-9999.
          visual0=-9
          downx0=-9999
          downy0=-9999

          rfile1=trim(hires)//trim(area)//'.catmxy.bin'
          print *, trim(rfile1)
          open(21,file=rfile1,form='unformatted',access='direct',recl=2*nx0*ny0,status='old',iostat=ios)
          if( ios==0 )then
            read(21,rec=1) catmXX0
            read(21,rec=2) catmYY0
            close(21)
          else
            print *, 'ERROR: required catmxy area is missing; continue with missing values: ', trim(rfile1)
            goto 1000
          endif

          rfile1=trim(hires)//trim(area)//'.downxy.bin'
          print *, trim(rfile1)
          open(21,file=rfile1,form='unformatted',access='direct',recl=2*nx0*ny0,status='old',iostat=ios)
          if( ios==0 )then
            isDownXY=1
            read(21,rec=1) downx0
            read(21,rec=2) downy0
            close(21)
          else
            call report_missing(rfile1)
            isDownXY=1
          endif

          rfile1=trim(hires)//trim(area)//'.flwdir.bin'
          print *, trim(rfile1)
          open(21,file=rfile1,form='unformatted',access='direct',recl=1*nx0*ny0,status='old',iostat=ios)
          if( ios==0 )then
            isFdir=1
            read(21,rec=1) flwdir0
            close(21)
          else
            call report_missing(rfile1)
            isFdir=1
          endif

          rfile1=trim(hires)//trim(area)//'.catmzz.bin'
          open(21,file=rfile1,form='unformatted',access='direct',recl=1*nx0*ny0,status='old',iostat=ios)
          if( ios==0 )then
            read(21,rec=1) catmZZ0
            close(21)
          else
            call report_missing(rfile1)
          endif

          rfile1=trim(hires)//trim(area)//'.catmz100.bin'
          open(21,file=rfile1,form='unformatted',access='direct',recl=1*nx0*ny0,status='old',iostat=ios)
          if( ios==0 )then
            read(21,rec=1) catmZ100_0
            close(21)
          else
            call report_missing(rfile1)
          endif

          rfile1=trim(hires)//trim(area)//'.flddif.bin'
          open(21,file=rfile1,form='unformatted',access='direct',recl=4*nx0*ny0,status='old',iostat=ios)
          if( ios==0 )then
            read(21,rec=1) flddif0
            close(21)
          else
            call report_missing(rfile1)
          endif

          rfile1=trim(hires)//trim(area)//'.grdare.bin'
          open(21,file=rfile1,form='unformatted',access='direct',recl=4*nx0*ny0,status='old',iostat=ios)
          if( ios==0 )then
            read(21,rec=1) grdare0
            close(21)
          else
            call report_missing(rfile1)
          endif

          rfile1=trim(hires)//trim(area)//'.hand.bin'
          open(21,file=rfile1,form='unformatted',access='direct',recl=4*nx0*ny0,status='old',iostat=ios)
          if( ios==0 )then
            read(21,rec=1) hand0
            close(21)
          else
            call report_missing(rfile1)
          endif

          rfile1=trim(hires)//trim(area)//'.elevtn.bin'
          open(21,file=rfile1,form='unformatted',access='direct',recl=4*nx0*ny0,status='old',iostat=ios)
          if( ios==0 )then
            read(21,rec=1) elevtn0
            close(21)
          else
            call report_missing(rfile1)
          endif

          rfile1=trim(hires)//trim(area)//'.uparea.bin'
          open(21,file=rfile1,form='unformatted',access='direct',recl=4*nx0*ny0,status='old',iostat=ios)
          if( ios==0 )then
            read(21,rec=1) uparea0
            close(21)
          else
            call report_missing(rfile1)
          endif

          rfile1=trim(hires)//trim(area)//'.rivwth.bin'
          open(21,file=rfile1,form='unformatted',access='direct',recl=4*nx0*ny0,status='old',iostat=ios)
          if( ios==0 )then
            read(21,rec=1) rivwth0
            close(21)
          else
            call report_missing(rfile1)
          endif

          rfile1=trim(hires)//trim(area)//'.visual.bin'
          open(21,file=rfile1,form='unformatted',access='direct',recl=1*nx0*ny0,status='old',iostat=ios)
          if( ios==0 )then
            read(21,rec=1) visual0
            close(21)
          else
            call report_missing(rfile1)
          endif

          do ix0=1, nx0
            lon0(ix0)=real( west0 +(real(ix0)-0.5)*csize )
          end do
          do iy0=1, ny0
            lat0(iy0)=real( north0-(real(iy0)-0.5)*csize )
          end do

          do iy0=1, ny0
            do ix0=1, nx0

              if( lon0(ix0)>west .and. lon0(ix0)<east .and. lat0(iy0)>south .and. lat0(iy0)<north )then

                ix=int( (lon0(ix0) -west)/csize )+1
                iy=int( (north-lat0(iy0))/csize )+1

                ! Source catmxy indices are global-map coordinates. Shift them
                ! into the cropped regional grid and mark catchments outside it.
                if( catmXX(ix,iy)==-9999 )then
                  if( catmXX0(ix0,iy0)>0 )then
                    catmXX(ix,iy)=catmXX0(ix0,iy0)-dXX
                    catmYY(ix,iy)=catmYY0(ix0,iy0)-dYY
                    if( catmXX(ix,iy)<1 .or. catmXX(ix,iy)>mXX .or. catmYY(ix,iy)<1 .or. catmYY(ix,iy)>mYY )then
                      catmXX(ix,iy)=-999
                      catmYY(ix,iy)=-999
                    endif
                    catmZZ(ix,iy)=catmZZ0(ix0,iy0)
                    catmZ100(ix,iy)=catmZ100_0(ix0,iy0)
                  else
                    catmXX(ix,iy)=catmXX0(ix0,iy0)
                    catmYY(ix,iy)=catmYY0(ix0,iy0)
                    catmZZ(ix,iy)=catmZZ0(ix0,iy0)
                    catmZ100(ix,iy)=catmZ100_0(ix0,iy0)
                  endif
                endif

                if( isDownXY==1 )then
                  downx(ix,iy)=downx0(ix0,iy0)
                  downy(ix,iy)=downy0(ix0,iy0)
                endif
                if( isFdir==1 )then
                  flwdir(ix,iy)=flwdir0(ix0,iy0)
                endif

                visual(ix,iy)=visual0(ix0,iy0)
                flddif(ix,iy)=flddif0(ix0,iy0)
                grdare(ix,iy)=grdare0(ix0,iy0)
                hand(ix,iy)  =hand0(ix0,iy0)
                elevtn(ix,iy)=elevtn0(ix0,iy0)
                uparea(ix,iy)=uparea0(ix0,iy0)
                rivwth(ix,iy)=rivwth0(ix0,iy0)
              endif
            end do
          end do
 1000     continue
          deallocate(catmXX0,catmYY0,catmZZ0,flddif0,grdare0,downx0,downy0,catmZ100_0)
          deallocate(flwdir0,hand0,elevtn0,uparea0,rivwth0,visual0,lon0,lat0)
 2000     continue
        end do

        wfile1=trim(out_hdir)//'/'//trim(CCname)//'.catmxy.bin'
        open(21,file=wfile1,form='unformatted',access='direct',recl=2*nx*ny)
        write(21,rec=1) catmXX
        write(21,rec=2) catmYY
        close(21)

        wfile1=trim(out_hdir)//'/'//trim(CCname)//'.catmzz.bin'
        open(21,file=wfile1,form='unformatted',access='direct',recl=1*nx*ny)
        write(21,rec=1) catmZZ
        close(21)

        wfile1=trim(out_hdir)//'/'//trim(CCname)//'.catmz100.bin'
        open(21,file=wfile1,form='unformatted',access='direct',recl=1*nx*ny)
        write(21,rec=1) catmZ100
        close(21)

        wfile1=trim(out_hdir)//'/'//trim(CCname)//'.flddif.bin'
        open(21,file=wfile1,form='unformatted',access='direct',recl=4*nx*ny)
        write(21,rec=1) flddif
        close(21)

        if( trim(hires_tag)/='3sec' )then
          wfile1=trim(out_hdir)//'/'//trim(CCname)//'.grdare.bin'
          open(21,file=wfile1,form='unformatted',access='direct',recl=4*nx*ny)
          write(21,rec=1) grdare
          close(21)
        endif

        wfile1=trim(out_hdir)//'/'//trim(CCname)//'.hand.bin'
        open(21,file=wfile1,form='unformatted',access='direct',recl=4*nx*ny)
        write(21,rec=1) hand
        close(21)

        wfile1=trim(out_hdir)//'/'//trim(CCname)//'.elevtn.bin'
        open(21,file=wfile1,form='unformatted',access='direct',recl=4*nx*ny)
        write(21,rec=1) elevtn
        close(21)

        wfile1=trim(out_hdir)//'/'//trim(CCname)//'.uparea.bin'
        open(21,file=wfile1,form='unformatted',access='direct',recl=4*nx*ny)
        write(21,rec=1) uparea
        close(21)

        wfile1=trim(out_hdir)//'/'//trim(CCname)//'.rivwth.bin'
        open(21,file=wfile1,form='unformatted',access='direct',recl=4*nx*ny)
        write(21,rec=1) rivwth
        close(21)

        wfile1=trim(out_hdir)//'/'//trim(CCname)//'.visual.bin'
        open(21,file=wfile1,form='unformatted',access='direct',recl=1*nx*ny)
        write(21,rec=1) visual
        close(21)

        if( isDownXY==1 )then
          wfile1=trim(out_hdir)//'/'//trim(CCname)//'.downxy.bin'
          open(21,file=wfile1,form='unformatted',access='direct',recl=2*nx*ny)
          write(21,rec=1) downx
          write(21,rec=2) downy
          close(21)
        endif

        if( isFdir==1 )then
          wfile1=trim(out_hdir)//'/'//trim(CCname)//'.flwdir.bin'
          open(21,file=wfile1,form='unformatted',access='direct',recl=1*nx*ny)
          write(21,rec=1) flwdir
          close(21)
        endif

        wfile1=trim(out_hdir)//'/location.txt'

        open(21,file=wfile1,form='formatted')
        write(21,'(i6,a10)') 1, '  narea'
        write(21,'(a6,a10,4a10,  2a8,a20)'   )   'iarea', 'carea', 'west', 'east', 'south', 'north', 'nx', 'ny', 'csize'
        write(21,'(i6,a10,4f10.3,2i8,f20.15)')   1, trim(hires_tag), west, east, south, north, nx, ny, csize
        close(21)
      endif

!###########################################################

! ================================================
! [3] Process large domains as geographic tiles
! ================================================
! For large domains, process and write geographic tiles instead of one array.
      if( isTile==1 )then        !! Tiles High Resolution Data
        if( trim(hires_tag)=="30sec" )then
          cnum=120
          csize=1./dble(cnum)
          mwin=30
        elseif( trim(hires_tag)=="15sec" )then
          cnum=240
          csize=1./dble(cnum)
          mwin=30
        elseif( trim(hires_tag)=="5sec" )then
          cnum=720
          csize=1./dble(cnum)
          mwin=10
        elseif( trim(hires_tag)=="3sec" )then
          cnum=1200
          csize=1./dble(cnum)
          mwin=10
        elseif( trim(hires_tag)=="1sec" )then
          cnum=3600
          csize=1./dble(cnum)
          mwin=5
        else
          stop
        endif

        nx=int( mwin*cnum )
        ny=int( mwin*cnum )
        nlon=int( 360/mwin )
        nlat=int( 180/mwin )

print *, nx, ny

        allocate(catmXX(nx,ny),catmYY(nx,ny),catmZZ(nx,ny),flddif(nx,ny),downx(nx,ny),downy(nx,ny),catmZ100(nx,ny))
        allocate(flwdir(nx,ny),hand(nx,ny),elevtn(nx,ny),uparea(nx,ny),rivwth(nx,ny),visual(nx,ny))

print *, nlon, nlat

        nloc=0
        do ilat=1, nlat
          do ilon=1, nlon
            west0 =-180.0 + mwin*(ilon-1)
            south0=  90.0 - mwin*(ilat)
            east0 = west0 + mwin
            north0=south0 + mwin
            if( west0>=east .or. east0<=west .or. north0<=south .or. south0>=north )then
              goto 3000
            endif

            call set_name(west0,south0,CCname)
            rfile1=trim(hires)//trim(CCname)//'.catmxy.bin'
            open(21,file=rfile1,form='unformatted',access='direct',recl=2*nx*ny,status='old',iostat=ios)
            if( ios==0 )then
              print *, rfile1
              read(21,rec=1) catmXX
              read(21,rec=2) catmYY
              close(21)
              nloc=nloc+1
            else
              print *, 'ERROR: required catmxy tile is missing; skip: ', trim(rfile1)
              goto 3000
            endif

! Reset optional layers for every tile so a missing file cannot reuse
! uninitialized values or data left over from the previous tile.
            catmZZ=-9
            catmZ100=-9
            flddif=-9999.
            hand=-9999.
            elevtn=-9999.
            uparea=-9999.
            rivwth=-9999.
            visual=-9
            downx=-9999
            downy=-9999
            flwdir=-9

            rfile1=trim(hires)//trim(CCname)//'.catmzz.bin'
            open(21,file=rfile1,form='unformatted',access='direct',recl=1*nx*ny,status='old',iostat=ios)
            if( ios==0 )then
              read(21,rec=1) catmZZ
              close(21)
            else
              call report_missing(rfile1)
            endif

            rfile1=trim(hires)//trim(CCname)//'.catmz100.bin'
            open(21,file=rfile1,form='unformatted',access='direct',recl=1*nx*ny,status='old',iostat=ios)
            if( ios==0 )then
              read(21,rec=1) catmZ100
              close(21)
            else
              call report_missing(rfile1)
            endif

            rfile1=trim(hires)//trim(CCname)//'.flddif.bin'
            open(21,file=rfile1,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
            if( ios==0 )then
              read(21,rec=1) flddif
              close(21)
            else
              call report_missing(rfile1)
            endif

            rfile1=trim(hires)//trim(CCname)//'.hand.bin'
            open(21,file=rfile1,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
            if( ios==0 )then
              read(21,rec=1) hand
              close(21)
            else
              call report_missing(rfile1)
            endif

            rfile1=trim(hires)//trim(CCname)//'.elevtn.bin'
            open(21,file=rfile1,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
            if( ios==0 )then
              read(21,rec=1) elevtn
              close(21)
            else
              call report_missing(rfile1)
            endif

            rfile1=trim(hires)//trim(CCname)//'.uparea.bin'
            open(21,file=rfile1,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
            if( ios==0 )then
              read(21,rec=1) uparea
              close(21)
            else
              call report_missing(rfile1)
            endif

            rfile1=trim(hires)//trim(CCname)//'.rivwth.bin'
            open(21,file=rfile1,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
            if( ios==0 )then
              read(21,rec=1) rivwth
              close(21)
            else
              call report_missing(rfile1)
            endif

            rfile1=trim(hires)//trim(CCname)//'.visual.bin'
            open(21,file=rfile1,form='unformatted',access='direct',recl=1*nx*ny,status='old',iostat=ios)
            if( ios==0 )then
              read(21,rec=1) visual
              close(21)
            else
              call report_missing(rfile1)
            endif

            rfile1=trim(hires)//trim(CCname)//'.downxy.bin'
            print *, trim(rfile1)
            open(21,file=rfile1,form='unformatted',access='direct',recl=2*nx*ny,status='old',iostat=ios)
            if( ios==0 )then
              isDownXY=1
              read(21,rec=1) downx
              read(21,rec=2) downy
              close(21)
            else
              call report_missing(rfile1)
              isDownXY=1
            endif
  
            rfile1=trim(hires)//trim(CCname)//'.flwdir.bin'
            print *, trim(rfile1)
            open(21,file=rfile1,form='unformatted',access='direct',recl=1*nx*ny,status='old',iostat=ios)
            if( ios==0 )then
              isFdir=1
              read(21,rec=1) flwdir
              close(21)
            else
              call report_missing(rfile1)
              isFdir=1
            endif

            do iy=1, ny
              do ix=1, nx
                if( catmXX(ix,iy)>-100 )then
                  catmXX(ix,iy)=catmXX(ix,iy)-dXX
                  catmYY(ix,iy)=catmYY(ix,iy)-dYY
                  if( catmXX(ix,iy)<1 .or. catmXX(ix,iy)>mXX .or. catmYY(ix,iy)<1 .or. catmYY(ix,iy)>mYY )then
                    catmXX(ix,iy)=-999
                    catmYY(ix,iy)=-999
                  endif
                endif
              end do
            end do

            wfile1=trim(out_hdir)//'/'//trim(CCname)//'.catmxy.bin'
            open(21,file=wfile1,form='unformatted',access='direct',recl=2*nx*ny)
            write(21,rec=1) catmXX
            write(21,rec=2) catmYY
            close(21)

            wfile1=trim(out_hdir)//'/'//trim(CCname)//'.catmzz.bin'
            open(21,file=wfile1,form='unformatted',access='direct',recl=1*nx*ny)
            write(21,rec=1) catmZZ
            close(21)

            wfile1=trim(out_hdir)//'/'//trim(CCname)//'.catmz100.bin'
            open(21,file=wfile1,form='unformatted',access='direct',recl=1*nx*ny)
            write(21,rec=1) catmZ100
            close(21)

            wfile1=trim(out_hdir)//'/'//trim(CCname)//'.flddif.bin'
            open(21,file=wfile1,form='unformatted',access='direct',recl=4*nx*ny)
            write(21,rec=1) flddif
            close(21)

            wfile1=trim(out_hdir)//'/'//trim(CCname)//'.hand.bin'
            open(21,file=wfile1,form='unformatted',access='direct',recl=4*nx*ny)
            write(21,rec=1) hand
            close(21)

            wfile1=trim(out_hdir)//'/'//trim(CCname)//'.elevtn.bin'
            open(21,file=wfile1,form='unformatted',access='direct',recl=4*nx*ny)
            write(21,rec=1) elevtn
            close(21)

            wfile1=trim(out_hdir)//'/'//trim(CCname)//'.uparea.bin'
            open(21,file=wfile1,form='unformatted',access='direct',recl=4*nx*ny)
            write(21,rec=1) uparea
            close(21)

            wfile1=trim(out_hdir)//'/'//trim(CCname)//'.rivwth.bin'
            open(21,file=wfile1,form='unformatted',access='direct',recl=4*nx*ny)
            write(21,rec=1) rivwth
            close(21)

            wfile1=trim(out_hdir)//'/'//trim(CCname)//'.visual.bin'
            open(21,file=wfile1,form='unformatted',access='direct',recl=1*nx*ny)
            write(21,rec=1) visual
            close(21)

            if( isDownXY==1 )then
              wfile1=trim(out_hdir)//'/'//trim(CCname)//'.downxy.bin'
              open(21,file=wfile1,form='unformatted',access='direct',recl=2*nx*ny)
              write(21,rec=1) downx
              write(21,rec=2) downy
              close(21)
            endif

            if( isFdir==1 )then
              wfile1=trim(out_hdir)//'/'//trim(CCname)//'.flwdir.bin'
              open(21,file=wfile1,form='unformatted',access='direct',recl=1*nx*ny)
              write(21,rec=1) flwdir
              close(21)
            endif

 3000       continue
          end do
        end do

        wfile1=trim(out_hdir)//'location.txt'
        open(21,file=wfile1,form='formatted')
        write(21,'(i6,a10)') nloc, '  narea'
        write(21,'(a6,a10,4a10,  2a8,a20)'   )   'iarea', 'carea', 'west', 'east', 'south', 'north', 'nx', 'ny', 'csize'

        iloc=0
        do ilat=1, nlat
          do ilon=1, nlon
            west0 =-180.0 +mwin*(ilon-1)
            south0=  90.0 -mwin*(ilat)
            east0 = west0 +mwin
            north0= south0+mwin
            call set_name(west0,south0,CCname)

            fcheck=trim(out_hdir)//trim(CCname)//'.catmxy.bin'
            open(31,file=fcheck,form='unformatted',access='direct',recl=2*nx*ny,status='old',iostat=ios)
            if( ios==0 )then
              close(31)
              iloc=iloc+1
              write(21,'(i6,a10,4f10.3,2i8,f20.15)')   iloc, trim(CCname), west0, east0, south0, north0, nx, ny, csize
            endif
          end do
        end do
      endif

! ====================
CONTAINS
!+
!+
!+
      subroutine report_missing(filename)
! ===============================================
      implicit none
      character*(*)            :: filename
! ===============================================
      print *, 'WARNING: could not open optional layer; continuing with missing values: ', trim(filename)
      end subroutine report_missing

      subroutine set_name(lon,lat,cname)
! ===============================================
      implicit none
!
      real*8                   :: lon, lat  !! longitude/latitude [deg]

      character*1              :: ew
      character*1              :: sn
      character*2              :: clat
      character*3              :: clon
      character*7              :: cname
! ===============================================
      if( lon<0 )then
        ew='w'
        write(clon,'(i3.3)') int(-lon)
      else
        ew='e'
        write(clon,'(i3.3)')  int(lon)
      endif

      if( lat<0 )then
        sn='s'
        write(clat,'(i2.2)') int(-lat)
      else
        sn='n'
        write(clat,'(i2.2)')  int(lat)
      endif

      cname=sn//clat//ew//clon

      end subroutine set_name
!+
!+
!+
      end program COMBINE_HIRES

