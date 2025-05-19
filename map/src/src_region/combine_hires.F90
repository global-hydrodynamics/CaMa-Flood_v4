      program COMBINE_HIRES
! ===============================================
      implicit none
      character*256          ::  tag

      character*256          ::  global_dir                    !! global map directory
      integer                ::  nXX, nYY
      real*8                 ::  lon_ori, lat_ori              !! west and north edge of global map
      real*8                 ::  lon_end, lat_end              !! east and south edge of global map
      real*8                 ::  csize                         !! hires map pixel size [deg]
      integer                ::  cnum

      character*256          ::  region_param
      parameter                 (region_param='../params.txt')
      integer                ::  mXX, mYY                      !! river map grid numbers
      integer                ::  dXX, dYY                      !! river map domain shift from global map
      real*8                 ::  gsize                         !! river map grid size [deg]
      real*8                 ::  west, east, north, south      !! domain

      integer                ::  ix, iy                
      integer                ::  nx, ny                        !! pixel number of regional hires map
!
      character*256          ::  list_loc
      character*256          ::  area                          !! area code
      integer                ::  i, narea                      !! area ID

      integer                ::  ix0, iy0, nx0, ny0            !! input hires map dimention
      real*8                 ::  west0, north0, east0, south0

      integer                ::  isTile
      integer                ::  ilon, ilat, nlon, nlat
      character*7            ::  CCname
      integer                ::  iloc, nloc
      real                   ::  mwin

! regional hires map
      integer*2,allocatable  ::  catmXX(:,:), catmYY(:,:), downx(:,:), downy(:,:)
      integer*1,allocatable  ::  catmZZ(:,:), flwdir(:,:)
      real,allocatable       ::  flddif(:,:), grdare(:,:)
      real,allocatable       ::  elevtn(:,:), uparea(:,:), rivwth(:,:), hand(:,:) 
      integer*1,allocatable  ::  visual(:,:)

! input hires map
      integer*2,allocatable  ::  catmXX0(:,:), catmYY0(:,:), downx0(:,:), downy0(:,:)
      integer*1,allocatable  ::  catmZZ0(:,:), flwdir0(:,:)
      real,allocatable       ::  flddif0(:,:), grdare0(:,:)
      real,allocatable       ::  elevtn0(:,:), uparea0(:,:), rivwth0(:,:), hand0(:,:)
      integer*1,allocatable  ::  visual0(:,:) 

      integer                ::  isDownXY, isFdir

      real,allocatable       ::  lon0(:), lat0(:)

! files
      character*256          ::  rfile1, wfile1, fcheck
      character*256          ::  hires, out_hdir
      integer                ::  ios
      character*256          ::  buf
! ===============================================
      call getarg(1,tag)

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

      hires=trim(global_dir)//'/'//trim(tag)//'/'
      out_hdir='../'//trim(tag)//'/'

      if( trim(tag)=="1min" )then
        cnum=60
        csize=1./dble(cnum)
      elseif( trim(tag)=="30sec" )then
        cnum=120
        csize=1./dble(cnum)
      elseif( trim(tag)=="15sec" )then
        cnum=240
        csize=1./dble(cnum)
      elseif( trim(tag)=="5sec" )then
        cnum=720
        csize=1./dble(cnum)
      elseif( trim(tag)=="3sec" )then
        cnum=1200
        csize=1./dble(cnum)
      elseif( trim(tag)=="1sec" )then
        cnum=3600
        csize=1./dble(cnum)
      else
        stop
      endif

      nx=int( (east-west)  *cnum )      !!  hires output nx*ny
      ny=int( (north-south)*cnum )

      isTile=0
      if( trim(tag)/='1min' )then
        if( nx>16000 .or. ny>16000)then
          isTile=1
        endif
      endif

      print '(a8,4f8.2)', 'W E N S ', west, east, north, south
      print '(a16,4i8)' , 'nx ny dXX dYY ', nx, ny, dXX, dYY


! ###### calculation for non-tiled hires map
      isDownXY=0
      isFdir=0
      if( isTile==0 )then

        CCname=trim(tag)

        allocate(catmXX(nx,ny),catmYY(nx,ny),catmZZ(nx,ny),flddif(nx,ny),grdare(nx,ny))
        allocate(flwdir(nx,ny),hand(nx,ny),elevtn(nx,ny),uparea(nx,ny),rivwth(nx,ny),visual(nx,ny),downx(nx,ny),downy(nx,ny))

        catmXX(:,:)=-9999
        catmYY(:,:)=-9999
        catmZZ(:,:)=-9
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

        do i=1, narea
          read(11,*) buf, area, west0, east0, south0, north0, nx0, ny0, buf

          if( west0>east .or. east0<west .or. north0<south .or. south0>north )then
            print *, '      out of domain: ', trim(area)
            goto 2000
          endif

          allocate(catmXX0(nx0,ny0),catmYY0(nx0,ny0),catmZZ0(nx0,ny0),flddif0(nx0,ny0),grdare0(nx0,ny0))
          allocate(flwdir0(nx0,ny0),hand0(nx0,ny0),elevtn0(nx0,ny0),uparea0(nx0,ny0),rivwth0(nx0,ny0),visual0(nx0,ny0))
          allocate(downx0(nx0,ny0),downy0(nx0,ny0))
          allocate(lon0(nx0),lat0(ny0))

          rfile1=trim(hires)//trim(area)//'.catmxy.bin'
          print *, trim(rfile1)
          open(21,file=rfile1,form='unformatted',access='direct',recl=2*nx0*ny0,status='old',iostat=ios)
          if( ios==0 )then
            read(21,rec=1) catmXX0
            read(21,rec=2) catmYY0
            close(21)
          else
            print *, '*******************'
            print *, 'no data: ', rfile1
            goto 1000
          endif

          isDownXY=0
          rfile1=trim(hires)//trim(area)//'.downxy.bin'
          print *, trim(rfile1)
          open(21,file=rfile1,form='unformatted',access='direct',recl=2*nx0*ny0,status='old',iostat=ios)
          if( ios==0 )then
            isDownXY=1
            read(21,rec=1) downx0
            read(21,rec=2) downy0
            close(21)
          else
            isDownXY=0
          endif

          isFdir=0
          rfile1=trim(hires)//trim(area)//'.flwdir.bin'
          print *, trim(rfile1)
          open(21,file=rfile1,form='unformatted',access='direct',recl=1*nx0*ny0,status='old',iostat=ios)
          if( ios==0 )then
            isFdir=1
            read(21,rec=1) flwdir0
            close(21)
          else
            isFdir=0
          endif

          rfile1=trim(hires)//trim(area)//'.catmzz.bin'
          open(21,file=rfile1,form='unformatted',access='direct',recl=1*nx0*ny0,status='old',iostat=ios)
          if( ios==0 )then
            read(21,rec=1) catmZZ0
            close(21)
          endif

          rfile1=trim(hires)//trim(area)//'.flddif.bin'
          open(21,file=rfile1,form='unformatted',access='direct',recl=4*nx0*ny0,status='old',iostat=ios)
          if( ios==0 )then
            read(21,rec=1) flddif0
            close(21)
          endif

          rfile1=trim(hires)//trim(area)//'.grdare.bin'
          open(21,file=rfile1,form='unformatted',access='direct',recl=4*nx0*ny0,status='old',iostat=ios)
          if( ios==0 )then
            read(21,rec=1) grdare0
            close(21)
          endif

          rfile1=trim(hires)//trim(area)//'.hand.bin'
          open(21,file=rfile1,form='unformatted',access='direct',recl=4*nx0*ny0,status='old',iostat=ios)
          if( ios==0 )then
            read(21,rec=1) hand0
            close(21)
          endif

          rfile1=trim(hires)//trim(area)//'.elevtn.bin'
          open(21,file=rfile1,form='unformatted',access='direct',recl=4*nx0*ny0,status='old',iostat=ios)
          if( ios==0 )then
            read(21,rec=1) elevtn0
            close(21)
          endif

          rfile1=trim(hires)//trim(area)//'.uparea.bin'
          open(21,file=rfile1,form='unformatted',access='direct',recl=4*nx0*ny0,status='old',iostat=ios)
          if( ios==0 )then
            read(21,rec=1) uparea0
            close(21)
          endif

          rfile1=trim(hires)//trim(area)//'.rivwth.bin'
          open(21,file=rfile1,form='unformatted',access='direct',recl=4*nx0*ny0,status='old',iostat=ios)
          if( ios==0 )then
            read(21,rec=1) rivwth0
            close(21)
          endif

          rfile1=trim(hires)//trim(area)//'.visual.bin'
          open(21,file=rfile1,form='unformatted',access='direct',recl=1*nx0*ny0,status='old',iostat=ios)
          if( ios==0 )then
            read(21,rec=1) visual0
            close(21)
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

                if( catmXX(ix,iy)==-9999 )then
                  if( catmXX0(ix0,iy0)>0 )then
                    catmXX(ix,iy)=catmXX0(ix0,iy0)-dXX
                    catmYY(ix,iy)=catmYY0(ix0,iy0)-dYY
                    if( catmXX(ix,iy)<1 .or. catmXX(ix,iy)>mXX .or. catmYY(ix,iy)<1 .or. catmYY(ix,iy)>mYY )then
                      catmXX(ix,iy)=-999
                      catmYY(ix,iy)=-999
                    endif
                    catmZZ(ix,iy)=catmZZ0(ix0,iy0)
                  else
                    catmXX(ix,iy)=catmXX0(ix0,iy0)
                    catmYY(ix,iy)=catmYY0(ix0,iy0)
                    catmZZ(ix,iy)=catmZZ0(ix0,iy0)
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
          deallocate(catmXX0,catmYY0,catmZZ0,flddif0,grdare0,downx0,downy0)
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

        wfile1=trim(out_hdir)//'/'//trim(CCname)//'.flddif.bin'
        open(21,file=wfile1,form='unformatted',access='direct',recl=4*nx*ny)
        write(21,rec=1) flddif
        close(21)

        if( trim(tag)/='3sec' )then
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
        open(21,file=wfile1,form='unformatted',access='direct',recl=4*nx*ny)
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
        write(21,'(i6,a10,4f10.3,2i8,f20.15)')   1, trim(tag), west, east, south, north, nx, ny, csize
        close(21)
      endif

!###########################################################

      if( isTile==1 )then        !! Tiles High Resolution Data
        if( trim(tag)=="30sec" )then
          cnum=120
          csize=1./dble(cnum)
          mwin=30
        elseif( trim(tag)=="15sec" )then
          cnum=240
          csize=1./dble(cnum)
          mwin=30
        elseif( trim(tag)=="5sec" )then
          cnum=720
          csize=1./dble(cnum)
          mwin=10
        elseif( trim(tag)=="3sec" )then
          cnum=1200
          csize=1./dble(cnum)
          mwin=10
        elseif( trim(tag)=="1sec" )then
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

        allocate(catmXX(nx,ny),catmYY(nx,ny),catmZZ(nx,ny),flddif(nx,ny),downx(nx,ny),downy(nx,ny))
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
              goto 3000
            endif

            rfile1=trim(hires)//trim(CCname)//'.catmzz.bin'
            open(21,file=rfile1,form='unformatted',access='direct',recl=1*nx*ny,status='old',iostat=ios)
            if( ios==0 )then
              read(21,rec=1) catmZZ
              close(21)
            endif

            rfile1=trim(hires)//trim(CCname)//'.flddif.bin'
            open(21,file=rfile1,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
            if( ios==0 )then
              read(21,rec=1) flddif
              close(21)
            endif

            rfile1=trim(hires)//trim(CCname)//'.hand.bin'
            open(21,file=rfile1,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
            if( ios==0 )then
              read(21,rec=1) hand
              close(21)
            endif

            rfile1=trim(hires)//trim(CCname)//'.elevtn.bin'
            open(21,file=rfile1,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
            if( ios==0 )then
              read(21,rec=1) elevtn
              close(21)
            endif

            rfile1=trim(hires)//trim(CCname)//'.uparea.bin'
            open(21,file=rfile1,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
            if( ios==0 )then
              read(21,rec=1) uparea
              close(21)
            endif

            rfile1=trim(hires)//trim(CCname)//'.rivwth.bin'
            open(21,file=rfile1,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
            if( ios==0 )then
              read(21,rec=1) rivwth
              close(21)
            endif

            rfile1=trim(hires)//trim(CCname)//'.visual.bin'
            open(21,file=rfile1,form='unformatted',access='direct',recl=1*nx*ny,status='old',iostat=ios)
            if( ios==0 )then
              read(21,rec=1) visual
              close(21)
            endif

            isDownXY=0
            rfile1=trim(hires)//trim(CCname)//'.downxy.bin'
            print *, trim(rfile1)
            open(21,file=rfile1,form='unformatted',access='direct',recl=2*nx*ny,status='old',iostat=ios)
            if( ios==0 )then
              isDownXY=1
              read(21,rec=1) downx
              read(21,rec=2) downy
              close(21)
            else
              isDownXY=0
            endif
  
            isFdir=0
            rfile1=trim(hires)//trim(CCname)//'.flwdir.bin'
            print *, trim(rfile1)
            open(21,file=rfile1,form='unformatted',access='direct',recl=1*nx*ny,status='old',iostat=ios)
            if( ios==0 )then
              isFdir=1
              read(21,rec=1) flwdir
              close(21)
            else
              isFdir=0
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
      subroutine set_name(lon,lat,cname)
! ===============================================
      implicit none
!
      real*8          ::  lon, lat

      character*1     ::  ew, sn
      character*2     ::  clat
      character*3     ::  clon
      character*7     ::  cname
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

