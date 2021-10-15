      program CALC_PRMWAT
! ===============================================
! calculate the area of parmanent water in each unit-catchment (in km2)
! ===============================================
      implicit none
      character*256          ::  tag

      integer                ::  iXX, iYY, nXX, nYY
      real                   ::  gsize                         !! river map grid size [deg]
      real                   ::  lon_ori, lat_ori              !! west and north edge of global map
      real                   ::  lon_end, lat_end              !! east and south edge of global map
      real*8                 ::  csize                         !! hires map pixel size [deg]
      integer                ::  cnum

!
      character*256          ::  list_loc
      character*256          ::  area                          !! area code
      integer                ::  i, narea                      !! area ID

      integer                ::  ix0, iy0, nx0, ny0            !! input hires map dimention
      real                   ::  west0, north0, east0, south0

! global map
      integer*4,allocatable  ::  nextXX(:,:)
      real,allocatable       ::  prmwat(:,:), ctmare(:,:)

! input hires map
      integer*2,allocatable  ::  catmXX0(:,:), catmYY0(:,:)
      real,allocatable       ::  rivwth0(:,:), grdare0(:,:), flddif0(:,:)

! files
      character*256          ::  rfile1, wfile1
      character*256          ::  hires
      integer                ::  ios
      character*256          ::  buf

! ===============================================
      call getarg(1,tag)

      rfile1='../params.txt'
      open(11,file=rfile1,form='formatted')
      read(11,*    ) nXX
      read(11,*    ) nYY
      read(11,*    ) ! nflp
      read(11,*    ) gsize
      read(11,*    ) lon_ori
      read(11,*    ) lon_end
      read(11,*    ) lat_end
      read(11,*    ) lat_ori
      close(11)

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
        print *, "Enter hires map resolution as 1st argument: % calc_prmwat 1min"
        stop
      endif

      allocate(nextXX(nXX,nYY),ctmare(nXX,nYY),prmwat(nXX,nYY))

      rfile1='..//nextxy.bin'
      open(21,file=rfile1,form='unformatted',access='direct',recl=4*nXX*nYY)
      read(21,rec=1) nextXX
      close(21)

      rfile1='..//ctmare.bin'
      open(21,file=rfile1,form='unformatted',access='direct',recl=4*nXX*nYY)
      read(21,rec=1) ctmare
      close(21)

      prmwat(:,:)=-9999
      do iYY=1, nYY
        do iXX=1, nXX
          if( nextXX(iXX,iYY)/=-9999 )then
            prmwat(iXX,iYY)=0
          endif
        end do
      end do

      hires='../'//trim(tag)//'/'
! ###### calculation for non-tiled hires map
      list_loc=trim(hires)//'location.txt'
      open(11,file=list_loc,form='formatted')
      read(11,*) narea
      read(11,*)

      do i=1, narea
        read(11,*) buf, area, west0, east0, south0, north0, nx0, ny0, buf

        allocate(catmXX0(nx0,ny0),catmYY0(nx0,ny0),rivwth0(nx0,ny0),grdare0(nx0,ny0),flddif0(nx0,ny0))

        rfile1=trim(hires)//trim(area)//'.catmxy.bin'
        print *, trim(rfile1)
        open(21,file=rfile1,form='unformatted',access='direct',recl=2*nx0*ny0,status='old',iostat=ios)
        if( ios==0 )then
          print *, rfile1
          read(21,rec=1) catmXX0
          read(21,rec=2) catmYY0
          close(21)
        else
          print *, '*******************'
          print *, 'no data: ', rfile1
          goto 1000
        endif

        rfile1=trim(hires)//trim(area)//'.rivwth.bin'
        open(21,file=rfile1,form='unformatted',access='direct',recl=4*nx0*ny0,status='old',iostat=ios)
        read(21,rec=1) rivwth0
        close(21)

        rfile1=trim(hires)//trim(area)//'.grdare.bin'
        open(21,file=rfile1,form='unformatted',access='direct',recl=4*nx0*ny0,status='old',iostat=ios)
        read(21,rec=1) grdare0
        close(21)

        rfile1=trim(hires)//trim(area)//'.flddif.bin'
        open(21,file=rfile1,form='unformatted',access='direct',recl=4*nx0*ny0,status='old',iostat=ios)
        read(21,rec=1) flddif0
        close(21)

        do iy0=1, ny0
          do ix0=1, nx0
            if( catmXX0(ix0,iy0)>0 )then
              if( rivwth0(ix0,iy0)>0 .or. rivwth0(ix0,iy0)==-1 )then
                if( flddif0(ix0,iy0)<=5 )then  !! exclude parmanent water pixel on the hill (above flddif>5m)
                  iXX=catmXX0(ix0,iy0)
                  iYY=catmYY0(ix0,iy0)
                  prmwat(iXX,iYY)=prmwat(iXX,iYY)+grdare0(ix0,iy0)*1000000  !!  km2 -> m2
                endif
              endif
            endif
          end do
        end do

 1000   continue
        deallocate(catmxx0,catmyy0,rivwth0,grdare0,flddif0)
      end do
      close(11)

! =============

!! limitter by catchment area (needed for coarse-resolution)
      do iYY=1, nYY
        do iXX=1, nXX
          if( nextXX(iXX,iYY)/=-9999 )then
            prmwat(iXX,iYY)=min(prmwat(iXX,iYY),ctmare(iXX,iYY))
          endif
        end do
      end do

      wfile1='..//prmwat.bin'
      open(31,file=wfile1,form='unformatted',access='direct',recl=4*nXX*nYY)
      write(31,rec=1) prmwat
      close(31)

      end program CALC_PRMWAT




      subroutine set_name(lon,lat,cname)
! ===============================================
      implicit none
!
      real            ::  lon, lat

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


