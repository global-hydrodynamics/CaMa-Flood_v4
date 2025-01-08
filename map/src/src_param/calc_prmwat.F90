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
! function
      real*8,allocatable     ::  clat(:)
      real*8,allocatable     ::  carea(:)
! ===============================================
      call getarg(1,tag)

      rfile1='./params.txt'
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

      rfile1='./nextxy.bin'
      open(21,file=rfile1,form='unformatted',access='direct',recl=4*nXX*nYY)
      read(21,rec=1) nextXX
      close(21)

      rfile1='./ctmare.bin'
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

      hires='./'//trim(tag)//'/'
! ###### calculation for non-tiled hires map
      list_loc=trim(hires)//'location.txt'
      open(11,file=list_loc,form='formatted')
      read(11,*) narea
      read(11,*)

      do i=1, narea
        read(11,*) buf, area, west0, east0, south0, north0, nx0, ny0, buf

        allocate(catmXX0(nx0,ny0),catmYY0(nx0,ny0),rivwth0(nx0,ny0),grdare0(nx0,ny0),flddif0(nx0,ny0))

        rfile1=trim(hires)//trim(area)//'.catmxy.bin'
        open(21,file=rfile1,form='unformatted',access='direct',recl=2*nx0*ny0,status='old',iostat=ios)
        if( ios==0 )then
          print *, trim(rfile1)
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
        if( ios==0 )then
          read(21,rec=1) grdare0
          close(21)
        else
          allocate(clat(ny0))
          allocate(carea(ny0))
          do iy0=1, ny0
            clat(iy0) =north0 - csize*(real(iy0)-0.5)
            carea(iy0)=rgetarea(dble(0.),10.*csize,clat(iy0)+5.*csize,clat(iy0)-5.*csize)*0.01*1.e-6    !! km2
          end do
          do iy0=1, ny0
            do ix0=1, nx0
              if( catmXX0(ix0,iy0)/=-9999 )then
                grdare0(ix0,iy0)=real(carea(iy0))
              else
                grdare0(ix0,iy0)=-9999
              endif
            end do
          end do
          deallocate(clat,carea)
        endif

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

      wfile1='./prmwat.bin'
      open(31,file=wfile1,form='unformatted',access='direct',recl=4*nXX*nYY)
      write(31,rec=1) prmwat
      close(31)



CONTAINS
      real*8 function rgetarea(rlon1, rlon2, rlat1, rlat2)
! ================================================
! to   calculate area of lat-lon bounding box
! by   algorithm by T. Oki, mathematics by S. Kanae, mod by nhanasaki
! on   26th Oct 2003
! at   IIS,UT
!
!     rlat1, rlat2 : latitude -90.0 (south pole) to 90.0 (north pole)
!     returns arealat : in [m^2]
!     by approximated equation
! ================================================
      implicit none
!
      real*8              ::  rlon1               !! longitude
      real*8              ::  rlon2               !! longitude
      real*8              ::  rlat1               !! latitude
      real*8              ::  rlat2               !! latitude
!
      real                ::  rpi                 !! Pi
      double precision    ::  dpi                 !! Pi
      double precision    ::  de                  !! e
      double precision    ::  de2                 !! e2
      double precision    ::  drad                !! radius of the earth
      double precision    ::  dfnc1               !! result of function for dlat1
      double precision    ::  dfnc2               !! result of function for dlat2
      double precision    ::  dsin1               !! result of sin(dlat1)
      double precision    ::  dsin2               !! result of sin(dlat2)
!
      data                    de2/0.00669447/
      data                    rpi/3.141592653589793238462643383/
      data                    dpi/3.141592653589793238462643383/
      data                    drad/6378136/
! ================================================
      de=sqrt(de2)

      rlat1=min(max(rlat1,-90.),90.)
      rlat2=min(max(rlat2,-90.),90.)
!
      if ((rlat1.gt.90).or.(rlat1.lt.-90).or.&
          (rlat2.gt.90).or.(rlat2.lt.-90)) then
!        write(6,*) 'rgetara: latitude out of range.'
!        write(*,*) 'rlon1(east) : ',rlon1
!        write(*,*) 'rlon2(west) : ',rlon2
!        write(*,*) 'rlat1(north): ',rlat1
!        write(*,*) 'rlat1(south): ',rlat2
        rgetarea = 0.0
      else
        dsin1 = dble(sin(rlat1 * rpi/180))
        dsin2 = dble(sin(rlat2 * rpi/180))
!
        dfnc1 = dsin1*(1+(de*dsin1)**2/2)
        dfnc2 = dsin2*(1+(de*dsin2)**2/2)
!
        rgetarea = real(dpi*drad**2*(1-de**2)/180*(dfnc1-dfnc2))*(rlon2-rlon1)
      end if
! ================================================
! Sign has been changed - to +.'
! ================================================
      if (rgetarea.lt.0.0) then
        rgetarea = - rgetarea
      end if

      end function rgetarea


      end program CALC_PRMWAT



