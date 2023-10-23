      program GENERATE_INPMAT
! ===============================================
      implicit none
! CaMa-Flood parameters                                        !! (from param.txt)
      character*256          ::  param                         !! river map parameters
      parameter                 (param='../params.txt')
      integer                ::  iXX, iYY
      integer                ::  nXX, nYY                      !! grid number (river network map)
      integer                ::  nflp                          !! floodplain layers
      real                   ::  gsize                         !! grid size [deg]
      real                   ::  west, east, north, south      !! domain (river network map)

! HydroSHEDS parameters                                        !! (from location.txt)
      integer                ::  i, narea               !! area ID
      character*256          ::  area                          !! area code
      integer                ::  ix, iy                        
      integer                ::  nx, ny                        !! grid number (hires data)
      real                   ::  csize                         !! size of pixel [deg]
      real                   ::  lon_ori                       !! west  edge
      real                   ::  lat_ori                       !! north edge
!
      character*256          ::  list_loc

! =============================
!   input data parameters
!      please modify this part to generate input matrix for different input data resolution.
!      default: linert Cartesian grid

      integer            ::  ixin, iyin
      integer            ::  nxin, nyin                        !! input grid numbers
      real               ::  gsizein                           !! input grid size [deg]

      real               ::  westin, eastin, northin, southin
      character*4        ::  olat                              !! north-south data order. (NtoS): for North to South, StoN for South to North

      data                   gsizein /   1.0/
      data                   westin  /-180.0/
      data                   eastin  / 180.0/
      data                   northin /  90.0/
      data                   southin / -90.0/
      data                   olat    /'NtoS'/

      real               ::  lon0, lat0
! =============================

! Input mattrix
      integer                ::  inum, nmax, mmax              !! 
      parameter                 (nmax=100)
      integer,allocatable    ::  inpn(:,:)                     !! number of input grids for river network grid (iXX,iYY)
      integer,allocatable    ::  inpx(:,:,:), inpy(:,:,:)      !! input grid (ixin, iyin)
      real,allocatable       ::  inpa(:,:,:)                   !! area overlapped between river network grid (iXX,iYY) and input grid (ixin,iyin)

      integer,allocatable    ::  inpx0(:,:,:), inpy0(:,:,:)
      real,allocatable       ::  inpa0(:,:,:)

! HydroSHEDS hires info
      integer*2,allocatable  ::  catmxx(:,:), catmyy(:,:)      !! catchment (iXX,iYY)  of hires pixel (ix,iy)
      real,allocatable       ::  lon(:),  lat(:)               !! longitude & latitude of hires pixel (ix) (iy)
      real,allocatable       ::  carea(:,:)                      !! area of hires pixel (iy)

! for calculation
      integer                ::  new
      integer                ::  iXX0, iYY0

! files
      integer                ::  ios
      character*256          ::  rfile1, wfile1
      character*256          ::  mapdir, hires, inpmat, tag
      character*256          ::  diminfo
      parameter                 (mapdir='../')
      parameter                 (inpmat='inpmat_test-1deg.bin')
      parameter                 (diminfo='../diminfo_test-1deg.txt')
! function
      character*256          ::  buf
! ===============================================
print *, 'CALC_INPMAT:'

      call getarg(1,tag)
       if( tag=='' ) then
         print *, 'usage % generate_inpmat $HIRES $GSISE $WEST $EAST $NORTH $SOUTH $OLAT'
         stop
      endif

      hires=trim(mapdir)//'/'//trim(tag)//'/'

      nxin=nint( (eastin -westin )/gsizein )
      nyin=nint( (northin-southin)/gsizein )

      print *, 'INPUT DIMENSION'
      print *, 'nxin, nyin, gsizein:',      nxin,   nyin,   gsizein
      print *, 'west, east, north, south:', westin, eastin, northin, southin
      print *, 'north-south order:   ',     olat

      open(11,file=param,form='formatted')
      read(11,*) nXX
      read(11,*) nYY
      read(11,*) nflp
      read(11,*) gsize
      read(11,*) west
      read(11,*) east
      read(11,*) south
      read(11,*) north
      close(11)

      allocate(inpn(nXX,nYY),inpx(nXX,nYY,nmax),inpy(nXX,nYY,nmax),inpa(nXX,nYY,nmax))

      inpn(:,:)=0
      inpx(:,:,:)=0
      inpy(:,:,:)=0
      inpa(:,:,:)=0

      list_loc=trim(hires)//'/location.txt'
      open(11,file=list_loc,form='formatted')
      read(11,*) narea
      read(11,*)

      do i=1, narea
        read(11,*) buf, area, lon_ori, buf, buf, lat_ori, nx, ny, csize

        allocate(catmxx(nx,ny),catmyy(nx,ny))
        allocate(lon(nx),lat(ny),carea(nx,ny))

        rfile1=trim(hires)//trim(area)//'.catmxy.bin'
        print *, trim(rfile1)
        open(21,file=rfile1,form='unformatted',access='direct',recl=2*nx*ny,status='old',iostat=ios)
        if( ios==0 )then
          read(21,rec=1) catmXX
          read(21,rec=2) catmYY
          close(21)
        else
          print *, '*******************'
          print *, 'no data: ', area
          goto 1000
        endif

        do ix=1, nx
          lon(ix)=lon_ori+(real(ix)-0.5)*csize
          if( lon(ix)>=180. ) lon(ix)=lon(ix)-360.
          if( lon(ix)<-180. ) lon(ix)=lon(ix)+360.
        end do
        do iy=1, ny
          lat(iy) =lat_ori-(real(iy)-0.5)*csize
          do ix=1, nx
            carea(ix,iy)=rgetarea(0.,csize,lat(iy)-0.5*csize,lat(iy)+0.5*csize)
          end do
        end do

        rfile1=trim(hires)//'/'//trim(area)//'.grdare.bin'  !! bugfix v4.1 grarea->grdare
        open(21,file=rfile1,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
        if( ios==0 )then
          print *, 'Replace carea with file: '
          print *, trim(rfile1)
          read(21,rec=1) carea
          close(21)

          do iy=1, ny
            do ix=1, nx
              if( carea(ix,iy)>0 ) carea(ix,iy)=carea(ix,iy)*1.e6  !!! km2 -> m2
            end do
          end do

        endif

        do iy=1, ny
          do ix=1, nx
            if( catmxx(ix,iy)>0 )then
              iXX=catmxx(ix,iy)
              iYY=catmyy(ix,iy)

          ! #################################################################################################
          ! please modify the relation between hi-res pixel (ix,iy) and input grid (ixin,iyin)
          !   in ordert to generate new input matrix
          !
          ! the folloing is the example for 0.5deg global input
          ! #######

              lon0=lon(ix)
              lat0=lat(iy)

              if( lon0<westin ) lon0=lon0+360.
              if( lon0>eastin ) lon0=lon0-360.

              if( lon0>=westin .and. lon0<=eastin .and. lat0>=southin .and. lat0<=northin )then

                ixin=int( (lon0-westin )/gsizein )+1
                iyin=int( (northin-lat0)/gsizein )+1

                ixin=max(1,min(nxin,ixin))  !! bug fix 18 Apr 2014
                iyin=max(1,min(nyin,iyin))

                if( olat=='StoN' )then    !! when input runoff is stored from south to north
                  iyin=nyin-iyin+1
                endif

              else
                goto 2000
              endif

          ! #################################################################################################

              new=1
              if( inpn(iXX,iYY)>=1 )then
                do inum=1, inpn(iXX,iYY)
                  if( inpx(iXX,iYY,inum)==ixin .and. inpy(iXX,iYY,inum)==iyin )then
                    new=0
                    inpa(iXX,iYY,inum)=inpa(iXX,iYY,inum)+carea(ix,iy)
                  endif
                end do
              endif
              if( new==1 )then
                inum=inpn(iXX,iYY)+1
                if( inum>nmax )then
                  print *, '*** error: nmax overflow **********'
                  stop
                endif
                inpn(iXX,iYY)=inum
                inpx(iXX,iYY,inum)=ixin
                inpy(iXX,iYY,inum)=iyin
                inpa(iXX,iYY,inum)=carea(ix,iy)
              endif
 2000         continue
            endif
          end do
        end do

! *** end of area loop ***
 1000   continue
        deallocate(catmxx,catmyy,lon,lat,carea)
      end do

      mmax=0
      do iYY=1, nYY
        do iXX=1, nXX
          if( inpn(iXX,iYY)>mmax )then
            mmax=inpn(iXX,iYY)
            iXX0=iXX
            iYY0=iYY
          endif
        end do
      end do
      print *, 'input_num_max: ', mmax, 'lon lat: ', west+real(nXX-1)*gsize, north-real(nYY-1)*gsize, 'ix, iy: ', iXX0, iYY0

      open(11,file=diminfo,form='formatted')
      write(11,'(i10,5x,a)') nXX,          '!! nXX'
      write(11,'(i10,5x,a)') nYY,          '!! nYY'
      write(11,'(i10,5x,a)') nflp,         '!! floodplain layer'
      write(11,'(i10,5x,a)') nxin,         '!! input nXX '
      write(11,'(i10,5x,a)') nyin,         '!! input nYY '
      write(11,'(i10,5x,a)') mmax,         '!! input num'
      write(11,'(a)')        trim(inpmat)
      write(11,'(f12.3,5x,a)') west,         '!! west  edge'
      write(11,'(f12.3,5x,a)') east,         '!! east  edge'
      write(11,'(f12.3,5x,a)') north,        '!! north edge'
      write(11,'(f12.3,5x,a)') south,        '!! south edge'
      close(11)

      allocate(inpx0(nXX,nYY,mmax),inpy0(nXX,nYY,mmax),inpa0(nXX,nYY,mmax))
      do inum=1, mmax
        inpx0(:,:,inum)=inpx(:,:,inum)
        inpy0(:,:,inum)=inpy(:,:,inum)
        inpa0(:,:,inum)=inpa(:,:,inum)
      end do

      wfile1=trim(mapdir)//trim(inpmat)
print *, wfile1
      open(21,file=wfile1,form='unformatted',access='direct',recl=4*nXX*nYY*mmax)
      write(21,rec=1) inpx0
      write(21,rec=2) inpy0
      write(21,rec=3) inpa0
      close(21)

      deallocate(inpx0,inpy0,inpa0)
! ====================
CONTAINS
!+
!+
!+
     real function rgetarea(rlon1, rlon2, rlat1, rlat2)
! ================================================
! to   calculate area of 1 degree longitude box at each latitude
! by   algorithm by T. Oki, mathematics by S. Kanae, mod by nhanasaki
! on   26th Oct 2003
! at   IIS,UT
!
!     rlat1, rlat2 : latitude -90.0 (south pole) to 90.0 (north pole)
!     returns arealat : in m^2
!     by approximated equation
! ================================================
      implicit none
!
      real                ::  rlon1               !! longitude
      real                ::  rlon2               !! longitude
      real                ::  rlat1               !! latitude
      real                ::  rlat2               !! latitude
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
!
      if ((rlat1.gt.90.).or.(rlat1.lt.-90.).or.&
          (rlat2.gt.90.).or.(rlat2.lt.-90.)) then
        write(6,*) 'rgetarea: latitude out of range.'
        write(*,*) 'rlon1(east) : ',rlon1
        write(*,*) 'rlon2(west) : ',rlon2
        write(*,*) 'rlat1(north): ',rlat1
        write(*,*) 'rlat1(south): ',rlat2
        rgetarea = 0.0
      else
        dsin1 = dble(sin(rlat1 * rpi/180.D0))
        dsin2 = dble(sin(rlat2 * rpi/180.D0))
!
        dfnc1 = dsin1*(1+(de*dsin1)**2.D0 /2.D0)
        dfnc2 = dsin2*(1+(de*dsin2)**2.D0 /2.D0)
!
        rgetarea = real(dpi*drad**2*(1-de**2)/180*(dfnc1-dfnc2))*(rlon2-rlon1)
      end if
! ================================================
! Sign has been changed - to +.'
! ================================================
      if (rgetarea.lt.0.0) then
        rgetarea = - rgetarea
      end if
!
      return
      end function rgetarea
!+
!+
!+
      end program GENERATE_INPMAT

