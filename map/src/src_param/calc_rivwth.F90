      program calc_rivwth
! ================================================
! calculate river channel width and depth (rivwth.bin and rivhgt.bin) 
! using power-law function of annual mean discharge (outclm.bin)
! + distributed manning roughness is also generated (rivman.bin)
! ================================================
#ifdef UseCDF_CMF
USE NETCDF
#endif
      implicit none
! ===================
! calculation type
      character*256       ::  buf

      character*256       ::  type            !! 'bin' for binary, 'cdf' for netCDF
      data                    type /'bin'/

      character*256       ::  diminfo             !! dimention info file
      data                    diminfo /'./diminfo_test-1deg.txt'/

! parameter
      real                ::  HC, HP, HO, HMIN    !! Coef, Power, Offset, Minimum for Height (H=max(HMIN,HC*Qave**HP)
      real                ::  WC, WP, WO, WMIN    !! Coef, Power, Offset, Minimum for Width  (W=max(WMIN,WC*Qave**WP)

      data                    HC   /0.1/
      data                    HP   /0.5/
      data                    HO   /0.0/
      data                    HMIN /1.0/

      data                    WC   /0.40/
      data                    WP   /0.75/
      data                    WO   /0.00/
      data                    WMIN /10.0/

! river network map parameters
      integer             ::  ix, iy
      integer             ::  nx, ny          !! river map grid number
! river netwrok map
      integer,allocatable ::  nextx(:,:)      !! downstream x
      integer,allocatable ::  nexty(:,:)      !! downstream y
      real,allocatable    ::  lon(:), lat(:)  !! longitude, latitude [deg]
      real                ::  west, east, north, south, gsize
! variable
      real,allocatable    ::  rivout(:,:)     !! discharge     [m3/s]
      real,allocatable    ::  rivwth(:,:)     !! channel width [m]
      real,allocatable    ::  rivhgt(:,:)     !! channel depth [m]
      real,allocatable    ::  rivhgt_inf(:,:) !! for no-floodplain experiment
      real,allocatable    ::  rivman(:,:)     !! river channel manning roughness 
! file
      character*256       ::  cnextxy, crivout, crivwth, crivhgt, crivhgt_inf, crivman
      parameter              (crivout='./outclm.bin')
      parameter              (crivwth='./rivwth.bin')
      parameter              (crivhgt='./rivhgt.bin')
      parameter              (crivhgt_inf='./rivhgt_inf.bin')
      parameter              (crivman='./rivman.bin')
      integer             ::  ios
!
#ifdef UseCDF_CMF
      character*256       ::  crivpar
      parameter              (crivpar='./rivpar.nc')
      integer             ::  ncid,varid,xid,yid
#endif
! Undefined Values
      integer             ::  imis                !! integer undefined value
      real                ::  rmis                !! real    undefined value
      parameter              (imis = -9999)
      parameter              (rmis = 1.e+20)
! ================================================
print *, 'calc_rivwth :read parameters from arguments'

      call getarg(1,buf)
       if( buf/='' ) read(buf,*) type
      call getarg(2,buf)
       if( buf/='' ) read(buf,'(a128)') diminfo

      call getarg(3,buf)
       if( buf/='' ) read(buf,*) HC
      call getarg(4,buf)
       if( buf/='' ) read(buf,*) HP
      call getarg(5,buf)
       if( buf/='' ) read(buf,*) HO
      call getarg(6,buf)
       if( buf/='' ) read(buf,*) HMIN

      call getarg(7,buf)
       if( buf/='' ) read(buf,*) WC
      call getarg(8,buf)
       if( buf/='' ) read(buf,*) WP
      call getarg(9,buf)
       if( buf/='' ) read(buf,*) WO
      call getarg(10,buf)
       if( buf/='' ) read(buf,*) WMIN

      print *, 'TYPE=',    trim(type)
      print *, 'DIMINFO=', trim(diminfo)

      print *, 'HEIGHT H=max(', HMIN, ',', HC, '*Qave**',HP, '+', HO, ')'
      print *, 'WIDTH  W=max(', WMIN, ',', WC, '*Qave**',WP, '+', WO, ')'

! ===============================
! read dimention from diminfo

      if( type=='cdf')then
        print *, 'calculation for netCDF map'
      else
        type='bin'
        print *, 'calculation for binary map'
      endif

      open(11,file=diminfo,form='formatted')
      read(11,*) nx
      read(11,*) ny
      read(11,*) 
      read(11,*) 
      read(11,*) 
      read(11,*) 
      read(11,*) 
      read(11,*) west
      read(11,*) east
      read(11,*) north
      read(11,*) south
      close(11)

! ==============================
      print *, nx, ny

      allocate(nextx(nx,ny),nexty(nx,ny))
      allocate(rivout(nx,ny))
      allocate(rivwth(nx,ny),rivhgt(nx,ny),rivhgt_inf(nx,ny),rivman(nx,ny))

      allocate(lon(nx),lat(ny)) 
      gsize=(east-west)/real(nx)
      do ix=1,nx
        lon(ix)=west+(real(ix)-0.5)*gsize
      enddo
      do iy=1,ny
        lat(iy)=north-(real(iy)-0.5)*gsize
      enddo

! ===================
print *, 'calc_rivwth: read nextxy.bin'
      cnextxy='./nextxy.bin'
      open(11,file=cnextxy,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
      read(11,rec=1) nextx
      read(11,rec=2) nexty
      close(11)

      open(13,file=crivout,form='unformatted',access='direct',recl=4*nx*ny)
      read(13,rec=1) rivout
      close(13)

! =============================
print *, 'calc_rivwth: width & depth calculation'
      do iy=1, ny
        do ix=1, nx
          if( nextx(ix,iy)/=imis )then
            rivhgt(ix,iy)=max( HMIN,  HC  * rivout(ix,iy)**HP +HO )
            rivwth(ix,iy)=max( WMIN,  WC  * rivout(ix,iy)**WP +WO )
            rivman(ix,iy)=0.03
          else
            rivwth(ix,iy)=-9999
            rivhgt(ix,iy)=-9999
            rivman(ix,iy)=-9999
          endif
        end do
      end do
! =============================

      open(21,file=crivwth,form='unformatted',access='direct',recl=4*nx*ny)
      write(21,rec=1) rivwth
      close(21)
      open(22,file=crivhgt,form='unformatted',access='direct',recl=4*nx*ny)
      write(22,rec=1) rivhgt
      close(22)
      open(22,file=crivman,form='unformatted',access='direct',recl=4*nx*ny)
      write(22,rec=1) rivman
      close(22)

      do iy=1, ny
        do ix=1, nx
          rivhgt_inf(ix,iy)=100000.e0   !! very large rivhgt data, used for non-floodplain simulation
        end do
      end do

      open(23,file=crivhgt_inf,form='unformatted',access='direct',recl=4*nx*ny)
      write(23,rec=1) rivhgt_inf
      close(23)

#ifdef UseCDF_CMF
      if( type=='cdf' )then

        !! == create netcdf and define dimensions
        CALL NCERROR( NF90_CREATE(CRIVPAR, NF90_64BIT_OFFSET, NCID),'create netcdf output' )

        !!====
        !! DIMENSIONS
        CALL NCERROR( NF90_DEF_DIM(NCID,'lon',   NX,   XID),'lon def' )
        CALL NCERROR( NF90_DEF_DIM(NCID,'lat',   NY,   YID),'kat def' )

        !!=====
        !! VARIABLES
        CALL NCERROR( NF90_DEF_VAR(NCID, 'lon', NF90_FLOAT, (/XID/), VARID),'lon var' ) !DONE
        CALL NCERROR( NF90_PUT_ATT(NCID, VARID, 'long_name','Longitude') )
        CALL NCERROR( NF90_PUT_ATT(NCID, VARID, 'units','degrees_east') )

        CALL NCERROR( NF90_DEF_VAR(NCID, 'lat', NF90_FLOAT, (/YID/), VARID),'lat var' ) !DONE
        CALL NCERROR( NF90_PUT_ATT(NCID, VARID, 'long_name','Latitude') )
        CALL NCERROR( NF90_PUT_ATT(NCID, VARID, 'units','degrees_north') )

        CALL NCERROR( NF90_DEF_VAR(NCID, 'rivhgt', NF90_FLOAT, (/XID,YID/), VARID),'rivght def' ) ! done
        CALL NCERROR( NF90_PUT_ATT(NCID, VARID, 'long_name','river bank height') )
        CALL NCERROR( NF90_PUT_ATT(NCID, VARID, 'units','m') )
        CALL NCERROR( NF90_PUT_ATT(NCID, VARID, 'missing_value',rmis) )

        CALL NCERROR( NF90_DEF_VAR(NCID, 'rivwth', NF90_FLOAT, (/XID,YID/), VARID),'rivght def' ) ! done
        CALL NCERROR( NF90_PUT_ATT(NCID, VARID, 'long_name','river channel width') )
        CALL NCERROR( NF90_PUT_ATT(NCID, VARID, 'units','m') )
        CALL NCERROR( NF90_PUT_ATT(NCID, VARID, 'missing_value',rmis) )

        CALL NCERROR( NF90_ENDDEF(NCID) )

        !!=========================

        CALL NCERROR( NF90_INQ_VARID(NCID,'lon',VARID))
        CALL NCERROR( NF90_PUT_VAR(NCID,VARID,LON))

        CALL NCERROR( NF90_INQ_VARID(NCID,'lat',VARID))
        CALL NCERROR( NF90_PUT_VAR(NCID,VARID,LAT))

        CALL NCERROR( NF90_INQ_VARID(NCID,'rivhgt',VARID))
        CALL NCERROR( NF90_PUT_VAR(NCID,VARID,rivhgt))

        CALL NCERROR( NF90_INQ_VARID(NCID,'rivwth',VARID))
        CALL NCERROR( NF90_PUT_VAR(NCID,VARID,rivwth))

        CALL NCERROR( NF90_CLOSE(NCID) )
      endif
#endif
! === Finish =====================================



#ifdef UseCDF_CMF
      CONTAINS
!!================================================
      SUBROUTINE NCERROR(STATUS,STRING)
      USE NETCDF
      IMPLICIT NONE
      INTEGER,INTENT(IN) :: STATUS
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: STRING

! ======
      IF ( STATUS /= 0 ) THEN
        PRINT*, TRIM(NF90_STRERROR(STATUS))
        IF( PRESENT(STRING) ) PRINT*,TRIM(STRING)
        PRINT*,'PROGRAM STOP ! '
        STOP 10
      ENDIF
      END SUBROUTINE NCERROR
#endif
!!================================================

      end program calc_rivwth
