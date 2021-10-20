      program calc_rivout
! ================================================
#ifdef UseCDF
USE NETCDF
#endif
      implicit none
! ==============================
! calculation type
      character*256       ::  buf

      character*256       ::  type                    !! 'bin' for binary, 'cdf' for netCDF
      character*256       ::  intp                    !! 'near' for nearest point interpolation, 'inpmat' for using input matrix
      data                    type /'bin'/
      data                    intp /'inpmat'/

! input files
      character*256       ::  diminfo                 !! dimention info file
      data                    diminfo /'./diminfo_test-1deg.txt'/

      character*256       ::  crofbin                 !! daily runoff climatology (plain binary), 365 day data in [mm/day]
      data                    crofbin /'../data/runoff_1981-2000_day.bin'/

      character*256       ::  crofcdf                 !! daily runoff climatology (/netCDF), 365 day data in [mm/day]
      character*256       ::  crofvar                 !! netCDF runoff variable name
      data                    crofcdf /'../data/rofcdf.nc'/
      data                    crofvar /'RO'/

#ifdef UseCDF
      integer             ::  ncid, varid
#endif

! dimention
      integer             ::  ix, iy, jx, jy
      integer             ::  nx, ny                  !! river map dimention
      integer             ::  nxin, nyin              !! input map dimention
      integer             ::  nflp                    !! floodplain layer

! parameter
      integer             ::  mday, dday              !! avaraging span, moving day
      parameter              (mday=30)                !!   30 day moving average
      parameter              (dday=5)                 !!   calculate moving ave. for each 5 day

! river network map
      integer,allocatable ::  nextx(:,:), nexty(:,:)  !! downstream xy
      integer,allocatable ::  rivseq(:,:)             !! river sequence
      real,allocatable    ::  ctmare(:,:)             !! unit-catchment area [m2]

      real,allocatable    ::  lon(:), lat(:)          !! longitude, latidude [deg]
      real                ::  west,east,north,south   !! domain boundary [deg]
      real                ::  gsize                   !! grid size [deg]

! input matrix
      integer             ::  i
      integer             ::  inpn                    !! max number of input grid for one river grid
      integer,allocatable ::  inpx(:,:,:),inpy(:,:,:) !! input grid (ixin,iyin) of river map grid (ix,iy)
      real,allocatable    ::  inpa(:,:,:)             !! input area [m2]

! input runoff
      real,allocatable    ::  roffin(:,:,:)           !! runof (input) [mm/day]

! variables
      real,allocatable    ::  runoff(:,:)             !! runof (converted) [m3/s]     
      real,allocatable    ::  rivout(:,:)             !! discharge [m3/s]
      real,allocatable    ::  rivout30(:,:)           !! annual max discharge, calculated from 30day mean

! 1d sequence for faster calculation
      integer,allocatable ::  i1seqx(:), i1seqy(:)
      integer             ::  iseq, nseq
      integer             ::  seqnow, seqmax, next

!
      integer             ::  iday, irec
      real,allocatable    ::  r2tmp(:,:), r2max1(:,:)

! files
      character*256       ::  cnextxy, cctmare        !! river network map, grid area
      integer             ::  ios

      character*256       ::  cinpmat                 !! input matrix

      character*256       ::  crivout                 !! annual max discharge [m3/s], rec=1: annual max;  rec=2; annual mean
      parameter              (crivout='./outclm.bin')

! undef
      integer             ::  imis                !! integer undefined value
      real                ::  rmis                !! real    undefined value
      parameter              (imis = -9999)
      parameter              (rmis = 1.e+20)
! ================================================
! read parameters from arguments

      call getarg(1,buf)
       if( buf/='' ) read(buf,*) type
      call getarg(2,buf)
       if( buf/='' ) read(buf,*) intp
      call getarg(3,buf)
       if( buf/='' ) read(buf,'(a128)') diminfo
      call getarg(4,buf)
       if( buf/='' )then
         if( type=='cdf' )then
           read(buf,'(a128)') crofcdf     !! bugfix v390
         else
           read(buf,'(a128)') crofbin
         endif
       endif
      if( type=='cdf' )then
        call getarg(5,buf)
        if( buf/='' ) read(buf,*) crofvar
      endif

      print *, 'TYPE=',    trim(type)
      print *, 'Interp=',  trim(intp)
      print *, 'DIMINFO=', trim(diminfo)
      if( type=='cdf' )then
        print *, 'Runoff=',  trim(crofcdf)
        print *, 'crofvar=', trim(crofvar)
      else
        print *, 'Runoff=',  trim(crofbin)
      endif

! ===============================
! read river network and input file dimentions

      if( type=='cdf')then
        print *, 'calculation for netCDF map'
      else
        type='bin'
        print *, 'calculation for binary map'
      endif

      open(11,file=diminfo,form='formatted')
      read(11,*) nx
      read(11,*) ny
      read(11,*) nflp
      read(11,*) nxin
      read(11,*) nyin
      read(11,*) inpn
      read(11,'(a)') cinpmat
      read(11,*) west
      read(11,*) east
      read(11,*) north
      read(11,*) south
      close(11)

      print *, trim(cinpmat)
      print *, nx, ny, nxin, nyin, inpn

      allocate(nextx(nx,ny),nexty(nx,ny),rivseq(nx,ny),ctmare(nx,ny))
      allocate(roffin(nxin,nyin,365+mday))
      allocate(runoff(nx,ny),rivout(nx,ny),rivout30(nx,ny))

      allocate(r2tmp(nxin,nyin),r2max1(nx,ny))

      allocate(i1seqx(nx*ny),i1seqy(nx*ny))

! ===========================================
      allocate(lon(nx),lat(ny))
      gsize=(east-west)/real(nx)
      do ix=1,nx
        lon(ix)=west+(real(ix)-0.5)*gsize
      enddo
      do iy=1,ny
        lat(iy)=north-(real(iy)-0.5)*gsize
      enddo
! =========

      if( intp/='near' )then
print *, 'set input matrix'
        allocate(inpx(nx,ny,inpn),inpy(nx,ny,inpn),inpa(nx,ny,inpn))
        open(11,file=cinpmat,form='unformatted',access='direct',recl=4*nx*ny)
        do i=1, inpn
          read(11,rec=       i) inpx(:,:,i:i)
          read(11,rec=  inpn+i) inpy(:,:,i:i)
          read(11,rec=2*inpn+i) inpa(:,:,i:i)
        end do
        close(11)
      endif
! ==========
      cnextxy='./nextxy.bin'
      open(11,file=cnextxy,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
      read(11,rec=1) nextx
      read(11,rec=2) nexty
      close(11)

      cctmare='./ctmare.bin'
      open(13,file=cctmare,form='unformatted',access='direct',recl=4*nx*ny)
      read(13,rec=1) ctmare
      close(13)
! ==========
print *, 'calculate river sequence'
      rivseq(:,:)=1
      do iy=1, ny
        do ix=1, nx
          if( nextx(ix,iy)>0 )then
            jx=nextx(ix,iy)
            jy=nexty(ix,iy)
            rivseq(jx,jy)=0
          elseif( nextx(ix,iy)==-9999 )then
            rivseq(ix,iy)=-9999
          endif
        end do
      end do

      seqnow=0
      next=1
      do while(next>0 )
        seqnow=seqnow+1
        next=0
        do iy=1, ny
          do ix=1, nx
            if( rivseq(ix,iy)==seqnow )then
              if( nextx(ix,iy)>0 )then
                jx=nextx(ix,iy)
                jy=nexty(ix,iy)
                if( rivseq(jx,jy)<=seqnow )then
                  next=next+1
                  rivseq(jx,jy)=seqnow+1
                endif
              endif
            endif
          end do
        enddo
      end do
      seqmax=seqnow
print *, seqmax

      iseq=0
      do seqnow=1, seqmax
        do iy=1, ny
          do ix=1, nx
            if( rivseq(ix,iy)==seqnow )then
              iseq=iseq+1
              i1seqx(iseq)=ix
              i1seqy(iseq)=iy
            endif
          end do
        enddo
      end do
      nseq=iseq
! ==========
print *, 'read runoff climatology file'
      if( type=='bin' )then
print *, trim(crofbin)
        open(14,file=crofbin,form='unformatted',access='direct',recl=4*nxin*nyin)
        irec=0
        roffin=0
        do irec=1, 365
          read(14,rec=irec) r2tmp
          roffin(:,:,irec)=r2tmp(:,:)
        end do
        do irec=1,mday
          read(14,rec=irec) r2tmp
          roffin(:,:,irec+365)=r2tmp(:,:)
        end do
        close(14)
      endif

#ifdef UseCDF
      if( type=='cdf')then
print *, trim(crofcdf)
        CALL NCERROR ( NF90_OPEN(crofcdf,NF90_NOWRITE,NCID),'READING'//TRIM(crofcdf) )
        CALL NCERROR ( NF90_INQ_VARID(NCID,crofvar,VARID) )
        irec=0
        roffin=0
        do irec=1, 365
          CALL NCERROR ( NF90_GET_VAR(NCID,VARID,r2tmp,(/1,1,irec/),(/nxin,nyin,1/)) )
          roffin(:,:,irec)=r2tmp(:,:)  
        end do
        do irec=1,mday
          CALL NCERROR ( NF90_GET_VAR(NCID,VARID,r2tmp,(/1,1,irec/),(/nxin,nyin,1/)) )
          roffin(:,:,irec+365)=r2tmp(:,:)
        end do
        CALL NCERROR ( NF90_CLOSE(NCID) )
      endif
#endif

      roffin(:,:,:)=roffin(:,:,:)/(24.*60.*60.)

! =======================================
print *, 'calc mday maximum: mday=', mday
      r2max1(:,:)=-9999
      do irec=1, 365, dday
        r2tmp(:,:)=0
        do iday=1, mday
          r2tmp(:,:)=r2tmp(:,:)+roffin(:,:,irec+iday-1)/real(mday)
        end do
        if( intp=='near' )then
          call conv_resol(nx,ny,nxin,nyin,imis,rmis,nextx,ctmare,r2tmp,runoff)
        else
          call intp_roff(nx,ny,nxin,nyin,imis,rmis,nextx,inpn,inpx,inpy,inpa,r2tmp,runoff)
        endif

        do iseq=1, nseq
          ix=i1seqx(iseq)
          iy=i1seqy(iseq)
          rivout(ix,iy)=runoff(ix,iy)
        end do

        do iseq=1, nseq
          ix=i1seqx(iseq)
          iy=i1seqy(iseq)
          if( nextx(ix,iy)>0 )then
            jx=nextx(ix,iy)
            jy=nexty(ix,iy)
            rivout(jx,jy)=rivout(jx,jy)+rivout(ix,iy)
          endif
        end do

        do iseq=1, nseq
          ix=i1seqx(iseq)
          iy=i1seqy(iseq)
          r2max1(ix,iy)=max(r2max1(ix,iy),rivout(ix,iy))
        end do
      end do

      rivout(:,:)=r2max1(:,:)
      rivout30(:,:)=rivout

! ============================================================
print *, 'calc annual average'
      r2tmp(:,:)=0.
      do irec=1, 365
        r2tmp(:,:)=r2tmp(:,:)+roffin(:,:,irec)/365.
      end do
      if( intp=='near' )then
        call conv_resol(nx,ny,nxin,nyin,imis,rmis,nextx,ctmare,r2tmp,runoff)
      else
        call intp_roff(nx,ny,nxin,nyin,imis,rmis,nextx,inpn,inpx,inpy,inpa,r2tmp,runoff)
      endif

      rivout(:,:)=-9999
      do iseq=1, nseq
        ix=i1seqx(iseq)
        iy=i1seqy(iseq)
        rivout(ix,iy)=runoff(ix,iy)
      end do

      do iseq=1, nseq
        ix=i1seqx(iseq)
        iy=i1seqy(iseq)
        if( nextx(ix,iy)>0 )then
          jx=nextx(ix,iy)
          jy=nexty(ix,iy)
          rivout(jx,jy)=rivout(jx,jy)+rivout(ix,iy)
        endif
      end do

!! annual average
      open(21,file=crivout,form='unformatted',access='direct',recl=4*nx*ny)
      write(21,rec=1) rivout       !! rec=1, annual average
      write(21,rec=2) rivout30     !! rec=2, max of 30day moving ave
      close(21)
! ============================================================

#ifdef UseCDF
      CONTAINS

!!==================================================
      SUBROUTINE NCERROR(STATUS,STRING)
      USE NETCDF

      IMPLICIT NONE
      INTEGER,INTENT(IN) :: STATUS
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: STRING

!!====================
      IF ( STATUS /= 0 ) THEN
        PRINT*, TRIM(NF90_STRERROR(STATUS))
        IF( PRESENT(STRING) ) PRINT*,TRIM(STRING)
        PRINT*,'PROGRAM STOP ! '
        STOP 10
      ENDIF

      END SUBROUTINE NCERROR
#endif

      end program calc_rivout




      subroutine intp_roff(nx,ny,nxin,nyin,imis,rmis,nextx,inpn,inpx,inpy,inpa,r2inp,r2out)
! ================================================
      implicit none
! input
      integer             ::  nx, ny, nxin, nyin  !! number of grids in horizontal
      integer             ::  imis                !! integer undefined value
      real                ::  rmis                !! real    undefined value
      integer             ::  nextx(nx,ny)      !! point downstream horizontal
!
      integer             ::  inpn
      integer             ::  inpx(nx,ny,inpn), inpy(nx,ny,inpn)
      real                ::  inpa(nx,ny,inpn)

      real                ::  r2inp (nxin,nyin)   !! runoff in [mm/s]
      real                ::  r2out(nx,ny)        !! runoff out [mm/s]
! local
      integer             ::  i
      integer             ::  ix, iy              !! for input
      integer             ::  jx, jy              !! for output
! ================================================
      r2out(:,:)=0
      do iy=1, ny
        do ix=1, nx
          if( nextx(ix,iy)/=imis )then
            do i=1, inpn
              jx=inpx(ix,iy,i)
              jy=inpy(ix,iy,i)
              if( jx>0 )then
                if( r2inp(jx,jy)/=rmis )then
                  r2out(ix,iy)=r2out(ix,iy)+max(r2inp(jx,jy),0.)*inpa(ix,iy,i)*1.e-3
                endif
              endif
            end do
          endif
        end do
      end do

      end subroutine intp_roff






      subroutine conv_resol(nx,ny,nxin,nyin,imis,rmis,nextx,ctmare,r2inp,r2out)
! ================================================
! to convert resolution
! by Dai YAMAZAKI
! on 30th Mar 2008
! at IIS,UT
! ================================================
      implicit none
! input
      integer             ::  nx                  !! number of grids in horizontal
      integer             ::  ny                  !! number of grids in vertical
      integer             ::  nxin                !! number of grids in horizontal
      integer             ::  nyin                !! number of grids in vertical
      integer             ::  imis                !! integer undefined value
      real                ::  rmis                !! real    undefined value
      integer             ::  nextx(nx,ny)      !! point downstream horizontal
      real                ::  ctmare(nx,ny)
!
      real                ::  r2inp (nxin,nyin) !! runoff in [mm/s]
      real                ::  r2out(nx,ny)     !! runoff out [mm/s]
! local
      integer             ::  ix, iy              !! for input
      integer             ::  jx, jy              !! for output
      integer             ::  xx, yy, by, n
! ================================================
      if( nx.ge.nxin )then
        by = nx/nxin
        do iy=1, nyin
          do ix =1,nxin
            do yy=1, by
              do xx=1, by
                jx=by*(ix-1)+xx
                jy=by*(iy-1)+yy
                if( nextx(jx,jy).ne.imis .and. r2inp(ix,iy).ne.rmis )then
                  r2out(jx,jy)=max(r2inp(ix,iy),0.)
                else
                  r2out(jx,jy)=0
                endif
              end do
            end do
          end do
        end do
!
      elseif( nx.lt.nxin )then
        by=nxin/nx
        do iy=1, ny
          do ix=1, nx
            r2out(ix,iy)=0
            if( nextx(ix,iy).ne.imis )then
              n=0
              do yy=1, by
                do xx=1, by
                  jx=by*(ix-1)+xx
                  jy=by*(iy-1)+yy
                  if( r2inp(jx,jy).ne.rmis )then
                    r2inp(jx,jy)=max(r2inp(jx,jy),0.)
                    r2out(ix,iy)=r2out(ix,iy)+r2inp(jx,jy)
                    n=n+1
                  endif
                end do
              end do
              r2out(ix,iy)=r2out(ix,iy)/real(n)
            endif
          end do
        end do
      endif

      do iy=1, ny
        do ix=1, nx
          if( nextx(ix,iy)/=imis )then
            r2out(ix,iy)=r2out(ix,iy)*ctmare(ix,iy)*1.e-3   !!  [mm] -> [m3]
          endif
        end do
      end do

      return
      end subroutine conv_resol
