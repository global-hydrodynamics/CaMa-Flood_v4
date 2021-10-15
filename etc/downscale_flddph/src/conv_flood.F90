      program conv_flood
! ===============================================
      implicit none
! CaMa-Flood parameters
      real*8                   ::  west, east, north, south      !! domain (river network map)

      integer                  ::  ix, iy, jx, jy, dx, dy, dd, nd
      integer                  ::  mx, my
      real*8                   ::  csize                         !! size of pixel [deg]

      real,allocatable         ::  flddif(:,:)                   !! height above channel [m]
      real,allocatable         ::  flood(:,:)                    !! downscaled flood depth [m]

      real,allocatable         ::  slope(:,:), depth(:,:), tmp(:,:)
      real,allocatable         ::  out(:,:)
      integer                  ::  ngrid, mx2, my2
      real                     ::  maxdph

      character*256            ::  fflood, fout
      character*256            ::  cdate, hires
      character*256            ::  buf
! ===============================================
      call getarg(1,buf)
        read(buf,*) west
      call getarg(2,buf)
        read(buf,*) east
      call getarg(3,buf)
        read(buf,*) south
      call getarg(4,buf)
        read(buf,*) north

      call getarg(5,cdate)    !! downscale file

      call getarg(6,buf)
        read(buf,*) ngrid
      call getarg(7,hires)

      call getarg(8,buf)
        read(buf,*) maxdph

! ===============================================
      if( trim(hires)=='1sec' )then
        csize=1./3600.
      elseif( trim(hires)=='3sec' )then
        csize=1./1200.
      elseif( trim(hires)=='5sec' )then
        csize=1./720.
      elseif( trim(hires)=='15sec' )then
        csize=1./240.
      elseif( trim(hires)=='30sec' )then
        csize=1./120.
      elseif( trim(hires)=='1min' )then
        csize=1./60.
      else
        stop
      endif

      mx=nint( (east -west )/csize )
      my=nint( (north-south)/csize )


      allocate(flddif(mx,my),flood(mx,my))
      allocate(slope(mx,my),depth(mx,my),tmp(mx,my))

! ===============================================

      fflood='./flood/flood_'//trim(cdate)//'.bin'
      open(11, file=fflood, form='unformatted', access='direct', recl=4*mx)
      do iy=1, my
        read(11,rec=iy   ) flood(:,iy:iy)
      end do
      close(11)

      depth(:,:)=flood(:,:)
      do iy=1, my
        do ix=1, mx
          if( depth(ix,iy)<0.01 ) depth(ix,iy)=-9999
        end do
      end do

      if( mx>2500 )then
        tmp(:,:)=depth(:,:)
        dd=max(1,ngrid-1)
        nd=-dd
        if( mx<4000 ) nd=nd+1
        do iy=1, my
          do ix=1, mx
            if( tmp(ix,iy)>0 )then
              do dx=nd, dd
                do dy=nd, dd
                  jx=ix+dx
                  jy=iy+dy
                  if( jx>0 .and. jx<=mx .and. jy>0 .and. jy<=my )then
                    depth(jx,jy)=max(depth(jx,jy),tmp(ix,iy))
                  endif
                end do
              end do
            endif
          end do
        end do
      endif

      do iy=1, my
        do ix=1, mx
          if( depth(ix,iy)>1000 ) depth(ix,iy)=-9999
          if( depth(ix,iy)>0 )then
            if( depth(ix,iy)<maxdph )then
              depth(ix,iy)=depth(ix,iy)*0.8
            else
              depth(ix,iy)=maxdph*0.8+(depth(ix,iy)-maxdph)*0.2
              depth(ix,iy)=min(depth(ix,iy),maxdph)
            endif
          endif
        end do
      end do

      mx2=mx/ngrid
      my2=my/ngrid
      allocate(out(mx2,my2))
      out(:,:)=-9999

      do iy=1, my2
        do ix=1, mx2
          do dx=1, ngrid
            do dy=1, ngrid
              jx=dx+ngrid*(ix-1)
              jy=dy+ngrid*(iy-1)
              out(ix,iy)=max(out(ix,iy),depth(jx,jy))
            end do
          end do
        end do
      end do

      out(1,1)=0
      out(1,2)=maxdph

      fout='./dph'//trim(cdate)//'.bin'
      open(11, file=fout, form='unformatted', access='direct', recl=4*mx2)
      do iy=1, my2
        write(11,rec=iy) out(:,iy:iy)
      end do
      close(11)

      end program conv_flood

