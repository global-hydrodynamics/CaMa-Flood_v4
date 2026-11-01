      program SET_MAP
! ===============================================
! to set various maps
! ===============================================
      implicit none
! parameter
      character*256      ::  params
      parameter             (params='../params.txt')
! index CaMa-Flood
      integer            ::  iXX, iYY, jXX, jYY
      integer            ::  nXX, nYY                        !! x-y matrix GLOBAL
      real               ::  gsize                         !! grid size [degree]
      real               ::  dstmth
      parameter             (dstmth=25)
      real               ::  west, north, east, south
! input
      integer,allocatable::  nextX(:,:)                  !! next grid X
      integer,allocatable::  nextY(:,:)                  !! flow direction conbined
! output
      integer,allocatable::  rivseq(:,:)                 !! river sequence
      real,allocatable   ::  grdare(:,:)                 !! drainage area (GRID base)
      real,allocatable   ::  uparea(:,:)                 !! upa drainage area (GRID base)
      real,allocatable   ::  nxtdst(:,:)                 !! next dstistance
      integer,allocatable::  color(:,:)                  !! color of basin
      integer,allocatable::  basin(:,:)                  !! basin ID
      integer,allocatable::  upgrid(:,:)                 !! upstream grids
! 
      real,allocatable   ::  lon(:)
      real,allocatable   ::  lat(:)
      real               ::  n_lat, s_lat
! local
      integer,allocatable::  seqxy(:,:)
      integer            ::  nseq, nseq_max
      integer            ::  n, m, nmax
      real               ::  upa
      integer            ::  upg
      real               ::  lon1, lon2, lat1, lat2
      integer            ::  nbsn, basin_this, nbsn_max
      integer,allocatable::  bsn_mask(:,:)
      integer            ::  color_this, color_max, grid
      integer            ::  col_used(10), icol

      integer,allocatable::  basin_grid(:), basin_order(:), basin_new(:)
! file
      character*256      ::  rfile1,  rfile2
      character*256      ::  wfile1,  wfile3,  wfile4,  wfile5,  wfile5a, wfile6, wfile7
      parameter             (rfile1='../nextxy.bin')
      parameter             (rfile2='../grdare.bin')

      parameter             (wfile1='../rivseq.bin')
      parameter             (wfile3='../uparea_grid.bin')
      parameter             (wfile4='../upgrid.bin')
      parameter             (wfile5='../nxtdst_grid.bin')
      parameter             (wfile5a='../rivlen_grid.bin')
      parameter             (wfile6='../basin.bin')
      parameter             (wfile7='../bsncol.bin')
! ===============================================
      open(11,file=params,form='formatted')
      read(11,*) nXX
      read(11,*) nYY
      read(11,*) 
      read(11,*) gsize
      read(11,*) west
      read(11,*) east
      read(11,*) south
      read(11,*) north
      close(11)

      allocate(nextx(nXX,nYY), nexty(nXX,nYY), rivseq(nXX,nYY),grdare(nXX,nYY),uparea(nXX,nYY))
      allocate(nxtdst(nXX,nYY),color(nXX,nYY), basin(nXX,nYY), upgrid(nXX,nYY),lon(nXX),lat(nYY))
      allocate(seqxy(nXX*nYY,2))

      east =west +real(nXX)*gsize
      south=north-real(nYY)*gsize

      open(11, file=rfile1, form='unformatted', access='direct', recl=4*nXX*nYY)
      read(11,rec=1) nextX
      read(11,rec=2) nextY
      close(11)

      open(11, file=rfile2, form='unformatted', access='direct', recl=4*nXX*nYY)
      read(11,rec=1) grdare
      close(11)

! ===============================================
! set latitude and grid area
! ===============================================
print *, 'SET_MAP longitude, latitude and grid area'
      do iXX=1, nXX
        lon(iXX)=west+(iXX-0.5)*gsize
      end do
!
      do iYY=1, nYY
        lat(iYY)=north-(real(iYY)-0.5)*gsize
        n_lat=min(lat(iYY)+0.5*gsize, 90.0)
        s_lat=max(lat(iYY)-0.5*gsize,-90.0)
      end do
! ===============================================
! clac river sequence
! ===============================================
print *, 'SET_MAP calc river sequence'
      rivseq=1
      do iYY=1, nYY
        do iXX=1, nXX
          if( nextX(iXX,iYY)>0)then
            jXX=nextX(iXX,iYY)
            jYY=nextY(iXX,iYY)
            rivseq(jXX,jYY)=0
          elseif( nextX(iXX,iYY)==-9999 )then
            rivseq(iXX,iYY)=-9999
          endif
        end do
      end do

      nseq=2
      n=0
      do iYY=1, nYY
        do iXX=1, nXX
          if( rivseq(iXX,iYY)==1 .and. nextX(iXX,iYY)>0 )then
            jXX=nextX(iXX,iYY)
            jYY=nextY(iXX,iYY)
            rivseq(jXX,jYY)=nseq
            n=n+1
            seqxy(n,1)=jXX
            seqxy(n,2)=jYY
          endif
        end do
      end do
      nmax=n

      do while (nmax>0)
        nseq=nseq+1
        m=0
        do n=1, nmax
          iXX=seqxy(n,1)
          iYY=seqxy(n,2)
          if( nextX(iXX,iYY)>0 )then
            jXX=nextX(iXX,iYY)
            jYY=nextY(iXX,iYY)
            rivseq(jXX,jYY)=nseq
            m=m+1
            seqxy(m,1)=jXX
            seqxy(m,2)=jYY
          endif
        end do
        nmax=m
      end do

      nseq_max=nseq
print *, '  nseqmax=', nseq_max

      open(21, file=wfile1, form='unformatted', access='direct', recl=4*nXX*nYY)
      write(21,rec=1) rivseq
      close(21)
! ===============================================
! clac upa drainage area
! ===============================================
print *, 'SET_MAP calc upa drainage area'
      do iYY=1, nYY
        do iXX=1, nXX
          if( nextX(iXX,iYY)/=-9999 )then
            uparea(iXX,iYY)=0
            upgrid(iXX,iYY)=0
          else
            uparea(iXX,iYY)=-9999
            upgrid(iXX,iYY)=-9999
          endif
        end do
      end do

      do iYY=1, nYY
        do iXX=1, nXX
          if( rivseq(iXX,iYY)==1 ) then
            jXX=iXX
            jYY=iYY
            uparea(jXX,jYY)=grdare(jXX,jYY)
            upgrid(jXX,jYY)=1
            upa=uparea(jXX,jYY)
            upg=upgrid(jXX,jYY)
            do while( nextX(jXX,jYY)>0 )  !! if river reaches mouth, end loop
              call nextGRID(jXX,jYY,nextX(jXX,jYY),nextY(jXX,jYY))
              if( uparea(jXX,jYY)==0 )then                     !! grids firstly checked
                uparea(jXX,jYY)=upa+grdare(jXX,jYY)
                upa=uparea(jXX,jYY)
                upgrid(jXX,jYY)=upg+1
                upg=upgrid(jXX,jYY)
              else                                       !! grids already checked
                uparea(jXX,jYY)=uparea(jXX,jYY)+upa
                upgrid(jXX,jYY)=upgrid(jXX,jYY)+upg
              endif
            end do
          endif
        end do
      end do

      open(23, file=wfile3, form='unformatted', access='direct', recl=4*nXX*nYY)
      write(23,rec=1) uparea
      close(23)

      open(24, file=wfile4, form='unformatted', access='direct', recl=4*nXX*nYY)
      write(24,rec=1) upgrid
      close(24)
! ===============================================
! clac distance to next grid
! ===============================================
print *, 'SET_MAP calc distance to next grid'
      do iYY=1, nYY
        do iXX=1, nXX
          if( nextX(iXX,iYY)>0 ) then
            jXX=nextX(iXX,iYY)
            jYY=nextY(iXX,iYY)
            lon1=lon(iXX)
            lon2=lon(jXX)
            lat1=lat(iYY)
            lat2=lat(jYY)
            if( lon1<-90 .and. lon2>270 ) lon2=lon2-360
            if( lon1>270 .and. lon2<-90 ) lon2=lon2+360
            nxtdst(iXX,iYY)=rgetlen(lon1,lat1,lon2,lat2)
          elseif( nextX(iXX,iYY)/=-9999 )then
            nxtdst(iXX,iYY)=dstmth*1000.
          elseif( nextX(iXX,iYY)==-9999 )then
            nxtdst(iXX,iYY)=-9999
          endif
        end do
      end do

      open(25, file=wfile5, form='unformatted', access='direct', recl=4*nXX*nYY)
      write(25,rec=1) nxtdst
      close(25)

      open(25, file=wfile5a, form='unformatted', access='direct', recl=4*nXX*nYY)
      write(25,rec=1) nxtdst
      close(25)
! ===============================================
! clac basin
! ===============================================
print *, 'SET_MAP calc basin'
      basin=0
      nbsn=0

      do iYY=1, nYY
        do iXX=1, nXX
          if( rivseq(iXX,iYY)==1 ) then
            nbsn=nbsn+1
! loop until river reach mouth or already decided river
! ============================
            jXX=iXX
            jYY=iYY
            basin(jXX,jYY)=nbsn
!             do while( nextX(jXX,jYY)>0 .and. basin(nextX(jXX,jYY),nextY(jXX,jYY))==0 )
            do while( nextX(jXX,jYY)>0 )
              if ( basin(nextX(jXX,jYY),nextY(jXX,jYY))/=0 ) exit 
              call nextGRID(jXX,jYY,nextX(jXX,jYY),nextY(jXX,jYY))
              basin(jXX,jYY)=nbsn
            end do
! again loop in case basinIS is already decided
! ============================
!             if( nextX(jXX,jYY)>0 .and. basin(nextX(jXX,jYY),nextY(jXX,jYY))>0 )then
              if( nextX(jXX,jYY)>0  )then
                if ( basin(nextX(jXX,jYY),nextY(jXX,jYY))>0 ) then
                  nbsn=nbsn-1
                  basin_this=int( basin(nextX(jXX,jYY),nextY(jXX,jYY)) )
                  jXX=iXX
                  jYY=iYY
                  basin(jXX,jYY)=basin_this
                  do while( basin(nextX(jXX,jYY),nextY(jXX,jYY))/=basin_this )
                    call nextGRID(jXX,jYY,nextX(jXX,jYY),nextY(jXX,jYY))
                    basin(jXX,jYY)=basin_this
                  end do
                endif
              endif
! ============================
          endif
        end do
      end do
      nbsn_max=nbsn
print *, '  number of basin=', nbsn_max

      allocate(basin_grid(nbsn_max))
      allocate(basin_order(nbsn_max))
      allocate(basin_new(nbsn_max))
      do iYY=1, nYY
        do iXX=1, nXX
          if( nextx(iXX,iYY)<0 .and. nextx(iXX,iYY)/=-9999 ) then
            nbsn=basin(iXX,iYY)
            basin_grid(nbsn)=upgrid(iXX,iYY)
            basin_order(nbsn)=nbsn
          endif
        end do
      end do

      call heap_sort2(nbsn_max,basin_grid,basin_order)

      do nbsn=1, nbsn_max
        basin_this=basin_order(nbsn)
        basin_new(basin_this)=nbsn_max-nbsn+1
      end do

      do iYY=1, nYY
        do iXX=1, nXX
          if( nextx(iXX,iYY)/=-9999 ) then
            nbsn=basin(iXX,iYY)
            basin(iXX,iYY)=basin_new(nbsn)
          else
            basin(iXX,iYY)=-9999
          endif
        end do
      end do

      open(26, file=wfile6, form='unformatted', access='direct', recl=4*nXX*nYY)
      write(26,rec=1) basin
      close(26)
!
! ===============================================
! decide color of each basin for use in GMT
! ===============================================
print *, 'SET_MAP color basin'
      color=-9999
      color_max=0
      allocate(bsn_mask(nbsn_max,4))
      bsn_mask(:,1)=999999
      bsn_mask(:,2)=999999
      bsn_mask(:,3)=-999999
      bsn_mask(:,4)=-999999
      do iYY=1, nYY
        do iXX=1, nXX
          if( basin(iXX,iYY)>0 )then
            nbsn=int( basin(iXX,iYY) )
            bsn_mask(nbsn,1) = min( bsn_mask(nbsn,1),iXX )
            bsn_mask(nbsn,2) = min( bsn_mask(nbsn,2),iYY )
            bsn_mask(nbsn,3) = max( bsn_mask(nbsn,3),iXX )
            bsn_mask(nbsn,4) = max( bsn_mask(nbsn,4),iYY )
          endif
        end do
      end do
      do nbsn=1, nbsn_max
        if( bsn_mask(nbsn,1)<=1 .or. bsn_mask(nbsn,3)>=nXX )then
          bsn_mask(nbsn,1) = 1
          bsn_mask(nbsn,2) = max(bsn_mask(nbsn,2)-1,1 )
          bsn_mask(nbsn,3) = nXX
          bsn_mask(nbsn,4) = min(bsn_mask(nbsn,4)+1,nYY)
        else
          bsn_mask(nbsn,1) = bsn_mask(nbsn,1)-1
          bsn_mask(nbsn,2) = max(bsn_mask(nbsn,2)-1,1 )
          bsn_mask(nbsn,3) = bsn_mask(nbsn,3)+1
          bsn_mask(nbsn,4) = min(bsn_mask(nbsn,4)+1,nYY)
        endif
      end do

! ==================
! = count basin grids num and
! = check color used in neighbour basin
! = (step1: large basin)
! ==================
      do nbsn=1, nbsn_max
        col_used=0
        grid=0
        do iYY=bsn_mask(nbsn,2), bsn_mask(nbsn,4)
          do iXX=bsn_mask(nbsn,1), bsn_mask(nbsn,3)
            if( basin(iXX,iYY)==nbsn )then
              grid=grid+1

              jXX=iXX
              jYY=iYY+1
              if( jXX>=1 .and. jXX<=nXX .and. jYY>=1 .and. jYY<=nYY) then
                if( color(jXX,jYY)>0 ) col_used(color(jXX,jYY))=1
              endif

              jXX=iXX+1
              jYY=iYY
              if( east-west==360 .and. jXX>nXX ) jXX=1
              if( jXX>=1 .and. jXX<=nXX .and. jYY>=1 .and. jYY<=nYY) then
                if( color(jXX,jYY)>0 ) col_used(color(jXX,jYY))=1
              endif

              jXX=iXX
              jYY=iYY-1
              if( jXX>=1 .and. jXX<=nXX .and. jYY>=1 .and. jYY<=nYY) then
                if( color(jXX,jYY)>0 ) col_used(color(jXX,jYY))=1
              endif

              jXX=iXX-1
              jYY=iYY
              if( east-west==360 .and. jXX==0 ) jXX=nXX
              if( jXX>=1 .and. jXX<=nXX .and. jYY>=1 .and. jYY<=nYY) then
                if( color(jXX,jYY)>0 ) col_used(color(jXX,jYY))=1
              endif

            endif
          end do
        end do
! === decide color for large basin
        if( grid>=20 )then
          icol=2
          do while( col_used(icol)==1 )
            icol=icol+1
          end do
          color_this=icol
          if( color_max<color_this )color_max=color_this

          do iYY=bsn_mask(nbsn,2), bsn_mask(nbsn,4)
            do iXX=bsn_mask(nbsn,1), bsn_mask(nbsn,3)
              if(basin(iXX,iYY)==nbsn) color(iXX,iYY)=color_this
            end do
          end do
        endif
      end do

! ==================
! = (step2: small basin)
! ==================
      do nbsn=1, nbsn_max
        col_used=0
        grid=0
        do iYY=bsn_mask(nbsn,2), bsn_mask(nbsn,4)
          do iXX=bsn_mask(nbsn,1), bsn_mask(nbsn,3)
            if( basin(iXX,iYY)==nbsn )then
              grid=grid+1

              jXX=iXX
              jYY=iYY+1
              if( jXX>=1 .and. jXX<=nXX .and. jYY>=1 .and. jYY<=nYY) then
                if( color(jXX,jYY)>0 ) col_used(color(jXX,jYY))=1
              endif

              jXX=iXX+1
              jYY=iYY
              if( east-west==360 .and. jXX>nXX ) jXX=1
              if( jXX>=1 .and. jXX<=nXX .and. jYY>=1 .and. jYY<=nYY) then
                if( color(jXX,jYY)>0 ) col_used(color(jXX,jYY))=1
              endif

              jXX=iXX
              jYY=iYY-1
              if( jXX>=1 .and. jXX<=nXX .and. jYY>=1 .and. jYY<=nYY) then
                if( color(jXX,jYY)>0 ) col_used(color(jXX,jYY))=1
              endif

              jXX=iXX-1
              jYY=iYY
              if( east-west==360 .and. jXX==0 ) jXX=nXX
              if( jXX>=1 .and. jXX<=nXX .and. jYY>=1 .and. jYY<=nYY) then
                if( color(jXX,jYY)>0 ) col_used(color(jXX,jYY))=1
              endif

            endif
          end do
        end do
! === decide color for small
        if( grid<20 )then
          icol=2
          do while(col_used(icol)==1)
            icol=icol+1
          end do
          color_this=icol
          if(grid==1) color_this=1
          if(color_max<color_this) color_max=color_this

          do iYY=bsn_mask(nbsn,2), bsn_mask(nbsn,4)
            do iXX=bsn_mask(nbsn,1), bsn_mask(nbsn,3)
              if(basin(iXX,iYY)==nbsn) color(iXX,iYY)=color_this
            end do
          end do
        endif
      end do
print *, '  number of color=', color_max

      open(27, file=wfile7, form='unformatted', access='direct', recl=4*nXX*nYY)
      write(27,rec=1) color
      close(27)

! ====================
CONTAINS
!+
!+
!+
       subroutine nextGRID(iXX, iYY, jXX, jYY)
! ===============================================
       implicit none
       integer            ::  iXX, iYY, jXX, jYY
! ===============================================
       iXX=jXX
       iYY=jYY
       return
       end
!+
!+
!+
      subroutine heap_sort2(nmax,a,b)
! ===============================================
! to sort array by heap
! by Dai YAMAZAKI
! on 11th, Oct, 2008
! at IIS, u-tokyo
! ===============================================
      implicit none
!
      integer            ::  nmax
      integer            ::  a(nmax)
      integer            ::  b(nmax)
!
      integer            ::  i, n
      integer            ::  c
      integer            ::  d
      integer            ::  mod
! ===============================================
      i=int(nmax/2)
      n=i
 1000 continue
      if( n.eq.0 )goto 9900
      if( 2*n > nmax )then
        i=i-1
        n=i
        goto 1000
      else
        if( 2*n+1 > nmax )then
          if( a(2*n) > a(n) )then
            c=a(n)
            a(n)=a(2*n)
            a(2*n)=c
            d=b(n)
            b(n)=b(2*n)
            b(2*n)=d
            n=2*n
            goto 1000
          else
            i=i-1
            n=i
            goto 1000
          endif
        else
          if( a(n) >= a(2*n) .and. a(n) >= a(2*n+1) )then
            i=i-1
            n=i
            goto 1000
          elseif( a(2*n) > a(2*n+1) )then
            c=a(n)
            a(n)=a(2*n)
            a(2*n)=c
            d=b(n)
            b(n)=b(2*n)
            b(2*n)=d
            n=2*n
            goto 1000
          else
            c=a(n)
            a(n)=a(2*n+1)
            a(2*n+1)=c
            d=b(n)
            b(n)=b(2*n+1)
            b(2*n+1)=d
            n=2*n+1
            goto 1000
          endif
        endif
      endif
 9900 continue
!
      do n=1, nmax
        c=a(1)
        a(1)=a(nmax-n+1)
        a(nmax-n+1)=c
        d=b(1)
        b(1)=b(nmax-n+1)
        b(nmax-n+1)=d
        i=1
        mod=1
        do while (mod==1)
          mod=0
          if( 2*i <= nmax-n )then
            if( 2*i+1 <= nmax-n )then
              if( a(2*i)>a(i) .and. a(2*i)>=a(2*i+1) )then
                c=a(i)
                a(i)=a(2*i)
                a(2*i)=c
                d=b(i)
                b(i)=b(2*i)
                b(2*i)=d
                i=2*i
                mod=1
              elseif( a(2*i+1)>a(i) .and. a(2*i+1)>a(2*i) )then
                c=a(i)
                a(i)=a(2*i+1)
                a(2*i+1)=c
                d=b(i)
                b(i)=b(2*i+1)
                b(2*i+1)=d
                i=2*i+1
                mod=1
              endif
            else
              if( a(2*i)>a(i) )then
                c=a(i)
                a(i)=a(2*i)
                a(2*i)=c
                d=b(i)
                b(i)=b(2*i)
                b(2*i)=d
                i=2*i
                mod=1
              endif
            endif
          endif
        end do
      end do


      return
      end subroutine heap_sort2
!+
!+
!+
      real function rgetlen(rlon1, rlat1, rlon2, rlat2)
! ================================================
! to   get the length (m) between (rlon1, rlat1) to (rlon2, rlat2)
! by   nhanasaki
! on   1st Nov 2003
! at   IIS,UT
!
!     see page 643 of Rika-Nenpyo (2000)
!     at the final calculation, earth is assumed to be a sphere
! ================================================
      implicit none
      real                ::  rpi                !! Pi
      double precision    ::  de2                !! eccentricity powered by 2
      double precision    ::  da                 !! the radius of the earth
!
      real                ::  rlon1              !! longitude of the origin
      real                ::  rlon2              !! longitude of the destination
      real                ::  rlat1              !! latitude of the origin
      real                ::  rlat2              !! latitude of the destination
      double precision    ::  dsinlat1           !! sin(lat1)
      double precision    ::  dsinlon1           !! sin(lon1)
      double precision    ::  dcoslat1           !! cos(lat1)
      double precision    ::  dcoslon1           !! cos(lon1)
      double precision    ::  dsinlat2           !! sin(lat2) 
      double precision    ::  dsinlon2           !! sin(lon2)
      double precision    ::  dcoslat2           !! cos(lat2)
      double precision    ::  dcoslon2           !! cos(lon2)
      double precision    ::  dh1                !! hegiht of the origin
      double precision    ::  dn1                !! intermediate val of calculation
      double precision    ::  dx1                !! X coordinate of the origin
      double precision    ::  dy1                !! Y coordinate of the origin
      double precision    ::  dz1                !! Z coordinate of the origin
      double precision    ::  dh2                !! height of the destination
      double precision    ::  dn2                !! intermediate val of calculation
      double precision    ::  dx2                !! X coordinate of the destination
      double precision    ::  dy2                !! Y coordinate of the destination
      double precision    ::  dz2                !! Z coordinate of the destination
!
      double precision    ::  dlen               !! length between origin and destination
      double precision    ::  drad               !! half of the angle
! parameters
      data             da/6378137.0/
      data             de2/0.006694470/
      data             rpi/3.141592/      
! ================================================
! (lon1,lat1) --> (x1,y1,z1)
! ================================================
      dh1=0
      dh2=0

      dsinlat1 = dble(sin(rlat1 * rpi/180))
      dsinlon1 = dble(sin(rlon1 * rpi/180))
      dcoslat1 = dble(cos(rlat1 * rpi/180))
      dcoslon1 = dble(cos(rlon1 * rpi/180))
!
      dn1 = da/(sqrt(1.0-de2*dsinlat1*dsinlat1))
      dx1 = (dn1+dh1)*dcoslat1*dcoslon1
      dy1 = (dn1+dh1)*dcoslat1*dsinlon1
      dz1 = (dn1*(1-de2)+dh1)*dsinlat1
! ================================================
! (lon2,lat2) --> (x2,y2,z2)
! ================================================
      dsinlat2 = dble(sin(rlat2 * rpi/180))
      dsinlon2 = dble(sin(rlon2 * rpi/180))
      dcoslat2 = dble(cos(rlat2 * rpi/180))
      dcoslon2 = dble(cos(rlon2 * rpi/180))
!
      dn2 = da/(sqrt(1.0-de2*dsinlat2*dsinlat2))
      dx2 = (dn2+dh2)*dcoslat2*dcoslon2
      dy2 = (dn2+dh2)*dcoslat2*dsinlon2
      dz2 = (dn2*(1-de2)+dh2)*dsinlat2      
! ================================================
! Calculate length
! ================================================
      dlen=sqrt((dx1-dx2)**2+(dy1-dy2)**2+(dz1-dz2)**2)
      drad=dble(asin(real(dlen/2/da)))
      rgetlen=real(drad*2*da)
!
      return
      end function rgetlen
!+
!+
!+
      real function rgetara(rlon1, rlon2, rlat1, rlat2)
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
      if ((rlat1.gt.90).or.(rlat1.lt.-90).or.&
          (rlat2.gt.90).or.(rlat2.lt.-90)) then
        write(6,*) 'rgetara: latitude out of range.'
        write(*,*) 'rlon1(east) : ',rlon1
        write(*,*) 'rlon2(west) : ',rlon2
        write(*,*) 'rlat1(north): ',rlat1
        write(*,*) 'rlat1(south): ',rlat2
        rgetara = 0.0
      else
        dsin1 = dble(sin(rlat1 * rpi/180))
        dsin2 = dble(sin(rlat2 * rpi/180))
!
        dfnc1 = dsin1*(1+(de*dsin1)**2/2)
        dfnc2 = dsin2*(1+(de*dsin2)**2/2)
!
        rgetara = real(dpi*drad**2*(1-de**2)/180*(dfnc1-dfnc2))*(rlon2-rlon1)
      end if
! ================================================
! Sign has been changed - to +.'
! ================================================
      if (rgetara.lt.0.0) then
        rgetara = - rgetara
      end if
!
      return
      end function rgetara
!+
!+
!+
      end program SET_MAP

