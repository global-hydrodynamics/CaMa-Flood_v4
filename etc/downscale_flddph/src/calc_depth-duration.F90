      program depthduration
! ===============================================
      implicit none
! CaMa-Flood parameters       
      character*256            ::  param                         !! river map parameters
      integer                  ::  iXX, iYY
      integer                  ::  nXX, nYY                      !! grid number (river network map)
      integer                  ::  nflp                          !! floodplain layers
      real                     ::  gsize                         !! grid size [deg]
      real                     ::  west, east, north, south      !! domain (river network map)

      integer                  ::  irec, nrec
      integer                  ::  ilev, nlev
      real                     ::  maxdph, intdph, thrs

      integer*4,allocatable    ::  nextXX(:,:)                    !! downstream (jXX,jYY)
      real,allocatable         ::  flddph(:,:)                    !! flood depth [m] (coarse resolution)
      real,allocatable         ::  fldday(:,:,:)                  !! flood duration [day]

!
      character*256            ::  mapdir
      parameter                   (mapdir='./map/')              !! map directory (please make a symbolic link)
      character*256            ::  fnextxy, ffldday
      character*256            ::  fflddph
      character*256            ::  buf
! ===============================================
! downscale target domain
      call getarg(1,fflddph)    !! input  time-series data
      call getarg(2,ffldday)    !! output flood day   file

      call getarg(3,buf)
       read(buf,*) nrec         !! rec number of input file
      call getarg(4,buf)
       read(buf,*) maxdph       !! maximum flood depth (should be large enough, default=20m)
      call getarg(5,buf)
       read(buf,*) intdph       !! depth interaval for depth-duration calculation (default=0.05m)

      param=trim(mapdir)//'params.txt'

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

      nlev=int(maxdph/intdph)+1

! ==========

      allocate(nextXX(nXX,nYY),flddph(nXX,nYY))
      allocate(fldday(nXX,nYY,nlev))

! ===============================================
      fnextxy=trim(mapdir)//'nextxy.bin'
      open(11, file=fnextxy, form='unformatted', access='direct', recl=4*nXX*nYY)
      read(11,rec=1) nextXX
      close(11)

      fldday(:,:,:)=0
      do iYY=1, nYY
        do iXX=1, nXX
          if( nextXX(iXX,iYY)==-9999 )then
            fldday(iXX,iYY,:)=-9999
          endif
        end do
      end do

      open(12, file=fflddph, form='unformatted', access='direct', recl=4*nXX*nYY)
      do irec=1, nrec
        read(12,rec=irec) flddph

        do iYY=1, nYY
          do iXX=1, nXX
            if( nextXX(iXX,iYY)/=-9999 )then
              do ilev=1, nlev
                thrs=intdph*(ilev-1)
                thrs=max(thrs,0.01)
                if( flddph(iXX,iYY)>thrs )then
                  fldday(iXX,iYY,ilev)=fldday(iXX,iYY,ilev)+1.0
                endif
              end do
            endif
          end do
        end do
      end do
      close(12)

      print *, trim(ffldday)
      open(11, file=ffldday, form='unformatted', access='direct', recl=4*nXX*nYY)
      do ilev=1, nlev
        write(11,rec=ilev) fldday(:,:,ilev:ilev)
      end do
      close(11)

      end program depthduration

