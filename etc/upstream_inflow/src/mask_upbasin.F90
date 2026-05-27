      program mask_upbasin
! ================================================
      implicit none
! ===================
! river network map parameters
      integer             ::  ix, iy, jx, jy, kx, ky
      integer             ::  nx, ny                  !! river map grid number
! river netwrok map
      integer,allocatable ::  nextx(:,:)      !! downstream x
      integer,allocatable ::  nexty(:,:)      !! downstream y
      integer,allocatable ::  basin(:,:)      !! basin mask
      integer,allocatable ::  mask(:,:)      !! inflow mask
      integer,allocatable ::  point(:,:)     !! inflow points


      character*128       ::  gauge
      integer             ::  ix0, iy0
      integer             ::  imask

      integer             ::  ipoint, npoint
      integer             ::  list_basin(1000)

      real                ::  west, east, north, south
      real                ::  gsize

! file
      character*256       ::  mapdir,    fparam
      character*256       ::  nextxy_in, nextxy_out  !! river map
      character*256       ::  basin_in,  mask_out  !! river map

      character*256       ::  inflist
      integer             ::  ios

      character*256       ::  mode
! ================================================
print *, "read parameters from arguments"
      call getarg(1,mapdir)
      call getarg(2,nextxy_in)
      call getarg(3,basin_in)
      call getarg(4,nextxy_out)
      call getarg(5,mask_out)

      call getarg(6,inflist)
      call getarg(7,mode)

      fparam=trim(mapdir)//'/params.txt'
      open(11,file=fparam,form='formatted')
      read(11,*) nx
      read(11,*) ny
      read(11,*) 
      read(11,*) gsize
      read(11,*) west
      read(11,*) east
      read(11,*) south
      read(11,*) north
      close(11)

      print *, 'Map Domain W-E-S-N:', west, east, south, north
      print *, 'Map Resolution: ', gsize
      print *, 'Map NX,NY: ',  nx, ny

      allocate(nextx(nx,ny),nexty(nx,ny),basin(nx,ny),mask(nx,ny),point(nx,ny))
! ===================
print *, 'read files'
      nextxy_in=trim(mapdir)//'/'//trim(nextxy_in)
      print *, trim(nextxy_in)
      open(11,file=nextxy_in,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
      read(11,rec=1) nextx
      read(11,rec=2) nexty
      close(11)

      basin_in=trim(mapdir)//'/'//trim(basin_in)
      print *, trim(basin_in)
      open(11,file=basin_in,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
      read(11,rec=1) basin
      close(11)

      point(:,:)=0
      do iy=1, ny
        do ix=1, nx
          if( nextx(ix,iy)==-9999 ) point(ix,iy)=-9999
        end do
      end do

      npoint=0
      open(21,file=inflist,form="formatted")
      read(21,*)
 1000 continue
        read(21,*,end=1090) gauge, ix0, iy0
        if( nextx(ix0,iy0)==-9999 ) print *, 'error: [ix,iy] is not land', trim(gauge), ix0, iy0
        npoint=npoint+1
        point(ix0,iy0)=npoint
        list_basin(npoint)=basin(ix0,iy0)
        print *, npoint, trim(gauge), ix0, iy0
      goto 1000
 1090 continue

print *, 'calculate mask for inflow points'
      mask(:,:)=-3  !! outside of inflow point basin
      do iy=1, ny
        do ix=1, nx
          if( nextx(ix,iy)==-9999 ) mask(ix,iy)=-9999 !! ocean
          do ipoint=1, npoint
            if( basin(ix,iy)==list_basin(ipoint) ) mask(ix,iy)=-1  !! within inflow point basin
          end do
          if( trim(mode)=="keep" ) mask(ix,iy)=-1  !! if mode=keep, keep all basins regardless of inflow points
        end do
      end do

      !! check downstream of inflow points
      do iy=1, ny
        do ix=1, nx
          if( point(ix,iy)>0 )then
            mask(ix,iy)=point(ix,iy)  !! mask=gaugeID where inflow point exist
            jx=ix
            jy=iy
            do while( nextx(jx,jy)>0 )
              kx=nextx(jx,jy)
              ky=nexty(jx,jy)
              jx=kx
              jy=ky
              if( point(jx,jy)/=0 )exit
            end do
            if( point(jx,jy)>0 ) then
              print *, "Error: Inflow point exists downsream of another inflow point: ", ix0,iy0, jx,jy
              stop
            endif
          endif
        end do
      end do

      !! mark river mouths of basin with inflow point (mask==-1)
      do iy=1, ny
        do ix=1, nx
          if( nextx(ix,iy)<=0 .and. mask(ix,iy)==-1 )then
            mask(ix,iy)=0
          endif
        end do
      end do

      !! calculate mask for all grids
      !! -9999 : ocean
      !! -3    : outside of target basin with inflow points
      !! -1    : upstream of inflow points
      !!  0    : downstream of inflow poinsts
      !! >0    : inflow point locations
      do iy=1, ny
        do ix=1, nx
          if( mask(ix,iy)==-1 )then
            jx=ix
            jy=iy
            do while( nextx(jx,jy)>0 )
              kx=nextx(jx,jy)
              ky=nexty(jx,jy)
              jx=kx
              jy=ky
              if( mask(jx,jy) /=-1 )exit
            end do
            if( mask(jx,jy)>0 ) then
              imask=-2  !! upstream of inflow point
            else
              imask=mask(jx,jy)   !! downstream of inflow point
            endif

            mask(ix,iy)=imask
            jx=ix
            jy=iy
            do while( nextx(jx,jy)>0 )
              mask(jx,jy)=imask
              kx=nextx(jx,jy)
              ky=nexty(jx,jy)
              jx=kx
              jy=ky
              if( mask(jx,jy)>=0 )exit
            end do
          endif
        end do
      end do

      mask_out=trim(mapdir)//'/'//trim(mask_out)
      print *, trim(mask_out)
      open(31,file=mask_out,form='unformatted',access='direct',recl=4*nx*ny)
      write(31,rec=1) mask
      close(31)

print *, 'update river network with mask'
      do iy=1, ny
        do ix=1, nx
          if( mask(ix,iy)<0 )then
            nextx(ix,iy)=-9999
            nexty(ix,iy)=-9999
          endif
        end do
      end do
 
      nextxy_out=trim(mapdir)//'/'//trim(nextxy_out)
      print *, trim(nextxy_out)
      open(31,file=nextxy_out,form='unformatted',access='direct',recl=4*nx*ny)
      write(31,rec=1) nextx
      write(31,rec=2) nexty
      close(31)

!!================================================

      end program mask_upbasin
