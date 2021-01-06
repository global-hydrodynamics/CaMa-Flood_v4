      program calc_rivwth
! ================================================
      implicit none
! ===================
! calculation type
      character*256       ::  buf

! river network map parameters
      integer             ::  ix, iy, jx, jy, kx, ky
      integer             ::  nx, ny                  !! river map grid number
      real                ::  west, east, north, south
      real                ::  gsize


! river netwrok map
      integer,allocatable ::  nextx(:,:)      !! downstream x
      integer,allocatable ::  nexty(:,:)      !! downstream y
      real,allocatable    ::  uparea(:,:)

      integer,allocatable ::  basin(:,:)      !! downstream y
      integer,allocatable ::  color(:,:)      !! downstream y

! local
      integer             ::  ibsn, nbsn, jbsn
      integer,allocatable ::  bsn_mask(:,:)
      integer             ::  color_this, color_max, grid
      integer             ::  col_used(10), icol

      integer,allocatable::  basin_order(:), basin_new(:)
      real,allocatable   ::  basin_size(:)
      real               ::  upa_thrs
! file
      character*256       ::  finp, fparam, fout
      integer             ::  ios
! ================================================
! read parameters from arguments
      print *, '######################################################'
      print *, 'USAGE: Input Uparea Threshold [km2] :   ./get_subbasin $UPA_THRS'

      upa_thrs=100000.
      call getarg(1,buf)
      if( trim(buf)=='' )then
        stop
      else
        read(buf,*) upa_thrs
      endif

      fparam='../params.txt'
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

! ==============================

      allocate(nextx(nx,ny), nexty(nx,ny), uparea(nx,ny))
      allocate(basin(nx,ny), color(nx,ny))

! ===================

      finp='../nextxy.bin'
      open(11,file=finp,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
      read(11,rec=1) nextx
      read(11,rec=2) nexty
      close(11)

      finp='../uparea.bin'
      open(11,file=finp,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
      read(11,rec=1) uparea
      close(11)

      do iy=1, ny
        do ix=1, nx
          if( uparea(ix,iy)>0 ) uparea(ix,iy)=uparea(ix,iy)*1.e-6
        end do
      end do

! ===================
      do iy=1, ny
        do ix=1, nx
          if( uparea(ix,iy)>upa_thrs )then
            jx=nextx(ix,iy)
            jy=nexty(ix,iy)
            if( uparea(jx,jy)>uparea(ix,iy)+upa_thrs )then
              nextx(ix,iy)=-9
              nexty(ix,iy)=-9
            endif
          endif
        end do
      end do

      basin(:,:)=0
      ibsn=0

      do iy=1, ny
        do ix=1, nx
          if( nextx(ix,iy)==-9 .or. nextx(ix,iy)==-10 ) then
            if( uparea(ix,iy)>=upa_thrs )then
              ibsn=ibsn+1
              basin(ix,iy)=ibsn
            else
              basin(ix,iy)=-999
            endif
          endif
        end do
      end do
      nbsn=ibsn

      do iy=1, ny
        do ix=1, nx
          if( nextx(ix,iy)>0 .and. basin(ix,iy)==0 ) then
            jx=ix
            jy=iy
            do while( nextx(jx,jy)>0 .and. basin(jx,jy)==0 )
              kx=nextx(jx,jy)
              ky=nexty(jx,jy)
              jx=kx
              jy=ky
            end do
            jbsn=basin(jx,jy)

            jx=ix
            jy=iy
            do while( nextx(jx,jy)>0 .and. basin(jx,jy)==0 )
              basin(jx,jy)=jbsn
              kx=nextx(jx,jy)
              ky=nexty(jx,jy)
              jx=kx
              jy=ky
            end do
          endif
        end do
      end do

      allocate(basin_size(nbsn))
      allocate(basin_order(nbsn))
      allocate(basin_new(nbsn))
      do iy=1, ny
        do ix=1, nx
          if( nextx(ix,iy)<0 .and. nextx(ix,iy)/=-9999 ) then
            ibsn=basin(ix,iy)
            basin_size(ibsn)=uparea(ix,iy)
            basin_order(ibsn)=ibsn
          endif
        end do
      end do

! ==========


      call set_color

! ==========

      fout='../subbsn.bin'
      open(11,file=fout,form='unformatted',access='direct',recl=4*nx*ny)
      write(11,rec=1) basin
      close(11)

      fout='../subcol.bin'
      open(11,file=fout,form='unformatted',access='direct',recl=4*nx*ny)
      write(11,rec=1) color
      close(11)


CONTAINS
      subroutine set_color

print *, 'SET_MAP color basin'
      color(:,:)=-9999
      color_max=0
      allocate(bsn_mask(nbsn,4))
      bsn_mask(:,1)=999999
      bsn_mask(:,2)=999999
      bsn_mask(:,3)=-999999
      bsn_mask(:,4)=-999999
      do iy=1, ny
        do ix=1, nx
          if( basin(ix,iy)>0 )then
            ibsn=int( basin(ix,iy) )
            bsn_mask(ibsn,1) = min( bsn_mask(ibsn,1),ix )
            bsn_mask(ibsn,2) = min( bsn_mask(ibsn,2),iy )
            bsn_mask(ibsn,3) = max( bsn_mask(ibsn,3),ix )
            bsn_mask(ibsn,4) = max( bsn_mask(ibsn,4),iy )
          endif
        end do
      end do
      do ibsn=1, nbsn
        if( bsn_mask(ibsn,1)<=1 .or. bsn_mask(ibsn,3)>=nx )then
          bsn_mask(ibsn,1) = 1
          bsn_mask(ibsn,2) = max(bsn_mask(ibsn,2)-1,1 )
          bsn_mask(ibsn,3) = nx
          bsn_mask(ibsn,4) = min(bsn_mask(ibsn,4)+1,ny)
        else
          bsn_mask(ibsn,1) = bsn_mask(ibsn,1)-1
          bsn_mask(ibsn,2) = max(bsn_mask(ibsn,2)-1,1 )
          bsn_mask(ibsn,3) = bsn_mask(ibsn,3)+1
          bsn_mask(ibsn,4) = min(bsn_mask(ibsn,4)+1,ny)
        endif
      end do

! ==================
! = count basin grids num and
! = check color used in neighbour basin
! = (step1: large basin)
! ==================
      do ibsn=1, nbsn
        col_used=0
        grid=0
        do iy=bsn_mask(ibsn,2), bsn_mask(ibsn,4)
          do ix=bsn_mask(ibsn,1), bsn_mask(ibsn,3)
            if( basin(ix,iy)==ibsn )then
              grid=grid+1

              jx=ix
              jy=iy+1
              if( jx>=1 .and. jx<=nx .and. jy>=1 .and. jy<=ny) then
                if( color(jx,jy)>0 ) col_used(color(jx,jy))=1
              endif

              jx=ix+1
              jy=iy
              if( east-west==360 .and. jx>nx ) jx=1
              if( jx>=1 .and. jx<=nx .and. jy>=1 .and. jy<=ny) then
                if( color(jx,jy)>0 ) col_used(color(jx,jy))=1
              endif

              jx=ix
              jy=iy-1
              if( jx>=1 .and. jx<=nx .and. jy>=1 .and. jy<=ny) then
                if( color(jx,jy)>0 ) col_used(color(jx,jy))=1
              endif

              jx=ix-1
              jy=iy
              if( east-west==360 .and. jx==0 ) jx=nx
              if( jx>=1 .and. jx<=nx .and. jy>=1 .and. jy<=ny) then
                if( color(jx,jy)>0 ) col_used(color(jx,jy))=1
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

          do iy=bsn_mask(ibsn,2), bsn_mask(ibsn,4)
            do ix=bsn_mask(ibsn,1), bsn_mask(ibsn,3)
              if(basin(ix,iy)==ibsn) color(ix,iy)=color_this
            end do
          end do
        endif
      end do

! ==================
! = (step2: small basin)
! ==================
      do ibsn=1, nbsn
        col_used=0
        grid=0
        do iy=bsn_mask(ibsn,2), bsn_mask(ibsn,4)
          do ix=bsn_mask(ibsn,1), bsn_mask(ibsn,3)
            if( basin(ix,iy)==ibsn )then
              grid=grid+1

              jx=ix
              jy=iy+1
              if( jx>=1 .and. jx<=nx .and. jy>=1 .and. jy<=ny) then
                if( color(jx,jy)>0 ) col_used(color(jx,jy))=1
              endif

              jx=ix+1
              jy=iy
              if( east-west==360 .and. jx>nx ) jx=1
              if( jx>=1 .and. jx<=nx .and. jy>=1 .and. jy<=ny) then
                if( color(jx,jy)>0 ) col_used(color(jx,jy))=1
              endif

              jx=ix
              jy=iy-1
              if( jx>=1 .and. jx<=nx .and. jy>=1 .and. jy<=ny) then
                if( color(jx,jy)>0 ) col_used(color(jx,jy))=1
              endif

              jx=ix-1
              jy=iy
              if( east-west==360 .and. jx==0 ) jx=nx
              if( jx>=1 .and. jx<=nx .and. jy>=1 .and. jy<=ny) then
                if( color(jx,jy)>0 ) col_used(color(jx,jy))=1
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

          do iy=bsn_mask(ibsn,2), bsn_mask(ibsn,4)
            do ix=bsn_mask(ibsn,1), bsn_mask(ibsn,3)
              if(basin(ix,iy)==ibsn) color(ix,iy)=color_this
            end do
          end do
        endif
      end do

! ====================
      end subroutine set_color

!!================================================

      end program calc_rivwth
