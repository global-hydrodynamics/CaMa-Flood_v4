      program set_bif_basin
! ================================================
! update basin data considering bifurcation channel connectivity
      implicit none
! ===================

! river network map parameters
      integer             ::  ix, iy, jx, jy
      integer             ::  nx, ny                  !! river map grid number
      real                ::  west, east, north, south
      real                ::  gsize


! river netwrok map
      integer,allocatable ::  nextx(:,:)      !! downstream x
      integer,allocatable ::  nexty(:,:)      !! downstream y
      integer,allocatable ::  basin(:,:)      !! downstream y

      integer,allocatable ::  bifmod(:,:)      !! downstream y
! bifurcation
      integer             ::  ipth, npth, nlev
      integer,allocatable ::  bnew(:), bmod(:)
! color for mpi basins
      integer,allocatable ::  color(:,:)      !! downstream y
      integer,allocatable ::  bsn_mask(:,:)
      integer             ::  color_this, color_max, grid
      integer             ::  col_used(10), icol
! local
      integer             ::  ibsn, nbsn, jbsn, kbsn
! 
      character*256       ::  finp, fparam, fout, fbifori
      integer             ::  ios
! ================================================
      print *, 'Update basin data considering bifurcation channel connectivity:'

! read parameters from arguments

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

      allocate(nextx(nx,ny), nexty(nx,ny), basin(nx,ny), bifmod(nx,ny),color(nx,ny))

! ===================

      finp='../nextxy.bin'
      open(11,file=finp,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
      read(11,rec=1) nextx
      read(11,rec=2) nexty
      close(11)

      finp='../basin.bin'
      open(11,file=finp,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
      read(11,rec=1) basin
      close(11)

      nbsn=0
      do iy=1, ny
        do ix=1, nx
          nbsn=max(nbsn,basin(ix,iy))
        end do
      end do

      print *,  'numnber of basins', nbsn

      allocate(bnew(nbsn),bmod(nbsn))
      bnew(:)=-9999
      bmod(:)=-9999

! --------
print *, 'read bifurcation channel data, and list basins to be merged'
      fbifori='../bifori.txt'
      open(12,file=fbifori,form='formatted')
      read(12,*) npth, nlev

      do ipth=1, npth
        read(12,*) ix,iy,jx,jy

        if( basin(ix,iy)/=basin(jx,jy) )then
          if( basin(ix,iy)<basin(jx,jy) )then
            ibsn=basin(ix,iy)
            jbsn=basin(jx,jy)
          else
            ibsn=basin(jx,jy)
            jbsn=basin(ix,iy)
          endif
          bnew(jbsn)=ibsn  !! jsbn should be integrated to ibsn
        endif
      end do

      close(12)

! --------
      do ibsn=1, nbsn
        if( bnew(ibsn)/=-9999 )then
          jbsn=bnew(ibsn)
          do while( bnew(jbsn)>0 )  !! if target basin is further merged to new basins
            kbsn=bnew(jbsn)
            jbsn=kbsn
          end do
          bnew(ibsn)=jbsn
        endif
      end do

      do ibsn=1, nbsn
        if( bnew(ibsn)/=-9999 )then
          jbsn=bnew(ibsn)
          bmod(ibsn)=1   !! basins to be merged
          bmod(jbsn)=2   !! basins to be expanded
        endif
      end do

! check merged grids
      bifmod(:,:)=-9999
      do iy=1, ny
        do ix=1, nx
          if( basin(ix,iy)>0 )then
            bifmod(ix,iy)=0
            ibsn=basin(ix,iy)
            if( bmod(ibsn)/=-9999 ) then
              bifmod(ix,iy)=bmod(ibsn)
            endif
          endif
        end do
      end do

! update basin number
      do iy=1, ny
        do ix=1, nx
          if( basin(ix,iy)>0 )then
            ibsn=basin(ix,iy)
            if( bnew(ibsn)/=-9999 ) then
              jbsn=bnew(ibsn)
              basin(ix,iy)=jbsn
            endif
          endif
        end do
      end do

      call set_color


! ==========

      fout='../bifbsn.bin'
      open(11,file=fout,form='unformatted',access='direct',recl=4*nx*ny)
      write(11,rec=1) basin
      close(11)

      fout='../bifmod.bin'
      open(11,file=fout,form='unformatted',access='direct',recl=4*nx*ny)
      write(11,rec=1) bifmod
      close(11)

      fout='../bifcol.bin'
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

      end program set_bif_basin
