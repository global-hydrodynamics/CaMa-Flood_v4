      program set_bif_basin
! ================================================
! update basin data considering bifurcation channel connectivity
      implicit none
! ===================

! river network map parameters
      integer             ::  ix, iy, jx, jy
      integer             ::  nx, ny                     !! river map grid number
      real                ::  west, east, north, south
      real                ::  gsize
!
      real                ::  rate                       !! max rate for basin integration (rate to global grid number)

! river netwrok map
      integer,allocatable ::  nextx(:,:)                 !! downstream x
      integer,allocatable ::  nexty(:,:)                 !! downstream y
      integer,allocatable ::  basin(:,:)                 !! basin

      integer,allocatable ::  bsnori(:,:)                !! to save original basin data for comparison
      integer,allocatable ::  bifmod(:,:)                !! highlihgt basins where connectivity is modified
      integer,allocatable ::  bpoint(:,:)                !! grids where inter-basin bifurcation happens
! bifurcation
      integer             ::  ipth, npth                 !! bifurcation pathway ID
      integer             ::  ilev, nlev                 !! bifurcation layers

      integer             ::  npth_mpi                   !! updated path ID

      integer,allocatable ::  bnew(:)                    !! new basin ID
      integer,allocatable ::  bmod(:)                    !! modification tag
      integer,allocatable ::  bnum(:),bnumori(:)         !! basin grid number

      real                ::  len, elv, dph              !! length, elevation, depth of the bifurcation channel
      real,allocatable    ::  wth(:)                     !! bifurcation width for each layer
      real                ::  lat, lon

! color for mpi basins
      integer,allocatable ::  color(:,:)                 !! basin color for visualization
      integer,allocatable ::  bsn_mask(:,:)
      integer             ::  color_this, color_max, grid
      integer             ::  col_used(10), icol
! local
      integer             ::  nbsn, ibsn, jbsn, kbsn
      integer             ::  allgrid, maxgrid, grid_thrs, thrs2

      integer             ::  mark
      integer             ::  again
! files
      character*256       ::  finp, fparam, fout, fbifori, fbifmpi
      integer             ::  ios
      character*256       ::  cfmt, clen, buf
! ================================================
      print *, 'Update basin ID considering inter-basin bifurcation:'

      mark=0
      call getarg(1,buf)
      if( trim(buf)=='MaxMPI' )then
        rate=0.03
      elseif( trim(buf)/='MaxMPI' )then
        rate=0.06
        print *, '######################################################'
        print *, 'If you want to use more MPI nodes : please add argument MaxMPI'
        print *, '% ./set_bif_basin MaxMPI '
        print *, ' '

        print *, 'When MaxMPI is specified:'
        print *, ' - a few inter-basin channels neglected, such as Amazon-Orinoco connection' 
        print *, ' - most inter-basin bifurcations in delta regions are still represented' 
        print *, ''       
        print *, 'Up to 16 MPI nodes available for usual case' 
        print *, 'When MaxMPI is specified, up to 30 MPI nodes are available' 
        print *, '######################################################'
        print *, ''       
        print *, ''       
      endif
print *, ' max rate: ', rate

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

      allocate(nextx(nx,ny), nexty(nx,ny), basin(nx,ny), bsnori(nx,ny ), color(nx,ny))
      allocate(bifmod(nx,ny),bpoint(nx,ny))

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

      bsnori(:,:)=basin(:,:)

      nbsn=0
      do iy=1, ny
        do ix=1, nx
          nbsn=max(nbsn,basin(ix,iy))  !! max basin ID
        end do
      end do
      print *,  'numnber of basins', nbsn

      allocate(bnew(nbsn),bmod(nbsn),bnum(nbsn),bnumori(nbsn))
      bnew(:)=-9999
      bmod(:)=-9999
      bnum(:)=0
      bnumori(:)=0

      print *, "count grid number of each basin"
      allgrid=0
      do iy=1, ny
        do ix=1, nx
          ibsn=basin(ix,iy)
          if( ibsn>0 )then
            bnum(ibsn)=bnum(ibsn)+1
            allgrid=allgrid+1
          endif
        end do
      end do
      bnumori(:)=bnum(:)

      maxgrid=0
      do ibsn=1, nbsn
        maxgrid=max(maxgrid,bnum(ibsn))
      end do
      print *, '-- max basin grid / all land grid, rate'
      print *, maxgrid, '/', allgrid, maxgrid/real(allgrid)

      fbifori='../bifprm.txt'
      open(12,file=fbifori,form='formatted')
      read(12,*) npth, nlev
      close(12)

      allocate( wth(nlev) )
print *, '******************'
!=================================================================
      grid_thrs = int( allgrid*rate  )
      thrs2     = int( allgrid*0.001 )

 500 continue

print *, 'STEP-1: merge basins with inter-basin channels'

      fbifori='../bifprm.txt'
      open(12,file=fbifori,form='formatted')
      read(12,*) npth, nlev

print *, '-- analyse inter-basin channels'
      do ipth=1, npth
        read(12,*) ix,iy,jx,jy, len, elv, dph, (wth(ilev),ilev=1,nlev), lat, lon
        if( wth(1)<=0 ) cycle                      !! no river bifurcation

        if( basin(ix,iy)/=basin(jx,jy) )then
          if( basin(ix,iy)<basin(jx,jy) )then
            ibsn=basin(ix,iy)
            jbsn=basin(jx,jy)
          else
            ibsn=basin(jx,jy)
            jbsn=basin(ix,iy)
          endif

          !! check size
          mark=0
          if( bnum(ibsn)+bnum(jbsn)<grid_thrs        ) mark=1  !! merge when sum of basin grid number < threshold-1 (avoid merge of very large basins)
          if( bnum(ibsn)<thrs2 .or. bnum(jbsn)<thrs2 ) mark=1  !! merge when one basin grid number < threshold-2 (small basins to be merged)

          if( mark==1 )then
            if( bnew(jbsn)==-9999 )then
              bnew(jbsn)=ibsn  !! jbsn should be integrated to ibsn
            elseif( bnew(jbsn)>ibsn )then
              bnew(jbsn)=ibsn  !! jbsn should be integrated to ibsn
            endif
          endif

        endif
      end do
      close(12)

print *, '-- decide basin IDs to be updated'
      do ibsn=1, nbsn
        if( bnew(ibsn)/=-9999 )then
          jbsn=bnew(ibsn)
          do while( bnew(jbsn)>0 )  !! if target basin is further merged to new basins
            kbsn=bnew(jbsn)
            jbsn=kbsn
          end do

          !! check size
          mark=0
          if( bnum(ibsn)+bnum(jbsn)<grid_thrs        ) mark=1  !! merge when sum of basin grid number < threshold-1 (avoid merge of very large basins)
          if( bnum(ibsn)<thrs2 .or. bnum(jbsn)<thrs2 ) mark=1  !! merge when one basin grid number < threshold-2 (small basins to be merged)

          if( mark==1 )then
            !!print *, ibsn, 'merged to ->', jbsn
            bnew(ibsn)=jbsn
            bnum(jbsn)=bnum(jbsn)+bnum(ibsn)  !! update number of grids in each basin
            bnum(ibsn)=0
          else
            bnew(ibsn)=-9999
          endif

        endif
      end do

print *, '-- update basin number'
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
      bnew(:)=-9999

print *, 'check all bifurcation merged or not' !! if one basin has multiple connection, avobe method does not work

      open(12,file=fbifori,form='formatted')
      read(12,*) npth, nlev

      again=0
      do ipth=1, npth
        read(12,*) ix,iy,jx,jy, len, elv, dph, (wth(ilev),ilev=1,nlev), lat, lon
        if( wth(1)<=0 ) cycle

        if( basin(ix,iy)/=basin(jx,jy) )then
          ibsn=basin(ix,iy)
          jbsn=basin(jx,jy)
          !! check size
          mark=0
          if( bnum(ibsn)+bnum(jbsn)<grid_thrs        ) mark=1  !! merge when sum of basin grid number < threshold-1 (avoid merge of very large basins)
          if( bnum(ibsn)<thrs2 .or. bnum(jbsn)<thrs2 ) mark=1  !! merge when one basin grid number < threshold-2 (small basins to be merged)

          if( mark==1 )then
            again=again+1
          endif
        endif
      end do
      close(12)

      if( again>0 )then
        print *, '-- repeat'
        goto 500
      endif

      maxgrid=0
      do ibsn=1, nbsn
        maxgrid=max(maxgrid,bnum(ibsn))
      end do

      print *, 'max grid number (when river bif is merged)', maxgrid, allgrid, maxgrid/real(allgrid)
      do ibsn=1, 20
        print *, ibsn, bnum(ibsn)
      end do

!**********************

print *, '*****************'
print *, 'STEP-2: merge basins with river bifurcation connectivity'


1000 continue

      fbifori='../bifprm.txt'
      open(12,file=fbifori,form='formatted')
      read(12,*) npth, nlev

print *, '-- analyse inter-basin channels (overland)'
      do ipth=npth, 1, -1
        read(12,*) ix,iy,jx,jy, len, elv, dph, (wth(ilev),ilev=1,nlev), lat, lon

        if( basin(ix,iy)/=basin(jx,jy) )then
          if( basin(ix,iy)<basin(jx,jy) )then
            ibsn=basin(ix,iy)
            jbsn=basin(jx,jy)
          else
            ibsn=basin(jx,jy)
            jbsn=basin(ix,iy)
          endif
          
          !! check size
          mark=0
          if( bnum(ibsn)+bnum(jbsn)<grid_thrs        ) mark=1  !! merge when sum of basin grid number < threshold-1 (avoid merge of very large basins)
          if( bnum(ibsn)<thrs2 .or. bnum(jbsn)<thrs2 ) mark=1  !! merge when one basin grid number < threshold-2 (small basins to be merged)

          if( mark==1 )then
            if( bnew(jbsn)==-9999 )then
              bnew(jbsn)=ibsn  !! jbsn should be integrated to ibsn
            elseif( bnew(jbsn)>ibsn )then
              bnew(jbsn)=ibsn  !! jbsn should be integrated to ibsn
            endif
          endif

        endif
      end do
      close(12)

! --------
print *, '-- decide basin IDs to be updated'
      do ibsn=nbsn, 1, -1
        if( bnew(ibsn)/=-9999 )then
          jbsn=bnew(ibsn)
          do while( bnew(jbsn)>0 )  !! if target basin is further merged to new basins
            kbsn=bnew(jbsn)
            jbsn=kbsn
          end do

          !! check size
          mark=0
          if( bnum(ibsn)+bnum(jbsn)<grid_thrs        ) mark=1  !! merge when sum of basin grid number < threshold-1 (avoid merge of very large basins)
          if( bnum(ibsn)<thrs2 .or. bnum(jbsn)<thrs2 ) mark=1  !! merge when one basin grid number < threshold-2 (small basins to be merged)

          if( mark==1 )then
            !!print *, ibsn, 'merged to ->', jbsn
            bnew(ibsn)=jbsn
            bnum(jbsn)=bnum(jbsn)+bnum(ibsn)  !! update number of grids in each basin
            bnum(ibsn)=0
          else
            bnew(ibsn)=-9999
          endif

        endif
      end do

      do ibsn=1, nbsn
        if( bnew(ibsn)/=-9999 )then
          jbsn=bnew(ibsn)
          bmod(ibsn)=1   !! basins modified
          bmod(jbsn)=1   !! basins modified
        endif
      end do

print *, '-- update basin number'
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
      bnew(:)=-9999

print *, 'check all bifurcation passway merged or not' !! if one basin has multiple connection, avobe method does not work

      open(12,file=fbifori,form='formatted')
      read(12,*) npth, nlev

      npth_mpi=0
      again=0
      do ipth=1, npth
        read(12,*) ix,iy,jx,jy, len, elv, dph, (wth(ilev),ilev=1,nlev), lat, lon

        ibsn=basin(ix,iy)
        jbsn=basin(jx,jy)
        if( ibsn==jbsn )then
          npth_mpi=npth_mpi+1
        else
          !! check size
          mark=0
          if( bnum(ibsn)+bnum(jbsn)<grid_thrs        ) mark=1  !! merge when sum of basin grid number < threshold-1 (avoid merge of very large basins)
          if( bnum(ibsn)<thrs2 .or. bnum(jbsn)<thrs2 ) mark=1  !! merge when one basin grid number < threshold-2 (small basins to be merged)

          if( mark==1 )then
            again=again+1
          endif
        endif
      end do
      close(12)

      if( again>0 )then
        print *, '-- repeat'
        goto 1000
      endif

      maxgrid=0
      do ibsn=1, nbsn
        maxgrid=max(maxgrid,bnum(ibsn))
      end do

      print *, 'max grid number (when overland pathway is merged)', maxgrid, allgrid, maxgrid/real(allgrid)
      do ibsn=1, 20
        print *, ibsn, bnum(ibsn)
      end do
!============================================
print *, '*************'
print *, 'Post Process: update basin color map'
      call set_color

print *, 'Mark inter-basin connectivity locations (bpoint.bin)'
      bpoint(:,:)=-9999
      open(12,file=fbifori,form='formatted')
      read(12,*) npth, nlev
      again=0
      do ipth=1, npth
        read(12,*) ix,iy,jx,jy, len, elv, dph, (wth(ilev),ilev=1,nlev), lat, lon
        if( bsnori(ix,iy)/=bsnori(jx,jy) )then
          if( basin(ix,iy)==basin(jx,jy) )then
            mark=2                                 !! 2= overland inter-basin connection
            if( wth(1)>0 ) mark=3                  !! 3= river channel inter-basin connection
            bpoint(ix,iy)=max(mark,bpoint(ix,iy))
            bpoint(jx,jy)=max(mark,bpoint(jx,jy))
          else
            mark=1                                 !! 1= inter-basin connection not represented in MPI region map
            bpoint(ix,iy)=max(mark,bpoint(ix,iy))
            bpoint(jx,jy)=max(mark,bpoint(jx,jy))
          endif
        endif
      end do
      close(12)

print *, 'Mark basins which exmands/absorved'
      bnum=0
      do iy=1, ny
        do ix=1, nx
          ibsn=basin(ix,iy)
          if( ibsn>0 )then
            bnum(ibsn)=bnum(ibsn)+1
          endif
        end do
      end do

      bifmod(:,:)=-9999
      do iy=1, ny
        do ix=1, nx
          if( bsnori(ix,iy)>0 )then
            bifmod(ix,iy)=0
            ibsn=bsnori(ix,iy)
            if( bnum(ibsn)>bnumori(ibsn) )then !! basins expanded by merging
              bifmod(ix,iy)=2
            elseif( bnum(ibsn)==0 )then        !! basins absorvd 
              bifmod(ix,iy)=1
            endif
          endif
        end do
      end do

!============================================
print *, 'Write to Files'

      fout='../bifbsn.bin'
      open(11,file=fout,form='unformatted',access='direct',recl=4*nx*ny)
      write(11,rec=1) basin
      close(11)

      fout='../bpoint.bin'
      open(11,file=fout,form='unformatted',access='direct',recl=4*nx*ny)
      write(11,rec=1) bpoint
      close(11)

      fout='../bifmod.bin'
      open(11,file=fout,form='unformatted',access='direct',recl=4*nx*ny)
      write(11,rec=1) bifmod
      close(11)

      fout='../bifcol.bin'
      open(11,file=fout,form='unformatted',access='direct',recl=4*nx*ny)
      write(11,rec=1) color
      close(11)
!============================================
print *, 'Update Bifparam file for MPI use'
      fbifmpi='../bifprm_mpi.txt'
      open(21,file=fbifmpi,form='formatted')
      write(21,'(2i8,a)') npth_mpi, nlev, &
               '   npath, nlev, (ix,iy), (jx,jy), length, elevtn, depth, (width1, width2, ... wodth_nlev), (lat,lon), basin'

      write(clen,'(i2)') 3+nlev
      cfmt='(4i8,'//trim(clen)//'f12.2,2f10.3,i8)'

!===
      open(12,file=fbifori,form='formatted')
      read(12,*) npth, nlev
      do ipth=1, npth
        read(12,*) ix,iy,jx,jy, len, elv, dph, (wth(ilev),ilev=1,nlev), lat, lon
        ibsn=basin(ix,iy)
        jbsn=basin(jx,jy)
        if( ibsn==jbsn ) then  !! write active bifurcation path after updating basins
          write(21,cfmt) ix, iy, jx, jy, len, elv, dph, (wth(ilev),ilev=1,nlev), lat, lon, ibsn
        endif
      end do
      close(12)
!===
      close(21)
!============================================

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
