! Shared v5 map-tool copy of map/src/src_param/set_bif_basin.F90.
! Scientific calculations are preserved; v5 diagnostic layout is described below.
! Generate bifbsn.bin by merging basin IDs connected through the pathways
! registered in bifprm.txt. STEP 1 handles river-channel bifurcations with a
! positive first-layer width; STEP 2 includes the remaining registered
! floodplain/overland pathways. STEP 2 does not evaluate instantaneous water
! levels or whether a pathway is active, so the result represents potential
! connectivity through the registered pathways.
! This is a general basin-connectivity preprocessing tool, independent of MPI.
! The current MPI domain-partitioning workflow generates a separate basin map,
! bifbsn_mpi.bin, using param_mpi_set_basin; it does not read this program's
! standard output bifbsn.bin as a direct input.
! It always writes bifcol.bin for merged-basin visualization. When invoked
! with the optional "check_map" argument, it writes check_bif_bsnmod.bin with
! bifmod and bpoint as records 1 and 2. Separate bifmod.bin and bpoint.bin are
! legacy outputs and are not produced by this v5 program.
! Run with CWD=<map>/script_params; input/output paths below are relative to it.
      program param_bif_set_basin
      use map_color_mod, only: color_map
      implicit none
! ================================================
! Basin-connectivity arrays and merge settings.
! ================================================

! River-network map geometry.
      integer                  :: ix, iy  !! x/y grid index
      integer                  :: jx, jy  !! target x/y grid index
      integer                  :: nx, ny  !! x/y grid dimensions
      real                     :: west, east, north, south  !! map bounds [deg]
      real                     :: gsize  !! map grid size [deg]

! River-network and basin maps.
      integer, allocatable      :: nextx(:,:)  !! downstream x grid index
      integer, allocatable      :: nexty(:,:)  !! downstream y grid index
      integer, allocatable      :: basin(:,:)  !! basin identifier

      integer, allocatable      :: bsnori(:,:)  !! original basin map for comparison
      integer, allocatable      :: bifmod(:,:)  !! basin modification class
      integer, allocatable      :: bpoint(:,:)  !! pathway endpoint class

! Bifurcation pathways and channel widths.
      integer                  :: ipth  !! bifurcation-path index
      integer                  :: npth  !! bifurcation-path count
      integer                  :: ilev  !! bifurcation-layer index
      integer                  :: nlev  !! layer count

      integer, allocatable      :: bnew(:)  !! basin merge destination ID
      integer, allocatable      :: bmod(:)  !! modification tag
      integer, allocatable      :: bnum(:)  !! grid-cell count per basin
      integer, allocatable      :: bnumori(:)  !! original grid-cell count per basin

      real                     :: len  !! pathway length [m]
      real                     :: elv  !! pathway elevation [m]
      real                     :: dph  !! bifurcation depth [m]
      real, allocatable         :: wth(:)  !! bifurcation width [m]
      real                     :: lat, lon  !! longitude/latitude [deg]

! Visualization colors for the merged basin map; not an MPI partition map.
      integer, allocatable      :: color(:,:)  !! basin display-color map

! The legacy visualization palette tracks ten color IDs; the greedy chooser
! below searches for an unused ID starting at 2.

! Working variables and merge counters.
      integer                  :: nbsn  !! basin count or basin ID bound
      integer                  :: ibsn  !! basin identifier
      integer                  :: jbsn  !! basin identifier
      integer                  :: kbsn  !! basin identifier
      integer                  :: nshown  !! number of basin sizes printed in diagnostics
      integer                  :: allgrid  !! total river-grid cells in the reported fraction
      integer                  :: maxgrid  !! largest merged-basin grid-cell count

      integer                  :: mark  !! pathway endpoint class written to bpoint
      integer                  :: again  !! number of eligible basin merges in the current pass

! Input and output filenames.
      character*256            :: finp
      character*256            :: fparam
      character*256            :: fout
      character*256            :: fbifori
      character*256            :: arg  !! command-line argument
      integer                  :: ios  !! I/O status code
      integer                  :: nargs
      logical                  :: write_check_map
! ================================================
      nargs=command_argument_count()
      write_check_map=.false.
      if( nargs>1 )then
        print *, 'ERROR: usage: param_bif_set_basin [check_map]'
        stop 1
      endif
      if( nargs==1 )then
        call getarg(1,arg)
        if( trim(adjustl(arg))=='check_map' )then
          write_check_map=.true.
        else
          print *, 'ERROR: optional argument must be check_map'
          stop 1
        endif
      endif

      print *, 'Update basin ID considering inter-basin bifurcation:'

! ================================================
! [1] Read map geometry, network maps, and bifurcation parameters
! ================================================
! Read base-map geometry from params.txt. The grid is traversed in Fortran array
! order (X first, then Y); map binaries use one direct record per 2-D field.
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

! Load the two river-network records and the original basin raster. Keep a copy
! of the original basin IDs for later diagnostic classification.
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

! Basin identifiers are used as array indices, so working arrays are sized to
! the largest positive basin ID found in the input raster.
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

! bifprm.txt header provides the number of retained paths and width layers.
! bnew maps each basin scheduled for merging to its selected destination ID.
      fbifori='../bifprm.txt'
      open(12,file=fbifori,form='formatted')
      read(12,*) npth, nlev
      close(12)

      allocate( wth(nlev) )
print *, '******************'
!=================================================================
! ================================================
! [2] Merge channel bifurcations, then potential floodplain connections
! ================================================
! STEP 1: merge basins connected by river-channel bifurcations identified by
! positive first-layer width (wth(1)>0). Repeat until those path endpoints
! have a common basin ID.
 500 continue

print *, 'STEP-1: merge basins with inter-basin channels'

      fbifori='../bifprm.txt'
      open(12,file=fbifori,form='formatted')
      read(12,*) npth, nlev

print *, '-- analyse inter-basin channels'
      do ipth=1, npth
        read(12,*) ix,iy,jx,jy, len, elv, dph, (wth(ilev),ilev=1,nlev), lat, lon
        if( wth(1)<=0 ) cycle                      !! not a river-channel bifurcation

        if( basin(ix,iy)/=basin(jx,jy) )then
          if( basin(ix,iy)<basin(jx,jy) )then
            ibsn=basin(ix,iy)
            jbsn=basin(jx,jy)
          else
            ibsn=basin(jx,jy)
            jbsn=basin(ix,iy)
          endif

          if( bnew(jbsn)==-9999 )then
            bnew(jbsn)=ibsn  !! merge the larger numeric basin ID into the smaller ID
          elseif( bnew(jbsn)>ibsn )then
            bnew(jbsn)=ibsn  !! retain the smaller numeric basin ID as representative
          endif

        endif
      end do
      close(12)

print *, '-- decide basin IDs to be updated'
! Resolve merge chains to their final representative ID, then update basin
! counts and apply the mapping. The representative is chosen by numeric basin
! ID, not by basin area or hydrological dominance.
      do ibsn=1, nbsn
        if( bnew(ibsn)/=-9999 )then
          jbsn=bnew(ibsn)
          do while( bnew(jbsn)>0 )  !! if target basin is further merged to new basins
            kbsn=bnew(jbsn)
            jbsn=kbsn
          end do
          bnew(ibsn)=jbsn
          bnum(jbsn)=bnum(jbsn)+bnum(ibsn)  !! update number of grids in each basin
          bnum(ibsn)=0
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

print *, 'check all bifurcation merged or not' !! repeated paths may require another merge pass

      open(12,file=fbifori,form='formatted')
      read(12,*) npth, nlev

      again=0
      do ipth=1, npth
        read(12,*) ix,iy,jx,jy, len, elv, dph, (wth(ilev),ilev=1,nlev), lat, lon
        if( wth(1)<=0 ) cycle

        if( basin(ix,iy)/=basin(jx,jy) )then
          again=again+1
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
! Show at most 20 non-empty basin IDs. bnum entries for basins absorbed by
! previous merges are zero; skip them and stop at the actual active-basin count.
      nshown=0
      do ibsn=1, nbsn
        if( bnum(ibsn)>0 )then
          print *, ibsn, bnum(ibsn)
          nshown=nshown+1
          if( nshown>=20 ) exit
        endif
      end do

!**********************

print *, '*****************'
print *, 'STEP-2: merge basins with all bifurcation pathways'


! STEP 2: consider every pathway registered in bifprm.txt, including the
! floodplain/overland pathways not merged in STEP 1. This is potential
! connectivity: no instantaneous water level or active-connection test is made.
! The loop index decreases, but each sequential formatted READ still consumes
! the next record in the original file order. Repeat until all path endpoints
! have a common basin ID. As in STEP 1, the smaller numeric basin ID represents
! each merged group; basin area and hydrological dominance are not considered.
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
          
          if( bnew(jbsn)==-9999 )then
            bnew(jbsn)=ibsn  !! merge the larger numeric basin ID into the smaller ID
          elseif( bnew(jbsn)>ibsn )then
            bnew(jbsn)=ibsn  !! retain the smaller numeric basin ID as representative
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
          bnew(ibsn)=jbsn
          bnum(jbsn)=bnum(jbsn)+bnum(ibsn)  !! update number of grids in each basin
          bnum(ibsn)=0
        endif
      end do

! Resolve the overland merge chains, mark source and destination basins for
! modification flags, and apply the resulting basin-ID mapping to each map cell.
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

print *, 'check all bifurcation pathways merged or not' !! repeated paths may require another merge pass

      open(12,file=fbifori,form='formatted')
      read(12,*) npth, nlev

      again=0
      do ipth=1, npth
        read(12,*) ix,iy,jx,jy, len, elv, dph, (wth(ilev),ilev=1,nlev), lat, lon
        if( basin(ix,iy)/=basin(jx,jy) )then
          again=again+1
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
      nshown=0
      do ibsn=1, nbsn
        if( bnum(ibsn)>0 )then
          print *, ibsn, bnum(ibsn)
          nshown=nshown+1
          if( nshown>=20 ) exit
        endif
      end do
!============================================
print *, '*************'
print *, 'Post Process: update basin color map'
! ================================================
! [3] Classify connections and assign visualization colors
! ================================================
! Assign neighboring basins visualization colors; this does not affect model
! calculations or the basin IDs written to bifbsn.bin.
      call color_map(basin,nx,ny,west,east,color)

print *, 'Mark inter-basin connectivity endpoints (bpoint array)'
! Mark both endpoints of each path that originally connected different basins.
! Codes: 1=the endpoints remain in different basins after this merge;
! 2=the overland connection was merged; 3=the river-channel connection was
! merged. This describes the resulting basin map, not an MPI region map. At a
! shared cell, the highest code is retained.
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
            mark=1                                 !! 1=original inter-basin pathway remains split in this basin map
            bpoint(ix,iy)=max(mark,bpoint(ix,iy))
            bpoint(jx,jy)=max(mark,bpoint(jx,jy))
          endif
        endif
      end do
      close(12)

print *, 'Mark basins that expand or are absorbed'
      bnum=0
      do iy=1, ny
        do ix=1, nx
          ibsn=basin(ix,iy)
          if( ibsn>0 )then
            bnum(ibsn)=bnum(ibsn)+1
          endif
        end do
      end do

! The bifmod array classifies original basin cells: 0=unchanged, 1=absorbed into
! another basin, and 2=the retained basin expanded through merging. Non-basin
! cells retain the legacy -9999 sentinel.
      bifmod(:,:)=-9999
      do iy=1, ny
        do ix=1, nx
          if( bsnori(ix,iy)>0 )then
            bifmod(ix,iy)=0
            ibsn=bsnori(ix,iy)
            if( bnum(ibsn)>bnumori(ibsn) )then !! basins expanded by merging
              bifmod(ix,iy)=2
            elseif( bnum(ibsn)==0 )then        !! basin absorbed into another basin
              bifmod(ix,iy)=1
            endif
          endif
        end do
      end do

!============================================
print *, 'Write to Files'

! ================================================
! [4] Write standard maps and optional diagnostic products
! ================================================
! Write the standard merged basin map and visualization colors. Keep both
! products unconditional; bifcol.bin is routinely used to inspect the map.
      fout='../bifbsn.bin'
      open(11,file=fout,form='unformatted',access='direct',recl=4*nx*ny)
      write(11,rec=1) basin
      close(11)

      fout='../bifcol.bin'
      open(11,file=fout,form='unformatted',access='direct',recl=4*nx*ny)
      write(11,rec=1) color
      close(11)

! Optional check output consolidates the existing diagnostic arrays without
! changing their classifications: record 1 is bifmod, record 2 is bpoint.
! Do not open this file in the standard mode, so a prior check file is kept.
      if( write_check_map )then
        fout='../check_bif_bsnmod.bin'
        open(11,file=fout,form='unformatted',access='direct',recl=4*nx*ny,status='replace')
        write(11,rec=1) bifmod
        write(11,rec=2) bpoint
        close(11)
      endif
!============================================
CONTAINS
! Compute basin bounding boxes and greedily assign visualization colors while
! avoiding colors already used by neighboring basins. These colors are for
! displaying the merged basin map and are not MPI rank or partition IDs.


!!================================================

      end program param_bif_set_basin
