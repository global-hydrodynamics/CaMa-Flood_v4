! MPI-specific basin aggregation for the v5 map-preparation workflow.
! Reads the ordinary bifprm.txt without changing it. Pathway order is explicit:
! parsed ibsn, then jbsn, then elevation, with the original text line as the
! deterministic final tie-breaker (matching the legacy sort's whole-line fallback).
program param_mpi_set_basin
  use map_color_mod, only: color_map
  implicit none

  ! One parsed bifurcation row; LINE preserves a deterministic final sort key.
  type                     :: pathway
    integer                  :: ix, iy  !! x/y grid index
    integer                  :: jx, jy  !! target x/y grid index
    integer                  :: ibsn  !! basin identifier
    integer                  :: jbsn  !! basin identifier
    real                     :: elv  !! pathway elevation [m]
    real                     :: w1
    character(len=2048)      :: line  !! formatted input row
  end type pathway

  ! Map geometry, path dimensions, merge thresholds, and counters.
  integer                  :: nx, ny  !! x/y grid dimensions
  integer                  :: npth  !! bifurcation-path count
  integer                  :: nlev  !! layer count
  integer                  :: nbsn  !! basin count or basin ID bound
  integer                  :: allgrid
  integer                  :: grid_thrs  !! combined basin threshold [cells]
  integer                  :: thrs2  !! small-basin threshold [cells]
  integer                  :: ios  !! I/O status code
  integer                  :: argc
  integer                  :: ix, iy  !! x/y grid index
  integer                  :: i
  integer                  :: k
  integer                  :: ibsn  !! basin identifier
  integer                  :: jbsn  !! basin identifier
  integer                  :: mark
  integer                  :: again
  integer                  :: pass
  integer                  :: u
  real                     :: gsize  !! map grid size [deg]
  real                     :: west, east, south, north  !! map bounds [deg]
  real                     :: rate  !! basin-merge threshold fraction
  real                     :: len  !! pathway length [m]
  real                     :: dph  !! bifurcation depth [m]
  real                     :: lat, lon  !! longitude/latitude [deg]

  ! Basin rasters and merge bookkeeping.
  integer, allocatable     :: basin(:,:)  !! basin identifier
  integer, allocatable     :: basin_color(:,:)  !! display color per aggregated basin
  integer, allocatable     :: basin_original(:,:)
  integer, allocatable     :: bnew(:)  !! basin merge destination ID
  integer, allocatable     :: bnum(:)  !! grid-cell count per basin
  integer, allocatable     :: bnum_original(:)
  integer, allocatable     :: bifmod_mpi(:,:)
  integer, allocatable     :: bpoint_mpi(:,:)

  ! Bifurcation widths and parsed pathway rows.
  real, allocatable        :: widths(:)  !! bifurcation widths [m]
  type(pathway), allocatable :: paths(:)  !! parsed bifurcation pathway records
  character(len=256)       :: arg  !! command-line argument
  character(len=256)       :: diagnostic_arg
  logical                  :: check_map

  ! ================================================
  ! [1] Select the merge mode and optional diagnostic output
  ! ================================================
  check_map=.false.
  call get_command_argument(1,arg)
  if (trim(arg) == 'MaxMPI') then
    rate=0.03
  else if (len_trim(arg) == 0 .or. trim(arg) == 'normal') then
    rate=0.06
  else
    write(*,'(a)') 'Usage: param_mpi_set_basin [normal|MaxMPI] [check_map]'
    stop 2
  end if
  call get_command_argument(2,diagnostic_arg)
  if (len_trim(diagnostic_arg)>0) then
    if (trim(diagnostic_arg)/='check_map') then
      write(*,'(a)') 'Usage: param_mpi_set_basin [normal|MaxMPI] [check_map]'
      stop 2
    end if
    check_map=.true.
  end if
  argc=command_argument_count()
  if (argc>2) then
    write(*,'(a)') 'Usage: param_mpi_set_basin [normal|MaxMPI] [check_map]'
    stop 2
  end if
  write(*,'(a,f6.3)') 'MPI basin-merge threshold fraction: ',rate

  ! ================================================
  ! [2] Read map geometry, basin IDs, and bifurcation pathways
  ! ================================================
  open(newunit=u,file='../params.txt',status='old',action='read',iostat=ios)
  if (ios /= 0) call die('cannot open ../params.txt')
  read(u,*,iostat=ios) nx
  if (ios==0) read(u,*,iostat=ios) ny
  if (ios==0) read(u,*,iostat=ios)
  if (ios==0) read(u,*,iostat=ios) gsize
  if (ios==0) read(u,*,iostat=ios) west
  if (ios==0) read(u,*,iostat=ios) east
  if (ios==0) read(u,*,iostat=ios) south
  if (ios==0) read(u,*,iostat=ios) north
  close(u)
  if (ios /= 0 .or. nx<1 .or. ny<1) call die('invalid map dimensions in ../params.txt')

  allocate(basin(nx,ny))
  open(newunit=u,file='../basin.bin',form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
  if (ios /= 0) call die('cannot open ../basin.bin')
  read(u,rec=1,iostat=ios) basin
  close(u)
  if (ios /= 0) call die('cannot read ../basin.bin record 1')
  if (check_map) then
    allocate(basin_original(nx,ny))
    basin_original=basin
  end if

  open(newunit=u,file='../bifprm.txt',status='old',action='read',iostat=ios)
  if (ios /= 0) call die('cannot open ../bifprm.txt')
  read(u,*,iostat=ios) npth,nlev
  if (ios /= 0 .or. npth<0 .or. nlev<1) call die('invalid bifprm.txt header')
  allocate(paths(npth),widths(nlev))
  do i=1,npth
    read(u,'(A)',iostat=ios) paths(i)%line
    if (ios /= 0) call die('unexpected end of ../bifprm.txt')
    read(paths(i)%line,*,iostat=ios) paths(i)%ix,paths(i)%iy,paths(i)%jx,paths(i)%jy, &
      len,paths(i)%elv,dph,(widths(k),k=1,nlev),lat,lon,paths(i)%ibsn,paths(i)%jbsn
    if (ios /= 0) call die('cannot parse bifprm.txt pathway; expected basin IDs at end of each row')
    paths(i)%w1=widths(1)
    if (min(paths(i)%ix,paths(i)%iy,paths(i)%jx,paths(i)%jy)<1 .or. &
        paths(i)%ix>nx .or. paths(i)%jx>nx .or. paths(i)%iy>ny .or. paths(i)%jy>ny) &
      call die('bifprm.txt contains an endpoint outside params.txt dimensions')
  end do
  close(u)

  ! [3] Sort pathways before the order-sensitive basin aggregation.
  ! The legacy shell sort's intended keys depend on BIFLAYER text columns.
  ! Sort these parsed fields instead, and retain its deterministic full-line
  ! fallback for equal keys. Sorting matters because the merge is order-sensitive.
  call quicksort(1,npth)

  ! ================================================
  ! [4] Count basin cells and derive size thresholds
  ! ================================================
  nbsn=maxval(basin)
  if (nbsn<1) call die('basin.bin contains no positive basin IDs')
  allocate(bnew(nbsn),bnum(nbsn))
  if (check_map) allocate(bnum_original(nbsn))
  bnew=-9999; bnum=0; allgrid=0
  do iy=1,ny
    do ix=1,nx
      ibsn=basin(ix,iy)
      if (ibsn>0) then
        bnum(ibsn)=bnum(ibsn)+1; allgrid=allgrid+1
      end if
    end do
  end do
  if (check_map) bnum_original=bnum
  if (allgrid<1) call die('basin.bin contains no valid river cells')
  grid_thrs=int(real(allgrid)*rate)
  thrs2=int(real(allgrid)*0.001)
  write(*,'(a,i0,a,i0)') 'Valid basin cells: ',allgrid,'; basin IDs: ',nbsn
  write(*,'(a,i0,a,i0)') 'Merge thresholds: combined < ',grid_thrs,' cells; small basin < ',thrs2

  ! ================================================
  ! [5] Merge channel bifurcations, then overland pathways
  ! ================================================
  ! Stage 1: integrate inter-basin channel bifurcations (width layer 1 > 0).
  pass=0
  do
    pass=pass+1
    bnew=-9999
    do i=1,npth
      if (paths(i)%w1<=0.0) cycle
      ix=paths(i)%ix; iy=paths(i)%iy; jbsn=basin(paths(i)%jx,paths(i)%jy); ibsn=basin(ix,iy)
      if (ibsn<=0 .or. jbsn<=0 .or. ibsn==jbsn) cycle
      if (ibsn>jbsn) then; mark=ibsn; ibsn=jbsn; jbsn=mark; end if
      if (bnum(ibsn)+bnum(jbsn)<grid_thrs .or. bnum(ibsn)<thrs2 .or. bnum(jbsn)<thrs2) then
        if (bnew(jbsn)==-9999 .or. bnew(jbsn)>ibsn) bnew(jbsn)=ibsn
      end if
    end do
    call commit_merges(.false.)
    again=0
    do i=1,npth
      if (paths(i)%w1<=0.0) cycle
      ibsn=basin(paths(i)%ix,paths(i)%iy); jbsn=basin(paths(i)%jx,paths(i)%jy)
      if (ibsn>0 .and. jbsn>0 .and. ibsn/=jbsn) then
        if (bnum(ibsn)+bnum(jbsn)<grid_thrs .or. bnum(ibsn)<thrs2 .or. bnum(jbsn)<thrs2) again=again+1
      end if
    end do
    write(*,'(a,i0,a,i0)') 'Channel merge pass ',pass,', remaining eligible connections: ',again
    if (again==0) exit
    if (pass>nbsn+1) call die('channel aggregation did not converge')
  end do

  ! Stage 2: integrate remaining overland/floodplain connections in reverse order.
  pass=0
  do
    pass=pass+1; bnew=-9999
    do i=npth,1,-1
      ibsn=basin(paths(i)%ix,paths(i)%iy); jbsn=basin(paths(i)%jx,paths(i)%jy)
      if (ibsn<=0 .or. jbsn<=0 .or. ibsn==jbsn) cycle
      if (ibsn>jbsn) then; mark=ibsn; ibsn=jbsn; jbsn=mark; end if
      if (bnum(ibsn)+bnum(jbsn)<grid_thrs .or. bnum(ibsn)<thrs2 .or. bnum(jbsn)<thrs2) then
        if (bnew(jbsn)==-9999 .or. bnew(jbsn)>ibsn) bnew(jbsn)=ibsn
      end if
    end do
    call commit_merges(.true.)
    again=0
    do i=1,npth
      ibsn=basin(paths(i)%ix,paths(i)%iy); jbsn=basin(paths(i)%jx,paths(i)%jy)
      if (ibsn>0 .and. jbsn>0 .and. ibsn/=jbsn) then
        if (bnum(ibsn)+bnum(jbsn)<grid_thrs .or. bnum(ibsn)<thrs2 .or. bnum(jbsn)<thrs2) again=again+1
      end if
    end do
    write(*,'(a,i0,a,i0)') 'Floodplain merge pass ',pass,', remaining eligible connections: ',again
    if (again==0) exit
    if (pass>nbsn+1) call die('floodplain aggregation did not converge')
  end do

  ! ================================================
  ! [6] Write MPI basin colors, optional diagnostics, and intermediate map
  ! ================================================
  ! This visualization belongs to the aggregation result and is independent
  ! of how many MPI regions are assigned later.
  allocate(basin_color(nx,ny))
  call color_map(basin,nx,ny,west,east,basin_color)
  open(newunit=u,file='../bifcol_mpi.bin',form='unformatted',access='direct', &
       recl=4*nx*ny,status='replace',iostat=ios)
  if (ios/=0) call die('cannot create ../bifcol_mpi.bin')
  write(u,rec=1,iostat=ios) basin_color
  if (ios/=0) call die('cannot write ../bifcol_mpi.bin record 1')
  close(u,iostat=ios)
  if (ios/=0) call die('cannot close ../bifcol_mpi.bin')
  write(*,'(a)') 'Created MPI-aggregated basin colors: ../bifcol_mpi.bin'

  ! Optional legacy-compatible diagnostic for basin changes and original
  ! bifurcation endpoints. It is written before region assignment and therefore
  ! does not depend on the number of MPI regions.
  if (check_map) call write_check_map()

  ! Temporary intermediate consumed by param_mpi_set_region; wrapper removes it.
  open(newunit=u,file='../bifbsn_mpi.tmp',form='unformatted',access='direct',recl=4*nx*ny,status='replace',iostat=ios)
  if (ios/=0) call die('cannot create ../bifbsn_mpi.tmp')
  write(u,rec=1,iostat=ios) basin
  close(u)
  if (ios/=0) call die('cannot write ../bifbsn_mpi.tmp')
  write(*,'(a)') 'Created temporary MPI basin groups: ../bifbsn_mpi.tmp'

contains
  subroutine die(message)
    character(len=*),intent(in) :: message
    write(*,'(a)') 'param_mpi_set_basin: ERROR: '//trim(message)
    stop 1
  end subroutine die

  logical function before(a,b)
    type(pathway),intent(in) :: a
    type(pathway),intent(in) :: b
    if (a%ibsn/=b%ibsn) then; before=a%ibsn<b%ibsn
    else if (a%jbsn/=b%jbsn) then; before=a%jbsn<b%jbsn
    else if (a%elv/=b%elv) then; before=a%elv<b%elv
    else; before=a%line<b%line
    end if
  end function before

  recursive subroutine quicksort(lo,hi)
    integer,intent(in)       :: lo
    integer,intent(in)       :: hi
    integer                  :: left
    integer                  :: right
    type(pathway)            :: pivot
    type(pathway)            :: swap
    if (lo>=hi) return
    left=lo; right=hi; pivot=paths((lo+hi)/2)
    do
      do while (before(paths(left),pivot)); left=left+1; end do
      do while (before(pivot,paths(right))); right=right-1; end do
      if (left>right) exit
      swap=paths(left); paths(left)=paths(right); paths(right)=swap
      left=left+1; right=right-1
      if (left>right) exit
    end do
    if (lo<right) call quicksort(lo,right)
    if (left<hi) call quicksort(left,hi)
  end subroutine quicksort

  subroutine write_check_map()
    integer, allocatable     :: bnum_final(:)
    integer                  :: record_unit  !! binary output unit
    integer                  :: class_value
    integer                  :: original_id
    character(len=64)        :: output_file

    allocate(bifmod_mpi(nx,ny),bpoint_mpi(nx,ny),bnum_final(nbsn))
    bnum_final=0
    do iy=1,ny
      do ix=1,nx
        ibsn=basin(ix,iy)
        if (ibsn>0) bnum_final(ibsn)=bnum_final(ibsn)+1
      end do
    end do

    ! Mark both endpoints of each original inter-basin pathway. If several
    ! pathways share a cell, retain the largest legacy class (3 > 2 > 1).
    bpoint_mpi=-9999
    do i=1,npth
      ix=paths(i)%ix; iy=paths(i)%iy
      jbsn=paths(i)%jx; k=paths(i)%jy
      if (basin_original(ix,iy)==basin_original(jbsn,k)) cycle
      if (basin(ix,iy)==basin(jbsn,k)) then
        class_value=2
        if (paths(i)%w1>0.0) class_value=3
      else
        class_value=1
      end if
      bpoint_mpi(ix,iy)=max(class_value,bpoint_mpi(ix,iy))
      bpoint_mpi(jbsn,k)=max(class_value,bpoint_mpi(jbsn,k))
    end do

    ! Classify each original basin cell by comparing its original basin ID's
    ! final size with that basin's size before MPI-specific aggregation.
    bifmod_mpi=-9999
    do iy=1,ny
      do ix=1,nx
        original_id=basin_original(ix,iy)
        if (original_id<=0) cycle
        bifmod_mpi(ix,iy)=0
        if (bnum_final(original_id)>bnum_original(original_id)) then
          bifmod_mpi(ix,iy)=2
        else if (bnum_final(original_id)==0) then
          bifmod_mpi(ix,iy)=1
        end if
      end do
    end do

    output_file='../check_bif_bsnmod_mpi.bin'
    open(newunit=record_unit,file=output_file,form='unformatted',access='direct', &
         recl=4*nx*ny,status='replace',iostat=ios)
    if (ios/=0) call die('cannot create '//trim(output_file))
    write(record_unit,rec=1,iostat=ios) bifmod_mpi
    if (ios/=0) call die('cannot write '//trim(output_file)//' record 1')
    write(record_unit,rec=2,iostat=ios) bpoint_mpi
    if (ios/=0) call die('cannot write '//trim(output_file)//' record 2')
    close(record_unit,iostat=ios)
    if (ios/=0) call die('cannot close '//trim(output_file))
    write(*,'(a)') 'Created optional MPI basin diagnostic: '//trim(output_file)
  end subroutine write_check_map

  subroutine commit_merges(reverse_order)
    logical,intent(in)       :: reverse_order
    integer                  :: first
    integer                  :: last
    integer                  :: step
    integer                  :: basin_id
    integer                  :: target
    integer                  :: origin
    if (reverse_order) then; first=nbsn; last=1; step=-1
    else; first=1; last=nbsn; step=1
    end if
    do basin_id=first,last,step
      if (bnew(basin_id)==-9999) cycle
      target=bnew(basin_id)
      do while (bnew(target)>0)
        target=bnew(target)
      end do
      if (bnum(basin_id)+bnum(target)<grid_thrs .or. bnum(basin_id)<thrs2 .or. bnum(target)<thrs2) then
        bnew(basin_id)=target
        bnum(target)=bnum(target)+bnum(basin_id)
        bnum(basin_id)=0
      else
        bnew(basin_id)=-9999
      end if
    end do
    do iy=1,ny
      do ix=1,nx
        origin=basin(ix,iy)
        if (origin>0) then
          if (bnew(origin)/=-9999) basin(ix,iy)=bnew(origin)
        end if
      end do
    end do
    bnew=-9999
  end subroutine commit_merges
end program param_mpi_set_basin
