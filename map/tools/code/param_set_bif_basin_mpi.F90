! MPI-specific basin aggregation for the v5 map-preparation workflow.
! Reads the ordinary bifprm.txt without changing it. Pathway order is explicit:
! parsed ibsn, then jbsn, then elevation, with the original text line as the
! deterministic final tie-breaker (matching the legacy sort's whole-line fallback).
program param_set_bif_basin_mpi
  implicit none
  type :: pathway
    integer :: ix, iy, jx, jy, ibsn, jbsn
    real :: elv, w1
    character(len=2048) :: line
  end type pathway
  integer :: nx, ny, npth, nlev, nbsn, allgrid, grid_thrs, thrs2, ios
  integer :: ix, iy, i, k, ibsn, jbsn, mark, again, pass
  integer :: u
  real :: gsize, west, east, south, north, rate, len, dph, lat, lon
  integer, allocatable :: basin(:,:), bnew(:), bnum(:)
  real, allocatable :: widths(:)
  type(pathway), allocatable :: paths(:)
  character(len=256) :: arg

  call get_command_argument(1,arg)
  if (trim(arg) == 'MaxMPI') then
    rate=0.03
  else if (len_trim(arg) == 0 .or. trim(arg) == 'normal') then
    rate=0.06
  else
    write(*,'(a)') 'Usage: param_set_bif_basin_mpi [normal|MaxMPI]'
    stop 2
  end if
  write(*,'(a,f6.3)') 'MPI basin-merge threshold fraction: ',rate

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

  ! The legacy shell sort's intended keys depend on BIFLAYER text columns.
  ! Sort these parsed fields instead, and retain its deterministic full-line
  ! fallback for equal keys. Sorting matters because the merge is order-sensitive.
  call quicksort(1,npth)

  nbsn=maxval(basin)
  if (nbsn<1) call die('basin.bin contains no positive basin IDs')
  allocate(bnew(nbsn),bnum(nbsn))
  bnew=-9999; bnum=0; allgrid=0
  do iy=1,ny
    do ix=1,nx
      ibsn=basin(ix,iy)
      if (ibsn>0) then
        bnum(ibsn)=bnum(ibsn)+1; allgrid=allgrid+1
      end if
    end do
  end do
  if (allgrid<1) call die('basin.bin contains no valid river cells')
  grid_thrs=int(real(allgrid)*rate)
  thrs2=int(real(allgrid)*0.001)
  write(*,'(a,i0,a,i0)') 'Valid basin cells: ',allgrid,'; basin IDs: ',nbsn
  write(*,'(a,i0,a,i0)') 'Merge thresholds: combined < ',grid_thrs,' cells; small basin < ',thrs2

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

  ! Temporary intermediate consumed by param_set_mpi_region; wrapper removes it.
  open(newunit=u,file='../.bifbsn_mpi.tmp',form='unformatted',access='direct',recl=4*nx*ny,status='replace',iostat=ios)
  if (ios/=0) call die('cannot create ../.bifbsn_mpi.tmp')
  write(u,rec=1,iostat=ios) basin
  close(u)
  if (ios/=0) call die('cannot write ../.bifbsn_mpi.tmp')
  write(*,'(a)') 'Created temporary MPI basin groups: ../.bifbsn_mpi.tmp'

contains
  subroutine die(message)
    character(len=*),intent(in) :: message
    write(*,'(a)') 'param_set_bif_basin_mpi: ERROR: '//trim(message)
    stop 1
  end subroutine die

  logical function before(a,b)
    type(pathway),intent(in) :: a,b
    if (a%ibsn/=b%ibsn) then; before=a%ibsn<b%ibsn
    else if (a%jbsn/=b%jbsn) then; before=a%jbsn<b%jbsn
    else if (a%elv/=b%elv) then; before=a%elv<b%elv
    else; before=a%line<b%line
    end if
  end function before

  recursive subroutine quicksort(lo,hi)
    integer,intent(in) :: lo,hi
    integer :: left,right
    type(pathway) :: pivot,swap
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

  subroutine commit_merges(reverse_order)
    logical,intent(in) :: reverse_order
    integer :: first,last,step,basin_id,target,origin
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
end program param_set_bif_basin_mpi
