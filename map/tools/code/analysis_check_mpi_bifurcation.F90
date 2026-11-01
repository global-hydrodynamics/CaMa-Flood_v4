! Classify bifurcation pathways using the model's CALC_1D_SEQ endpoint indexing.
! This includes the out-of-region downstream cell that CALC_1D_SEQ can append
! because its traversal step does not re-check the destination region label.
program analysis_check_mpi_bifurcation
  implicit none

  ! Map dimensions, region labels, and grid-traversal counters.
  integer                  :: nx, ny  !! x/y grid dimensions
  integer                  :: npth  !! bifurcation-path count
  integer                  :: nlev  !! layer count
  integer                  :: u
  integer                  :: ucsv  !! CSV output unit
  integer                  :: ios  !! I/O status code
  integer                  :: i
  integer                  :: k
  integer                  :: ix, iy  !! x/y grid index
  integer                  :: jx, jy  !! target x/y grid index
  integer                  :: regcount
  integer                  :: cat  !! pathway class: 1 retained, 2 dropped, 3 invalid
  integer                  :: region
  integer                  :: head
  integer                  :: tail
  integer                  :: total(3)  !! pathway counts by class
  integer                  :: channel(3)  !! channel-path counts by class
  integer                  :: flood(3)  !! floodplain-only counts by class
  integer                  :: cross_region_retained  !! retained cross-region pathway count

  ! Map arrays and queue used to reproduce model-local river-vector membership.
  integer, allocatable      :: regions(:,:)  !! MPI region labels
  integer, allocatable      :: nextx(:,:), nexty(:,:)  !! downstream cell x/y indices
  integer, allocatable      :: upst_count(:,:)  !! upstream-link count per cell
  integer, allocatable      :: upstream_now(:,:)  !! visited upstream-link count
  integer, allocatable      :: queue_x(:)
  integer, allocatable      :: queue_y(:)

  ! Bifurcation endpoints and their assigned MPI regions.
  integer, allocatable      :: path_ix(:), path_iy(:)
  integer, allocatable      :: path_jx(:), path_jy(:)
  integer, allocatable      :: region_up(:)  !! upstream endpoint region
  integer, allocatable      :: region_down(:)  !! downstream endpoint region

  ! Pathway classification flags and parsed physical attributes.
  logical, allocatable      :: vector(:,:)  !! model-local river-vector membership
  logical, allocatable      :: up_valid(:)
  logical, allocatable      :: down_valid(:)
  logical, allocatable      :: retained_path(:)
  logical, allocatable      :: valid_coords(:)
  real, allocatable         :: widths(:)  !! bifurcation widths [m]
  real, allocatable         :: path_elv(:)  !! pathway elevation [m]
  real, allocatable         :: path_lat(:)  !! pathway latitude [deg]
  real, allocatable         :: path_lon(:)  !! pathway longitude [deg]
  real, allocatable         :: path_w1(:)  !! first-layer bifurcation width [m]
  real                     :: len  !! pathway length [m]
  real                     :: dph  !! bifurcation depth [m]
  character(len=512)       :: region_file  !! MPI-region map filename
  character(len=512)       :: arg  !! command-line argument
  character(len=512)       :: csv_file  !! optional dropped-path CSV filename
  character(len=512)       :: line  !! formatted input row
  character(len=24)        :: kind_label  !! pathway classification label
  logical                  :: write_csv  !! whether to write the dropped-path CSV

  ! ================================================
  ! [1] Read command-line options and map geometry
  ! ================================================
  call get_command_argument(1,region_file)
  if(len_trim(region_file)==0) call die('usage: analysis_check_mpi_bifurcation <region-map.bin> [--csv <dropped.csv>]')
  write_csv=.false.; csv_file=''
  if(command_argument_count()>1) then
    call get_command_argument(2,arg)
    if(trim(arg)/='--csv' .or. command_argument_count()/=3) call die('usage: analysis_check_mpi_bifurcation <region-map.bin> [--csv <dropped.csv>]')
    call get_command_argument(3,csv_file); write_csv=.true.
  end if

  ! Standalone diagnostics run with the map root as their working directory.
  open(newunit=u,file='params.txt',status='old',action='read',iostat=ios)
  if(ios/=0) call die('cannot open params.txt')
  read(u,*,iostat=ios) nx
  if(ios==0) read(u,*,iostat=ios) ny
  close(u)
  if(ios/=0.or.nx<1.or.ny<1) call die('invalid map dimensions in params.txt')

  ! ================================================
  ! [2] Read the region map, river network, and bifurcation table
  ! ================================================
  allocate(regions(nx,ny),nextx(nx,ny),nexty(nx,ny))
  open(newunit=u,file=trim(region_file),form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
  if(ios/=0) call die('cannot open region map: '//trim(region_file))
  read(u,rec=1,iostat=ios) regions
  close(u)
  if(ios/=0) call die('cannot read region map record 1')
  open(newunit=u,file='nextxy.bin',form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
  if(ios/=0) call die('cannot open nextxy.bin')
  read(u,rec=1,iostat=ios) nextx
  if(ios==0) read(u,rec=2,iostat=ios) nexty
  close(u)
  if(ios/=0) call die('cannot read nextxy.bin records 1 and 2')

  open(newunit=u,file='bifprm.txt',status='old',action='read',iostat=ios)
  if(ios/=0) call die('cannot open bifprm.txt')
  read(u,*,iostat=ios) npth,nlev
  if(ios/=0.or.npth<0.or.nlev<1) call die('invalid bifprm.txt header')
  allocate(widths(nlev),path_ix(npth),path_iy(npth),path_jx(npth),path_jy(npth), &
           path_elv(npth),path_lat(npth),path_lon(npth),path_w1(npth),region_up(npth),region_down(npth))
  allocate(up_valid(npth),down_valid(npth),retained_path(npth),valid_coords(npth))
  do i=1,npth
    read(u,'(A)',iostat=ios) line
    if(ios/=0) call die('unexpected end of bifprm.txt')
    read(line,*,iostat=ios) path_ix(i),path_iy(i),path_jx(i),path_jy(i),len,path_elv(i),dph, &
      (widths(k),k=1,nlev),path_lat(i),path_lon(i)
    if(ios/=0) call die('cannot parse bifprm.txt pathway')
    path_w1(i)=widths(1)
  end do
  close(u)

  up_valid=.false.; down_valid=.false.; retained_path=.false.; valid_coords=.false.
  region_up=-9999; region_down=-9999
  do i=1,npth
    if(path_ix(i)<1.or.path_ix(i)>nx.or.path_iy(i)<1.or.path_iy(i)>ny .or. &
       path_jx(i)<1.or.path_jx(i)>nx.or.path_jy(i)<1.or.path_jy(i)>ny) cycle
    valid_coords(i)=.true.
    region_up(i)=regions(path_ix(i),path_iy(i))
    region_down(i)=regions(path_jx(i),path_jy(i))
  end do

  ! ================================================
  ! [3] Reconstruct model-local river-vector membership
  ! ================================================
  regcount=max(0,maxval(regions))
  allocate(upst_count(nx,ny),upstream_now(nx,ny),vector(nx,ny),queue_x(nx*ny),queue_y(nx*ny))
  ! Reproduce membership in I2VECTOR built by src/cmf_ctrl_maps_mod.F90:
  ! NUPST counts downstream links originating in REGIONTHIS; traversal then
  ! follows those links without an additional region check. This can expose one
  ! downstream boundary cell to the upstream partition's local vector.
  do region=1,regcount
    upst_count=0; upstream_now=0; vector=.false.; head=1; tail=0
    do iy=1,ny
      do ix=1,nx
        if(regions(ix,iy)==region .and. nextx(ix,iy)>0) then
          jx=nextx(ix,iy); jy=nexty(ix,iy)
          if(jx<1.or.jx>nx.or.jy<1.or.jy>ny) call die('nextxy.bin has an out-of-map downstream index')
          upst_count(jx,jy)=upst_count(jx,jy)+1
        end if
      end do
    end do
    ! Seed assigned river cells with no upstream links in this rank's NUPST.
    do iy=1,ny
      do ix=1,nx
        if(regions(ix,iy)==region .and. nextx(ix,iy)>0) then
          if(upst_count(ix,iy)==upstream_now(ix,iy)) then
            tail=tail+1
            if(tail>nx*ny) call die('model-style river traversal exceeded the map cell count')
            queue_x(tail)=ix; queue_y(tail)=iy; vector(ix,iy)=.true.
          end if
        end if
      end do
    end do
    do while(head<=tail)
      ix=queue_x(head); iy=queue_y(head); head=head+1
      jx=nextx(ix,iy); jy=nexty(ix,iy)
      if(jx<1.or.jx>nx.or.jy<1.or.jy>ny) call die('nextxy.bin has an out-of-map downstream index')
      upstream_now(jx,jy)=upstream_now(jx,jy)+1
      if(upstream_now(jx,jy)==upst_count(jx,jy) .and. nextx(jx,jy)>0) then
        if(.not.vector(jx,jy)) then
          tail=tail+1
          if(tail>nx*ny) call die('model-style river traversal exceeded the map cell count')
          queue_x(tail)=jx; queue_y(tail)=jy; vector(jx,jy)=.true.
        end if
      end if
    end do
    ! The model registers local outlet cells separately.
    do iy=1,ny
      do ix=1,nx
        if(regions(ix,iy)==region .and. nextx(ix,iy)<0 .and. nextx(ix,iy)/=-9999) vector(ix,iy)=.true.
      end do
    end do
    do i=1,npth
      if(.not.valid_coords(i)) cycle
      if(region_up(i)==region) then
        if(vector(path_ix(i),path_iy(i))) up_valid(i)=.true.
      end if
      if(region_down(i)==region) then
        if(vector(path_jx(i),path_jy(i))) down_valid(i)=.true.
      end if
      if(vector(path_ix(i),path_iy(i)).and.vector(path_jx(i),path_jy(i))) retained_path(i)=.true.
    end do
  end do

  ! ================================================
  ! [4] Classify pathways and report connectivity diagnostics
  ! ================================================
  total=0; channel=0; flood=0; cross_region_retained=0
  if(write_csv) then
    open(newunit=ucsv,file=trim(csv_file),status='replace',action='write',iostat=ios)
    if(ios/=0) call die('cannot create CSV: '//trim(csv_file))
    write(ucsv,'(a)') 'ix,iy,jx,jy,region_up,region_down,elevation,latitude,longitude,type,width1'
  end if
  do i=1,npth
    cat=3
    if(valid_coords(i).and.up_valid(i).and.down_valid(i)) then
      if(retained_path(i)) then; cat=1; else; cat=2; end if
    end if
    total(cat)=total(cat)+1
    if(path_w1(i)>0) then
      channel(cat)=channel(cat)+1; kind_label='channel'
    else
      flood(cat)=flood(cat)+1; kind_label='floodplain_only'
    end if
    if(cat==1.and.region_up(i)/=region_down(i)) cross_region_retained=cross_region_retained+1
    if(write_csv.and.cat==2) write(ucsv,'(4(i0,","),2(i0,","),f0.3,",",f0.6,",",f0.6,",",a,",",f0.3)') &
      path_ix(i),path_iy(i),path_jx(i),path_jy(i),region_up(i),region_down(i),path_elv(i), &
      path_lat(i),path_lon(i),trim(kind_label),path_w1(i)
  end do
  if(write_csv) close(ucsv)

  write(*,'(a,i0)') 'MPI regions: ',regcount
  write(*,'(a,i0)') 'Bifurcation pathways: ',sum(total)
  write(*,'(a,i0)') 'Retained by a model-local vector: ',total(1)
  write(*,'(a,i0)') '  including endpoints in different assigned regions via downstream vector extension: ',cross_region_retained
  write(*,'(a,i0)') 'Dropped (both endpoints have a vector, but never on the same rank): ',total(2)
  write(*,'(a,i0)') 'Invalid/unassigned (an endpoint has no model vector): ',total(3)
  if(sum(total)>0) write(*,'(a,f8.4,a)') 'Dropped fraction of all pathways: ',real(total(2))/real(sum(total))*100.0,' %'
  write(*,'(a,3(i0,1x))') 'Channel retained/dropped/invalid: ',channel
  write(*,'(a,3(i0,1x))') 'Floodplain-only retained/dropped/invalid: ',flood
  write(*,'(a)') 'The dropped fraction measures connectivity exclusion, not simulation error.'
  if(write_csv) write(*,'(a)') 'Dropped-path CSV: '//trim(csv_file)
contains
  subroutine die(message)
    character(len=*),intent(in) :: message
    write(*,'(a)') 'analysis_check_mpi_bifurcation: ERROR: '//trim(message)
    stop 1
  end subroutine die
end program analysis_check_mpi_bifurcation
