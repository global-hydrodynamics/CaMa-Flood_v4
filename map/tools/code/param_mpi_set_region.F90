! Assign aggregated basins to MPI regions using the legacy greedy load balance.
! Ascending basin order and first-region tie resolution are intentional.
program param_mpi_set_region
  implicit none

  ! Map dimensions and requested MPI-region count.
  integer                  :: nx, ny  !! map grid dimensions
  integer                  :: nmpi  !! requested MPI-region count
  integer                  :: nbsn  !! highest aggregated basin ID

  ! Grid, basin, and region indices.
  integer                  :: ix, iy  !! x/y grid index
  integer                  :: ibsn  !! basin identifier
  integer                  :: i  !! candidate MPI region index
  integer                  :: imin  !! smallest current region load [river cells]
  integer                  :: jmpi  !! selected least-loaded MPI region
  integer                  :: u  !! active input/output-file unit
  integer                  :: ios  !! I/O status code

  ! Basin raster, output region raster, and balancing arrays.
  integer, allocatable      :: basin(:,:)  !! MPI-aggregated basin IDs
  integer, allocatable      :: mpireg(:,:)  !! MPI computational region IDs
  integer, allocatable      :: bnum(:)  !! grid-cell count per basin
  integer, allocatable      :: bmpi(:)  !! basin-to-region assignment
  integer, allocatable      :: mpgrid(:)  !! river-cell count per MPI region

  character(len=64)         :: arg  !! command-line argument
  character(len=64)         :: count_text  !! process count formatted for the output filename

  ! Read the requested number of MPI regions.
  if(command_argument_count()/=1) call die('usage: param_mpi_set_region <number-of-regions>')
  call get_command_argument(1,arg)
  read(arg,*,iostat=ios) nmpi
  if(ios/=0 .or. nmpi<1) call die('region count must be a positive integer')

  ! Read map dimensions and the MPI-aggregated basin map.
  open(newunit=u,file='../params.txt',status='old',action='read',iostat=ios)
  if(ios/=0) call die('cannot open ../params.txt')
  read(u,*,iostat=ios) nx
  if(ios==0) read(u,*,iostat=ios) ny
  close(u)
  if(ios/=0 .or. nx<1 .or. ny<1) call die('invalid map dimensions in ../params.txt')

  allocate(basin(nx,ny),mpireg(nx,ny))
  open(newunit=u,file='../bifbsn_mpi.tmp',form='unformatted',access='direct', &
       recl=4*nx*ny,status='old',iostat=ios)
  if(ios/=0) call die('temporary basin groups missing; run param_mpi_set_basin first')
  read(u,rec=1,iostat=ios) basin
  close(u)
  if(ios/=0) call die('cannot read temporary basin groups')

  ! Count cells in each aggregated basin for load balancing.
  nbsn=maxval(basin)
  if(nbsn<1) call die('temporary basin groups contain no river basin')
  allocate(bnum(nbsn),bmpi(nbsn),mpgrid(nmpi))
  bnum=0
  bmpi=0
  mpgrid=0
  do iy=1,ny
    do ix=1,nx
      if(basin(ix,iy)>0) bnum(basin(ix,iy))=bnum(basin(ix,iy))+1
    end do
  end do

  ! Preserve the legacy greedy assignment: each basin goes to the currently
  ! least-loaded region; equal loads select the lowest region number.
  do ibsn=1,nbsn
    if(bnum(ibsn)<=0) cycle
    imin=huge(imin)
    jmpi=1
    do i=1,nmpi
      if(mpgrid(i)<imin) then
        jmpi=i
        imin=mpgrid(i)
      end if
    end do
    bmpi(ibsn)=jmpi
    mpgrid(jmpi)=mpgrid(jmpi)+bnum(ibsn)
  end do

  mpireg=-9999
  do iy=1,ny
    do ix=1,nx
      if(basin(ix,iy)>0) then
        if(bmpi(basin(ix,iy))>0) mpireg(ix,iy)=bmpi(basin(ix,iy))
      end if
    end do
  end do

  ! Write only the process-count-specific region map and report partition sizes.
  write(count_text,'(i0)') nmpi
  open(newunit=u,file='../mpireg-'//trim(count_text)//'.bin',form='unformatted',access='direct', &
       recl=4*nx*ny,status='replace',iostat=ios)
  if(ios/=0) call die('cannot create MPI region map')
  write(u,rec=1,iostat=ios) mpireg
  close(u)
  if(ios/=0) call die('cannot write MPI region map')
  write(*,'(a,i0,a)') 'Created ../mpireg-',nmpi,'.bin'
  do i=1,nmpi
    write(*,'(a,i0,a,i0,a)') '  region ',i,': ',mpgrid(i),' river cells'
  end do

contains
  subroutine die(message)
    character(len=*),intent(in) :: message
    write(*,'(a)') 'param_mpi_set_region: ERROR: '//trim(message)
    stop 1
  end subroutine die
end program param_mpi_set_region
