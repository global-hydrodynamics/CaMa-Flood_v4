! Greedy basin-to-region assignment used by the legacy MPI map workflow.
! Keep ascending basin traversal and first-region tie resolution (<, not <=).
program param_set_mpi_region
  implicit none
  integer :: nx,ny,nmpi,nbsn,ix,iy,ibsn,i,imin,jmpi,u,ios
  integer,allocatable :: basin(:,:),mpireg(:,:),bnum(:),bmpi(:),mpgrid(:)
  character(len=64) :: arg,count_text
  call get_command_argument(1,arg)
  if(len_trim(arg)==0) call die('usage: param_set_mpi_region <number-of-regions>')
  read(arg,*,iostat=ios) nmpi
  if(ios/=0 .or. nmpi<1) call die('region count must be a positive integer')
  open(newunit=u,file='../params.txt',status='old',action='read',iostat=ios)
  if(ios/=0) call die('cannot open ../params.txt')
  read(u,*,iostat=ios) nx
  if(ios==0) read(u,*,iostat=ios) ny
  close(u)
  if(ios/=0 .or. nx<1 .or. ny<1) call die('invalid map dimensions in ../params.txt')
  allocate(basin(nx,ny),mpireg(nx,ny))
  open(newunit=u,file='../.bifbsn_mpi.tmp',form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
  if(ios/=0) call die('temporary basin groups missing; run param_set_bif_basin_mpi first')
  read(u,rec=1,iostat=ios) basin; close(u)
  if(ios/=0) call die('cannot read temporary basin groups')
  nbsn=maxval(basin); if(nbsn<1) call die('temporary basin groups contain no river basin')
  allocate(bnum(nbsn),bmpi(nbsn),mpgrid(nmpi))
  bnum=0; bmpi=0; mpgrid=0
  do iy=1,ny
    do ix=1,nx
      if(basin(ix,iy)>0) bnum(basin(ix,iy))=bnum(basin(ix,iy))+1
    end do
  end do
  ! Preserve legacy greedy assignment: next basin goes to the least-loaded
  ! region; equal loads select the lowest region number.
  do ibsn=1,nbsn
    if(bnum(ibsn)<=0) cycle
    imin=huge(imin); jmpi=1
    do i=1,nmpi
      if(mpgrid(i)<imin) then; jmpi=i; imin=mpgrid(i); end if
    end do
    bmpi(ibsn)=jmpi; mpgrid(jmpi)=mpgrid(jmpi)+bnum(ibsn)
  end do
  mpireg=-9999
  do iy=1,ny
    do ix=1,nx
      if(basin(ix,iy)>0) then
        if(bmpi(basin(ix,iy))>0) mpireg(ix,iy)=bmpi(basin(ix,iy))
      end if
    end do
  end do
  write(count_text,'(i0)') nmpi
  open(newunit=u,file='../mpireg-'//trim(count_text)//'.bin',form='unformatted',access='direct', &
       recl=4*nx*ny,status='replace',iostat=ios)
  if(ios/=0) call die('cannot create MPI region map')
  write(u,rec=1,iostat=ios) mpireg; close(u)
  if(ios/=0) call die('cannot write MPI region map')
  write(*,'(a,i0,a)') 'Created ../mpireg-',nmpi,'.bin'
  do i=1,nmpi
    write(*,'(a,i0,a,i0,a)') '  region ',i,': ',mpgrid(i),' river cells'
  end do
contains
  subroutine die(message)
    character(len=*),intent(in)::message
    write(*,'(a)') 'param_set_mpi_region: ERROR: '//trim(message)
    stop 1
  end subroutine die
end program param_set_mpi_region
