! Color an existing MPI region raster for map visualization.
! Uses the shared legacy set_color algorithm from map/src/src_param/set_bif_basin_mpi.F90.
! Colors identify display classes only; the region IDs remain in mpireg-N.bin.
program analysis_mpi_region_color
  use map_color_mod, only: color_map
  implicit none

  ! Map dimensions and requested partition count.
  integer                  :: nx=0, ny=0  !! map grid dimensions
  integer                  :: mpi_count=0  !! process count encoded in input filename
  integer                  :: nregion  !! highest positive MPI region ID
  integer                  :: u  !! active input-file unit
  integer                  :: ios  !! I/O status code

  ! Input region map and generated visualization colors.
  integer, allocatable      :: regions(:,:)  !! MPI region IDs
  integer, allocatable      :: color(:,:)  !! display color per MPI region

  ! Map geometry and filenames.
  real                     :: west, east  !! map longitude bounds [deg]
  character(len=512)       :: region_file  !! input mpireg-N.bin path
  character(len=512)       :: output_file  !! output bsncol_mpi-N.bin path
  character(len=512)       :: base_name  !! input filename without its directory
  character(len=64)        :: count_text  !! formatted MPI process count
  integer                  :: slash  !! position of the final directory separator
  integer                  :: file_length  !! input filename length
  integer                  :: record_unit  !! output binary-file unit

  ! Read and validate the process-count-specific input filename.
  if(command_argument_count()/=1) call die('usage: analysis_mpi_region_color mpireg-N.bin')
  call get_command_argument(1,region_file)
  file_length=len_trim(region_file)
  if(file_length==0) call die('input filename must be mpireg-N.bin')
  slash=index(trim(region_file),'/',back=.true.)
  base_name=region_file(slash+1:file_length)
  if(len_trim(base_name)<12) call die('input filename must be mpireg-N.bin')
  if(base_name(1:7)/='mpireg-' .or. base_name(len_trim(base_name)-3:len_trim(base_name))/='.bin') &
    call die('input filename must be mpireg-N.bin')
  read(base_name(8:len_trim(base_name)-4),*,iostat=ios) mpi_count
  if(ios/=0 .or. mpi_count<1) call die('MPI count in input filename must be a positive integer')
  write(count_text,'(i0)') mpi_count
  if(slash>0) then
    output_file=region_file(1:slash)//'bsncol_mpi-'//trim(count_text)//'.bin'
  else
    output_file='bsncol_mpi-'//trim(count_text)//'.bin'
  end if

  ! Read dimensions and wraparound geometry from the map-root params.txt.
  open(newunit=u,file='params.txt',status='old',action='read',iostat=ios)
  if(ios/=0) call die('cannot open params.txt; run from the map root')
  read(u,*,iostat=ios) nx
  if(ios==0) read(u,*,iostat=ios) ny
  if(ios==0) read(u,*,iostat=ios)
  if(ios==0) read(u,*,iostat=ios)
  if(ios==0) read(u,*,iostat=ios) west
  if(ios==0) read(u,*,iostat=ios) east
  close(u)
  if(ios/=0 .or. nx<1 .or. ny<1) call die('invalid map geometry in params.txt')

  allocate(regions(nx,ny),color(nx,ny))
  open(newunit=u,file=trim(region_file),form='unformatted',access='direct', &
       recl=4*nx*ny,status='old',iostat=ios)
  if(ios/=0) call die('cannot open '//trim(region_file))
  read(u,rec=1,iostat=ios) regions
  close(u)
  if(ios/=0) call die('cannot read region-map record 1')
  nregion=maxval(regions)
  if(nregion<1 .or. nregion>mpi_count) call die('region IDs do not match MPI count in input filename')

  ! Apply the shared legacy size-prioritized, adjacency-aware color assignment.
  call color_map(regions,nx,ny,west,east,color)
  open(newunit=record_unit,file=trim(output_file),form='unformatted',access='direct', &
       recl=4*nx*ny,status='replace',iostat=ios)
  if(ios/=0) call die('cannot create '//trim(output_file))
  write(record_unit,rec=1,iostat=ios) color
  close(record_unit)
  if(ios/=0) call die('cannot write '//trim(output_file))
  write(*,'(a)') 'Created '//trim(output_file)//' (visualization only)'

contains
  subroutine die(message)
    character(len=*),intent(in) :: message
    write(*,'(a)') 'analysis_mpi_region_color: ERROR: '//trim(message)
    stop 1
  end subroutine die


end program analysis_mpi_region_color
