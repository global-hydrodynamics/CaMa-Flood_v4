module allocation_map_io
  use iso_fortran_env, only: int16
  implicit none
  private
  public :: read_allocation_base, read_allocation_hires

contains

  subroutine read_allocation_base(map_dir, nx, ny, gsize, west, east, south, north, &
                                  uparea, nextx, nexty, ctmare, elevtn)
    character(len=*), intent(in) :: map_dir
    integer, intent(out) :: nx, ny
    real, intent(out) :: gsize, west, east, south, north
    real, allocatable, intent(out) :: uparea(:,:)
    real, allocatable, intent(out), optional :: ctmare(:,:), elevtn(:,:)
    integer, allocatable, intent(out) :: nextx(:,:), nexty(:,:)
    integer :: unit, ios
    character(len=256) :: line

    open(newunit=unit, file=join_path(map_dir, 'params.txt'), status='old', &
         action='read', iostat=ios)
    call require_io(ios, 'open params.txt')
    read(unit, *, iostat=ios) nx
    call require_io(ios, 'read NX from params.txt')
    read(unit, *, iostat=ios) ny
    call require_io(ios, 'read NY from params.txt')
    read(unit, '(A)', iostat=ios) line
    call require_io(ios, 'read params.txt header')
    read(unit, *, iostat=ios) gsize
    call require_io(ios, 'read grid size from params.txt')
    read(unit, *, iostat=ios) west
    call require_io(ios, 'read west bound from params.txt')
    read(unit, *, iostat=ios) east
    call require_io(ios, 'read east bound from params.txt')
    read(unit, *, iostat=ios) south
    call require_io(ios, 'read south bound from params.txt')
    read(unit, *, iostat=ios) north
    call require_io(ios, 'read north bound from params.txt')
    close(unit)
    if (nx <= 0 .or. ny <= 0) call fail('params.txt dimensions must be positive')

    allocate(uparea(nx,ny), nextx(nx,ny), nexty(nx,ny))
    call read_real_grid(join_path(map_dir,'uparea.bin'), nx, ny, uparea, 'uparea.bin')
    call read_int32_pair(join_path(map_dir,'nextxy.bin'), nx, ny, nextx, nexty, 'nextxy.bin')

    if (present(ctmare)) then
      allocate(ctmare(nx,ny))
      call read_real_grid(join_path(map_dir,'ctmare.bin'), nx, ny, ctmare, 'ctmare.bin')
    end if
    if (present(elevtn)) then
      allocate(elevtn(nx,ny))
      call read_real_grid(join_path(map_dir,'elevtn.bin'), nx, ny, elevtn, 'elevtn.bin')
    end if
  end subroutine read_allocation_base

  ! Allocation traces one selected river network; this reader intentionally rejects multiple areas.
  ! High-resolution terrain resolution is a separate concern from network correspondence.
  subroutine read_allocation_hires(map_dir, tag, nx, ny, west, east, south, north, &
                                   uparea, catmx, catmy, downx, downy, elevtn)
    character(len=*), intent(in) :: map_dir, tag
    integer, intent(out) :: nx, ny
    real, intent(out) :: west, east, south, north
    real, allocatable, intent(out) :: uparea(:,:)
    real, allocatable, intent(out), optional :: elevtn(:,:)
    integer(int16), allocatable, intent(out) :: catmx(:,:), catmy(:,:), downx(:,:), downy(:,:)
    integer :: unit, ios, narea
    character(len=32) :: label1, label2
    character(len=512) :: locfile, hires_dir

    hires_dir = join_path(map_dir, tag)
    locfile = join_path(hires_dir, 'location.txt')
    open(newunit=unit, file=trim(locfile), status='old', action='read', iostat=ios)
    call require_io(ios, 'open '//trim(locfile))
    read(unit, *, iostat=ios) narea
    call require_io(ios, 'read high-resolution area count from '//trim(locfile))
    if (narea /= 1) then
      close(unit)
      call fail('only one high-resolution area is supported: '//trim(locfile))
    end if
    read(unit, *, iostat=ios)
    call require_io(ios, 'read high-resolution header from '//trim(locfile))
    read(unit, *, iostat=ios) label1, label2, west, east, south, north, nx, ny
    call require_io(ios, 'read high-resolution dimensions from '//trim(locfile))
    close(unit)
    if (nx <= 0 .or. ny <= 0) call fail('high-resolution dimensions must be positive')

    allocate(uparea(nx,ny), catmx(nx,ny), catmy(nx,ny), downx(nx,ny), downy(nx,ny))
    call read_real_grid(join_path(hires_dir,trim(tag)//'.uparea.bin'), nx, ny, uparea, 'high-resolution uparea')
    call read_int16_pair(join_path(hires_dir,trim(tag)//'.catmxy.bin'), nx, ny, catmx, catmy, 'high-resolution catmxy')
    call read_int16_pair(join_path(hires_dir,trim(tag)//'.downxy.bin'), nx, ny, downx, downy, 'high-resolution downxy')
    if (present(elevtn)) then
      allocate(elevtn(nx,ny))
      call read_real_grid(join_path(hires_dir,trim(tag)//'.elevtn.bin'), nx, ny, elevtn, 'high-resolution elevtn')
    end if
  end subroutine read_allocation_hires

  subroutine read_real_grid(path, nx, ny, grid, label)
    character(len=*), intent(in) :: path, label
    integer, intent(in) :: nx, ny
    real, intent(out) :: grid(nx,ny)
    integer :: unit, ios
    open(newunit=unit, file=path, form='unformatted', access='direct', recl=4*nx*ny, &
         status='old', action='read', iostat=ios)
    call require_io(ios, 'open '//trim(label)//' ('//trim(path)//')')
    read(unit, rec=1, iostat=ios) grid
    call require_io(ios, 'read '//trim(label)//' ('//trim(path)//')')
    close(unit)
  end subroutine read_real_grid

  subroutine read_int32_pair(path, nx, ny, grid1, grid2, label)
    character(len=*), intent(in) :: path, label
    integer, intent(in) :: nx, ny
    integer, intent(out) :: grid1(nx,ny), grid2(nx,ny)
    integer :: unit, ios
    open(newunit=unit, file=path, form='unformatted', access='direct', recl=4*nx*ny, &
         status='old', action='read', iostat=ios)
    call require_io(ios, 'open '//trim(label)//' ('//trim(path)//')')
    read(unit, rec=1, iostat=ios) grid1
    call require_io(ios, 'read '//trim(label)//' record 1 ('//trim(path)//')')
    read(unit, rec=2, iostat=ios) grid2
    call require_io(ios, 'read '//trim(label)//' record 2 ('//trim(path)//')')
    close(unit)
  end subroutine read_int32_pair

  subroutine read_int16_pair(path, nx, ny, grid1, grid2, label)
    character(len=*), intent(in) :: path, label
    integer, intent(in) :: nx, ny
    integer(int16), intent(out) :: grid1(nx,ny), grid2(nx,ny)
    integer :: unit, ios
    open(newunit=unit, file=path, form='unformatted', access='direct', recl=2*nx*ny, &
         status='old', action='read', iostat=ios)
    call require_io(ios, 'open '//trim(label)//' ('//trim(path)//')')
    read(unit, rec=1, iostat=ios) grid1
    call require_io(ios, 'read '//trim(label)//' record 1 ('//trim(path)//')')
    read(unit, rec=2, iostat=ios) grid2
    call require_io(ios, 'read '//trim(label)//' record 2 ('//trim(path)//')')
    close(unit)
  end subroutine read_int16_pair

  function join_path(parent, child) result(path)
    character(len=*), intent(in) :: parent, child
    character(len=512) :: path
    if (len_trim(parent) == 0 .or. trim(parent) == '.') then
      path = trim(child)
    else if (parent(len_trim(parent):len_trim(parent)) == '/') then
      path = trim(parent)//trim(child)
    else
      path = trim(parent)//'/'//trim(child)
    end if
  end function join_path

  subroutine require_io(ios, action)
    integer, intent(in) :: ios
    character(len=*), intent(in) :: action
    if (ios /= 0) call fail(trim(action)//' failed (I/O status '//integer_text(ios)//')')
  end subroutine require_io

  subroutine fail(message)
    character(len=*), intent(in) :: message
    write(*,'(a)') 'ERROR: allocation map input: '//trim(message)
    error stop 1
  end subroutine fail

  function integer_text(value) result(text)
    integer, intent(in) :: value
    character(len=32) :: text
    write(text,'(i0)') value
  end function integer_text

end module allocation_map_io
