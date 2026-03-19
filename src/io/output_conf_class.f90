module output_conf_class
    use PARKIND1, only: &
    &   JPIM
    use YOS_CMF_INPUT, only: &
    &   TMPNAM, LOGNAM

    use const_mod, only: &
    &   CLEN_ITEM, CLEN_PATH, CLEN_SHORT
    use glob_mod, only: &
    &   NML_PATH, BINMAP_SUF, BINVEC_SUF
    use io_namelist_mod, only: &
    &   read_nml_output_default, read_nml_output
    implicit none
    private

    public :: OutputConf
    public :: init_OutputConf
    public :: append_OutputConf

    type :: OutputConf
        private
        character(len=CLEN_ITEM) :: item = ''          ! variable identifier in model (e.g., 'Tair', 'Roff')
        character(len=CLEN_PATH) :: path = ''          ! resolved output path with suffix
        logical(kind=4)          :: is_mapfmt   = .TRUE. ! .TRUE.: 2D map output, .FALSE.: 1D vector output
        logical                  :: is_mean  = .TRUE.   ! .TRUE.: time averaged, .FALSE.: instantaneous
        integer(kind=JPIM) :: dt      ! output interval [sec]
        integer(kind=JPIM) :: now_t   ! current output time anchor [sec]
        integer(kind=JPIM) :: nxt_t   ! next output time [sec]

    contains

        procedure :: get_item
        procedure :: get_path
        procedure :: get_is_mapfmt
        procedure :: get_is_mean
        procedure :: get_dt
        procedure :: get_next_t
        procedure :: write_needed
        procedure :: set_next
    end type OutputConf

contains


! =============================================================================================
! Constructor
! =============================================================================================
subroutine init_OutputConf(out_item, is_found, obj, t)
    character(len=*), intent(in)  :: out_item
    logical, intent(out)          :: is_found
    type(OutputConf), intent(out) :: obj
    integer(kind=JPIM), intent(in) :: t ! [sec] current time

    logical :: &
    &   is_mapfmt, &
    &   is_mean
    character(len=CLEN_PATH) :: &
    &   path
    integer(kind=JPIM) :: &
    &   dt_out

    call read_nml_output(out_item, is_found, path, is_mapfmt, is_mean)
    if (.not. is_found) return

    call read_nml_output_default(dt_out)

    obj%item = out_item
    obj%is_mapfmt = is_mapfmt
    obj%is_mean = is_mean
    obj%dt = dt_out
    obj%now_t = t
    obj%nxt_t = t + dt_out

    if (obj%is_mapfmt) then
        obj%path = trim(path)//trim(BINMAP_SUF)
    else
        obj%path = trim(path)//trim(BINVEC_SUF)
    endif

    write(LOGNAM, '(a,1x,a,1x,a,1x,L1,1x,L1,1x,i0)') &
    &   '  [OutputConf]', trim(obj%item), trim(obj%path), &
    &   obj%is_mapfmt, obj%is_mean, obj%dt
end subroutine init_OutputConf

! =============================================================================================
! Getters
! =============================================================================================
function get_item(self) result(item)
    class(OutputConf), intent(in) :: self
    character(len=CLEN_ITEM) :: item
    item = self%item
end function get_item

function get_path(self) result(path)
    class(OutputConf), intent(in) :: self
    character(len=CLEN_PATH) :: path
    path = self%path
end function get_path

function get_is_mapfmt(self) result(res)
    class(OutputConf), intent(in) :: self
    logical :: res
    res = self%is_mapfmt
end function get_is_mapfmt

function get_is_mean(self) result(res)
    class(OutputConf), intent(in) :: self
    logical :: res
    res = self%is_mean
end function get_is_mean

integer(kind=JPIM) function get_dt(self) result(dt)
    class(OutputConf), intent(in) :: self
    dt = self%dt
end function get_dt

integer(kind=JPIM) function get_next_t(self) result(nxt_t)
    class(OutputConf), intent(in) :: self
    nxt_t = self%nxt_t
end function get_next_t

logical function write_needed(self, now_t) result(is_needed)
    class(OutputConf),  intent(in) :: self
    integer(kind=JPIM), intent(in) :: now_t
    is_needed = .FALSE.
    if (self%get_next_t() <= now_t) is_needed = .TRUE.
end function write_needed

subroutine set_next(self)
    class(OutputConf), intent(inout) :: self
    self%now_t = self%nxt_t
    self%nxt_t = self%nxt_t + self%dt
end subroutine set_next

! =============================================================================================
! Array helper
!   - simple append that grows by 1 each time (consistent with your recent policy)
! =============================================================================================
subroutine append_OutputConf(array, obj)
    type(OutputConf), allocatable, intent(inout) :: array(:)
    type(OutputConf),              intent(in)    :: obj

    type(OutputConf), allocatable :: tmp(:)
    integer :: n

    if (.not. allocated(array)) then
        allocate(array(1))
        array(1) = obj
        return
    endif

    n = size(array)
    allocate(tmp(n + 1))
    tmp(1:n)   = array(1:n)
    tmp(n + 1) = obj
    call move_alloc(tmp, array)
end subroutine append_OutputConf

end module output_conf_class