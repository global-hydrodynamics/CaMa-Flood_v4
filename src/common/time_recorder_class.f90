module time_recorder_class
    use array_mod, only : append, find_index
    implicit none
    private
    public :: TimeRecorder

    type :: TimeRecorder
        private
        real(8),           allocatable :: total_times(:) ! [s]
        integer,           allocatable :: start_times(:)
        character(len=64), allocatable :: items(:)

        contains

        procedure :: start => start
        procedure :: stop  => stop
        procedure :: get   => get
        procedure :: write => write
    end type TimeRecorder


contains

subroutine start(self, item)
    class(TimeRecorder), intent(inout) :: self
    character(len=*),    intent(in)    :: item
    integer :: t1, idx
    call system_clock(t1)
    idx = find_index(self%items, item)
    if (idx >= 1) then
        self%start_times(idx) = t1
    else
        call append(self%items      , item)
        call append(self%start_times, t1  )
        call append(self%total_times, 0.d0)
    endif
end subroutine start


subroutine stop(self, item)
    class(TimeRecorder), intent(inout) :: self
    character(len=*),    intent(in)    :: item
    integer :: t1, t2, t_rate, t_max, idx
    real(8) :: elps_time
    call system_clock(t2, t_rate, t_max)
    idx = find_index(self%items, item)
    if (idx < 1) then
        write(*, '(2a)') '[TimeRecorder%stop ERROR] item not found: ', trim(item)
        stop
    endif
    t1        = self%start_times(idx)
    elps_time = calc_elapsed_time(t1, t2, t_rate, t_max)
    self%total_times(idx) = self%total_times(idx) + elps_time
    self%start_times(idx) = 0

    contains

    real(8) function calc_elapsed_time(t1, t2, t_rate, t_max)
        integer, intent(in) :: t1, t2, t_rate, t_max
        integer :: diff
        if (t2 < t1) then
            diff = (t_max - t1) + t2 + 1
        else
            diff = t2 - t1
        endif
        calc_elapsed_time = diff / dble(t_rate)
    end function calc_elapsed_time

end subroutine stop


real(8) function get(self, item)
    class(TimeRecorder), intent(in) :: self
    character(len=*),    intent(in) :: item
    integer :: idx
    idx = find_index(self%items, item)
    if (idx < 1) then
        write(*, '(2a)') '[TimeRecorder%get ERROR] item not found: ', trim(item)
        stop
    endif
    get = self%total_times(idx)
end function get


subroutine write(self, unit, items)
    class(TimeRecorder), intent(in) :: &
    &   self
    integer, intent(in), optional :: &
    &   unit ! unit number to write
    character(len=*), intent(in), optional :: &
    &   items(:)
    character(len=32) :: &
    &   c_len_max, format
    integer :: &
    &   idx
    if (present(items)) then
        write(c_len_max, '(i0)') get_max_len(items(:))
        format = '(a4,a'//trim(c_len_max)//',a2,f0.3)'
        do idx = 1, size(items)
            if (present(unit)) then
                write(unit, format) '  - ', items(idx), ': ', self%get(items(idx))
            else
                write(*, format) '  - ', items(idx), ': ', self%get(items(idx))
            endif
        enddo
    else
        write(c_len_max, '(i0)') get_max_len(self%items(:))
        format = '(a4,a'//trim(c_len_max)//',a2,f0.3)'
        do idx = 1, size(self%items)
            if (present(unit)) then
                write(unit, format) '  - ', self%items(idx), ': ', self%total_times(idx)
            else
                write(*, format) '  - ', self%items(idx), ': ', self%total_times(idx)
            endif
        enddo
    endif

contains

    integer function get_max_len(texts)
        character(len=*), intent(in) :: texts(:)
        integer :: idx
        get_max_len = 0
        do idx = 1, size(texts)
            get_max_len = max(get_max_len, len_trim(texts(idx)))
        enddo
    end function get_max_len

end subroutine write

end module time_recorder_class
