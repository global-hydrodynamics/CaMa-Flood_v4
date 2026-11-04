module accumulated_array_class
    use PARKIND1, only: JPRB
    implicit none
    private

    integer, parameter, public :: RK_UNDEF = 0
    integer, parameter, public :: RK_1D    = 1
    integer, parameter, public :: RK_2D    = 2

    public :: AccumulatedArray, append_AccumulatedArray

    type :: AccumulatedArray
        private
        integer :: rank = RK_UNDEF
        logical :: is_mean = .true.
        integer :: n_added = 0
        real(kind=JPRB), allocatable :: buf1(:)
        real(kind=JPRB), allocatable :: buf2(:,:)
    contains
        procedure :: clear
        procedure :: reset

        procedure :: get_rank
        procedure :: get_added_count
        procedure :: get_size
        procedure :: get_shape
        procedure :: get_is_mean

        procedure :: configure_1d
        procedure :: configure_2d
        generic   :: configure => configure_1d, configure_2d

        procedure :: add_1d
        procedure :: add_2d
        generic   :: add => add_1d, add_2d

        procedure :: flush_1d
        procedure :: flush_2d
        generic   :: flush => flush_1d, flush_2d
    end type AccumulatedArray

contains

    ! =============================================================================================
    ! Errors
    ! =============================================================================================
    subroutine raise_rank_mismatch_error(expected, actual)
        integer, intent(in) :: expected, actual
        write(*, *) '[AccumulatedArray ERROR] rank mismatch. expected=', expected, ' actual=', actual
        stop
    end subroutine raise_rank_mismatch_error

    subroutine raise_undefined_error()
        write(*, *) '[AccumulatedArray ERROR] array is undefined (rank=RK_UNDEF).'
        stop
    end subroutine raise_undefined_error

    subroutine raise_not_configured_error()
        write(*, *) '[AccumulatedArray ERROR] not configured yet. Call configure() first.'
        stop
    end subroutine raise_not_configured_error

    subroutine raise_shape_mismatch_error_1d(expected, actual)
        real(kind=JPRB), intent(in) :: expected(:)
        real(kind=JPRB), intent(in) :: actual(:)
        write(*, *) '[AccumulatedArray ERROR] shape mismatch (1D).'
        write(*, *) '  expected size =', size(expected)
        write(*, *) '  actual   size =', size(actual)
        stop
    end subroutine raise_shape_mismatch_error_1d

    subroutine raise_shape_mismatch_error_2d(expected, actual)
        real(kind=JPRB), intent(in) :: expected(:,:)
        real(kind=JPRB), intent(in) :: actual(:,:)
        write(*, *) '[AccumulatedArray ERROR] shape mismatch (2D).'
        write(*, *) '  expected shape =', size(expected, 1), size(expected, 2)
        write(*, *) '  actual   shape =', size(actual,   1), size(actual,   2)
        stop
    end subroutine raise_shape_mismatch_error_2d

    ! =============================================================================================
    ! Basic operations
    ! =============================================================================================
    subroutine clear(self)
        class(AccumulatedArray), intent(inout) :: self

        if (allocated(self%buf1)) deallocate(self%buf1)
        if (allocated(self%buf2)) deallocate(self%buf2)

        self%rank    = RK_UNDEF
        self%is_mean = .true.
        self%n_added = 0
    end subroutine clear

    subroutine reset(self)
        class(AccumulatedArray), intent(inout) :: self

        select case (self%rank)
        case (RK_1D)
            if (.not. allocated(self%buf1)) call raise_undefined_error()
            self%buf1(:) = 0.0_JPRB
        case (RK_2D)
            if (.not. allocated(self%buf2)) call raise_undefined_error()
            self%buf2(:,:) = 0.0_JPRB
        case default
            call raise_undefined_error()
        end select

        self%n_added = 0
    end subroutine reset

    integer function get_rank(self) result(rk)
        class(AccumulatedArray), intent(in) :: self
        rk = self%rank
    end function get_rank

    logical function get_is_mean(self) result(v)
        class(AccumulatedArray), intent(in) :: self
        v = self%is_mean
    end function get_is_mean

    integer function get_added_count(self) result(n)
        class(AccumulatedArray), intent(in) :: self
        n = self%n_added
    end function get_added_count

    integer function get_size(self) result(n)
        class(AccumulatedArray), intent(in) :: self

        select case (self%rank)
        case (RK_1D)
            if (.not. allocated(self%buf1)) call raise_undefined_error()
            n = size(self%buf1)
        case (RK_2D)
            if (.not. allocated(self%buf2)) call raise_undefined_error()
            n = size(self%buf2)
        case default
            call raise_undefined_error()
        end select
    end function get_size

    integer function get_shape(self, dim) result(n)
        class(AccumulatedArray), intent(in) :: self
        integer,                 intent(in) :: dim

        if (self%rank < dim) call raise_rank_mismatch_error(dim, self%rank)

        select case (self%rank)
        case (RK_1D)
            if (.not. allocated(self%buf1)) call raise_undefined_error()
            n = size(self%buf1, dim)

        case (RK_2D)
            if (.not. allocated(self%buf2)) call raise_undefined_error()
            n = size(self%buf2, dim)

        case default
            call raise_undefined_error()
        end select
    end function get_shape

    ! =============================================================================================
    ! Configure (fix shape + is_mean forever)
    ! =============================================================================================
    subroutine configure_1d(self, n, is_mean)
        class(AccumulatedArray), intent(inout) :: self
        integer,                 intent(in)    :: n
        logical,                 intent(in)    :: is_mean

        if (self%rank /= RK_UNDEF) then
            write(*, *) '[AccumulatedArray ERROR] configure_1d called more than once for the same object.'
            stop
        end if

        if (n <= 0) then
            write(*, *) '[AccumulatedArray ERROR] invalid size for configure_1d: n=', n
            stop
        end if

        self%rank    = RK_1D
        self%is_mean = is_mean
        allocate(self%buf1(n))
        self%buf1(:) = 0.0_JPRB
        self%n_added = 0
    end subroutine configure_1d


    subroutine configure_2d(self, n1, n2, is_mean)
        class(AccumulatedArray), intent(inout) :: self
        integer,                 intent(in)    :: n1, n2
        logical,                 intent(in)    :: is_mean

        if (self%rank /= RK_UNDEF) then
            write(*, *) '[AccumulatedArray ERROR] configure_2d called more than once for the same object.'
            stop
        end if

        if (n1 <= 0 .or. n2 <= 0) then
            write(*, *) '[AccumulatedArray ERROR] invalid shape for configure_2d: n1,n2=', n1, n2
            stop
        end if

        self%rank    = RK_2D
        self%is_mean = is_mean
        allocate(self%buf2(n1, n2))
        self%buf2(:,:) = 0.0_JPRB
        self%n_added = 0
    end subroutine configure_2d

    ! =============================================================================================
    ! Accumulate (shape + is_mean fixed; add never reconfigures)
    ! =============================================================================================
    subroutine add_1d(self, arr)
        class(AccumulatedArray), intent(inout) :: self
        real(kind=JPRB),         intent(in)    :: arr(:)

        if (self%rank == RK_UNDEF) call raise_not_configured_error()
        if (self%rank /= RK_1D) call raise_rank_mismatch_error(RK_1D, self%rank)
        if (.not. allocated(self%buf1)) call raise_undefined_error()
        if (size(self%buf1) /= size(arr)) call raise_shape_mismatch_error_1d(self%buf1, arr)

        if (self%is_mean) then
            self%buf1(:) = self%buf1(:) + arr(:)
            self%n_added = self%n_added + 1
        else
            self%buf1(:) = arr(:)
            self%n_added = 1
        end if
    end subroutine add_1d

    subroutine add_2d(self, arr)
        class(AccumulatedArray), intent(inout) :: self
        real(kind=JPRB),         intent(in)    :: arr(:,:)

        if (self%rank == RK_UNDEF) call raise_not_configured_error()
        if (self%rank /= RK_2D) call raise_rank_mismatch_error(RK_2D, self%rank)
        if (.not. allocated(self%buf2)) call raise_undefined_error()
        if (size(self%buf2, 1) /= size(arr, 1) .or. size(self%buf2, 2) /= size(arr, 2)) then
            call raise_shape_mismatch_error_2d(self%buf2, arr)
        end if

        if (self%is_mean) then
            self%buf2(:,:) = self%buf2(:,:) + arr(:,:)
            self%n_added = self%n_added + 1
        else
            self%buf2(:,:) = arr(:,:)
            self%n_added = 1
        end if
    end subroutine add_2d

    ! =============================================================================================
    ! Flush (copy/average into out) and then reset() unconditionally
    ! =============================================================================================
    subroutine flush_1d(self, out)
        class(AccumulatedArray), intent(inout) :: self
        real(kind=JPRB),         intent(inout) :: out(:)
        real(kind=JPRB) :: inv

        if (self%rank == RK_UNDEF) call raise_not_configured_error()
        if (self%rank /= RK_1D) call raise_rank_mismatch_error(RK_1D, self%rank)
        if (.not. allocated(self%buf1)) call raise_undefined_error()
        if (size(out) /= size(self%buf1)) call raise_shape_mismatch_error_1d(self%buf1, out)

        if (.not. self%is_mean) then
            out(:) = self%buf1(:)
            call self%reset()
            return
        end if

        if (self%n_added > 0) then
            inv = 1.0_JPRB / real(self%n_added, kind=JPRB)
            out(:) = self%buf1(:) * inv
        else
            out(:) = 0.0_JPRB
        end if

        call self%reset()
    end subroutine flush_1d

    subroutine flush_2d(self, out)
        class(AccumulatedArray), intent(inout) :: self
        real(kind=JPRB),         intent(inout) :: out(:,:)
        real(kind=JPRB) :: inv

        if (self%rank == RK_UNDEF) call raise_not_configured_error()
        if (self%rank /= RK_2D) call raise_rank_mismatch_error(RK_2D, self%rank)
        if (.not. allocated(self%buf2)) call raise_undefined_error()
        if (size(out, 1) /= size(self%buf2, 1) .or. size(out, 2) /= size(self%buf2, 2)) then
            call raise_shape_mismatch_error_2d(self%buf2, out)
        end if

        if (.not. self%is_mean) then
            out(:,:) = self%buf2(:,:)
            call self%reset()
            return
        end if

        if (self%n_added > 0) then
            inv = 1.0_JPRB / real(self%n_added, kind=JPRB)
            out(:,:) = self%buf2(:,:) * inv
        else
            out(:,:) = 0.0_JPRB
        end if

        call self%reset()
    end subroutine flush_2d

    ! =============================================================================================
    subroutine append_AccumulatedArray(array)
        type(AccumulatedArray), allocatable, intent(inout) :: array(:)
        type(AccumulatedArray), allocatable :: tmp(:)
        integer :: n

        if (.not. allocated(array)) then
            allocate(array(1))
            call array(1)%clear()
            return
        endif

        n = size(array)
        allocate(tmp(n + 1))
        tmp(1:n) = array(1:n)
        call tmp(n + 1)%clear()
        call move_alloc(tmp, array)
    end subroutine append_AccumulatedArray

end module accumulated_array_class