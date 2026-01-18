module accumulated_array_class
    use PARKIND1, only: JPRB
    implicit none
    private

    integer, parameter, public :: RK_UNDEF = 0
    integer, parameter, public :: RK_1D    = 1
    integer, parameter, public :: RK_2D    = 2

    public :: AccumulatedArray

    type :: AccumulatedArray
        private
        integer :: rank = RK_UNDEF
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

        procedure :: accumulate_1d
        procedure :: accumulate_2d
        generic   :: accumulate => accumulate_1d, accumulate_2d

        procedure :: apply_1d
        procedure :: apply_2d
        generic   :: apply => apply_1d, apply_2d
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

    subroutine raise_shape_rank_error()
        write(*, *) '[AccumulatedArray ERROR] shape array has wrong length for get_shape().'
        stop
    end subroutine raise_shape_rank_error

    ! =============================================================================================
    ! Basic operations
    ! =============================================================================================
    subroutine clear(self)
        class(AccumulatedArray), intent(inout) :: self

        if (allocated(self%buf1)) deallocate(self%buf1)
        if (allocated(self%buf2)) deallocate(self%buf2)

        self%rank    = RK_UNDEF
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

    subroutine get_shape(self, shape)
        class(AccumulatedArray), intent(in)    :: self
        integer,                intent(inout) :: shape(:)

        select case (self%rank)
        case (RK_1D)
            if (.not. allocated(self%buf1)) call raise_undefined_error()
            if (size(shape) /= 1) call raise_shape_rank_error()
            shape(1) = size(self%buf1)
        case (RK_2D)
            if (.not. allocated(self%buf2)) call raise_undefined_error()
            if (size(shape) /= 2) call raise_shape_rank_error()
            shape(1) = size(self%buf2, 1)
            shape(2) = size(self%buf2, 2)
        case default
            call raise_undefined_error()
        end select
    end subroutine get_shape

    ! =============================================================================================
    ! Accumulate
    ! =============================================================================================
    subroutine accumulate_1d(self, arr, is_mean)
        class(AccumulatedArray), intent(inout) :: self
        real(kind=JPRB),         intent(in)    :: arr(:)
        logical,                 intent(in)    :: is_mean
        integer :: n

        n = size(arr)

        if (self%rank == RK_UNDEF) then
            self%rank = RK_1D
            allocate(self%buf1(n))
            self%buf1(:) = 0.0_JPRB
            self%n_added = 0
        end if

        if (self%rank /= RK_1D) call raise_rank_mismatch_error(RK_1D, self%rank)
        if (.not. allocated(self%buf1)) call raise_undefined_error()
        if (size(self%buf1) /= n) call raise_shape_mismatch_error_1d(self%buf1, arr)

        if (is_mean) then
            self%buf1(:) = self%buf1(:) + arr(:)
            self%n_added = self%n_added + 1
        else
            self%buf1(:) = arr(:)
            self%n_added = 1
        end if
    end subroutine accumulate_1d

    subroutine accumulate_2d(self, arr, is_mean)
        class(AccumulatedArray), intent(inout) :: self
        real(kind=JPRB),         intent(in)    :: arr(:,:)
        logical,                 intent(in)    :: is_mean
        integer :: n1, n2

        n1 = size(arr, 1)
        n2 = size(arr, 2)

        if (self%rank == RK_UNDEF) then
            self%rank = RK_2D
            allocate(self%buf2(n1, n2))
            self%buf2(:,:) = 0.0_JPRB
            self%n_added = 0
        end if

        if (self%rank /= RK_2D) call raise_rank_mismatch_error(RK_2D, self%rank)
        if (.not. allocated(self%buf2)) call raise_undefined_error()
        if (size(self%buf2, 1) /= n1 .or. size(self%buf2, 2) /= n2) then
            call raise_shape_mismatch_error_2d(self%buf2, arr)
        end if

        if (is_mean) then
            self%buf2(:,:) = self%buf2(:,:) + arr(:,:)
            self%n_added = self%n_added + 1
        else
            self%buf2(:,:) = arr(:,:)
            self%n_added = 1
        end if
    end subroutine accumulate_2d

    ! =============================================================================================
    ! Prepare-for-write (copy/average into out) and then reset() unconditionally
    ! =============================================================================================
    subroutine apply_1d(self, out, is_mean)
        class(AccumulatedArray), intent(inout) :: self
        real(kind=JPRB),         intent(inout) :: out(:)
        logical,                 intent(in)    :: is_mean
        real(kind=JPRB) :: inv

        if (self%rank /= RK_1D) call raise_rank_mismatch_error(RK_1D, self%rank)
        if (.not. allocated(self%buf1)) call raise_undefined_error()
        if (size(out) /= size(self%buf1)) call raise_shape_mismatch_error_1d(self%buf1, out)

        if (.not. is_mean) then
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
    end subroutine apply_1d

    subroutine apply_2d(self, out, is_mean)
        class(AccumulatedArray), intent(inout) :: self
        real(kind=JPRB),         intent(inout) :: out(:,:)
        logical,                 intent(in)    :: is_mean
        real(kind=JPRB) :: inv

        if (self%rank /= RK_2D) call raise_rank_mismatch_error(RK_2D, self%rank)
        if (.not. allocated(self%buf2)) call raise_undefined_error()
        if (size(out, 1) /= size(self%buf2, 1) .or. size(out, 2) /= size(self%buf2, 2)) then
            call raise_shape_mismatch_error_2d(self%buf2, out)
        end if

        if (.not. is_mean) then
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
    end subroutine apply_2d

end module accumulated_array_class