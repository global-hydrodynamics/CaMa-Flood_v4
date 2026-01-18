module ranked_array_class
    use PARKIND1, only: JPRB
    implicit none
    private

    integer, parameter, public :: RK_UNDEF = 0
    integer, parameter, public :: RK_1D    = 1
    integer, parameter, public :: RK_2D    = 2

    public :: RankedArray
    public :: append_ranked_array

    type :: RankedArray
        integer :: rank = RK_UNDEF
        real(kind=JPRB), allocatable :: arr1(:)
        real(kind=JPRB), allocatable :: arr2(:,:)
    contains
        procedure :: clear

        procedure :: set_1d
        procedure :: set_2d
        generic   :: set => set_1d, set_2d

        procedure :: get_1d
        procedure :: get_2d
        generic   :: get => get_1d, get_2d

        procedure :: get_rank
        procedure :: get_size
        procedure :: get_shape
    end type RankedArray

    interface append_ranked_array
        module procedure append_ranked_array_1d
        module procedure append_ranked_array_2d
    end interface

contains

    subroutine raise_rank_mismatch_error(expected, actual)
        integer, intent(in) :: expected, actual
        write(*, *) '[RankedArray ERROR] rank mismatch. expected=', expected, ' actual=', actual
        stop
    end subroutine raise_rank_mismatch_error

    subroutine raise_shape_mismatch_error()
        write(*, *) '[RankedArray ERROR] shape mismatch.'
        stop
    end subroutine raise_shape_mismatch_error

    subroutine raise_undefined_error()
        write(*, *) '[RankedArray ERROR] array is undefined (rank=RK_UNDEF).'
        stop
    end subroutine raise_undefined_error

    subroutine clear(self)
        class(RankedArray), intent(inout) :: self
        if (allocated(self%arr1)) deallocate(self%arr1)
        if (allocated(self%arr2)) deallocate(self%arr2)
        self%rank = RK_UNDEF
    end subroutine clear

    subroutine set_1d(self, arr)
        class(RankedArray), intent(inout) :: self
        real(kind=JPRB), intent(in) :: arr(:)
        integer :: n

        n = size(arr)

        if (self%rank == RK_1D) then
            if (.not. allocated(self%arr1)) then
                allocate(self%arr1(n))
            else
                if (size(self%arr1) /= n) call raise_shape_mismatch_error()
            end if
            self%arr1(:) = arr(:)
            return
        end if

        call self%clear()
        self%rank = RK_1D
        allocate(self%arr1(n))
        self%arr1(:) = arr(:)
    end subroutine set_1d

    subroutine set_2d(self, arr)
        class(RankedArray), intent(inout) :: self
        real(kind=JPRB), intent(in) :: arr(:,:)
        integer :: n1, n2

        n1 = size(arr, 1)
        n2 = size(arr, 2)

        if (self%rank == RK_2D) then
            if (.not. allocated(self%arr2)) then
                allocate(self%arr2(n1, n2))
            else
                if (size(self%arr2, 1) /= n1 .or. size(self%arr2, 2) /= n2) then
                    call raise_shape_mismatch_error()
                end if
            end if
            self%arr2(:,:) = arr(:,:)
            return
        end if

        call self%clear()
        self%rank = RK_2D
        allocate(self%arr2(n1, n2))
        self%arr2(:,:) = arr(:,:)
    end subroutine set_2d

    subroutine get_1d(self, arr)
        class(RankedArray), intent(in)    :: self
        real(kind=JPRB),     intent(inout):: arr(:)

        if (self%rank /= RK_1D) call raise_rank_mismatch_error(RK_1D, self%rank)
        if (.not. allocated(self%arr1)) call raise_undefined_error()
        if (size(arr) /= size(self%arr1)) call raise_shape_mismatch_error()

        arr(:) = self%arr1(:)
    end subroutine get_1d

    subroutine get_2d(self, arr)
        class(RankedArray), intent(in)    :: self
        real(kind=JPRB),     intent(inout):: arr(:,:)

        if (self%rank /= RK_2D) call raise_rank_mismatch_error(RK_2D, self%rank)
        if (.not. allocated(self%arr2)) call raise_undefined_error()
        if (size(arr, 1) /= size(self%arr2, 1) .or. size(arr, 2) /= size(self%arr2, 2)) then
            call raise_shape_mismatch_error()
        end if

        arr(:,:) = self%arr2(:,:)
    end subroutine get_2d

    integer function get_rank(self) result(rk)
        class(RankedArray), intent(in) :: self
        rk = self%rank
    end function get_rank

    integer function get_size(self) result(n)
        class(RankedArray), intent(in) :: self

        select case (self%rank)
        case (RK_1D)
            if (.not. allocated(self%arr1)) call raise_undefined_error()
            n = size(self%arr1)
        case (RK_2D)
            if (.not. allocated(self%arr2)) call raise_undefined_error()
            n = size(self%arr2)
        case default
            call raise_undefined_error()
        end select
    end function get_size

    subroutine get_shape(self, shape)
        class(RankedArray), intent(in) :: self
        integer, intent(inout) :: shape(:)

        select case (self%rank)
        case (RK_1D)
            if (.not. allocated(self%arr1)) call raise_undefined_error()
            if (size(shape) /= 1) call raise_shape_mismatch_error()
            shape(1) = size(self%arr1)
        case (RK_2D)
            if (.not. allocated(self%arr2)) call raise_undefined_error()
            if (size(shape) /= 2) call raise_shape_mismatch_error()
            shape(1) = size(self%arr2, 1)
            shape(2) = size(self%arr2, 2)
        case default
            call raise_undefined_error()
        end select
    end subroutine get_shape

! ===================================================================================================
subroutine append_ranked_array_1d(arrs, new_arr)
    implicit none
    type(RankedArray), allocatable, intent(inout) :: arrs(:)
    real(kind=JPRB),                intent(in)    :: new_arr(:)

    type(RankedArray), allocatable :: tmp(:)
    type(RankedArray) :: new_item
    integer :: n_old

    call new_item%set(new_arr)

    if (.not. allocated(arrs)) then
        allocate(arrs(1))
        arrs(1) = new_item
        return
    end if

    n_old = size(arrs)
    allocate(tmp(n_old + 1))
    tmp(1:n_old)     = arrs(1:n_old)
    tmp(n_old + 1)   = new_item
    call move_alloc(tmp, arrs)
end subroutine append_ranked_array_1d


subroutine append_ranked_array_2d(arrs, new_arr)
    implicit none
    type(RankedArray), allocatable, intent(inout) :: arrs(:)
    real(kind=JPRB),                intent(in)    :: new_arr(:,:)

    type(RankedArray), allocatable :: tmp(:)
    type(RankedArray) :: new_item
    integer :: n_old

    call new_item%set(new_arr)

    if (.not. allocated(arrs)) then
        allocate(arrs(1))
        arrs(1) = new_item
        return
    end if

    n_old = size(arrs)
    allocate(tmp(n_old + 1))
    tmp(1:n_old)     = arrs(1:n_old)
    tmp(n_old + 1)   = new_item
    call move_alloc(tmp, arrs)
end subroutine append_ranked_array_2d

end module ranked_array_class