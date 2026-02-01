module array_dict_class
    use PARKIND1, only: JPRB
    use const_mod, only: CLEN_SHORT
    use array_mod, only: find_index
    implicit none
    private

    public :: ArrayDict

    integer, parameter :: RK_UNDEF = 0
    integer, parameter :: RK_1D    = 1
    integer, parameter :: RK_2D    = 2

    type :: ArrayItem
        integer :: rank = RK_UNDEF
        real(kind=JPRB), allocatable :: arr1(:)
        real(kind=JPRB), allocatable :: arr2(:,:)
    end type ArrayItem

    type :: ArrayDict
        integer :: n    = 0        ! number of used entries
        integer :: nmax = 0        ! allocated capacity
        integer :: key_len = CLEN_SHORT
        character(len=CLEN_SHORT), allocatable :: keys(:)
        type(ArrayItem), allocatable :: items(:)
    contains
        procedure :: init
        procedure :: clear

        procedure :: set_1d
        procedure :: set_2d
        generic   :: set => set_1d, set_2d

        procedure :: get_1d
        procedure :: get_2d
        generic   :: get => get_1d, get_2d

        procedure :: has_key
        procedure :: get_rank
        procedure :: get_size
        procedure :: get_shape
    end type ArrayDict

contains

! ===================================================================================================
subroutine raise_not_found_error(key)
    character(len=*), intent(in) :: key
    write(*, *) '[ArrayDict ERROR] key not found: ', trim(key)
    stop
end subroutine raise_not_found_error

subroutine raise_rank_mismatch_error(key, expected_rank, actual_rank)
    character(len=*), intent(in) :: key
    integer, intent(in) :: expected_rank, actual_rank
    write(*, *) '[ArrayDict ERROR] rank mismatch for key: ', trim(key), &
                ' expected=', expected_rank, ' actual=', actual_rank
    stop
end subroutine raise_rank_mismatch_error

subroutine raise_shape_mismatch_error(key)
    character(len=*), intent(in) :: key
    write(*, *) '[ArrayDict ERROR] shape mismatch for key: ', trim(key)
    stop
end subroutine raise_shape_mismatch_error

subroutine raise_shape_expected_error_1d(key, n_expected, n_got)
    character(len=*), intent(in) :: key
    integer, intent(in) :: n_expected, n_got
    write(*, *) '[ArrayDict ERROR] shape mismatch for key: ', trim(key)
    write(*, *) '  expected: (', n_expected, ')'
    write(*, *) '  got     : (', n_got, ')'
    stop
end subroutine raise_shape_expected_error_1d

subroutine raise_shape_expected_error_2d(key, n1e, n2e, n1g, n2g)
    character(len=*), intent(in) :: key
    integer, intent(in) :: n1e, n2e, n1g, n2g
    write(*, *) '[ArrayDict ERROR] shape mismatch for key: ', trim(key)
    write(*, *) '  expected: (', n1e, ',', n2e, ')'
    write(*, *) '  got     : (', n1g, ',', n2g, ')'
    stop
end subroutine raise_shape_expected_error_2d

subroutine raise_key_length_error(key, maxlen)
    character(len=*), intent(in) :: key
    integer, intent(in)          :: maxlen
    write(*, *) '[ArrayDict ERROR] key length exceeds maximum allowed length'
    write(*, *) '  key    = "', trim(key), '"'
    write(*, *) '  length = ', len_trim(key), ', max = ', maxlen
    stop
end subroutine raise_key_length_error

subroutine check_key_length(key, maxlen)
    character(len=*), intent(in) :: key
    integer, intent(in)          :: maxlen

    if (len_trim(key) > maxlen) then
        call raise_key_length_error(key, maxlen)
    end if
end subroutine check_key_length

! ===================================================================================================
subroutine init(self, capacity)
    class(ArrayDict), intent(inout) :: self
    integer, intent(in), optional :: capacity ! default: 0 (no pre-allocation)
    integer :: cap

    call self%clear()

    cap = 0
    if (present(capacity)) cap = max(0, capacity)

    if (cap > 0) then
        allocate(self%keys(cap))
        allocate(self%items(cap))
        self%keys(:) = ''
        self%n = 0
    end if
end subroutine init


subroutine clear(self)
    class(ArrayDict), intent(inout) :: self
    integer :: i

    if (allocated(self%items)) then
        do i = 1, size(self%items)
            call item_dealloc(self%items(i))
        end do
        deallocate(self%items)
    end if

    if (allocated(self%keys)) deallocate(self%keys)
    self%n = 0
end subroutine clear


subroutine set_1d(self, key, arr)
    class(ArrayDict), intent(inout) :: self
    character(len=*), intent(in)    :: key
    real(kind=JPRB), intent(in)     :: arr(:)

    integer :: idx

    call check_key_length(key, self%key_len)
    idx = find_index(self%keys, key)

    if (idx > 0) then
        if (self%items(idx)%rank /= 1) then
            write(*, *) '[ArrayDict ERROR] rank mismatch in set_1d: ', trim(key)
            stop
        endif
        if (.not. allocated(self%items(idx)%arr1)) then
            write(*, *) '[ArrayDict ERROR] internal error: arr1 not allocated: ', trim(key)
            stop
        endif
        if (size(self%items(idx)%arr1) /= size(arr)) then
            write(*, *) '[ArrayDict ERROR] shape mismatch in set_1d: ', trim(key)
            stop
        endif

        ! ここは再allocateさせない（形状は固定）
        self%items(idx)%arr1(:) = arr(:)
        return
    endif

    ! 新規キー：スロット確保（必要なら内部で拡張）
    idx = ensure_slot(self, key)

    self%items(idx)%rank = 1
    if (allocated(self%items(idx)%arr2)) deallocate(self%items(idx)%arr2)
    if (allocated(self%items(idx)%arr1)) deallocate(self%items(idx)%arr1)

    allocate(self%items(idx)%arr1(size(arr)))
    self%items(idx)%arr1(:) = arr(:)
end subroutine set_1d


subroutine set_2d(self, key, arr)
    class(ArrayDict), intent(inout) :: self
    character(len=*), intent(in)    :: key
    real(kind=JPRB), intent(in)     :: arr(:,:)

    integer :: idx
    integer :: n1, n2

    call check_key_length(key, self%key_len)
    idx = find_index(self%keys, key)

    if (idx > 0) then
        if (self%items(idx)%rank /= 2) then
            write(*, *) '[ArrayDict ERROR] rank mismatch in set_2d: ', trim(key)
            stop
        endif
        if (.not. allocated(self%items(idx)%arr2)) then
            write(*, *) '[ArrayDict ERROR] internal error: arr2 not allocated: ', trim(key)
            stop
        endif
        if (size(self%items(idx)%arr2, 1) /= size(arr, 1) .or. &
            size(self%items(idx)%arr2, 2) /= size(arr, 2)) then
            write(*, *) '[ArrayDict ERROR] shape mismatch in set_2d: ', trim(key)
            stop
        endif

        ! 再allocateさせない（形状は固定）
        self%items(idx)%arr2(:,:) = arr(:,:)
        return
    endif

    ! 新規キー：スロット確保（必要なら内部で拡張）
    idx = ensure_slot(self, key)

    self%items(idx)%rank = 2
    if (allocated(self%items(idx)%arr1)) deallocate(self%items(idx)%arr1)
    if (allocated(self%items(idx)%arr2)) deallocate(self%items(idx)%arr2)

    n1 = size(arr, 1)
    n2 = size(arr, 2)
    allocate(self%items(idx)%arr2(n1, n2))
    self%items(idx)%arr2(:,:) = arr(:,:)
end subroutine set_2d


subroutine get_1d(self, key, arr)
    class(ArrayDict), intent(in) :: self
    character(len=*), intent(in) :: key
    real(kind=JPRB), intent(out) :: arr(:)

    integer :: idx

    call check_key_length(key, self%key_len)
    idx = find_index(self%keys, key)
    if (idx <= 0) call raise_not_found_error(key)

    if (self%items(idx)%rank /= RK_1D) then
        call raise_rank_mismatch_error(key, expected_rank=RK_1D, actual_rank=self%items(idx)%rank)
    end if
    if (.not. allocated(self%items(idx)%arr1)) then
        call raise_shape_mismatch_error(key)
    end if

    if (size(arr, 1) /= size(self%items(idx)%arr1, 1)) then
        call raise_shape_expected_error_1d(key, size(self%items(idx)%arr1, 1), size(arr, 1))
    end if

    arr(:) = self%items(idx)%arr1(:)
end subroutine get_1d


subroutine get_2d(self, key, arr)
    class(ArrayDict), intent(in) :: self
    character(len=*), intent(in) :: key
    real(kind=JPRB), intent(out) :: arr(:,:)

    integer :: idx
    integer :: n1, n2

    call check_key_length(key, self%key_len)
    idx = find_index(self%keys, key)
    if (idx <= 0) call raise_not_found_error(key)

    if (self%items(idx)%rank /= RK_2D) then
        call raise_rank_mismatch_error(key, expected_rank=RK_2D, actual_rank=self%items(idx)%rank)
    end if
    if (.not. allocated(self%items(idx)%arr2)) then
        call raise_shape_mismatch_error(key)
    end if

    n1 = size(self%items(idx)%arr2, 1)
    n2 = size(self%items(idx)%arr2, 2)
    if (size(arr, 1) /= n1 .or. size(arr, 2) /= n2) then
        call raise_shape_expected_error_2d(key, n1, n2, size(arr, 1), size(arr, 2))
    end if

    arr(:,:) = self%items(idx)%arr2(:,:)
end subroutine get_2d

! ===================================================================================================
logical function has_key(self, key) result(ok)
    class(ArrayDict), intent(in) :: self
    character(len=*), intent(in) :: key
    call check_key_length(key, self%key_len)
    ok = (find_index(self%keys, key) > 0)
end function has_key


integer function get_rank(self, key) result(rk)
    class(ArrayDict), intent(in) :: self
    character(len=*), intent(in) :: key
    integer :: idx

    call check_key_length(key, self%key_len)
    idx = find_index(self%keys, key)
    if (idx <= 0) call raise_not_found_error(key)

    rk = self%items(idx)%rank
end function get_rank


integer function get_size(self, key) result(n)
    class(ArrayDict), intent(in) :: self
    character(len=*), intent(in) :: key
    integer :: idx

    call check_key_length(key, self%key_len)
    idx = find_index(self%keys, key)
    if (idx <= 0) call raise_not_found_error(key)

    select case (self%items(idx)%rank)
    case (RK_1D)
        if (.not. allocated(self%items(idx)%arr1)) call raise_shape_mismatch_error(key)
        n = size(self%items(idx)%arr1)
    case (RK_2D)
        if (.not. allocated(self%items(idx)%arr2)) call raise_shape_mismatch_error(key)
        n = size(self%items(idx)%arr2)
    case default
        call raise_rank_mismatch_error(key, expected_rank=RK_1D, actual_rank=self%items(idx)%rank)
        ! 上で stop するので到達しません
        n = 0
    end select
end function get_size


subroutine get_shape(self, key, shape)
    class(ArrayDict), intent(in) :: self
    character(len=*), intent(in) :: key
    integer, allocatable, intent(out) :: shape(:)
    integer :: idx

    call check_key_length(key, self%key_len)
    idx = find_index(self%keys, key)
    if (idx <= 0) call raise_not_found_error(key)

    select case (self%items(idx)%rank)
    case (RK_1D)
        if (.not. allocated(self%items(idx)%arr1)) call raise_shape_mismatch_error(key)
        allocate(shape(1))
        shape(1) = size(self%items(idx)%arr1, 1)
    case (RK_2D)
        if (.not. allocated(self%items(idx)%arr2)) call raise_shape_mismatch_error(key)
        allocate(shape(2))
        shape(1) = size(self%items(idx)%arr2, 1)
        shape(2) = size(self%items(idx)%arr2, 2)
    case default
        call raise_rank_mismatch_error(key, expected_rank=RK_1D, actual_rank=self%items(idx)%rank)
        ! stop
    end select
end subroutine get_shape

! ===================================================================================================
integer function ensure_slot(self, key) result(idx)
    class(arraydict), intent(inout) :: self
    character(len=*), intent(in)    :: key

    integer :: newcap
    character(len=CLEN_SHORT), allocatable :: keys_new(:)
    type(ArrayItem),           allocatable :: items_new(:)

    ! increase capacity if needed
    if (self%n >= self%nmax) then
        if (self%nmax <= 0) then
            newcap = 8
        else
            newcap = self%nmax * 2
        endif

        allocate(keys_new(newcap))
        allocate(items_new(newcap))
        keys_new(:) = ''

        if (self%n > 0) then
            keys_new(1:self%n)  = self%keys(1:self%n)
            items_new(1:self%n) = self%items(1:self%n)
        endif

        call move_alloc(keys_new,  self%keys)
        call move_alloc(items_new, self%items)
        self%nmax = newcap
    endif

    ! new slot
    self%n = self%n + 1
    idx = self%n

    self%keys(idx) = adjustl(key)

    self%items(idx)%rank = 0
    if (allocated(self%items(idx)%arr1)) deallocate(self%items(idx)%arr1)
    if (allocated(self%items(idx)%arr2)) deallocate(self%items(idx)%arr2)
end function ensure_slot


subroutine item_dealloc(item)
    type(ArrayItem), intent(inout) :: item

    if (allocated(item%arr1)) deallocate(item%arr1)
    if (allocated(item%arr2)) deallocate(item%arr2)
    item%rank = RK_UNDEF
end subroutine item_dealloc


subroutine item_copy(dst, src)
    type(ArrayItem), intent(inout) :: dst
    type(ArrayItem), intent(in)    :: src

    call item_dealloc(dst)
    dst%rank = src%rank

    select case (src%rank)
    case (RK_1D)
        if (allocated(src%arr1)) then
            allocate(dst%arr1(size(src%arr1, 1)))
            dst%arr1 = src%arr1
        end if
    case (RK_2D)
        if (allocated(src%arr2)) then
            allocate(dst%arr2(size(src%arr2, 1), size(src%arr2, 2)))
            dst%arr2 = src%arr2
        end if
    case default
        ! do nothing
    end select
end subroutine item_copy

end module array_dict_class