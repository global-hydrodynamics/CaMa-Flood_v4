module key_table_class
    use array_lib, only: find_index
    use glob_mod, only: CLEN_SHORT
    implicit none
    private

    public :: KeyTable
    public :: raise_not_found_error
    public :: check_key_length

    type :: KeyTable
        integer :: n = 0    ! used
        integer :: nmax = 0 ! allocated
        integer :: key_len = CLEN_SHORT
        character(len=CLEN_SHORT), allocatable :: keys(:)
    contains
        procedure :: init
        procedure :: clear

        procedure :: find
        procedure :: has_key
        procedure :: ensure
    end type KeyTable

contains

    subroutine init(self, nmax_init)
        class(KeyTable), intent(inout) :: self
        integer, intent(in), optional  :: nmax_init
        integer :: cap

        call self%clear()

        cap = 0
        if (present(nmax_init)) cap = nmax_init
        if (cap < 0) cap = 0

        if (cap > 0) then
            allocate(self%keys(cap))
            self%keys(:) = ''
            self%nmax = cap
        else
            self%nmax = 0
        end if

        self%n = 0
        self%key_len = CLEN_SHORT
    end subroutine init


    subroutine clear(self)
        class(KeyTable), intent(inout) :: self

        if (allocated(self%keys)) deallocate(self%keys)
        self%n = 0
        self%nmax = 0
        self%key_len = CLEN_SHORT
    end subroutine clear


    logical function has_key(self, key) result(found)
        class(KeyTable), intent(in) :: self
        character(len=*), intent(in) :: key

        found = (self%find(key) > 0)
    end function has_key


    integer function find(self, key) result(idx)
        class(KeyTable), intent(in) :: self
        character(len=*), intent(in) :: key
        integer :: i
        character(len=CLEN_SHORT) :: kfix

        call check_key_length(key, self%key_len)
        call normalize_key_fixed(kfix, key, self%key_len)

        idx = 0
        do i = 1, self%n
            if (self%keys(i) == kfix) then
                idx = i
                return
            end if
        end do
    end function find


    integer function ensure(self, key) result(idx)
        class(KeyTable), intent(inout) :: self
        character(len=*), intent(in)   :: key
        character(len=CLEN_SHORT) :: kfix

        call check_key_length(key, self%key_len)
        call normalize_key_fixed(kfix, key, self%key_len)

        ! if exists, return index
        idx = self%find(key)
        if (idx > 0) return

        ! new key, append
        call ensure_capacity(self, self%n + 1)

        self%n = self%n + 1
        self%keys(self%n) = kfix
        idx = self%n
    end function ensure


    subroutine ensure_capacity(self, needed)
        class(KeyTable), intent(inout) :: self
        integer, intent(in)            :: needed
        integer :: newcap
        character(len=CLEN_SHORT), allocatable :: keys_new(:)

        if (needed <= self%nmax) return

        if (self%nmax <= 0) then
            newcap = max(1, needed)
        else
            newcap = self%nmax
            do while (newcap < needed)
                newcap = newcap * 2
            end do
        end if

        allocate(keys_new(newcap))
        keys_new(:) = ''

        if (self%n > 0) then
            keys_new(1:self%n) = self%keys(1:self%n)
        end if

        call move_alloc(keys_new, self%keys)
        self%nmax = newcap
    end subroutine ensure_capacity

    ! ===============================================================================================
    subroutine normalize_key_fixed(kfix, key, key_len)
        character(len=CLEN_SHORT), intent(out) :: kfix
        character(len=*), intent(in)           :: key
        integer, intent(in)                    :: key_len
        integer :: n

        kfix = ''
        n = len_trim(key)
        if (n <= 0) return
        n = min(n, key_len)
        kfix(1:n) = key(1:n)
    end subroutine normalize_key_fixed


    subroutine check_key_length(key, key_len)
        character(len=*), intent(in) :: key
        integer, intent(in)          :: key_len
        integer :: n

        n = len_trim(key)
        if (n > key_len) then
            call raise_key_length_error(key, key_len)
        end if
    end subroutine check_key_length


    subroutine raise_key_length_error(key, key_len)
        character(len=*), intent(in) :: key
        integer, intent(in)          :: key_len
        write(*, *) '[KeyTable ERROR] key is too long (len_trim=', len_trim(key), &
                    ', max=', key_len, '): ', trim(key)
        stop
    end subroutine raise_key_length_error


    subroutine raise_not_found_error(key)
        character(len=*), intent(in) :: key
        write(*, *) '[KeyTable ERROR] key not found: ', trim(key)
        stop
    end subroutine raise_not_found_error

end module key_table_class