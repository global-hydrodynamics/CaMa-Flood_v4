module key_table_class
    use YOS_CMF_INPUT, only: &
    &   LOGNAM
    use const_mod, only: CLEN_SHORT
    use array_mod, only: find_index
    implicit none
    private
    public :: KeyTable
    type :: KeyTable
        integer :: key_len = CLEN_SHORT
        character(len=CLEN_SHORT), allocatable :: keys(:)
    contains
        procedure :: init
        procedure :: clear
        procedure :: has_key
        procedure :: find
        procedure :: append
    end type KeyTable

contains

    subroutine raise_not_found_error(key)
        character(len=*), intent(in) :: key
        write(LOGNAM, '(2a)') '[KeyTable ERROR] key not found: ', trim(key)
        stop 1
    end subroutine raise_not_found_error

    subroutine raise_key_length_error(key, key_len)
        character(len=*), intent(in) :: key
        integer,          intent(in) :: key_len
        write(LOGNAM, '(6a)') '[KeyTable ERROR] key too long (len_trim(key)=', len_trim(key), &
                    ', allowed<=', key_len, '): ', trim(key)
        stop 1
    end subroutine raise_key_length_error

    subroutine check_key_length(key, key_len)
        character(len=*), intent(in) :: key
        integer,          intent(in) :: key_len
        if (len_trim(key) > key_len) then
            call raise_key_length_error(key, key_len)
        end if
    end subroutine check_key_length

    subroutine raise_duplicate_key_error(key)
        character(len=*), intent(in) :: key
        write(LOGNAM, '(2a)') '[KeyTable ERROR] duplicate key: ', trim(key)
        stop 1
    end subroutine raise_duplicate_key_error

    !==========================================================================
    subroutine init(self)
        class(KeyTable), intent(inout) :: self
        call self%clear()
    end subroutine init

    subroutine clear(self)
        class(KeyTable), intent(inout) :: self
        if (allocated(self%keys)) deallocate(self%keys)
    end subroutine clear

    logical function has_key(self, key) result(found)
        class(KeyTable), intent(in) :: self
        character(len=*), intent(in) :: key
        integer :: idx
        call check_key_length(key, self%key_len)
        if (.not. allocated(self%keys)) then
            found = .false.
            return
        end if
        idx = find_index(self%keys, key)
        found = (idx > 0)
    end function has_key

    integer function find(self, key) result(idx)
        class(KeyTable), intent(in) :: self
        character(len=*), intent(in) :: key
        call check_key_length(key, self%key_len)
        !if (.not. allocated(self%keys)) then
        !    call raise_not_found_error(key)
        !end if
        idx = find_index(self%keys, key)
        !if (idx <= 0) then
        !    call raise_not_found_error(key)
        !end if
    end function find

    subroutine append(self, key)
        class(KeyTable), intent(inout) :: self
        character(len=*), intent(in)   :: key

        character(len=CLEN_SHORT), allocatable :: keys_new(:)
        integer :: n_old

        call check_key_length(key, self%key_len)

        if (self%has_key(key)) then
            call raise_duplicate_key_error(key)
        end if

        if (.not. allocated(self%keys)) then
            allocate(self%keys(1))
            self%keys(1) = ''
            self%keys(1)(1:len_trim(key)) = key(1:len_trim(key))
            return
        end if

        n_old = size(self%keys)
        allocate(keys_new(n_old + 1))
        keys_new(1:n_old) = self%keys(1:n_old)

        keys_new(n_old + 1) = ''
        keys_new(n_old + 1)(1:len_trim(key)) = key(1:len_trim(key))

        call move_alloc(keys_new, self%keys)
    end subroutine append

end module key_table_class