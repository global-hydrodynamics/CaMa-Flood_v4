module output_writer_class
    use PARKIND1, only: &
    &   JPIM, JPRM
    use YOS_CMF_INPUT, only: &
    &   LOGNAM
    use CMF_UTILS_MOD, only: &
    &   INQUIRE_FID
    use YOS_CMF_INPUT, only: &
    &   NX, NY
    use const_mod, only: &
    &   CLEN_PATH
    implicit none
    private

    public :: &
    &   OutputWriter, BYTE_RECL, append_OutputWriter

    integer, parameter :: &
    &   BYTE_RECL = 4 ! 4-byte binary (real(4))

    type :: OutputWriter
        private
        integer(kind=JPIM) :: &
        &   unit = -1, &
        &   rec  = 0,  &
        &   recl = 0
        character(len=CLEN_PATH) :: &
        &   path = ''
        logical :: &
        &   is_open = .FALSE.
    contains
        procedure :: open_1d
        procedure :: open_2d
        procedure :: write_1d
        procedure :: write_2d
        procedure :: close
        procedure :: get_unit
        procedure :: get_rec
        procedure :: is_opened
    end type OutputWriter

contains

    subroutine raise_not_open_error(path)
        character(len=*), intent(in) :: path
        write(*, *) '[OutputWriter ERROR] file is not open: ', trim(path)
        stop
    end subroutine raise_not_open_error

    subroutine raise_already_open_error(path)
        character(len=*), intent(in) :: path
        write(*, *) '[OutputWriter ERROR] file is already open: ', trim(path)
        stop
    end subroutine raise_already_open_error

    subroutine raise_open_error(path)
        character(len=*), intent(in) :: path
        write(*, *) '[OutputWriter ERROR] failed to open: ', trim(path)
        stop
    end subroutine raise_open_error

    subroutine open_1d(self, path, n1, mapfmt)
        class(OutputWriter), intent(inout) :: self
        character(len=*),    intent(in)    :: path
        integer(kind=JPIM),  intent(in)    :: n1
        logical,             intent(in)    :: mapfmt

        integer :: unit, recl_local, ios

        if (self%is_open) call raise_already_open_error(self%path)

        !unit = INQUIRE_FID()
        if (mapfmt) then
            recl_local = BYTE_RECL * NX * NY
        else
            recl_local = BYTE_RECL * n1
        endif

        !open(unit, file=trim(path), status='replace', form='unformatted', &
        !&    access='direct', recl=recl_local, iostat=ios)
        open(newunit=unit, file=trim(path), status='replace', form='unformatted', &
        &    access='direct', recl=recl_local, iostat=ios)
        if (ios /= 0) call raise_open_error(path)

        self%unit    = unit
        self%rec     = 0
        self%recl    = recl_local
        self%path    = trim(path)
        self%is_open = .TRUE.

        write(LOGNAM, '(a,i0,2a)') '  [OutputWriter] opened unit=', self%unit, ' path=', trim(self%path)
    end subroutine open_1d

    subroutine open_2d(self, path, n1, n2, mapfmt)
        class(OutputWriter), intent(inout) :: self
        character(len=*),    intent(in)    :: path
        integer,             intent(in)    :: n1, n2
        logical,             intent(in)    :: mapfmt

        integer :: unit, recl_local, ios

        if (self%is_open) call raise_already_open_error(self%path)

        unit = INQUIRE_FID()
        if (mapfmt) then
            recl_local = BYTE_RECL * NX * NY
        else
            recl_local = BYTE_RECL * n1 * n2
        endif

        open(unit, file=trim(path), status='replace', form='unformatted', &
        &    access='direct', recl=recl_local, iostat=ios)
        if (ios /= 0) call raise_open_error(path)

        self%unit    = unit
        self%rec     = 0
        self%recl    = recl_local
        self%path    = trim(path)
        self%is_open = .TRUE.

        write(LOGNAM, '(a,i0,2a)') '  [OutputWriter] opened unit=', self%unit, ' path=', trim(self%path)
    end subroutine open_2d

    subroutine write_1d(self, vec_r4)
        class(OutputWriter), intent(inout) :: self
        real(kind=JPRM), intent(in)    :: vec_r4(:)

        if (.not. self%is_open) call raise_not_open_error(self%path)

        self%rec = self%rec + 1
        write(self%unit, rec=self%rec) vec_r4(:)
    end subroutine write_1d

    subroutine write_2d(self, map_r4)
        class(OutputWriter), intent(inout) :: self
        real(kind=JPRM), intent(in)    :: map_r4(:,:)

        if (.not. self%is_open) call raise_not_open_error(self%path)

        self%rec = self%rec + 1
        write(self%unit, rec=self%rec) map_r4(:,:)
    end subroutine write_2d

    subroutine close(self)
        class(OutputWriter), intent(inout) :: self

        if (.not. self%is_open) return

        close(self%unit)
        write(LOGNAM, '(a,i0,2a)') '  [OutputWriter] closed unit=', self%unit, ' path=', trim(self%path)

        self%unit    = -1
        self%rec     = 0
        self%recl    = 0
        self%path    = ''
        self%is_open = .FALSE.
    end subroutine close

    integer(kind=JPIM) function get_unit(self) result(unit)
        class(OutputWriter), intent(in) :: self
        unit = self%unit
    end function get_unit

    integer(kind=JPIM) function get_rec(self) result(rec)
        class(OutputWriter), intent(in) :: self
        rec = self%rec
    end function get_rec

    logical function is_opened(self) result(res)
        class(OutputWriter), intent(in) :: self
        res = self%is_open
    end function is_opened

    ! ==============================================================================================
    subroutine append_OutputWriter(array)
        type(OutputWriter), allocatable, intent(inout) :: array(:)
        type(OutputWriter), allocatable :: tmp(:)
        integer :: n

        if (.not. allocated(array)) then
            allocate(array(1))
            return
        endif

        n = size(array)
        allocate(tmp(n + 1))
        tmp(1:n) = array(1:n)
        call move_alloc(tmp, array)
    end subroutine append_OutputWriter

end module output_writer_class