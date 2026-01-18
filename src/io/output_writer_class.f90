module output_writer_class
    use PARKIND1, only: &
    &   JPIM, JPRB, JPRM
    use YOS_CMF_INPUT, only: &
    &   LOGNAM
    use CMF_UTILS_MOD, only: &
    &   INQUIRE_FID
    use YOS_CMF_INPUT, only: &
    &   NX, NY

    use glob_mod,  only: &
    &   CLEN_PATH
    use dim_converter, only: &
    &   vec2map
    implicit none
    private

    public :: OutputWriter

    integer, parameter :: BYTE_RECL = 4

    type :: OutputWriter
        private
        integer(kind=JPIM) :: unit = -1
        integer(kind=JPIM) :: recl = 0
        integer(kind=JPIM) :: rec  = 0
        logical :: is_open = .false.
        logical :: mapfmt  = .false.
        character(len=CLEN_PATH) :: path = ''
    contains
        procedure :: init          => init_OutputWriter
        procedure :: write_1d      => write_OutputWriter_1d
        procedure :: write_2d      => write_OutputWriter_2d
        procedure :: close         => close_OutputWriter
        procedure :: get_path      => get_path_OutputWriter
        procedure :: get_mapfmt    => get_mapfmt_OutputWriter
    end type OutputWriter

contains

    subroutine init_OutputWriter(self, path, mapfmt, n1, n2)
        class(OutputWriter), intent(inout) :: self
        character(len=*),    intent(in)    :: path
        logical,             intent(in)    :: mapfmt
        integer(kind=JPIM),  intent(in)    :: n1
        integer(kind=JPIM), optional, intent(in) :: n2

        integer(kind=JPIM) :: unit_local
        integer(kind=JPIM) :: recl_local
        integer(kind=JPIM) :: n_elem

        self%path   = trim(path)
        self%mapfmt = mapfmt
        self%rec    = 0

        if (self%mapfmt) then
            recl_local = BYTE_RECL * NX * NY
        else
            if (present(n2)) then
                n_elem = n1 * n2
            else
                n_elem = n1
            end if
            recl_local = BYTE_RECL * n_elem
        end if

        self%recl = recl_local
        unit_local = INQUIRE_FID()
        self%unit = unit_local

        open(self%unit, file=trim(self%path), status='replace', &
            form='unformatted', access='direct', recl=self%recl)

        self%is_open = .true.
        write(LOGNAM, '(a,i3,2a,L1)') '  [OutputWriter] open unit=', self%unit, ' path=', trim(self%path), self%mapfmt
    end subroutine init_OutputWriter


    subroutine write_OutputWriter_1d(self, vec)
        class(OutputWriter), intent(inout) :: self
        real(kind=JPRB),     intent(in)    :: vec(:)

        real(4) :: tmp_map(NX, NY)

        if (.not. self%is_open) then
            write(LOGNAM, *) '[OutputWriter ERROR] write_1d called before open: ', trim(self%path)
            stop
        end if

        self%rec = self%rec + 1

        if (self%mapfmt) then
            tmp_map(:,:) = 0.0
            call vec2map(vec(:), tmp_map(:,:))
            write(self%unit, rec=self%rec) tmp_map(:,:)
        else
            write(self%unit, rec=self%rec) real(vec(:), kind=JPRM)
        end if
    end subroutine write_OutputWriter_1d


    subroutine write_OutputWriter_2d(self, mat)
        class(OutputWriter), intent(inout) :: self
        real(kind=JPRB),     intent(in)    :: mat(:,:)

        if (.not. self%is_open) then
            write(LOGNAM, *) '[OutputWriter ERROR] write_2d called before open: ', trim(self%path)
            stop
        end if

        self%rec = self%rec + 1

        if (self%mapfmt) then
            if (size(mat, 1) /= NX .or. size(mat, 2) /= NY) then
                write(LOGNAM, *) '[OutputWriter ERROR] mapfmt=TRUE requires mat(NX,NY). got: ', size(mat,1), size(mat,2)
                stop
            end if
            write(self%unit, rec=self%rec) real(mat(:,:), kind=JPRM)
        else
            write(self%unit, rec=self%rec) real(mat(:,:), kind=JPRM)
        end if
    end subroutine write_OutputWriter_2d


    subroutine close_OutputWriter(self)
        class(OutputWriter), intent(inout) :: self
        if (self%is_open) then
            close(self%unit)
            self%is_open = .false.
            write(LOGNAM, '(a,i3,2a)') '  [OutputWriter] close unit=', self%unit, ' path=', trim(self%path)
        end if
    end subroutine close_OutputWriter


    character(len=CLEN_PATH) function get_path_OutputWriter(self) result(path)
        class(OutputWriter), intent(in) :: self
        path = self%path
    end function get_path_OutputWriter


    logical function get_mapfmt_OutputWriter(self) result(mapfmt)
        class(OutputWriter), intent(in) :: self
        mapfmt = self%mapfmt
    end function get_mapfmt_OutputWriter

end module output_writer_class