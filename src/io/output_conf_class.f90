module output_conf_class
    use PARKIND1, only: &
    &   JPIM
    use YOS_CMF_INPUT, only: &
    &   TMPNAM, LOGNAM
    use glob_mod, only: &
    &   NML_PATH, BINMAP_SUF, BINVEC_SUF, CLEN_PATH, CLEN_ITEM, CLEN_SHORT
    use io_namelist_mod, only: &
    &   read_nml_output
    implicit none
    private

    public :: OutputConf
    public :: init_OutputConf
    public :: append_OutputConf

    type :: OutputConf
        private
        character(len=CLEN_ITEM) :: item = ''          ! variable identifier in model (e.g., 'Tair', 'Roff')
        character(len=CLEN_PATH) :: path = ''          ! resolved output path with suffix
        logical(kind=4)          :: is_found = .FALSE. ! found in namelist
        logical(kind=4)          :: mapfmt   = .TRUE. ! .TRUE.: 2D map output, .FALSE.: 1D vector output
        logical                  :: is_mean  = .TRUE.   ! .TRUE.: time averaged, .FALSE.: instantaneous
    contains
        procedure :: get_item
        procedure :: get_path
        procedure :: get_is_found
        procedure :: get_mapfmt
        procedure :: get_is_mean
    end type OutputConf

contains


    ! =============================================================================================
    ! Constructor
    ! =============================================================================================
    subroutine init_OutputConf(out_item, obj)
        character(len=*), intent(in)  :: out_item
        type(OutputConf), intent(out) :: obj

        logical(kind=4)          :: is_found, mapfmt
        logical                 :: is_mean
        character(len=CLEN_PATH) :: path

        call read_nml_output(out_item, is_found, path, mapfmt, is_mean)

        obj%item     = out_item
        obj%is_found = is_found

        if (.not. is_found) then
            obj%path   = ''
            obj%mapfmt = .FALSE.
            obj%is_mean = .TRUE.
            return
        endif

        obj%mapfmt  = mapfmt
        obj%is_mean = is_mean

        if (obj%mapfmt) then
            obj%path = trim(path)//trim(BINMAP_SUF)
        else
            obj%path = trim(path)//trim(BINVEC_SUF)
        endif

        write(LOGNAM, '(a,1x,a,1x,a,1x,L1,1x,L1)') '  [OutputConf]', trim(obj%item), trim(obj%path), obj%mapfmt, obj%is_mean
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

    function get_is_found(self) result(res)
        class(OutputConf), intent(in) :: self
        logical :: res
        res = self%is_found
    end function get_is_found

    function get_mapfmt(self) result(res)
        class(OutputConf), intent(in) :: self
        logical :: res
        res = self%mapfmt
    end function get_mapfmt

    function get_is_mean(self) result(res)
        class(OutputConf), intent(in) :: self
        logical :: res
        res = self%is_mean
    end function get_is_mean

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