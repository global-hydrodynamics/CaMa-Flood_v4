module output_mod
    use PARKIND1, only: &
    &   JPIM, JPRB, JPRM
    use YOS_CMF_INPUT, only: &
    &   LOGNAM
    use YOS_CMF_INPUT, only: &
    &   NX, NY

    use glob_mod, only: &
    &   CLEN_ITEM
    use dim_converter, only: &
    &   vec2map

    use key_table_class, only: &
    &   KeyTable
    use output_conf_class, only: &
    &   OutputConf, init_OutputConf, append_OutputConf
    use output_writer_class, only: &
    &   OutputWriter, append_OutputWriter
    use accumulated_array_class, only: &
    &   AccumulatedArray, append_AccumulatedArray

    implicit none
    private
    public :: &
    &   init_output_mod, update_output, write_output, close_output

    ! ==============================================================================================
    ! Active outputs (all arrays have the same length; index is managed by items_output)
    ! ==============================================================================================
    type(KeyTable) :: &
    &   items_output
    type(OutputConf), allocatable :: &
    &   confs(:)
    type(AccumulatedArray), allocatable :: &
    &   arrs(:)
    type(OutputWriter), allocatable :: &
    &   writers(:)

    ! ----------------------------------------------------------------------------------------------
    ! Cache for "not output" items (avoid repeated namelist scans)
    ! ----------------------------------------------------------------------------------------------
    type(KeyTable) :: &
    &   items_checked

    ! ----------------------------------------------------------------------------------------------
    ! Work buffer for mapfmt conversion (vec -> map)
    ! ----------------------------------------------------------------------------------------------
    real(kind=JPRM), allocatable :: tmp_map(:,:)

    interface update_output
        module procedure update_output_1d
        module procedure update_output_2d
    end interface update_output

contains

    ! ==============================================================================================
    subroutine init_output_mod()
        call items_output%clear()
        call items_checked%clear()

        if (allocated(confs))   deallocate(confs)
        if (allocated(arrs))    deallocate(arrs)
        if (allocated(writers)) deallocate(writers)

        if (.not. allocated(tmp_map)) then
            allocate(tmp_map(NX, NY))
        endif
        tmp_map(:,:) = 0.0
    end subroutine init_output_mod

    ! ==============================================================================================
    subroutine update_output_1d(item, arr)
        character(len=*), intent(in) :: item
        real(kind=JPRB), intent(in) :: arr(:)
        integer(kind=JPIM) :: idx

        idx = ensure_registered_1d(item, size(arr))
        if (idx <= 0) return

        call arrs(idx)%add(arr)
    end subroutine update_output_1d

    subroutine update_output_2d(item, arr)
        character(len=*), intent(in) :: item
        real(kind=JPRB), intent(in) :: arr(:,:)
        integer(kind=JPIM) :: idx

        idx = ensure_registered_2d(item, size(arr, 1), size(arr, 2))
        if (idx <= 0) return

        call arrs(idx)%add(arr)
    end subroutine update_output_2d

    ! ==============================================================================================
    subroutine write_output()
        integer :: i
        if (.not. allocated(confs)) return

        do i = 1, size(confs)
            call write_one(i)
        enddo
    end subroutine write_output

    subroutine close_output()
        integer :: i
        if (.not. allocated(writers)) return

        do i = 1, size(writers)
            call writers(i)%close()
        enddo
    end subroutine close_output

    ! ==============================================================================================
    subroutine write_one(idx)
        integer, intent(in) :: idx

        real(8), allocatable :: buf1(:)
        real(8), allocatable :: buf2(:,:)
        integer :: rk
        logical :: is_mapfmt

        rk = arrs(idx)%get_rank()
        is_mapfmt = confs(idx)%get_is_mapfmt()

        if (is_mapfmt) then
            select case (rk)
            case (1)
                allocate(buf1(arrs(idx)%get_shape(1)))
                call arrs(idx)%flush(buf1) ! average/instantaneous -> buf1; reset inside
                tmp_map(:,:) = 0.0
                call vec2map(buf1(:), tmp_map(:,:))
                call writers(idx)%write_2d(tmp_map(:,:))
                deallocate(buf1)
            case (2)
                call raise_mapfmt_not_supported_for_2d()
            case default
                call raise_unexpected_rank(rk)
            end select

        else
            select case (rk)
            case (1)
                allocate(buf1(arrs(idx)%get_shape(1)))
                call arrs(idx)%flush(buf1)
                call writers(idx)%write_1d(real(buf1(:), kind=JPRM))
                deallocate(buf1)
            case (2)
                allocate(buf2(arrs(idx)%get_shape(1), arrs(idx)%get_shape(2)))
                call arrs(idx)%flush(buf2)
                call writers(idx)%write_2d(real(buf2(:,:), kind=JPRM))
                deallocate(buf2)
            case default
                call raise_unexpected_rank(rk)
            end select
        endif
    end subroutine write_one

    ! ==============================================================================================
    integer function ensure_registered_1d(item, n1) result(idx)
        character(len=*), intent(in) :: item
        integer,          intent(in) :: n1

        type(OutputConf) :: conf
        logical :: is_found

        ! Already checked as not output?
        if (items_checked%find(item) > 0) then
            idx = 0
            return
        endif

        ! Already active?
        idx = items_output%find(item)
        if (idx > 0) return

        ! Query configuration (namelist)
        call init_OutputConf(item, is_found, conf)
        if (.not. is_found) then
            call items_checked%append(item)   ! cache "not output"
            idx = 0
            return
        endif

        ! Register as active (append all arrays in the same order)
        call append_OutputConf(confs, conf)
        call append_AccumulatedArray(arrs)
        call append_OutputWriter(writers)
        call items_output%append(item)

        idx = size(confs)

        ! Open writer and initialize accumulator shape/state
        call writers(idx)%open_1d(confs(idx)%get_path(), n1, confs(idx)%get_is_mapfmt())
        call arrs(idx)%configure(n1, confs(idx)%get_is_mean())

        write(LOGNAM, '(a,1x,a)') '  [output_mod] registered:', trim(item)
    end function ensure_registered_1d

    integer function ensure_registered_2d(item, n1, n2) result(idx)
        character(len=*), intent(in) :: item
        integer,          intent(in) :: n1, n2

        type(OutputConf) :: conf
        logical :: is_found

        if (items_checked%find(item) > 0) then
            idx = 0
            return
        endif

        idx = items_output%find(item)
        if (idx > 0) return

        call init_OutputConf(item, is_found, conf)
        if (.not. is_found) then
            call items_checked%append(item)
            idx = 0
            return
        endif

        call append_OutputConf(confs, conf)
        call append_AccumulatedArray(arrs)
        call append_OutputWriter(writers)
        call items_output%append(item)

        idx = size(confs)

        call writers(idx)%open_2d(confs(idx)%get_path(), n1, n2, confs(idx)%get_is_mapfmt())
        call arrs(idx)%configure(n1, n2, confs(idx)%get_is_mean())

        write(LOGNAM, '(a,1x,a)') '  [output_mod] registered:', trim(item)
    end function ensure_registered_2d

    ! ==============================================================================================
    subroutine raise_mapfmt_not_supported_for_2d()
        write(*, *) '[output_mod ERROR] mapfmt for 2D output is not supported.'
        stop
    end subroutine raise_mapfmt_not_supported_for_2d

    subroutine raise_unexpected_rank(rk)
        integer, intent(in) :: rk
        write(*, *) '[output_mod ERROR] unexpected rank:', rk
        stop
    end subroutine raise_unexpected_rank

end module output_mod