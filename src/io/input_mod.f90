module input_mod
    use PARKIND1, only: &
    &   JPIM, JPRB, JPRM
    use glob_mod, only: &
    &   CLEN_ITEM
    use array_lib, only: &
    &   find_index, append
    use funit_lib, only: &
    &   TMP_UNIT, LOG_UNIT
    use YOS_CMF_MAP, only: &
    &   NSEQMAX
    use glob_mod, only: &
    &   LHEATLINK, LLAKE, CSHORT_DEF
    use ranked_array_class, only: &
    &   RankedArray, append_ranked_array
    use input_conf_class, only: &
    &   InputConf, init_InputConf, append_InputConf
    use util_lib, only: &
    &   write_string_with_indent
    !use correct_mod, only: &
    !&   fill_map, scale_map, minimum, div
    use io_namelist_mod, only: &
    &   open_namelist
    !use output_mod, only: &
    !&   update_output
    implicit none
    private
    public :: &
    &   init_input_mod, update_input, is_input, log_input, get_shortest_input_dt, get_input

    character(len=CLEN_ITEM), allocatable :: &
    &   items(:)
    integer(kind=JPIM) :: &
    &   IN_ITEM_NUM = 0! number of input item
    type(InputConf), allocatable :: &
    &   confs(:)
    type(RankedArray), allocatable :: &
    &   arrs(:)
    logical, allocatable :: &
    &   IS_UPDATED(:)

contains

! ===================================================================================================
! Initialization
! ===================================================================================================
subroutine init_input_mod(t)
    integer(kind=JPIM), intent(in) :: t

    write(LOG_UNIT, '(a)') '[init_input_mod]'
    call open_namelist(TMP_UNIT)

    call add_input_conf('ROFF', TMP_UNIT, t)
    !if (LROFSCL) call add_input_conf('ROFSCL', TMP_UNIT, t)
    if ( LLAKE ) then
        call add_input_conf('RAIN', TMP_UNIT, t)
        call add_input_conf('SNOW', TMP_UNIT, t)
    endif
    if (LHEATLINK) then
        call add_input_conf('LWDN', TMP_UNIT, t)
        call add_input_conf('PSRF', TMP_UNIT, t)
        call add_input_conf('QAIR', TMP_UNIT, t)
        call add_input_conf('SWDN', TMP_UNIT, t)
        call add_input_conf('TAIR', TMP_UNIT, t)
        call add_input_conf('WIND', TMP_UNIT, t)
        call add_input_conf('TROF', TMP_UNIT, t)
    endif
    allocate(IS_UPDATED(IN_ITEM_NUM)); IS_UPDATED(:) = .TRUE.
    close(TMP_UNIT)
    write(LOG_UNIT, *)
end subroutine init_input_mod


subroutine add_input_conf(item, nml_unit, t)
    character(len=*), intent(in) :: item
    integer(kind=JPIM), intent(in) :: &
    &   nml_unit, &
    &   t ! [sec] current time
    real(kind=JPRB) :: &
    &   arr(NSEQMAX)
    integer(kind=JPIM) :: &
    &   idx

    idx = find_index(items, item)
    if (idx < 1) then
        call append_InputConf(confs, init_InputConf(item, nml_unit, t))
        call append(items, item)
        IN_ITEM_NUM = IN_ITEM_NUM + 1
        arr(:) = 0.0_JPRB
        call append_ranked_array(arrs, arr)
    endif
end subroutine add_input_conf

! ===================================================================================================
logical function is_input(item) result(isin)
    character(len=*), intent(in) :: item
    isin = .FALSE.
    if (find_index(items, item) > 0) isin = .TRUE.
end function is_input

! ===================================================================================================
! Update Input
! ===================================================================================================
subroutine update_input(now_t)
    integer(kind=JPIM), intent(in) :: &
    &   now_t ! [sec] current time
    real(kind=JPRB) :: &
    &   arr(NSEQMAX)
    integer(kind=JPIM) :: i

    IS_UPDATED(:) = .FALSE.
!    write(LOG_UNIT, '(2a)'), '  update input for ', now_t%strftime('%Y/%m/%d/%H:%M')

    do i = 1, IN_ITEM_NUM
        if (confs(i)%update_needed(now_t)) then
            call confs(i)%update_input(arr)
            call arrs(i)%set(arr)
            IS_UPDATED(i) = .TRUE.
        else
            write(LOG_UNIT, '(a10,a)') trim(confs(i)%get_item()), ': not updated'
        endif
    enddo
    ! omp has to wait for other variables
    ! because the following correction refers to other variables

    do i = 1, IN_ITEM_NUM
        if (.not. IS_UPDATED(i)) cycle
        call arrs(i)%get(arr)
        !call correct_input(confs(i), arr)
    enddo

    !do i = 1, IN_ITEM_NUM
    !    call update_output(trim(items(i)), confs(i)%now_data(:,1))
    !enddo
    !call log_input
end subroutine update_input

! ===================================================================================================
subroutine log_input
    integer(kind=JPIM) :: i
    real(kind=JPRB) :: &
    &   arr(NSEQMAX)
    call write_string_with_indent(2, 'Input value range:')
    do i = 1, IN_ITEM_NUM
        call arrs(i)%get(arr)
        write(LOG_UNIT, *) trim(confs(i)%get_item()), minval(arr), maxval(arr)
    enddo
end subroutine log_input


integer(kind=JPIM) function get_shortest_input_dt() result(min_dt)
    integer(kind=JPIM) :: i
    min_dt = 3600 * 24 * 365 ! large value
    do i = 1, IN_ITEM_NUM
        min_dt = min(min_dt, confs(i)%get_dt())
    enddo
end function get_shortest_input_dt

! ===================================================================================================
subroutine get_input(item, arr)
    character(len=*), intent(in) :: item
    real(kind=JPRB), intent(out) :: arr(:)
    integer(kind=JPIM) :: idx
    idx = find_index(items, item)
    call arrs(idx)%get(arr)
end subroutine get_input

end module input_mod
