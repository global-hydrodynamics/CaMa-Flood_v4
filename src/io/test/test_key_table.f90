program test_key_table
    use glob_mod, only: CLEN_SHORT
    use key_table_class, only: KeyTable
    implicit none

    type(KeyTable) :: kt
    integer :: idx1, idx2, idx3, idx4
    integer :: nmax0

    call banner('KeyTable test start')

    !------------------------------------------------------------
    ! 1) init / empty
    !------------------------------------------------------------
    call kt%init(2)
    call assert_i('init: n==0', kt%n, 0)
    call assert_i('init: nmax==2', kt%nmax, 2)
    call assert_i('init: key_len==CLEN_SHORT', kt%key_len, CLEN_SHORT)

    call assert_l('empty: has_key("Tair")==F', kt%has_key('Tair'), .false.)
    call assert_i('empty: find("Tair")==0', kt%find('Tair'), 0)

    nmax0 = kt%nmax

    !------------------------------------------------------------
    ! 2) ensure: new keys
    !------------------------------------------------------------
    idx1 = kt%ensure('Tair')
    call assert_i('ensure Tair: idx==1', idx1, 1)
    call assert_i('ensure Tair: n==1', kt%n, 1)
    call assert_l('has_key Tair', kt%has_key('Tair'), .true.)
    call assert_i('find Tair', kt%find('Tair'), 1)

    idx2 = kt%ensure('Qair')
    call assert_i('ensure Qair: idx==2', idx2, 2)
    call assert_i('ensure Qair: n==2', kt%n, 2)
    call assert_l('has_key Qair', kt%has_key('Qair'), .true.)
    call assert_i('find Qair', kt%find('Qair'), 2)

    call assert_i('capacity unchanged (still 2)', kt%nmax, nmax0)

    !------------------------------------------------------------
    ! 3) ensure: existing key must return same idx, n not increase
    !------------------------------------------------------------
    idx3 = kt%ensure('Tair')
    call assert_i('ensure existing Tair: idx==1', idx3, 1)
    call assert_i('ensure existing Tair: n still 2', kt%n, 2)

    !------------------------------------------------------------
    ! 4) ensure: trigger growth
    !------------------------------------------------------------
    idx4 = kt%ensure('Wind_E')
    call assert_i('ensure Wind_E: idx==3', idx4, 3)
    call assert_i('ensure Wind_E: n==3', kt%n, 3)
    call assert_l('has_key Wind_E', kt%has_key('Wind_E'), .true.)
    call assert_i('find Wind_E', kt%find('Wind_E'), 3)

    call assert_l('capacity grew', kt%nmax > nmax0, .true.)

    !------------------------------------------------------------
    ! 5) clear
    !------------------------------------------------------------
    call kt%clear()
    call assert_i('clear: n==0', kt%n, 0)
    call assert_i('clear: nmax==0', kt%nmax, 0)
    call assert_l('clear: has_key("Tair")==F', kt%has_key('Tair'), .false.)

    call banner('KeyTable test PASSED')
contains

    subroutine banner(msg)
        character(len=*), intent(in) :: msg
        write(*,'(a)') '============================================================'
        write(*,'(a)') trim(msg)
        write(*,'(a)') '============================================================'
    end subroutine banner


    subroutine assert_i(label, got, expect)
        character(len=*), intent(in) :: label
        integer, intent(in)          :: got, expect
        if (got /= expect) then
            write(*,*) '[TEST FAILED] ', trim(label)
            write(*,*) '  got   = ', got
            write(*,*) '  expect= ', expect
            stop 1
        end if
    end subroutine assert_i


    subroutine assert_l(label, got, expect)
        character(len=*), intent(in) :: label
        logical, intent(in)          :: got, expect
        if (got .neqv. expect) then
            write(*,*) '[TEST FAILED] ', trim(label)
            write(*,*) '  got   = ', got
            write(*,*) '  expect= ', expect
            stop 1
        end if
    end subroutine assert_l

end program test_key_table