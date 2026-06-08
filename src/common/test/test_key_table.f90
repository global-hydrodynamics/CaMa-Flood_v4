program test_key_table
    use key_table_class, only: KeyTable
    implicit none

    type(KeyTable) :: kt

    call test_append_and_has_key(kt)

    write(*, *) '[TEST PASSED] test_key_table'
contains

    subroutine test_append_and_has_key(kt)
        type(KeyTable), intent(inout) :: kt

        call kt%clear()

        call assert_int_equal(0, count_keys(kt), 'initial count is 0')

        call kt%append('Tair')
        call assert_int_equal(1, count_keys(kt), 'count after 1 append')
        call assert_true(kt%has_key('Tair'), 'has_key(Tair) after append')
        call assert_false(kt%has_key('Qair'), 'has_key(Qair) before append')

        call kt%append('Qair')
        call assert_int_equal(2, count_keys(kt), 'count after 2 appends')
        call assert_true(kt%has_key('Qair'), 'has_key(Qair) after append')

        call kt%append('Wind')
        call assert_int_equal(3, count_keys(kt), 'count after 3 appends')
        call assert_true(kt%has_key('Wind'), 'has_key(Wind) after append')
    end subroutine test_append_and_has_key


    ! keys(:) のサイズ取得（「n/nmax は不要で size(keys)」方針に対応）
    integer function count_keys(kt) result(n)
        type(KeyTable), intent(in) :: kt
        if (.not. allocated(kt%keys)) then
            n = 0
        else
            n = size(kt%keys)
        end if
    end function count_keys


    subroutine assert_true(cond, msg)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: msg
        if (.not. cond) then
            write(*, *) '[TEST FAILED] ', trim(msg)
            stop 1
        end if
    end subroutine assert_true

    subroutine assert_false(cond, msg)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: msg
        if (cond) then
            write(*, *) '[TEST FAILED] ', trim(msg)
            stop 1
        end if
    end subroutine assert_false

    subroutine assert_int_equal(a, b, msg)
        integer, intent(in) :: a, b
        character(len=*), intent(in) :: msg
        if (a /= b) then
            write(*, *) '[TEST FAILED] ', trim(msg), ' a=', a, ' b=', b
            stop 1
        end if
    end subroutine assert_int_equal

end program test_key_table