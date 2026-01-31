program test_numeric_utils
    use PARKIND1, only: JPIM, JPRB, JPRM
    use numeric_utils_mod, only: nearly_equal
    implicit none

    integer(kind=JPIM) :: nfail

    nfail = 0

    call test_nearly_equal_r4(nfail)  ! JPRM を real(4) として使っている想定
    call test_nearly_equal_r8(nfail)  ! JPRB を real(8) として使っている想定

    if (nfail == 0) then
        write(*, '(a)') '[OK] numeric_utils_mod tests passed.'
        stop 0
    else
        write(*, '(a,i0)') '[NG] numeric_utils_mod tests failed: nfail=', nfail
        stop 1
    endif

contains

    !------------------------------------------------------------
    subroutine assert_true(cond, msg, nfail)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: msg
        integer(kind=JPIM), intent(inout) :: nfail
        if (.not. cond) then
            nfail = nfail + 1
            write(*, '(a)') '[FAIL] ' // trim(msg)
        endif
    end subroutine assert_true

    subroutine assert_false(cond, msg, nfail)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: msg
        integer(kind=JPIM), intent(inout) :: nfail
        call assert_true(.not. cond, msg, nfail)
    end subroutine assert_false

    !------------------------------------------------------------
    subroutine test_nearly_equal_r4(nfail)
        integer(kind=JPIM), intent(inout) :: nfail
        real(kind=JPRM) :: a, b, d

        write(*, '(a)') '[TEST] nearly_equal (JPRM)'

        ! 1) 完全一致
        a = 1.0_JPRM
        b = 1.0_JPRM
        call assert_true(nearly_equal(a, b), 'JPRM: exact equality should be true', nfail)

        ! 2) 極小差（機械イプシロン程度）
        d = epsilon(1.0_JPRM)
        a = 1.0_JPRM
        b = 1.0_JPRM + d
        call assert_true(nearly_equal(a, b), 'JPRM: within ~epsilon should be true', nfail)

        ! 3) 明確な差
        a = 1.0_JPRM
        b = 1.0_JPRM + 1000.0_JPRM * epsilon(1.0_JPRM)
        call assert_false(nearly_equal(a, b), 'JPRM: beyond tolerance should be false', nfail)

        ! 4) ゼロ近傍（相対誤差だけ実装だと壊れやすい箇所）
        a = 0.0_JPRM
        b = 0.0_JPRM + epsilon(1.0_JPRM)
        call assert_true(nearly_equal(a, b), 'JPRM: near zero small diff should be true (expects abs-tol handling)', nfail)

        ! 5) 大きい値（相対誤差が効くか）
        a = 1.0e10_JPRM
        b = a + a * 5.0_JPRM * epsilon(1.0_JPRM)
        call assert_true(nearly_equal(a, b), 'JPRM: large magnitude small relative diff should be true', nfail)

        b = a + a * 1.0e-3_JPRM
        call assert_false(nearly_equal(a, b), 'JPRM: large magnitude 1e-3 relative diff should be false', nfail)

    end subroutine test_nearly_equal_r4

    !------------------------------------------------------------
    subroutine test_nearly_equal_r8(nfail)
        integer(kind=JPIM), intent(inout) :: nfail
        real(kind=JPRB) :: a, b, d

        write(*, '(a)') '[TEST] nearly_equal (JPRB)'

        ! 1) 完全一致
        a = 1.0_JPRB
        b = 1.0_JPRB
        call assert_true(nearly_equal(a, b), 'JPRB: exact equality should be true', nfail)

        ! 2) 極小差（機械イプシロン程度）
        d = epsilon(1.0_JPRB)
        a = 1.0_JPRB
        b = 1.0_JPRB + d
        call assert_true(nearly_equal(a, b), 'JPRB: within ~epsilon should be true', nfail)

        ! 3) 明確な差
        a = 1.0_JPRB
        b = 1.0_JPRB + 100000.0_JPRB * epsilon(1.0_JPRB)
        call assert_false(nearly_equal(a, b), 'JPRB: beyond tolerance should be false', nfail)

        ! 4) ゼロ近傍
        a = 0.0_JPRB
        b = 0.0_JPRB + epsilon(1.0_JPRB)
        call assert_true(nearly_equal(a, b), 'JPRB: near zero small diff should be true (expects abs-tol handling)', nfail)

        ! 5) 大きい値
        a = 1.0e200_JPRB
        b = a + a * 10.0_JPRB * epsilon(1.0_JPRB)
        call assert_true(nearly_equal(a, b), 'JPRB: huge magnitude small relative diff should be true', nfail)

        b = a + a * 1.0e-6_JPRB
        call assert_false(nearly_equal(a, b), 'JPRB: huge magnitude 1e-6 relative diff should be false', nfail)

    end subroutine test_nearly_equal_r8

end program test_numeric_utils