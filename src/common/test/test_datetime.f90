program test_datetime
    use PARKIND1, only: &
    &   JPIM
    use datetime_mod, only: &
    &   date_hour2datetime, seconds_since_year_start
    implicit none

    call assert_equal(seconds_since_year_start(date_hour2datetime(20000101_JPIM, 0_JPIM)), &
    &   0_JPIM, 'start of leap year [s]')
    call assert_equal(seconds_since_year_start(date_hour2datetime(20000102_JPIM, 0_JPIM)), &
    &   86400_JPIM, 'second day of leap year [s]')
    call assert_equal(seconds_since_year_start(date_hour2datetime(20000229_JPIM, 3_JPIM)), &
    &   (59_JPIM * 24_JPIM + 3_JPIM) * 3600_JPIM, 'leap day [s]')
    call assert_equal(seconds_since_year_start(date_hour2datetime(20010301_JPIM, 0_JPIM)), &
    &   59_JPIM * 86400_JPIM, 'March in common year [s]')
    call assert_equal(seconds_since_year_start(date_hour2datetime(20000301_JPIM, 0_JPIM)), &
    &   60_JPIM * 86400_JPIM, 'March in leap year [s]')

    write(*, '(a)') '[ALL TESTS PASSED] test_datetime'

contains

subroutine assert_equal(actual_value, expected_value, label)
    integer(kind=JPIM), intent(in) :: &
    &   actual_value, &        ! [s] Computed seconds since the start of the year.
    &   expected_value         ! [s] Expected seconds since the start of the year.
    character(len=*), intent(in) :: &
    &   label                  ! [-] Human-readable assertion label.

    if (actual_value == expected_value) return
    write(*, '(a)') '[TEST FAILED] '//trim(label)
    write(*, '(a,i0)') '  actual   = ', actual_value
    write(*, '(a,i0)') '  expected = ', expected_value
    error stop 1
end subroutine assert_equal

end program test_datetime
