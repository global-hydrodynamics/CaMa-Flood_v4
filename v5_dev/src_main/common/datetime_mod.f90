module datetime_mod
    use PARKIND1, only: JPIM
    implicit none

    private

    public :: DateTime
    public :: date_hour2datetime
    public :: datetime2string
    public :: seconds_since_year_start

    type :: DateTime
        integer(kind=JPIM) :: yyyymmdd = 0_JPIM
        integer(kind=JPIM) :: hour     = 0_JPIM
    end type DateTime

contains

    function date_hour2datetime(yyyymmdd, hour) result(dt)

        integer(kind=JPIM), intent(in) :: yyyymmdd
        integer(kind=JPIM), intent(in) :: hour
        type(DateTime) :: dt

        dt%yyyymmdd = yyyymmdd
        dt%hour     = hour

    end function date_hour2datetime

    function datetime2string(dt) result(cdate)

        type(DateTime), intent(in) :: dt
        character(len=10) :: cdate

        write(cdate, '(i8.8,i2.2)') dt%yyyymmdd, dt%hour

    end function datetime2string

    pure integer(kind=JPIM) function seconds_since_year_start(dt) result(seconds)
        type(DateTime), intent(in) :: &
        &   dt                      ! [-] Calendar date and hour within a Gregorian year.
        integer(kind=JPIM), parameter :: &
        &   days_before_month(12) = [ &
        &   0_JPIM, 31_JPIM, 59_JPIM, 90_JPIM, 120_JPIM, 151_JPIM, &
        &   181_JPIM, 212_JPIM, 243_JPIM, 273_JPIM, 304_JPIM, 334_JPIM]
        integer(kind=JPIM) :: &
        &   year, &                  ! [year] Four-digit Gregorian year.
        &   month, &                 ! [month] Calendar month from 1 to 12.
        &   day, &                   ! [day] Calendar day within the month.
        &   elapsed_days             ! [day] Complete days elapsed since January 1.
        logical :: &
        &   is_leap_year             ! [-] True for a Gregorian leap year.

        year = dt%yyyymmdd / 10000_JPIM
        month = mod(dt%yyyymmdd / 100_JPIM, 100_JPIM)
        day = mod(dt%yyyymmdd, 100_JPIM)
        is_leap_year = mod(year, 4_JPIM) == 0_JPIM .and. &
        &   (mod(year, 100_JPIM) /= 0_JPIM .or. mod(year, 400_JPIM) == 0_JPIM)

        elapsed_days = days_before_month(month) + day - 1_JPIM
        if (is_leap_year .and. month > 2_JPIM) elapsed_days = elapsed_days + 1_JPIM
        seconds = (elapsed_days * 24_JPIM + dt%hour) * 3600_JPIM
    end function seconds_since_year_start

end module datetime_mod
