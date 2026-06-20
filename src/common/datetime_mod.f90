module datetime_mod
    use PARKIND1, only: JPIM
    implicit none

    private

    public :: DateTime
    public :: date_hour2datetime
    public :: datetime2string

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

end module datetime_mod