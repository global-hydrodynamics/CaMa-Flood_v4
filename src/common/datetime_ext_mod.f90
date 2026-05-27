module datetime_ext_mod
use datetime_module
implicit none

! For yearly/monthly interval (e.g. for I/O)
type, extends(TimeDelta) :: RelativeDelta
    private
    integer years, months
    contains
    procedure :: getYears      => getYears
    procedure :: getMonths     => getMonths
    procedure :: total_seconds => total_seconds
    !procedure :: operator(+)   => operator(+)
!    procedure, pass(dt) :: datetime_plus_relativedelta => datetime_plus_relativedelta
!    procedure :: relativedelta_plus_datetime => relativedelta_plus_datetime
!    generic   :: operator(+) => datetime_plus_relativedelta, relativedelta_plus_datetime
end type RelativeDelta

interface init_RelativeDelta
    module procedure init_relativedelta1
    module procedure init_relativedelta_with_unit
end interface init_RelativeDelta

interface init_DateTime
    module procedure init_DateTime_from_array
end interface init_DateTime

interface init_TimeDelta
    module procedure init_timedelta_with_unit
end interface init_TimeDelta

interface operator(+)
    module procedure datetime_plus_relativedelta
    module procedure relativedelta_plus_datetime
end interface operator(+)

interface shorter
    module procedure shorter_tt
    module procedure shorter_rr
    module procedure shorter_tr
    module procedure shorter_rt
end interface shorter
contains

! ===================================================================================================
! Constructor
! ===================================================================================================
function init_DateTime_from_array(array) result(obj)
    type(DateTime)      :: obj
    integer, intent(in) :: array(:)
    integer :: i, year = 1, month = 1, day = 1, hour = 0, minute = 0, second = 0, millisecond = 0
    do i = 1, size(array)
        select case (i)
            case (1)
                year = array(i)
            case (2)
                month = array(i)
            case (3)
                day = array(i)
            case (4)
                hour = array(i)
            case (5)
                minute = array(i)
            case (6)
                second = array(i)
            case (7)
                millisecond = array(i)
        end select
    enddo
    obj = DateTime(year, month, day, hour, minute, second, millisecond)
end function init_DateTime_from_array

! ---------------------------------------------------------------------------------------------------
function init_relativedelta1(years, months, days, hours, minutes, seconds, milliseconds) result(obj)
    type(RelativeDelta) obj
    integer, intent(in), optional :: years, months, days, hours, minutes, seconds, milliseconds
    integer :: years_, months_, days_, hours_, minutes_, seconds_, milliseconds_
    years_        = 0
    months_       = 0
    days_         = 0
    hours_        = 0
    minutes_      = 0
    seconds_      = 0
    milliseconds_ = 0
    if (present(years)       ) years_        = years
    if (present(months)      ) months_       = months
    if (present(days)        ) days_         = days
    if (present(hours)       ) hours_        = hours
    if (present(minutes)     ) minutes_      = minutes
    if (present(seconds)     ) seconds_      = seconds
    if (present(milliseconds)) milliseconds_ = milliseconds
    obj%TimeDelta = TimeDelta(days_, hours_, minutes_, seconds_, milliseconds_)
    if (years_ > 0 .or. months_ > 0) then
        if (obj%TimeDelta%total_seconds() > 0.d0) then
            write(*, *) 'init_relativedelta ERROR, ', years_, months_, obj%TimeDelta%total_seconds()
            stop
        endif
    endif
    obj%years  = years_
    obj%months = months_
end function init_relativedelta1

! ---------------------------------------------------------------------------------------------------
function init_timedelta_with_unit(dt, dt_unit) result(obj)
    type(TimeDelta) obj
    integer         , intent(in) :: dt
    character(len=*), intent(in) :: dt_unit
    select case (trim(dt_unit))
        case ('day', 'dy', 'd', 'DAY', 'DY', 'D')
            obj = TimeDelta(dt, 0, 0, 0, 0)
        case ('hour', 'h', 'HOUR', 'H')
!            if (dt == 24) then
!                obj = TimeDelta(1, 0, 0, 0, 0)
!            else
                obj = TimeDelta(0, dt, 0, 0, 0)
!            endif
        case ('minute', 'min', 'MINUTE', 'MIN')
            obj = TimeDelta(0, 0, dt, 0, 0)
        case ('second', 'sec', 's', 'SECOND', 'SEC', 'S')
            obj = TimeDelta(0, 0, 0, dt, 0)
    end select
end function init_timedelta_with_unit

function init_relativedelta_with_unit(dt, dt_unit) result(obj)
!    type(TimeDelta) obj
    type(RelativeDelta) :: obj
    integer         , intent(in) :: dt
    character(len=*), intent(in) :: dt_unit
    select case (trim(dt_unit))
        case ('year', 'yr', 'y', 'YEAR', 'YR', 'Y')
            obj = init_RelativeDelta(years=dt)
        case ('month', 'mon', 'MONTH', 'MON')
            obj = init_RelativeDelta(months=dt)
        case ('day', 'dy', 'd', 'DAY', 'DY', 'D')
            obj = init_RelativeDelta(days=dt)
        case ('hour', 'h', 'HOUR', 'H')
            obj = init_RelativeDelta(hours=dt)
        case ('minute', 'min', 'MINUTE', 'MIN')
            obj = init_RelativeDelta(minutes=dt)
        case ('second', 'sec', 's', 'SECOND', 'SEC', 'S')
            obj = init_RelativeDelta(seconds=dt)
    end select
end function init_relativedelta_with_unit

! ===================================================================================================
! Getter/ Setter
! ===================================================================================================
pure elemental function getYears(self) result(years)
    class(RelativeDelta), intent(in) :: self
    integer                          :: years
    years = self%years
end function getYears

pure elemental function getMonths(self) result(months)
    class(RelativeDelta), intent(in) :: self
    integer                          :: months
    months = self%months
end function getMonths

! ===================================================================================================
! Operator (+)
! ===================================================================================================
pure elemental function datetime_plus_relativedelta(t1, dt) result(t2)
!    type(DateTime),      intent(in) :: t1
!    type(RelativeDelta), intent(in) :: dt
    class(DateTime),      intent(in) :: t1
    class(RelativeDelta), intent(in) :: dt
    type(DateTime)                  :: t2
    integer :: iyear, imon, years, mons
!    write(*, *) 'hello'
    t2 = t1
    years = dt%getYears()
    do iyear = 1, years!dt%getYears()
        t2 = t2 + TimeDelta(days=daysInYear(t2%getYear()))
    enddo
!    write(*, *) t2%strftime('%Y/%m/%d %H:%M')

    mons = dt%getMonths()
    do imon = 1, mons!dt%getMonths()
        t2 = t2 + TimeDelta(days=daysInMonth(t2%getMonth(), t2%getYear()))
    enddo

    t2 = t2 + dt%TimeDelta
end function datetime_plus_relativedelta

pure elemental function relativedelta_plus_datetime(dt, t1) result(t2)
!    type(RelativeDelta), intent(in) :: dt
!    type(DateTime),      intent(in) :: t1
    class(RelativeDelta), intent(in) :: dt
    class(DateTime),      intent(in) :: t1
    type(DateTime)                  :: t2
    t2 = t1 + dt
end function relativedelta_plus_datetime

! ===================================================================================================
! Method (override)
! ===================================================================================================
pure elemental function total_seconds(self) result(sec)
    class(RelativeDelta), intent(in) :: self
    real(8) :: sec
    sec = self%TimeDelta%total_seconds()
    sec = sec + self%months * 86400.d0 * 30.d0
    sec = sec + self%years  * 86400.d0 * 365.d0
end function total_seconds

! ===================================================================================================
function td2rd(td) result(rd)
    type(TimeDelta), intent(in) :: td
    type(RelativeDelta)         :: rd
    integer days, hours, minutes, seconds, milliseconds
    days    = td%getDays()
    hours   = td%getHours()
    minutes = td%getMinutes()
    seconds = td%getSeconds()
    milliseconds = td%getMilliseconds()
    rd = RelativeDelta(0, 0, days, hours, minutes, seconds, milliseconds)
end function td2rd

! ===================================================================================================
! shorter
! ===================================================================================================
function shorter_tt(a, b) result(obj)
    type(TimeDelta), intent(in) :: a, b
    type(TimeDelta)             :: obj
    if (a <= b) then
        obj = a
    else
        obj = b
    endif
end function shorter_tt

function shorter_rr(a, b) result(obj)
    type(RelativeDelta), intent(in) :: a, b
    type(RelativeDelta)             :: obj
    if (a%total_seconds() <= b%total_seconds()) then
        obj = a
    else
        obj = b
    endif
end function shorter_rr

function shorter_tr(a, b) result(obj)
    type(TimeDelta), intent(in) :: a
    type(RelativeDelta), intent(in) :: b
    type(RelativeDelta)             :: obj
    if (a%total_seconds() <= b%total_seconds()) then
        obj = td2rd(a)
    else
        obj = b
    endif
end function shorter_tr

function shorter_rt(a, b) result(obj)
    type(RelativeDelta), intent(in) :: a
    type(TimeDelta),     intent(in) :: b
    type(RelativeDelta)             :: obj
    if (a%total_seconds() <= b%total_seconds()) then
        obj = a
    else
        obj = td2rd(b)
    endif
end function shorter_rt

! ===================================================================================================
function calc_timestep_number(t1, t2, dt) result(nstep)
    type(DateTime),  intent(in) :: t1, t2
    type(RelativeDelta), intent(in) :: dt
    integer                     :: nstep
    type(TimeDelta) dt_12
    if (t1 <= t2) then
        dt_12 = t2 - t1
    else
        dt_12 = t1 - t2
    endif
    nstep = int(real(dt_12%total_seconds() / dt%total_seconds()) - 0.1) + 1
end function calc_timestep_number

end module datetime_ext_mod
