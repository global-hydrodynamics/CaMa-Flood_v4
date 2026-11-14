module cmf_cf_time_mod
!==========================================================
    !* PURPOSE: Read and validate CF-compliant NetCDF time axes.
    !*
    !* The top-level Makefile compiles this source directly so the legacy
    !* reader can share it with src/common + src/io without making the
    !* optional libcommon archive mandatory for a standard build.
!==========================================================
    use parkind1, only: JPIM, JPIB, JPRD
#ifdef UseCDF_CMF
    use netcdf
#endif
    implicit none
    private

    integer, parameter :: CF_STRLEN = 256
    integer, parameter :: CAL_GREGORIAN = 1
    integer, parameter :: CAL_NOLEAP = 2

    type, public :: cf_time_axis
        integer(kind=JPIM) :: ntime = 0
        integer(kind=JPIB) :: dt_minutes = 0_JPIB
        logical :: has_bounds = .false.
        character(len=CF_STRLEN) :: units = ''
        character(len=32) :: calendar = ''
        integer(kind=JPIB), allocatable :: center_minutes(:)
        integer(kind=JPIB), allocatable :: lower_minutes(:)
        integer(kind=JPIB), allocatable :: upper_minutes(:)
    end type cf_time_axis

    public :: cf_datetime_to_minutes
    public :: cf_find_time_record
    public :: cf_calendar_matches_lleapyr
    public :: cf_calendar_uses_leap_day
#ifdef UseCDF_CMF
    public :: cf_read_time_axis
    public :: cf_resolve_time_record
#endif

contains

!####################################################################
pure function lowercase(text) result(lower)
    character(len=*), intent(in) :: text
    character(len=len(text)) :: lower
    integer :: i, code

    lower=text
    do i=1,len(text)
        code=iachar(text(i:i))
        if ( code>=iachar('A') .and. code<=iachar('Z') ) lower(i:i)=achar(code+32)
    enddo
end function lowercase
!####################################################################

!####################################################################
pure function sanitize_string(text) result(clean)
    character(len=*), intent(in) :: text
    character(len=len(text)) :: clean
    integer :: i

    clean=text
    do i=1,len(text)
        if ( iachar(clean(i:i))==0 ) clean(i:i)=' '
    enddo
end function sanitize_string
!####################################################################

!####################################################################
subroutine set_error(ierr,message,text)
    integer(kind=JPIM), intent(out) :: ierr
    character(len=*), intent(out) :: message
    character(len=*), intent(in) :: text

    ierr=1
    message=trim(text)
end subroutine set_error
!####################################################################

!####################################################################
subroutine normalize_calendar(calendar,ical,normalized,ierr,message)
    character(len=*), intent(in) :: calendar
    integer, intent(out) :: ical
    character(len=*), intent(out) :: normalized
    integer(kind=JPIM), intent(out) :: ierr
    character(len=*), intent(out) :: message
    character(len=32) :: cal

    ierr=0
    message=''
    ical=0
    normalized=''
    cal=trim(lowercase(adjustl(sanitize_string(calendar))))
    if ( len_trim(cal)==0 ) cal='standard'

    select case (trim(cal))
    case ('gregorian','standard','proleptic_gregorian')
        ical=CAL_GREGORIAN
        normalized=trim(cal)
    case ('365_day','noleap')
        ical=CAL_NOLEAP
        normalized=trim(cal)
    case default
        call set_error(ierr,message,'unsupported CF calendar: '//trim(cal))
    end select
end subroutine normalize_calendar
!####################################################################

!####################################################################
pure logical function is_gregorian_leap_year(year)
    integer(kind=JPIM), intent(in) :: year

    is_gregorian_leap_year = mod(year, 400_JPIM) == 0_JPIM .or. &
    &   (mod(year, 4_JPIM) == 0_JPIM .and. mod(year, 100_JPIM) /= 0_JPIM)
end function is_gregorian_leap_year
!####################################################################

!####################################################################
subroutine date_parts_to_minutes(year,month,day,hour,minute,ical,value,ierr,message)
    integer(kind=JPIM), intent(in) :: year,month,day,hour,minute
    integer, intent(in) :: ical
    integer(kind=JPIB), intent(out) :: value
    integer(kind=JPIM), intent(out) :: ierr
    character(len=*), intent(out) :: message
    integer(kind=JPIM) :: month_days(12), imon
    integer(kind=JPIB) :: days

    ierr=0
    message=''
    value=0_JPIB
    if ( year<1 .or. month<1 .or. month>12 .or. hour<0 .or. hour>23 .or. minute<0 .or. minute>59 ) then
        call set_error(ierr,message,'invalid datetime component in CF time metadata')
        return
    endif

    month_days=(/31,28,31,30,31,30,31,31,30,31,30,31/)
    if ( ical==CAL_GREGORIAN .and. is_gregorian_leap_year(year) ) month_days(2)=29
    if ( day<1 .or. day>month_days(month) ) then
        call set_error(ierr,message,'invalid day for CF calendar')
        return
    endif

    if ( ical==CAL_NOLEAP ) then
        days=int(year-1,kind=JPIB)*365_JPIB
    else
        days = int(year - 1, kind=JPIB) * 365_JPIB + int((year - 1) / 4, kind=JPIB) &
        &   - int((year - 1) / 100, kind=JPIB) + int((year - 1) / 400, kind=JPIB)
    endif
    do imon=1,month-1
        days=days+int(month_days(imon),kind=JPIB)
    enddo
    days=days+int(day-1,kind=JPIB)
    value=(days*24_JPIB+int(hour,kind=JPIB))*60_JPIB+int(minute,kind=JPIB)
end subroutine date_parts_to_minutes
!####################################################################

!####################################################################
subroutine cf_datetime_to_minutes(calendar,yyyymmdd,hour,minute,value,ierr,message)
    character(len=*), intent(in) :: calendar
    integer(kind=JPIM), intent(in) :: yyyymmdd,hour,minute
    integer(kind=JPIB), intent(out) :: value
    integer(kind=JPIM), intent(out) :: ierr
    character(len=*), intent(out) :: message
    integer :: ical
    integer(kind=JPIM) :: year,month,day
    character(len=32) :: normalized

    ical=0
    call normalize_calendar(calendar,ical,normalized,ierr,message)
    if ( ierr/=0 ) return
    year=yyyymmdd/10000_JPIM
    month=mod(yyyymmdd/100_JPIM,100_JPIM)
    day=mod(yyyymmdd,100_JPIM)
    call date_parts_to_minutes(year,month,day,hour,minute,ical,value,ierr,message)
end subroutine cf_datetime_to_minutes
!####################################################################

#ifdef UseCDF_CMF
!####################################################################
subroutine parse_reference_datetime(text,ical,reference_minutes,ierr,message)
    character(len=*), intent(in) :: text
    integer, intent(in) :: ical
    integer(kind=JPIB), intent(out) :: reference_minutes
    integer(kind=JPIM), intent(out) :: ierr
    character(len=*), intent(out) :: message
    character(len=CF_STRLEN) :: ref,timezone,tzcompact
    integer(kind=JPIM) :: year,month,day,hour,minute,second,tzhour,tzminute,tzoffset
    integer :: ios, n, i, j, tzsign, tzstart

    ierr=0
    message=''
    reference_minutes=0_JPIB
    ref=adjustl(text)
    n=len_trim(ref)
    timezone=''
    tzstart=0
    do i=11,n
        if ( ref(i:i)=='+' .or. ref(i:i)=='-' ) then
            tzstart=i
            exit
        endif
    enddo
    if ( tzstart==0 .and. n>=1 ) then
        if ( ref(n:n)=='Z' .or. ref(n:n)=='z' ) tzstart=n
    endif
    if ( tzstart==0 .and. n>=3 ) then
        if ( lowercase(ref(n-2:n))=='utc' ) tzstart=n-2
    endif
    if ( tzstart>0 ) then
        timezone=trim(adjustl(ref(tzstart:n)))
        ref(tzstart:n)=' '
        n=len_trim(ref)
    endif
    if ( n<10 ) then
        call set_error(ierr,message,'invalid reference datetime in CF time units: '//trim(ref))
        return
    endif
    if ( ref(5:5)/='-' .or. ref(8:8)/='-' ) then
        call set_error(ierr,message,'CF reference date must use YYYY-MM-DD: '//trim(ref))
        return
    endif

    read(ref(1:4),'(I4)',iostat=ios) year
    if ( ios/=0 ) goto 900
    read(ref(6:7),'(I2)',iostat=ios) month
    if ( ios/=0 ) goto 900
    read(ref(9:10),'(I2)',iostat=ios) day
    if ( ios/=0 ) goto 900
    hour=0
    minute=0
    second=0
    if ( n>=13 ) then
        if ( ref(11:11)=='T' .or. ref(11:11)=='t' ) ref(11:11)=' '
        if ( ref(11:11)/=' ' ) goto 900
        read(ref(12:13),'(I2)',iostat=ios) hour
        if ( ios/=0 ) goto 900
    endif
    if ( n>=16 ) then
        if ( ref(14:14)/=':' ) goto 900
        read(ref(15:16),'(I2)',iostat=ios) minute
        if ( ios/=0 ) goto 900
    endif
    if ( n>=19 ) then
        if ( ref(17:17)/=':' ) goto 900
        read(ref(18:19),'(I2)',iostat=ios) second
        if ( ios/=0 ) goto 900
    endif
    if ( n/=10 .and. n/=13 .and. n/=16 .and. n/=19 ) goto 900
    if ( second/=0 ) then
        call set_error(ierr,message,'CF reference time must align to a whole minute: '//trim(ref))
        return
    endif
    call date_parts_to_minutes(year,month,day,hour,minute,ical,reference_minutes,ierr,message)
    if ( ierr/=0 ) return

    ! CF reference datetimes may include a time-zone suffix. Convert the
    ! local reference time to UTC: UTC = local time - UTC offset.
    tzoffset=0_JPIM
    if ( len_trim(timezone)>0 ) then
        if ( trim(lowercase(timezone))/='z' .and. trim(lowercase(timezone))/='utc' ) then
            if ( timezone(1:1)=='+' ) then
                tzsign=1
            elseif ( timezone(1:1)=='-' ) then
                tzsign=-1
            else
                call set_error(ierr,message,'unsupported CF reference-time timezone: '//trim(timezone))
                return
            endif
            tzcompact=''
            j=0
            do i=2,len_trim(timezone)
                if ( timezone(i:i)==':' ) cycle
                j=j+1
                tzcompact(j:j)=timezone(i:i)
            enddo
            if ( j/=2 .and. j/=4 ) then
                call set_error(ierr,message,'CF timezone must be Z, UTC, +/-HH, +/-HHMM, or +/-HH:MM')
                return
            endif
            read(tzcompact(1:2),'(I2)',iostat=ios) tzhour
            if ( ios/=0 ) goto 910
            tzminute=0_JPIM
            if ( j==4 ) then
                read(tzcompact(3:4),'(I2)',iostat=ios) tzminute
                if ( ios/=0 ) goto 910
            endif
            if ( tzhour>23 .or. tzminute>59 ) goto 910
            tzoffset=tzsign*(tzhour*60_JPIM+tzminute)
        endif
    endif
    reference_minutes=reference_minutes-int(tzoffset,kind=JPIB)
    return

    900 continue
    call set_error(ierr,message,'cannot parse reference datetime in CF time units: '//trim(ref))
    return
    910 continue
    call set_error(ierr,message,'invalid CF reference-time timezone: '//trim(timezone))
end subroutine parse_reference_datetime
!####################################################################

!####################################################################
subroutine parse_cf_units(units,calendar,unit_minutes,reference_minutes,normalized_calendar,ierr,message)
    character(len=*), intent(in) :: units,calendar
    real(kind=JPRD), intent(out) :: unit_minutes
    integer(kind=JPIB), intent(out) :: reference_minutes
    character(len=*), intent(out) :: normalized_calendar
    integer(kind=JPIM), intent(out) :: ierr
    character(len=*), intent(out) :: message
    character(len=CF_STRLEN) :: lower_units, unit_name, reference_text
    integer :: isince,ical

    ierr=0
    message=''
    lower_units=trim(lowercase(adjustl(sanitize_string(units))))
    isince=index(lower_units,' since ')
    if ( isince<=1 ) then
        call set_error(ierr,message,'CF time units must contain " since ": '//trim(units))
        return
    endif
    unit_name=trim(adjustl(lower_units(1:isince-1)))
    reference_text=trim(adjustl(sanitize_string(units(isince+7:))))

    select case (trim(unit_name))
    case ('second','seconds','sec','secs')
        unit_minutes=1._JPRD/60._JPRD
    case ('minute','minutes','min','mins')
        unit_minutes=1._JPRD
    case ('hour','hours','hr','hrs')
        unit_minutes=60._JPRD
    case ('day','days')
        unit_minutes=1440._JPRD
    case default
        call set_error(ierr,message,'unsupported CF time unit: '//trim(unit_name))
        return
    end select

    call normalize_calendar(calendar,ical,normalized_calendar,ierr,message)
    if ( ierr/=0 ) return
    call parse_reference_datetime(reference_text,ical,reference_minutes,ierr,message)
end subroutine parse_cf_units
!####################################################################

!####################################################################
subroutine values_to_minutes(values,unit_minutes,reference_minutes,minutes,ierr,message)
    real(kind=JPRD), intent(in) :: values(:),unit_minutes
    integer(kind=JPIB), intent(in) :: reference_minutes
    integer(kind=JPIB), intent(out) :: minutes(:)
    integer(kind=JPIM), intent(out) :: ierr
    character(len=*), intent(out) :: message
    real(kind=JPRD) :: offset,rounded
    integer :: i

    ierr=0
    message=''
    do i=1,size(values)
        offset=values(i)*unit_minutes
        rounded=anint(offset)
        if ( abs(offset-rounded)>1.e-7_JPRD ) then
            call set_error(ierr,message,'CF time value does not align to CaMa-Flood whole-minute time')
            return
        endif
        minutes(i)=reference_minutes+int(nint(offset,kind=JPIB),kind=JPIB)
    enddo
end subroutine values_to_minutes
!####################################################################

!####################################################################
subroutine validate_time_axis(axis,ierr,message)
    type(cf_time_axis), intent(inout) :: axis
    integer(kind=JPIM), intent(out) :: ierr
    character(len=*), intent(out) :: message
    integer :: i
    integer(kind=JPIB) :: delta

    ierr=0
    message=''
    if ( axis%ntime<1 ) then
        call set_error(ierr,message,'CF time axis must contain at least one record')
        return
    endif
    if ( axis%ntime==1 ) then
        axis%dt_minutes=0_JPIB
        if ( axis%has_bounds ) then
            if ( axis%lower_minutes(1)>=axis%upper_minutes(1) ) then
                call set_error(ierr,message,'CF time bounds must be strictly increasing within each record')
                return
            endif
            if (axis%center_minutes(1) < axis%lower_minutes(1) .or. &
            &   axis%center_minutes(1) > axis%upper_minutes(1)) then
                call set_error(ierr, message, 'CF time coordinate lies outside its bounds')
                return
            endif
            axis%dt_minutes = axis%upper_minutes(1) - axis%lower_minutes(1)
        endif
        return
    endif

    axis%dt_minutes = axis%center_minutes(2) - axis%center_minutes(1)
    if (axis%dt_minutes <= 0_JPIB) then
        call set_error(ierr, message, 'CF time axis must be strictly increasing')
        return
    endif
    do i = 2, axis%ntime
        delta = axis%center_minutes(i) - axis%center_minutes(i - 1)
        if (delta /= axis%dt_minutes) then
            call set_error(ierr, message, 'CF time axis has a gap, overlap, or variable interval')
            return
        endif
    enddo
    if (axis%has_bounds) then
        do i = 1, axis%ntime
            if (axis%lower_minutes(i) >= axis%upper_minutes(i)) then
                call set_error(ierr, message, 'CF time bounds must be strictly increasing within each record')
                return
            endif
            if (axis%center_minutes(i) < axis%lower_minutes(i) .or. &
            &   axis%center_minutes(i) > axis%upper_minutes(i)) then
                call set_error(ierr, message, 'CF time coordinate lies outside its bounds')
                return
            endif
            if (axis%upper_minutes(i) - axis%lower_minutes(i) /= axis%dt_minutes) then
                call set_error(ierr, message, 'CF time-bounds width differs from the time-coordinate interval')
                return
            endif
            if (i > 1) then
                if (axis%lower_minutes(i) /= axis%upper_minutes(i - 1)) then
                    call set_error(ierr, message, 'CF time bounds have a gap or overlap')
                    return
                endif
            endif
        enddo
    endif
end subroutine validate_time_axis
!####################################################################
#endif

#ifdef UseCDF_CMF
!####################################################################
subroutine cf_read_time_axis(ncid,time_name,axis,ierr,message)
    integer, intent(in) :: ncid
    character(len=*), intent(in) :: time_name
    type(cf_time_axis), intent(out) :: axis
    integer(kind=JPIM), intent(out) :: ierr
    character(len=*), intent(out) :: message
    integer :: status,time_varid,ndims
    integer :: dimids(nf90_max_var_dims),bound_dimids(nf90_max_var_dims)
    integer :: bound_varid,bound_ndims,len1,len2,i
    real(kind=JPRD), allocatable :: raw_time(:),raw_bound(:,:),lower_raw(:),upper_raw(:)
    real(kind=JPRD) :: unit_minutes
    integer(kind=JPIB) :: reference_minutes
    character(len=CF_STRLEN) :: calendar,bound_name

    ierr=0
    message=''
    calendar='standard'
    bound_name=''

    status=nf90_inq_varid(ncid,trim(time_name),time_varid)
    if ( status/=nf90_noerr ) then
        call set_error(ierr,message,'cannot find NetCDF time coordinate variable: '//trim(time_name))
        return
    endif
    status=nf90_inquire_variable(ncid,time_varid,ndims=ndims,dimids=dimids)
    if ( status/=nf90_noerr .or. ndims/=1 ) then
        call set_error(ierr,message,'NetCDF time coordinate must be one-dimensional: '//trim(time_name))
        return
    endif
    status=nf90_inquire_dimension(ncid,dimids(1),len=axis%ntime)
    if ( status/=nf90_noerr ) then
        call set_error(ierr,message,'cannot read NetCDF time dimension length')
        return
    endif
    status=nf90_get_att(ncid,time_varid,'units',axis%units)
    if ( status/=nf90_noerr ) then
        call set_error(ierr,message,'NetCDF time coordinate is missing required units attribute')
        return
    endif
    axis%units=trim(sanitize_string(axis%units))
    status=nf90_get_att(ncid,time_varid,'calendar',calendar)
    if ( status/=nf90_noerr .and. status/=nf90_enotatt ) then
        call set_error(ierr,message,'cannot read NetCDF time calendar attribute')
        return
    endif
    call parse_cf_units(axis%units,calendar,unit_minutes,reference_minutes,axis%calendar,ierr,message)
    if ( ierr/=0 ) return

    allocate(raw_time(axis%ntime),axis%center_minutes(axis%ntime))
    status=nf90_get_var(ncid,time_varid,raw_time)
    if ( status/=nf90_noerr ) then
        call set_error(ierr,message,'cannot read NetCDF time coordinate values')
        return
    endif
    call values_to_minutes(raw_time,unit_minutes,reference_minutes,axis%center_minutes,ierr,message)
    if ( ierr/=0 ) return

    status=nf90_get_att(ncid,time_varid,'bounds',bound_name)
    if ( status==nf90_noerr ) then
        bound_name=trim(sanitize_string(bound_name))
        status=nf90_inq_varid(ncid,trim(bound_name),bound_varid)
        if ( status/=nf90_noerr ) then
            call set_error(ierr,message,'time bounds attribute names a missing variable: '//trim(bound_name))
            return
        endif
        status=nf90_inquire_variable(ncid,bound_varid,ndims=bound_ndims,dimids=bound_dimids)
        if ( status/=nf90_noerr .or. bound_ndims/=2 ) then
            call set_error(ierr,message,'NetCDF time bounds variable must be two-dimensional')
            return
        endif
        call nf90_check_dimension(ncid,bound_dimids(1),len1,ierr,message)
        if ( ierr/=0 ) return
        call nf90_check_dimension(ncid,bound_dimids(2),len2,ierr,message)
        if ( ierr/=0 ) return
        allocate(raw_bound(len1,len2),lower_raw(axis%ntime),upper_raw(axis%ntime))
        status=nf90_get_var(ncid,bound_varid,raw_bound)
        if ( status/=nf90_noerr ) then
            call set_error(ierr,message,'cannot read NetCDF time bounds values')
            return
        endif
        if ( len1==2 .and. len2==axis%ntime ) then
            lower_raw=raw_bound(1,:)
            upper_raw=raw_bound(2,:)
        elseif ( len1==axis%ntime .and. len2==2 ) then
            do i=1,axis%ntime
                lower_raw(i)=raw_bound(i,1)
                upper_raw(i)=raw_bound(i,2)
            enddo
        else
            call set_error(ierr,message,'NetCDF time bounds dimensions must be time x 2')
            return
        endif
        axis%has_bounds=.true.
        allocate(axis%lower_minutes(axis%ntime),axis%upper_minutes(axis%ntime))
        call values_to_minutes(lower_raw,unit_minutes,reference_minutes,axis%lower_minutes,ierr,message)
        if ( ierr/=0 ) return
        call values_to_minutes(upper_raw,unit_minutes,reference_minutes,axis%upper_minutes,ierr,message)
        if ( ierr/=0 ) return
    elseif ( status/=nf90_enotatt ) then
        call set_error(ierr,message,'cannot read NetCDF time bounds attribute')
        return
    endif

    call validate_time_axis(axis,ierr,message)
end subroutine cf_read_time_axis
!####################################################################

!####################################################################
subroutine nf90_check_dimension(ncid,dimid,length,ierr,message)
    integer, intent(in) :: ncid,dimid
    integer, intent(out) :: length
    integer(kind=JPIM), intent(out) :: ierr
    character(len=*), intent(out) :: message
    integer :: status

    ierr=0
    message=''
    status=nf90_inquire_dimension(ncid,dimid,len=length)
    if ( status/=nf90_noerr ) call set_error(ierr,message,'cannot inspect NetCDF time bounds dimension')
end subroutine nf90_check_dimension
!####################################################################
#endif

!####################################################################
subroutine cf_find_time_record(axis,yyyymmdd,hour,minute,record,ierr,message,require_interval_start)
    type(cf_time_axis), intent(in) :: axis
    integer(kind=JPIM), intent(in) :: yyyymmdd,hour,minute
    integer(kind=JPIM), intent(out) :: record
    integer(kind=JPIM), intent(out) :: ierr
    character(len=*), intent(out) :: message
    logical, optional, intent(in) :: require_interval_start
    integer(kind=JPIB) :: target
    integer :: i
    logical :: require_start

    record=0
    require_start=.false.
    if ( present(require_interval_start) ) require_start=require_interval_start
    call cf_datetime_to_minutes(axis%calendar,yyyymmdd,hour,minute,target,ierr,message)
    if ( ierr/=0 ) return
    if ( axis%has_bounds ) then
        ! Prefer the lower edge when it is also the previous record's coordinate
        ! or upper edge. End-stamped averages then select the interval beginning
        ! at the simulation start rather than the interval that has just ended.
        do i=1,axis%ntime
            if ( target==axis%lower_minutes(i) ) then
                record=i
                return
            endif
        enddo
        ! A coordinate value is also a valid update anchor for centered data whose
        ! bounds do not start at the model start hour.
        do i=1,axis%ntime
            if ( target==axis%center_minutes(i) ) then
                record=i
                return
            endif
        enddo
        do i=1,axis%ntime
            if ( target>=axis%lower_minutes(i) .and. target<axis%upper_minutes(i) ) then
                if ( require_start .and. target/=axis%lower_minutes(i) ) then
                    call set_error(ierr, message, &
                    &   'simulation start lies inside a time bound; start at its coordinate or lower edge')
                    return
                endif
                record=i
                return
            endif
        enddo
    else
        do i=1,axis%ntime
            if ( target==axis%center_minutes(i) ) then
                record=i
                return
            endif
        enddo
    endif
    call set_error(ierr,message,'simulation start time does not match any NetCDF time record')
end subroutine cf_find_time_record
!####################################################################

#ifdef UseCDF_CMF
!####################################################################
subroutine cf_resolve_time_record(ncid, time_name, yyyymmdd, hour, minute, lleapyr, &
&   expected_interval_seconds, expected_ntime, axis, record, ierr, message)
    integer, intent(in) :: ncid
    character(len=*), intent(in) :: time_name
    integer(kind=JPIM), intent(in) :: yyyymmdd,hour,minute,expected_interval_seconds,expected_ntime
    logical, intent(in) :: lleapyr
    type(cf_time_axis), intent(out) :: axis
    integer(kind=JPIM), intent(out) :: record,ierr
    character(len=*), intent(out) :: message
    logical :: calendar_ok

    call cf_read_time_axis(ncid, time_name, axis, ierr, message)
    if (ierr /= 0) return
    if (axis%ntime /= expected_ntime) then
        call set_error(ierr, message, 'NetCDF time coordinate length differs from its dimension')
        return
    endif
    if (axis%dt_minutes > 0_JPIB .and. &
    &   axis%dt_minutes * 60_JPIB /= int(expected_interval_seconds, kind=JPIB)) then
        call set_error(ierr, message, 'configured input interval differs from the NetCDF CF time interval')
        return
    endif
    calendar_ok = cf_calendar_matches_lleapyr(axis%calendar, lleapyr, ierr, message)
    if (ierr /= 0) return
    if (.not. calendar_ok) then
        call set_error(ierr, message, 'NetCDF calendar and LLEAPYR are inconsistent')
        return
    endif
    call cf_find_time_record(axis, yyyymmdd, hour, minute, record, ierr, message, .true.)
end subroutine cf_resolve_time_record
!####################################################################
#endif

!####################################################################
logical function cf_calendar_uses_leap_day(calendar,ierr,message)
    character(len=*), intent(in) :: calendar
    integer(kind=JPIM), intent(out) :: ierr
    character(len=*), intent(out) :: message
    integer :: ical
    character(len=32) :: normalized

    ical=0
    call normalize_calendar(calendar,ical,normalized,ierr,message)
    cf_calendar_uses_leap_day=(ierr==0 .and. ical==CAL_GREGORIAN)
end function cf_calendar_uses_leap_day
!####################################################################

!####################################################################
logical function cf_calendar_matches_lleapyr(calendar,lleapyr,ierr,message)
    character(len=*), intent(in) :: calendar
    logical, intent(in) :: lleapyr
    integer(kind=JPIM), intent(out) :: ierr
    character(len=*), intent(out) :: message
    logical :: uses_leap

    uses_leap=cf_calendar_uses_leap_day(calendar,ierr,message)
    cf_calendar_matches_lleapyr=(ierr==0 .and. uses_leap .eqv. lleapyr)
end function cf_calendar_matches_lleapyr
!####################################################################

end module cmf_cf_time_mod
