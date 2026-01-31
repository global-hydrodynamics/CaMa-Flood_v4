module text_lib
    implicit none
contains

function endswith(longtext, tailtext) result(ends)
    logical ends
    character(len=*), intent(in) :: longtext, tailtext
    character(len=256) :: longtext_, tailtext_
    integer :: long_eloc, tail_eloc

    ends = .FALSE.
    longtext_ = trim(longtext)
    tailtext_ = trim(tailtext)
    long_eloc = len_trim(longtext)
    tail_eloc = len_trim(tailtext)
    if (long_eloc == 0 .or. tail_eloc == 0) return
    if (long_eloc < tail_eloc) return
    if (longtext_(long_eloc - tail_eloc + 1:) == tailtext_) then
        ends = .TRUE.
    endif
end function endswith


logical function startswith( &
&   text, prefix)
    character(len=*), intent(in) :: text, prefix
    character(len=:), allocatable :: adjusted_text
    integer :: len_text, len_prefix

    startswith = .FALSE.
    len_text = len(text)
    len_prefix = len(prefix)    
    if (len_text < len_prefix) return

    adjusted_text = adjustl(text)
    if (adjusted_text(:len_prefix) == prefix) then
        startswith = .TRUE.
    endif
end function startswith

! ===================================================================================================
function lstrip(text, chars) result(ret)
    character(len=*), intent(in) :: text, chars
    character(len=:), allocatable :: ret
    integer :: len_chars

    ret = text
    len_chars = len(chars)
    do
        if (startswith(ret, chars)) then
            ret = ret(len_chars+1:)
        else
            exit
        endif
    enddo
end function lstrip

function rstrip(text, chars) result(ret)
    character(len=*), intent(in) :: &
    &   text, chars
    character(len=:), allocatable :: ret
    integer :: len_chars

    ret = trim(text)
    len_chars = len(chars)
    do
        if (endswith(ret, chars)) then
            ret = adjustr(ret)
            ret = adjustl(ret(:len(ret) - len_chars))
        else
            exit
        endif
    enddo
end function rstrip

! ===================================================================================================
function to_uppercase(text) result(ret)
    ! https://www.mk-mode.com/blog/2017/06/03/fortran-convert-chars-to-uppercase/
    character(len=*), intent(in) :: text
    character(len=:), allocatable :: ret
    integer :: i
    do i = 1, len(text)
        if (text(i:i) >= 'a' .and. text(i:i) <= 'z') then
            ret(i:i) = char(ichar(text(i:i)) - 32)
        else
            ret(i:i) = text(i:i)
        end if
    end do
end function to_uppercase


function to_lowercase(text) result(ret)
    character(len=*), intent(in) :: text
    character(len=:), allocatable :: ret
    integer :: i, n

    n = len(text)
    allocate(character(n) :: ret)
    ret = text

    do i = 1, n
        if (ret(i:i) >= 'A' .and. ret(i:i) <= 'Z') then
            ret(i:i) = char(ichar(ret(i:i)) + 32)
        end if
    end do
end function to_lowercase

end module text_lib
