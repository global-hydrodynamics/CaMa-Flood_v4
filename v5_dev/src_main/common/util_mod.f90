module util_mod
    use YOS_CMF_INPUT, only: &
    &   LOGNAM
    implicit none
contains

subroutine write_string_with_indent(indent, string)
    integer, intent(in) :: indent
    character(*), intent(in) :: string
    character(len=256)          wrtFmt

    write(wrtFmt, '("(a" i1 ", a" i3 ")")') indent, len_trim(string)
    write(LOGNAM, wrtFmt) '', string
end subroutine write_string_with_indent

end module util_mod
