module sed_utils_mod

contains
subroutine splitchar(allvars,vnames)
  ! same function as splitting characters in CaMa
  use PARKIND1,                only: JPIM
  implicit none
  save
  character(len=256), intent(in)  :: allvars
  character(len=256), intent(out) :: vnames(:)
  integer(kind=JPIM)              :: nvarsout, j0, j
  character(len=256)              :: ctmp

  nvarsout = 0
  j0 = 1
  do j = 1, len(trim(allvars))
    if ( (j>j0) .and. (allvars(j:j).eq.',') ) then
      ctmp = trim(adjustl(allvars(j0:j-1)))
      if ( len(ctmp) > 0 ) then
        nvarsout = nvarsout + 1
        vnames(nvarsout) = ctmp
      endif
      j0 = j + 1
    endif
  enddo

  ! last one
  if ( j0 < len(trim(allvars)) ) then
    j = len(trim(allvars))
    ctmp = trim(adjustl(allvars(j0:j)))
    if ( len(ctmp) > 0 ) then
      nvarsout = nvarsout + 1
      vnames(nvarsout) = ctmp
    endif
  endif
end subroutine splitchar

end module sed_utils_mod
