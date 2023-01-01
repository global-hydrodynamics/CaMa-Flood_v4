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

subroutine sed_diag_average
  use yos_cmf_sed,             only: d2rivout_sed, d2rivvel_sed, sadd_riv, sadd_out
  implicit none

  d2rivout_sed(:) = d2rivout_sed(:) /dble(sadd_riv)
  d2rivvel_sed(:) = d2rivvel_sed(:) /dble(sadd_riv)
end subroutine sed_diag_average

subroutine sed_diag_reset
  use PARKIND1,                only: JPRB
  use YOS_CMF_PROG,            only: D2RIVSTO
  use yos_cmf_sed,             only: d2rivsto_pre, d2rivout_sed, d2rivvel_sed, &
                                     sadd_riv, sadd_out, sedDT
  implicit none

  sadd_riv = 0
  d2rivout_sed(:) = 0._JPRB
  d2rivvel_sed(:) = 0._JPRB
  d2rivsto_pre(:) = D2RIVSTO(:,1)

  sadd_out = sadd_out + sedDT
end subroutine sed_diag_reset


end module sed_utils_mod
