program verify_reader_contract
  use netcdf
  implicit none
  integer :: ncid, varid, status, i
  integer :: nextx(3,2), nexty(3,2)
  real :: ctmare(3,2), elevtn(3,2), nxtdst(3,2), rivlen(3,2)
  real :: fldhgt(3,2,2)
  real(kind=8) :: lon(3), lat(2)
  character(len=512) :: filename
  call get_command_argument(1, filename)
  call check(nf90_open(trim(filename), nf90_nowrite, ncid), 'open')
  call get_int('nextx', nextx)
  call get_int('nexty', nexty)
  call get_real('ctmare', ctmare)
  call get_real('elevtn', elevtn)
  call get_real('nxtdst', nxtdst)
  call get_real('rivlen', rivlen)
  call check(nf90_inq_varid(ncid, 'fldhgt', varid), 'find fldhgt')
  do i=1,2
    call check(nf90_get_var(ncid, varid, fldhgt(:,:,i), start=(/1,1,i/), count=(/3,2,1/)), 'read fldhgt')
  enddo
  call check(nf90_inq_varid(ncid, 'lon', varid), 'find lon')
  call check(nf90_get_var(ncid, varid, lon), 'read lon')
  call check(nf90_inq_varid(ncid, 'lat', varid), 'find lat')
  call check(nf90_get_var(ncid, varid, lat), 'read lat')
  if (nextx(1,1) /= 1 .or. nextx(3,2) /= -9 .or. nexty(3,2) /= -9999) error stop 'next indices/order mismatch'
  if (ctmare(2,1) /= 2.5 .or. elevtn(3,2) /= -9999.0) error stop 'scalar map order mismatch'
  if (nxtdst(1,2) /= 23.0 .or. rivlen(3,1) /= 32.0) error stop 'scalar map values mismatch'
  if (fldhgt(2,2,2) /= 54.0) error stop 'profile record order mismatch'
  if (abs(lon(1) + 1.0d0) > 1.d-12 .or. abs(lat(1) - 0.5d0) > 1.d-12) error stop 'coordinate order mismatch'
  call check(nf90_close(ncid), 'close')
  print '(a)', 'PASS: existing CaMa-Flood NetCDF GET_VAR shape/order contract'
contains
  subroutine check(code, action)
    integer, intent(in) :: code
    character(len=*), intent(in) :: action
    if (code /= nf90_noerr) then
      print *, trim(action), ': ', trim(nf90_strerror(code))
      error stop 1
    endif
  end subroutine check
  subroutine get_int(name, values)
    character(len=*), intent(in) :: name
    integer, intent(out) :: values(3,2)
    call check(nf90_inq_varid(ncid, name, varid), 'find '//name)
    call check(nf90_get_var(ncid, varid, values), 'read '//name)
  end subroutine get_int
  subroutine get_real(name, values)
    character(len=*), intent(in) :: name
    real, intent(out) :: values(3,2)
    call check(nf90_inq_varid(ncid, name, varid), 'find '//name)
    call check(nf90_get_var(ncid, varid, values), 'read '//name)
  end subroutine get_real
end program verify_reader_contract
