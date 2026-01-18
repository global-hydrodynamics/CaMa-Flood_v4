module input_namelist_mod
    use PARKIND1, only: &
    &   JPIM, JPRB, JPRM
    use glob_mod, only: &
    &   NML_PATH, CLEN_ITEM, CLEN_PATH, CLEN_SHORT
    use funit_lib, only: &
    &   LOG_UNIT
    implicit none

contains

subroutine open_namelist( &
&   nml_unit)
    integer(kind=JPIM), intent(in) :: &
    &   nml_unit
    open(nml_unit, file=trim(NML_PATH), status='old')
end subroutine open_namelist


subroutine raise_item_not_found_error( &
&   procedure_name, item_name)
    character(len=*), intent(in) :: &
    &   procedure_name, item_name
    write(LOG_UNIT, '(4a)') '[', trim(procedure_name), ' ERROR] item not found: ', trim(item_name)
    stop
end subroutine raise_item_not_found_error

! ===================================================================================================
subroutine read_nml_input_item( &
&   nml_unit, item_name, &
&   fmt, path, z_in, is_catm, is_fldstg, scale, offset, div_item)
    integer(kind=JPIM), intent(in) :: &
    &   nml_unit
    character(len=*), intent(in) :: &
    &   item_name
    character(len=CLEN_SHORT), intent(out) :: &
    &   fmt
    character(len=CLEN_ITEM), intent(out) :: &
    &   div_item
    character(len=CLEN_PATH), intent(out) :: &
    &   path
    integer(kind=JPIM), intent(out) :: &
    &   z_in
    logical, intent(out) :: &
    &   is_catm, is_fldstg
    real(kind=JPRM), intent(out) :: &
    &   scale, offset
    character(len=CLEN_ITEM) :: &
    &   item
    namelist /input_item/ &
    &   item, fmt, path, z_in, is_catm, is_fldstg, scale, offset, div_item
    integer(kind=JPIM) :: &
    &   ios

    rewind(nml_unit)
    do
        ! default
        fmt = ''
        path = ''
        z_in = 1
        is_catm = .FALSE.
        is_fldstg = .FALSE.
        scale = 1.d0
        offset = 0.d0
        div_item = ''
        read(nml_unit, nml=input_item, iostat=ios)
        if (ios < 0) call raise_item_not_found_error('read_nml_input_item', item_name)
        if (trim(item) == trim(item_name)) exit
    enddo
    if (trim(fmt) == '') stop 'init_indata ERROR: fmt not specified'
    if (trim(path) == '') stop 'init_indata ERROR: path not specified'
    write(LOG_UNIT, '(2a)')   '    fmt       = ', trim(fmt)
    write(LOG_UNIT, '(2a)')   '    path      = ', trim(path)
    write(LOG_UNIT, '(a,i0)') '    z_in      = ', z_in
    write(LOG_UNIT, '(a,L)')  '    is_catm   =', is_catm
    write(LOG_UNIT, '(a,L)')  '    is_fldstg =', is_fldstg
    write(LOG_UNIT, '(2a)')   '    div_item  = ', trim(div_item)
end subroutine read_nml_input_item


subroutine read_nml_input_domain( &
&   nml_unit, item_name, &
&   is_found, left, right, top, bottom)
    integer(kind=JPIM), intent(in) :: &
    &   nml_unit
    character(len=*), intent(in) :: &
    &   item_name
    logical, intent(out) :: &
    &   is_found
    real(kind=JPRB), intent(out) :: &
    &   left, right, top, bottom
    namelist /input_domain/ &
    &   item, left, right, top, bottom
    character(len=CLEN_ITEM) :: &
    &   item
    integer(kind=JPIM) :: &
    &   ios

    is_found = .FALSE.
    rewind(nml_unit)
    do
        read(nml_unit, nml=input_domain, iostat=ios)
        if (ios < 0) exit
        if (trim(item) == trim(item_name)) then
            is_found = .TRUE.
            exit
        endif
    enddo
end subroutine read_nml_input_domain


subroutine read_nml_input_shape( &
&   nml_unit, item_name, &
&   is_found, nx, ny, nz)
    integer(kind=JPIM), intent(in) :: &
    &   nml_unit
    character(len=*), intent(in) :: &
    &   item_name
    logical, intent(out) :: &
    &   is_found
    integer(kind=JPIM), intent(out) :: &
    &   nx, ny, nz
    namelist /input_shape/ &
    &   item, nx, ny, nz
    character(len=CLEN_ITEM) :: &
    &   item
    integer(kind=JPIM) :: &
    &   ios

    is_found = .FALSE.
    rewind(nml_unit)
    do
        read(nml_unit, nml=input_shape, iostat=ios)
        if (ios < 0) exit
        if (trim(item) == trim(item_name)) then
            is_found = .TRUE.
            exit
        endif
    enddo
end subroutine read_nml_input_shape


subroutine read_nml_input_tres( &
&   nml_unit, item_name, &
&   is_found, dt, dt_unit)
    integer(kind=JPIM), intent(in) :: &
    &   nml_unit
    character(len=*), intent(in) :: &
    &   item_name
    logical, intent(out) :: &
    &   is_found
    integer(kind=JPIM), intent(out) :: &
    &   dt
    character(len=CLEN_SHORT), intent(out) :: &
    &   dt_unit
    namelist /input_tres/ &
    &   item, dt, dt_unit
    character(len=16) :: &
    &   item
    integer(kind=JPIM) :: &
    &   ios

    rewind(nml_unit)
    is_found = .FALSE.
    do
        read(nml_unit, nml=input_tres, iostat=ios)
        if (ios < 0) exit
        if (trim(item) == trim(item_name)) then
            is_found = .TRUE.
            exit
        endif
    enddo
end subroutine read_nml_input_tres


subroutine read_nml_input_nc( &
&   nml_unit, item_name, &
&   is_found, var_name)
    integer(kind=JPIM), intent(in) :: &
    &   nml_unit
    character(len=*), intent(in) :: &
    &   item_name
    logical, intent(out) :: &
    &   is_found
    character(len=CLEN_ITEM), intent(out) :: &
    &   var_name
    character(len=CLEN_ITEM) :: &
    &   item
    namelist /input_nc/ &
    &   item, var_name
    integer(kind=JPIM) :: ios

    rewind(nml_unit)
    is_found = .FALSE.
    do
        var_name = ''
        read(nml_unit, nml=input_nc, iostat=ios)
        if (ios < 0) exit
        if (trim(item) == trim(item_name)) then
            is_found = .TRUE.
            exit
        endif
    enddo
    write(LOG_UNIT, '(2a)') '    var_name = ', trim(var_name)
end subroutine read_nml_input_nc


subroutine read_nml_input_scale( &
&   nml_unit, item_name, &
&   is_found, scale_item)
    integer(kind=JPIM), intent(in) :: &
    &   nml_unit
    character(len=*), intent(in) :: &
    &   item_name
    logical, intent(out) :: &
    &   is_found
    character(len=CLEN_ITEM) :: &
    &   scale_item
    character(len=CLEN_ITEM) :: &
    &   item
    namelist /input_scale/ &
    &   item, scale_item
    integer(kind=JPIM) :: &
    &   ios

    rewind(nml_unit)
    is_found = .FALSE.
    do
        read(nml_unit, nml=input_scale, iostat=ios)
        if (ios < 0) exit
        if (trim(item) == trim(item_name)) then
            is_found = .TRUE.
            exit
        endif
    enddo
end subroutine read_nml_input_scale

end module input_namelist_mod
