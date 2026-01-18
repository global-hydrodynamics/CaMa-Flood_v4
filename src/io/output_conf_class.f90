module mod_outdata
    use funit_lib, only: &
    &   TMP_UNIT, LOG_UNIT, get_new_file_unit
    use glob_mod, only: &
    &   NML_PATH, BINMAP_SUF, BINVEC_SUF, CLEN_PATH, CLEN_ITEM
    use map_mod, only: &
    &   NX, NY
    use dim_converter, only: &
    &   vec2map
    use output_config_mod, only: &
    &   MAP_FMT
    implicit none
    private
    public :: OutData, init_OutData, append_OutData

    type OutData
        integer            unit
        character(len=CLEN_PATH) path
        double precision, allocatable :: d1d(:), d2d(:,:)
        logical            isMean ! .TRUE.: time averaged, .FALSE.: instantaneous
        integer            addedTimes, recNum
        logical(kind=4)    mapfmt

        contains

        procedure alloc_1d_dble  => alloc_1d_dble
        procedure alloc_2d_dble  => alloc_2d_dble
        procedure update_1d_dble => update_1d_dble
        procedure update_2d_dble => update_2d_dble
        procedure open_file  => open_file
        procedure write_file => write_file
        procedure close_file => close_file
        generic :: alloc  => alloc_1d_dble,  alloc_2d_dble
        generic :: update => update_1d_dble, update_2d_dble
    end type OutData

contains

! ===================================================================================================
! Constructor
! ===================================================================================================
subroutine read_nml_output( &
&   out_item, is_found, path, mapfmt)
    character(len=*), intent(in)  :: &
    &   out_item
    logical(kind=4), intent(out) :: &
    &   is_found, mapfmt
    character(len=CLEN_PATH), intent(out) :: &
    &   path
    character(len=CLEN_ITEM) :: &
    &   item
    character(len=CLEN_PATH) :: &
    &   file
    integer            :: &
    &   ios
    namelist /nml_out/ item, file, mapfmt

    is_found = .FALSE.
    open(TMP_UNIT, file=trim(NML_PATH), status='old')
    do
        mapfmt = MAP_FMT
        read(TMP_UNIT, nml=nml_out, iostat=ios)
        if (ios < 0) exit
        if (trim(item) == trim(out_item)) then
            is_found = .TRUE.
            exit
        endif
    enddo
    close(TMP_UNIT)
    if (mapfmt) then
        path = trim(file)//trim(BINMAP_SUF)
    else
        path = trim(file)//trim(BINVEC_SUF)
    endif
end subroutine read_nml_output


subroutine init_OutData(out_item, is_found, obj)
    character(len=*), intent(in)  :: out_item
    logical(kind=4) , intent(out) :: is_found ! in namelist
    type(OutData), intent(out) :: obj
    logical(kind=4)    mapfmt
    character(len=CLEN_PATH) outPath

    call read_nml_output(out_item, is_found, outPath, mapfmt)
    if (is_found) then
        obj%path       = trim(outPath)
        obj%unit       = get_new_file_unit()
        obj%isMean     = .TRUE.
        obj%addedTimes = 0
        obj%recNum     = 0
        obj%mapfmt     = mapfmt
    endif
    if (is_found) then
        write(LOG_UNIT, '(a,i3,3a,L)') '  ', obj%unit, ' ', trim(obj%path), ' ', obj%mapfmt
    endif
end subroutine init_OutData

! ---------------------------------------------------------------------------------------------------
subroutine alloc_1d_dble(self, data)
    class(OutData), intent(inout) :: self
    real(8),        intent(in)    :: data(:)
    allocate(self%d1d, source=data); self%d1d(:) = 0.d0
end subroutine alloc_1d_dble

subroutine alloc_2d_dble(self, data)
    class(OutData), intent(inout) :: self
    real(8),        intent(in)    :: data(:,:)
    allocate(self%d2d, source=data); self%d2d(:,:) = 0.d0
end subroutine alloc_2d_dble

! ---------------------------------------------------------------------------------------------------
subroutine open_file(self)
    class(OutData), intent(in) :: self
    integer recl
    if (self%mapfmt) then
        recl = 4 * NX * NY
    else
        if (allocated(self%d1d)) recl = 4 * size(self%d1d)
        if (allocated(self%d2d)) recl = 4 * size(self%d2d)
    endif
    open(self%unit, file=trim(self%path), &
    &    status='replace', form='unformatted', access='direct', recl=recl)
end subroutine open_file

! ===================================================================================================
subroutine update_1d_dble(self, array)
    class(OutData), intent(inout) :: self
    double precision, intent(in)  :: array(:)
    if (self%isMean) then
        self%d1d(:) = self%d1d(:) + array(:)
    else
        self%d1d(:) = array(:)
    endif
    self%addedTimes = self%addedTimes + 1
end subroutine update_1d_dble

subroutine update_2d_dble(self, array)
    class(OutData), intent(inout) :: self
    double precision, intent(in)  :: array(:,:)
    if (self%isMean) then
        self%d2d(:,:) = self%d2d(:,:) + array(:,:)
    else
        self%d2d(:,:) = array(:,:)
    endif
    self%addedTimes = self%addedTimes + 1
end subroutine update_2d_dble

! ===================================================================================================
! Write file
! ===================================================================================================
subroutine write_file(self)
    class(OutData), intent(inout) :: self
    self%recNum = self%recNum + 1
    if (allocated(self%d1d)) then
        call write_file_1d_dble(self)
        self%d1d(:) = 0.d0
    elseif (allocated(self%d2d)) then
        call write_file_2d_dble(self)
        self%d2d(:,:) = 0.d0
    else
        write(LOG_UNIT, *) 'write_file ERROR : both of d1d and d2d are not allocated'
    endif
    self%addedTimes = 0
!    write(LOG_UNIT, '(a,i3,3a,L)') '  ', self%unit, ' ', trim(self%path), ' ', self%mapfmt
end subroutine write_file


subroutine write_file_1d_dble(self)
    class(OutData), intent(inout) :: self
    double precision div_Nadd
    real(4) :: tmpMap(NX,NY) ! used for 1d->2d conversion

    if (self%isMean) then
        if (self%addedTimes > 0) then
            div_Nadd = 1.d0 / dble(self%addedTimes)
            self%d1d(:) = self%d1d(:) * div_Nadd
        else
            self%d1d(:) = 0.d0
        endif
    endif
!write(LOG_UNIT, *) trim(self%path), shape(self%d1d)
    if (self%mapfmt) then
        tmpMap(:,:) = 0.0
!write(LOG_UNIT, *) trim(self%path), maxval(self%d1d)
        call vec2map(self%d1d(:), tmpMap(:,:))
        write(self%unit, rec=self%recNum) tmpMap(:,:)
    else
        write(self%unit, rec=self%recNum) real(self%d1d(:))
    endif
end subroutine write_file_1d_dble


subroutine write_file_2d_dble(self)
    class(OutData), intent(inout) :: self
    double precision div_Nadd

    if (self%isMean) then
        if (self%addedTimes > 0) then
            div_Nadd = 1.d0 / dble(self%addedTimes)
            self%d2d(:,:) = self%d2d(:,:) * div_Nadd
        else
            self%d2d(:,:) = 0.d0
        endif
    endif

    if (self%mapfmt) then
        write(LOG_UNIT, *) 'write_file_2d_dble TODO : mapfmt = .TRUE.'
    else
        write(self%unit, rec=self%recNum) real(self%d2d(:,:))
    endif
end subroutine write_file_2d_dble

! ===================================================================================================
subroutine close_file(self)
    class(OutData), intent(in) :: self
    close(self%unit)
end subroutine close_file

! ===================================================================================================
subroutine append_OutData(array, obj)
    type(OutData), allocatable, intent(inout) :: array(:)
    type(OutData),              intent(in)    :: obj
    type(OutData), allocatable :: tmp(:)
    integer n
    if (.not. allocated(array)) then
        allocate(array(1)); array(1) = obj
        return
    endif
    n = size(array)
    allocate(tmp, source=array)
    deallocate(array)
    allocate(array(n + 1))
    array(1:n) = tmp(:)
    array(n+1) = obj
    deallocate(tmp)
end subroutine append_OutData

end module mod_outdata
