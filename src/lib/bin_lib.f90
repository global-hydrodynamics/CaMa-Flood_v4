module bin_lib
    use funit_lib, only: TMP_UNIT
    implicit none

    interface read_bin
        module procedure read_bin_1d_r
        module procedure read_bin_2d_r
        module procedure read_bin_3d_r
        module procedure read_bin_2d_i
        module procedure read_bin_2d_i1
        module procedure read_bin_1d_d
        module procedure read_bin_2d_d
        module procedure read_bin_3d_d
    end interface read_bin

contains

subroutine open_bin(unit, path, recl)
    integer         , intent(in)  :: unit, recl
    character(len=*), intent(in)  :: path
    open (unit, file=trim(path), status='old', form='unformatted', access='direct', recl=recl)
end subroutine open_bin

! ===================================================================================================
subroutine read_bin_1d_r(array, inpath, rec)
    real            ,  intent(out) :: array(:)
    character(len=*),  intent(in)  :: inpath
    integer, optional, intent(in)  :: rec
    integer, parameter :: byte_recl = 4
    integer rec_
    call open_bin(TMP_UNIT, trim(inpath), byte_recl*size(array))
    rec_ = 1
    if (present(rec)) rec_ = rec
    read (TMP_UNIT, rec=rec_) array
    close(TMP_UNIT)
end subroutine read_bin_1d_r

subroutine read_bin_2d_r(array, inpath, rec)
    real            ,  intent(out) :: array(:,:)
    character(len=*),  intent(in)  :: inpath
    integer, optional, intent(in)  :: rec
    integer, parameter :: byte_recl = 4
    integer rec_
    call open_bin(TMP_UNIT, trim(inpath), byte_recl*size(array))
    rec_ = 1
    if (present(rec)) rec_ = rec
    read (TMP_UNIT, rec=rec_) array
    close(TMP_UNIT)
end subroutine read_bin_2d_r

subroutine read_bin_3d_r(array, inpath, rec)
    real            ,  intent(out) :: array(:,:,:)
    character(len=*),  intent(in)  :: inpath
    integer, optional, intent(in)  :: rec
    integer, parameter :: byte_recl = 4
    integer rec_
    call open_bin(TMP_UNIT, trim(inpath), byte_recl*size(array))
    rec_ = 1
    if (present(rec)) rec_ = rec
    read (TMP_UNIT, rec=rec_) array
    close(TMP_UNIT)
end subroutine read_bin_3d_r

subroutine read_bin_2d_i(array, inpath, rec)
    integer         ,  intent(out) :: array(:,:)
    character(len=*),  intent(in)  :: inpath
    integer, optional, intent(in)  :: rec
    integer, parameter :: byte_recl = 4
    integer rec_
    call open_bin(TMP_UNIT, trim(inpath), byte_recl*size(array))
    rec_ = 1
    if (present(rec)) rec_ = rec
    read (TMP_UNIT, rec=rec_) array
    close(TMP_UNIT)
end subroutine read_bin_2d_i

subroutine read_bin_2d_i1(array, inpath, rec)
    integer(1)      ,  intent(out) :: array(:,:)
    character(len=*),  intent(in)  :: inpath
    integer, optional, intent(in)  :: rec
    integer, parameter :: byte_recl = 1
    integer rec_
    call open_bin(TMP_UNIT, trim(inpath), byte_recl*size(array))
    rec_ = 1
    if (present(rec)) rec_ = rec
    read (TMP_UNIT, rec=rec_) array
    close(TMP_UNIT)
end subroutine read_bin_2d_i1

! ---------------------------------------------------------------------------------------------------
subroutine read_bin_1d_d(array, inpath, recnum)
    double precision, intent(out) :: array(:)
    character(len=*), intent(in)  :: inpath
    integer         , intent(in)  :: recnum
    real, allocatable :: rarray(:)
    allocate(rarray, source=real(array))
    call read_bin(rarray, inpath, recnum)
    array = dble(rarray)
    deallocate(rarray)
end subroutine read_bin_1d_d

subroutine read_bin_2d_d(array, inpath, recnum)
    double precision, intent(out) :: array(:,:)
    character(len=*), intent(in)  :: inpath
    integer         , intent(in)  :: recnum
    real, allocatable :: rarray(:,:)
    allocate(rarray, source=real(array))
    call read_bin(rarray, inpath, recnum)
    array = dble(rarray)
    deallocate(rarray)
end subroutine read_bin_2d_d

subroutine read_bin_3d_d(array, inpath, recnum)
    double precision, intent(out) :: array(:,:,:)
    character(len=*), intent(in)  :: inpath
    integer         , intent(in)  :: recnum
    real, allocatable :: rarray(:,:,:)
    allocate(rarray, source=real(array))
    call read_bin(rarray, inpath, recnum)
    array = dble(rarray)
    deallocate(rarray)
end subroutine read_bin_3d_d

end module bin_lib
