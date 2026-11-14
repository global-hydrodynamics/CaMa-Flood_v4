module bin_mod
    use PARKIND1, only: &
    &   JPRM, JPRD
    implicit none
    private
    public :: &
    &   open_bin, read_bin, write_bin

    interface read_bin
        module procedure read_bin_single_1d
        module procedure read_bin_double_1d
        module procedure read_bin_single_2d
        module procedure read_bin_double_2d
        module procedure read_bin_single_3d
        module procedure read_bin_double_3d
        module procedure read_bin_integer_2d
        module procedure read_bin_integer1_2d
    end interface read_bin

    interface write_bin
        module procedure write_bin_single_1d
        module procedure write_bin_double_1d
        module procedure write_bin_single_2d
        module procedure write_bin_double_2d
        module procedure write_bin_single_3d
        module procedure write_bin_double_3d
    end interface write_bin

contains

subroutine open_bin(unit, path, recl)
    integer         , intent(in)  :: unit, recl
    character(len=*), intent(in)  :: path
    open (unit, file=trim(path), status='old', form='unformatted', access='direct', recl=recl)
end subroutine open_bin

! ===================================================================================================
subroutine read_bin_single_1d(array, inpath, rec)
    real(kind=JPRM) ,  intent(out) :: array(:)
    character(len=*),  intent(in)  :: inpath
    integer, optional, intent(in)  :: rec
    integer rec_, reclen, unit
    reclen = storage_size(array) / 8 * size(array)
    open(newunit=unit, file=trim(inpath), status='old', form='unformatted', &
    &   access='direct', recl=reclen)
    rec_ = 1
    if (present(rec)) rec_ = rec
    read(unit, rec=rec_) array
    close(unit)
end subroutine read_bin_single_1d

subroutine read_bin_double_1d(array, inpath, rec)
    real(kind=JPRD) ,  intent(out) :: array(:)
    character(len=*),  intent(in)  :: inpath
    integer, optional, intent(in)  :: rec
    integer rec_, reclen, unit
    reclen = storage_size(array) / 8 * size(array)
    open(newunit=unit, file=trim(inpath), status='old', form='unformatted', &
    &   access='direct', recl=reclen)
    rec_ = 1
    if (present(rec)) rec_ = rec
    read(unit, rec=rec_) array
    close(unit)
end subroutine read_bin_double_1d

subroutine read_bin_single_2d(array, inpath, rec)
    real(kind=JPRM) ,  intent(out) :: array(:,:)
    character(len=*),  intent(in)  :: inpath
    integer, optional, intent(in)  :: rec
    integer rec_, reclen, unit
    reclen = storage_size(array) / 8 * size(array)
    open(newunit=unit, file=trim(inpath), status='old', form='unformatted', &
    &   access='direct', recl=reclen)
    rec_ = 1
    if (present(rec)) rec_ = rec
    read(unit, rec=rec_) array
    close(unit)
end subroutine read_bin_single_2d

subroutine read_bin_double_2d(array, inpath, rec)
    real(kind=JPRD) ,  intent(out) :: array(:,:)
    character(len=*),  intent(in)  :: inpath
    integer, optional, intent(in)  :: rec
    integer rec_, reclen, unit
    reclen = storage_size(array) / 8 * size(array)
    open(newunit=unit, file=trim(inpath), status='old', form='unformatted', &
    &   access='direct', recl=reclen)
    rec_ = 1
    if (present(rec)) rec_ = rec
    read(unit, rec=rec_) array
    close(unit)
end subroutine read_bin_double_2d

subroutine read_bin_single_3d(array, inpath, rec)
    real(kind=JPRM) ,  intent(out) :: array(:,:,:)
    character(len=*),  intent(in)  :: inpath
    integer, optional, intent(in)  :: rec
    integer rec_, reclen, unit
    reclen = storage_size(array) / 8 * size(array)
    open(newunit=unit, file=trim(inpath), status='old', form='unformatted', &
    &   access='direct', recl=reclen)
    rec_ = 1
    if (present(rec)) rec_ = rec
    read(unit, rec=rec_) array
    close(unit)
end subroutine read_bin_single_3d

subroutine read_bin_double_3d(array, inpath, rec)
    real(kind=JPRD) ,  intent(out) :: array(:,:,:)
    character(len=*),  intent(in)  :: inpath
    integer, optional, intent(in)  :: rec
    integer rec_, reclen, unit
    reclen = storage_size(array) / 8 * size(array)
    open(newunit=unit, file=trim(inpath), status='old', form='unformatted', &
    &   access='direct', recl=reclen)
    rec_ = 1
    if (present(rec)) rec_ = rec
    read(unit, rec=rec_) array
    close(unit)
end subroutine read_bin_double_3d

subroutine read_bin_integer_2d(array, inpath, rec)
    integer         ,  intent(out) :: array(:,:)
    character(len=*),  intent(in)  :: inpath
    integer, optional, intent(in)  :: rec
    integer rec_, reclen, unit
    reclen = storage_size(array) / 8 * size(array)
    open(newunit=unit, file=trim(inpath), status='old', form='unformatted', &
    &   access='direct', recl=reclen)
    rec_ = 1
    if (present(rec)) rec_ = rec
    read(unit, rec=rec_) array
    close(unit)
end subroutine read_bin_integer_2d

subroutine read_bin_integer1_2d(array, inpath, rec)
    integer(1)      ,  intent(out) :: array(:,:)
    character(len=*),  intent(in)  :: inpath
    integer, optional, intent(in)  :: rec
    integer rec_, reclen, unit
    reclen = storage_size(array) / 8 * size(array)
    open(newunit=unit, file=trim(inpath), status='old', form='unformatted', &
    &   access='direct', recl=reclen)
    rec_ = 1
    if (present(rec)) rec_ = rec
    read(unit, rec=rec_) array
    close(unit)
end subroutine read_bin_integer1_2d

! ===================================================================================================
subroutine write_bin_single_1d(array, outpath, rec)
    real(kind=JPRM) ,  intent(in) :: array(:)
    character(len=*),  intent(in) :: outpath
    integer, optional, intent(in) :: rec
    integer rec_, reclen, unit
    reclen = storage_size(array) / 8 * size(array)
    open(newunit=unit, file=trim(outpath), status='unknown', form='unformatted', &
    &   access='direct', recl=reclen)
    rec_ = 1
    if (present(rec)) rec_ = rec
    write(unit, rec=rec_) array
    close(unit)
end subroutine write_bin_single_1d

subroutine write_bin_double_1d(array, outpath, rec)
    real(kind=JPRD) ,  intent(in) :: array(:)
    character(len=*),  intent(in) :: outpath
    integer, optional, intent(in) :: rec
    integer rec_, reclen, unit
    reclen = storage_size(array) / 8 * size(array)
    open(newunit=unit, file=trim(outpath), status='unknown', form='unformatted', &
    &   access='direct', recl=reclen)
    rec_ = 1
    if (present(rec)) rec_ = rec
    write(unit, rec=rec_) array
    close(unit)
end subroutine write_bin_double_1d

subroutine write_bin_single_2d(array, outpath, rec)
    real(kind=JPRM) ,  intent(in) :: array(:,:)
    character(len=*),  intent(in) :: outpath
    integer, optional, intent(in) :: rec
    integer rec_, reclen, unit
    reclen = storage_size(array) / 8 * size(array)
    open(newunit=unit, file=trim(outpath), status='unknown', form='unformatted', &
    &   access='direct', recl=reclen)
    rec_ = 1
    if (present(rec)) rec_ = rec
    write(unit, rec=rec_) array
    close(unit)
end subroutine write_bin_single_2d

subroutine write_bin_double_2d(array, outpath, rec)
    real(kind=JPRD) ,  intent(in) :: array(:,:)
    character(len=*),  intent(in) :: outpath
    integer, optional, intent(in) :: rec
    integer rec_, reclen, unit
    reclen = storage_size(array) / 8 * size(array)
    open(newunit=unit, file=trim(outpath), status='unknown', form='unformatted', &
    &   access='direct', recl=reclen)
    rec_ = 1
    if (present(rec)) rec_ = rec
    write(unit, rec=rec_) array
    close(unit)
end subroutine write_bin_double_2d

subroutine write_bin_single_3d(array, outpath, rec)
    real(kind=JPRM) ,  intent(in) :: array(:,:,:)
    character(len=*),  intent(in) :: outpath
    integer, optional, intent(in) :: rec
    integer rec_, reclen, unit
    reclen = storage_size(array) / 8 * size(array)
    open(newunit=unit, file=trim(outpath), status='unknown', form='unformatted', &
    &   access='direct', recl=reclen)
    rec_ = 1
    if (present(rec)) rec_ = rec
    write(unit, rec=rec_) array
    close(unit)
end subroutine write_bin_single_3d

subroutine write_bin_double_3d(array, outpath, rec)
    real(kind=JPRD) ,  intent(in) :: array(:,:,:)
    character(len=*),  intent(in) :: outpath
    integer, optional, intent(in) :: rec
    integer rec_, reclen, unit
    reclen = storage_size(array) / 8 * size(array)
    open(newunit=unit, file=trim(outpath), status='unknown', form='unformatted', &
    &   access='direct', recl=reclen)
    rec_ = 1
    if (present(rec)) rec_ = rec
    write(unit, rec=rec_) array
    close(unit)
end subroutine write_bin_double_3d

end module bin_mod
