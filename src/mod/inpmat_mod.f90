module inpmat_mod
    use YOS_CMF_INPUT, only: &
    &   TMPNAM, LOGNAM, &
    &   RMIS, DMIS, &
    &   CSETFILE
    use CMF_UTILS_MOD, only: &
    &   INQUIRE_FID
    use YOS_CMF_INPUT, only: &
    &   NX, NY
    use YOS_CMF_MAP, only: &
    &   NSEQMAX, I1SEQX, I1SEQY

    use mapframe_mod, only: &
    &   MapFrame
    use glob_mod, only: &
    &   MNTSEQ
    implicit none
    private
    public :: Inpmat, append_inpmat

    type Inpmat
        type(MapFrame) :: &
        &   map
        integer :: &
        &   inpn
        integer, allocatable :: &
        &   inpx(:,:,:), inpy(:,:,:)
        real(8), allocatable :: &
        &   inpa(:,:,:)

        contains

        procedure :: map2vec_intrp => map2vec_intrp

    end type Inpmat

    interface Inpmat
        module procedure init_inpmat
    end interface Inpmat

contains

! ===================================================================================================
! Constructor
! ===================================================================================================
function init_inpmat(item) result(obj)
    type(Inpmat) obj
    character(len=*), intent(in) :: item
    character(len=256) :: dir, prefix
    integer :: nxin, nyin
    real(8) :: west, east, south, north
    logical :: is_n2s
    call read_inpmat_nml( &
    &   item, &
    &   dir, prefix)
    call read_inpmat_dim( &
    &   dir, prefix, nxin, nyin, obj%inpn)
    call obj%map%set_shape( &
    &   nxin, nyin)
    call read_inpmat_area( &
    &   dir, prefix, west, east, south, north, is_n2s)
    if (is_n2s) then
        call obj%map%set_domain(west, east, north, south)
    else
        call obj%map%set_domain(west, east, south, north)
    endif
    call read_inpmat_bin( &
    &   dir, prefix, obj%inpx, obj%inpy, obj%inpa, obj%inpn)
    write(LOGNAM, '(2a,i3)') '          ', trim(obj%map%str()), obj%inpn
!    if (.not. allocated(grda)) then
!        allocate(grda(NSEQMAX)); grda(:) = 0.d0
!    endif
    !call calc_gridarea(ainpmat%grda, &
    !&    ainpmat%inpx, ainpmat%inpy, ainpmat%inpa, ainpmat%nxin, ainpmat%nyin, ainpmat%inpn)

    contains

    subroutine read_inpmat_nml(item_name, dir, prefix)
        character(len=*)  , intent(in)  :: item_name
        character(len=256), intent(out) :: dir, prefix
        character(len=256) :: item
        namelist /nml_inpmat/ item, dir, prefix
        integer :: ios

        TMPNAM = INQUIRE_FID()
        open(TMPNAM, file=trim(CSETFILE), status='old', iostat=ios)
        if (ios /= 0) stop 'read_inpmat_nml ERROR : open namelist'
        do
            read(TMPNAM, nml=nml_inpmat, iostat=ios)
            if (ios < 0) stop 'read_inpmat_nml ERROR : not found item'
            if (trim(item) == trim(item_name)) exit
        enddo
        close(TMPNAM)
        write(LOGNAM, '(6a)') '      ', trim(item), ' : ', trim(dir), '/', trim(prefix)
    end subroutine read_inpmat_nml


    function path_inpmat(dir, prefix, suffix) result(path)
        character(len=256) :: path
        character(len=*)   :: dir, prefix, suffix
        path = trim(dir)//'/'//trim(prefix)//trim(suffix)
    end function path_inpmat


    subroutine read_inpmat_dim(dir, prefix, nxin, nyin, inpn)
        character(len=*), intent(in)  :: dir, prefix
        integer         , intent(out) :: nxin, nyin, inpn
        character(len=256) :: path

        TMPNAM = INQUIRE_FID()
        path = path_inpmat(dir, prefix, '.dim')
        open(TMPNAM, file=trim(path), form='formatted')
        read(TMPNAM, *) nxin
        read(TMPNAM, *) nyin
        read(TMPNAM, *) inpn
        close(TMPNAM)
        !write(LOGNAM, '(a,2i5,i3)') '  nxin, nyin, inpn =', nxin, nyin, inpn
    end subroutine read_inpmat_dim


    subroutine read_inpmat_area(dir, prefix, west, east, south, north, is_n2s)
        character(len=*), intent(in)  :: dir, prefix
        real(8),          intent(out) :: west, east, south, north
        logical,          intent(out) :: is_n2s
        character(len=256) :: path

        TMPNAM = INQUIRE_FID()
        path = path_inpmat(dir, prefix, '.cfg')
        open(TMPNAM, file=trim(path), form='formatted')
        read(TMPNAM, *)
        read(TMPNAM, *) west
        read(TMPNAM, *) east
        read(TMPNAM, *) south
        read(TMPNAM, *) north
        read(TMPNAM, *) is_n2s
        close(TMPNAM)
        !write(LOGNAM, '(a,4f0.2,L)') '  west, east, south, north =', west, east, south, north, is_n2s
    end subroutine read_inpmat_area


    subroutine read_inpmat_bin(dir, prefix, inpx, inpy, inpa, inpn)
        character(len=*),              intent(in)  :: dir, prefix
        integer,          allocatable, intent(out) :: inpx(:,:,:), inpy(:,:,:)
        double precision, allocatable, intent(out) :: inpa(:,:,:)
        integer         ,              intent(in)  :: inpn
        real, allocatable  :: rinp(:,:,:)
        integer, parameter :: byte_recl = 4
        character(len=256) :: path

        TMPNAM = INQUIRE_FID()
        path = path_inpmat(dir, prefix, '.bin')
        allocate(inpx(NX,NY,inpn), inpy(NX,NY,inpn), inpa(NX,NY,inpn), rinp(NX,NY,inpn))
        open(TMPNAM, file=trim(path), form='unformatted', access='direct', &
        &    recl=byte_recl*size(inpx))
        read(TMPNAM, rec=1) inpx(:,:,:)
        read(TMPNAM, rec=2) inpy(:,:,:)
        read(TMPNAM, rec=3) rinp(:,:,:)
        close(TMPNAM)
        inpa(:,:,:) = dble(rinp(:,:,:))
        deallocate(rinp)
    end subroutine read_inpmat_bin

end function init_inpmat

! ===================================================================================================
! Interpolation with inpmat
! ===================================================================================================
subroutine map2vec_intrp(self, map, vec)
    class(Inpmat)   , intent(in)  :: self
    real            , intent(in)  :: map(:,:)
    double precision, intent(out) :: vec(:)
    real(4), parameter :: fill_val = 1e16
    integer :: iseq, ix, iy, inpi, ixin, iyin
    real(8) :: grda

    !$omp parallel do private(ix, iy, inpi, ixin, iyin, grda)
    do iseq = 1, NSEQMAX
        vec(iseq) = 0.d0
        ix = I1SEQX(iseq)
        iy = I1SEQY(iseq)
        grda = 0.d0
        do inpi = 1, self%INPN
            if ( self%inpa(ix,iy,inpi) <= 0.0 ) exit
            ixin = self%INPX(ix, iy, inpi)
            iyin = self%INPY(ix, iy, inpi)
            if (ixin < 1) exit
            if (isnan(map(ixin,iyin))) cycle
            if ( map(ixin,iyin) < 0.0 ) cycle
            if ( map(ixin,iyin) >= fill_val ) cycle
            vec(iseq) = vec(iseq) + map(ixin,iyin) * self%inpa(ix,iy,inpi)
            grda      = grda      +                  self%inpa(ix,iy,inpi)
        enddo
        if (grda > 0.d0) vec(iseq) = vec(iseq) / grda
    enddo
    !$omp end parallel do
end subroutine map2vec_intrp

! ===================================================================================================
subroutine append_inpmat(inpmats, ainpmat)
    type(Inpmat), allocatable, intent(inout) :: inpmats(:)
    type(Inpmat),              intent(in)    :: ainpmat
    type(Inpmat), allocatable                :: tmp(:)
    integer :: old_size

    if (.not. allocated(inpmats)) then
        allocate(inpmats(1)); inpmats(1) = ainpmat
        return
    endif
    old_size = size(inpmats)
    allocate(tmp, source=inpmats)
    deallocate(inpmats)
    allocate(inpmats(old_size + 1))
    inpmats(1:old_size) = tmp(:)
    inpmats(old_size+1) = ainpmat
    deallocate(tmp)
end subroutine append_inpmat

end module inpmat_mod
