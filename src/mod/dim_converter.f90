module dim_converter
    use PARKIND1, only: &
    &   JPRM, JPRD
    use YOS_CMF_INPUT, only: &
    &   TMPNAM, LOGNAM, &
    &   LLOGOUT, CLOGOUT, &
    &   CSETFILE
    use YOS_CMF_MAP, only: &
    &   NSEQMAX
    use CMF_UTILS_MOD, only: &
    &   INQUIRE_FID
    use CMF_UTILS_MOD, only: &
    &   map2vec_catm, vec2map => vec2map_catm

    use array_mod, only: &
    &   find_index, append
    use numeric_utils_mod, only: &
    &   nearly_equal
    use inpmat_mod, only: &
    &   Inpmat, append_inpmat
    use mapframe_mod
    use camaframe_mod
    implicit none
    private
    public :: &
    &   init_dim_converter, map2vec, vec2map, find_inpmat

    type(Inpmat), allocatable :: &
    &   inpmats(:)
    character(len=32), allocatable :: &
    &   inpmat_names(:)
    logical :: &
    &   all_diff, & ! .TRUE.: all of inpmats have different size (nxin,nyin)
    &   LINTRP

    interface map2vec
        module procedure :: map2vec_r2d
        module procedure :: map2vec_r2r
    end interface map2vec

contains

! ===================================================================================================
subroutine init_dim_converter
    integer, parameter :: &
    &   inpmat_max = 100
    character(len=256), allocatable :: &
    &   inpmat_names(:)
    character(len=256), parameter :: &
    &   sentinel_item = 'none'
    integer :: &
    &   i
    namelist /intrp_map/ &
    &   LINTRP, inpmat_names

    allocate(inpmat_names(inpmat_max)); inpmat_names(:) = sentinel_item
    TMPNAM = INQUIRE_FID()
    open(TMPNAM, file=trim(CSETFILE), status='old')
    read(TMPNAM, nml=intrp_map)
    close(TMPNAM)
    write(LOGNAM, '(a)') 'init_dim_converter'
    write(LOGNAM, '(a,L)') '    LINTRP =', LINTRP
    do i = 1, inpmat_max
        if (trim(inpmat_names(i)) == trim(sentinel_item)) exit
        call add_inpmat(inpmat_names(i))
    enddo
    write(LOGNAM, *)
end subroutine init_dim_converter


subroutine add_inpmat(item)
    character(len=*), intent(in) :: item
    if (find_index(inpmat_names, item) > 0) return
    call append(inpmat_names, item)
    call append_inpmat(inpmats, Inpmat(item))
    all_diff = all_inpmat_have_different_size(inpmats(:))
end subroutine add_inpmat


function all_inpmat_have_different_size(inpmats) result(havediff)
!    use mapframe_lib, only : MapFrame
    logical havediff
    type(Inpmat), intent(in) :: inpmats(:)
    integer i, j, n
    havediff = .TRUE.
    n        = size(inpmats)
    if (n < 2) return

    do i = 1, n - 1
        do j = i + 1, n
            if (inpmats(i)%map == inpmats(j)%map) then
                havediff = .FALSE.
                return
            endif
        enddo
    enddo
end function all_inpmat_have_different_size

! ===================================================================================================
subroutine map2vec_r2d(map, vec, cmf, inpmat_idx)
    real(kind=JPRM), intent(in)  :: map(:, :)
    real(kind=JPRD), intent(out) :: vec(NSEQMAX)
    type(CaMaFrame), intent(in), optional :: cmf
    integer,         intent(in), optional :: inpmat_idx
    integer idx
    if (present(cmf)) then
        if (cmf%is_catm()) then
            call map2vec_catm(map(:,:), vec(:))
            return
        endif
    else
        if (.not. present(inpmat_idx))then
            call map2vec_catm(map(:,:), vec(:))
            return
        endif
    endif

    if (LINTRP) then
        if (present(inpmat_idx)) then
            idx = inpmat_idx
        else
            idx = find_inpmat(cmf)
        endif
        call inpmats(idx)%map2vec_intrp(map(:,:), vec(:))
    else
        stop 'TODO: map2vec, not LINTRP'
    endif
end subroutine map2vec_r2d


subroutine map2vec_r2r(map, vec, cmf, inpmat_idx)
    real(kind=JPRM), intent(in)  :: map(:, :)
    real(kind=JPRM), intent(out) :: vec(NSEQMAX)
    type(CaMaFrame), intent(in), optional :: cmf
    integer,         intent(in), optional :: inpmat_idx
    real(kind=JPRD) :: dvec(NSEQMAX)
    dvec(:) = real(vec(:), kind=JPRD)
    call map2vec(map, dvec, cmf, inpmat_idx)
    vec(:) = real(dvec(:), kind=JPRM)
end subroutine map2vec_r2r

! ===================================================================================================
function find_inpmat(cmf, inpmat_name) result(idx)
    integer idx
    type(CaMaFrame), intent(in) :: &
    &   cmf
    character(len=*), optional, intent(in) :: &
    &   inpmat_name
    integer &
    &   idx_
    idx = 0
    if (present(inpmat_name)) then
        idx = find_index(inpmat_names, inpmat_name)
        if (idx < 1) then
            write(LOGNAM, *) '[dim_converter/find_inpmat ERROR] not found: ', trim(inpmat_name)
            stop
        endif
        if (.not. eqmap(inpmats(idx)%map, cmf)) then
            write(LOGNAM, *) '[dim_converter/find_inpmat ERROR] incorrect inpmat name', trim(inpmat_name)
            stop
        endif
    else
        if (.not. all_diff) stop '[dim_converter/find_inpmat ERROR] not identigy inpmat due to same size'
        do idx_ = 1, size(inpmats)
            if (eqmap(inpmats(idx_)%map, cmf)) then
                idx = idx_
                exit
            endif
        enddo
        if (idx == 0) then
            write(LOGNAM, *) '[dim_converter/find_inpmat ERROR] inpmat not found'
            write(LOGNAM, *) cmf%str()
            stop
        endif
    endif

    contains

    logical function eqmap(amap, bmap) result(eq)
        class(MapFrame), intent(in) :: amap, bmap
        eq = nearly_equal(amap%left,   bmap%left)   .and. &
            nearly_equal(amap%right,  bmap%right)  .and. &
            nearly_equal(amap%top,    bmap%top)    .and. &
            nearly_equal(amap%bottom, bmap%bottom) .and. &
            (amap%nx == bmap%nx) .and. (amap%ny == bmap%ny)
    end function eqmap

end function find_inpmat

end module dim_converter
