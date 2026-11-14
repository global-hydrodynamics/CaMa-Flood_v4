module dim_converter
    use, intrinsic :: iso_c_binding, only: c_associated, c_char, c_null_char, c_ptr
    use const_mod, only: CLEN_PATH
    use PARKIND1, only: &
    &   JPRM, JPRB
    use YOS_CMF_INPUT, only: LOGNAM
    use YOS_CMF_MAP, only: &
    &   NSEQMAX
    use CMF_UTILS_MOD, only: &
    &   map2vec_catm_jprb => mapR2vecD, &
    &   vec2map_catm_jprm => vecD2mapR

    use inpmat_mod, only: &
    &   Inpmat, load_inpmat_files, move_append_inpmat
    use camaframe_mod, only: CaMaFrame
    implicit none
    private
    public :: &
    &   init_dim_converter, map2vec, vec2map, get_inpmat_index

    ! This module handles map/vector conversion for standard input/output.
    ! File-side arrays are JPRM, and model-side arrays are JPRB.
    ! JPRD storage variables should be explicitly converted to JPRB
    ! before calling these conversion routines.

    ! Positive indices identify file pairs; CaMa-grid inputs need no mapping.
    type(Inpmat), allocatable :: inpmats(:)
    type MappingFiles
        character(len=CLEN_PATH) :: diminfo_file, inpmat_file
    end type MappingFiles
    type(MappingFiles), allocatable :: mapping_files(:)
    integer, parameter :: PATH_BUFFER_LEN = 4096
    interface
        function c_realpath(path, resolved_path) bind(C, name='realpath') result(result_ptr)
            import :: c_char, c_ptr
            character(kind=c_char), intent(in) :: path(*)
            character(kind=c_char), intent(out) :: resolved_path(*)
            type(c_ptr) :: result_ptr
        end function c_realpath
    end interface

    interface map2vec
        module procedure :: map2vec_m2b
    end interface map2vec

    interface vec2map
        module procedure :: vec2map_b2m
    end interface vec2map

contains

! ===================================================================================================
subroutine init_dim_converter
    if (allocated(inpmats)) deallocate(inpmats)
    if (allocated(mapping_files)) deallocate(mapping_files)
end subroutine init_dim_converter


subroutine map2vec_m2b(map, vec, cmf, inpmat_idx)
    real(kind=JPRM), intent(in)  :: map(:, :)
    real(kind=JPRB), intent(out) :: vec(NSEQMAX)
    type(CaMaFrame), intent(in), optional :: cmf
    integer,         intent(in), optional :: inpmat_idx
    integer idx
    real(kind=JPRB) :: tmpvec(NSEQMAX, 1)
    if (present(cmf)) then
        if (cmf%is_catm()) then
            call map2vec_catm_jprb(map(:,:), tmpvec(:,:))
            vec(:) = tmpvec(:,1)
            return
        endif
    else
        if (.not. present(inpmat_idx))then
            call map2vec_catm_jprb(map(:,:), tmpvec(:,:))
            vec(:) = tmpvec(:,1)
            return
        endif
    endif

    if (.not. present(inpmat_idx)) error stop 'mapping file pair must be selected for gridded input'
    if (.not. allocated(inpmats)) error stop 'mapping cache is empty'
    idx = inpmat_idx
    if (idx < 1 .or. idx > size(inpmats)) error stop 'invalid mapping index'
    if (.not. inpmats(idx)%matches_shape(size(map,1), size(map,2))) then
        error stop 'mapping source shape mismatch'
    endif
    call inpmats(idx)%map2vec_intrp(map, vec)
end subroutine map2vec_m2b


subroutine vec2map_b2m(vec, map)
    real(kind=JPRB), intent(in)  :: vec(NSEQMAX)
    real(kind=JPRM), intent(out) :: map(:,:)
    real(kind=JPRB) :: tmpvec(NSEQMAX, 1)

    tmpvec(:,1) = vec(:)
    call vec2map_catm_jprm(tmpvec(:,:), map(:,:))
end subroutine vec2map_b2m

integer function get_inpmat_index(diminfo_file, inpmat_file, nxin, nyin) result(idx)
    character(len=*), intent(in) :: diminfo_file, inpmat_file
    integer, intent(in) :: nxin, nyin
    type(MappingFiles) :: files
    type(MappingFiles), allocatable :: tmp(:)
    type(Inpmat) :: mapping
    integer :: i, n

    if (len_trim(diminfo_file) == 0 .or. len_trim(inpmat_file) == 0) then
        error stop 'both diminfo_file and inpmat_file are required'
    endif
    files%diminfo_file = normalize_file(diminfo_file)
    files%inpmat_file = normalize_file(inpmat_file)
    n = 0
    if (allocated(mapping_files)) then
        n = size(mapping_files)
        do i = 1, n
            if (files%diminfo_file /= mapping_files(i)%diminfo_file) cycle
            if (files%inpmat_file /= mapping_files(i)%inpmat_file) cycle
            if (.not. inpmats(i)%matches_shape(nxin, nyin)) error stop 'mapping source shape mismatch'
            idx = i
            return
        enddo
    endif
    call load_inpmat_files(mapping, files%diminfo_file, files%inpmat_file)
    if (.not. mapping%matches_shape(nxin, nyin)) error stop 'mapping source shape mismatch'
    call move_append_inpmat(inpmats, mapping)
    allocate(tmp(n+1))
    if (n > 0) tmp(1:n) = mapping_files
    tmp(n+1) = files
    call move_alloc(tmp, mapping_files)
    idx = n+1
end function get_inpmat_index

function normalize_file(path) result(normalized)
    character(len=*), intent(in) :: path
    character(len=CLEN_PATH) :: normalized
    character(len=len(path)) :: file_path
    character(kind=c_char) :: c_path(PATH_BUFFER_LEN), c_resolved_path(PATH_BUFFER_LEN)
    type(c_ptr) :: result_ptr
    integer :: i, path_len, resolved_len

    normalized = ''
    file_path = trim(path)
    path_len = len_trim(file_path)
    if (path_len < 1) return
    if (path_len >= PATH_BUFFER_LEN) then
        write(LOGNAM, '(a,i0)') '[dim_converter/normalize_file ERROR] input path is too long: ', path_len
        stop 1
    endif

    c_path(:) = c_null_char
    c_resolved_path(:) = c_null_char
    do i = 1, path_len
        c_path(i) = file_path(i:i)
    enddo
    result_ptr = c_realpath(c_path, c_resolved_path)
    if (.not. c_associated(result_ptr)) then
        write(LOGNAM, '(2a)') '[dim_converter/normalize_file ERROR] cannot resolve mapping file: ', trim(file_path)
        stop 1
    endif

    resolved_len = 0
    do i = 1, PATH_BUFFER_LEN
        if (c_resolved_path(i) == c_null_char) exit
        resolved_len = i
    enddo
    if (resolved_len > len(normalized)) then
        write(LOGNAM, '(a,i0,a,i0)') &
        &   '[dim_converter/normalize_file ERROR] resolved path length ', resolved_len, &
        &   ' exceeds CLEN_PATH=', len(normalized)
        stop 1
    endif
    do i = 1, resolved_len
        normalized(i:i) = c_resolved_path(i)
    enddo
end function normalize_file
end module dim_converter
