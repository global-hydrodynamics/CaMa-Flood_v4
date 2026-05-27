module glob_mod
    use PARKIND1,  only: &
    &   JPIM, JPRB, JPRM
    use YOS_CMF_INPUT, only: &
    &   TMPNAM, LOGNAM
    use CMF_UTILS_MOD, only: &
    &   INQUIRE_FID

    use const_mod, only: &
    &   CLEN_PATH, CLEN_SHORT
    use time_recorder_class, only: &
    &   TimeRecorder
    implicit none
    save

    logical :: LLAKE = .FALSE.
    logical :: LHEATLINK = .FALSE.
    logical :: LICE = .FALSE.
    logical :: LMETEOR = .FALSE.
    logical :: LADVECTION      = .FALSE. ! horizontal flow
    logical :: LVERTICAL_WATER = .FALSE. ! P + R - E
    logical :: LPTHOUT = .FALSE. ! true  flood path flow active
    integer :: MNTSEQ = 0
    namelist /conf_global/ &
    &   LLAKE, LHEATLINK, LICE, LMETEOR, LADVECTION, LVERTICAL_WATER, LPTHOUT, MNTSEQ

    type(TimeRecorder) :: &
    &   TIME_RECORDER

    character(len=CLEN_PATH), parameter :: &
    &   NML_PATH = 'input_cmf.nam'

    character(len=CLEN_SHORT) :: &
    &   BINMAP_SUF = '.bin', &
    &   BINVEC_SUF = '.bin'
    character(len=CLEN_SHORT) :: &
    &   CSHORT_DEF = ''

contains

subroutine init_glob_mod
    write(LOGNAM, '(a)') '[glod_mod/init_glob_mod]'
    call read_config
    call check_config

    contains

    subroutine read_config
        TMPNAM = INQUIRE_FID()
        open(TMPNAM, file=trim(nml_path), status='old')
        read(TMPNAM, conf_global)
        close(TMPNAM)

        if (.not. LHEATLINK) LICE = .FALSE.
        write(LOGNAM, '(a,L)') '  LLAKE        =', LLAKE
        write(LOGNAM, '(a,L)') '  LHEATLINK    =', LHEATLINK
        write(LOGNAM, '(a,L)') '  LICE         =', LICE
        write(LOGNAM, '(a,L)') '  LMETEOR      =', LMETEOR
        write(LOGNAM, '(a,L)') '  LADVECTION      =', LADVECTION
        write(LOGNAM, '(a,L)') '  LVERTICAL_WATER =', LVERTICAL_WATER
        write(LOGNAM, '(a,L)') '  LPTHOUT =', LPTHOUT
        write(LOGNAM, '(a,i0)') '  MNTSEQ       = ', MNTSEQ
        write(LOGNAM, *) ''
    end subroutine read_config

    subroutine check_config
        write(LOGNAM, '(a)') '[glod_mod/check_config]'
        if ((.not. LHEATLINK) .and. LICE) then
            write(LOGNAM, '(a,2L)') '    ERROR: LHEATLINK, LICE =', LHEATLINK, LICE
        endif
    end subroutine check_config

end subroutine init_glob_mod

end module glob_mod
