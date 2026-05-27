module const_mod
    use PARKIND1,  only: &
    &   JPIM, JPRB, JPRM
    implicit none

    integer, parameter :: &
    &   CLEN_LONG = 256, &
    &   CLEN_SHORT = 16, &
    &   CLEN_PATH = 256, &
    &   CLEN_ITEM = 32

    integer(kind=JPIM), parameter :: &
    &   IMIS = -9999_JPIM
    real   (kind=JPRM), parameter :: &
    &   RMIS = 1.E20_JPRM
    real   (kind=JPRB), parameter :: &
    &   DMIS = 1.E20_JPRB
    double precision  , parameter :: &
    &   STO_IGNORE = 1.d-9

end module const_mod