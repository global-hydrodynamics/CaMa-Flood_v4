module topo_mod
#ifdef heatlink
    use PARKIND1, only: &
    &   JPIM, JPRB
    use YOS_CMF_INPUT, only: &
    &   LOGNAM
    implicit none
    private
    public :: &
    &   D2RIVSLP, &
    &   init_topo_mod
    save

    ! river channel
    real(kind=JPRB), allocatable :: &
    &   D2RIVSLP   (:)    ! bed slope [m/m]

contains

subroutine init_topo_mod
    write(LOGNAM, '(a)') '[heatlink/topo_mod/init_topo_mod] start'
    call calc_rivslp

    contains

    subroutine calc_rivslp
        use YOS_CMF_MAP, only: &
        &   I1NEXT, NSEQALL, NSEQRIV, &
        &   D2RIVELV, D2NXTDST
        real(kind=JPRB), parameter :: &
        &   slope_max = 1.0_JPRB, slope_min = 0.001_JPRB
        integer(kind=JPIM) :: &
        &   iseq, dseq

        allocate(D2RIVSLP(NSEQALL), source=0.0_JPRB)
        do iseq = 1, NSEQRIV
            dseq = I1NEXT(iseq)
            D2RIVSLP(iseq) = (D2RIVELV(iseq,1) - D2RIVELV(dseq,1)) / D2NXTDST(iseq,1)
            D2RIVSLP(iseq) = min(D2RIVSLP(iseq), slope_max)
        enddo

        do iseq = NSEQRIV + 1, NSEQALL
            D2RIVSLP(iseq) = slope_min
        enddo
    end subroutine calc_rivslp

end subroutine init_topo_mod
#endif
end module topo_mod
