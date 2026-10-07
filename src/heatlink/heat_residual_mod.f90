module heat_residual_mod
    use PARKIND1, only: JPIM, JPIB, JPRD
    implicit none
    private
    public :: HeatResidualStats, record_heat_residual, write_heat_residual
    public :: HeatConservationStats, record_heat_exchange, write_heat_conservation

    ! Diagnostics only: no value in this type is returned to a physical state.
    ! Accumulation starts at process initialization, including for restart runs.
    type HeatResidualStats
        real(kind=JPRD) :: positive_j = 0.0_JPRD ! [J] Sum of positive expected-minus-represented energy differences.
        real(kind=JPRD) :: negative_j = 0.0_JPRD ! [J] Sum of negative differences, retaining their sign.
        real(kind=JPRD) :: maximum_absolute_j = 0.0_JPRD ! [J] Largest absolute cellwise difference seen in this run.
        real(kind=JPRD) :: throughput_j = 0.0_JPRD ! [J] Accumulated absolute energy exchange for selected cells.
        integer(kind=JPIB) :: events = 0_JPIB ! [-] Number of nonzero cellwise differences recorded.
        integer(kind=JPIM) :: maximum_cell = 0_JPIM ! [-] One-based index of the largest difference; zero before any event.
        integer(kind=JPIM) :: affected_cells = 0_JPIM ! [-] Count of cells with at least one event; avoids rescanning the mask for logging.
        logical, allocatable :: affected(:) ! [-] Cells with at least one nonzero difference during this run.
    end type HeatResidualStats
    type HeatConservationStats
        logical :: initialized = .FALSE. ! [-] Whether the initial represented energy has been captured.
        real(kind=JPRD) :: initial_j = 0.0_JPRD ! [J] Represented domain energy at the start of this run.
        ! Process order: water boundaries, ice boundaries, then local heat input.
        real(kind=JPRD) :: net_j(3) = 0.0_JPRD ! [J] Kahan sums of net external input by process.
        real(kind=JPRD) :: absolute_j(3) = 0.0_JPRD ! [J] Kahan sums of absolute external exchange by process.
        real(kind=JPRD) :: net_correction_j(3) = 0.0_JPRD ! [J] Kahan roundoff corrections for net input sums.
        real(kind=JPRD) :: absolute_correction_j(3) = 0.0_JPRD ! [J] Kahan roundoff corrections for absolute exchange sums.
        integer(kind=JPIB) :: steps(3) = 0_JPIB ! [-] Number of recorded exchanges for each process.
    end type HeatConservationStats
contains
subroutine record_heat_exchange(stats, process, net_j, absolute_j)
    type(HeatConservationStats), intent(inout) :: stats
    integer(kind=JPIM), intent(in) :: process
    real(kind=JPRD), intent(in) :: net_j, absolute_j
    call compensated_add(stats%net_j(process), stats%net_correction_j(process), net_j)
    call compensated_add(stats%absolute_j(process), stats%absolute_correction_j(process), absolute_j)
    stats%steps(process) = stats%steps(process) + 1_JPIB
end subroutine

subroutine compensated_add(total, correction, value)
    real(kind=JPRD), intent(inout) :: total, correction
    real(kind=JPRD), intent(in) :: value
    real(kind=JPRD) :: adjusted, updated
    adjusted = value - correction
    updated = total + adjusted
    correction = (updated - total) - adjusted
    total = updated
end subroutine

subroutine write_heat_conservation(unit, stats, current_j, unapplied_j)
    integer(kind=JPIM), intent(in) :: unit
    type(HeatConservationStats), intent(in) :: stats
    real(kind=JPRD), intent(in) :: current_j, unapplied_j
    real(kind=JPRD) :: raw_j, adjusted_j, scale_j
    raw_j = (current_j - stats%initial_j) - sum(stats%net_j)
    adjusted_j = raw_j + unapplied_j
    scale_j = max(sum(stats%absolute_j), 1.0_JPRD)
    write(unit,'(a)') '[cumulative heat budget]'
    write(unit,'(a,es24.16,a,es24.16)') '  storage [J]: initial = ',stats%initial_j,'; current = ',current_j
    write(unit,'(a,es24.16,2(a,es24.16))') '  net input [J]: water = ',stats%net_j(1), &
    &   '; ice = ',stats%net_j(2),'; local = ',stats%net_j(3)
    write(unit,'(a,es24.16,2(a,es24.16))') '  absolute exchange [J]: water = ',stats%absolute_j(1), &
    &   '; ice = ',stats%absolute_j(2),'; local = ',stats%absolute_j(3)
    write(unit,'(a,es24.16)') '  unapplied [J]: signed = ',unapplied_j
    write(unit,'(a,es24.16,a,es24.16)') '  residual [J]: raw = ',raw_j,'; adjusted = ',adjusted_j
    write(unit,'(a,es24.16,a,es24.16)') '  ratios [-]: raw/exchange = ',raw_j/scale_j, &
    &   '; adjusted/exchange = ',adjusted_j/scale_j
    write(unit,'(a,i0,a,i0,a,i0)') '  exchanges: water = ',stats%steps(1),'; ice = ',stats%steps(2),'; local = ',stats%steps(3)

end subroutine

subroutine record_heat_residual(stats, residual_j, throughput_j, mask)
    type(HeatResidualStats), intent(inout) :: stats
    real(kind=JPRD), intent(in) :: residual_j(:) ! [J] Expected minus represented energy.
    real(kind=JPRD), intent(in) :: throughput_j(:) ! [J] Nonnegative absolute energy input/output scale.
    logical, intent(in), optional :: mask(:)
    integer, parameter :: BLOCK_SIZE = 4096 ! [cells] Fixed auxiliary reduction block size, independent of thread count.
    integer(kind=JPIM) :: i, block, new_cells, maximum_cell
    integer(kind=JPIB) :: events
    real(kind=JPRD) :: positive, negative, throughput, maximum
    real(kind=JPRD) :: sums(4,(size(residual_j)+BLOCK_SIZE-1)/BLOCK_SIZE)
    integer(kind=JPIB) :: counts(3,(size(residual_j)+BLOCK_SIZE-1)/BLOCK_SIZE)
    if (.not. allocated(stats%affected)) allocate(stats%affected(size(residual_j)), source=.FALSE.)
    ! Each block owns a disjoint slice of affected. Merge scalar totals in fixed cell order.
    !$omp parallel do if(size(residual_j) >= 32768) schedule(static) &
    !$omp private(i,positive,negative,throughput,maximum,new_cells,maximum_cell,events)
    do block = 1,size(sums,2)
        positive = 0.0_JPRD
        negative = 0.0_JPRD
        throughput = 0.0_JPRD
        maximum = 0.0_JPRD
        new_cells = 0
        maximum_cell = 0
        events = 0_JPIB
        do i = (block-1)*BLOCK_SIZE+1,min(block*BLOCK_SIZE,size(residual_j))
            if (present(mask)) then
                if (.not. mask(i)) cycle
            endif
            throughput = throughput + throughput_j(i)
            positive = positive + max(residual_j(i),0.0_JPRD)
            negative = negative + min(residual_j(i),0.0_JPRD)
            if (residual_j(i) /= 0.0_JPRD) then
                events = events + 1_JPIB
                if (.not. stats%affected(i)) new_cells = new_cells + 1
                stats%affected(i) = .TRUE.
            endif
            if (abs(residual_j(i)) > maximum) then
                maximum = abs(residual_j(i))
                maximum_cell = i
            endif
        enddo
        sums(:,block) = [positive,negative,throughput,maximum]
        counts(:,block) = [events,int(new_cells,JPIB),int(maximum_cell,JPIB)]
    enddo
    !$omp end parallel do
    do block = 1,size(sums,2)
        stats%positive_j = stats%positive_j + sums(1,block)
        stats%negative_j = stats%negative_j + sums(2,block)
        stats%throughput_j = stats%throughput_j + sums(3,block)
        stats%events = stats%events + counts(1,block)
        stats%affected_cells = stats%affected_cells + int(counts(2,block),JPIM)
        if (sums(4,block) > stats%maximum_absolute_j) then
            stats%maximum_absolute_j = sums(4,block)
            stats%maximum_cell = int(counts(3,block),JPIM)
        endif
    enddo
end subroutine record_heat_residual

subroutine write_heat_residual(unit, reason, stats, prefix)
    integer(kind=JPIM), intent(in) :: unit
    character(len=*), intent(in) :: reason
    type(HeatResidualStats), intent(in) :: stats
    character(len=*), intent(in), optional :: prefix
    character(len=24) :: category
    integer(kind=JPIM) :: cells
    category = 'unapplied heat'
    if (present(prefix)) category = prefix
    cells = stats%affected_cells
    write(unit,'(5a)') '  ',trim(category),' since run start: ',trim(reason),':'
    write(unit,'(a,es24.16,2(a,es24.16))') '    energy [J]: signed = ',stats%positive_j + stats%negative_j, &
    &   '; positive = ',stats%positive_j,'; negative = ',stats%negative_j
    write(unit,'(a,es24.16,a,es24.16)') '    magnitude [J]: absolute total = ',stats%positive_j - stats%negative_j, &
    &   '; cell maximum = ',stats%maximum_absolute_j
    write(unit,'(a,i0,a,i0,a,i0)') '    events = ',stats%events,'; affected cells = ',cells,'; maximum cell = ',stats%maximum_cell
    write(unit,'(a,es24.16)') '    absolute throughput [J] = ',stats%throughput_j

end subroutine write_heat_residual
end module heat_residual_mod
