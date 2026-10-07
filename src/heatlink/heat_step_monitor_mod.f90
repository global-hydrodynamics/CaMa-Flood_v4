module heat_step_monitor_mod
    use PARKIND1, only: JPIM, JPIB, JPRD
    implicit none
    private
    public :: HeatStepState, HeatStepLedger, HeatStepStats, capture_heat_step, measure_heat_step
    public :: add_step_heat, step_sum, monitor_heat_step, write_heat_step_extrema

    ! Independent diagnostics: snapshots and interval sums never update model state.
    type HeatStepState
        real(kind=JPRD), allocatable :: volume(:) ! [m3] Cellwise liquid volume at interval start.
        real(kind=JPRD), allocatable :: theta(:) ! [K] Cellwise liquid temperature relative to TMELT at interval start.
        real(kind=JPRD), allocatable :: ice(:) ! [m3] Mobile surface-ice volume at interval start; optional.
        real(kind=JPRD), allocatable :: excess(:) ! [m3] Immobile excess-ice volume at interval start; optional.
    end type
    type HeatStepLedger
        real(kind=JPRD) :: value(4) = 0.0_JPRD ! [J] Interval sums: net input, absolute exchange, signed and absolute unapplied heat.
        real(kind=JPRD) :: correction(4) = 0.0_JPRD ! [J] Neumaier corrections for the corresponding interval sums.
    end type
    integer, parameter :: SUM_BLOCK_SIZE = 4096 ! [cells] Fixed reduction block size, independent of OpenMP thread count.
    integer, parameter :: PARALLEL_MIN_SIZE = 32768 ! [cells] Minimum domain size for parallel diagnostic reductions.
    integer, parameter :: NMETRIC = 8 ! [-] Number of independently monitored extrema metrics.
    integer, parameter :: NFIELD = 16 ! [-] Number of real-valued fields in a diagnostic record.
    integer, parameter :: metric_field(NMETRIC) = [8,9,6,7,11,12,13,14] ! [-] One-based record field index for each metric.
    character(len=24), parameter :: metric_name(NMETRIC) = [character(len=24) :: & ! [-] Log labels in metric_field order.
    &   'raw [J]','adjusted [J]','unapplied [J]','absolute unapplied [J]', &
    &   'raw/exchange [-]','adjusted/exchange [-]','unapplied/exchange [-]','adjusted/storage [-]']
    ! Record fields 1:2 are elapsed time/duration [s]; 3:10 and 15:16 are energy [J]; 11:14 are ratios [-].
    type HeatStepStats
        integer(kind=JPIB) :: steps = 0_JPIB ! [-] Number of completed intervals for this stage.
        integer(kind=JPIB) :: samples(NMETRIC) = 0_JPIB ! [-] Valid sample count for each metric, excluding undefined ratios.
        integer(kind=JPIB) :: no_exchange_ratio = 0_JPIB ! [-] Intervals with undefined external-exchange ratios.
        integer(kind=JPIB) :: no_storage_ratio = 0_JPIB ! [-] Intervals with an undefined storage-scale ratio.
        integer(kind=JPIB) :: min_step(NMETRIC) = 0_JPIB ! [-] Interval index of each metric's minimum.
        integer(kind=JPIB) :: max_step(NMETRIC) = 0_JPIB ! [-] Interval index of each metric's maximum.
        real(kind=JPRD) :: min_record(NFIELD,NMETRIC) = 0.0_JPRD ! [s,J,-] Full record at each minimum; field units are listed above.
        real(kind=JPRD) :: max_record(NFIELD,NMETRIC) = 0.0_JPRD ! [s,J,-] Full record at each maximum; field units are listed above.
    end type
contains

! Neumaier summation also handles a small partial sum followed by a larger term.
subroutine add_compensated(total, correction, value)
    real(kind=JPRD), intent(inout) :: total, correction
    real(kind=JPRD), intent(in) :: value
    real(kind=JPRD) :: updated
    updated = total + value
    if (abs(total) >= abs(value)) then
        correction = correction + ((total - updated) + value)
    else
        correction = correction + ((value - updated) + total)
    endif
    total = updated
end subroutine

function step_sum(values) result(total)
    real(kind=JPRD), intent(in) :: values(:)
    real(kind=JPRD) :: total, correction
    real(kind=JPRD) :: partial(2,(size(values)+SUM_BLOCK_SIZE-1)/SUM_BLOCK_SIZE)
    integer :: i, block
    ! Keep both the leading sum and correction until the deterministic final merge.
    !$omp parallel do if(size(values) >= PARALLEL_MIN_SIZE) schedule(static) private(i,total,correction)
    do block = 1, size(partial,2)
        total = 0.0_JPRD
        correction = 0.0_JPRD
        do i = (block-1)*SUM_BLOCK_SIZE+1, min(block*SUM_BLOCK_SIZE,size(values))
            call add_compensated(total, correction, values(i))
        enddo
        partial(:,block) = [total,correction]
    enddo
    !$omp end parallel do
    total = 0.0_JPRD
    correction = 0.0_JPRD
    do block = 1, size(partial,2)
        call add_compensated(total,correction,partial(1,block))
        call add_compensated(total,correction,partial(2,block))
    enddo
    total = total + correction
end function

subroutine add_step_heat(ledger, net_j, exchange_j, unapplied_j, absolute_unapplied_j)
    type(HeatStepLedger), intent(inout) :: ledger
    real(kind=JPRD), intent(in) :: net_j, exchange_j, unapplied_j, absolute_unapplied_j
    real(kind=JPRD) :: values(4)
    integer :: i
    values = [net_j,exchange_j,unapplied_j,absolute_unapplied_j]
    do i = 1, 4
        call add_compensated(ledger%value(i),ledger%correction(i),values(i))
    enddo
end subroutine

subroutine capture_heat_step(state, volume, theta, ice, excess)
    type(HeatStepState), intent(inout) :: state
    real(kind=JPRD), intent(in) :: volume(:), theta(:)
    real(kind=JPRD), intent(in), optional :: ice(:), excess(:)
    state%volume = volume
    state%theta = theta
    if (present(ice)) state%ice = ice
    if (present(excess)) state%excess = excess
end subroutine

subroutine measure_heat_step(state, volume, theta, capacity, latent, delta_j, storage_j, naive_delta_j, ice, excess)
    type(HeatStepState), intent(in) :: state
    real(kind=JPRD), intent(in) :: volume(:), theta(:), capacity, latent
    real(kind=JPRD), intent(in), optional :: ice(:), excess(:)
    real(kind=JPRD), intent(out) :: delta_j, storage_j, naive_delta_j
    real(kind=JPRD) :: correction, scale_correction, initial_j, final_j
    real(kind=JPRD) :: delta, storage, old_water, new_water, old_ice, new_ice, old_excess, new_excess
    real(kind=JPRD) :: partial(10,(size(volume)+SUM_BLOCK_SIZE-1)/SUM_BLOCK_SIZE)
    integer :: i, block
    ! Fixed blocks avoid a thread-count-dependent OpenMP reduction tree. No physical state is modified.
    !$omp parallel do if(size(volume) >= PARALLEL_MIN_SIZE) schedule(static) &
    !$omp private(i,delta,storage,correction,scale_correction,old_water,new_water,old_ice,new_ice,old_excess,new_excess)
    do block = 1, size(partial,2)
        delta = 0.0_JPRD
        storage = 0.0_JPRD
        correction = 0.0_JPRD
        scale_correction = 0.0_JPRD
        old_water = 0.0_JPRD
        new_water = 0.0_JPRD
        old_ice = 0.0_JPRD
        new_ice = 0.0_JPRD
        old_excess = 0.0_JPRD
        new_excess = 0.0_JPRD
        do i = (block-1)*SUM_BLOCK_SIZE+1, min(block*SUM_BLOCK_SIZE,size(volume))
            old_water = old_water + state%volume(i)*state%theta(i)
            new_water = new_water + volume(i)*theta(i)
            ! Difference of products without first constructing two large cell energies.
            call add_compensated(delta,correction,capacity * state%volume(i) * (theta(i)-state%theta(i)))
            call add_compensated(delta,correction,capacity * (volume(i)-state%volume(i)) * theta(i))
            call add_compensated(storage,scale_correction,capacity * &
            &   max(abs(state%volume(i)*state%theta(i)),abs(volume(i)*theta(i))))
            if (present(ice)) then
                old_ice = old_ice + state%ice(i)
                new_ice = new_ice + ice(i)
                call add_compensated(delta,correction,latent*(state%ice(i)-ice(i)))
                call add_compensated(storage,scale_correction,latent*max(abs(state%ice(i)),abs(ice(i))))
            endif
            if (present(excess)) then
                old_excess = old_excess + state%excess(i)
                new_excess = new_excess + excess(i)
                call add_compensated(delta,correction,latent*(state%excess(i)-excess(i)))
                call add_compensated(storage,scale_correction,latent*max(abs(state%excess(i)),abs(excess(i))))
            endif
        enddo
        partial(:,block) = [delta,correction,storage,scale_correction,old_water,new_water,old_ice,new_ice,old_excess,new_excess]
    enddo
    !$omp end parallel do
    delta_j = 0.0_JPRD
    storage_j = 0.0_JPRD
    correction = 0.0_JPRD
    scale_correction = 0.0_JPRD
    do block = 1, size(partial,2)
        call add_compensated(delta_j,correction,partial(1,block))
        call add_compensated(delta_j,correction,partial(2,block))
        call add_compensated(storage_j,scale_correction,partial(3,block))
        call add_compensated(storage_j,scale_correction,partial(4,block))
    enddo
    delta_j = delta_j + correction
    storage_j = storage_j + scale_correction
    ! Retain the large-total subtraction only as a comparison, never as the main monitor.
    ! Fuse this auxiliary comparison into the same traversal. These are deliberately
    ! uncompensated domain totals; block grouping can change their roundoff.
    initial_j = capacity*sum(partial(5,:))
    final_j = capacity*sum(partial(6,:))
    if (present(ice)) then
        initial_j = initial_j - latent*sum(partial(7,:))
        final_j = final_j - latent*sum(partial(8,:))
    endif
    if (present(excess)) then
        initial_j = initial_j - latent*sum(partial(9,:))
        final_j = final_j - latent*sum(partial(10,:))
    endif
    naive_delta_j = final_j-initial_j
end subroutine

subroutine monitor_heat_step(stats, unit, stage, end_seconds, dt_seconds, delta_j, storage_j, naive_delta_j, ledger)
    type(HeatStepStats), intent(inout) :: stats
    integer, intent(in) :: unit
    character(len=*), intent(in) :: stage
    real(kind=JPRD), intent(in) :: end_seconds, dt_seconds, delta_j, storage_j, naive_delta_j
    type(HeatStepLedger), intent(in) :: ledger
    real(kind=JPRD) :: q(4), v(NFIELD)
    logical :: valid(NMETRIC), exchange_valid, storage_valid
    integer :: i, k
    q = ledger%value + ledger%correction
    v = 0.0_JPRD
    v(1:7) = [end_seconds,dt_seconds,delta_j,q]
    v(8) = step_sum([delta_j,-q(1)])
    v(9) = step_sum([delta_j,-q(1),q(3)])
    v(10) = storage_j
    ! Zero denominators have no ratio. No arbitrary 1 J floor or silent division by zero.
    exchange_valid = q(2) > 0.0_JPRD
    if (exchange_valid .and. q(2) < 1.0_JPRD) &
    &   exchange_valid = max(abs(v(8)),abs(v(9)),abs(q(4))) <= huge(1.0_JPRD)*q(2)
    storage_valid = storage_j > 0.0_JPRD
    if (storage_valid .and. storage_j < 1.0_JPRD) &
    &   storage_valid = abs(v(9)) <= huge(1.0_JPRD)*storage_j
    if (exchange_valid) v(11:13) = [v(8),v(9),q(4)]/q(2)
    if (storage_valid) v(14) = v(9)/storage_j
    v(15) = naive_delta_j
    v(16) = step_sum([naive_delta_j,-q(1),q(3)])
    stats%steps = stats%steps + 1_JPIB
    if (.not. exchange_valid) stats%no_exchange_ratio = stats%no_exchange_ratio + 1_JPIB
    if (.not. storage_valid) stats%no_storage_ratio = stats%no_storage_ratio + 1_JPIB
    valid = .TRUE.
    valid(5:7) = exchange_valid
    valid(8) = storage_valid
    do i = 1, NMETRIC
        if (.not. valid(i)) cycle
        k = metric_field(i)
        if (stats%samples(i) == 0 .or. v(k) < stats%min_record(k,i)) then
            stats%min_record(:,i) = v
            stats%min_step(i) = stats%steps
        endif
        if (stats%samples(i) == 0 .or. v(k) > stats%max_record(k,i)) then
            stats%max_record(:,i) = v
            stats%max_step(i) = stats%steps
        endif
        stats%samples(i) = stats%samples(i) + 1_JPIB
    enddo
    call write_stage(unit,stage)
    call write_step_record(unit,'  ',stats%steps,v,exchange_valid,storage_valid)
end subroutine

subroutine write_stage(unit,stage)
    integer, intent(in) :: unit
    character(len=*), intent(in) :: stage
    select case(stage)
    case('local')
        write(unit,'(a)') '[local heat budget]'
    case('hour')
        write(unit,'(a)') '[combined step]'
    case default
        write(unit,'(3a)') '[',trim(stage),']'
    end select
end subroutine

subroutine write_step_record(unit,indent,step,v,exchange_valid,storage_valid)
    integer, intent(in) :: unit
    character(len=*), intent(in) :: indent
    integer(kind=JPIB), intent(in) :: step
    real(kind=JPRD), intent(in) :: v(NFIELD)
    logical, intent(in) :: exchange_valid,storage_valid
    write(unit,'(2a,i0,2(a,es24.16))') indent,'interval = ',step,'; end [s] = ',v(1),'; duration [s] = ',v(2)
    write(unit,'(4a,es24.16,2(a,es24.16))') indent,'energy [J]: ', &
    &   'change',' = ',v(3),'; net input = ',v(4),'; absolute exchange = ',v(5)
    write(unit,'(2a,es24.16,a,es24.16)') indent,'unapplied [J]: signed = ',v(6),'; absolute = ',v(7)
    write(unit,'(2a,es24.16,a,es24.16)') indent,'residual [J]: raw = ',v(8),'; adjusted = ',v(9)
    write(unit,'(2a,es24.16)') indent,'storage scale [J] = ',v(10)
    if (exchange_valid) then
        write(unit,'(2a,es24.16,2(a,es24.16))') indent,'ratios [-]: raw/exchange = ',v(11), &
        &   '; adjusted/exchange = ',v(12),'; unapplied/exchange = ',v(13)
    else
        write(unit,'(2a)') indent,'ratios [-]: raw/exchange = undefined; adjusted/exchange = undefined; unapplied/exchange = undefined'
    endif
    if (storage_valid) then
        write(unit,'(2a,es24.16)') indent,'ratio [-]: adjusted/storage = ',v(14)
    else
        write(unit,'(2a)') indent,'ratio [-]: adjusted/storage = undefined'
    endif
    write(unit,'(2a,es24.16,a,es24.16)') indent,'large-total comparison [J]: change = ',v(15),'; adjusted = ',v(16)
end subroutine

subroutine write_heat_step_extrema(unit, stage, stats)
    integer, intent(in) :: unit
    character(len=*), intent(in) :: stage
    type(HeatStepStats), intent(in) :: stats
    integer :: i,j
    logical :: exchange_valid,storage_valid
    real(kind=JPRD) :: v(NFIELD)
    integer(kind=JPIB) :: step
    call write_stage(unit,stage)
    write(unit,'(a)') '  extrema over this run (domain interval budgets)'
    write(unit,'(a,i0,a,i0,a,i0)') '  intervals = ',stats%steps, &
    &   '; undefined exchange ratios = ',stats%no_exchange_ratio,'; undefined storage ratios = ',stats%no_storage_ratio
    do i = 1, NMETRIC
        if (stats%samples(i) == 0) cycle
        do j = 1,2
            if (j == 1) then
                write(unit,'(3a)') '  ',trim(metric_name(i)),' minimum:'
                v = stats%min_record(:,i)
                step = stats%min_step(i)
            else
                write(unit,'(3a)') '  ',trim(metric_name(i)),' maximum:'
                v = stats%max_record(:,i)
                step = stats%max_step(i)
            endif
            exchange_valid = v(5) > 0.0_JPRD
            if (exchange_valid .and. v(5) < 1.0_JPRD) &
            &   exchange_valid = max(abs(v(8)),abs(v(9)),abs(v(7))) <= huge(1.0_JPRD)*v(5)
            storage_valid = v(10) > 0.0_JPRD
            if (storage_valid .and. v(10) < 1.0_JPRD) storage_valid = abs(v(9)) <= huge(1.0_JPRD)*v(10)
            call write_step_record(unit,'    ',step,v,exchange_valid,storage_valid)
        enddo
    enddo
end subroutine
end module heat_step_monitor_mod
