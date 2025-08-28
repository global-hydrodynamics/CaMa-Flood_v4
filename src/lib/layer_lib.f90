module layer_lib
    use LU, &
    &   only: solve_matrix
    use funit_lib, only: &
    &   LOG_UNIT
    implicit none
    ! Updating from N to M layers, the maximum number of the "packets" is N - M + 1

    type LayerDistributer
        integer :: &
        &   oldlyr, & ! layer number (before update)
        &   newlyr    ! layer number (after  update)
        real(8) :: &
        &   volume ! [m3] water volume

        contains

        procedure :: init => LayerDistributer_init
        procedure :: reset => LayerDistributer_init
    end type LayerDistributer


    type LayerUpdater
        integer :: &
        &   size, & ! size of registerd LayerDistributer
        &   maxsize ! size of lyrdists
        type(LayerDistributer), allocatable :: &
        &   lyrdists(:)
        real(8), allocatable :: &
        &   oldsto(:), &
        &   min_oldlyr(:), max_oldlyr(:) ! from the previous one

        contains

        procedure :: init     => LayerUpdater_init
        procedure :: reset    => LayerUpdater_reset
        procedure :: add      => LayerUpdater_add
        procedure :: write    => LayerUpdater_write
        procedure :: register => LayerUpdater_register
        procedure :: update   => LayerUpdater_update
        procedure :: reset_update => LayerUpdater_reset_update
    end type LayerUpdater

contains

subroutine LayerDistributer_init( &
    &   self)
    class(LayerDistributer), intent(inout) :: &
    &   self
    self%oldlyr = 0
    self%newlyr = 0
    self%volume = 0.d0
end subroutine LayerDistributer_init

! ===================================================================================================
subroutine LayerUpdater_init( &
&   self, &
&   nlyr)
    class(LayerUpdater), intent(inout) :: &
    &   self
    integer, intent(in) :: &
    &   nlyr ! max No. of layers
    self%maxsize = 2 * nlyr + 1
    allocate(self%lyrdists(self%maxsize))

    allocate(self%oldsto(nlyr))
    allocate(self%min_oldlyr(nlyr))
    allocate(self%max_oldlyr(nlyr))
    call self%reset()
end subroutine LayerUpdater_init


subroutine LayerUpdater_reset( &
&   self)
    class(LayerUpdater), intent(inout) :: &
    &   self
    integer :: ilyr
    self%size = 0
    self%oldsto(:) = 0.d0
    do ilyr = 1, self%maxsize
        call self%lyrdists(ilyr)%reset()
    enddo
    call self%reset_update()
end subroutine LayerUpdater_reset


subroutine LayerUpdater_reset_update( &
    &   self)
        class(LayerUpdater), intent(inout) :: &
        &   self
    self%min_oldlyr(:) = 1.d20
    self%max_oldlyr(:) = -1.d20
end subroutine LayerUpdater_reset_update


subroutine LayerUpdater_add( &
&   self, &
&   oldlyr, newlyr, volume)
    class(LayerUpdater), intent(inout) :: &
    &   self
    integer, intent(in) :: &
    &   oldlyr, &
    &   newlyr
    real(8), intent(in) :: &
    &   volume
    self%size = self%size + 1
    self%lyrdists(self%size)%oldlyr = oldlyr
    self%lyrdists(self%size)%newlyr = newlyr
    self%lyrdists(self%size)%volume = volume
end subroutine LayerUpdater_add


subroutine LayerUpdater_write( &
&   self)
    class(LayerUpdater), intent(in) :: &
    &   self
    integer :: isize
    do isize = 1, self%size
        write(LOG_UNIT, '(i2,a,i2,a,f0.1)') self%lyrdists(isize)%oldlyr, '->', self%lyrdists(isize)%newlyr, ': ', self%lyrdists(isize)%volume
    enddo
end subroutine LayerUpdater_write


subroutine LayerUpdater_register( &
&   self, &
&   oldsto, newsto)
    class(LayerUpdater), intent(inout) :: &
    &   self
    real(8), intent(in) :: &
    &   oldsto(:), & ! [m3] storage of each layer (old)
    &   newsto(:)    ! [m3] \\                    (new)
    integer :: &
    &   ioldlyr, inewlyr, noldlyr, nnewlyr
    real(8) :: &
    &   oldsto_rst, newsto_rst

    call self%reset()
    self%oldsto(:) = oldsto(:)
    noldlyr = size(oldsto)
    nnewlyr = size(newsto)

    inewlyr = 1
    newsto_rst = newsto(inewlyr)
    loop1: do ioldlyr = 1, noldlyr
        if (oldsto(ioldlyr) <= 0.d0) cycle
        oldsto_rst = oldsto(ioldlyr)

        loop2: do
            if (newsto_rst <= oldsto_rst) then
                call self%add(ioldlyr, inewlyr, newsto_rst)
                oldsto_rst = max(oldsto_rst - newsto_rst, 0.d0)
                inewlyr = find_next_new_layer(inewlyr)
                if (inewlyr > nnewlyr) exit loop1
                newsto_rst = newsto(inewlyr)
            else
                newsto_rst = max(newsto_rst - oldsto_rst, 0.d0)
                call self%add(ioldlyr, inewlyr, oldsto_rst)
                exit loop2
            endif
        enddo loop2
    enddo loop1
    !call validate_updater

    contains

    integer function find_next_new_layer(inewlyr_pre)
        integer, intent(in) :: &
        &   inewlyr_pre
        integer :: &
        &   ilyr
        do ilyr = inewlyr_pre + 1, nnewlyr
            if (newsto(ilyr) > 0.d0) then
                find_next_new_layer = ilyr
                return
            endif
        enddo
        find_next_new_layer = nnewlyr + 1
    end function find_next_new_layer

#ifdef OPT_DEBUG
    subroutine validate_updater
        real(8), allocatable :: &
        &   oldsto_tmp(:), newsto_tmp(:)
        integer :: &
        &   i, olyr, nlyr
        real(8) :: &
        &   vol
        allocate(oldsto_tmp(size(oldsto)), source=0.d0)
        allocate(newsto_tmp(size(newsto)), source=0.d0)
        do i = 1, self%size
            olyr = self%lyrdists(i)%oldlyr
            nlyr = self%lyrdists(i)%newlyr
            vol = self%lyrdists(i)%volume
            oldsto_tmp(olyr) = oldsto_tmp(olyr) + vol
            newsto_tmp(nlyr) = newsto_tmp(nlyr) + vol
        enddo
        write(LOG_UNIT, *) 'old'
        do i = 1, size(oldsto)
            write(LOG_UNIT, *) oldsto(i), oldsto_tmp(i), oldsto(i) - oldsto_tmp(i)
        enddo
        write(LOG_UNIT, *) 'new'
        do i = 1, size(newsto)
            write(LOG_UNIT, *) newsto(i), newsto_tmp(i), newsto(i) - newsto_tmp(i)
        enddo
        write(LOG_UNIT, *) 'old', sum(oldsto(:)), sum(oldsto_tmp(:))
        write(LOG_UNIT, *) 'new', sum(newsto(:)), sum(newsto_tmp(:))
    end subroutine validate_updater
#endif
end subroutine LayerUpdater_register


subroutine LayerUpdater_update( &
&   self, oldlyr, newsto, &
&   newlyr)
    class(LayerUpdater), intent(inout) :: &
    &   self
    real(8), intent(in) :: &
    &   oldlyr(:), & ! [X] solute concentration of each layer (before layer update)
    &   newsto(:)    ! [m3] water storage of each layer (after layer update)
    real(8), intent(out) :: &
    &   newlyr(:)    ! [X] solute concentration of each layer (after layer update)
    real(8) :: vol
    integer :: &
    &   i, olyr, nlyr, & ! layer index for old/newlyr
    &   max_nlyr

    newlyr(:) = 0.d0
    call self%reset_update()
    max_nlyr = 0

    !write(LOG_UNIT, *) 'step1'
    do i = 1, self%size
        olyr = self%lyrdists(i)%oldlyr
        nlyr = self%lyrdists(i)%newlyr
        vol  = self%lyrdists(i)%volume
        newlyr(nlyr) = newlyr(nlyr) + oldlyr(olyr) * vol
        self%min_oldlyr(nlyr) = min(oldlyr(olyr), self%min_oldlyr(nlyr))
        self%max_oldlyr(nlyr) = max(oldlyr(olyr), self%max_oldlyr(nlyr))
        max_nlyr = max(max_nlyr, nlyr)
        !write(LOG_UNIT, *) olyr, nlyr, vol, oldlyr(olyr)
    enddo

    !write(LOG_UNIT, *) 'step2'
    do nlyr = 1, size(newsto)
        if (newsto(nlyr) > 0.d0) then
            newlyr(nlyr) = newlyr(nlyr) / newsto(nlyr)
        endif
        !write(LOG_UNIT, *) nlyr, newsto(nlyr), newlyr(nlyr)
        newlyr(nlyr) = max(newlyr(nlyr), self%min_oldlyr(nlyr))
        newlyr(nlyr) = min(newlyr(nlyr), self%max_oldlyr(nlyr))
    enddo
    !write(LOG_UNIT, *) 'step3'
    if (max_nlyr > 0) then
        do nlyr = max_nlyr + 1, size(newlyr)
            newlyr(nlyr) = newlyr(max_nlyr)
        enddo
    endif
    !write(LOG_UNIT, *) dot_product(self%oldsto(:), oldlyr(:))
    !write(LOG_UNIT, *) dot_product(newsto(:), newlyr(:))
end subroutine LayerUpdater_update

! ===================================================================================================
subroutine divlyr(lyr, ttllyr, maxlyr)
    ! divide ttllyr into lyr(:) according to maxlyr(:)
    ! applicable to both of storage & thickness
    real(8), intent(out) :: lyr(:)
    real(8), intent(in)  :: ttllyr, maxlyr(:)
    real(8) ttlrst
    integer ilyr, nlyr
    lyr(:) = 0.d0
    if (ttllyr == 0.d0) return
    nlyr   = size(maxlyr)
    ttlrst = ttllyr
    do ilyr = 1, nlyr
        if (ttlrst <= maxlyr(ilyr)) then
            lyr(ilyr) = ttlrst
            ttlrst    = 0.d0
            exit
        else
            lyr(ilyr) = maxlyr(ilyr)
            ttlrst    = ttlrst - lyr(ilyr)
        endif
    enddo
    lyr(nlyr+1) = ttlrst
end subroutine divlyr


function cntlyr(lyrval) result(nlyr)
    ! count layer number
    integer nlyr
    real(8), intent(in) :: lyrval(:)
    integer ilyr
    if (sum(lyrval) == 0.d0) then
        nlyr = 0
        return
    endif
    nlyr = size(lyrval)
    do ilyr = 1, size(lyrval)
        if (lyrval(ilyr) == 0.d0) then
            nlyr = ilyr - 1
            exit
        endif
    enddo
end function cntlyr


subroutine corlyr(lyrval, nlyr, minlyr)
    ! if last layer is less than minlyr, add it into previous layer
    real(8), intent(inout) :: lyrval(:)
    integer, intent(inout) :: nlyr
    real(8), intent(in)    :: minlyr
    if ( nlyr < 2 ) return
    if ( lyrval(nlyr) < minlyr ) then
        lyrval(nlyr-1) = lyrval(nlyr-1) + lyrval(nlyr)
        lyrval(nlyr)   = 0.d0
        nlyr = nlyr - 1
    endif
end subroutine corlyr


subroutine sftlyr(lyrval)
    ! shift values in layers with skip val=0
    real(8), intent(inout) :: lyrval(:)
    real(8), allocatable   :: lyrtmp(:)
    integer ilyr, nlyr, tmp
    nlyr = size(lyrval)
    allocate(lyrtmp(nlyr)); lyrtmp(:) = 0.d0
    tmp = 0
    do ilyr = 1, nlyr
        if (lyrval(ilyr) > 0.d0) then
            tmp = tmp + 1
            lyrtmp(tmp) = lyrval(ilyr)
        endif
    enddo
    lyrval(:) = lyrtmp(:)
    deallocate(lyrtmp)
end subroutine sftlyr

! ===================================================================================================
! Water reduction
! ===================================================================================================
subroutine rdclyr_upper(lyrval, rdclyr, rdcval)
    ! reduce layer from upper
    real(8), intent(inout) :: lyrval(:) ! [X e.g. m3] layer
    real(8), intent(out)   :: rdclyr(:) ! [X e.g. m3] reduction of each layer
    real(8), intent(in)    :: rdcval    ! [X e.g. m3] total reduction
    real(8) rstrdc
    integer ilyr, nlyr
    nlyr   = size(lyrval)
    rdclyr(:) = 0.d0
    rstrdc = rdcval
    do ilyr = 1, nlyr
        if (rstrdc < lyrval(ilyr)) then
            rdclyr(ilyr) = rstrdc
            lyrval(ilyr) = lyrval(ilyr) - rstrdc
            exit
        else
            rstrdc = rstrdc - lyrval(ilyr)
            rdclyr(ilyr) = lyrval(ilyr)
            lyrval(ilyr) = 0.d0
        endif
    enddo
end subroutine rdclyr_upper


subroutine rdclyr_ratio
end subroutine rdclyr_ratio

! ===================================================================================================
! Outflow
! ===================================================================================================
function ttlout(lyrmat, lyrout) result(matout)
    real(8) :: matout ! [X*m3/s or X*m3] outflow from all layers
    real(8), intent(in)  :: &
    &   lyrmat(:), & ! [X] material of each layer
    &   lyrout(:)    ! [m3/s or m3] outflow  from each layer
    integer ilyr
    matout = 0.d0
    do ilyr = 1, size(lyrout)
!write(LOG_UNIT, *) ilyr, lyrmat(ilyr), lyrout(ilyr)
        if (lyrout(ilyr) > 0.d0) then
            matout = matout + lyrmat(ilyr) * lyrout(ilyr)
        endif
    enddo
end function ttlout


function get_higher_layer(lyrelv, trgelv) result(uprlyr)
    ! get layer index higher than target elevation
    ! if bottom elev. of the layer <= target elev. <= top elev., return that layer
    ! if top layer is lower than target elev., return 0
    integer             :: uprlyr
    real(8), intent(in) :: lyrelv(:), trgelv
    integer ilyr, nlyr
    if (lyrelv(1) < trgelv) then
        uprlyr = 0
        return
    endif
    nlyr = size(lyrelv)
    do ilyr = 1, nlyr-1
        if (lyrelv(ilyr+1) <= trgelv) then
            uprlyr = ilyr
            return
        endif
    enddo
    uprlyr = nlyr
end function get_higher_layer

! ===================================================================================================
! Eddy mixing
! ===================================================================================================
subroutine eddmix( lyrx, lyrthk, veredd, setvel, dt )
    real(8), intent(inout) :: lyrx(:) ! [X] material concentration
    real(8), intent(in)    :: &
    &   lyrthk(:), & ! [m] layer thickness
    &   veredd(:), & ! [?] vertical eddy coefficient, flux = veredd * dMat
    &   setvel,    & ! [m/s] settling velocity
    &   dt           ! [s]   time step width
    if ( setvel > 0.d0 ) then
        call eddmix_settle__  ( lyrx, lyrthk, veredd, setvel, dt )
    else
        call eddmix_nosettle__( lyrx, lyrthk, veredd, dt )
    endif
end subroutine eddmix

subroutine eddmix_nosettle__(lyrx, lyrthk, veredd, dt)
    real(8), intent(inout) :: lyrx(:) ! [X] material concentration
    real(8), intent(in)    :: &
    &   lyrthk(:), & ! [m] layer thickness
    &   veredd(:), & ! [?] vertical eddy coefficient, flux = veredd * dMat
    &   dt           ! [s]   time step width
    real(8), allocatable :: A(:,:), b(:), lyrdx(:) ! [A][lyrdx] = [b]
    integer :: ilyr, nlyr

    nlyr = size(lyrx)
    if ( nlyr == 1 ) return

    allocate( A(nlyr,nlyr) ); A(:,:) = 0.d0
    allocate( b(nlyr)      ); b(:)   = 0.d0
    do ilyr = 1, nlyr
        A(ilyr,ilyr) = 1.d0
    enddo
    call add_eddmix_nosettle
    allocate( lyrdx(nlyr) ); lyrdx(:) = 0.d0
    call solve_matrix(lyrdx(:), A(:,:), b(:), nlyr)
    deallocate( A, b )
    lyrx(:) = lyrx(:) + lyrdx(:)
    deallocate( lyrdx )

    contains

    subroutine add_eddmix_nosettle
        real(8) :: cup, cdn
        integer :: ilyr
        do ilyr = 1, nlyr-1 ! b/w ilyr and ilyr+1
            cdn = veredd(ilyr) * dt / lyrthk(ilyr)
            A(ilyr,ilyr  ) = A(ilyr  ,ilyr) + cdn
            A(ilyr,ilyr+1) = A(ilyr,ilyr+1) - cdn
            b(ilyr) = b(ilyr) + cdn * (lyrx(ilyr+1) - lyrx(ilyr))
        enddo
        do ilyr = 2, nlyr ! b/w ilyr and ilyr-1
            cup = veredd(ilyr-1) * dt / lyrthk(ilyr)
            A(ilyr,ilyr  ) = A(ilyr,ilyr  ) + cup
            A(ilyr,ilyr-1) = A(ilyr,ilyr-1) - cup
            b(ilyr) = b(ilyr) - cup * (lyrx(ilyr) - lyrx(ilyr-1))
        enddo
    end subroutine add_eddmix_nosettle

end subroutine eddmix_nosettle__


subroutine eddmix_settle__(lyrx, lyrthk, veredd, setvel, dt)
    real(8), intent(inout) :: lyrx(:) ! [X] material concentration
    real(8), intent(in)    :: &
    &   lyrthk(:), & ! [m] layer thickness
    &   veredd(:), & ! [?] vertical eddy coefficient, flux = veredd * dMat
    &   setvel,    & ! [m/s] settling velocity
    &   dt           ! [s]   time step width
    real(8), allocatable :: A(:,:), b(:)
    integer :: ilyr, nlyr

    nlyr = size(lyrx)
    if ( nlyr == 1 ) return

    allocate( A(nlyr,nlyr) ); A(:,:) = 0.d0
    allocate( b(nlyr)      ); b(:)   = 0.d0
    do ilyr = 1, nlyr
        A(ilyr,ilyr) = 1.d0
    enddo
    b(:) = lyrx(:)
    call add_eddmix_settle
!    allocate( lyrdx(nlyr) ); lyrdx(:) = 0.d0
!    call solve_matrix(lyrdx(:), A(:,:), b(:), nlyr)
    call solve_matrix(lyrx(:), A(:,:), b(:), nlyr)
    deallocate(A, b)
!    lyrx(:) = lyrx(:) + lyrdx(:)
!    deallocate(lyrdx)

    contains

    subroutine add_eddmix_settle
        real(8) :: pe, fpe, dz, k, c, edd, set ! fpe = 1/exp(pe)-1
        real(8) :: pe_max = 46.d0 ! exp(pe) = 10**20
        integer :: ilyr
        do ilyr = 1, nlyr-1 ! b/w ilyr and ilyr+1
            if ( abs(veredd(ilyr)) <= 1.d-20 ) then
                fpe = 0.d0
            else
                pe  = max( -10.d0, min( setvel * lyrthk(ilyr) / veredd(ilyr), pe_max ) )
    !            fpe = max( 0.d0, (1.d0 - 0.1d0 * abs(pe) ) ** 5 / pe )
                fpe = exp(pe) - 1.d0
                if ( fpe /= 0.d0 ) then
                    fpe = 1.d0 / fpe
                else
                    fpe = 0.d0
                endif
            endif
            dz  = ( lyrthk(ilyr) + lyrthk(ilyr+1) ) * 0.5d0
            k   = setvel * dt * ( 1.d0 + fpe ) / dz
!            k   = setvel * dt * fpe / dz
            A(ilyr,ilyr  ) = A(ilyr  ,ilyr) + k
            A(ilyr,ilyr+1) = A(ilyr,ilyr+1) - k

!            c   = setvel * dt / lyrthk(ilyr)
!            edd =  c * fpe * ( lyrx(ilyr+1) - lyrx(ilyr) )
!            set = -c * lyrx(ilyr)
!            b(ilyr) = b(ilyr) + edd + set
        enddo
        do ilyr = 2, nlyr ! b/w ilyr and ilyr-1
            if ( abs(veredd(ilyr-1)) <= 1.d-20 ) then
                fpe = 0.d0
            else
                pe  = max( -10.d0, min( setvel * lyrthk(ilyr) / veredd(ilyr-1), pe_max ) )
    !            fpe = max( 0.d0, (1.d0 - 0.1d0 * abs(pe) ) ** 5 / pe )
                fpe = exp(pe) - 1.d0
                if ( fpe /= 0.d0 ) then
                    fpe = 1.d0 / fpe
                else
                    fpe = 0.d0
                endif
            endif
            dz  = ( lyrthk(ilyr-1) + lyrthk(ilyr) ) * 0.5d0
            k   = setvel * dt * fpe / dz
!            k   = setvel * dt * ( 1.d0 + fpe ) / dz
            A(ilyr,ilyr  ) = A(ilyr,ilyr  ) + k
            A(ilyr,ilyr-1) = A(ilyr,ilyr-1) - k

!            c   = setvel * dt / lyrthk(ilyr)
!            edd = -c * fpe * ( lyrx(ilyr) - lyrx(ilyr-1) )
!            set =  c * lyrx(ilyr-1)
!            b(ilyr) = b(ilyr) + edd + set
        enddo
    end subroutine add_eddmix_settle

end subroutine eddmix_settle__

end module layer_lib
