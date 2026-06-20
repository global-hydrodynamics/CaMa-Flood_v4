module channel_mod
    use PARKIND1,  only: &
    &   JPRB
    use phys_const_mod, only: &
    &   GRAVITY_ACCEL
    implicit none

contains

function calc_channel_fricvel( watdph, watvel, manning ) result( us )
    ! Hino, p.147-148
    real(kind=JPRB), intent(in) :: watdph, & ! [m] water depth
    &                      watvel, & ! [m/s] water velocity
    &                      manning   ! [s/m**1/3] manning coefficient
    real(kind=JPRB)             :: us ! [m/s] friction velocity
    real(kind=JPRB) :: fp
    fp = 2.d0 * GRAVITY_ACCEL * manning * manning / ( watdph ** (1.0/3.0) )
    us = sqrt( 0.5d0 * fp ) * watvel
end function calc_channel_fricvel
end module channel_mod
