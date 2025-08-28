module const_lib
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: ieee_arithmetic
    implicit none
    private
    public :: &
    &   NaN_R4, NaN_R8, is_nan

    real(8), parameter :: &
    &   NaN_R4 = transfer(-1_8, 0.0), &
    &   NaN_R8 = transfer(-1_8, 0.d0)

    interface is_nan
        module procedure is_nan_r4
        module procedure is_nan_r8
    end interface is_nan

contains

logical function is_nan_r4(val)
    real(4), intent(in) :: &
    &   val
    is_nan_r4 = isNaN(val)
end function is_nan_r4

logical function is_nan_r8(val)
    real(8), intent(in) :: &
    &   val
    is_nan_r8 = isNaN(val)
end function is_nan_r8

end module const_lib