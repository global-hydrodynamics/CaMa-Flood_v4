module numeric_utils_mod
    use PARKIND1, only: JPRB, JPRM
    implicit none
    private

    ! =========================
    ! Default tolerances
    ! =========================
    ! You can overwrite these from outside via set_default_tolerances().
    real(kind=JPRB), save :: default_abs_tol_r8 = 1.0e-12_JPRB
    real(kind=JPRB), save :: default_rel_tol_r8 = 1.0e-12_JPRB
    real(kind=JPRM), save :: default_abs_tol_r4 = 1.0e-6_JPRM
    real(kind=JPRM), save :: default_rel_tol_r4 = 1.0e-6_JPRM

    public :: set_default_tolerances
    public :: nearly_equal, nearly_zero, safe_divide, clamp

    interface nearly_equal
        module procedure nearly_equal_r8
        module procedure nearly_equal_r4
    end interface nearly_equal

    interface nearly_zero
        module procedure nearly_zero_r8
        module procedure nearly_zero_r4
    end interface nearly_zero

    interface safe_divide
        module procedure safe_divide_r8
        module procedure safe_divide_r4
    end interface safe_divide

    interface clamp
        module procedure clamp_r8
        module procedure clamp_r4
    end interface clamp

contains

!===========================================================
! Set module-level default tolerances
!===========================================================
subroutine set_default_tolerances(abs_tol_r8, rel_tol_r8, abs_tol_r4, rel_tol_r4)
    real(kind=JPRB), intent(in), optional :: abs_tol_r8, rel_tol_r8
    real(kind=JPRM), intent(in), optional :: abs_tol_r4, rel_tol_r4

    if (present(abs_tol_r8)) default_abs_tol_r8 = abs_tol_r8
    if (present(rel_tol_r8)) default_rel_tol_r8 = rel_tol_r8
    if (present(abs_tol_r4)) default_abs_tol_r4 = abs_tol_r4
    if (present(rel_tol_r4)) default_rel_tol_r4 = rel_tol_r4
end subroutine set_default_tolerances


!===========================================================
! nearly_equal
! |a-b| <= max(atol, rtol * max(1, |a|, |b|))
!===========================================================
pure logical function nearly_equal_r4(a, b) result(eq)
    real(kind=JPRM), intent(in) :: a, b
    real(kind=JPRM) :: diff, scale, tol

    diff  = abs(a - b)
    scale = max(1.0_JPRM, abs(a), abs(b))
    tol   = max(default_abs_tol_r4, default_rel_tol_r4 * scale)

    eq = (diff <= tol)
end function nearly_equal_r4

pure logical function nearly_equal_r8(a, b) result(eq)
    real(kind=JPRB), intent(in) :: a, b
    real(kind=JPRB) :: diff, scale, tol

    diff  = abs(a - b)
    scale = max(1.0_JPRB, abs(a), abs(b))
    tol   = max(default_abs_tol_r8, default_rel_tol_r8 * scale)

    eq = (diff <= tol)
end function nearly_equal_r8

!===========================================================
! nearly_zero: |x| <= tol (default: abs_tol)
!===========================================================
pure logical function nearly_zero_r8(x, tol) result(ok)
    real(kind=JPRB), intent(in) :: x
    real(kind=JPRB), intent(in), optional :: tol

    real(kind=JPRB) :: t
    t = default_abs_tol_r8
    if (present(tol)) t = tol
    ok = abs(x) <= t
end function nearly_zero_r8

pure logical function nearly_zero_r4(x, tol) result(ok)
    real(kind=JPRM), intent(in) :: x
    real(kind=JPRM), intent(in), optional :: tol

    real(kind=JPRM) :: t
    t = default_abs_tol_r4
    if (present(tol)) t = tol
    ok = abs(x) <= t
end function nearly_zero_r4


!===========================================================
! safe_divide: returns a/b if b is not (nearly) zero
! else returns default_value (default: 0)
!===========================================================
pure real(kind=JPRB) function safe_divide_r8(a, b, default_value, tol) result(res)
    real(kind=JPRB), intent(in) :: a, b
    real(kind=JPRB), intent(in), optional :: default_value, tol

    real(kind=JPRB) :: dv, t
    dv = 0.0_JPRB
    if (present(default_value)) dv = default_value

    t = default_abs_tol_r8
    if (present(tol)) t = tol

    if (abs(b) <= t) then
        res = dv
    else
        res = a / b
    end if
end function safe_divide_r8

pure real(kind=JPRM) function safe_divide_r4(a, b, default_value, tol) result(res)
    real(kind=JPRM), intent(in) :: a, b
    real(kind=JPRM), intent(in), optional :: default_value, tol

    real(kind=JPRM) :: dv, t
    dv = 0.0_JPRM
    if (present(default_value)) dv = default_value

    t = default_abs_tol_r4
    if (present(tol)) t = tol

    if (abs(b) <= t) then
        res = dv
    else
        res = a / b
    end if
end function safe_divide_r4


!===========================================================
! clamp: restrict x into [xmin, xmax]
!===========================================================
pure real(kind=JPRB) function clamp_r8(x, xmin, xmax) result(y)
    real(kind=JPRB), intent(in) :: x, xmin, xmax
    y = min(max(x, xmin), xmax)
end function clamp_r8

pure real(kind=JPRM) function clamp_r4(x, xmin, xmax) result(y)
    real(kind=JPRM), intent(in) :: x, xmin, xmax
    y = min(max(x, xmin), xmax)
end function clamp_r4

end module numeric_utils_mod