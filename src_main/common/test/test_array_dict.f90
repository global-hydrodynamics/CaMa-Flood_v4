program test_arraydict
    use PARKIND1, only: JPRB
    use array_dict_class, only: ArrayDict
    implicit none

    type(ArrayDict) :: d

    real(kind=JPRB), allocatable :: x(:), y(:), z(:)
    real(kind=JPRB), allocatable :: a(:,:), b(:,:), c(:,:)
    integer, allocatable :: shape(:)
    real(kind=JPRB) :: err

    call d%init(2)

    !-------------------------
    ! Test 1: set/get 1D copy
    !-------------------------
    allocate(x(5), y(5), z(5))
    x = (/ 1._JPRB, 2._JPRB, 3._JPRB, 4._JPRB, 5._JPRB /)

    call d%set_1d('x', x)

    y = -999._JPRB
    call d%get_1d('x', y)

    err = maxval(abs(y - x))
    call assert_close(err, 0._JPRB, 'Test1: get_1d returns identical values')

    ! コピー確認：元配列 x を変えても dict は変わらない
    x = 0._JPRB
    z = -777._JPRB
    call d%get_1d('x', z)
    err = maxval(abs(z - (/ 1._JPRB, 2._JPRB, 3._JPRB, 4._JPRB, 5._JPRB /)))
    call assert_close(err, 0._JPRB, 'Test1b: dict stores a copy (independent of x)')

    !-------------------------
    ! Test 2: set/get 2D copy
    !-------------------------
    allocate(a(2,3), b(2,3), c(2,3))
    a = reshape((/ 1._JPRB, 2._JPRB, 3._JPRB, 4._JPRB, 5._JPRB, 6._JPRB /), (/2,3/))

    call d%set_2d('A', a)

    b = -999._JPRB
    call d%get_2d('A', b)

    err = maxval(abs(b - a))
    call assert_close(err, 0._JPRB, 'Test2: get_2d returns identical values')

    ! コピー確認
    a = 0._JPRB
    c = -777._JPRB
    call d%get_2d('A', c)
    err = maxval(abs(c - reshape((/ 1._JPRB, 2._JPRB, 3._JPRB, 4._JPRB, 5._JPRB, 6._JPRB /), (/2,3/))))
    call assert_close(err, 0._JPRB, 'Test2b: dict stores a copy (independent of a)')

    !-------------------------
    ! Test 3: metadata access
    !-------------------------
    call d%get_shape('x', shape)
    call assert_int_equal(shape(1), 5, 'Test3: shape_1d')

    call d%get_shape('A', shape)
    call assert_int_equal(shape(1), 2, 'Test3b: shape_2d dim1')
    call assert_int_equal(shape(2), 3, 'Test3c: shape_2d dim2')
    call assert_int_equal(d%get_rank('x'), 1, 'Test3d: rank(x)=1')
    call assert_int_equal(d%get_rank('A'), 2, 'Test3e: rank(A)=2')

    call assert_int_equal(d%get_size('x'), 5, 'Test3f: size(x)=5')
    call assert_int_equal(d%get_size('A'), 6, 'Test3g: size(A)=6')

    if (.not. d%has_key('x')) call fail('Test3h: has_key(x) should be true')
    if (d%has_key('missing')) call fail('Test3i: has_key(missing) should be false')

    !-------------------------
    ! Test 4: update existing key with same shape (no reallocate)
    !-------------------------
    x = (/ 10._JPRB, 20._JPRB, 30._JPRB, 40._JPRB, 50._JPRB /)
    call d%set_1d('x', x)
    y = 0._JPRB
    call d%get_1d('x', y)
    err = maxval(abs(y - x))
    call assert_close(err, 0._JPRB, 'Test4: overwrite existing key keeps shape and updates values')

    write(*,*) 'All non-fatal tests passed.'
    write(*,*) 'NOTE: fatal tests (shape mismatch / missing key) are commented out below.'

    !-------------------------
    ! Fatal tests (uncomment to confirm stop behavior)
    !-------------------------
    ! 1) missing key -> stop
    ! call d%get_1d('no_such_key', y)

    ! 2) shape mismatch on get -> stop
    ! deallocate(y); allocate(y(4))
    ! call d%get_1d('x', y)

    ! 3) shape mismatch on set existing -> stop
    ! deallocate(x); allocate(x(4)); x = 1._JPRB
    ! call d%set_1d('x', x)

    !call d%finalize()

contains

    subroutine assert_close(val, ref, msg)
        real(kind=JPRB), intent(in) :: val, ref
        character(len=*), intent(in) :: msg
        real(kind=JPRB), parameter :: tol = 0._JPRB
        if (abs(val - ref) > tol) then
            write(*,*) '[FAIL] ', trim(msg)
            write(*,*) '  val=', val, ' ref=', ref
            stop 1
        else
            write(*,*) '[OK]   ', trim(msg)
        end if
    end subroutine assert_close

    subroutine assert_int_equal(val, ref, msg)
        integer, intent(in) :: val, ref
        character(len=*), intent(in) :: msg
        if (val /= ref) then
            write(*,*) '[FAIL] ', trim(msg)
            write(*,*) '  val=', val, ' ref=', ref
            stop 1
        else
            write(*,*) '[OK]   ', trim(msg)
        end if
    end subroutine assert_int_equal

    subroutine fail(msg)
        character(len=*), intent(in) :: msg
        write(*,*) '[FAIL] ', trim(msg)
        stop 1
    end subroutine fail

end program test_arraydict