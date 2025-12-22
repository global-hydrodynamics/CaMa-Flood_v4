program test_ranked_array
    use PARKIND1,         only: JPRB
    use ranked_array_class, only: RankedArray, append_ranked_arrays, RK_1D, RK_2D
    implicit none

    call test_set_get_1d()
    call test_set_get_2d()
    call test_append_values_and_shapes()
    call test_append_mixed_ranks_independence()

    write(*,*) '[ALL TESTS PASSED] test_ranked_array'
contains

    subroutine fail(msg)
        character(len=*), intent(in) :: msg
        write(*,*) '[TEST FAILED] ', trim(msg)
        stop 1
    end subroutine fail

    subroutine assert_i_eq(a, b, msg)
        integer, intent(in) :: a, b
        character(len=*), intent(in) :: msg
        if (a /= b) then
            write(*,*) '[TEST FAILED] ', trim(msg), ' a=', a, ' b=', b
            stop 1
        end if
    end subroutine assert_i_eq

    subroutine assert_allclose_1d(a, b, tol, msg)
        real(kind=JPRB), intent(in) :: a(:), b(:)
        real(kind=JPRB), intent(in) :: tol
        character(len=*), intent(in) :: msg
        integer :: i

        if (size(a) /= size(b)) call fail(msg // ' (size mismatch)')
        do i = 1, size(a)
            if (abs(a(i) - b(i)) > tol) then
                write(*,*) '[TEST FAILED] ', trim(msg), ' i=', i, ' a=', a(i), ' b=', b(i)
                stop 1
            end if
        end do
    end subroutine assert_allclose_1d

    subroutine assert_allclose_2d(a, b, tol, msg)
        real(kind=JPRB), intent(in) :: a(:,:), b(:,:)
        real(kind=JPRB), intent(in) :: tol
        character(len=*), intent(in) :: msg
        integer :: i, j

        if (size(a,1) /= size(b,1) .or. size(a,2) /= size(b,2)) call fail(msg // ' (shape mismatch)')
        do j = 1, size(a,2)
            do i = 1, size(a,1)
                if (abs(a(i,j) - b(i,j)) > tol) then
                    write(*,*) '[TEST FAILED] ', trim(msg), ' (i,j)=', i, j, ' a=', a(i,j), ' b=', b(i,j)
                    stop 1
                end if
            end do
        end do
    end subroutine assert_allclose_2d

    subroutine test_set_get_1d()
        type(RankedArray) :: ra
        real(kind=JPRB) :: a_in(5), a_out(5)
        integer :: rk

        a_in = (/ 1.0_JPRB, 2.0_JPRB, 3.0_JPRB, 4.0_JPRB, 5.0_JPRB /)
        call ra%set(a_in)

        rk = ra%get_rank()
        call assert_i_eq(rk, RK_1D, 'rank after set_1d')

        a_out = 0.0_JPRB
        call ra%get(a_out)
        call assert_allclose_1d(a_out, a_in, 0.0_JPRB, 'set/get 1d values')
    end subroutine test_set_get_1d

    subroutine test_set_get_2d()
        type(RankedArray) :: ra
        real(kind=JPRB) :: a_in(3,4), a_out(3,4)
        integer :: rk

        a_in = reshape((/ &
            1.0_JPRB,  2.0_JPRB,  3.0_JPRB, &
            4.0_JPRB,  5.0_JPRB,  6.0_JPRB, &
            7.0_JPRB,  8.0_JPRB,  9.0_JPRB, &
            10.0_JPRB, 11.0_JPRB, 12.0_JPRB  /), (/3,4/))

        call ra%set(a_in)

        rk = ra%get_rank()
        call assert_i_eq(rk, RK_2D, 'rank after set_2d')

        a_out = 0.0_JPRB
        call ra%get(a_out)
        call assert_allclose_2d(a_out, a_in, 0.0_JPRB, 'set/get 2d values')
    end subroutine test_set_get_2d

    ! 追加要求1:
    ! ・具体的な値・形状を入れた後で append して、それらが一致するか
    subroutine test_append_values_and_shapes()
        type(RankedArray), allocatable :: arrs(:)
        real(kind=JPRB) :: a1(5), out1(5)
        real(kind=JPRB) :: a2(3,4), out2(3,4)
        integer :: sh1(1), sh2(2)

        a1 = (/ 1.0_JPRB, 2.0_JPRB, 3.0_JPRB, 4.0_JPRB, 5.0_JPRB /)
        a2 = reshape((/ &
            1.0_JPRB,  2.0_JPRB,  3.0_JPRB, &
            4.0_JPRB,  5.0_JPRB,  6.0_JPRB, &
            7.0_JPRB,  8.0_JPRB,  9.0_JPRB, &
            10.0_JPRB, 11.0_JPRB, 12.0_JPRB  /), (/3,4/))

        if (allocated(arrs)) deallocate(arrs)

        call append_ranked_arrays(arrs, a1)
        call append_ranked_arrays(arrs, a2)

        call assert_i_eq(size(arrs), 2, 'append count')

        call assert_i_eq(arrs(1)%get_rank(), RK_1D, 'arrs(1) rank')
        call assert_i_eq(arrs(2)%get_rank(), RK_2D, 'arrs(2) rank')

        call arrs(1)%get_shape(sh1)
        call assert_i_eq(sh1(1), 5, 'arrs(1) shape(1)')

        call arrs(2)%get_shape(sh2)
        call assert_i_eq(sh2(1), 3, 'arrs(2) shape(1)')
        call assert_i_eq(sh2(2), 4, 'arrs(2) shape(2)')

        out1 = 0.0_JPRB
        out2 = 0.0_JPRB
        call arrs(1)%get(out1)
        call arrs(2)%get(out2)
        call assert_allclose_1d(out1, a1, 0.0_JPRB, 'arrs(1) values after append')
        call assert_allclose_2d(out2, a2, 0.0_JPRB, 'arrs(2) values after append')
    end subroutine test_append_values_and_shapes

    ! 追加要求2:
    ! ・異なる次元の RankedArray が append でき、互いに影響を与えないこと
    subroutine test_append_mixed_ranks_independence()
        type(RankedArray), allocatable :: arrs(:)
        real(kind=JPRB) :: a1(3), out1(3)
        real(kind=JPRB) :: a2(2,2), out2(2,2)

        if (allocated(arrs)) deallocate(arrs)

        a1 = (/ -1.0_JPRB, -2.0_JPRB, -3.0_JPRB /)
        a2 = reshape((/ 1.0_JPRB, 2.0_JPRB, 3.0_JPRB, 4.0_JPRB /), (/2,2/))

        call append_ranked_arrays(arrs, a1)
        call append_ranked_arrays(arrs, a2)

        ! arrs(1) を更新しても arrs(2) が変わらない
        a1 = (/ 10.0_JPRB, 20.0_JPRB, 30.0_JPRB /)
        call arrs(1)%set(a1)

        out2 = 0.0_JPRB
        call arrs(2)%get(out2)
        call assert_allclose_2d(out2, reshape((/ 1.0_JPRB,2.0_JPRB,3.0_JPRB,4.0_JPRB /), (/2,2/)), &
                                0.0_JPRB, 'arrs(2) unchanged after arrs(1) set')

        ! arrs(2) を更新しても arrs(1) が変わらない
        a2 = reshape((/ -10.0_JPRB,-20.0_JPRB,-30.0_JPRB,-40.0_JPRB /), (/2,2/))
        call arrs(2)%set(a2)

        out1 = 0.0_JPRB
        call arrs(1)%get(out1)
        call assert_allclose_1d(out1, (/ 10.0_JPRB, 20.0_JPRB, 30.0_JPRB /), 0.0_JPRB, &
                                'arrs(1) unchanged after arrs(2) set')
    end subroutine test_append_mixed_ranks_independence

end program test_ranked_array