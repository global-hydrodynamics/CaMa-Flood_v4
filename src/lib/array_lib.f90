module array_lib
implicit none
private
public :: &
&   append, trim_array, allocate_array, find_index, arange, &
&   sum_dif_with_rest, heapsort, bisect_left

interface append
!    module procedure append_c32
    module procedure :: append_c
    module procedure :: append_integer
    module procedure :: append_double
end interface append

interface trim_array
    module procedure :: trim_array_i4
    module procedure :: trim_array_r8
end interface trim_array

interface allocate_array
    module procedure :: allocate_array_1d_dble
end interface allocate_array
! if not found, return 0
interface find_index
    module procedure :: find_index_c
!    module procedure find_index_i
    module procedure :: find_index_i_alloc
    module procedure :: find_index_d_alloc
end interface find_index

interface arange
    module procedure :: arange_dble
end interface arange

contains

! ===================================================================================================
! append
subroutine append_c(a, val)
    character(len=*), allocatable, intent(inout) :: a(:)
    character(len=*), intent(in)  :: val
    character(len=:), allocatable :: tmp(:)
    integer n
    if (.not. allocated(a)) then
        allocate(a(1)); a(1) = val
        return
    endif
    n = size(a)
    allocate(character(len(a(1))) :: tmp(n))
    tmp(:) = a(:)
    deallocate(a); allocate(a(n+1))
    a(1:n) = tmp(:)
    a(n+1) = val
    deallocate(tmp)
end subroutine append_c


!subroutine append_c32(a, val)
!    character(len=*) , allocatable, intent(inout) :: a(:)
!    character(len=*) , intent(in)  :: val
!    character(len=32), allocatable :: tmp(:)
!    integer n
!    if (.not. allocated(a)) then
!        allocate(a(1)); a(1) = val
!        return
!    endif
!    n = size(a)
!    allocate(tmp, source=a)
!    if (allocated(a)) deallocate(a)
!    allocate(a(n+1))
!    a(1:n) = tmp(:)
!    a(n+1) = val
!    deallocate(tmp)
!end subroutine append_c32


subroutine append_integer(a, val)
    integer, allocatable, intent(inout) :: a(:)
    integer, intent(in)  :: val
    integer, allocatable :: tmp(:)
    integer n
    if (.not. allocated(a)) then
        allocate(a(1)); a(1) = val
        return
    endif
    n = size(a)
    allocate(tmp, source=a)
    if (allocated(a)) deallocate(a)
    allocate(a(n+1))
    a(1:n) = tmp(:)
    a(n+1) = val
    deallocate(tmp)
end subroutine append_integer

subroutine append_double(a, val)
    double precision, allocatable, intent(inout) :: a(:)
    double precision, intent(in)  :: val
    double precision, allocatable :: tmp(:)
    integer n
    if (.not. allocated(a)) then
        allocate(a(1)); a(1) = val
        return
    endif
    n = size(a)
    allocate(tmp, source=a)
    if (allocated(a)) deallocate(a)
    allocate(a(n+1))
    a(1:n) = tmp(:)
    a(n+1) = val
    deallocate(tmp)
end subroutine append_double

! ===================================================================================================
! trim_array
! ===================================================================================================
subroutine trim_array_i4( & ! ***
&   arr, n)
    integer, allocatable, intent(inout) :: & ! ***
    &   arr(:)
    integer, intent(in) :: &
    &   n
    integer, allocatable :: & ! ***
    &   tmp(:)
    allocate(tmp, source=arr(:n))
    deallocate(arr)
    allocate(arr, source=tmp)
    deallocate(tmp)
end subroutine trim_array_i4 ! ***

subroutine trim_array_r8( & ! ***
&   arr, n)
    real(8), allocatable, intent(inout) :: & ! ***
    &   arr(:)
    integer, intent(in) :: &
    &   n
    real(8), allocatable :: & ! ***
    &   tmp(:)
    allocate(tmp, source=arr(:n))
    deallocate(arr)
    allocate(arr, source=tmp)
    deallocate(tmp)
end subroutine trim_array_r8 ! ***

! ===================================================================================================
! find_index
! If array is not allocated, idx = 0
! ===================================================================================================
function find_index_c(array, target) result(idx)
    character(len=*), allocatable, intent(in) :: array(:)
    character(len=*),              intent(in) :: target
    integer           idx
    logical(kind=4) isFound
    isFound = .FALSE.
    do idx = 1, size(array)
        if (trim(array(idx)) == trim(target)) then
            isFound = .TRUE.
            exit
        endif
    enddo
    if (.not. isFound) then
        idx = 0
    endif
end function find_index_c

function find_index_i_alloc(array, target) result(idx)
    integer, allocatable, intent(in) :: array(:)
    ! without "allocatable", error when array is not allocated
    integer,              intent(in) :: target
    integer                idx
    logical(kind=4)      isFound
    isFound = .FALSE.
    do idx = 1, size(array)
        if (array(idx) == target) then
            isFound = .TRUE.
            exit
        endif
    enddo
    if (.not. isFound) then
        idx = 0
    endif
end function find_index_i_alloc

function find_index_d_alloc(array, target) result(idx)
    real(8), allocatable, intent(in) :: array(:)
    ! without "allocatable", error when array is not allocated
    real(8),              intent(in) :: target
    integer                idx
    do idx = 1, size(array)
        if (array(idx) == target) return
    enddo
    idx = 0
end function find_index_d_alloc
!function find_index_i(array, target) result(idx)
!    integer, intent(in) :: array(:)
!    integer, intent(in) :: target
!    integer                idx
!    logical(kind=4)      isFound
!    isFound = .FALSE.
!    do idx = 1, size(array)
!        if (array(idx) == target) then
!            isFound = .TRUE.
!            exit
!        endif
!    enddo
!    if (.not. isFound) then
!        idx = 0
!    endif
!end function find_index_i

! ===================================================================================================
subroutine allocate_array_1d_dble(out_array, nsize)
    double precision, allocatable, intent(out) :: out_array(:)
    integer, intent(in) :: nsize
    if (allocated(out_array) .and. size(out_array) /= nsize) then
        deallocate(out_array)
    endif
    if (.not. allocated(out_array)) then
        allocate(out_array(nsize))
    endif
    out_array(:) = 0.d0
end subroutine allocate_array_1d_dble
! ===================================================================================================
subroutine sum_dif_with_rest(sumErr, aVec, bVec)
    double precision, intent(out) :: sumErr
    double precision, intent(in)  :: aVec(:), bVec(:)
    double precision seqErr, rstErr, tmpErr
    integer          iseq

    sumErr = 0.d0
    rstErr = 0.d0
!    !$omp parallel do private(iseq, seqErr, rstErr, tmpErr) reduction(+:sumErr)
    do iseq = 1, size(aVec)
        seqErr = aVec(iseq) - bVec(iseq)
        rstErr = rstErr + seqErr
        tmpErr = sumErr
        sumErr = sumErr + rstErr
        tmpErr = tmpErr - sumErr
        rstErr = rstErr + tmpErr
    enddo
!    !$omp end parallel do
end subroutine sum_dif_with_rest

! ===================================================================================================
subroutine heapsort(array) ! smaller->bigger
    ! double precision, intent(inout) :: array(:)
    integer, intent(inout) :: array(:)
    integer i,k,j,l, n
    ! double precision :: t
    integer t

    n = size(array)

    if(n.le.0)then
        write(6,*)"Error, at heapsort"; stop
    endif
    if(n.eq.1)return

    l=n/2+1
    k=n
    do while(k.ne.1)
        if(l.gt.1)then
            l=l-1
            t=array(L)
        else
            t=array(k)
            array(k)=array(1)
            k=k-1
            if(k.eq.1) then
                array(1)=t
                exit
            endif
        endif
        i=l
        j=l+l
        do while(j.le.k)
            if(j.lt.k)then
                if(array(j).lt.array(j+1))j=j+1
            endif
            if (t.lt.array(j))then
                array(i)=array(j)
                i=j
                j=j+j
            else
                j=k+1
            endif
        enddo
        array(i)=t
    enddo
    return
end subroutine heapsort

! ===================================================================================================
function arange_dble(start, end, step) result(arr)
    ! step = 0 returns ZeroDivisionError
    real(8), intent(in) :: &
    &   start, end, step
    real(8), allocatable :: &
    &   arr(:)
    integer :: &
    &   i, n
    if ((start < end .and. step < 0) .or. (start > end .and. step > 0)) then
        allocate(arr(0), source=0.d0)
        return
    endif

    n = int(abs(end - start) / abs(step))
    if (start <= end .and. start + step * dble(n) < end) then
        n = n + 1
    elseif (start > end .and. start + step * dble(n) > end) then
        n = n + 1
    endif
    allocate(arr(n), source=0.d0)
    do i = 1, n
        arr(i) = start + step * dble(i - 1)
    enddo
end function arange_dble

! ===================================================================================================
function bisect_left(arr, target) result(i)
    ! return i if arr(i-1) < target <= arr(i)
    ! min = 1, max = size(target) + 1 if arr(-1) < target
    real(8), intent(in) :: &
    &   arr(:), & ! should be sorted from smaller to bigger
    &   target
    integer :: &
    &   i

    do i = 1, size(arr)
        if (target <= arr(i)) return
    enddo
end function bisect_left

end module array_lib

