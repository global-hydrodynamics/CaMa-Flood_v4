module mapframe_mod
    implicit none
    private
    public :: &
    &   MapFrame, operator(==), init_mapframe

type MapFrame
    real(8) :: &
    &   left, right, top, bottom
    integer :: &
    &   nx, ny

contains

    procedure :: west => west
    procedure :: east => east
    procedure :: north => north
    procedure :: south => south
    procedure :: shape => shape
    procedure :: set_shape => set_shape
    procedure :: domain => domain
    procedure :: set_domain => set_domain
    procedure :: is_n2s => is_n2s
    procedure :: is_w2e => is_w2e
    procedure :: str => str

end type MapFrame

!interface MapFrame
!    module procedure init_mapframe
!end interface MapFrame

interface operator (==)
    module procedure eq
end interface operator (==)

contains

! ===================================================================================================
! Constructor
! ===================================================================================================
function init_mapframe(left, right, top, bottom, nx, ny) result(obj)
    type(MapFrame) :: &
    &   obj
    real(8), intent(in), optional :: &
    &   left, right, top, bottom
    integer, intent(in), optional :: &
    &   nx, ny
    obj%left = left
    obj%right = right
    obj%top = top
    obj%bottom = bottom
    obj%nx    = nx
    obj%ny    = ny
end function init_mapframe

! ===================================================================================================
! Getter/Setter
! ===================================================================================================
subroutine domain(self, left, right, top, bottom)
    class(MapFrame),  intent(in) :: &
    &   self
    real(8), intent(out) :: &
    &   left, right, top, bottom
    left = self%left
    right = self%right
    top = self%top
    bottom = self%bottom
end subroutine domain

real(8) function west(self)
    class(MapFrame), intent(in) :: &
    &   self
    west = min(self%left, self%right)
end function west

real(8) function east(self)
    class(MapFrame), intent(in) :: &
    &   self
    east = max(self%left, self%right)
end function east

real(8) function north(self)
    class(MapFrame), intent(in) :: &
    &   self
    north = min(self%top, self%bottom)
end function north

real(8) function south(self)
    class(MapFrame), intent(in) :: &
    &   self
    south = max(self%top, self%bottom)
end function south

logical function is_n2s(self)
    class(MapFrame), intent(in) :: &
    &   self
    is_n2s = self%top > self%bottom
end function is_n2s

logical function is_w2e(self)
    class(MapFrame), intent(in) :: &
    &   self
    is_w2e = self%left < self%right
end function is_w2e

subroutine set_domain(self, left, right, top, bottom)
    class(MapFrame), intent(inout) :: &
    &   self
    real(8), intent(in) :: &
    &   left, right, top, bottom
    self%left = left
    self%right = right
    self%top = top
    self%bottom = bottom
end subroutine set_domain

subroutine shape(self, nx, ny)
    class(MapFrame), intent(in) :: &
    &   self
    integer, intent(out) :: &
    &   nx, ny
    nx = self%nx
    ny = self%ny
end subroutine shape

subroutine set_shape(self, nx, ny)
    class(MapFrame), intent(inout) :: &
    &   self
    integer, intent(in) :: &
    &   nx, ny
    self%nx = nx
    self%ny = ny
end subroutine set_shape

! ===================================================================================================
character(len=256) function str(self)
    class(MapFrame), intent(in) :: &
    &   self
    real(8) :: &
    &   rl, rr, rt, rb
    integer :: &
    &   ix, iy
    character(len=16) :: &
    &   cl, cr, ct, cb, cx, cy
    call self%domain(rl, rr, rt, rb)
    write(cl, '(f0.2)') rl
    write(cr, '(f0.2)') rr
    write(ct, '(f0.2)') rt
    write(cb, '(f0.2)') rb
    call self%shape(ix, iy)
    write(cx, '(i5)') ix
    write(cy, '(i5)') iy
    str = trim(cl)//' '//trim(cr)//' '//trim(ct)//' '//trim(cb)//' '//trim(cx)//' '//trim(cy)
end function str

! ===================================================================================================
! Operator
! ===================================================================================================
logical function eq(a, b)
    type(MapFrame), intent(in) :: &
    &   a, b
    real(8) :: &
    &   al, ar, at, ab, &
    &   bl, br, bt, bb
    integer :: &
    &   ax, ay, &
    &   bx, by
    call a%domain(al, ar, at, ab)
    call b%domain(bl, br, bt, bb)
    call a%shape(ax, ay)
    call b%shape(bx, by)
    eq = (al == bl .and. ar == br .and. at == bt .and. ab == bb .and. ax == bx .and. ay == by)
end function eq

end module mapframe_mod
