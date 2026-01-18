module camaframe_mod
    use mapframe_lib, only: &
    &   MapFrame, operator(==), init_MapFrame
    implicit none
    private
    public :: CaMaFrame, operator(==), init_CaMaFrame

type, extends(MapFrame) :: CaMaFrame
    private
    logical :: &
    &   is_catm_, is_fldstg_

    contains

    procedure :: is_catm => is_catm
    procedure :: set_is_catm => set_is_catm
    procedure :: is_fldstg => is_fldstg
    procedure :: set_is_fldstg => set_is_fldstg

end type CaMaFrame

!interface CaMaFrame
!    module procedure init_CaMaFrame
!end interface CaMaFrame

interface operator(==)
    module procedure eq_mf_cmf
    module procedure eq_cmf_mf
end interface operator(==)

contains

! ===================================================================================================
! Constructor
! ===================================================================================================
function init_CaMaFrame(left, right, top, bottom, nx, ny, is_catm, is_fldstg) result(obj)
    type(CaMaFrame) :: &
    &   obj
    real(8), intent(in), optional :: &
    &   left, right, top, bottom
    integer, intent(in), optional :: &
    &   nx, ny
    logical, intent(in), optional :: &
    &   is_catm, is_fldstg
    obj%MapFrame = init_MapFrame( &
    &   left, right, top, bottom, nx, ny)
    if (present(is_catm)) call obj%set_is_catm(is_catm)
    if (present(is_fldstg)) call obj%set_is_fldstg(is_fldstg)
end function init_CaMaFrame

! ===================================================================================================
! Getter/ Setter
! ===================================================================================================
logical function is_catm(self)
    class(CaMaFrame), intent(in) :: &
    &   self
    is_catm = self%is_catm_
end function is_catm

subroutine set_is_catm(self, is_catm)
    class(CaMaFrame), intent(inout) :: &
    &   self
    logical, intent(in) :: &
    &   is_catm
    self%is_catm_ = is_catm
end subroutine set_is_catm

logical function is_fldstg(self)
    class(CaMaFrame), intent(in) :: &
    &   self
    is_fldstg = self%is_fldstg_
end function is_fldstg

subroutine set_is_fldstg(self, is_fldstg)
    class(CaMaFrame), intent(inout) :: &
    &   self
    logical, intent(in) :: &
    &   is_fldstg
    self%is_fldstg_ = is_fldstg
end subroutine set_is_fldstg

! ---------------------------------------------------------------------------------------------------
character(len=256) function str(self)
    class(CaMaFrame), intent(in) :: &
    &   self
    character(len=16) :: cc, cf
    write(cc, *) self%is_catm()
    write(cf, *) self%is_fldstg()
    str = trim(self%MapFrame%str())//', catm ='//trim(cc)//', fldstg ='//trim(cf)
end function str
! ===================================================================================================
! Operator
! ===================================================================================================
function eq_mf_cmf(a, b) result(is_eq)
    logical is_eq
    type(MapFrame),  intent(in) :: a
    type(CaMaFrame), intent(in) :: b
    is_eq = a == b%MapFrame
end function eq_mf_cmf

function eq_cmf_mf(a, b) result(is_eq)
    logical is_eq
    type(CaMaFrame), intent(in) :: a
    type(MapFrame),  intent(in) :: b
    is_eq = a%MapFrame == b
end function eq_cmf_mf

end module camaframe_mod
