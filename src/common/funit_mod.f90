module funit_mod
    implicit none

    integer, parameter :: &
    &   STDOUT = 6, &
    &   TMP_UNIT = 11
    integer, protected :: &
    &   LOG_UNIT = STDOUT

contains

! Sources:
! cmf_utils_mod.F90
FUNCTION INQUIRE_FID() RESULT(FID)
INTEGER :: FID ! FILE ID
!* local variable
LOGICAL :: I_OPENED ! FILE ID IS ALREADY USED OR NOT?
!================================================
DO FID = 10, 999
  INQUIRE(FID,OPENED=I_OPENED)
  IF ( .NOT. I_OPENED ) RETURN
ENDDO
END FUNCTION INQUIRE_FID

end module funit_mod
