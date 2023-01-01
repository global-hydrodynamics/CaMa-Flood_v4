module yos_cmf_sed
!==========================================================
!* PURPOSE: Shared variables for sediment in ILS
! (C) M.Hatono (Hiroshima-U)  May 2021
!==========================================================
  use PARKIND1,                only: JPIM, JPRB
  implicit none
  save
  !================================================
  integer(kind=JPIM)              :: nsed             ! number of sediment particle size
  integer(kind=JPIM)              :: psedDT           ! number of timestep within river timestep (DT/sedDT)
  integer(kind=JPIM)              :: totlyrnum        ! number of deposition layers
  
  logical                         :: revEgia          ! if use Egiazoroff
  logical                         :: lsedflw          ! if calculate sediment
  
  real(kind=JPRB)                 :: lambda           ! porosity (default:0.4)
  real(kind=JPRB)                 :: lyrdph           ! exchange layer depth
  real(kind=JPRB)                 :: psedD            ! density of sediment (default:2.65g/m3)
  real(kind=JPRB)                 :: pset             ! parameter for setting velocity
  real(kind=JPRB)                 :: pwatD            ! density of water (default:1g/m3)
  real(kind=JPRB)                 :: visKin           ! viscosity (default:1e-6)
  real(kind=JPRB)                 :: vonKar           ! von Karman coefficient (default: 0.4)
  
  real(kind=JPRB),allocatable,target :: d2sedv(:,:,:)    ! storage array for sediment variables
  real(kind=JPRB),pointer         :: d2bedout(:,:)    ! bedflow (m3/s)
  real(kind=JPRB),pointer         :: d2layer(:,:)     ! exchange layer storage (m3)
  real(kind=JPRB),pointer         :: d2netflw(:,:)    ! suspension - deposition (m3/s)
  real(kind=JPRB),pointer         :: d2sedcon(:,:)    ! suspended sediment concentration (m3/m3)
  real(kind=JPRB),pointer         :: d2sedfrc(:,:)    ! sediment distribution fraction [-]
  real(kind=JPRB),pointer         :: d2sedout(:,:)    ! suspended sediment flow (m3/s)
  real(kind=JPRB),pointer         :: d2sedinp(:,:)    ! sediment inflow (m3/s)
  
  real(kind=JPRB),allocatable,target :: d2depv(:,:,:)    ! storage array for sediment variables
  real(kind=JPRB),pointer         :: d2seddep(:,:,:)  ! deposition storage
  
  real(kind=JPRB),allocatable,target :: d2sedv_avg(:,:,:)    ! storage array for averaged sediment variables
  real(kind=JPRB),pointer         :: d2bedout_avg(:,:)  ! bedflow (m3/s)
  real(kind=JPRB),pointer         :: d2netflw_avg(:,:)    ! suspension - deposition (m3/s)
  real(kind=JPRB),pointer         :: d2sedout_avg(:,:)    ! suspended sediment flow (m3/s)
  real(kind=JPRB),pointer         :: d2sedinp_avg(:,:)    ! sediment inflow (m3/s)
  
  real(kind=JPRB),allocatable     :: sDiam(:)          ! sediment diameter
  real(kind=JPRB),allocatable     :: setVel(:)         ! setting velocity (m/s)
  
  integer(kind=JPIM)              :: STEP_SED          ! number of river timesteps within sediment timestep (sedDT/DT)
  real(kind=JPRB)                 :: sadd_riv          ! sum DT to calculate river variable average for sediment
  real(kind=JPRB)                 :: sadd_out          ! sum sedDT to calculate output average
  real(kind=JPRB)                 :: sedDT             ! sediment timestep (s)
  real(kind=JPRB),allocatable     :: d2rivout_sed(:)   ! accumulate rivout at DT to average into sedDT
  real(kind=JPRB),allocatable     :: d2rivvel_sed(:)   ! accumulate rivvel at DT to average into sedDT
  real(kind=JPRB),allocatable     :: d2rivsto_pre(:)   ! save river storage from previous timestep

!================================================
end module yos_cmf_sed
