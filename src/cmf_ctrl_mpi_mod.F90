MODULE CMF_CTRL_MPI_MOD
!! contains nothing if UseMPI_CMF is not defined
#ifdef UseMPI_CMF
!==========================================================
!* PURPOSE: modules related to MPI usage 
!
!* CONTAINS:
! -- CMF_MPI_INIT     : MPI Initialization
! -- CMF_MPI_END      : MPI Finalization
!
! (C) D.Yamazaki (U-Tokyo)  Oct 2021
!
! Licensed under the Apache License, Version 2.0 (the "License");
!   You may not use this file except in compliance with the License.
!   You may obtain a copy of the License at: http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software distributed under the License is 
!  distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
! See the License for the specific language governing permissions and limitations under the License.
!==========================================================
!** shared variables in module

#ifdef IFS_CMF
USE MPL_MODULE
#else
USE MPI
#endif

USE PARKIND1,                ONLY: JPIM, JPRB, JPRM, JPRD
USE YOS_CMF_INPUT,           ONLY: LOGNAM
USE YOS_CMF_MAP,             ONLY: REGIONALL, REGIONTHIS, MPI_COMM_CAMA
!$ USE OMP_LIB
IMPLICIT NONE
!** local variables
!** MPI setting
INTEGER(KIND=JPIM),SAVE          :: ierr, Nproc, Nid
INTEGER(KIND=JPIM),SAVE          :: iOMP, nOMP
!==========================================================
CONTAINS
!####################################################################
! -- CMF_DRV_INPUT    : Set namelist & logfile
! -- CMF_DRV_INIT     : Initialize        CaMa-Flood
! -- CMF_DRV_END      : Finalize          CaMa-Flood
!
!####################################################################
#ifdef IFS_CMF
SUBROUTINE CMF_MPI_INIT(ICOMM_CMF)
IMPLICIT NONE

INTEGER(KIND=JPIM),OPTIONAL,INTENT(IN)  :: ICOMM_CMF
#else
SUBROUTINE CMF_MPI_INIT
IMPLICIT NONE
#endif
!================================================
!*** 0. MPI specific setting
REGIONTHIS=1

#ifdef IFS_CMF
MPI_COMM_CAMA=ICOMM_CMF
REGIONALL=MPL_NPROC(MPI_COMM_CAMA)
REGIONTHIS=MPL_MYRANK(MPI_COMM_CAMA)

#else
CALL MPI_Init(ierr)

MPI_COMM_CAMA=MPI_COMM_WORLD

CALL MPI_Comm_size(MPI_COMM_CAMA, Nproc, ierr)
CALL MPI_Comm_rank(MPI_COMM_CAMA, Nid, ierr)

REGIONALL =Nproc
REGIONTHIS=Nid+1
#endif

! For BUGFIX: Check MPI  / OpenMPI is working or not.
! Write to standard output (log file is not opened yet)
!!!!!#ifdef _OPENMP
!!!!!nOMP = omp_get_max_threads();
!!!!!!$OMP PARALLEL DO SIMD
!!!!!DO iOMP=1, nOMP
!!!!!  print *, 'MPI: ', REGIONTHIS, REGIONALL, ' OMP: ', omp_get_thread_num(), nOMP
!!!!!END DO
!!!!!!$OMP END PARALLEL DO SIMD
!!!!!#endif

END SUBROUTINE CMF_MPI_INIT
!####################################################################



!####################################################################
SUBROUTINE CMF_MPI_END
IMPLICIT NONE
INTEGER(KIND=JPIM)              :: ierr
!================================================
CALL MPI_Finalize(ierr)
END SUBROUTINE CMF_MPI_END
!####################################################################


!####################################################################
SUBROUTINE CMF_MPI_AllReduce_R2MAP(R2MAP)
USE YOS_CMF_INPUT,           ONLY: RMIS, NX,NY
IMPLICIT NONE
!* input/output
REAL(KIND=JPRM),INTENT(INOUT)   :: R2MAP(NX,NY)
!* local variable
REAL(KIND=JPRM)                 :: R2TMP(NX,NY)
!================================================
! gather to master node
#ifdef IFS_CMF
CALL MPL_ALLREDUCE(R2MAP,CDOPER='MIN',KCOMM=MPI_COMM_CAMA,KERROR=ierr)
#else
R2TMP(:,:)=RMIS
CALL MPI_AllReduce(R2MAP,R2TMP,NX*NY,MPI_REAL4,MPI_MIN,MPI_COMM_CAMA,ierr)
R2MAP(:,:)=R2TMP(:,:)
#endif

END SUBROUTINE CMF_MPI_AllReduce_R2MAP
!####################################################################




!####################################################################
SUBROUTINE CMF_MPI_AllReduce_R1PTH(R1PTH)
USE YOS_CMF_INPUT,           ONLY: RMIS
USE YOS_CMF_MAP,             ONLY: NPTHOUT, NPTHLEV
IMPLICIT NONE
!* input/output
REAL(KIND=JPRM),INTENT(INOUT)   :: R1PTH(NPTHOUT,NPTHLEV)
!* local variable
REAL(KIND=JPRM)                 :: R1PTMP(NPTHOUT,NPTHLEV)
!================================================
! gather to master node
#ifdef IFS_CMF
CALL MPL_ALLREDUCE(R1PTH,CDOPER='MIN',KCOMM=MPI_COMM_CAMA,KERROR=ierr)
#else
R1PTMP(:,:)=RMIS
CALL MPI_AllReduce(R1PTH,R1PTMP,NPTHOUT*NPTHLEV,MPI_REAL4,MPI_MIN,MPI_COMM_CAMA,ierr)
R1PTH(:,:)=R1PTMP(:,:)
#endif
END SUBROUTINE CMF_MPI_AllReduce_R1PTH
!####################################################################


!####################################################################
SUBROUTINE CMF_MPI_AllReduce_D2MAP(D2MAP)
! only used in netCDF restart file. (cannot be compiled due to a bug in MacOS mpif90)
USE YOS_CMF_INPUT,           ONLY: DMIS, NX,NY
IMPLICIT NONE
!* input/output
REAL(KIND=JPRB),INTENT(INOUT)   :: D2MAP(NX,NY)
!* local variable
REAL(KIND=JPRB)                 :: D2TMP(NX,NY)
!================================================
! gather to master node
#ifdef IFS_CMF
CALL MPL_ALLREDUCE(D2MAP,CDOPER='MIN',KCOMM=MPI_COMM_CAMA,KERROR=ierr)
#else
D2TMP(:,:)=DMIS
#ifdef SinglePrec_CMF
CALL MPI_AllReduce(D2MAP,D2TMP,NX*NY,MPI_REAL4,MPI_MIN,MPI_COMM_CAMA,ierr)
#else
CALL MPI_AllReduce(D2MAP,D2TMP,NX*NY,MPI_REAL8,MPI_MIN,MPI_COMM_CAMA,ierr)
#endif
D2MAP(:,:)=D2TMP(:,:)
#endif
END SUBROUTINE CMF_MPI_AllReduce_D2MAP
!####################################################################



!####################################################################
SUBROUTINE CMF_MPI_AllReduce_P2MAP(P2MAP)
! only used in netCDF restart file. (cannot be compiled due to a bug in MacOS mpif90)
USE YOS_CMF_INPUT,           ONLY: DMIS, NX,NY
IMPLICIT NONE
!* input/output
REAL(KIND=JPRD),INTENT(INOUT)   :: P2MAP(NX,NY)
!* local variable
REAL(KIND=JPRD)                 :: P2TMP(NX,NY)
!================================================
! gather to master node
#ifdef IFS_CMF
CALL MPL_ALLREDUCE(P2MAP,CDOPER='MIN',KCOMM=MPI_COMM_CAMA,KERROR=ierr)
#else
P2TMP(:,:)=DMIS
CALL MPI_AllReduce(P2MAP,P2TMP,NX*NY,MPI_REAL8,MPI_MIN,MPI_COMM_CAMA,ierr)
P2MAP(:,:)=P2TMP(:,:)
#endif
END SUBROUTINE CMF_MPI_AllReduce_P2MAP
!####################################################################




!####################################################################
SUBROUTINE CMF_MPI_AllReduce_D1PTH(D1PTH)
USE YOS_CMF_INPUT,           ONLY: DMIS
USE YOS_CMF_MAP,             ONLY: NPTHOUT, NPTHLEV, PTH_UPST, PTH_DOWN
IMPLICIT NONE
!* input/output
REAL(KIND=JPRB),INTENT(INOUT)   :: D1PTH(NPTHOUT,NPTHLEV)
!* local variable
REAL(KIND=JPRB)                 :: D1PTMP(NPTHOUT,NPTHLEV)
INTEGER(KIND=JPIM)              :: IPTH
!================================================
! gather to master node
DO IPTH=1,NPTHOUT
  IF (PTH_UPST(IPTH)<=0 .OR. PTH_DOWN(IPTH)<=0 ) THEN
    D1PTH(IPTH,:)=DMIS
  ENDIF
END DO
#ifdef IFS_CMF
CALL MPL_ALLREDUCE(D1PTH,CDOPER='MIN',KCOMM=MPI_COMM_CAMA,KERROR=ierr)
#else
D1PTMP(:,:)=DMIS
#ifdef SinglePrec_CMF
CALL MPI_AllReduce(D1PTH,D1PTMP,NPTHOUT*NPTHLEV,MPI_REAL4,MPI_MIN,MPI_COMM_CAMA,ierr)
#else
CALL MPI_AllReduce(D1PTH,D1PTMP,NPTHOUT*NPTHLEV,MPI_REAL8,MPI_MIN,MPI_COMM_CAMA,ierr)
#endif
D1PTH(:,:)=D1PTMP(:,:)
#endif
END SUBROUTINE CMF_MPI_AllReduce_D1PTH
!####################################################################


!####################################################################
SUBROUTINE CMF_MPI_AllReduce_P1PTH(P1PTH)
USE YOS_CMF_INPUT,           ONLY: DMIS
USE YOS_CMF_MAP,             ONLY: NPTHOUT, NPTHLEV, PTH_UPST, PTH_DOWN
IMPLICIT NONE
!* input/output
REAL(KIND=JPRD),INTENT(INOUT)   :: P1PTH(NPTHOUT,NPTHLEV)
!* local variable
REAL(KIND=JPRD)                 :: P1PTMP(NPTHOUT,NPTHLEV)
INTEGER(KIND=JPIM)              :: IPTH
!================================================
! gather to master node
DO IPTH=1,NPTHOUT
  IF (PTH_UPST(IPTH)<=0 .OR. PTH_DOWN(IPTH)<=0 ) THEN
    P1PTH(IPTH,:)=DMIS
  ENDIF
END DO
#ifdef IFS_CMF
CALL MPL_ALLREDUCE(P1PTH,CDOPER='MIN',KCOMM=MPI_COMM_CAMA,KERROR=ierr)
#else
P1PTMP(:,:)=DMIS
CALL MPI_AllReduce(P1PTH,P1PTMP,NPTHOUT*NPTHLEV,MPI_REAL8,MPI_MIN,MPI_COMM_CAMA,ierr)
P1PTH(:,:)=P1PTMP(:,:)
#endif
END SUBROUTINE CMF_MPI_AllReduce_P1PTH
!####################################################################



!####################################################################
SUBROUTINE CMF_MPI_ADPSTP(DT_MIN)
USE YOS_CMF_INPUT,           ONLY: LOGNAM
IMPLICIT NONE
!* input/output
REAL(KIND=JPRB),INTENT(INOUT)   :: DT_MIN
!* local variable
REAL(KIND=JPRD)                 :: DT_LOC, DT_OUT
!================================================
!*** MPI: use same DT in all node
DT_LOC=DT_MIN

#ifdef IFS_CMF
DT_OUT=DT_LOC
CALL MPL_ALLREDUCE(DT_OUT,CDOPER='MIN',KCOMM=MPI_COMM_CAMA,KERROR=ierr)
#else
CALL MPI_AllReduce(DT_LOC, DT_OUT, 1, MPI_DOUBLE_PRECISION, MPI_MIN, MPI_COMM_CAMA,ierr)
#endif
DT_MIN=REAL(DT_OUT,KIND=JPRB)
WRITE(LOGNAM,'(A,2F10.2)') "ADPSTP (MPI_AllReduce): DT_LOC->DTMIN", DT_LOC, DT_MIN

END SUBROUTINE CMF_MPI_ADPSTP
!####################################################################

#endif
END MODULE CMF_CTRL_MPI_MOD
