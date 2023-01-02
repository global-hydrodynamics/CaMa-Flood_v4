module cmf_ctrl_sedrest_mod
!==========================================================
!* PURPOSE: physics for sediment transport
! (C) M.Hatono  (Hiroshima-U)  May 2021
!
! Licensed under the Apache License, Version 2.0 (the "License");
!   You may not use this file except in compliance with the License.
!   You may obtain a copy of the License at: http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software distributed under the License is 
!  distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
! See the License for the specific language governing permissions and limitations under the License.
!==========================================================
#ifdef UseMPI_CMF
  use MPI
#endif
  use PARKIND1,                only: JPIM, JPRM
  use YOS_CMF_INPUT,           only: LOGNAM, NX, NY
  use YOS_CMF_MAP,             only: MPI_COMM_CAMA, NSEQALL, NSEQMAX, REGIONTHIS
  use CMF_UTILS_MOD,           only: INQUIRE_FID
  use yos_cmf_sed,             only: nsed, totlyrnum, d2layer, d2sedcon, d2seddep

  implicit none
  save
  integer(kind=JPIM)              :: ifrq_rst_sed
  character(len=256)              :: sedrest_infile, sedrest_outpre

  namelist/sediment_restart/  sedrest_infile, sedrest_outpre, ifrq_rst_sed

contains
!####################################################################
! -- sediment_restart_init
! -- sediment_restart_write
! --
!####################################################################
subroutine sediment_restart_init
  use YOS_CMF_MAP,             only: D2RIVLEN, D2RIVWTH
  use YOS_CMF_PROG,            only: P2RIVSTO
  use yos_cmf_sed,             only: d2sedfrc, lyrdph, d2rivsto_pre, &
                                     d2rivout_sed, d2rivvel_sed, sadd_riv, sadd_out
  use CMF_UTILS_MOD,           only: mapR2vecD, inquire_fid

  implicit none
  save
#ifdef UseMPI_CMF
  integer(kind=JPIM)             :: ierr
#endif
  integer(kind=JPIM)             :: ilyr, irec, ised, iseq, tmpnam, nsetfile
  real(kind=JPRM)                :: r2temp(NX,NY)

  nsetfile = inquire_fid()
  open(nsetfile, file='input_sed.nam', status='old')
  rewind(nsetfile)
  read(nsetfile,nml=sediment_restart)
  close(nsetfile)

  if ( sedrest_infile == "" ) then  ! set layer/bedload if no restart file
    !$omp parallel do
    do iseq = 1, NSEQALL
      d2layer(iseq,:) = lyrdph * D2RIVWTH(iseq,1) * D2RIVLEN(iseq,1) * d2sedfrc(iseq,:)
      do ilyr = 1, totlyrnum-1
        d2seddep(iseq,ilyr,:) = d2layer(iseq,:)
      enddo
      d2seddep(iseq,totlyrnum,:) = ( max(10.d0-lyrdph*totlyrnum,0.d0) ) * D2RIVWTH(iseq,1) * D2RIVLEN(iseq,1) * d2sedfrc(iseq,:)
    enddo
    !$omp end parallel do

  else
    if ( REGIONTHIS == 1 ) then
      tmpnam = INQUIRE_FID()
      open(tmpnam,file=sedrest_infile,form='unformatted',access='direct',recl=4*NX*NY)
    endif
    do irec = 1, 2
      do ised = 1, nsed
        if ( REGIONTHIS == 1 ) read(tmpnam,rec=(irec-1)*nsed+ised) r2temp
#ifdef UseMPI_CMF
        call MPI_Bcast(r2temp(1,1),NX*NY,mpi_real4,0,MPI_COMM_CAMA,ierr)
#endif
        select case(irec)
          case (1)
            call mapR2vecD(r2temp,d2layer(:,ised))
          case (2)
            call mapR2vecD(r2temp,d2sedcon(:,ised))
        end select
      enddo
    enddo

    do irec = 1, totlyrnum
      do ised = 1, nsed
        if ( REGIONTHIS == 1 ) read(tmpnam,rec=(irec+1)*nsed+ised) r2temp
#ifdef UseMPI_CMF
        call MPI_Bcast(r2temp(1,1),NX*NY,mpi_real4,0,MPI_COMM_CAMA,ierr)
#endif
        call mapR2vecD(r2temp,d2seddep(:,irec,ised))
      enddo
    enddo
    if ( REGIONTHIS == 1 ) close(tmpnam)
    write(LOGNAM,*) 'read restart sediment',maxval(d2seddep(:,totlyrnum,:))
  endif

  allocate(d2rivsto_pre(NSEQMAX), d2rivout_sed(NSEQMAX), d2rivvel_sed(NSEQMAX))
  sadd_riv = 0.d0
  sadd_out = 0.d0
  d2rivsto_pre(:) = P2RIVSTO(:,1)
  d2rivout_sed(:) = 0.d0
  d2rivvel_sed(:) = 0.d0
end subroutine sediment_restart_init
!==========================================================
!+
!==========================================================
subroutine sediment_restart_write
  use YOS_CMF_TIME,            only: KSTEP, NSTEPS, JDD, JHHMM, JHOUR, JMIN, JYYYYMMDD
  use YOS_CMF_INPUT,           only: CSUFBIN, RMIS
  use CMF_CTRL_RESTART_MOD,    only: CRESTDIR
  use CMF_UTILS_MOD,           only: vecD2mapR
#ifdef UseMPI_CMF
  use CMF_CTRL_MPI_MOD,        only: CMF_MPI_AllReduce_R2MAP
#endif
  
  implicit none
  save
  integer(kind=JPIM)              :: irec, irest, ised, tmpnam
  real(kind=JPRM)                 :: r3final(NX,NY,nsed), r2temp(NX,NY)
  character(len=256)              :: cdate, cfile

  irest = 0

  if ( ifrq_rst_sed>=0 .and. KSTEP==NSTEPS ) then  !! end of run
    irest = 1
  endif

  if ( ifrq_rst_sed>=1 .and. ifrq_rst_sed<=24 ) then  !! at selected hour
    if ( mod(JHOUR,ifrq_rst_sed)==0 .and. JMIN==0 ) then
      irest = 1
    endif
  endif

  if ( ifrq_rst_sed==30 ) then  !! at end of month
    if ( JDD==1 .and. JHOUR==0 .and. JMIN==0 ) then
      irest = 1
    endif
  endif

  if ( irest==1 ) then
    write(LOGNAM,*) ""
    write(LOGNAM,*) "!---------------------!"
    write(LOGNAM,*) 'cmf::sediment_restart_write: write time: ' , JYYYYMMDD, JHHMM

    write(cdate,'(I8.8,I2.2)') JYYYYMMDD,JHOUR
    cfile=trim(CRESTDIR)//TRIM(sedrest_outpre)//TRIM(cdate)//TRIM(CSUFBIN)
    write(LOGNAM,*) 'wrte_rest_bin: restart file:',cfile

    !*** write restart data (2D map)
    if ( REGIONTHIS == 1 ) then
      tmpnam = INQUIRE_FID()
      open(TMPNAM,file=cfile,form='unformatted',access='direct',recl=4*NX*NY*nsed)
    endif
    do irec = 1, 2
     r3final(:,:,:) = RMIS
     do ised = 1, nsed
       select case(irec)
         case (1)
           call vecD2mapR(d2layer(:,ised),r2temp)
         case (2)
           call vecD2mapR(d2sedcon(:,ised),r2temp)
       end select
#ifdef UseMPI_CMF
       call CMF_MPI_AllReduce_R2MAP(r2temp)
#endif
       r3final(:,:,ised) = r2temp(:,:)
     enddo
     if ( REGIONTHIS == 1 ) write(tmpnam,rec=irec) r3final
    enddo

    do irec = 1, totlyrnum
      r3final(:,:,:) = RMIS
      do ised = 1, nsed
        call vecD2mapR(d2seddep(:,irec,ised),r2temp)
#ifdef UseMPI_CMF
        call CMF_MPI_AllReduce_R2MAP(r2temp)
#endif
        r3final(:,:,ised) = r2temp
      enddo
      if ( REGIONTHIS == 1 ) write(tmpnam,rec=irec+2) r3final
    enddo

    if ( REGIONTHIS == 1 ) close(tmpnam)

  endif
end subroutine sediment_restart_write
!####################################################################

end module cmf_ctrl_sedrest_mod
