module cmf_ctrl_sedrest_mod
!==========================================================
!* PURPOSE: physics for sediment transport
!
! (C) M.Hatono  (Hiroshima-U)  May 2021
!==========================================================
#ifdef UseMPI_CMF
  use MPI
#endif
  use PARKIND1,                only: JPIM, JPRM
  use YOS_CMF_INPUT,           only: LOGNAM, NX, NY
  use YOS_CMF_MAP,             only: MPI_COMM_CAMA, NSEQALL, NSEQMAX, REGIONTHIS
  use CMF_UTILS_MOD,           only: INQUIRE_FID
  use yos_cmf_sed,             only: nsed, totlyrnum, b2layer, b2sedcon, b2seddep

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
! --
!####################################################################
subroutine sediment_restart_init
  use YOS_CMF_MAP,             only: B2RIVLEN, B2RIVWTH
  use YOS_CMF_PROG,            only: D2RIVSTO
  use yos_cmf_sed,             only: b2sedfrc, lyrdph, b2rivsto_pre, &
                                     b2rivout_sed, b2rivvel_sed, sadd_riv, sadd_out
  use CMF_UTILS_MOD,           only: MAPR2VECB, inquire_fid

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
      b2layer(iseq,:) = lyrdph * B2RIVWTH(iseq,1) * B2RIVLEN(iseq,1) * b2sedfrc(iseq,:)
      do ilyr = 1, totlyrnum-1
        b2seddep(iseq,ilyr,:) = b2layer(iseq,:)
      enddo
      b2seddep(iseq,totlyrnum,:) = ( max(10.d0-lyrdph*totlyrnum,0.d0) ) * B2RIVWTH(iseq,1) * B2RIVLEN(iseq,1) * b2sedfrc(iseq,:)
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
            call MAPR2VECB(r2temp,b2layer(:,ised))
          case (2)
            call MAPR2VECB(r2temp,b2sedcon(:,ised))
        end select
      enddo
    enddo

    do irec = 1, totlyrnum
      do ised = 1, nsed
        if ( REGIONTHIS == 1 ) read(tmpnam,rec=(irec+1)*nsed+ised) r2temp
#ifdef UseMPI_CMF
        call MPI_Bcast(r2temp(1,1),NX*NY,mpi_real4,0,MPI_COMM_CAMA,ierr)
#endif
        call MAPR2VECB(r2temp,b2seddep(:,irec,ised))
      enddo
    enddo
    if ( REGIONTHIS == 1 ) close(tmpnam)
    write(LOGNAM,*) 'read restart sediment',maxval(b2seddep(:,totlyrnum,:))
  endif

  allocate(b2rivsto_pre(NSEQMAX), b2rivout_sed(NSEQMAX), b2rivvel_sed(NSEQMAX))
  sadd_riv = 0.d0
  sadd_out = 0.d0
  b2rivsto_pre(:) = D2RIVSTO(:,1)
  b2rivout_sed(:) = 0.d0
  b2rivvel_sed(:) = 0.d0
end subroutine sediment_restart_init
!==================================
!
!==================================
subroutine sediment_restart_write
  use YOS_CMF_TIME,            only: KSTEP, NSTEPS, JDD, JHHMM, JHOUR, JMIN, JYYYYMMDD
  use YOS_CMF_INPUT,           only: CSUFBIN, RMIS
  use CMF_CTRL_RESTART_MOD,    only: CRESTDIR
  use CMF_UTILS_MOD,           only: VECB2MAPR
#ifdef UseMPI_CMF
  use CMF_CTRL_MPI_MOD,        only: CMF_MPI_REDUCE_R2MAP
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
           call VECB2MAPR(b2layer(:,ised),r2temp)
         case (2)
           call VECB2MAPR(b2sedcon(:,ised),r2temp)
       end select
#ifdef UseMPI_CMF
       call CMF_MPI_REDUCE_R2MAP(r2temp)
#endif
       r3final(:,:,ised) = r2temp(:,:)
     enddo
     if ( REGIONTHIS == 1 ) write(tmpnam,rec=irec) r3final
    enddo

    do irec = 1, totlyrnum
      r3final(:,:,:) = RMIS
      do ised = 1, nsed
        call VECB2MAPR(b2seddep(:,irec,ised),r2temp)
#ifdef UseMPI_CMF
        call CMF_MPI_REDUCE_R2MAP(r2temp)
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
