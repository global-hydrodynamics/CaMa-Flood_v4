module CMF_CTRL_SED_MOD
!==========================================================
!* PURPOSE: physics for sediment transport
!
! (C) M.Hatono  (Hiroshima-U)  May 2021
!==========================================================
#ifdef UseMPI_CMF
  use MPI
#endif
  use PARKIND1,                only: JPIM, JPRB, JPRM
  use YOS_CMF_INPUT,           only: LOGNAM, NX, NY
  use YOS_CMF_MAP,             only: MPI_COMM_CAMA, NSEQALL, NSEQMAX, REGIONTHIS
  use CMF_UTILS_MOD,           only: INQUIRE_FID
  use yos_cmf_sed,             only: lyrdph, nsed, totlyrnum, sDiam

  implicit none
  !*** namelist/sediment_map/
  character(len=256)              :: crocdph
  character(len=256)              :: csedfrc
  character(len=256)              :: sedD
  namelist/sediment_map/ crocdph,sedD,csedfrc

contains
!####################################################################
! -- cmf_sed_input
! -- cmf_sed_init
! -- cmf_sed_forcing_get
! --
! --
!####################################################################

subroutine cmf_sed_nmlist
  use yos_cmf_sed,             only: lambda, psedDT, psedD, pset, pwatD, &
                                     revEgia, visKin, vonKar

  implicit none
  integer(kind=JPIM)              :: nsetfile

  namelist/sediment_param/  lambda, lyrdph, nsed, psedDT, psedD, &
                            pset, pwatD, revEgia, totlyrnum, &
                            visKin, vonKar

  nsetfile = INQUIRE_FID()
  open(nsetfile,file='input_sed.nam',status='OLD')
 
  lambda = 0.4d0
  lyrdph = 0.00005d0
  nsed = 3
  psedDT = 1 
  psedD = 2.65d0
  pset = 1.d0
  pwatD = 1.d0
  revEgia = .true.
  totlyrnum = 5
  visKin = 1.d-6
  vonKar = 0.4d0

  rewind(nsetfile)
  read(nsetfile,nml=sediment_param)
  !defaults
  write(LOGNAM,*) 'nml sediment_param'
  write(LOGNAM,*) 'lambda    :', lambda
  write(LOGNAM,*) 'lyrdph    :', lyrdph
  write(LOGNAM,*) 'psedDT    :', psedDT
  write(LOGNAM,*) 'psedD     :', psedD
  write(LOGNAM,*) 'pset      :', pset
  write(LOGNAM,*) 'pwatD     :', pwatD
  write(LOGNAM,*) 'revEgia   :', revEgia
  write(LOGNAM,*) 'totlyrnum :', totlyrnum
  write(LOGNAM,*) 'visKin    :', visKin
  write(LOGNAM,*) 'vonKar    :', vonKar

  rewind(nsetfile)
  read(nsetfile,nml=sediment_map)
  !defaults
  write(LOGNAM,*) 'nml sediment_map'
  write(LOGNAM,*) 'crocdph  :', trim(crocdph)
  write(LOGNAM,*) 'sDiam   :', sedD
  write(LOGNAM,*) 'csedfrc  :', csedfrc

  close(nsetfile)
end subroutine cmf_sed_nmlist

subroutine cmf_sed_init
  use YOS_CMF_INPUT,           only: LOUTPUT
  use cmf_ctrl_sedinp_mod,     only: sediment_input_init
  use cmf_ctrl_sedout_mod,     only: sediment_output_init
  use cmf_ctrl_sedrest_mod,    only: sediment_restart_init
  
  implicit none

  call sediment_vars_init

  call sediment_map_init

  call sediment_input_init

  if ( LOUTPUT ) then
    call sediment_output_init
  endif

  call sediment_restart_init

contains

  subroutine sediment_map_init
    use YOS_CMF_INPUT,           only: NLFP, PGRV
    use CMF_UTILS_MOD,           only: MAP2VEC
    use yos_cmf_sed,             only: d2sedfrc, psedD, pset, pwatD, setVel, visKin
    use cmf_calc_sedpar_mod,     only: calc_settingVelocity
    use sed_utils_mod,           only: splitchar

    implicit none
    integer(kind=JPIM)              :: i, ierr, ised, iseq, tmpnam
    real(kind=JPRM)                 :: r2temp(NX,NY), sTmp1(nsed)
    character(len=256)              :: ctmp(20)

    !------------------------!
    ! get sediment diameters !
    !------------------------!
    ctmp(:) = '-999'
    call splitchar(sedD,ctmp)
    ised = 0
    allocate(sDiam(nsed))
    do i = 1, nsed
      if ( ctmp(i) /= '-999' ) then
        ised = ised + 1
        read(ctmp(i),*) sDiam(ised)
      endif
    enddo
    if ( ised /= nsed ) then
      write(LOGNAM,*) 'nsed and sedD do not match',ised,nsed
      stop
    endif
    write(LOGNAM,*) ised,' grain sizes: ',sDiam(:)

    !----------------------------!
    ! calculate setting velocity !
    !----------------------------!
    allocate(setVel(nsed))
    setVel(:) = calc_settingVelocity()

    !-----------------------------!
    ! read sediment fraction file !
    !-----------------------------!
    allocate(d2sedfrc(NSEQMAX,nsed))
    if ( REGIONTHIS == 1 ) then
      tmpnam = INQUIRE_FID()
      open(tmpnam,file=csedfrc,form='unformatted',access='direct',recl=4*NX*NY)
    endif
    do ised = 1, nsed
      if ( REGIONTHIS == 1 ) read(tmpnam,rec=ised) r2temp
#ifdef UseMPI_CMF
      call MPI_Bcast(r2temp(1,1),NX*NY,mpi_real4,0,MPI_COMM_CAMA,ierr)
#endif
      call MAP2VEC(r2temp,d2sedfrc(:,ised))
    enddo
    if ( REGIONTHIS == 1 ) close(tmpnam)
   
    ! adjust if any fractions are negative or if sum is not equal to 1
    if ( nsed == 1 ) then
      d2sedfrc(:,:) = 1.d0
    else
      !$omp parallel do
      do iseq = 1, NSEQALL
        if ( minval(d2sedfrc(iseq,:)) < 0.d0 .or. sum(d2sedfrc(iseq,:)) == 0.d0 ) then
          d2sedfrc(iseq,:) = 1.d0 / dble(nsed)
        else if ( sum(d2sedfrc(iseq,:)) /= 1.d0 ) then
          d2sedfrc(iseq,:) = d2sedfrc(iseq,:) / sum(d2sedfrc(iseq,:))
        endif
      enddo
      !$omp end parallel do
    endif
  end subroutine sediment_map_init

  subroutine sediment_vars_init
    use yos_cmf_sed,             only: d2bedout, d2netflw, d2seddep, &
                                       d2bedout_avg, d2netflw_avg,   &
                                       d2sedout, d2sedcon, d2sedinp, &
                                       d2sedout_avg, d2sedinp_avg, d2layer, &
                                       d2sedv, d2sedv_avg, d2depv
    implicit none

    allocate(d2sedv(NSEQMAX,nsed,6))
    d2sedv(:,:,:) = 0._JPRB
    d2sedout => d2sedv(:,:,1)
    d2sedcon => d2sedv(:,:,2)
    d2sedinp => d2sedv(:,:,3)
    d2bedout => d2sedv(:,:,4)
    d2netflw => d2sedv(:,:,5)
    d2layer => d2sedv(:,:,6)

    allocate(d2depv(NSEQMAX,totlyrnum,nsed))
    d2depv(:,:,:) = 0._JPRB
    d2seddep => d2depv

    allocate(d2sedv_avg(NSEQMAX,nsed,4))
    d2sedv_avg(:,:,:) = 0._JPRB
    d2sedout_avg => d2sedv_avg(:,:,1)
    d2sedinp_avg => d2sedv_avg(:,:,2)
    d2bedout_avg => d2sedv_avg(:,:,3)
    d2netflw_avg => d2sedv_avg(:,:,4)
  end subroutine sediment_vars_init


end subroutine cmf_sed_init

!####################################################################

end module CMF_CTRL_SED_MOD
