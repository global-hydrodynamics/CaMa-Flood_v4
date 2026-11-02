module cmf_ctrl_sedinp_mod
!==========================================================
!* PURPOSE: Manage sediment input
! (C) M.Hatono  (Hiroshima-U)  Oct 2022
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
  use PARKIND1,                only: JPIM, JPRB, JPRM
  use YOS_CMF_INPUT,           only: LOGNAM
  use YOS_CMF_MAP,             only: NSEQALL, NSEQMAX, D2GRAREA
  use CMF_CTRL_FORCING_MOD,    only: INPX, INPY, INPA

  implicit none
  save
  character(len=256)              :: sedinput_dir, sedinput_pre, sedinput_suf

  real(kind=JPRB),allocatable     :: d2slope(:,:)     ! floodplain slope [deg]
  integer(kind=JPIM)              :: iseq

  real(kind=JPRB)                 :: dsylunit         ! unit conversion for sediment [m3/km2] -> [m3/m2]
  real(kind=JPRB)                 :: pyld, pyldc, pyldpc  ! parameters for sediment erosion calculation

contains
!####################################################################
!-- sediment_input_init
!-- cmf_sed_forcing
!-- calc_sedyld
!-- sedinp_interp
!####################################################################
subroutine sediment_input_init
#ifdef UseMPI_CMF
  use YOS_CMF_MAP,             only: MPI_COMM_CAMA
#endif
  use CMF_UTILS_MOD,           only: INQUIRE_FID, mapR2vecD

  implicit none
  save
  character(len=256)              :: cslope       ! slope file
  character(len=256)              :: cinpmat_sed  ! input matrix for sediment

  call read_sedinp_nmlist

  call read_slope
contains
!================================
  subroutine read_sedinp_nmlist
    implicit none
    integer(kind=JPIM)            :: nsetfile
    
    namelist/sediment_input/ sedinput_dir, sedinput_pre, sedinput_suf, &
                             cslope, dsylunit, pyld, pyldc, pyldpc,    &
                             cinpmat_sed

    nsetfile = INQUIRE_FID()
    open(nsetfile,file='input_sed.nam',status='OLD')

    sedinput_dir='./'
    sedinput_pre='./'
    sedinput_suf='./'
    cslope='./slope.bin'
    dsylunit = 1.d-6
    pyld = 0.01d0
    pyldc = 2.d0
    pyldpc = 2.d0
    cinpmat_sed = './inpmat.bin'

    rewind(nsetfile)
    read(nsetfile,nml=sediment_input)
    !defaults
    write(LOGNAM,*) 'nml sediment_input'
    write(LOGNAM,*) 'cslope    :', trim(cslope)
    write(LOGNAM,*) 'dsylunit  :', dsylunit
    write(LOGNAM,*) 'pyld      :', pyld
    write(LOGNAM,*) 'pyldc     :', pyldc
    write(LOGNAM,*) 'pyldpc    :', pyldpc
    write(LOGNAM,*) 'cinpmat_sed:', trim(cinpmat_sed)
  end subroutine

  subroutine read_slope
    use YOS_CMF_INPUT,         only: NX,NY, NLFP
    use YOS_CMF_MAP,           only: REGIONTHIS

    implicit none
    integer                       :: ierr, tmpnam, i
    real(kind=jprm)               :: r2temp(nx,ny)
    allocate(d2slope(NSEQMAX,NLFP))
    if ( REGIONTHIS == 1 ) then
      tmpnam = INQUIRE_FID()
      open(tmpnam,file=cslope,form='unformatted',access='direct',recl=4*NX*NY)
    endif
    do i = 1, NLFP
      if ( REGIONTHIS == 1 ) read(tmpnam,rec=i) r2temp
#ifdef UseMPI_CMF
      call MPI_Bcast(r2temp(1,1),NX*NY,mpi_real4,0,MPI_COMM_CAMA,ierr)
#endif
      call mapR2vecD(r2temp,d2slope(:,i))
    enddo
    if ( REGIONTHIS == 1 ) close(tmpnam)
  end subroutine read_slope
  
end subroutine sediment_input_init
!==========================================================
!+
!==========================================================
subroutine cmf_sed_forcing
  ! read forcing from file
  use YOS_CMF_INPUT,           only: TMPNAM,NXIN,NYIN,DTIN
  use YOS_CMF_TIME,            only: IYYYY, IMM, IDD, IHOUR, IMIN
  use CMF_UTILS_MOD,           only: CONV_END,INQUIRE_FID
  
  implicit none
  save
  real(kind=JPRB)                 :: d2temp(nseqmax)
  !* local variables
  integer(kind=jpim)              :: irecinp
  integer(kind=jpim)              :: isec
  character(len=256)              :: cifname             !! input file
  character(len=256)              :: cdate               !!
  real(kind=jprm)                 :: r2tmp(nxin,nyin)
  
  !*** 1. calculate irec for sub-daily precipitation
  isec    = ihour*60*60+imin*60   !! current second in a day
  irecinp = int( isec/dtin ) +1   !! precipitation irec (sub-daily precipitation)

  !*** 2. set file name
  write(cdate,'(i4.4,i2.2,i2.2)') iyyyy,imm,idd
  cifname=trim(sedinput_dir)//'/'//trim(sedinput_pre)//trim(cdate)//trim(sedinput_suf)
  write(LOGNAM,*) "cmf::sed_forcing_get_bin:",trim(cifname)

  !*** 3. open & read forcing data
  tmpnam=inquire_fid()
  open(tmpnam,file=cifname,form='unformatted',access='direct',recl=4*nxin*nyin)
  read(tmpnam,rec=irecinp) r2tmp
  close(tmpnam)

  !*** 5. conduct necessary conversion
  call sedinp_interp(r2tmp,d2temp)   ! interpolate forcing grid to model grid
  call calc_sedyld(d2temp)           ! calculate sediment yield into rivers
end subroutine cmf_sed_forcing
!==========================================================
!+
!==========================================================
subroutine calc_sedyld(pbuffin)
  use PARKIND1,                only: JPIM, JPRB
  use YOS_CMF_INPUT,           only: DTIN
  use yos_cmf_sed,             only: d2sedinp, d2sedinp_avg, d2sedfrc
  
  implicit none
  save
  real(kind=JPRB), intent(in)     :: pbuffin(:)
  real(kind=JPRB)                 :: sbuff(NSEQMAX)
  !================================================

  
  call prcp_convert_sed(pbuffin, sbuff) ! convert precipitation to sediment yield based on Sunada&Hasegawa(1993)
 
  !$omp parallel do 
  do iseq = 1, NSEQALL
    d2sedinp(iseq,:) = sbuff(iseq) * d2sedfrc(iseq,:)  ! distribute sediment yield to proportionate to sediment grain fraction
    d2sedinp_avg(iseq,:) = d2sedinp_avg(iseq,:) + d2sedinp(iseq,:) * DTIN
  enddo
  !$omp end parallel do

contains
!=============================
!+ prcp_convert_sed
!=============================
  subroutine prcp_convert_sed(pbuffin,pbuffout)
    use YOS_CMF_DIAG,          only: D2FLDFRC
    use YOS_CMF_INPUT,         only: NLFP

    implicit none
    save
    real(kind=JPRB), intent(in)   :: pbuffin(:)     !! kg/m2/s
    real(kind=JPRB), intent(out)  :: pbuffout(:)  !! m3/s
    integer(kind=JPIM)            :: i, iseq

    !$omp parallel do
    do iseq = 1, NSEQALL
      pbuffout(iseq) = 0.d0
      if ( pbuffin(iseq) * 86400.d0 <= 10.d0 ) cycle

      do i = 1, NLFP
        if ( D2FLDFRC(iseq,1) * NLFP > dble(i) ) cycle  ! no erosion if submerged
        pbuffout(iseq) = pbuffout(iseq) + pyld * (pbuffin(iseq)*3600.d0)**pyldpc * d2slope(iseq,i)**pyldc / 3600.d0 & 
          & * D2GRAREA(iseq,1) * min(dble(i)/dble(NLFP)-D2FLDFRC(iseq,1), 1.d0/dble(NLFP)) * dsylunit
      enddo
    enddo
    !$omp end parallel do
  end subroutine prcp_convert_sed
    
end subroutine calc_sedyld
!==========================================================
!+
!==========================================================
subroutine sedinp_interp(pbuffin,pbuffout)
! interporlate sediment forcing data using "input matrix"
  use YOS_CMF_INPUT,           only: NXIN, NYIN, INPN, RMIS
  
  implicit none
  real(kind=JPRM),intent(in)      :: pbuffin(:,:)     !! default for prcp[kg/m2/s]
  real(kind=JPRB),intent(out)     :: pbuffout(:)    !! kg/m2/s
  ! save for omp
  integer(kind=jpim),save  ::  iseq, ixin, iyin, inpi  !! for output
  !$omp threadprivate    (ixin, iyin)
  !============================
  !$omp parallel do
  do iseq=1, NSEQALL
    pbuffout(iseq)=0._JPRB
    do inpi=1, INPN
      ixin=INPX(iseq,inpi)
      iyin=INPY(iseq,inpi)
      if( ixin>0 )then
        if( ixin > NXIN .or. iyin > NYIN ) then
          write(LOGNAM,*)  "error"
          write(LOGNAM,*)  'xxx',iseq,inpi,ixin,iyin
          cycle
        endif
        if( pbuffin(ixin,iyin).ne.RMIS )then
          pbuffout(iseq) = pbuffout(iseq) + pbuffin(ixin,iyin) * INPA(iseq,inpi) / D2GRAREA(iseq,1)
        endif
      endif
    end do
    pbuffout(iseq)=max(pbuffout(iseq), 0._JPRB)
  end do
  !$omp end parallel do
end subroutine sedinp_interp
!####################################################################

end module cmf_ctrl_sedinp_mod
