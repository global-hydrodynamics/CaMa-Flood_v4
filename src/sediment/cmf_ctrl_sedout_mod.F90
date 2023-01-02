module cmf_ctrl_sedout_mod
!==========================================================
!* PURPOSE: Output module for CaMa-Flood sediment scheme
! (C) M. Hatono (Hiroshima Univ)  Jan 2023
!
! Licensed under the Apache License, Version 2.0 (the "License");
!   You may not use this file except in compliance with the License.
!   You may obtain a copy of the License at: http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software distributed under the License is 
!  distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
! See the License for the specific language governing permissions and limitations under the License.
!==========================================================
  use PARKIND1,                only: JPIM, JPRB, JPRM
  use YOS_CMF_INPUT,           only: LOGNAM, NX, NY
  use YOS_CMF_MAP,             only: REGIONTHIS
  use CMF_CTRL_OUTPUT_MOD,     only: COUTDIR, IRECOUT, LOUTCDF, LOUTVEC, TVAROUT
  use yos_cmf_sed,             only: nsed

  implicit none
  save
  type(TVAROUT),allocatable       :: varout(:)          ! output variable type set

  integer(kind=JPIM)              :: nvarsout

  !*** namelist/sediment_output
  character(len=256)         :: csedsout
  namelist/sediment_output/ csedsout

contains
!####################################################################
!-- sediment_output_init
!-- cmf_sed_output
!-- sediment_output_end
!####################################################################
subroutine sediment_output_init
  use CMF_CTRL_OUTPUT_MOD,     only: COUTTAG
  use CMF_UTILS_MOD,           only: INQUIRE_FID
  use sed_utils_mod,           only: splitchar
  
  implicit none
  save
  integer(kind=JPIM)              :: jf, j
  integer(kind=JPIM)              :: nvars, nsetfile
  parameter                         (nvars=30)
  character(len=256)              :: cvnames(nvars), fName
  
  nsetfile = INQUIRE_FID()
  open(nsetfile,file='input_sed.nam',status='OLD')
  rewind(nsetfile)
  read(nsetfile,nml=sediment_output)
  close(nsetfile)    

  !---------------------------!
  ! get output variable names !
  !---------------------------!
  cvnames(:) = 'none'
  nvarsout = 0
  call splitchar(csedsout,cvnames)
  do j = 1, nvars
    if ( cvnames(j) /= 'none' ) then
      nvarsout = nvarsout + 1
    endif
  enddo

  if ( nvarsout == 0 ) then
    write(LOGNAM,*) "cmf::sed_output_init: no output files will be produced!"
    return
  endif

  allocate(varout(nvarsout))

  !* loop on variables and create files
  do jf=1,nvarsout
    write(LOGNAM,*) "creating output for variable:", trim( cvnames(jf) )
    select case (cvnames(jf))
      case ('sedout')
        varout(jf)%cvname=cvnames(jf)
        varout(jf)%cvlname='suspended sediment flow'
        varout(jf)%cvunits='m3/s'
      case ('sedcon')
        varout(jf)%cvname=cvnames(jf)
        varout(jf)%cvlname='suspended sediment concentration'
        varout(jf)%cvunits='m3/m3'
      case ('sedinp')
        varout(jf)%cvname=cvnames(jf)
        varout(jf)%cvlname='sediment inflow from land'
        varout(jf)%cvunits='m3/s'
      case ('bedout')
        varout(jf)%cvname=cvnames(jf)
        varout(jf)%cvlname='bedload'
        varout(jf)%cvunits='m3/s'
      case ('netflw')
        varout(jf)%cvname=cvnames(jf)
        varout(jf)%cvlname='net entrainment flow'
        varout(jf)%cvunits='m3/s'
      case ('layer')
        varout(jf)%cvname=cvnames(jf)
        varout(jf)%cvlname='exchange layer volume'
        varout(jf)%cvunits='m3'
      case default  ! should only be seddep
        if ( cvnames(jf)(:6) == 'deplyr' ) then
          varout(jf)%cvname=cvnames(jf)
          varout(jf)%cvlname='river bed volume (vertical layer)'
          varout(jf)%cvunits='m3'
        else
          write(LOGNAM,*) trim(cvnames(jf)), 'Not defined in sediment output init'
        endif
    end select
    varout(jf)%binid=INQUIRE_FID()

    if ( trim(varout(jf)%cvname(:6)) == 'deplyr' ) then
      fName = trim(varout(jf)%cvname)//'_'//trim(COUTTAG)
    else
      fName = trim(varout(jf)%cvname)//trim(COUTTAG)
    endif

    if ( LOUTCDF ) then
      if ( REGIONTHIS==1 ) then
        call create_outcdf
      endif
    else
      call create_outbin
    endif
  enddo

contains

  subroutine create_outcdf
#ifdef UseCDF_CMF
    use YOS_CMF_INPUT,           only: RMIS, CSUFCDF
    use YOS_CMF_TIME,            only: ISYYYY, ISMM,   ISDD,   ISHOUR, ISMIN
    use YOS_CMF_MAP,             only: D1LON, D1LAT
    use CMF_UTILS_MOD,           only: NCERROR
    use CMF_CTRL_OUTPUT_MOD,     only: NDLEVEL
    use yos_cmf_sed,             only: sDiam
    use NETCDF
    
    implicit none
    save
    integer(kind=JPIM)              :: timeid, varid, latid, lonid, sedid
    character(len=256)              :: ctime
 
    varout(jf)%irecnc = 1

    varout(jf)%cfile = trim(COUTDIR)//trim(fName)//trim(CSUFCDF)
    call NCERROR( nf90_create(varout(jf)%cfile,nf90_netcdf4,varout(jf)%ncid),&
                  'creating file:'//trim(varout(jf)%cfile) )
    !=== set dimension ===
    call NCERROR( nf90_def_dim(varout(jf)%ncid, 'time', nf90_unlimited, timeid) )
    call NCERROR( nf90_def_dim(varout(jf)%ncid, 'lat', NY, latid) )
    call NCERROR( nf90_def_dim(varout(jf)%ncid, 'lon', NX, lonid) )
    call NCERROR( nf90_def_dim(varout(jf)%ncid, 'sedD', nsed, sedid) )   
 
    !=== define variables ===
    call NCERROR( nf90_def_var(varout(jf)%ncid, 'sedD', nf90_double, (/sedid/), varid) )
    call NCERROR( nf90_put_att(varout(jf)%ncid, varid, 'long_name','sediment grain size') )
    call NCERROR( nf90_put_att(varout(jf)%ncid, varid, 'units','meters') )

    call NCERROR( nf90_def_var(varout(jf)%ncid, 'lat', nf90_float, (/latid/), varid) )
    call NCERROR( nf90_put_att(varout(jf)%ncid, varid, 'long_name','latitude') )
    call NCERROR( nf90_put_att(varout(jf)%ncid, varid, 'units','degrees_north') )
    
    call NCERROR( nf90_def_var(varout(jf)%ncid, 'lon', nf90_float, (/lonid/), varid) )
    call NCERROR( nf90_put_att(varout(jf)%ncid, varid, 'long_name','longitude') )
    call NCERROR( nf90_put_att(varout(jf)%ncid, varid, 'units','degrees_east') )
   
    write(ctime,'(a14,i4.4,a1,i2.2,a1,i2.2,a1,i2.2,a1,i2.2)') 'seconds since ',ISYYYY,'-',ISMM,'-',ISDD,' ',ISHOUR,":",ISMIN
    call NCERROR( nf90_def_var(varout(jf)%ncid, 'time', nf90_double, (/timeid/), varout(jf)%timid) )
    call NCERROR( nf90_put_att(varout(jf)%ncid, varout(jf)%timid, 'long_name','time') )
    call NCERROR( nf90_put_att(varout(jf)%ncid, varout(jf)%timid, 'units',ctime) )
    
    !===
    call NCERROR( nf90_def_var(varout(jf)%ncid, varout(jf)%cvname, nf90_float, &
                  (/lonid,latid,sedid,timeid/), varout(jf)%varid,deflate_level=ndlevel),     &
                  'creating variable')
    
    call NCERROR( nf90_put_att(varout(jf)%ncid, varout(jf)%varid, 'long_name', trim(varout(jf)%cvlname)) )
    call NCERROR( nf90_put_att(varout(jf)%ncid, varout(jf)%varid, 'units',     trim(varout(jf)%cvunits)) )
    call NCERROR( nf90_put_att(varout(jf)%ncid, varout(jf)%varid, '_fillvalue',rmis) )
    
    call NCERROR( nf90_enddef(varout(jf)%ncid) )
    
    !=== put nsed lon lat info ===
    call NCERROR ( nf90_inq_varid(varout(jf)%ncid,'sedD',varid),'getting id' )
    call NCERROR( nf90_put_var(varout(jf)%ncid,varid,sDiam))

    call NCERROR ( nf90_inq_varid(varout(jf)%ncid,'lon',varid),'getting id' )
    call NCERROR( nf90_put_var(varout(jf)%ncid,varid,D1LON))
    
    call NCERROR ( nf90_inq_varid(varout(jf)%ncid,'lat',varid),'getting id' )
    call NCERROR( nf90_put_var(varout(jf)%ncid,varid,D1LAT))
    
    write(LOGNAM,*) 'cfile: ',trim(varout(jf)%cfile),' cvar:',trim(varout(jf)%cvname),&
                    ' clname: ',trim(varout(jf)%cvlname),' cunits: ',trim(varout(jf)%cvunits)
    write(LOGNAM,*) 'open in unit: ',varout(jf)%ncid
#endif
  end subroutine create_outcdf

  subroutine create_outbin
    use YOS_CMF_INPUT,           only: CSUFBIN, CSUFVEC
    use YOS_CMF_MAP,             only: NSEQMAX, REGIONALL
    
    implicit none
  
    if ( LOUTVEC ) then
      varout(jf)%cfile=trim(coutdir)//trim(fName)//trim(CSUFVEC)
      open(varout(jf)%binid,file=varout(jf)%cfile,form='unformatted',access='direct',recl=4*NSEQMAX*nsed)
    else
      if ( REGIONTHIS==1 ) then
        varout(jf)%cfile=trim(coutdir)//trim(fName)//trim(CSUFBIN)
        open(varout(jf)%binid,file=varout(jf)%cfile,form='unformatted',access='direct',recl=4*NX*NY*nsed)
      endif
    endif
    write(LOGNAM,*) "output file opened in unit: ", TRIM(VAROUT(JF)%CFILE), VAROUT(JF)%BINID
  end subroutine create_outbin

end subroutine sediment_output_init
!==========================================================
!+
!==========================================================
subroutine cmf_sed_output
  use CMF_UTILS_MOD,           only: vecD2mapR
  use YOS_CMF_INPUT,           only: IFRQ_OUT, RMIS
  use YOS_CMF_MAP,             only: NSEQMAX
  use YOS_CMF_TIME,            only: JHOUR, JMIN
  use yos_cmf_sed,             only: d2layer, d2sedcon, d2seddep, d2bedout_avg, d2netflw_avg, &
                                     d2sedout_avg, d2sedinp_avg, d2sedv_avg, sadd_out
  use cmf_ctrl_sedrest_mod,    only: sediment_restart_write
#ifdef UseMPI_CMF
  use CMF_CTRL_MPI_MOD,        only: CMF_MPI_AllReduce_R2MAP
#endif
  
  implicit none
  save
  integer(kind=JPIM)              :: ilyr, ised
  integer(kind=JPIM)              :: jf
  real(kind=JPRB),pointer         :: d2vec(:,:) ! point data location to output
  !*** local
  real(kind=JPRM)                 :: r3out(NX,NY,nsed)
  !================================================
  call sediment_restart_write

  d2sedv_avg(:,:,:) = d2sedv_avg(:,:,:) / dble(sadd_out)
  write(LOGNAM,*) 'cmf_sed_output: average ',sadd_out,' seconds'

  !*** 0. check date:hour with output frequency
  if ( mod(JHOUR,IFRQ_OUT)==0 .and. JMIN==0 ) then             ! JHOUR: end of time step , nfpph: output frequency (hour)

    !*** 1. calc average variable
    write(LOGNAM,*) 'cmf::sediment_output_write: write irec: ', IRECOUT

    !*** 2. check variable name & allocate data to pointer dvec
    do jf=1,nvarsout
      select case (varout(jf)%cvname)
        case ('sedout')
          d2vec => d2sedout_avg
        case ('sedcon')
          d2vec => d2sedcon
        case ('sedinp')
          d2vec => d2sedinp_avg
        case ('bedout')
          d2vec => d2bedout_avg
        case ('netflw')
          d2vec => d2netflw_avg
        case ('layer')
          d2vec => d2layer
        case default
          if ( varout(jf)%cvname(:6) == 'deplyr' ) then
            read(varout(jf)%cvname(7:8),*) ilyr
            d2vec => d2seddep(:,ilyr,:)
          else
            write(LOGNAM,*) varout(jf)%cvname, ' not defined in cmf_output_mod'
          endif
      end select   !! variable name select

  !! convert 1dvector to 3dmap
      r3out(:,:,:) = RMIS
      
      if ( .not. LOUTVEC ) then
        do ised = 1, nsed
          call vecD2mapR(d2vec(:,ised),r3out(:,:,ised))             !! mpi node data is gathered by vec2map
#ifdef UseMPI_CMF
          call CMF_MPI_AllReduce_R2MAP(r3out(:,:,ised))
#endif
        enddo
        
        if ( REGIONTHIS==1 ) then
          if ( LOUTCDF ) then
            call wrte_outcdf
          else
            call wrte_outbin(varout(jf)%binid,IRECOUT,r3out)
          endif
        endif
      else 
        call wrte_outvec(varout(jf)%binid,IRECOUT,d2vec)
      endif
    end do

    write(LOGNAM,*) 'cmf::sediment_output_write: end'
  endif

  d2sedv_avg(:,:,:) = 0._JPRB
  sadd_out = 0._JPRB

contains
  subroutine wrte_outcdf
#ifdef UseCDF_CMF
    use NETCDF
    use YOS_CMF_TIME,            only: KMINSTART, KMINNEXT
    use CMF_UTILS_MOD,           only: NCERROR
    
    implicit none
    save
    real(kind=JPRB)                 :: xtime
    xtime = real( (KMINNEXT-KMINSTART), JPRB) *60._JPRB
    call NCERROR( nf90_put_var(varout(jf)%ncid,varout(jf)%timid,xtime,(/varout(jf)%irecnc/)) )

    call NCERROR( nf90_put_var(varout(jf)%ncid,varout(jf)%varid,r3out(1:NX,1:NY,1:nsed),&
                  (/1,1,1,varout(jf)%irecnc/),(/NX,NY,nsed,1/)) )
    
    ! update irec
    varout(jf)%irecnc=varout(jf)%irecnc+1
#endif
  end subroutine wrte_outcdf
  !==========================================================
  subroutine wrte_outbin(ifn,irec,r2outdat)
    
    implicit none
    !*** input
    save
    integer(kind=JPIM),intent(in)   :: ifn                 !! file number
    integer(kind=JPIM),intent(in)   :: irec                !! record
    real(kind=JPRM)                 :: r2outdat(NX,NY,nsed)
    !================================================
    write(ifn,rec=irec) r2outdat
  end subroutine wrte_outbin
  !==========================================================
  subroutine wrte_outvec(ifn,irec,d2outdat)
    
    implicit none
    !*** input
    save
    integer(kind=JPIM),intent(in)   :: ifn                 !! file number
    integer(kind=JPIM),intent(in)   :: irec                !! record
    real(kind=JPRB),intent(in)      :: d2outdat(NSEQMAX,nsed) !! output data
    !*** local
    real(kind=JPRM)                 :: r2outdat(NSEQMAX,nsed)
    !================================================
    r2outdat(:,:)=real(d2outdat(:,:))
    write(ifn,rec=irec) r2outdat
  end subroutine wrte_outvec
  !==========================================================
end subroutine cmf_sed_output
!==========================================================
!+
!==========================================================
subroutine sediment_output_end
#ifdef UseCDF_CMF
  use NETCDF
  use CMF_UTILS_MOD,           only: NCERROR
#endif
  use YOS_CMF_MAP,             only: REGIONTHIS
  
  implicit none
  save
  integer(kind=JPIM)              :: jf

  write(LOGNAM,*) ""
  write(LOGNAM,*) "!---------------------!"
  write(LOGNAM,*) "sediment_output_end: finalize output module"

  if ( LOUTVEC ) then
    do jf = 1, nvarsout
      close(varout(jf)%binid)
    enddo
  else if ( REGIONTHIS==1 ) then
    do jf = 1, nvarsout
      if ( LOUTCDF ) then
#ifdef UseCDF_CMF
        call NCERROR( nf90_close(varout(jf)%ncid) )
#endif
      else
        close(varout(jf)%binid)
      endif
    enddo
  endif 
  
  write(LOGNAM,*) 'sediment_output_end: end'
end subroutine sediment_output_end
  
end module cmf_ctrl_sedout_mod
