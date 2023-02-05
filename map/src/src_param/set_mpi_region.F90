      program set_mpi_region
! ================================================
! set MPI region mask
      implicit none
! ===================
! calculation type
      character*256       ::  buf
      integer             ::  nMPI
! river network map parameters
      integer             ::  ix, iy, jx, jy
      integer             ::  nx, ny                  !! river map grid number
      real                ::  west, east, north, south
      real                ::  gsize


! river netwrok map
      integer,allocatable ::  basin(:,:)   
      real,allocatable    ::  nxtdst(:,:)  
      real,allocatable    ::  rivhgt(:,:)  

      integer,allocatable ::  mpireg(:,:)   

! local
      integer             ::  ibsn, nbsn
      integer,allocatable ::  bnum(:), bmpi(:)
!
      integer             ::  iMPI, jMPI, imin
      integer,allocatable ::  mpgrid(:)
! bifurcation
      integer             ::  ipth, npth, mpth                 !! bifurcation pathway ID
      integer             ::  ilev, nlev                 !! bifurcation layers

      real                ::  len, elv, dph              !! length, elevation, depth of the bifurcation channel
      real,allocatable    ::  wth(:)                     !! bifurcation width for each layer
      real                ::  lat, lon
!
      character*256       ::  finp, fparam, fout, fbifori, cMPI
      character*256       ::  cfmt, clen
      integer             ::  ios
! ================================================
      print *, 'Set MPI region mask'

      print *, '######################################################'
      print *, 'USAGE: Input number of MPI regions :   ./set_mpi_region $NMPI'

      nMPI=1
      call getarg(1,buf)
      if( trim(buf)=='' )then
        stop
      else
        read(buf,*) nMPI
      endif

! read parameters from arguments

      fparam='../params.txt'
      open(11,file=fparam,form='formatted')
      read(11,*) nx
      read(11,*) ny
      read(11,*) 
      read(11,*) gsize
      read(11,*) west
      read(11,*) east
      read(11,*) south
      read(11,*) north
      close(11)

      print *, 'Map Domain W-E-S-N:', west, east, south, north
      print *, 'Map Resolution: ', gsize
      print *, 'Map NX,NY: ',  nx, ny

! ==============================

      allocate(basin(nx,ny), nxtdst(nx,ny), rivhgt(nx,ny), mpireg(nx,ny) )

! ===================

      finp='../bifbsn.bin'
      open(11,file=finp,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
      read(11,rec=1) basin
      close(11)

      imin=0
      nbsn=0
      do iy=1, ny
        do ix=1, nx
          if( basin(ix,iy)>0 )then
            nbsn=max(nbsn,basin(ix,iy))
            imin=imin+1
          endif
        end do
      end do

      print *,  'numnber of basins', nbsn
      print *,  'numnber of grids',  imin

      allocate(bnum(nbsn),bmpi(nbsn))
      bnum(:)=0
      bmpi(:)=0
      do iy=1, ny
        do ix=1, nx
          if( basin(ix,iy)>0 )then
            ibsn=basin(ix,iy)
            bnum(ibsn)=bnum(ibsn)+1
          endif
        end do
      end do

      allocate(mpgrid(nMPI))
      mpgrid(:)=0

      jMPI=0
      do ibsn=1, nbsn
        if( bnum(ibsn)>0 )then
          imin=nx*ny
          do iMPI=1, nMPI
            if( mpgrid(iMPI)<imin )then
              jMPI=iMPI
              imin=mpgrid(iMPI)
            endif
          end do

          bmpi(ibsn)  =jMPI
          mpgrid(jMPI)=mpgrid(jMPI)+bnum(ibsn)
        endif
      end do

      do iMPI=1, nMPI
        print *, iMPI, mpgrid(iMPI)
      end do

      mpireg(:,:)=-9999
      do iy=1, ny
        do ix=1, nx
          if( basin(ix,iy)>0 )then
            mpireg(ix,iy)=0
            ibsn=basin(ix,iy)
            if( bmpi(ibsn)>0 )then
              mpireg(ix,iy)=bmpi(ibsn)
            endif
          endif
        end do
      end do

! ==========

      write(cMPI,'(i0)') nMPI
      fout='../mpireg-'//trim(cMPI)//'.bin'
      open(11,file=fout,form='unformatted',access='direct',recl=4*nx*ny)
      write(11,rec=1) mpireg
      close(11)

!===========
print *, 'update bifprm.txt'

      mpth=0
      fbifori='../bifprm.txt'
      open(12,file=fbifori,form='formatted')
      read(12,*) npth, nlev

      allocate( wth(nlev) )
      do ipth=1, npth
        read(12,*) ix,iy,jx,jy, len, elv, dph, (wth(ilev),ilev=1,nlev), lat, lon, ibsn
        if( mpireg(ix,iy)==mpireg(jx,jy) )then
          mpth=mpth+1
        endif
      end do
      close(12)

      print *, 'npth reduced: ', npth, mpth

      fout='../bifprm-'//trim(cMPI)//'.txt'
      print *, 'write to ', fout
      open(21,file=fout,form='formatted')
      write(21,'(2i8,a)') mpth, nlev, &
               '   npath, nlev, (ix,iy), (jx,jy), length, elevtn, depth, (width1, width2, ... wodth_nlev), (lat,lon), basin'

      fbifori='../bifprm.txt'
      open(12,file=fbifori,form='formatted')
      read(12,*) npth, nlev

      write(clen,'(i2)') 3+nlev
      cfmt='(4i8,'//trim(clen)//'f12.2,2f10.3,i8)'

      do ipth=1, npth
        read(12,*) ix,iy,jx,jy, len, elv, dph, (wth(ilev),ilev=1,nlev), lat, lon, ibsn
        if( mpireg(ix,iy)==mpireg(jx,jy) )then
          write(21,cfmt) ix, iy, jx, jy, len, elv, dph, (wth(ilev),ilev=1,nlev), lat, lon, ibsn
        endif
      end do
      close(12)
      close(21)

!!================================================

      end program set_mpi_region
