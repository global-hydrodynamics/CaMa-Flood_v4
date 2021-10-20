      program roff_coast
! ================================================
      implicit none
! ==============================
! input files
      character*256       ::  crunoff                 !! runoff input 
      character*256       ::  cout                    !! modified runoff output
      character*256       ::  diminfo                 !! dimention info file
      character*256       ::  cinpmat                 !! input matrix
      character*256       ::  crofave                 !! runoff long-term ave
      integer             ::  ios
! dimention
      integer             ::  ix, iy, jx, jy, dx, dy
      integer             ::  nx, ny                  !! river map dimention
      integer             ::  nxin, nyin              !! input map dimention

      real,allocatable    ::  lon(:), lat(:)          !! longitude, latidude [deg]
      real                ::  west,east,north,south   !! domain boundary [deg]
      real                ::  gsize                   !! grid size [deg]

! input matrix
      integer             ::  i, inpn                 !! max number of input grid for one river grid
      integer,allocatable ::  inpx(:,:,:),inpy(:,:,:) !! input grid (ixin,iyin) of river map grid (ix,iy)

! input runoff
      real,allocatable    ::  roffin(:,:)           !! runof (input) [mm/day]
      real,allocatable    ::  rofave(:,:)
! variables
      real,allocatable    ::  out(:,:)                !! modified runof
      integer,allocatable :: check(:,:)

      real                ::  rofmin


! ================================================
! read parameters from arguments

      call getarg(1,crunoff)
      call getarg(2,cout)
      call getarg(3,diminfo)
      call getarg(4,cinpmat)
      call getarg(5,crofave)

! ===============================
! read river network and input file dimentions

      print *, trim(diminfo)
      open(11,file=diminfo,form='formatted')
      read(11,*) nx
      read(11,*) ny
      read(11,*)  !! flfp
      read(11,*) nxin
      read(11,*) nyin
      read(11,*) inpn
      read(11,*)  !! inpmat
      read(11,*) west
      read(11,*) east
      read(11,*) north
      read(11,*) south
      close(11)

      print *, trim(cinpmat)
      print *, nx, ny, nxin, nyin, inpn

      allocate(rofave(nxin,nyin),roffin(nxin,nyin),out(nxin,nyin),check(nxin,nyin))

! ===========================================
      allocate(lon(nx),lat(ny))
      gsize=(east-west)/real(nx)
      do ix=1,nx
        lon(ix)=west+(real(ix)-0.5)*gsize
      enddo
      do iy=1,ny
        lat(iy)=north-(real(iy)-0.5)*gsize
      enddo
! =========

print *, 'set input matrix'
      allocate(inpx(nx,ny,inpn),inpy(nx,ny,inpn))
      open(11,file=cinpmat,form='unformatted',access='direct',recl=4*nx*ny)
      do i=1, inpn
        read(11,rec=       i) inpx(:,:,i:i)
        read(11,rec=  inpn+i) inpy(:,:,i:i)
      end do
      close(11)

! ==========
      open(11,file=crunoff,form='unformatted',access='direct',recl=4*nxin*nyin,status='old',iostat=ios)
      read(11,rec=1) roffin
      close(11)

      open(11,file=crofave,form='unformatted',access='direct',recl=4*nxin*nyin,status='old',iostat=ios)
      read(11,rec=1) rofave
      close(11)
! ==========
print *, 'calculate runoff grid'
      check(:,:)=0
      do iy=1, ny
        do ix=1, nx
          do i=1, inpn
            jx=inpx(ix,iy,i)
            jy=inpy(ix,iy,i)
            if( jx>=1 .and. jx<=nxin .and. jy>=1 .and. jy<=nyin )then
              if( rofave(jx,jy)<=0 ) check(jx,jy)=2
              if( rofave(jx,jy)>0  ) check(jx,jy)=1
            endif
          end do
        end do
      end do

      out(:,:)=roffin(:,:)
      do iy=1, nyin
        do ix=1, nxin
          if( check(ix,iy)==2 )then
            rofmin=1.e20
            do dy=-1, 1
              do dx=-1, 1
                jx=ix+dx
                jy=iy+dy
                if( jx==0  ) jx=nxin
                if( jx>nxin) jx=1
                if( jy>0 .and. jy<=nyin )then
                  if( check(jx,jy)==1 )then
                    rofmin=min(rofmin,roffin(jx,jy) )
                  endif
                endif
              end do
            end do
            if( rofmin/=1.e20 )then
              out(ix,iy)=rofmin
            endif
          endif
        end do
      end do

      open(11,file=cout,form='unformatted',access='direct',recl=4*nxin*nyin)
      write(11,rec=1) out
      close(11)
! ============================================================



      end program roff_coast



