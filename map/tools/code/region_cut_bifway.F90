      program COMBINE_HIRES
! ===============================================
! Filter source bifurcation pathways to the selected regional map.
! ===============================================
      implicit none

! Crop offsets and dimensions, plus pathway-table fields.

      character*256            :: global_dir  !! global map directory
      integer                  :: nXX, nYY  !! x/y map dimensions
      real                     :: lon_ori, lat_ori  !! west and north edge of global map
      real                     :: lon_end, lat_end  !! east and south edge of global map
      integer                  :: mXX, mYY  !! regional x/y dimensions
      integer                  :: dXX, dYY  !! river map domain shift from global map
      real                     :: gsize  !! map grid size [deg]
      real                     :: west, east, north, south  !! map bounds [deg]

      integer, allocatable      :: nextx(:,:)  !! downstream x grid index

      integer                  :: ipath  !! source pathway row index
      integer                  :: npath  !! bifurcation-path count
      integer                  :: mpath  !! number of pathways retained inside the crop
      integer                  :: ilev  !! bifurcation-layer index
      integer                  :: nlev  !! layer count

      integer                  :: iXX, iYY    !! source-map upstream endpoint
      integer                  :: jXX, jYY    !! source-map downstream endpoint
      integer                  :: iXX2, iYY2  !! regional upstream endpoint after offset
      integer                  :: jXX2, jYY2  !! regional downstream endpoint after offset
      real                     :: dst         !! bifurcation pathway length [m]
      real                     :: elv  !! pathway elevation [m]
      real                     :: lon, lat  !! longitude/latitude [deg]
      real, allocatable         :: wth(:)  !! bifurcation width [m]

! files
      character*256            :: rfile1
      character*256            :: wfile1
      character*256            :: cfmt
      character*256            :: clen

! ===============================================
! ================================================
! [1] Read crop metadata and the regional river network
! ================================================
! Read the crop offsets and dimensions produced by cut_domain.
      rfile1='./dim_change.txt'
      open(11,file=rfile1,form='formatted')
      read(11,'(a)') global_dir
      read(11,*    ) nXX
      read(11,*    ) nYY
      read(11,*    ) gsize
      read(11,*    ) lon_ori
      read(11,*    ) lon_end
      read(11,*    ) lat_end
      read(11,*    ) lat_ori
      read(11,*    ) 
      read(11,*    ) mXX
      read(11,*    ) mYY
      read(11,*    ) dXX
      read(11,*    ) dYY
      read(11,*    ) west
      read(11,*    ) east
      read(11,*    ) south
      read(11,*    ) north
      close(11)

      allocate(nextx(mXX,mYY))
      rfile1='../nextxy_noedge.bin'
      open(11,file=rfile1,form='unformatted',access='direct',recl=4*mXX*mYY)
      read(11,rec=1) nextx
      close(11)
      
! ====================

! ================================================
! [2] Count retained paths and write their regional coordinates
! ================================================
! Count source bifurcation paths whose two endpoints remain inside the region.
! The second pass writes those paths with their indices shifted to regional grid.
      rfile1=trim(global_dir)//'/bifori.txt'
      wfile1='../bifori.txt'
      open(11,file=rfile1,form='formatted')
      read(11,*) npath, nlev
      allocate(wth(nlev))
      mpath=0
      do ipath=1, npath
        read(11,*) iXX, iYY, jXX, jYY
        iXX2=iXX-dXX
        iYY2=iYY-dYY
        jXX2=jXX-dXX
        jYY2=jYY-dYY

        if( iXX2>0 .and. iXX2<mXX .and. iYY2>0 .and. iYY2<mYY )then
          if( jXX2>0 .and. jXX2<mXX .and. jYY2>0 .and. jYY2<mYY )then
            if( nextx(iXX2,iYY2)/=-9999 .and. nextx(jXX2,jYY2)/=-9999 )then
              mpath=mpath+1
            endif
          endif
        endif
      end do
      close(11)

      open(11,file=rfile1,form='formatted')
      open(31,file=wfile1,form='formatted')

      write(clen,'(i2)') 2+nlev
      cfmt='(4i8,'//trim(clen)//'f12.2,2f10.3)'

      read(11,*) npath, nlev
      write(31,'(2i8,a)') mpath, nlev, & 
      '   npath, nlev, (ix,iy), (jx,jy), length, elevtn, (width1, width2, ... wodth_nlev), (lon,lat)'

      do ipath=1, npath
        read(11,*) iXX, iYY, jXX, jYY, dst, elv, (wth(ilev),ilev=1,nlev), lon, lat
        iXX2=iXX-dXX
        iYY2=iYY-dYY
        jXX2=jXX-dXX
        jYY2=jYY-dYY

        if( iXX2>0 .and. iXX2<mXX .and. iYY2>0 .and. iYY2<mYY )then
          if( jXX2>0 .and. jXX2<mXX .and. jYY2>0 .and. jYY2<mYY )then
            if( nextx(iXX2,iYY2)/=-9999 .and. nextx(jXX2,jYY2)/=-9999 )then
              write(31,cfmt) iXX2, iYY2, jXX2, jYY2, dst, elv, (wth(ilev),ilev=1,nlev), lon, lat
            endif
          endif
        endif
      end do
      close(11)
      close(31)


      end program COMBINE_HIRES


      subroutine nextxy(ix,iy,jx,jy,dir)
! =============================================
      implicit none
!
      integer                  :: ix, iy  !! x/y grid index
      integer                  :: jx, jy  !! target x/y grid index
      integer*1                :: dir
! ============================================
      jx=ix
      jy=iy

      if( dir==2 .or. dir==3 .or. dir==4 )then
        jx=ix+1
!        if( jx>nx ) jx=1
      elseif( dir==6 .or. dir==7 .or. dir==8 )then
        jx=ix-1
!        if( jx==0 ) jx=nx
      endif

      if( dir==1 .or. dir==2 .or. dir==8 )then
        jy=iy-1
      elseif( dir==4 .or. dir==5 .or. dir==6 )then
        jy=iy+1
      endif

      end subroutine nextxy




      subroutine set_name(lon,lat,cname)
! ===============================================
      implicit none
!
      real                     :: lon, lat  !! longitude/latitude [deg]

      character*1              :: ew
      character*1              :: sn
      character*2              :: clat
      character*3              :: clon
      character*7              :: cname
! ===============================================
      if( lon<0 )then
        ew='w'
        write(clon,'(i3.3)') int(-lon)
      else
        ew='e'
        write(clon,'(i3.3)')  int(lon)
      endif

      if( lat<0 )then
        sn='s'
        write(clat,'(i2.2)') int(-lat)
      else
        sn='n'
        write(clat,'(i2.2)')  int(lat)
      endif

      cname=sn//clat//ew//clon

      end subroutine set_name

