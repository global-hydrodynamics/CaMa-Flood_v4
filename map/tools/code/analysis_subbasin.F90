! Divide the directed river network at sufficiently large adjacent drainage-area increases.
! Usage: analysis_subbasin THRESHOLD1 [THRESHOLD2] [SUBBSN_OUTPUT SUBCOL_OUTPUT]
! Stage 2 only subdivides Stage 1 unassigned cells; positive Stage 1 IDs are frozen.
! Working directory: a directory directly under the map root; map inputs are read via ../.
! The threshold is applied to the increase in upstream area between a cell and
! its downstream cell, not as a simple test of whether either cell exceeds it.
! Outputs are one int32 NX x NY record each: parameter-labeled analysis files
! in the map root by default, unless output paths are supplied. In subbsn,
! positive IDs are segments, zero marks unassigned modeled river cells, and
! -9999 marks cells outside the modeled river network. subcol uses the same
! zero/-9999 mask convention; positive IDs receive display colors 1-10.
! These optional products are not runtime inputs.
      program analysis_subbasin
! ================================================
      use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
      implicit none
! ===================
! calculation type
      character*256       ::  buf

! river network map parameters
      integer             ::  ix, iy, jx, jy, kx, ky
      integer             ::  nx, ny                  !! river map grid number
      real                ::  west, east, north, south
      real                ::  gsize


! river netwrok map
      integer,allocatable ::  nextx(:,:)      !! downstream x
      integer,allocatable ::  nexty(:,:)      !! downstream y
      real,allocatable    ::  uparea(:,:)      !! source area [m2], converted to km2 before thresholding

      integer,allocatable ::  basin(:,:)      !! segment ID; 0 for small network cells, -9999 outside network
      integer,allocatable ::  color(:,:)      !! display color index for positive segment IDs

! local
      integer             ::  ibsn, nbsn, jbsn
      integer,allocatable ::  bsn_mask(:,:)
      integer             ::  color_this, color_max, grid
      integer             ::  col_used(10), icol

      integer,allocatable::  basin_order(:), basin_new(:)
      real,allocatable   ::  basin_size(:)
      real               ::  upa_thrs          !! minimum downstream area increase used to split a segment [km2]
      real               ::  upa_thrs2        !! optional smaller threshold [km2]
      integer            ::  nargs, output_arg, nbsn_stage1
      logical            ::  two_stage
      character*256      ::  threshold_label
! file
      character*256       ::  finp, fparam, fout_subbsn, fout_subcol
      integer             ::  ios
! ================================================
! read parameters from arguments
      print *, '######################################################'
      print *, 'USAGE: analysis_subbasin THRESHOLD1 [THRESHOLD2] [SUBBSN_OUTPUT SUBCOL_OUTPUT]'
      print *, '    Positive thresholds in km2; THRESHOLD2 must be smaller than THRESHOLD1.'
      print *, '    Output paths are an optional pair; defaults are parameter-labeled files in ../.'

      nargs=command_argument_count()
      if( nargs<1 .or. nargs>4 )then
        print *, 'ERROR: provide one or two thresholds and optionally both output paths'
        stop 1
      endif
      two_stage=(nargs==2 .or. nargs==4)
      call get_command_argument(1,buf)
      read(buf,*,iostat=ios) upa_thrs
      if( ios/=0 )then
        print *, 'ERROR: THRESHOLD1 must be a positive finite number'
        stop 1
      endif
      if( .not.ieee_is_finite(upa_thrs) .or. upa_thrs<=0 )then
        print *, 'ERROR: THRESHOLD1 must be a positive finite number'
        stop 1
      endif
      threshold_label=trim(adjustl(buf))
      output_arg=2
      if( two_stage )then
        call get_command_argument(2,buf)
        read(buf,*,iostat=ios) upa_thrs2
        if( ios/=0 )then
          print *, 'ERROR: THRESHOLD2 must be positive and smaller than THRESHOLD1'
          stop 1
        endif
        if( .not.ieee_is_finite(upa_thrs2) .or. upa_thrs2<=0 .or. upa_thrs2>=upa_thrs )then
          print *, 'ERROR: THRESHOLD2 must be positive and smaller than THRESHOLD1'
          stop 1
        endif
        threshold_label=trim(threshold_label)//'_'//trim(adjustl(buf))
        output_arg=3
      endif
      fout_subbsn='../analysis_subbsn_'//trim(threshold_label)//'.bin'
      fout_subcol='../analysis_subcol_'//trim(threshold_label)//'.bin'
      if( nargs>=3 )then
        call get_command_argument(output_arg,fout_subbsn)
        call get_command_argument(output_arg+1,fout_subcol)
        if( trim(fout_subbsn)=='' .or. trim(fout_subcol)=='' )then
          print *, 'ERROR: both output paths must be non-empty'
          stop 1
        endif
      endif

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

      allocate(nextx(nx,ny), nexty(nx,ny), uparea(nx,ny))
      allocate(basin(nx,ny), color(nx,ny))

! ===================

      finp='../nextxy.bin'
      open(11,file=finp,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
      read(11,rec=1) nextx
      read(11,rec=2) nexty
      close(11)

      finp='../uparea.bin'
      open(11,file=finp,form='unformatted',access='direct',recl=4*nx*ny,status='old',iostat=ios)
      read(11,rec=1) uparea
      close(11)

      ! Convert the source drainage-area map from m2 to km2 before applying
      ! the user-supplied threshold.
      do iy=1, ny
        do ix=1, nx
          if( uparea(ix,iy)>0 ) uparea(ix,iy)=uparea(ix,iy)*1.e-6
        end do
      end do

! ===================
      ! Split a link only when its downstream drainage area jumps by more than
      ! the threshold; terminal links are left unchanged.
      do iy=1, ny
        do ix=1, nx
          if( uparea(ix,iy)>upa_thrs )then
            jx=nextx(ix,iy)
            jy=nexty(ix,iy)
            ! Terminal links use negative codes; compare areas only for a valid
            ! downstream grid cell. This preserves the legacy cut rule on river links.
            if( jx>=1 .and. jx<=nx .and. jy>=1 .and. jy<=ny )then
              if( uparea(jx,jy)>uparea(ix,iy)+upa_thrs )then
                nextx(ix,iy)=-9
                nexty(ix,iy)=-9
              endif
            endif
          endif
        end do
      end do

      ! Keep outside-network cells distinct from in-network cells that remain
      ! below the split threshold; -999 is only an internal unassigned marker.
      basin(:,:)=-9999
      do iy=1, ny
        do ix=1, nx
          if( nextx(ix,iy)/=-9999 ) basin(ix,iy)=-999
        end do
      end do
      ibsn=0

      do iy=1, ny
        do ix=1, nx
          if( nextx(ix,iy)==-9 .or. nextx(ix,iy)==-10 ) then
            if( uparea(ix,iy)>=upa_thrs )then
              ibsn=ibsn+1
              basin(ix,iy)=ibsn
            else
              basin(ix,iy)=0
            endif
          endif
        end do
      end do
      nbsn=ibsn

      ! Seed segment IDs at cut links, then propagate each seed upstream along
      ! the directed network until a previously labeled cell is reached.
      do iy=1, ny
        do ix=1, nx
          if( nextx(ix,iy)>0 .and. basin(ix,iy)==-999 ) then
            jx=ix
            jy=iy
            do while( nextx(jx,jy)>0 .and. basin(jx,jy)==-999 )
              kx=nextx(jx,jy)
              ky=nexty(jx,jy)
              jx=kx
              jy=ky
            end do
            jbsn=basin(jx,jy)

            jx=ix
            jy=iy
            do while( nextx(jx,jy)>0 .and. basin(jx,jy)==-999 )
              basin(jx,jy)=jbsn
              kx=nextx(jx,jy)
              ky=nexty(jx,jy)
              jx=kx
              jy=ky
            end do
          endif
        end do
      end do

      ! Stage 1 is unchanged above. Stage 2 may only relabel its zero cells.
      nbsn_stage1=nbsn
      print *, 'Stage 1 subbasins / unassigned cells:', nbsn, count(basin==0)
      if( two_stage )then
        ! Freeze positive IDs and outside-network cells. Reset only zeros to
        ! the internal marker, and make Stage 1 connections terminal boundaries.
        where( basin==0 ) basin=-999
        do iy=1, ny
          do ix=1, nx
            if( basin(ix,iy)/=-999 ) cycle
            jx=nextx(ix,iy)
            jy=nexty(ix,iy)
            if( jx>=1 .and. jx<=nx .and. jy>=1 .and. jy<=ny )then
              if( basin(jx,jy)>0 )then
                nextx(ix,iy)=-9
                nexty(ix,iy)=-9
              else if( uparea(ix,iy)>upa_thrs2 .and. &
                       uparea(jx,jy)>uparea(ix,iy)+upa_thrs2 )then
                nextx(ix,iy)=-9
                nexty(ix,iy)=-9
              endif
            endif
          end do
        end do

        ! Scan order and seed rule match Stage 1; new IDs follow all old IDs.
        ibsn=nbsn_stage1
        do iy=1, ny
          do ix=1, nx
            if( basin(ix,iy)/=-999 ) cycle
            if( nextx(ix,iy)==-9 .or. nextx(ix,iy)==-10 )then
              if( uparea(ix,iy)>=upa_thrs2 )then
                ibsn=ibsn+1
                basin(ix,iy)=ibsn
              else
                basin(ix,iy)=0
              endif
            endif
          end do
        end do
        nbsn=ibsn

        ! Propagate only within the remaining regions, never through frozen IDs.
        do iy=1, ny
          do ix=1, nx
            if( nextx(ix,iy)>0 .and. basin(ix,iy)==-999 )then
              jx=ix
              jy=iy
              do while( nextx(jx,jy)>0 .and. basin(jx,jy)==-999 )
                kx=nextx(jx,jy)
                ky=nexty(jx,jy)
                jx=kx
                jy=ky
              end do
              jbsn=basin(jx,jy)
              jx=ix
              jy=iy
              do while( nextx(jx,jy)>0 .and. basin(jx,jy)==-999 )
                basin(jx,jy)=jbsn
                kx=nextx(jx,jy)
                ky=nexty(jx,jy)
                jx=kx
                jy=ky
              end do
            endif
          end do
        end do
        print *, 'Stage 2 added subbasins / remaining unassigned cells:', nbsn-nbsn_stage1, count(basin==0)
      endif

      ! Legacy ordering arrays are not consumed by current outputs. basin_size
      ! and basin_order are populated, but basin_new is only allocated.
      allocate(basin_size(nbsn))
      allocate(basin_order(nbsn))
      allocate(basin_new(nbsn))
      do iy=1, ny
        do ix=1, nx
          ! Populate the legacy ordering arrays for positive assigned IDs.
          if( nextx(ix,iy)<0 .and. nextx(ix,iy)/=-9999 .and. basin(ix,iy)>0 ) then
            ibsn=basin(ix,iy)
            basin_size(ibsn)=uparea(ix,iy)
            basin_order(ibsn)=ibsn
          endif
        end do
      end do

! ==========


      call set_color

! ==========

      open(11,file=fout_subbsn,form='unformatted',access='direct',recl=4*nx*ny,status='replace')
      write(11,rec=1) basin
      close(11)

      open(11,file=fout_subcol,form='unformatted',access='direct',recl=4*nx*ny,status='replace')
      write(11,rec=1) color
      close(11)


CONTAINS
      subroutine set_color
! Assign display colors by basin adjacency; large basins are colored first.

print *, 'SET_MAP color basin'
      color(:,:)=-9999
      color_max=0
      allocate(bsn_mask(nbsn,4))
      bsn_mask(:,1)=999999
      bsn_mask(:,2)=999999
      bsn_mask(:,3)=-999999
      bsn_mask(:,4)=-999999
      do iy=1, ny
        do ix=1, nx
          if( basin(ix,iy)>0 )then
            ibsn=int( basin(ix,iy) )
            bsn_mask(ibsn,1) = min( bsn_mask(ibsn,1),ix )
            bsn_mask(ibsn,2) = min( bsn_mask(ibsn,2),iy )
            bsn_mask(ibsn,3) = max( bsn_mask(ibsn,3),ix )
            bsn_mask(ibsn,4) = max( bsn_mask(ibsn,4),iy )
          endif
        end do
      end do
      do ibsn=1, nbsn
        if( bsn_mask(ibsn,1)<=1 .or. bsn_mask(ibsn,3)>=nx )then
          bsn_mask(ibsn,1) = 1
          bsn_mask(ibsn,2) = max(bsn_mask(ibsn,2)-1,1 )
          bsn_mask(ibsn,3) = nx
          bsn_mask(ibsn,4) = min(bsn_mask(ibsn,4)+1,ny)
        else
          bsn_mask(ibsn,1) = bsn_mask(ibsn,1)-1
          bsn_mask(ibsn,2) = max(bsn_mask(ibsn,2)-1,1 )
          bsn_mask(ibsn,3) = bsn_mask(ibsn,3)+1
          bsn_mask(ibsn,4) = min(bsn_mask(ibsn,4)+1,ny)
        endif
      end do

! ==================
! = count basin grids num and
! = check color used in neighbour basin
! = (step1: large basin)
! ==================
      do ibsn=1, nbsn
        col_used=0
        grid=0
        do iy=bsn_mask(ibsn,2), bsn_mask(ibsn,4)
          do ix=bsn_mask(ibsn,1), bsn_mask(ibsn,3)
            if( basin(ix,iy)==ibsn )then
              grid=grid+1

              jx=ix
              jy=iy+1
              if( jx>=1 .and. jx<=nx .and. jy>=1 .and. jy<=ny) then
                if( color(jx,jy)>0 ) col_used(color(jx,jy))=1
              endif

              jx=ix+1
              jy=iy
              if( east-west==360 .and. jx>nx ) jx=1
              if( jx>=1 .and. jx<=nx .and. jy>=1 .and. jy<=ny) then
                if( color(jx,jy)>0 ) col_used(color(jx,jy))=1
              endif

              jx=ix
              jy=iy-1
              if( jx>=1 .and. jx<=nx .and. jy>=1 .and. jy<=ny) then
                if( color(jx,jy)>0 ) col_used(color(jx,jy))=1
              endif

              jx=ix-1
              jy=iy
              if( east-west==360 .and. jx==0 ) jx=nx
              if( jx>=1 .and. jx<=nx .and. jy>=1 .and. jy<=ny) then
                if( color(jx,jy)>0 ) col_used(color(jx,jy))=1
              endif

            endif
          end do
        end do
! === decide color for large basin
        if( grid>=20 )then
          icol=2
          do while( col_used(icol)==1 )
            icol=icol+1
          end do
          color_this=icol
          if( color_max<color_this )color_max=color_this

          do iy=bsn_mask(ibsn,2), bsn_mask(ibsn,4)
            do ix=bsn_mask(ibsn,1), bsn_mask(ibsn,3)
              if(basin(ix,iy)==ibsn) color(ix,iy)=color_this
            end do
          end do
        endif
      end do

! ==================
! = (step2: small basin)
! ==================
      do ibsn=1, nbsn
        col_used=0
        grid=0
        do iy=bsn_mask(ibsn,2), bsn_mask(ibsn,4)
          do ix=bsn_mask(ibsn,1), bsn_mask(ibsn,3)
            if( basin(ix,iy)==ibsn )then
              grid=grid+1

              jx=ix
              jy=iy+1
              if( jx>=1 .and. jx<=nx .and. jy>=1 .and. jy<=ny) then
                if( color(jx,jy)>0 ) col_used(color(jx,jy))=1
              endif

              jx=ix+1
              jy=iy
              if( east-west==360 .and. jx>nx ) jx=1
              if( jx>=1 .and. jx<=nx .and. jy>=1 .and. jy<=ny) then
                if( color(jx,jy)>0 ) col_used(color(jx,jy))=1
              endif

              jx=ix
              jy=iy-1
              if( jx>=1 .and. jx<=nx .and. jy>=1 .and. jy<=ny) then
                if( color(jx,jy)>0 ) col_used(color(jx,jy))=1
              endif

              jx=ix-1
              jy=iy
              if( east-west==360 .and. jx==0 ) jx=nx
              if( jx>=1 .and. jx<=nx .and. jy>=1 .and. jy<=ny) then
                if( color(jx,jy)>0 ) col_used(color(jx,jy))=1
              endif

            endif
          end do
        end do
! === decide color for small
        if( grid<20 )then
          icol=2
          do while(col_used(icol)==1)
            icol=icol+1
          end do
          color_this=icol
          if(grid==1) color_this=1
          if(color_max<color_this) color_max=color_this

          do iy=bsn_mask(ibsn,2), bsn_mask(ibsn,4)
            do ix=bsn_mask(ibsn,1), bsn_mask(ibsn,3)
              if(basin(ix,iy)==ibsn) color(ix,iy)=color_this
            end do
          end do
        endif
      end do

      ! Match the subbasin mask in the color product: keep unassigned river
      ! cells distinct from cells outside the modeled network.
      where( basin==0 ) color=0

! ====================
      end subroutine set_color

!!================================================

      end program analysis_subbasin
