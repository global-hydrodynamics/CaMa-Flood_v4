! Shared legacy-compatible display-color assignment for integer region maps.
! Large classes are colored first, then small classes; neighbor colors are avoided.
module map_color_mod
  implicit none
  private
  public :: color_map
contains
  subroutine color_map(class_map,nx,ny,west,east,colors)
    integer,intent(in)  :: nx, ny  !! map grid dimensions
    real,intent(in)     :: west, east  !! longitude bounds; 360 degrees enables wraparound [deg]
    integer,intent(in)  :: class_map(nx,ny)  !! basin or MPI region IDs
    integer,intent(out) :: colors(nx,ny)  !! display palette indices; -9999 outside classes

    ! Class bounds and neighbor-color assignment state.
    integer                  :: nclass  !! highest positive class identifier
    integer                  :: class_id  !! class currently assigned a display color
    integer                  :: ix, iy  !! current map-cell indices
    integer                  :: jx, jy  !! neighboring map-cell indices
    integer                  :: grid  !! number of cells in the current class
    integer                  :: color_this  !! palette index assigned to the current class
    integer                  :: color_max  !! largest palette index assigned so far
    integer                  :: icol  !! candidate palette index
    integer                  :: col_used(10)  !! palette indices already used by adjacent classes
    integer,allocatable      :: class_bounds(:,:)  !! min/max x/y extent for each class

    nclass=maxval(class_map)
    colors=-9999
    if(nclass<1) return


      colors(:,:)=-9999
      color_max=0
      allocate(class_bounds(nclass,4))
      class_bounds(:,1)=999999
      class_bounds(:,2)=999999
      class_bounds(:,3)=-999999
      class_bounds(:,4)=-999999
      do iy=1, ny
        do ix=1, nx
          if( class_map(ix,iy)>0 )then
            class_id=int( class_map(ix,iy) )
            class_bounds(class_id,1) = min( class_bounds(class_id,1),ix )
            class_bounds(class_id,2) = min( class_bounds(class_id,2),iy )
            class_bounds(class_id,3) = max( class_bounds(class_id,3),ix )
            class_bounds(class_id,4) = max( class_bounds(class_id,4),iy )
          endif
        end do
      end do
      do class_id=1, nclass
        if( class_bounds(class_id,1)<=1 .or. class_bounds(class_id,3)>=nx )then
          class_bounds(class_id,1) = 1
          class_bounds(class_id,2) = max(class_bounds(class_id,2)-1,1 )
          class_bounds(class_id,3) = nx
          class_bounds(class_id,4) = min(class_bounds(class_id,4)+1,ny)
        else
          class_bounds(class_id,1) = class_bounds(class_id,1)-1
          class_bounds(class_id,2) = max(class_bounds(class_id,2)-1,1 )
          class_bounds(class_id,3) = class_bounds(class_id,3)+1
          class_bounds(class_id,4) = min(class_bounds(class_id,4)+1,ny)
        endif
      end do

! ==================
! = count cells and occupied neighboring colors (large classes first)
! ==================
      do class_id=1, nclass
        col_used=0
        grid=0
        do iy=class_bounds(class_id,2), class_bounds(class_id,4)
          do ix=class_bounds(class_id,1), class_bounds(class_id,3)
            if( class_map(ix,iy)==class_id )then
              grid=grid+1

              jx=ix
              jy=iy+1
              if( jx>=1 .and. jx<=nx .and. jy>=1 .and. jy<=ny) then
                if( colors(jx,jy)>0 ) col_used(colors(jx,jy))=1
              endif

              jx=ix+1
              jy=iy
              if( east-west==360 .and. jx>nx ) jx=1
              if( jx>=1 .and. jx<=nx .and. jy>=1 .and. jy<=ny) then
                if( colors(jx,jy)>0 ) col_used(colors(jx,jy))=1
              endif

              jx=ix
              jy=iy-1
              if( jx>=1 .and. jx<=nx .and. jy>=1 .and. jy<=ny) then
                if( colors(jx,jy)>0 ) col_used(colors(jx,jy))=1
              endif

              jx=ix-1
              jy=iy
              if( east-west==360 .and. jx==0 ) jx=nx
              if( jx>=1 .and. jx<=nx .and. jy>=1 .and. jy<=ny) then
                if( colors(jx,jy)>0 ) col_used(colors(jx,jy))=1
              endif

            endif
          end do
        end do
! === choose colors for large classes
        if( grid>=20 )then
! Select the first unused palette entry; if exhausted, use color 10.
          icol=2
          do
            if( col_used(icol)/=1 ) exit
            if( icol==10 ) exit
            icol=icol+1
          end do
          color_this=icol
          if( color_max<color_this )color_max=color_this

          do iy=class_bounds(class_id,2), class_bounds(class_id,4)
            do ix=class_bounds(class_id,1), class_bounds(class_id,3)
              if(class_map(ix,iy)==class_id) colors(ix,iy)=color_this
            end do
          end do
        endif
      end do

! ==================
! = assign colors to the remaining small classes
! ==================
      do class_id=1, nclass
        col_used=0
        grid=0
        do iy=class_bounds(class_id,2), class_bounds(class_id,4)
          do ix=class_bounds(class_id,1), class_bounds(class_id,3)
            if( class_map(ix,iy)==class_id )then
              grid=grid+1

              jx=ix
              jy=iy+1
              if( jx>=1 .and. jx<=nx .and. jy>=1 .and. jy<=ny) then
                if( colors(jx,jy)>0 ) col_used(colors(jx,jy))=1
              endif

              jx=ix+1
              jy=iy
              if( east-west==360 .and. jx>nx ) jx=1
              if( jx>=1 .and. jx<=nx .and. jy>=1 .and. jy<=ny) then
                if( colors(jx,jy)>0 ) col_used(colors(jx,jy))=1
              endif

              jx=ix
              jy=iy-1
              if( jx>=1 .and. jx<=nx .and. jy>=1 .and. jy<=ny) then
                if( colors(jx,jy)>0 ) col_used(colors(jx,jy))=1
              endif

              jx=ix-1
              jy=iy
              if( east-west==360 .and. jx==0 ) jx=nx
              if( jx>=1 .and. jx<=nx .and. jy>=1 .and. jy<=ny) then
                if( colors(jx,jy)>0 ) col_used(colors(jx,jy))=1
              endif

            endif
          end do
        end do
! === choose a color for small classes
        if( grid<20 )then
! Use the same palette search for small classes; exhausted palettes use color 10.
          icol=2
          do
            if( col_used(icol)/=1 ) exit
            if( icol==10 ) exit
            icol=icol+1
          end do
          color_this=icol
          if(grid==1) color_this=1
          if(color_max<color_this) color_max=color_this

          do iy=class_bounds(class_id,2), class_bounds(class_id,4)
            do ix=class_bounds(class_id,1), class_bounds(class_id,3)
              if(class_map(ix,iy)==class_id) colors(ix,iy)=color_this
            end do
          end do
        endif
      end do

! ====================
  end subroutine color_map
end module map_color_mod
