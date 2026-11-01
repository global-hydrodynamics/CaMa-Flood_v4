program param_water_calc_prmwat
! ================================================================
! Estimate permanent-water surface area in each unit catchment.
!
! The result is an optional post-processing map, not a simulation input.
! Count permanent water only within terrain elevations where CaMa-Flood can
! reproduce inundation. Isolated lakes high on hills are outside that modeled
! flood extent and must not be subtracted as pre-existing inundated area.
! High-resolution pixels are selected with the legacy rules:
!   catmXX > 0, (rivwth > 0 or rivwth == -1), and flddif <= 5 m.
! In particular, rivwth == -1 means a water-surface pixel away from a
! river centerline. The maintainer described the intended elevation criterion
! as flddif < 5 m; legacy v4 and this migration use <= 5 m, so equality is
! currently included pending clarification. The result is accumulated in m2
! and capped by ctmare.
! ================================================================
use, intrinsic :: iso_fortran_env, only: int16, int32, int64, real32, real64, error_unit
use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
implicit none

! ------------------------------------------------
! Command-line configuration and map geometry
! ------------------------------------------------
integer, parameter :: PATH_LEN = 1024                 ! Maximum input path or area-name length.
integer, parameter :: MISSING_INT = -9999             ! Stored missing value in integer map files.
real(real32), parameter :: MISSING_REAL = -9999.0_real32 ! Stored missing value in float map files.
character(len=PATH_LEN) :: hires_tag                  ! Explicit high-resolution identifier, e.g. 1min.
character(len=PATH_LEN) :: hires_dir                  ! Directory containing high-resolution inputs.
integer :: cells_per_degree                           ! Resolution lookup used to validate location.txt.
real(real64) :: hires_cell_size                       ! High-resolution cell size in degrees.

integer :: nx, ny                                     ! Base-map columns and rows.
integer :: nfpl                                       ! Floodplain-layer count from params.txt.
integer :: target_x, target_y                         ! Catchment indices receiving an area sum.
real(real32) :: map_grid_size                         ! Base-map grid spacing in degrees.
real(real32) :: map_west, map_east                    ! Base-map longitude bounds in degrees.
real(real32) :: map_south, map_north                  ! Base-map latitude bounds in degrees.
integer(int64) :: map_record_cells                    ! Number of cells in one base-map record.
integer(int64) :: map_float_record_bytes              ! Byte count for one float32 base-map record.
integer(int64) :: map_integer_record_bytes            ! Byte count for one int32 base-map record.
integer :: map_float_recl                             ! Fortran direct-access RECL for float32 maps.
integer :: map_integer_recl                           ! Fortran direct-access RECL for int32 maps.

! ------------------------------------------------
! Base-resolution river network and output
! ------------------------------------------------
integer(int32), allocatable :: nextx(:,:)             ! nextxy record 1; -9999 marks non-network cells.
real(real32), allocatable :: catchment_area(:,:)      ! Unit-catchment area from ctmare.bin, in m2.
real(real32), allocatable :: prmwat(:,:)              ! Aggregated permanent-water area, in m2.

! ------------------------------------------------
! High-resolution area metadata and input arrays
! ------------------------------------------------
integer :: narea, area_index                         ! Number of source areas and current list row.
integer :: nx0, ny0                                  ! Source-area columns and rows.
integer :: ix0, iy0                                  ! Current high-resolution pixel indices.
character(len=PATH_LEN) :: area_name                 ! File-name tag for this source area.
character(len=PATH_LEN) :: area_id                   ! Numeric area identifier from location.txt.
character(len=PATH_LEN) :: unused_header             ! Descriptive location.txt column header.
real(real32) :: west0, east0                         ! Source-area longitude bounds, read as legacy real32.
real(real32) :: south0, north0                       ! Source-area latitude bounds, read as legacy real32.
real(real64) :: location_cell_size                   ! Cell size recorded for the current area, degrees.
integer(int64) :: area_cells                         ! Number of pixels in the current area.
integer(int64) :: area_float_record_bytes            ! Bytes in one float32 source-area record.
integer(int64) :: area_integer_record_bytes          ! Bytes in one int16 catmxy record.
integer :: area_float_recl                           ! Direct-access RECL for float32 source maps.
integer :: area_integer_recl                         ! Direct-access RECL for int16 catmxy maps.

integer(int16), allocatable :: catm_x(:,:)           ! Base-map x index for each high-resolution pixel.
integer(int16), allocatable :: catm_y(:,:)           ! Base-map y index for each high-resolution pixel.
real(real32), allocatable :: rivwth(:,:)             ! Width/class layer; -1 marks non-centerline water.
real(real32), allocatable :: pixel_area_km2(:,:)     ! High-resolution pixel area in km2.
real(real32), allocatable :: flddif(:,:)             ! Height above catchment reference river, in m.
real(real64), allocatable :: center_latitude(:)       ! Latitude at the center of each source row, degrees.
real(real64), allocatable :: fallback_area_km2(:)     ! Legacy fallback pixel area by source row, km2.

! ------------------------------------------------
! File units and file names
! ------------------------------------------------
integer :: location_unit, data_unit, output_unit     ! Active formatted, input-binary, and output units.
logical :: location_open, data_open, output_open     ! Track open units for error cleanup.
character(len=PATH_LEN) :: filename                  ! Current input path or formatted diagnostic.
character(len=PATH_LEN) :: params_file               ! Base-map dimensions and geographic metadata.
character(len=PATH_LEN) :: nextxy_file               ! Base river-network mask input.
character(len=PATH_LEN) :: ctmare_file               ! Base unit-catchment area input.
character(len=PATH_LEN) :: location_file             ! High-resolution source-area listing.
character(len=PATH_LEN) :: output_file               ! Aggregated permanent-water output path.
integer :: io_status                                 ! Fortran I/O and allocation status.
integer(int64) :: file_size                           ! Actual binary file size in bytes.
logical :: file_exists                                ! Optional grdare.bin presence.

! ------------------------------------------------
! Counts reported after processing
! ------------------------------------------------
integer(int64) :: valid_catchment_count              ! Network cells initialized for output.
integer(int64) :: selected_pixel_count                ! Pixels contributing to permanent-water area.
integer(int64) :: capped_catchment_count              ! Cells limited to their ctmare area.
integer(int64) :: missing_flddif_selected_count       ! Selected pixels with flddif=-9999.
logical :: used_grdare_fallback                       ! Whether any area used legacy rgetarea fallback.

! Geometry and binary-size checks must complete before allocation.
location_open = .false.
data_open = .false.
output_open = .false.
valid_catchment_count = 0_int64
selected_pixel_count = 0_int64
capped_catchment_count = 0_int64
missing_flddif_selected_count = 0_int64
used_grdare_fallback = .false.

call get_command_argument(1, hires_tag, status=io_status)
if (io_status /= 0 .or. len_trim(hires_tag) == 0) then
    call fail('Usage: param_water_calc_prmwat HIRES_TAG (1min, 30sec, 15sec, 5sec, 3sec, or 1sec)')
end if

call select_resolution(trim(hires_tag), cells_per_degree, hires_cell_size)

hires_dir = './' // trim(hires_tag) // '/'
params_file = './params.txt'
nextxy_file = './nextxy.bin'
ctmare_file = './ctmare.bin'
location_file = trim(hires_dir) // 'location.txt'
output_file = './prmwat.bin'

! ================================================================
! [1] Read the base map geometry and validate its binary inputs
! ================================================================
open(newunit=location_unit, file=params_file, status='old', action='read', &
     form='formatted', iostat=io_status)
if (io_status /= 0) call fail('Cannot open ' // trim(params_file))
location_open = .true.

read(location_unit, *, iostat=io_status) nx
if (io_status /= 0) call fail('Cannot read NX from ' // trim(params_file))
read(location_unit, *, iostat=io_status) ny
if (io_status /= 0) call fail('Cannot read NY from ' // trim(params_file))
read(location_unit, *, iostat=io_status) nfpl
if (io_status /= 0) call fail('Cannot read floodplain layer count from ' // trim(params_file))
read(location_unit, *, iostat=io_status) map_grid_size
if (io_status /= 0) call fail('Cannot read map grid size from ' // trim(params_file))
read(location_unit, *, iostat=io_status) map_west
if (io_status /= 0) call fail('Cannot read western bound from ' // trim(params_file))
read(location_unit, *, iostat=io_status) map_east
if (io_status /= 0) call fail('Cannot read eastern bound from ' // trim(params_file))
read(location_unit, *, iostat=io_status) map_south
if (io_status /= 0) call fail('Cannot read southern bound from ' // trim(params_file))
read(location_unit, *, iostat=io_status) map_north
if (io_status /= 0) call fail('Cannot read northern bound from ' // trim(params_file))
close(location_unit, iostat=io_status)
location_open = .false.
if (io_status /= 0) call fail('Cannot close ' // trim(params_file))

if (nx <= 0 .or. ny <= 0 .or. nfpl < 0) then
    call fail('Invalid NX, NY, or floodplain layer count in ' // trim(params_file))
end if
if (.not. ieee_is_finite(map_grid_size) .or. map_grid_size <= 0.0_real32) then
    call fail('Invalid map grid size in ' // trim(params_file))
end if
if (.not. ieee_is_finite(map_west) .or. .not. ieee_is_finite(map_east) .or. &
    .not. ieee_is_finite(map_south) .or. .not. ieee_is_finite(map_north)) then
    call fail('Nonfinite map bounds in ' // trim(params_file))
end if

map_record_cells = int(nx, int64) * int(ny, int64)
map_float_record_bytes = map_record_cells * int(storage_size(0.0_real32) / 8, int64)
map_integer_record_bytes = map_record_cells * int(storage_size(0_int32) / 8, int64)
if (map_float_record_bytes > int(huge(map_float_recl), int64) .or. &
    map_integer_record_bytes > int(huge(map_integer_recl), int64)) then
    call fail('Map dimensions exceed supported direct-access record size')
end if
map_float_recl = int(map_float_record_bytes)
map_integer_recl = int(map_integer_record_bytes)

call allocate_base_arrays()

! nextxy.bin has two int32 records; only record 1 is needed as the legacy
! river-network mask, but the complete standard two-record file is checked.
call open_exact_direct(nextxy_file, map_integer_recl, 2_int64, data_unit)
read(data_unit, rec=1, iostat=io_status) nextx
if (io_status /= 0) call fail('Incomplete read of record 1 from ' // trim(nextxy_file))
call close_data_file(nextxy_file)

call open_exact_direct(ctmare_file, map_float_recl, 1_int64, data_unit)
read(data_unit, rec=1, iostat=io_status) catchment_area
if (io_status /= 0) call fail('Incomplete read of record 1 from ' // trim(ctmare_file))
call close_data_file(ctmare_file)

prmwat = MISSING_REAL
do iy0 = 1, ny
    do ix0 = 1, nx
        if (nextx(ix0, iy0) /= MISSING_INT) then
            if (.not. ieee_is_finite(catchment_area(ix0, iy0)) .or. &
                catchment_area(ix0, iy0) <= 0.0_real32) then
                write(filename, '(a,i0,a,i0,a)') 'Invalid ctmare at map cell (', ix0, ',', iy0, ')'
                call fail(trim(filename) // ' in ' // trim(ctmare_file))
            end if
            prmwat(ix0, iy0) = 0.0_real32
            valid_catchment_count = valid_catchment_count + 1_int64
        end if
    end do
end do

! ================================================================
! [2] Read each non-tiled high-resolution area and accumulate water area
! ================================================================
open(newunit=location_unit, file=location_file, status='old', action='read', &
     form='formatted', iostat=io_status)
if (io_status /= 0) call fail('Cannot open ' // trim(location_file))
location_open = .true.

read(location_unit, *, iostat=io_status) narea
if (io_status /= 0 .or. narea <= 0) call fail('Invalid area count in ' // trim(location_file))
read(location_unit, '(a)', iostat=io_status) unused_header
if (io_status /= 0) call fail('Cannot read column header from ' // trim(location_file))

do area_index = 1, narea
    read(location_unit, *, iostat=io_status) area_id, area_name, west0, east0, &
         south0, north0, nx0, ny0, location_cell_size
    if (io_status /= 0) then
        write(filename, '(a,i0,a)') 'Cannot read area record ', area_index, ' from '
        call fail(trim(filename) // trim(location_file))
    end if

    if (.not. safe_area_name(trim(area_name))) then
        call fail('Unsafe area name in ' // trim(location_file) // ': ' // trim(area_name))
    end if
    if (nx0 <= 0 .or. ny0 <= 0) then
        call fail('Invalid dimensions for high-resolution area ' // trim(area_name))
    end if
    if (.not. ieee_is_finite(west0) .or. .not. ieee_is_finite(east0) .or. &
        .not. ieee_is_finite(south0) .or. .not. ieee_is_finite(north0) .or. &
        .not. ieee_is_finite(location_cell_size)) then
        call fail('Nonfinite geometry for high-resolution area ' // trim(area_name))
    end if
    if (location_cell_size <= 0.0_real64 .or. &
        abs(location_cell_size - hires_cell_size) > 1.0e-8_real64) then
        call fail('Resolution in location.txt does not match HIRES_TAG for area ' // trim(area_name))
    end if

    area_cells = int(nx0, int64) * int(ny0, int64)
    area_integer_record_bytes = area_cells * int(storage_size(0_int16) / 8, int64)
    area_float_record_bytes = area_cells * int(storage_size(0.0_real32) / 8, int64)
    if (area_integer_record_bytes > int(huge(area_integer_recl), int64) .or. &
        area_float_record_bytes > int(huge(area_float_recl), int64)) then
        call fail('High-resolution dimensions exceed supported record size in area ' // trim(area_name))
    end if
    area_integer_recl = int(area_integer_record_bytes)
    area_float_recl = int(area_float_record_bytes)

    call allocate_area_arrays()

    filename = trim(hires_dir) // trim(area_name) // '.catmxy.bin'
    call open_exact_direct(filename, area_integer_recl, 2_int64, data_unit)
    read(data_unit, rec=1, iostat=io_status) catm_x
    if (io_status /= 0) call fail('Incomplete catmxy record 1 in ' // trim(filename))
    read(data_unit, rec=2, iostat=io_status) catm_y
    if (io_status /= 0) call fail('Incomplete catmxy record 2 in ' // trim(filename))
    call close_data_file(filename)

    filename = trim(hires_dir) // trim(area_name) // '.rivwth.bin'
    call open_exact_direct(filename, area_float_recl, 1_int64, data_unit)
    read(data_unit, rec=1, iostat=io_status) rivwth
    if (io_status /= 0) call fail('Incomplete rivwth record 1 in ' // trim(filename))
    call close_data_file(filename)

    filename = trim(hires_dir) // trim(area_name) // '.grdare.bin'
    inquire(file=trim(filename), exist=file_exists, iostat=io_status)
    if (io_status /= 0) call fail('Cannot inspect ' // trim(filename))
    if (file_exists) then
        call open_exact_direct(filename, area_float_recl, 1_int64, data_unit)
        read(data_unit, rec=1, iostat=io_status) pixel_area_km2
        if (io_status /= 0) call fail('Incomplete grdare record 1 in ' // trim(filename))
        call close_data_file(filename)
    else
        used_grdare_fallback = .true.
        call calculate_fallback_pixel_area()
        write(*, '(a)') 'WARNING: missing ' // trim(filename) // &
             '; using the legacy scaled rgetarea fallback.'
    end if

    filename = trim(hires_dir) // trim(area_name) // '.flddif.bin'
    call open_exact_direct(filename, area_float_recl, 1_int64, data_unit)
    read(data_unit, rec=1, iostat=io_status) flddif
    if (io_status /= 0) call fail('Incomplete flddif record 1 in ' // trim(filename))
    call close_data_file(filename)

    do iy0 = 1, ny0
        do ix0 = 1, nx0
            if (catm_x(ix0, iy0) > 0_int16) then
                call validate_catchment_assignment(ix0, iy0, trim(area_name))

                if (.not. ieee_is_finite(rivwth(ix0, iy0))) then
                    call report_area_pixel_error('Nonfinite rivwth', area_name, ix0, iy0)
                end if
                if (.not. ieee_is_finite(flddif(ix0, iy0))) then
                    call report_area_pixel_error('Nonfinite flddif', area_name, ix0, iy0)
                end if

                ! Only count permanent water within modeled inundation terrain;
                ! elevated isolated lakes must not be subtracted as flood area.
                if (rivwth(ix0, iy0) > 0.0_real32 .or. rivwth(ix0, iy0) == -1.0_real32) then
                    ! The maintainer described flddif < 5 m. Preserve legacy
                    ! <= behavior for now; the exact-5 m boundary is unresolved.
                    if (flddif(ix0, iy0) <= 5.0_real32) then
                        if (flddif(ix0, iy0) == MISSING_REAL) then
                            missing_flddif_selected_count = missing_flddif_selected_count + 1_int64
                        end if
                        if (.not. ieee_is_finite(pixel_area_km2(ix0, iy0)) .or. &
                            pixel_area_km2(ix0, iy0) <= 0.0_real32) then
                            call report_area_pixel_error('Invalid grdare area for selected water pixel', &
                                                         area_name, ix0, iy0)
                        end if

                        target_x = int(catm_x(ix0, iy0))
                        target_y = int(catm_y(ix0, iy0))
                        prmwat(target_x, target_y) = prmwat(target_x, target_y) + &
                                                     pixel_area_km2(ix0, iy0) * 1000000.0_real32
                        if (.not. ieee_is_finite(prmwat(target_x, target_y))) then
                            call report_area_pixel_error('Nonfinite accumulated permanent-water area', &
                                                         area_name, ix0, iy0)
                        end if
                        selected_pixel_count = selected_pixel_count + 1_int64
                    end if
                end if
            end if
        end do
    end do

    if (allocated(catm_x)) deallocate(catm_x)
    if (allocated(catm_y)) deallocate(catm_y)
    if (allocated(rivwth)) deallocate(rivwth)
    if (allocated(pixel_area_km2)) deallocate(pixel_area_km2)
    if (allocated(flddif)) deallocate(flddif)
    if (allocated(center_latitude)) deallocate(center_latitude)
    if (allocated(fallback_area_km2)) deallocate(fallback_area_km2)
end do
close(location_unit, iostat=io_status)
location_open = .false.
if (io_status /= 0) call fail('Cannot close ' // trim(location_file))

if (missing_flddif_selected_count > 0_int64) then
    write(*, '(a,i0,a)') 'WARNING: ', missing_flddif_selected_count, &
         ' selected pixels have flddif=-9999; legacy flddif<=5 selection is preserved.'
end if

! ================================================================
! [3] Cap by unit-catchment area and write the single float32 record
! ================================================================
do iy0 = 1, ny
    do ix0 = 1, nx
        if (nextx(ix0, iy0) /= MISSING_INT) then
            if (prmwat(ix0, iy0) > catchment_area(ix0, iy0)) then
                prmwat(ix0, iy0) = min(prmwat(ix0, iy0), catchment_area(ix0, iy0))
                capped_catchment_count = capped_catchment_count + 1_int64
            end if
        end if
    end do
end do

open(newunit=output_unit, file=output_file, status='replace', action='write', &
     access='direct', form='unformatted', recl=map_float_recl, iostat=io_status)
if (io_status /= 0) call fail('Cannot create ' // trim(output_file))
output_open = .true.
write(output_unit, rec=1, iostat=io_status) prmwat
if (io_status /= 0) call fail('Cannot write record 1 to ' // trim(output_file))
close(output_unit, iostat=io_status)
output_open = .false.
if (io_status /= 0) call fail('Cannot close ' // trim(output_file))

write(*, '(a,i0)') 'Processed high-resolution areas: ', narea
write(*, '(a,i0)') 'Valid unit catchments: ', valid_catchment_count
write(*, '(a,i0)') 'Selected permanent-water pixels: ', selected_pixel_count
write(*, '(a,i0)') 'Catchments capped at ctmare: ', capped_catchment_count
write(*, '(a,l1)') 'Used grdare fallback: ', used_grdare_fallback
write(*, '(a)') 'Wrote prmwat.bin (float32, one NX x NY record, units m2).'

call cleanup_arrays()

contains

subroutine select_resolution(tag, count_per_degree, pixel_size)
    character(len=*), intent(in) :: tag
    integer, intent(out) :: count_per_degree
    real(real64), intent(out) :: pixel_size

    select case (trim(tag))
    case ('1min')
        count_per_degree = 60
    case ('30sec')
        count_per_degree = 120
    case ('15sec')
        count_per_degree = 240
    case ('5sec')
        count_per_degree = 720
    case ('3sec')
        count_per_degree = 1200
    case ('1sec')
        count_per_degree = 3600
    case default
        call fail('Unsupported HIRES_TAG: ' // trim(tag))
    end select
    pixel_size = 1.0_real64 / real(count_per_degree, real64)
end subroutine select_resolution

subroutine allocate_base_arrays()
    integer :: stat

    allocate(nextx(nx, ny), catchment_area(nx, ny), prmwat(nx, ny), stat=stat)
    if (stat /= 0) call fail('Cannot allocate base-map arrays for params.txt dimensions')
end subroutine allocate_base_arrays

subroutine allocate_area_arrays()
    integer :: stat

    allocate(catm_x(nx0, ny0), catm_y(nx0, ny0), rivwth(nx0, ny0), &
             pixel_area_km2(nx0, ny0), flddif(nx0, ny0), stat=stat)
    if (stat /= 0) call fail('Cannot allocate high-resolution arrays for area ' // trim(area_name))
end subroutine allocate_area_arrays

subroutine open_exact_direct(path, record_length, record_count, unit_number)
    character(len=*), intent(in) :: path
    integer, intent(in) :: record_length
    integer(int64), intent(in) :: record_count
    integer, intent(out) :: unit_number
    integer(int64) :: expected_size

    if (record_length <= 0 .or. record_count <= 0_int64) then
        call fail('Invalid expected binary layout for ' // trim(path))
    end if
    expected_size = int(record_length, int64) * record_count
    open(newunit=unit_number, file=path, status='old', action='read', &
         access='direct', form='unformatted', recl=record_length, iostat=io_status)
    if (io_status /= 0) call fail('Cannot open required input ' // trim(path))
    data_open = .true.
    inquire(unit=unit_number, size=file_size, iostat=io_status)
    if (io_status /= 0) call fail('Cannot inspect binary size for ' // trim(path))
    if (file_size /= expected_size) then
        write(filename, '(a,i0,a,i0)') 'Wrong size for ', file_size, ' bytes; expected ', expected_size
        call fail(trim(path) // ': ' // trim(filename))
    end if
end subroutine open_exact_direct

subroutine close_data_file(path)
    character(len=*), intent(in) :: path

    close(data_unit, iostat=io_status)
    data_open = .false.
    if (io_status /= 0) call fail('Cannot close ' // trim(path))
end subroutine close_data_file

subroutine validate_catchment_assignment(x_index, y_index, source_area)
    integer, intent(in) :: x_index, y_index
    character(len=*), intent(in) :: source_area
    integer :: target_x, target_y

    target_x = int(catm_x(x_index, y_index))
    target_y = int(catm_y(x_index, y_index))
    if (target_x < 1 .or. target_x > nx .or. target_y < 1 .or. target_y > ny) then
        call report_area_pixel_error('catmxy assignment is outside the base map', &
                                     source_area, x_index, y_index)
    end if
    if (nextx(target_x, target_y) == MISSING_INT) then
        call report_area_pixel_error('catmxy assignment points outside the river network', &
                                     source_area, x_index, y_index)
    end if
end subroutine validate_catchment_assignment

subroutine report_area_pixel_error(condition, source_area, x_index, y_index)
    character(len=*), intent(in) :: condition, source_area
    integer, intent(in) :: x_index, y_index
    character(len=PATH_LEN) :: message

    write(message, '(a,a,a,a,i0,a,i0,a)') trim(condition), ' in area ', trim(source_area), &
         ' at pixel (', x_index, ',', y_index, ')'
    call fail(trim(message))
end subroutine report_area_pixel_error

subroutine calculate_fallback_pixel_area()
    integer :: stat, row

    allocate(center_latitude(ny0), fallback_area_km2(ny0), stat=stat)
    if (stat /= 0) call fail('Cannot allocate fallback pixel-area arrays for area ' // trim(area_name))

    do row = 1, ny0
        ! Preserve the legacy single-precision index expression and its
        ! 10-by-10 rectangle scaling before conversion back to float32 km2.
        center_latitude(row) = north0 - hires_cell_size * &
                               (real(row, real32) - 0.5_real32)
        fallback_area_km2(row) = legacy_rgetarea(0.0_real64, 10.0_real32 * hires_cell_size, &
            center_latitude(row) + 5.0_real32 * hires_cell_size, &
            center_latitude(row) - 5.0_real32 * hires_cell_size) * 0.01_real32 * 1.0e-6_real32
        if (.not. ieee_is_finite(fallback_area_km2(row)) .or. fallback_area_km2(row) <= 0.0_real64) then
            call fail('Invalid fallback pixel area for area ' // trim(area_name))
        end if
    end do

    do row = 1, ny0
        do ix0 = 1, nx0
            if (catm_x(ix0, row) /= int(MISSING_INT, int16)) then
                pixel_area_km2(ix0, row) = real(fallback_area_km2(row), real32)
            else
                pixel_area_km2(ix0, row) = MISSING_REAL
            end if
        end do
    end do
end subroutine calculate_fallback_pixel_area

real(real64) function legacy_rgetarea(lon_west, lon_east, lat_north, lat_south)
    real(real64), intent(in) :: lon_west, lon_east, lat_north, lat_south
    real(real32), parameter :: rpi = 3.141592653589793238462643383_real32
    real(real64), parameter :: dpi = 3.141592653589793238462643383_real64
    real(real64), parameter :: eccentricity_squared = 0.00669447_real64
    real(real64), parameter :: earth_radius_m = 6378136.0_real64
    real(real64) :: eccentricity
    real(real64) :: sine_north, sine_south
    real(real64) :: function_north, function_south
    real(real64) :: clipped_north, clipped_south

    ! This is the legacy Oki/Kanae approximation, including its real32 cast.
    eccentricity = sqrt(eccentricity_squared)
    clipped_north = min(max(lat_north, -90.0_real64), 90.0_real64)
    clipped_south = min(max(lat_south, -90.0_real64), 90.0_real64)
    sine_north = sin(clipped_north * rpi / 180.0_real32)
    sine_south = sin(clipped_south * rpi / 180.0_real32)
    function_north = sine_north * (1.0_real64 + (eccentricity * sine_north)**2 / 2.0_real64)
    function_south = sine_south * (1.0_real64 + (eccentricity * sine_south)**2 / 2.0_real64)
    legacy_rgetarea = real(dpi * earth_radius_m**2 * (1.0_real64 - eccentricity**2) / &
                           180.0_real64 * (function_north - function_south), real32) * &
                      (lon_east - lon_west)
    if (legacy_rgetarea < 0.0_real64) legacy_rgetarea = -legacy_rgetarea
end function legacy_rgetarea

logical function safe_area_name(name)
    character(len=*), intent(in) :: name
    safe_area_name = len_trim(name) > 0 .and. trim(name) /= '.' .and. trim(name) /= '..'
    if (index(name, '/') > 0 .or. index(name, achar(92)) > 0) safe_area_name = .false.
end function safe_area_name

subroutine fail(message)
    character(len=*), intent(in) :: message

    write(error_unit, '(a)') 'ERROR: ' // trim(message)
    call cleanup_files()
    call cleanup_arrays()
    error stop 1
end subroutine fail

subroutine cleanup_files()
    integer :: close_status

    if (location_open) then
        close(location_unit, iostat=close_status)
        location_open = .false.
    end if
    if (data_open) then
        close(data_unit, iostat=close_status)
        data_open = .false.
    end if
    if (output_open) then
        close(output_unit, status='delete', iostat=close_status)
        output_open = .false.
    end if
end subroutine cleanup_files

subroutine cleanup_arrays()
    if (allocated(nextx)) deallocate(nextx)
    if (allocated(catchment_area)) deallocate(catchment_area)
    if (allocated(prmwat)) deallocate(prmwat)
    if (allocated(catm_x)) deallocate(catm_x)
    if (allocated(catm_y)) deallocate(catm_y)
    if (allocated(rivwth)) deallocate(rivwth)
    if (allocated(pixel_area_km2)) deallocate(pixel_area_km2)
    if (allocated(flddif)) deallocate(flddif)
    if (allocated(center_latitude)) deallocate(center_latitude)
    if (allocated(fallback_area_km2)) deallocate(fallback_area_km2)
end subroutine cleanup_arrays

end program param_water_calc_prmwat
