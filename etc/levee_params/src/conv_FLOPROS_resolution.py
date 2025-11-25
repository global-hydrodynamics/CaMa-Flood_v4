"""
Downsample a regular lat-lon GeoTIFF onto a CaMa-style grid defined in
map/params.txt by simple nearest-neighbor decimation (no reprojection),
apply an ocean mask from uparea.bin, and also output a plain binary file
(ny * nx float32, GRaDS order) for the first band.

Ocean mask:
  - uparea.bin is a float32 (ny*nx) GRaDS-style binary at the target grid.
  - Grid cells where uparea == -9999 are treated as ocean.
  - For those cells, the output value is forced to -9999 in both:
      * the GeoTIFF (all bands)
      * the plain binary for band 1.

The plain binary is written in GRaDS-compatible order:

  - Start from the north-west corner (upper-left).
  - X (longitude) varies fastest from west to east.
  - Then Y (latitude) increases from north to south (row by row).

Usage:
    python resample_from_params.py INPUT_TIF OUTPUT_TIF OUTPUT_BIN PARAMS_TXT [UPAREA_BIN]

If UPAREA_BIN is omitted, the script looks for "uparea.bin" in the same
directory as PARAMS_TXT.
"""

import sys
import os
import numpy as np
import rasterio
from rasterio.transform import Affine


def read_params(path):
    """Read CaMa-style params.txt and return nx, ny, gsize, west, east, south, north."""
    with open(path, "r") as f:
        # Keep non-empty lines only
        lines = [l.strip() for l in f.readlines() if l.strip()]

    if len(lines) < 8:
        raise ValueError("params.txt must have at least 8 non-empty lines.")

    def first_token(line: str) -> str:
        # Take the first token before any comment
        return line.split()[0]

    nx    = int(first_token(lines[0]))
    ny    = int(first_token(lines[1]))
    # nlyr = int(first_token(lines[2]))   # not used in this script
    gsize = float(first_token(lines[3]))
    west  = float(first_token(lines[4]))
    east  = float(first_token(lines[5]))
    south = float(first_token(lines[6]))
    north = float(first_token(lines[7]))

    return nx, ny, gsize, west, east, south, north


def main():
    # ------------------------------------------------------------------
    # Argument parsing
    # ------------------------------------------------------------------
    if len(sys.argv) not in (5, 6):
        print("Error: Incorrect number of arguments.")
        print(__doc__)
        sys.exit(1)

    in_tif      = sys.argv[1]
    out_tif     = sys.argv[2]
    out_bin     = sys.argv[3]
    params_path = sys.argv[4]

    # Default uparea path: same directory as params.txt
    if len(sys.argv) == 6:
        uparea_path = sys.argv[5]
    else:
        params_dir = os.path.dirname(os.path.abspath(params_path))
        uparea_path = os.path.join(params_dir, "uparea.bin")

    print("Input GeoTIFF   :", in_tif)
    print("Output GeoTIFF  :", out_tif)
    print("Output binary   :", out_bin)
    print("Params file     :", params_path)
    print("Uparea binary   :", uparea_path)

    # ------------------------------------------------------------------
    # 1. Read target grid definition from params.txt
    # ------------------------------------------------------------------
    nx, ny, gsize, west, east, south, north = read_params(params_path)

    print(f"Target nx, ny   : {nx}, {ny}")
    print(f"Target grid size (deg) : {gsize}")
    print(f"Target extent (west, east, south, north): {west}, {east}, {south}, {north}")

    # ------------------------------------------------------------------
    # 2. Open source GeoTIFF and inspect its grid
    # ------------------------------------------------------------------
    with rasterio.open(in_tif) as src:
        src_profile   = src.profile.copy()
        src_transform = src.transform
        src_crs       = src.crs

        print("Source CRS      :", src_crs)
        print("Source width, height:", src.width, src.height)
        print("Source transform:", src_transform)

        # Extract basic georeferencing info
        src_dx = src_transform.a           # pixel width  (deg)
        src_dy = src_transform.e           # pixel height (deg, usually negative for north-up)
        src_west  = src_transform.c        # x coord of upper-left corner
        src_north = src_transform.f        # y coord of upper-left corner

        if src_dy >= 0.0:
            raise ValueError("This script assumes north-up images (transform.e < 0).")

        # Compute integer decimation factor
        fx = gsize / src_dx
        fy = gsize / abs(src_dy)

        fx_int = int(round(fx))
        fy_int = int(round(fy))

        print(f"Source dx, dy   : {src_dx}, {src_dy}")
        print(f"Nominal factors : fx={fx}, fy={fy}")
        print(f"Rounded factors : fx_int={fx_int}, fy_int={fy_int}")

        if abs(fx - fx_int) > 1e-6 or abs(fy - fy_int) > 1e-6:
            raise ValueError("Target grid size is not an integer multiple of source grid size.")

        # Compute starting indices in the source grid corresponding to
        # the north-west corner of the target domain (west, north).
        col0 = int(round((west  - src_west)  / src_dx))
        row0 = int(round((src_north - north) / abs(src_dy)))

        print(f"Source start indices (row0, col0): {row0}, {col0}")

        # Check bounds
        if col0 < 0 or row0 < 0:
            raise ValueError("Target west/north is outside source domain.")
        if col0 + nx * fx_int > src.width or row0 + ny * fy_int > src.height:
            raise ValueError("Target domain exceeds source domain.")

        # Read all bands
        src_data = src.read()  # shape: (bands, height, width)

    # ------------------------------------------------------------------
    # 3. Simple nearest-neighbor decimation by slicing
    # ------------------------------------------------------------------
    print("Decimating source grid by integer factors (nearest neighbor)...")

    row_end = row0 + ny * fy_int
    col_end = col0 + nx * fx_int

    # Slice: start at (row0, col0), step by (fy_int, fx_int)
    dst_data = src_data[:, row0:row_end:fy_int, col0:col_end:fx_int]

    # Sanity check shapes
    if dst_data.shape[1] != ny or dst_data.shape[2] != nx:
        raise RuntimeError(
            f"Unexpected output shape: got ({dst_data.shape[1]}, {dst_data.shape[2]}), "
            f"expected ({ny}, {nx})."
        )

    # ------------------------------------------------------------------
    # 4. Read uparea.bin and build ocean mask
    # ------------------------------------------------------------------
    if not os.path.isfile(uparea_path):
        raise FileNotFoundError(f"uparea.bin not found: {uparea_path}")

    print("Reading uparea mask from:", uparea_path)
#    up_flat = np.fromfile(uparea_path, dtype="float32")
    up_flat = np.fromfile(uparea_path, dtype="<f4")

    if up_flat.size != nx * ny:
        raise ValueError(
            f"uparea.bin size mismatch: got {up_flat.size}, expected {nx*ny}."
        )

    # Reshape to (ny, nx), GRaDS-style order (north->south, west->east)
    uparea = up_flat.reshape((ny, nx))

    # Ocean mask: uparea == -9999 → True
    mask_ocean = (uparea <= -9998.5)  # a bit tolerant

    print(f"Number of ocean cells in mask: {mask_ocean.sum()}")

    # Apply mask to all bands: set ocean cells to -9999
    dst_data = dst_data.astype("<f4", copy=False)
    dst_data[:, mask_ocean] = -9999.0

    # ------------------------------------------------------------------
    # 5. Build target transform and write GeoTIFF
    # ------------------------------------------------------------------
    dst_transform = Affine(gsize, 0.0, west,
                           0.0, -gsize, north)

    out_profile = src_profile
    out_profile.update(
        width=nx,
        height=ny,
        transform=dst_transform,
        compress="lzw",
        dtype="float32",
#        dtype="<f4",
        nodata=-9999.0,
    )

    with rasterio.open(out_tif, "w", **out_profile) as dst:
        dst.write(dst_data)

    print("GeoTIFF written :", out_tif)

    # ------------------------------------------------------------------
    # 6. Write plain binary (float32 or <f4, ny*nx) for the first band
    #    in GRaDS-compatible order:
    #      - band1[0, :]  : north-most row, west -> east
    #      - band1[1, :]  : next row to the south
    #      ...
    #      - band1[ny-1,:]: south-most row
    # ------------------------------------------------------------------
    print("Writing plain binary (float32 or <f4, ny*nx, GRaDS order) for band 1 ...")

    band1 = dst_data[0, :, :]               # shape (ny, nx)
    band1_f32 = np.asarray(band1, dtype="<f4", order="C")
    band1_f32.tofile(out_bin)

    print("Binary written   :", out_bin)
    print("Done.")


if __name__ == "__main__":
    main()
