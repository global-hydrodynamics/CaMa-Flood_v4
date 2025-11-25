import numpy as np
import netCDF4 as nc
import xarray as xr
import glob
import struct
import os
import warnings
from tqdm import tqdm
from tifffile import TiffFile
from lmoments3 import distr
import argparse

# ==============================================================================
# --- User settings (default values) ---
#   These can be overridden by command-line arguments.
# ==============================================================================

# --- Step 1: Simulation Information ---
# Start and end year to calculate return period values
START_YEAR = 1990
END_YEAR   = 2020

# Input data: CaMa-Flood simulation output directory
CFM_OUTPUT = "./sim00_out/"
# Input data format ('nc' or 'bin')
INPUT_FORMAT = 'bin'
#INPUT_FORMAT = 'nc'
# Target variable (do not put "o_" for NetCDF)
VAR_NAME = 'rivdph'

# Levee fraction data (file name)
LEVFRC_MAP = 'levfrc.bin'
# Channel depth data (file name)
RIVHGT_MAP = 'rivhgt.bin'

# --- Step 2: Protection Level Info ---
# Target return period (GeoTIFF format)
TARGET_RP_MAP_FILE = "./map/protect.tif"

# --- Step 3 & 4: Masking and adjustment ---
# CaMa-Flood map directory
MAP_DIR = "./map"

# Working directory to save all intermediate files
WORK_DIR = "./sim00_calc/"

# ==============================================================================


def parse_args():
    """Parse command-line arguments to override default settings."""
    parser = argparse.ArgumentParser(
        description="Calculate levee height map from CaMa-Flood outputs."
    )

    # --- Simulation years ---
    parser.add_argument(
        "--start-year", type=int, default=START_YEAR,
        help=f"Start year for analysis (default: {START_YEAR})"
    )
    parser.add_argument(
        "--end-year", type=int, default=END_YEAR,
        help=f"End year for analysis (default: {END_YEAR})"
    )

    # --- Input data settings ---
    parser.add_argument(
        "--cfm-output", type=str, default=CFM_OUTPUT,
        help=f"CaMa-Flood output directory (default: {CFM_OUTPUT})"
    )
    parser.add_argument(
        "--input-format", type=str, choices=["nc", "bin"], default=INPUT_FORMAT,
        help=f"Input data format: 'nc' or 'bin' (default: {INPUT_FORMAT})"
    )
    parser.add_argument(
        "--var-name", type=str, default=VAR_NAME,
        help=f"Target variable name (default: {VAR_NAME})"
    )

    # --- Map / parameter files ---
    parser.add_argument(
        "--levfrc-map", type=str, default=LEVFRC_MAP,
        help=f"Levee fraction map file name (default: {LEVFRC_MAP})"
    )
    parser.add_argument(
        "--rivhgt-map", type=str, default=RIVHGT_MAP,
        help=f"River height map file name (default: {RIVHGT_MAP})"
    )
    parser.add_argument(
        "--target-rp-map-file", type=str, default=TARGET_RP_MAP_FILE,
        help=f"Protection level GeoTIFF file (default: {TARGET_RP_MAP_FILE})"
    )
    parser.add_argument(
        "--map-dir", type=str, default=MAP_DIR,
        help=f"CaMa-Flood map directory (default: {MAP_DIR})"
    )
    parser.add_argument(
        "--work-dir", type=str, default=WORK_DIR,
        help=f"Working directory for intermediate files (default: {WORK_DIR})"
    )

    return parser.parse_args()


def main():
    # Override global settings with command-line arguments
    args = parse_args()
    global START_YEAR, END_YEAR
    global CFM_OUTPUT, INPUT_FORMAT, VAR_NAME
    global LEVFRC_MAP, RIVHGT_MAP
    global TARGET_RP_MAP_FILE, MAP_DIR, LEVHGT_MAP, WORK_DIR

    START_YEAR = args.start_year
    END_YEAR = args.end_year
    CFM_OUTPUT = args.cfm_output
    INPUT_FORMAT = args.input_format
    VAR_NAME = args.var_name
    LEVFRC_MAP = args.levfrc_map
    RIVHGT_MAP = args.rivhgt_map
    TARGET_RP_MAP_FILE = args.target_rp_map_file
    MAP_DIR = args.map_dir
    WORK_DIR = args.work_dir

    os.makedirs(WORK_DIR, exist_ok=True)

    # Check data length
    n_years = END_YEAR - START_YEAR + 1
    if n_years < 20:
        print("WARNING: Annual maximum series contains only "
              f"{n_years} years. Extreme value estimates may be unstable.")
    if n_years < 2:
        print("ERROR: At least 2 years are required for fitting. "
              "Process may fail.")

    # --- Step 1: Calculate annual maximum of rivdph ---
    annual_max_dir = os.path.join(WORK_DIR, "1_annual_max")
    os.makedirs(annual_max_dir, exist_ok=True)
    print("\n--- [Step 1] Calculating annual maximum river depth ---")
    calculate_annual_max(annual_max_dir)

    # --- Step 2: Calculate return-period depth ---
    prob_depth_file = os.path.join(WORK_DIR, "2_probabilistic_depth.bin")
    print("\n--- [Step 2] Calculating return-period depth ---")
    calculate_probabilistic_depth(annual_max_dir, prob_depth_file)

    # --- Step 3: Mask levee grids ---
    masked_depth_file = os.path.join(WORK_DIR, "3_masked_depth.bin")
    print("\n--- [Step 3] Applying levee mask ---")
    apply_levee_mask(prob_depth_file, masked_depth_file)

    # --- Step 4: Subtract river water depth and create final levhgt.bin ---
    final_levhgt_file = os.path.join(WORK_DIR, "levhgt.bin")
    print("\n--- [Step 4] Subtracting normal river depth and creating "
          "final levee height map ---")
    subtract_river_height(masked_depth_file, final_levhgt_file)

    print(f"\nAll processes have been completed. "
          f"The final output is {final_levhgt_file}.")


def calculate_annual_max(output_dir):
    """Calculate annual maximum river depth and save as NetCDF files."""
    print("\n@@@@@@ [1] Calculating annual maximum river depth and "
          "saving as NetCDF files")
    params = get_params()
    for year in range(START_YEAR, END_YEAR + 1):
        print(f"Processing year {year}")
        output_path = os.path.join(output_dir, f"annual_max_{VAR_NAME}_{year}.nc")
        if os.path.exists(output_path):
            print(f"-- Annual maximum file for year {year} exists. Skipping.")
            continue

        if INPUT_FORMAT == 'nc':
            file_path = os.path.join(CFM_OUTPUT, f"o_{VAR_NAME}{year}.nc")
            if not os.path.exists(file_path):
                warnings.warn(f"*** {file_path} file not found")
                continue
            with nc.Dataset(file_path, 'r') as dataset:
                print(f"-- Input file: {file_path}")
                daily_data = dataset.variables[VAR_NAME][:].filled(np.nan)
        elif INPUT_FORMAT == 'bin':
            file_path = os.path.join(CFM_OUTPUT, f"{VAR_NAME}{year}.bin")
            if not os.path.exists(file_path):
                warnings.warn(f"*** {file_path} file not found")
                continue
            print(f"-- Input file: {file_path}")
            daily_data = np.fromfile(
                file_path, dtype=np.float32
            ).reshape((-1, params['ny'], params['nx']))
        else:
            raise ValueError("INPUT_FORMAT must be 'nc' or 'bin'.")

        annual_max = np.nanmax(daily_data, axis=0)

        with nc.Dataset(output_path, 'w', format='NETCDF4') as ds:
            ds.createDimension('lat', params['ny'])
            ds.createDimension('lon', params['nx'])
            var = ds.createVariable('annual_max', np.float32, ('lat', 'lon'))
            var[:] = annual_max.astype(np.float32)
        print(f"-- Annual maximum for year {year} saved as {output_path}")


def calculate_probabilistic_depth(annual_max_dir, output_file):
    """Perform extreme value analysis and create probabilistic depth map."""
    print("\n@@@@@@ [2] Extreme value analysis with lmoments3")

    params = get_params()
    ny, nx = params['ny'], params['nx']

    # List annual maximum files (sorted)
    file_paths = sorted(glob.glob(os.path.join(annual_max_dir, '*.nc')))
    if not file_paths:
        raise FileNotFoundError("*** Error: No annual_max files found.")

    n_years = len(file_paths)
    print(f"-- Reading {n_years} annual_max files")

    # Create an array (year, ny, nx)
    annual_max_data = np.full((n_years, ny, nx), np.nan, dtype=np.float32)

    for i, fp in enumerate(file_paths):
        with nc.Dataset(fp, 'r') as ds:
            var = ds.variables['annual_max'][:]
            annual_max_data[i, :, :] = np.array(var, dtype=np.float32)

    # Load target return period map
    with TiffFile(TARGET_RP_MAP_FILE) as tif:
        target_rp_map = tif.asarray()
        if target_rp_map.shape != (ny, nx):
            raise ValueError(
                "*** Error: Size mismatch between RP map and simulation grid."
            )

    # Output array initialized with missing value
    prob_depth = np.full((ny, nx), -9999.0, dtype=np.float32)

    # Fit Gumbel distribution for each pixel
    for iy in tqdm(range(ny), desc="[Step 2] Frequency analysis"):
        for ix in range(nx):
            target_rp = target_rp_map[iy, ix]
            if target_rp <= 0:
                # Skip cells without defined protection level
                continue

            pixel_data = annual_max_data[:, iy, ix]
            pixel_data = pixel_data[~np.isnan(pixel_data)]

            # Require at least 2 years for fitting
            if len(pixel_data) >= 2:
                try:
                    fit_params = distr.gum.lmom_fit(pixel_data)
                    prob = 1.0 - 1.0 / float(target_rp)
                    prob_depth[iy, ix] = distr.gum.ppf(prob, **fit_params)
                except Exception:
                    continue

    prob_depth.astype(np.float32).tofile(output_file)
    print(f"Probabilistic depth map saved to {output_file}.")


def apply_levee_mask(input_file, output_file):
    """Apply levee mask using levee fraction (levfrc) data."""
    params = get_params()

    # Load probabilistic depth map
    prob_depth = np.fromfile(input_file, dtype=np.float32).reshape(
        params['ny'], params['nx']
    )

    # Load levee fraction map
    levfrc_file = os.path.join(MAP_DIR, f"{LEVFRC_MAP}")
    levfrc_data = np.fromfile(levfrc_file, dtype=np.float32).reshape(
        params['ny'], params['nx']
    )

    # Apply mask: keep values only where levfrc > 0
    masked_depth = np.where(levfrc_data > 0, prob_depth, -9999.0)

    masked_depth.astype(np.float32).tofile(output_file)
    print(f"Masked depth file saved to {output_file}.")


def subtract_river_height(input_file, output_file):
    """Subtract normal river water depth from masked probabilistic depth."""
    params = get_params()

    # Load masked probabilistic depth
    masked_depth = np.fromfile(input_file, dtype=np.float32).reshape(
        params['ny'], params['nx']
    )

    # Load normal river water depth map
    rivhgt_file = os.path.join(MAP_DIR, f"{RIVHGT_MAP}")
    rivhgt_data = np.fromfile(rivhgt_file, dtype=np.float32).reshape(
        params['ny'], params['nx']
    )

    # Subtract river height
    final_levhgt = masked_depth - rivhgt_data

    # (1) Negative values → set to 0
    final_levhgt[final_levhgt < 0] = 0.0

    # (2) Sea (rivhgt = -9999) → undefined (-9999)
    sea_mask = (rivhgt_data == -9999.0)
    final_levhgt[sea_mask] = -9999.0

    # Save output
    final_levhgt.astype(np.float32).tofile(output_file)
    print(f"Final levee height file saved to {output_file}.")


def get_params():
    """Read CaMa-Flood map parameters from map/params.txt."""
    params_file = os.path.join(MAP_DIR, "params.txt")
    with open(params_file, "r") as f:
        lines = f.readlines()
    nx = int(lines[0].split()[0])
    ny = int(lines[1].split()[0])
    return {'nx': nx, 'ny': ny}


if __name__ == "__main__":
    main()
