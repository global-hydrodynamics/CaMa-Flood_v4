#!/usr/bin/env python3
#==========================================================
# Generate CaMa-Flood runoff input information.
#
# This script writes a plain-text summary for CaMa-Flood runoff forcing.
# It supports both:
#
#   1. NetCDF runoff forcing
#      runoff_YYYY.nc or runoff_YYYYMM.nc
#
#   2. Plain-binary runoff forcing with GrADS ctl
#      daily    : runoff_YYYYMMDD.bin
#      subdaily : runoff_YYYYMMDDHH.bin
#
# For binary forcing, IFRQ_INP and DROFUNIT are inferred from the ctl
# tdef time step whenever possible. Therefore sub-daily binary forcing
# such as 1hr, 3hr, 6hr, 12hr is handled correctly.
#
# DROFUNIT is calculated as:
#   DROFUNIT = 1000 * forcing_timestep_hours * 3600
#
#==========================================================

from __future__ import annotations

import argparse
import glob
import math
import re
import sys
from pathlib import Path
from typing import Any

import numpy as np
import pandas as pd
import xarray as xr


#==========================================================
# Arguments
#==========================================================

def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Write CaMa-Flood runoff forcing metadata from NetCDF or binary/CTL files."
    )

    parser.add_argument("--outdir", type=Path, required=True, help="Directory containing converted runoff files")
    parser.add_argument("--outfile", type=Path, required=True, help="Output text file")

    parser.add_argument(
        "--source-format",
        choices=["netcdf", "binary", "auto"],
        default="auto",
        help="Source format to inspect. Default: auto.",
    )

    # NetCDF options
    parser.add_argument("--pattern", default="runoff_*.nc", help="NetCDF file pattern. Default: runoff_*.nc")
    parser.add_argument("--variable", default="runoff", help="NetCDF runoff variable name. Default: runoff")
    parser.add_argument("--merge-unit", choices=["yearly", "monthly"], default="monthly", help="NetCDF output grouping")

    # Binary / GrADS options
    parser.add_argument("--ctl-file", type=Path, default=None, help="GrADS ctl file for binary forcing")
    parser.add_argument("--binary-prefix", default="runoff_", help='Binary runoff file prefix. Default: "runoff_"')
    parser.add_argument("--binary-suffix", default=".bin", help='Binary runoff file suffix. Default: ".bin"')
    parser.add_argument("--binary-variable", default="var", help='Binary/GrADS variable name. Default: "var"')

    # Common options
    parser.add_argument(
        "--timestep-hours",
        type=float,
        required=True,
        help=(
            "Fallback forcing timestep in hours. "
            "For binary forcing, the ctl tdef step is used when available."
        ),
    )
    parser.add_argument("--start", default="", help="Conversion period start date, YYYY-MM-DD")
    parser.add_argument("--end", default="", help="Conversion period end date, YYYY-MM-DD")
    parser.add_argument("--sample-name", default="ERA5-Land runoff", help="Name shown in the text header")
    parser.add_argument("--prefix", default="runoff_", help='NetCDF runoff file prefix only. Default: "runoff_"')
    parser.add_argument("--suffix", default=".nc", help='NetCDF runoff file suffix. Default: ".nc"')
    parser.add_argument("--overwrite", action="store_true", help="Overwrite output text file")

    return parser.parse_args()


#==========================================================
# Small utilities
#==========================================================

def fmt_float(x: Any, ndigits: int = 8) -> str:
    try:
        xf = float(x)
    except Exception:
        return str(x)

    if not np.isfinite(xf):
        return "NaN"

    if abs(xf - round(xf)) < 1.0e-9:
        return str(int(round(xf)))

    text = f"{xf:.{ndigits}f}".rstrip("0").rstrip(".")
    if text == "-0":
        text = "0"
    return text


def parse_tdef_step_hours(step: str, fallback_hours: float) -> float:
    """Parse GrADS tdef step, e.g. 1dy, 3hr, 6hr, 1mo.

    This tool is intended for hourly/daily runoff forcing. Monthly/yearly
    steps are not converted to hours because forcing files should not use
    those for CaMa-Flood runoff input.
    """

    s = str(step).strip().lower()

    m = re.fullmatch(r"([0-9]+(?:\.[0-9]+)?)(hr|hrs|hour|hours)", s)
    if m:
        return float(m.group(1))

    m = re.fullmatch(r"([0-9]+(?:\.[0-9]+)?)(dy|day|days)", s)
    if m:
        return float(m.group(1)) * 24.0

    m = re.fullmatch(r"([0-9]+(?:\.[0-9]+)?)(mn|min|mins|minute|minutes)", s)
    if m:
        return float(m.group(1)) / 60.0

    return float(fallback_hours)


def grads_step_label(hours: float) -> str:
    if abs(hours - 24.0) < 1.0e-9:
        return "1dy"
    if abs(hours - round(hours)) < 1.0e-9:
        return f"{int(round(hours))}hr"
    return f"{hours:g}hr"


def expected_binary_rule(prefix: str, suffix: str, timestep_hours: float, dset_from_ctl: str | None = None) -> str:
    if dset_from_ctl and dset_from_ctl != "N/A":
        # Strip common GrADS relative markers for readability.
        return dset_from_ctl.replace("^./", "").replace("^", "")

    if abs(timestep_hours - 24.0) < 1.0e-9:
        return f"{prefix}YYYYMMDD{suffix}"
    return f"{prefix}YYYYMMDDHH{suffix}"


#==========================================================
# NetCDF utilities
#==========================================================

def find_files(outdir: Path, pattern: str) -> list[Path]:
    return sorted(Path(p) for p in glob.glob(str(outdir / pattern)))


def find_coord_name(ds: xr.Dataset, candidates: list[str]) -> str:
    for name in candidates:
        if name in ds.coords:
            return name
        if name in ds.dims:
            return name
    raise KeyError(f"Could not find coordinate from candidates: {candidates}")


def unique_finite_sorted(values: np.ndarray) -> np.ndarray:
    arr = np.asarray(values, dtype=float).ravel()
    arr = arr[np.isfinite(arr)]
    if arr.size == 0:
        return arr
    return np.sort(np.unique(np.round(arr, 10)))


def extract_axis_values(ds: xr.Dataset, coord_name: str) -> np.ndarray:
    coord = ds[coord_name]
    values = np.asarray(coord.values, dtype=float)

    if values.ndim == 1:
        return values

    if values.ndim >= 2:
        return unique_finite_sorted(values)

    vals = unique_finite_sorted(values)
    if vals.size == 0:
        raise ValueError(f"Coordinate {coord_name} has no finite values.")
    return vals


def infer_axis_order(ds: xr.Dataset, coord_name: str, axis: str) -> str:
    coord = ds[coord_name]
    values = np.asarray(coord.values, dtype=float)

    if values.ndim == 1 and values.size >= 2:
        if values[0] > values[-1]:
            return "NtoS" if axis == "lat" else "decreasing"
        if values[0] < values[-1]:
            return "StoN" if axis == "lat" else "increasing"

    if values.ndim == 2:
        if axis == "lat":
            col = values[:, 0]
            col = col[np.isfinite(col)]
            if col.size >= 2:
                if col[0] > col[-1]:
                    return "NtoS"
                if col[0] < col[-1]:
                    return "StoN"

            row = values[0, :]
            row = row[np.isfinite(row)]
            if row.size >= 2:
                if row[0] > row[-1]:
                    return "NtoS"
                if row[0] < row[-1]:
                    return "StoN"

        if axis == "lon":
            row = values[0, :]
            row = row[np.isfinite(row)]
            if row.size >= 2:
                if row[0] < row[-1]:
                    return "increasing"
                if row[0] > row[-1]:
                    return "decreasing"

    return "unknown"


def infer_resolution(values: np.ndarray) -> float:
    vals = np.asarray(values, dtype=float).ravel()
    vals = vals[np.isfinite(vals)]
    vals = np.sort(np.unique(np.round(vals, 10)))

    if vals.size < 2:
        return float("nan")

    diffs = np.diff(vals)
    diffs = diffs[(diffs > 0) & np.isfinite(diffs)]

    if diffs.size == 0:
        return float("nan")

    return float(np.min(diffs))


def infer_time_info(ds: xr.Dataset, timestep_hours_arg: float) -> dict[str, Any]:
    if "time" not in ds.coords:
        return {
            "ntime": 0,
            "time_start": "N/A",
            "time_end": "N/A",
            "time_step_hours_inferred": timestep_hours_arg,
        }

    times = pd.to_datetime(ds["time"].values)
    ntime = len(times)

    if ntime >= 2:
        dt_seconds = np.diff(times.values).astype("timedelta64[s]").astype(float)
        dt_seconds = dt_seconds[np.isfinite(dt_seconds)]
        dt_hours = float(np.median(dt_seconds) / 3600.0) if dt_seconds.size else timestep_hours_arg
    else:
        dt_hours = timestep_hours_arg

    return {
        "ntime": ntime,
        "time_start": str(times[0]) if ntime else "N/A",
        "time_end": str(times[-1]) if ntime else "N/A",
        "time_step_hours_inferred": dt_hours,
    }


def read_netcdf_metadata(args: argparse.Namespace) -> dict[str, Any] | None:
    files = find_files(args.outdir, args.pattern)
    if not files:
        return None

    sample_file = files[0]
    ds = xr.open_dataset(sample_file)

    if args.variable not in ds.data_vars:
        available = list(ds.data_vars)
        raise KeyError(f"Variable '{args.variable}' not found in {sample_file}. Available: {available}")

    lat_name = find_coord_name(ds, ["lat", "latitude", "y"])
    lon_name = find_coord_name(ds, ["lon", "longitude", "x"])

    lat_values = extract_axis_values(ds, lat_name)
    lon_values = extract_axis_values(ds, lon_name)

    lat_unique = unique_finite_sorted(lat_values)
    lon_unique = unique_finite_sorted(lon_values)

    ny = int(lat_unique.size)
    nx = int(lon_unique.size)

    dlat = infer_resolution(lat_values)
    dlon = infer_resolution(lon_values)

    if np.isfinite(dlat) and np.isfinite(dlon) and math.isclose(dlat, dlon, rel_tol=1.0e-4, abs_tol=1.0e-8):
        gsize = dlat
    elif np.isfinite(dlat) and not np.isfinite(dlon):
        gsize = dlat
    elif np.isfinite(dlon) and not np.isfinite(dlat):
        gsize = dlon
    else:
        gsize = float("nan")

    olat = infer_axis_order(ds, lat_name, "lat")

    lon_min = float(np.nanmin(lon_values))
    lon_max = float(np.nanmax(lon_values))
    lat_min = float(np.nanmin(lat_values))
    lat_max = float(np.nanmax(lat_values))

    westin = lon_min - dlon * 0.5 if np.isfinite(dlon) else float("nan")
    eastin = lon_max + dlon * 0.5 if np.isfinite(dlon) else float("nan")
    southin = lat_min - dlat * 0.5 if np.isfinite(dlat) else float("nan")
    northin = lat_max + dlat * 0.5 if np.isfinite(dlat) else float("nan")

    var = ds[args.variable]
    units = str(var.attrs.get("units", "unknown"))
    long_name = str(var.attrs.get("long_name", ""))
    fill_value = var.encoding.get("_FillValue", var.attrs.get("_FillValue", var.attrs.get("missing_value", "N/A")))
    time_info = infer_time_info(ds, args.timestep_hours)

    file_rule = "runoff_YYYY.nc" if args.merge_unit == "yearly" else "runoff_YYYYMM.nc"

    return {
        "source_format": "netcdf",
        "files": files,
        "sample_file": sample_file,
        "nx": nx,
        "ny": ny,
        "gsize": gsize,
        "westin": westin,
        "eastin": eastin,
        "northin": northin,
        "southin": southin,
        "olat": olat,
        "dlon": dlon,
        "dlat": dlat,
        "forcing_timestep_hours": float(args.timestep_hours),
        "time_info": time_info,
        "file_rule": file_rule,
        "lon_name": lon_name,
        "lat_name": lat_name,
        "lon_min_center": lon_min,
        "lon_max_center": lon_max,
        "lat_min_center": lat_min,
        "lat_max_center": lat_max,
        "variable_units": units,
        "variable_long_name": long_name,
        "fill_value": fill_value,
    }


#==========================================================
# GrADS CTL utilities
#==========================================================

def parse_ctl_file(path: Path) -> dict[str, Any]:
    if not path.exists():
        raise FileNotFoundError(f"CTL file not found: {path}")

    info: dict[str, Any] = {
        "ctl_file": path,
        "dset": "N/A",
        "undef": "N/A",
        "title": "",
        "options": "",
        "xdef": None,
        "ydef": None,
        "tdef": None,
        "varname": "var",
    }

    lines = path.read_text(encoding="utf-8", errors="replace").splitlines()

    in_vars = False
    for raw in lines:
        line = raw.strip()
        if not line:
            continue
        lower = line.lower()

        if lower.startswith("dset"):
            info["dset"] = line.split(None, 1)[1] if len(line.split(None, 1)) > 1 else ""
        elif lower.startswith("undef"):
            info["undef"] = line.split(None, 1)[1] if len(line.split(None, 1)) > 1 else ""
        elif lower.startswith("title"):
            info["title"] = line.split(None, 1)[1] if len(line.split(None, 1)) > 1 else ""
        elif lower.startswith("options"):
            info["options"] = line.split(None, 1)[1] if len(line.split(None, 1)) > 1 else ""
        elif lower.startswith("xdef"):
            parts = line.split()
            info["xdef"] = {
                "n": int(parts[1]),
                "method": parts[2],
                "start": float(parts[3]),
                "step": float(parts[4]),
            }
        elif lower.startswith("ydef"):
            parts = line.split()
            info["ydef"] = {
                "n": int(parts[1]),
                "method": parts[2],
                "start": float(parts[3]),
                "step": float(parts[4]),
            }
        elif lower.startswith("tdef"):
            parts = line.split()
            info["tdef"] = {
                "n": int(parts[1]),
                "method": parts[2],
                "start": parts[3],
                "step": parts[4],
            }
        elif lower.startswith("vars"):
            in_vars = True
        elif in_vars and not lower.startswith("endvars"):
            parts = line.split()
            if parts:
                info["varname"] = parts[0]
                in_vars = False

    return info


def read_binary_metadata(args: argparse.Namespace) -> dict[str, Any] | None:
    ctl_path = args.ctl_file if args.ctl_file is not None else args.outdir / "runoff.ctl"
    if not ctl_path.exists():
        return None

    ctl = parse_ctl_file(ctl_path)

    xdef = ctl.get("xdef")
    ydef = ctl.get("ydef")
    tdef = ctl.get("tdef")

    if xdef is None or ydef is None:
        raise ValueError(f"CTL file lacks xdef/ydef: {ctl_path}")

    nx = int(xdef["n"])
    ny = int(ydef["n"])
    dlon = float(xdef["step"])
    dlat = float(ydef["step"])
    gsize = dlat if math.isclose(dlat, dlon, rel_tol=1.0e-4, abs_tol=1.0e-8) else float("nan")

    westin = float(xdef["start"] - 0.5 * dlon)
    eastin = float(xdef["start"] + (nx - 1) * dlon + 0.5 * dlon)

    south_center = float(ydef["start"])
    north_center = float(ydef["start"] + (ny - 1) * dlat)
    southin = south_center - 0.5 * dlat
    northin = north_center + 0.5 * dlat

    options = str(ctl.get("options", ""))
    olat = "NtoS" if "yrev" in options.lower().split() else "StoN"

    tdef_step = tdef["step"] if tdef else ""
    forcing_timestep_hours = parse_tdef_step_hours(tdef_step, args.timestep_hours)

    file_rule = expected_binary_rule(
        prefix=args.binary_prefix,
        suffix=args.binary_suffix,
        timestep_hours=forcing_timestep_hours,
        dset_from_ctl=str(ctl.get("dset", "N/A")),
    )

    binary_files = sorted(args.outdir.glob(f"{args.binary_prefix}*{args.binary_suffix}"))

    return {
        "source_format": "binary",
        "ctl_file": ctl_path,
        "binary_files": binary_files,
        "sample_file": binary_files[0] if binary_files else ctl_path,
        "nx": nx,
        "ny": ny,
        "gsize": gsize,
        "westin": westin,
        "eastin": eastin,
        "northin": northin,
        "southin": southin,
        "olat": olat,
        "dlon": dlon,
        "dlat": dlat,
        "forcing_timestep_hours": forcing_timestep_hours,
        "time_info": {
            "ntime": tdef["n"] if tdef else "N/A",
            "time_start": tdef["start"] if tdef else "N/A",
            "time_end": "N/A",
            "time_step_hours_inferred": forcing_timestep_hours,
        },
        "undef": ctl.get("undef", "N/A"),
        "title": ctl.get("title", ""),
        "options": options,
        "binary_variable": ctl.get("varname", args.binary_variable),
        "dset": ctl.get("dset", "N/A"),
        "tdef_step": tdef_step,
        "file_rule": file_rule,
    }


def choose_metadata(args: argparse.Namespace) -> tuple[dict[str, Any] | None, dict[str, Any] | None]:
    nc_meta = None
    bin_meta = None

    if args.source_format in {"auto", "netcdf"}:
        nc_meta = read_netcdf_metadata(args)
        if args.source_format == "netcdf" and nc_meta is None:
            raise FileNotFoundError(f"No NetCDF files found: {args.outdir / args.pattern}")

    if args.source_format in {"auto", "binary"}:
        bin_meta = read_binary_metadata(args)
        if args.source_format == "binary" and bin_meta is None:
            ctl_path = args.ctl_file if args.ctl_file is not None else args.outdir / "runoff.ctl"
            raise FileNotFoundError(f"No binary metadata found. CTL missing: {ctl_path}")

    return nc_meta, bin_meta


#==========================================================
# Main
#==========================================================

def main() -> int:
    args = parse_args()

    try:
        if args.outfile.exists() and not args.overwrite:
            raise FileExistsError(f"Output file exists: {args.outfile}. Use --overwrite to replace it.")

        nc_meta, bin_meta = choose_metadata(args)

        primary = nc_meta if nc_meta is not None else bin_meta
        if primary is None:
            raise FileNotFoundError("No NetCDF files or binary CTL file found to inspect.")

        forcing_timestep_hours = float(primary.get("forcing_timestep_hours", args.timestep_hours))
        drofunit = 1000.0 * forcing_timestep_hours * 3600.0

        args.outfile.parent.mkdir(parents=True, exist_ok=True)

        netcdf_rule = "runoff_YYYY.nc" if args.merge_unit == "yearly" else "runoff_YYYYMM.nc"
        binary_rule = expected_binary_rule(args.binary_prefix, args.binary_suffix, forcing_timestep_hours)
        binary_step = grads_step_label(forcing_timestep_hours)

        lines: list[str] = []
        lines.append("#==========================================================")
        lines.append("# CaMa-Flood runoff forcing information")
        lines.append("#==========================================================")
        lines.append(f"# Sample name: {args.sample_name}")
        lines.append(f"# Source format inspected: {primary['source_format']}")
        lines.append(f"# Created from: {primary.get('sample_file', 'N/A')}")
        if args.start or args.end:
            lines.append(f"# Requested period: {args.start} to {args.end}")
        lines.append("")

        lines.append("#----------------------------------------------------------")
        lines.append("# 1. Parameters for generating the CaMa-Flood input mapping table")
        lines.append("#----------------------------------------------------------")
        lines.append(f"GRSIZEIN={fmt_float(primary['gsize'])}       # input grid size [degree]")
        lines.append(f"WESTIN={fmt_float(primary['westin'])}        # input domain west edge")
        lines.append(f"EASTIN={fmt_float(primary['eastin'])}        # input domain east edge")
        lines.append(f"NORTHIN={fmt_float(primary['northin'])}      # input domain north edge")
        lines.append(f"SOUTHIN={fmt_float(primary['southin'])}      # input domain south edge")
        lines.append(f"OLAT=\"{primary['olat']}\"        # north-south order of input data")
        lines.append("")
        lines.append("# Example:")
        lines.append("# ./generate_inpmat $MAPTAG $GRSIZEIN $WESTIN $EASTIN $NORTHIN $SOUTHIN $OLAT diminfo.txt inpmat.bin")
        lines.append("")

        lines.append("#----------------------------------------------------------")
        lines.append("# 2. Common CaMa-Flood namelist parameters")
        lines.append("#----------------------------------------------------------")
        lines.append("#============================")
        lines.append("#*** 1f. forcing setting")
        lines.append(f"IFRQ_INP=\"{fmt_float(forcing_timestep_hours, 3)}\"       # input forcing frequency [hour]")
        lines.append(f"DROFUNIT=\"{fmt_float(drofunit, 3)}\"       # [mm/{fmt_float(forcing_timestep_hours, 3)}h -> m/s] runoff unit conversion")
        lines.append("# Unit conversion:")
        lines.append("#   runoff_m_per_s = runoff_mm_per_timestep / DROFUNIT")
        lines.append("")

        lines.append("#----------------------------------------------------------")
        lines.append("# 3. NetCDF runoff forcing namelist parameters")
        lines.append("#----------------------------------------------------------")
        lines.append("#----- for netCDF runoff forcing")
        lines.append(f"CROFPRE=\"{args.prefix}\"       # runoff file prefix")
        lines.append(f"CROFSUF=\"{args.suffix}\"       # runoff file suffix")
        lines.append("CROFCDF=\"\"              # netCDF runoff file; set each year/month if needed")
        lines.append(f"CVNROF=\"{args.variable}\"       # netCDF runoff variable name")
        lines.append(f"# File naming rule: {netcdf_rule}")
        lines.append("")

        lines.append("#----------------------------------------------------------")
        lines.append("# 4. Plain binary runoff forcing namelist parameters")
        lines.append("#----------------------------------------------------------")
        lines.append("#----- for plain binary runoff forcing")
        lines.append(f"CROFPRE=\"{args.binary_prefix}\"       # runoff prefix")
        lines.append(f"CROFSUF=\"{args.binary_suffix}\"       # runoff suffix")
        lines.append(f"# File naming rule: {binary_rule}")
        lines.append(f"# GrADS tdef step: {binary_step}")
        lines.append("")

        lines.append("#----------------------------------------------------------")
        lines.append("# 5. Source metadata summary")
        lines.append("#----------------------------------------------------------")
        lines.append(f"source_format={primary['source_format']}")
        lines.append(f"nx={primary['nx']}")
        lines.append(f"ny={primary['ny']}")
        lines.append(f"dlon={fmt_float(primary['dlon'])}")
        lines.append(f"dlat={fmt_float(primary['dlat'])}")
        lines.append(f"forcing_timestep_hours={fmt_float(forcing_timestep_hours, 3)}")
        lines.append(f"time_step_hours_config={fmt_float(args.timestep_hours, 3)}")
        lines.append(f"time_step_hours_inferred={fmt_float(primary['time_info']['time_step_hours_inferred'], 3)}")
        lines.append(f"ntime_in_sample={primary['time_info']['ntime']}")
        lines.append(f"time_start_in_sample={primary['time_info']['time_start']}")
        lines.append(f"time_end_in_sample={primary['time_info']['time_end']}")

        if nc_meta is not None:
            lines.append("")
            lines.append("# NetCDF metadata")
            lines.append(f"netcdf_files_found={len(nc_meta['files'])}")
            lines.append(f"netcdf_sample_file={nc_meta['sample_file']}")
            lines.append(f"lon_name={nc_meta['lon_name']}")
            lines.append(f"lat_name={nc_meta['lat_name']}")
            lines.append(f"lon_min_center={fmt_float(nc_meta['lon_min_center'])}")
            lines.append(f"lon_max_center={fmt_float(nc_meta['lon_max_center'])}")
            lines.append(f"lat_min_center={fmt_float(nc_meta['lat_min_center'])}")
            lines.append(f"lat_max_center={fmt_float(nc_meta['lat_max_center'])}")
            lines.append(f"variable_name={args.variable}")
            lines.append(f"variable_units={nc_meta['variable_units']}")
            lines.append(f"variable_long_name={nc_meta['variable_long_name']}")
            lines.append(f"fill_value={nc_meta['fill_value']}")

        if bin_meta is not None:
            lines.append("")
            lines.append("# Binary/CTL metadata")
            lines.append(f"ctl_file={bin_meta['ctl_file']}")
            lines.append(f"binary_files_found={len(bin_meta['binary_files'])}")
            lines.append(f"dset={bin_meta['dset']}")
            lines.append(f"undef={bin_meta['undef']}")
            lines.append(f"options={bin_meta['options']}")
            lines.append(f"binary_variable={bin_meta['binary_variable']}")
            lines.append(f"title={bin_meta['title']}")
            lines.append(f"tdef_step={bin_meta['tdef_step']}")

        lines.append("")

        args.outfile.write_text("\n".join(lines), encoding="utf-8")
        print(f"[INFO] Wrote runoff information file: {args.outfile}")
        return 0

    except Exception as exc:
        print(f"[ERROR] {exc}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
