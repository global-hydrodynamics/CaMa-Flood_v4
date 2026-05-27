#!/usr/bin/env python3
#==========================================================
# Convert downloaded ERA5-Land NetCDF files to CaMa-Flood
# runoff forcing NetCDF files.
#
# Input file unit:
#   daily   -> read ERA5-Land_runoff_YYYYMMDD.nc
#   monthly -> read ERA5-Land_runoff_YYYYMM.nc
#
# Output:
#   runoff_YYYY.nc or runoff_YYYYMM.nc
#
# Main processing:
#   1. Read downloaded ERA5-Land NetCDF files.
#   2. Convert accumulated runoff [m] to timestep runoff [mm].
#   3. Merge files over the requested period.
#   4. Write a CaMa-Flood-friendly NetCDF file.
#
# Notes:
#   - ERA5-Land runoff is an accumulated variable in metres.
#   - Output runoff is mm per input timestep.
#   - Longitude is kept as stored in the downloaded NetCDF files.
#==========================================================

from __future__ import annotations

import argparse
import calendar
import sys
from datetime import date, timedelta
from pathlib import Path
from typing import Iterable

import numpy as np
import pandas as pd
import xarray as xr


COMMON_RUNOFF_NAMES = [
    "runoff",
    "ro",
    "total_runoff",
    "surface_runoff",
    "sro",
    "sub_surface_runoff",
    "ssro",
]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Convert ERA5-Land accumulated runoff NetCDF files to CaMa-Flood forcing."
    )

    parser.add_argument("--start", required=True, help="Start date, inclusive: YYYY-MM-DD")
    parser.add_argument("--end", required=True, help="End date, inclusive: YYYY-MM-DD")
    parser.add_argument("--indir", type=Path, required=True, help="Input directory")
    parser.add_argument("--out", type=Path, required=True, help="Output NetCDF file")

    parser.add_argument(
        "--input-file-unit",
        choices=["daily", "monthly"],
        default="daily",
        help="Downloaded input file unit. Default: daily.",
    )
    parser.add_argument(
        "--input-pattern",
        default=None,
        help=(
            "Input filename pattern using strftime syntax. "
            "Default is ERA5-Land_runoff_%%Y%%m%%d.nc for daily, "
            "and ERA5-Land_runoff_%%Y%%m.nc for monthly."
        ),
    )

    parser.add_argument("--input-variable", default="runoff", help="Input variable name. Default: runoff")
    parser.add_argument("--output-variable", default="runoff", help="Output variable name. Default: runoff")
    parser.add_argument("--timestep-hours", type=int, default=24, help="Forcing timestep in hours. Default: 24.")
    parser.add_argument("--fill-value", type=float, default=1.0e20, help="Output _FillValue. Default: 1.0e20.")
    parser.add_argument(
        "--lat-order",
        choices=["NtoS", "StoN", "keep"],
        default="NtoS",
        help="Latitude order in output. Default: NtoS.",
    )
    parser.add_argument(
        "--first-step",
        choices=["keep", "zero", "missing"],
        default="keep",
        help=(
            "How to handle the first accumulated value of each day. "
            "'keep' uses it as the first timestep accumulation. Default: keep."
        ),
    )
    parser.add_argument("--clip-negative", action="store_true", default=True, help="Clip negative increments to zero.")
    parser.add_argument("--no-clip-negative", action="store_false", dest="clip_negative")
    parser.add_argument("--overwrite", action="store_true", help="Overwrite output file.")
    parser.add_argument("--dry-run", action="store_true", help="List input files and exit.")

    return parser.parse_args()


def parse_date(value: str) -> date:
    return date.fromisoformat(value)


def date_range(start: date, end: date) -> Iterable[date]:
    cur = start
    while cur <= end:
        yield cur
        cur += timedelta(days=1)


def month_range(start: date, end: date) -> Iterable[date]:
    cur = date(start.year, start.month, 1)
    last = date(end.year, end.month, 1)

    while cur <= last:
        yield cur
        if cur.month == 12:
            cur = date(cur.year + 1, 1, 1)
        else:
            cur = date(cur.year, cur.month + 1, 1)


def default_pattern(input_file_unit: str) -> str:
    if input_file_unit == "daily":
        return "ERA5-Land_runoff_%Y%m%d.nc"
    if input_file_unit == "monthly":
        return "ERA5-Land_runoff_%Y%m.nc"
    raise ValueError(input_file_unit)


def input_files(indir: Path, pattern: str, input_file_unit: str, start: date, end: date) -> list[Path]:
    files: list[Path] = []
    missing: list[Path] = []

    if input_file_unit == "daily":
        for d in date_range(start, end):
            path = indir / d.strftime(pattern)
            if path.exists():
                files.append(path)
            else:
                missing.append(path)

    elif input_file_unit == "monthly":
        for d in month_range(start, end):
            path = indir / d.strftime(pattern)
            if path.exists():
                files.append(path)
            else:
                missing.append(path)

    else:
        raise ValueError(input_file_unit)

    if missing:
        text = "\n".join(str(p) for p in missing[:20])
        more = "" if len(missing) <= 20 else f"\n... and {len(missing) - 20} more"
        raise FileNotFoundError(f"Missing input files:\n{text}{more}")

    return files


def standardize_coords(ds: xr.Dataset) -> xr.Dataset:
    rename = {}

    if "valid_time" in ds.coords and "time" not in ds.coords:
        rename["valid_time"] = "time"
    if "latitude" in ds.coords and "lat" not in ds.coords:
        rename["latitude"] = "lat"
    if "longitude" in ds.coords and "lon" not in ds.coords:
        rename["longitude"] = "lon"

    if rename:
        ds = ds.rename(rename)

    for dim in ["expver", "number"]:
        if dim in ds.dims and ds.sizes[dim] == 1:
            ds = ds.isel({dim: 0}, drop=True)

    return ds


def open_inputs(files: list[Path]) -> xr.Dataset:
    datasets = []

    for path in files:
        ds = xr.open_dataset(path)
        ds = standardize_coords(ds)
        datasets.append(ds)

    if len(datasets) == 1:
        ds_all = datasets[0]
    else:
        ds_all = xr.concat(datasets, dim="time", data_vars="minimal", coords="minimal", compat="override")

    if "time" in ds_all.coords:
        ds_all = ds_all.sortby("time")

    return ds_all


def find_variable(ds: xr.Dataset, requested: str) -> str:
    if requested in ds.data_vars:
        return requested

    for name in COMMON_RUNOFF_NAMES:
        if name in ds.data_vars:
            return name

    for name, da in ds.data_vars.items():
        long_name = str(da.attrs.get("long_name", "")).lower()
        short_name = str(da.attrs.get("GRIB_shortName", "")).lower()
        standard_name = str(da.attrs.get("standard_name", "")).lower()

        if "runoff" in long_name or "runoff" in standard_name or short_name in {"ro", "sro", "ssro"}:
            return name

    raise KeyError(f"Could not find runoff variable. Available variables: {list(ds.data_vars)}")


def normalize_latitude(ds: xr.Dataset, mode: str) -> xr.Dataset:
    if mode == "keep" or "lat" not in ds.coords:
        return ds
    if mode == "NtoS":
        return ds.sortby("lat", ascending=False)
    if mode == "StoN":
        return ds.sortby("lat", ascending=True)
    raise ValueError(mode)


def accumulated_to_increment_daily_groups(
    da: xr.DataArray,
    first_step: str,
    clip_negative: bool,
) -> xr.DataArray:
    """Convert accumulated runoff [m] to timestep increments [m].

    Differencing is applied within each UTC day.
    This works for both daily input files and monthly input files, because
    grouping is based on the time coordinate, not on file boundaries.
    """

    if "time" not in da.dims:
        raise ValueError("Input runoff variable must have a time dimension.")

    pieces = []
    times = pd.to_datetime(da["time"].values)
    day_keys = pd.to_datetime(times.date).unique()

    for day in day_keys:
        day_start = np.datetime64(pd.Timestamp(day))
        day_end = day_start + np.timedelta64(1, "D")
        sub = da.sel(time=(da["time"] >= day_start) & (da["time"] < day_end))

        if sub.sizes.get("time", 0) == 0:
            continue

        if sub.sizes["time"] == 1:
            inc = sub.copy()
            if first_step == "zero":
                inc = inc * 0.0
            elif first_step == "missing":
                inc = inc.where(False)
            pieces.append(inc)
            continue

        diff = sub.diff("time", label="upper")
        first = sub.isel(time=0)

        if first_step == "keep":
            first_inc = first
        elif first_step == "zero":
            first_inc = first * 0.0
        elif first_step == "missing":
            first_inc = first.where(False)
        else:
            raise ValueError(first_step)

        inc = xr.concat([first_inc, diff], dim=sub["time"])
        inc = inc.assign_coords(time=sub["time"])

        # If accumulation resets or numerical issues produce negative differences,
        # use the current accumulated value at that timestep.
        inc = inc.where(inc >= 0.0, sub)

        if clip_negative:
            inc = inc.clip(min=0.0)

        pieces.append(inc)

    if not pieces:
        raise ValueError("No time steps found for increment conversion.")

    out = xr.concat(pieces, dim="time")
    out = out.sortby("time")
    return out


def build_output_dataset(ds_raw: xr.Dataset, args: argparse.Namespace, start: date, end: date) -> xr.Dataset:
    varname = find_variable(ds_raw, args.input_variable)
    runoff_acc_m = ds_raw[varname]

    runoff_inc_m = accumulated_to_increment_daily_groups(
        runoff_acc_m,
        first_step=args.first_step,
        clip_negative=args.clip_negative,
    )

    runoff_mm = runoff_inc_m * 1000.0
    runoff_mm.name = args.output_variable

    start_dt = np.datetime64(start.isoformat())
    end_dt = np.datetime64((end + timedelta(days=1)).isoformat())
    runoff_mm = runoff_mm.sel(time=(runoff_mm["time"] >= start_dt) & (runoff_mm["time"] < end_dt))

    runoff_mm.attrs = {
        "long_name": "runoff accumulated over each input timestep",
        "units": "mm",
        "description": f"ERA5-Land runoff converted from accumulated metres to mm per {args.timestep_hours}-hour timestep",
        "time_step_hours": args.timestep_hours,
        "source_variable": varname,
        "source_units": str(ds_raw[varname].attrs.get("units", "m")),
    }

    out = runoff_mm.to_dataset()
    out = normalize_latitude(out, args.lat_order)

    if "time" in out.coords:
        out["time"].attrs.update({
            "long_name": "time",
            "description": "Timestamp of each runoff input timestep",
        })

    out.attrs.update({
        "title": "ERA5-Land runoff forcing for CaMa-Flood",
        "source_dataset": "reanalysis-era5-land",
        "processing": "Converted accumulated ERA5-Land runoff to timestep runoff in millimetres.",
        "period_start": start.isoformat(),
        "period_end": end.isoformat(),
        "input_file_unit": args.input_file_unit,
        "input_pattern": args.input_pattern,
        "Conventions": "CF-1.8",
    })

    return out


def save_output(ds: xr.Dataset, path: Path, varname: str, fill_value: float, overwrite: bool) -> None:
    if path.exists() and not overwrite:
        raise FileExistsError(f"Output file already exists: {path}. Use --overwrite to replace it.")

    path.parent.mkdir(parents=True, exist_ok=True)

    fill = np.float32(fill_value)
    ds[varname] = ds[varname].astype("float32")
    ds[varname].attrs["missing_value"] = fill

    encoding = {
        varname: {
            "dtype": "float32",
            "zlib": True,
            "complevel": 4,
            "_FillValue": fill,
        }
    }

    ds.to_netcdf(path, encoding=encoding)


def main() -> int:
    args = parse_args()

    try:
        start = parse_date(args.start)
        end = parse_date(args.end)

        if end < start:
            raise ValueError("--end is earlier than --start")

        if args.timestep_hours < 1 or 24 % args.timestep_hours != 0:
            raise ValueError("--timestep-hours must divide 24 exactly.")

        if args.input_pattern is None:
            args.input_pattern = default_pattern(args.input_file_unit)

        files = input_files(args.indir, args.input_pattern, args.input_file_unit, start, end)

        print("[INFO] convert_CMF_nc.py")
        print(f"[INFO] Period          : {start} to {end}")
        print(f"[INFO] Input directory : {args.indir}")
        print(f"[INFO] Input file unit : {args.input_file_unit}")
        print(f"[INFO] Input pattern   : {args.input_pattern}")
        print(f"[INFO] Output          : {args.out}")
        print(f"[INFO] Input files     : {len(files)}")
        print(f"[INFO] Timestep hours  : {args.timestep_hours}")
        print(f"[INFO] Variable        : {args.input_variable} -> {args.output_variable}")

        if args.dry_run:
            print("[INFO] Dry run. Input files:")
            for path in files:
                print(path)
            return 0

        ds_raw = open_inputs(files)
        ds_out = build_output_dataset(ds_raw, args, start, end)

        save_output(
            ds_out,
            args.out,
            varname=args.output_variable,
            fill_value=args.fill_value,
            overwrite=args.overwrite,
        )

        print(f"[INFO] Wrote: {args.out}")
        print(f"[INFO] Output dimensions: {dict(ds_out.sizes)}")

        return 0

    except Exception as exc:
        print(f"[ERROR] {exc}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
