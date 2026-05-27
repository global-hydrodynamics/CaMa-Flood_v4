#!/usr/bin/env python3
#==========================================================
# Convert downloaded ERA5-Land NetCDF runoff files to
# CaMa-Flood/GrADS-style plain binary runoff files.
#
# CaMa-Flood binary file convention:
#   - Files are daily:
#       runoff_YYYYMMDD.bin
#   - Sub-daily forcing is stored as multiple time records
#     inside each daily file.
#
# Examples:
#   output timestep 24h:
#       runoff_20000101.bin contains 1 record
#       ctl dset: ^./runoff_%y4%m2%d2.bin
#       ctl tdef: 1dy
#
#   output timestep 3h:
#       runoff_20000101.bin contains 8 records
#       runoff_20000102.bin contains 8 records
#       ctl dset: ^./runoff_%y4%m2%d2.bin
#       ctl tdef: 3hr
#
# Input:
#   ERA5-Land accumulated runoff [m]
#
# Output:
#   runoff accumulated over each output timestep [mm/timestep]
#
# Required Python packages:
#   numpy pandas xarray netCDF4
#==========================================================

from __future__ import annotations

import argparse
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
        description="Convert ERA5-Land NetCDF runoff to CaMa-Flood/GrADS daily binary forcing."
    )

    parser.add_argument("--start", required=True, help="Start date, inclusive: YYYY-MM-DD")
    parser.add_argument("--end", required=True, help="End date, inclusive: YYYY-MM-DD")

    parser.add_argument("--indir", type=Path, required=True, help="Input NetCDF directory")
    parser.add_argument("--outdir", type=Path, required=True, help="Output binary directory")

    parser.add_argument("--input-file-unit", choices=["daily", "monthly"], required=True)
    parser.add_argument("--input-pattern", required=True)
    parser.add_argument("--input-variable", default="runoff")

    parser.add_argument(
        "--timestep-hours",
        type=int,
        required=True,
        help="Timestep in downloaded NetCDF files. Must divide 24.",
    )
    parser.add_argument(
        "--output-timestep-hours",
        type=int,
        default=None,
        help=(
            "Timestep of output binary forcing. "
            "Default: same as --timestep-hours. "
            "Use 24 for daily binary output."
        ),
    )

    parser.add_argument("--binary-prefix", default="runoff_")
    parser.add_argument("--binary-suffix", default=".bin")
    parser.add_argument("--ctl-file", type=Path, required=True)

    parser.add_argument("--undef", type=float, default=1.0e20)
    parser.add_argument("--grads-variable", default="var")
    parser.add_argument("--grads-title", default="ERA5Land Runoff")
    parser.add_argument("--endian", choices=["little", "big"], default="little")
    parser.add_argument("--skip-existing", type=int, choices=[0, 1], default=0)
    parser.add_argument("--dry-run", action="store_true")

    return parser.parse_args()


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


def collect_input_files(indir: Path, pattern: str, unit: str, start: date, end: date) -> list[Path]:
    files: list[Path] = []
    missing: list[Path] = []

    iterator = date_range(start, end) if unit == "daily" else month_range(start, end)

    for d in iterator:
        path = indir / d.strftime(pattern)
        if path.exists():
            files.append(path)
        else:
            missing.append(path)

    if missing:
        msg = "\n".join(str(p) for p in missing[:20])
        if len(missing) > 20:
            msg += f"\n... and {len(missing) - 20} more"
        raise FileNotFoundError(f"Missing input files:\n{msg}")

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
        print(f"[INFO] Open: {path}", flush=True)
        ds = xr.open_dataset(path)
        ds = standardize_coords(ds)
        datasets.append(ds)

    if len(datasets) == 1:
        ds_all = datasets[0]
    else:
        ds_all = xr.concat(
            datasets,
            dim="time",
            data_vars="minimal",
            coords="minimal",
            compat="override",
        )

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


def coord_name(ds: xr.Dataset, names: list[str]) -> str:
    for name in names:
        if name in ds.coords:
            return name
        if name in ds.dims:
            return name
    raise KeyError(f"Could not find coordinate from candidates: {names}")


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


def accumulated_to_increment_daily_groups(da: xr.DataArray) -> xr.DataArray:
    """Convert accumulated runoff [m] to timestep increments [m].

    Differencing is applied within each UTC day. This works for both
    daily input files and monthly input files because grouping uses the
    time coordinate, not file boundaries.
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
            pieces.append(inc)
            continue

        diff = sub.diff("time", label="upper")
        first = sub.isel(time=0)

        inc = xr.concat([first, diff], dim=sub["time"])
        inc = inc.assign_coords(time=sub["time"])

        # Negative increments are interpreted as reset/numerical issues.
        # Use the current accumulated value and clip remaining negatives.
        inc = inc.where(inc >= 0.0, sub)
        inc = inc.clip(min=0.0)

        pieces.append(inc)

    if not pieces:
        raise ValueError("No time steps found after differencing.")

    return xr.concat(pieces, dim="time").sortby("time")


def make_output_runoff_mm(
    da_acc_m: xr.DataArray,
    start: date,
    end: date,
    input_timestep_hours: int,
    output_timestep_hours: int,
) -> xr.DataArray:
    """Return runoff [mm/output timestep]."""

    inc_m = accumulated_to_increment_daily_groups(da_acc_m)
    inc_mm = inc_m * 1000.0

    start64 = np.datetime64(start.isoformat())
    end64 = np.datetime64((end + timedelta(days=1)).isoformat())

    inc_mm = inc_mm.sel(time=(inc_mm["time"] >= start64) & (inc_mm["time"] < end64))

    if output_timestep_hours == input_timestep_hours:
        out = inc_mm
    else:
        if output_timestep_hours < input_timestep_hours:
            raise ValueError(
                "--output-timestep-hours must be greater than or equal to --timestep-hours "
                "when temporal aggregation is needed."
            )
        if output_timestep_hours % input_timestep_hours != 0:
            raise ValueError("--output-timestep-hours must be a multiple of --timestep-hours.")

        out = inc_mm.resample(time=f"{output_timestep_hours}h").sum(skipna=False)
        out = out.sel(time=(out["time"] >= start64) & (out["time"] < end64))

    out.name = "runoff"
    out.attrs["units"] = f"mm/{output_timestep_hours}h" if output_timestep_hours != 24 else "mm/day"
    return out


def format_grads_time(ts: pd.Timestamp) -> str:
    mon = ["jan", "feb", "mar", "apr", "may", "jun",
           "jul", "aug", "sep", "oct", "nov", "dec"][ts.month - 1]
    return f"{ts.hour:02d}Z{ts.day:02d}{mon}{ts.year:04d}"


def ctl_tdef_step(output_timestep_hours: int) -> str:
    if output_timestep_hours == 24:
        return "1dy"
    return f"{output_timestep_hours}hr"


def daily_binary_filename(ts: pd.Timestamp, prefix: str, suffix: str) -> str:
    """Daily file name even when records are sub-daily."""
    return f"{prefix}{ts.year:04d}{ts.month:02d}{ts.day:02d}{suffix}"


def ctl_dset_template(binary_prefix: str, binary_suffix: str) -> str:
    """CaMa-Flood convention: daily file template only."""
    return f"^./{binary_prefix}%y4%m2%d2{binary_suffix}"


def write_ctl(
    ctl_file: Path,
    nx: int,
    ny: int,
    start_time: pd.Timestamp,
    nt: int,
    x0: float,
    dx: float,
    y0: float,
    dy: float,
    undef: float,
    title: str,
    varname: str,
    endian: str,
    binary_prefix: str,
    binary_suffix: str,
    output_timestep_hours: int,
) -> None:
    options = ["yrev", "template"]
    if endian == "little":
        options.insert(1, "little_endian")

    dset = ctl_dset_template(binary_prefix, binary_suffix)
    step = ctl_tdef_step(output_timestep_hours)

    unit_label = "mm/day" if output_timestep_hours == 24 else f"mm/{output_timestep_hours}h"

    lines = [
        f"dset  {dset}",
        f"undef {undef:.6g}",
        f"title {title}",
        f"options {' '.join(options)}",
        f"xdef  {nx:5d} linear {x0:.10g} {dx:.10g}",
        f"ydef  {ny:5d} linear {y0:.10g} {dy:.10g}",
        f"tdef  {nt:5d} linear  {format_grads_time(start_time)} {step}",
        "zdef      1 linear  1 1",
        "vars 1",
        f"{varname} 0 99       ** runoff [{unit_label}]",
        "ENDVARS",
        "",
    ]

    ctl_file.parent.mkdir(parents=True, exist_ok=True)
    ctl_file.write_text("\n".join(lines), encoding="utf-8")
    print(f"[INFO] Wrote CTL: {ctl_file}", flush=True)


def write_binary_files(
    runoff: xr.DataArray,
    outdir: Path,
    binary_prefix: str,
    binary_suffix: str,
    undef: float,
    endian: str,
    skip_existing: int,
) -> None:
    lon_name = coord_name(runoff.to_dataset(name="runoff"), ["lon", "longitude", "x"])
    lat_name = coord_name(runoff.to_dataset(name="runoff"), ["lat", "latitude", "y"])

    # Binary layout for each record:
    #   X: west to east
    #   Y: north to south
    # CTL uses ydef south-to-north plus options yrev.
    runoff = runoff.sortby(lon_name, ascending=True)
    runoff = runoff.sortby(lat_name, ascending=False)

    lon = np.asarray(runoff[lon_name].values, dtype=float)
    lat = np.asarray(runoff[lat_name].values, dtype=float)

    if lon.ndim != 1 or lat.ndim != 1:
        raise ValueError("This binary writer currently expects 1-D lon/lat coordinates.")

    nx = lon.size
    ny = lat.size

    dtype = "<f4" if endian == "little" else ">f4"
    undef32 = np.float32(undef)

    outdir.mkdir(parents=True, exist_ok=True)

    times = pd.to_datetime(runoff["time"].values)
    grouped_indices: dict[str, list[int]] = {}

    for it, ts in enumerate(times):
        outfile = outdir / daily_binary_filename(ts, binary_prefix, binary_suffix)
        grouped_indices.setdefault(str(outfile), []).append(it)

    for outfile_str, indices in grouped_indices.items():
        outfile = Path(outfile_str)

        if skip_existing == 1 and outfile.exists():
            print(f"[INFO] Skip existing: {outfile}", flush=True)
            continue

        with outfile.open("wb") as fp:
            for it in indices:
                arr = runoff.isel(time=it).values

                if arr.shape != (ny, nx):
                    raise ValueError(f"Unexpected array shape {arr.shape}; expected {(ny, nx)}")

                arr = np.asarray(arr, dtype=np.float32)
                arr = np.where(np.isfinite(arr), arr, undef32).astype(dtype, copy=False)
                arr.tofile(fp)

        print(f"[INFO] Wrote binary: {outfile} ({len(indices)} record(s))", flush=True)


def main() -> int:
    args = parse_args()

    start = date.fromisoformat(args.start)
    end = date.fromisoformat(args.end)

    if end < start:
        raise ValueError("--end is earlier than --start")

    if args.timestep_hours < 1 or args.timestep_hours > 24 or 24 % args.timestep_hours != 0:
        raise ValueError("--timestep-hours must divide 24 exactly.")

    output_timestep_hours = args.output_timestep_hours
    if output_timestep_hours is None:
        output_timestep_hours = args.timestep_hours

    if output_timestep_hours < 1 or output_timestep_hours > 24 or 24 % output_timestep_hours != 0:
        raise ValueError("--output-timestep-hours must divide 24 exactly.")

    input_files = collect_input_files(
        indir=args.indir,
        pattern=args.input_pattern,
        unit=args.input_file_unit,
        start=start,
        end=end,
    )

    print(f"[INFO] Input files: {len(input_files)}", flush=True)
    print(f"[INFO] Input timestep hours : {args.timestep_hours}", flush=True)
    print(f"[INFO] Output timestep hours: {output_timestep_hours}", flush=True)
    print("[INFO] Binary file convention: one file per day; sub-daily steps are records inside each daily file.", flush=True)

    if args.dry_run:
        for path in input_files:
            print(path)
        return 0

    ds = open_inputs(input_files)
    varname = find_variable(ds, args.input_variable)

    lon_name = coord_name(ds, ["lon", "longitude", "x"])
    lat_name = coord_name(ds, ["lat", "latitude", "y"])

    runoff = make_output_runoff_mm(
        da_acc_m=ds[varname],
        start=start,
        end=end,
        input_timestep_hours=args.timestep_hours,
        output_timestep_hours=output_timestep_hours,
    )

    runoff_sorted = runoff.sortby(lon_name, ascending=True).sortby(lat_name, ascending=False)

    lon = np.asarray(runoff_sorted[lon_name].values, dtype=float)
    lat = np.asarray(runoff_sorted[lat_name].values, dtype=float)

    if lon.ndim != 1 or lat.ndim != 1:
        raise ValueError("This binary writer currently expects 1-D lon/lat coordinates.")

    nx = lon.size
    ny = lat.size
    nt = runoff_sorted.sizes["time"]

    dx = infer_resolution(lon)
    dy = infer_resolution(lat)

    if not np.isfinite(dx) or not np.isfinite(dy):
        raise ValueError(f"Could not infer grid spacing: dx={dx}, dy={dy}")

    x0 = float(np.nanmin(lon))
    y0 = float(np.nanmin(lat))
    start_time = pd.to_datetime(runoff_sorted["time"].values[0])

    write_binary_files(
        runoff=runoff_sorted,
        outdir=args.outdir,
        binary_prefix=args.binary_prefix,
        binary_suffix=args.binary_suffix,
        undef=args.undef,
        endian=args.endian,
        skip_existing=args.skip_existing,
    )

    write_ctl(
        ctl_file=args.ctl_file,
        nx=nx,
        ny=ny,
        start_time=start_time,
        nt=nt,
        x0=x0,
        dx=dx,
        y0=y0,
        dy=dy,
        undef=float(args.undef),
        title=args.grads_title,
        varname=args.grads_variable,
        endian=args.endian,
        binary_prefix=args.binary_prefix,
        binary_suffix=args.binary_suffix,
        output_timestep_hours=output_timestep_hours,
    )

    print("[INFO] Done.", flush=True)
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except Exception as exc:
        print(f"[ERROR] {exc}", file=sys.stderr)
        sys.exit(1)
