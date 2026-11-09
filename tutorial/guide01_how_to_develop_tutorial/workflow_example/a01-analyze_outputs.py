#!/usr/bin/env python3
"""Validate paired 2000 runs and write compact, reproducible diagnostics."""

from __future__ import annotations

import argparse
import csv
from pathlib import Path

import numpy as np
import pandas as pd
import xarray as xr

ROOT = Path(__file__).resolve().parent
REPO = ROOT.parents[2]
MAP_DIR = REPO / "map/mekong_15min"
OUT = ROOT / "out"
ANALYSIS = ROOT / "analysis"
VARIABLES = ("outflw", "rivdph", "sfcelv", "fldfrc")
SITES = (
    ("Vientiane", 102.613, 17.929),
    ("Phnom Penh", 104.932, 11.562),
    ("Can Tho", 105.79775, 10.058),
)
REGION = {"west": 104.0, "east": 108.0, "south": 8.0, "north": 13.0}


def valid_values(array: xr.DataArray) -> np.ndarray:
    values = np.asarray(array.values, dtype=np.float64)
    fill = array.attrs.get("_FillValue", array.encoding.get("_FillValue"))
    missing = array.attrs.get("missing_value")
    valid = np.isfinite(values)
    for sentinel in (fill, missing):
        if sentinel is not None:
            valid &= values != float(sentinel)
    return np.where(valid, values, np.nan)


def open_outputs(case: str) -> tuple[xr.Dataset, dict[str, xr.DataArray]]:
    folder = OUT / case
    datasets: dict[str, xr.Dataset] = {}
    arrays: dict[str, xr.DataArray] = {}
    for variable in VARIABLES:
        path = folder / f"o_{variable}2000.nc"
        if not path.is_file():
            raise FileNotFoundError(f"Missing {case} output: {path}")
        ds = xr.open_dataset(path, decode_times=True, mask_and_scale=True)
        datasets[variable] = ds
        if variable not in ds:
            raise ValueError(f"{path} has no variable named {variable}")
        arrays[variable] = ds[variable].load()
    reference = datasets[VARIABLES[0]]
    for variable in VARIABLES[1:]:
        candidate = datasets[variable]
        for coordinate in ("time", "lat", "lon"):
            if not np.array_equal(reference[coordinate].values, candidate[coordinate].values):
                raise ValueError(f"{case}: {variable} coordinate {coordinate} differs across output files")
    return reference, arrays


def nearest_valid_cell(lon2d: np.ndarray, lat2d: np.ndarray, valid: np.ndarray,
                       target_lon: float, target_lat: float) -> tuple[int, int, float]:
    dy = lat2d - target_lat
    dx = (lon2d - target_lon) * np.cos(np.deg2rad(target_lat))
    distance2 = dx * dx + dy * dy
    distance2[~valid] = np.inf
    if not np.isfinite(distance2).any():
        raise ValueError("No valid model cells are available for point selection")
    row, col = np.unravel_index(np.argmin(distance2), distance2.shape)
    distance_km = float(np.sqrt(distance2[row, col]) * 111.195)
    return int(row), int(col), distance_km


def write_outputs(validate_only: bool = False) -> None:
    control_ds, control = open_outputs("control")
    deep_ds, deep = open_outputs("deep_channel")
    for coordinate in ("time", "lat", "lon"):
        if not np.array_equal(control_ds[coordinate].values, deep_ds[coordinate].values):
            raise ValueError(f"Control and Deep Channel {coordinate} coordinates differ")
    if control_ds.sizes.get("time") != 366:
        raise ValueError(f"Expected 366 daily intervals for leap year 2000; got {control_ds.sizes.get('time')}")
    times = pd.to_datetime(control_ds.time.values)
    if times[0] != pd.Timestamp("2000-01-02") or times[-1] != pd.Timestamp("2001-01-01"):
        raise ValueError(f"Unexpected interval-end timestamps: {times[0]} through {times[-1]}")

    ctmare_ds = xr.open_dataset(MAP_DIR / "netcdf/ctmare.nc", decode_times=False, mask_and_scale=True)
    ctmare = ctmare_ds["ctmare"].load()
    if not np.array_equal(ctmare.lon.values, control_ds.lon.values) or not np.array_equal(ctmare.lat.values, control_ds.lat.values):
        raise ValueError("ctmare grid does not match model output grid")
    area = valid_values(ctmare)
    network_valid = np.isfinite(area) & (area > 0.0)
    lon2d, lat2d = np.meshgrid(control_ds.lon.values, control_ds.lat.values)

    site_rows = []
    for name, target_lon, target_lat in SITES:
        row, col, distance_km = nearest_valid_cell(lon2d, lat2d, network_valid, target_lon, target_lat)
        site_rows.append({
            "site": name, "selection_status": "provisional nearest valid catchment cell",
            "target_lon": target_lon, "target_lat": target_lat,
            "ix_fortran_1based": col + 1, "iy_fortran_1based": row + 1,
            "cell_lon": float(lon2d[row, col]), "cell_lat": float(lat2d[row, col]),
            "distance_km_approx": distance_km,
        })
    phnom = next(site for site in site_rows if site["site"] == "Phnom Penh")
    pp_row, pp_col = int(phnom["iy_fortran_1based"]) - 1, int(phnom["ix_fortran_1based"]) - 1

    pp_values = valid_values(control["sfcelv"][:, pp_row, pp_col])
    if not np.isfinite(pp_values).any():
        raise ValueError("Phnom Penh provisional cell has no valid Control sfcelv values")
    peak = float(np.nanmax(pp_values))
    event_index = int(np.flatnonzero(pp_values == peak)[0])  # Earliest timestamp wins ties.
    event_time = pd.Timestamp(times[event_index])
    represented_day = event_time - pd.Timedelta(days=1)

    summary_rows = []
    difference_arrays = {}
    for variable in VARIABLES:
        c = valid_values(control[variable])
        d = valid_values(deep[variable])
        matched = np.isfinite(c) & np.isfinite(d)
        if not np.array_equal(np.isfinite(c), np.isfinite(d)):
            raise ValueError(f"Control and Deep Channel valid masks differ for {variable}")
        if not matched.any():
            raise ValueError(f"No matched valid cells for {variable}")
        difference = d - c
        for experiment, data in (("control", c), ("deep_channel", d), ("deep_minus_control", difference)):
            summary_rows.append({
                "variable": variable, "experiment_or_difference": experiment,
                "units": control[variable].attrs.get("units", "unknown"),
                "valid_values": int(np.isfinite(data).sum()),
                "minimum": float(np.nanmin(data)), "mean": float(np.nanmean(data)),
                "maximum": float(np.nanmax(data)),
            })
        difference_arrays[f"delta_{variable}"] = xr.DataArray(
            difference[event_index].astype(np.float32),
            dims=("lat", "lon"),
            coords={"lat": control_ds.lat, "lon": control_ds.lon},
            attrs={"long_name": f"Deep Channel minus Control {variable}",
                   "units": control[variable].attrs.get("units", "unknown")},
        )
    fldfrc = valid_values(control["fldfrc"])
    if np.nanmin(fldfrc) < -1e-6 or np.nanmax(fldfrc) > 1.0 + 1e-6:
        raise ValueError("Control fldfrc contains values outside [0,1]")

    if validate_only:
        print("[SUCCESS] Outputs aligned: 366 daily records (2000 leap year), 80x120 grid.")
        print(f"[CHECK] First/last interval-end time: {times[0]} / {times[-1]}")
        print(f"[CHECK] Provisional Phnom Penh cell: ix={pp_col + 1}, iy={pp_row + 1}; event={represented_day.date()}")
        for site in site_rows:
            print(f"[SITE:PROVISIONAL] {site['site']}: ({site['cell_lon']:.3f}E, {site['cell_lat']:.3f}N), "
                  f"distance~{site['distance_km_approx']:.1f} km")
        return

    ANALYSIS.mkdir(exist_ok=True)
    with (ANALYSIS / "provisional_sites.csv").open("w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=site_rows[0].keys())
        writer.writeheader(); writer.writerows(site_rows)
    with (ANALYSIS / "output_summary.csv").open("w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=summary_rows[0].keys())
        writer.writeheader(); writer.writerows(summary_rows)

    event_row = {
        "selection_status": "provisional; researcher review required",
        "rule": "earliest interval-end timestamp attaining the maximum Control daily-snapshot sfcelv",
        "site": "Phnom Penh", "target_lon": phnom["target_lon"], "target_lat": phnom["target_lat"],
        "ix_fortran_1based": phnom["ix_fortran_1based"], "iy_fortran_1based": phnom["iy_fortran_1based"],
        "cell_lon": phnom["cell_lon"], "cell_lat": phnom["cell_lat"],
        "distance_km_approx": phnom["distance_km_approx"],
        "time_coordinate_interval_end": event_time.isoformat(),
        "represented_day": represented_day.date().isoformat(), "control_peak_sfcelv_m": peak,
    }
    with (ANALYSIS / "event_selection.csv").open("w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=event_row.keys())
        writer.writeheader(); writer.writerow(event_row)

    series_rows = []
    for site in site_rows:
        row, col = int(site["iy_fortran_1based"]) - 1, int(site["ix_fortran_1based"]) - 1
        for i, timestamp in enumerate(times):
            record = {"site": site["site"], "selection_status": site["selection_status"],
                      "ix_fortran_1based": col + 1, "iy_fortran_1based": row + 1,
                      "time_coordinate_interval_end": timestamp.isoformat(),
                      "represented_day": (timestamp - pd.Timedelta(days=1)).date().isoformat()}
            for variable in ("outflw", "rivdph", "sfcelv"):
                record[f"control_{variable}"] = float(valid_values(control[variable][i, row, col]))
                record[f"deep_channel_{variable}"] = float(valid_values(deep[variable][i, row, col]))
            series_rows.append(record)
    with (ANALYSIS / "provisional_timeseries.csv").open("w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=series_rows[0].keys())
        writer.writeheader(); writer.writerows(series_rows)

    west, east, south, north = REGION["west"], REGION["east"], REGION["south"], REGION["north"]
    region = ((lon2d >= west) & (lon2d < east) & (lat2d >= south) & (lat2d < north) & network_valid)
    area_rows = []
    for i, timestamp in enumerate(times):
        record = {"selection_status": "provisional bbox; cells selected by center coordinates",
                  "west": west, "east_exclusive": east, "south": south, "north_exclusive": north,
                  "time_coordinate_interval_end": timestamp.isoformat(),
                  "represented_day": (timestamp - pd.Timedelta(days=1)).date().isoformat()}
        for experiment, arrays in (("control", control), ("deep_channel", deep)):
            frac = valid_values(arrays["fldfrc"][i])
            valid = region & np.isfinite(frac)
            record[f"{experiment}_flooded_area_km2"] = float(np.sum(frac[valid] * area[valid]) / 1.0e6)
            record[f"{experiment}_included_cells"] = int(valid.sum())
        record["deep_minus_control_km2"] = record["deep_channel_flooded_area_km2"] - record["control_flooded_area_km2"]
        area_rows.append(record)
    with (ANALYSIS / "provisional_flooded_area.csv").open("w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=area_rows[0].keys())
        writer.writeheader(); writer.writerows(area_rows)

    event_ds = xr.Dataset(difference_arrays, attrs={
        "title": "Event-time model output differences: Deep Channel minus Control",
        "selection_status": "provisional; researcher review required",
        "event_time_coordinate_interval_end": event_time.isoformat(),
        "represented_day": represented_day.date().isoformat(),
        "selection_rule": event_row["rule"],
    })
    event_ds.to_netcdf(ANALYSIS / "difference_at_event.nc", engine="netcdf4")
    print("[SUCCESS] Wrote analysis summaries, provisional sites/event, regional area series, and event difference map.")
    print(f"[EVENT:PROVISIONAL] {event_row}")
    print(f"[REGION:PROVISIONAL] Cell-center box: {west}-{east}E, {south}-{north}N; boundary-crossing catchments assigned by center.")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--validate-only", action="store_true", help="Check model output contracts without writing analysis products")
    args = parser.parse_args()
    write_outputs(args.validate_only)


if __name__ == "__main__":
    main()
