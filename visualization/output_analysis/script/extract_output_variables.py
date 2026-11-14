#!/usr/bin/env python3
"""Extract dated 2-D fields from annual CaMa-Flood NetCDF outputs."""

from __future__ import annotations

import argparse
import csv
from datetime import date, datetime, timezone
from pathlib import Path
import sys

import numpy as np
from netCDF4 import Dataset, num2date

SCRIPT_DIR = Path(__file__).resolve().parent
RIVER_MAP_SCRIPT_DIR = SCRIPT_DIR.parents[1] / "river_map" / "script"
sys.path.insert(0, str(RIVER_MAP_SCRIPT_DIR))

from river_map_common import read_geometry


def _copy_attributes(source, target) -> None:
    for name in source.ncattrs():
        if name != "_FillValue":
            target.setncattr(name, source.getncattr(name))


def _validate_grid(lon: np.ndarray, lat: np.ndarray, params_path: Path):
    nx, ny, resolution, west, east, south, north = read_geometry(params_path)
    expected_lon = west + (np.arange(nx) + 0.5) * resolution
    expected_lat = north - (np.arange(ny) + 0.5) * resolution
    if lon.shape != (nx,) or lat.shape != (ny,):
        raise ValueError(
            f"Coordinate lengths lon={lon.size}, lat={lat.size} do not match "
            f"params.txt grid {nx}x{ny}"
        )
    if not (np.all(np.diff(lon) > 0) and np.all(np.diff(lat) < 0)):
        raise ValueError("Expected longitude ascending eastward and latitude north-first")
    if (not np.allclose(lon, expected_lon, rtol=0, atol=1e-5) or
            not np.allclose(lat, expected_lat, rtol=0, atol=1e-5)):
        raise ValueError("NetCDF cell centers do not match the map params.txt grid")
    if (not np.isclose(east, west + nx * resolution) or
            not np.isclose(north, south + ny * resolution)):
        raise ValueError("params.txt dimensions, resolution, and bounds are inconsistent")
    return nx, ny


def _find_date_record(times, target_date: date):
    matches = [
        index for index, value in enumerate(times)
        if (value.year, value.month, value.day, value.hour, value.minute, value.second)
        == (target_date.year, target_date.month, target_date.day, 0, 0, 0)
    ]
    if len(matches) != 1:
        raise ValueError(
            f"Expected exactly one midnight CF time for {target_date}; found {len(matches)}"
        )
    return matches[0], times[matches[0]]


def _append_log(log_path: Path, record: dict[str, str]) -> None:
    fields = (
        "extracted_at_utc", "variable", "requested_date", "cf_time",
        "source_file", "source_record_index_zero_based", "destination_file",
        "dimensions", "shape", "dtype", "units",
    )
    log_path.parent.mkdir(parents=True, exist_ok=True)
    write_header = not log_path.exists() or log_path.stat().st_size == 0
    with log_path.open("a", newline="", encoding="utf-8") as stream:
        writer = csv.DictWriter(stream, fieldnames=fields)
        if write_header:
            writer.writeheader()
        writer.writerow(record)


def extract_output_variable(
    source_dir: Path,
    variable_name: str,
    target_date: date,
    params_path: Path,
    destination: Path,
) -> dict[str, str]:
    """Copy one variable's exact CF-dated record and required static metadata."""
    source_path = source_dir / f"o_{variable_name}{target_date.year:04d}.nc"
    if not source_path.is_file():
        raise FileNotFoundError(f"Annual output file not found: {source_path}")
    destination.parent.mkdir(parents=True, exist_ok=True)

    with Dataset(source_path, "r") as source:
        for name in ("time", variable_name):
            if name not in source.variables:
                raise ValueError(f"{source_path} is missing variable {name!r}")
        time_var = source.variables["time"]
        data_var = source.variables[variable_name]
        units = str(getattr(data_var, "units", ""))
        if not data_var.dimensions or data_var.dimensions[0] != "time":
            raise ValueError(
                f"Expected {variable_name} to start with time; found {data_var.dimensions}"
            )
        if not np.issubdtype(data_var.dtype, np.number):
            raise ValueError(f"Output variable {variable_name} is not numeric")
        if not time_var.units or not np.issubdtype(time_var.dtype, np.number):
            raise ValueError(f"{source_path} has an invalid CF time coordinate")

        calendar = getattr(time_var, "calendar", "standard")
        times = num2date(
            time_var[:], time_var.units, calendar=calendar,
            only_use_cftime_datetimes=False,
        )
        record_index, selected_time = _find_date_record(times, target_date)
        if variable_name == "outflw":
            if data_var.dimensions != ("time", "lat", "lon"):
                raise ValueError(
                    f"Expected outflw dimensions (time, lat, lon); found {data_var.dimensions}"
                )
            for name in ("lat", "lon"):
                if name not in source.variables:
                    raise ValueError(f"{source_path} is missing variable {name!r}")
            lon = np.asarray(source.variables["lon"][:], dtype=np.float64)
            lat = np.asarray(source.variables["lat"][:], dtype=np.float64)
            nx, ny = _validate_grid(lon, lat, params_path)
            if data_var.shape[1:] != (ny, nx):
                raise ValueError(
                    f"outflw grid shape {data_var.shape[1:]} does not match "
                    f"params.txt grid {(ny, nx)}"
                )
        elif variable_name == "pthflw":
            if len(data_var.dimensions) != 3:
                raise ValueError(
                    f"Expected pthflw dimensions (time, layer, path); found {data_var.dimensions}"
                )
            path_dim = source.variables.get("pth_id")
            if path_dim is None or path_dim.dimensions != (data_var.dimensions[2],):
                raise ValueError("pthflw path dimension does not match one-dimensional pth_id")
            if units != "m3/s" or getattr(data_var, "cell_methods", "") != "time: mean":
                raise ValueError("pthflw must be a daily-mean discharge in m3/s")
        data_var.set_auto_maskandscale(False)
        field = np.asarray(data_var[record_index, ...])
        output_dimensions = data_var.dimensions
        output_shape = (1, *field.shape)
        selected_time_value = np.asarray(time_var[record_index:record_index + 1])
        with Dataset(destination, "w", format="NETCDF4_CLASSIC") as sample:
            sample.createDimension("time", None)
            data_dimensions = data_var.dimensions[1:]
            for dim_name in data_dimensions:
                if dim_name == "time":
                    raise ValueError(f"Unexpected repeated time dimension in {data_var.dimensions}")
                sample.createDimension(dim_name, len(source.dimensions[dim_name]))

            # Keep coordinate and path metadata needed to interpret the extracted data.
            for name, source_coord in source.variables.items():
                if name in ("time", variable_name) or "time" in source_coord.dimensions:
                    continue
                if source_coord.dimensions and set(source_coord.dimensions).issubset(data_dimensions):
                    output_coord = sample.createVariable(
                        name, source_coord.dtype, source_coord.dimensions,
                    )
                    _copy_attributes(source_coord, output_coord)
                    output_coord[:] = source_coord[:]

            output_time = sample.createVariable("time", time_var.dtype, ("time",))
            _copy_attributes(time_var, output_time)
            output_time[:] = selected_time_value

            create_options = {"zlib": True, "complevel": 4, "shuffle": True}
            fill_value = getattr(data_var, "_FillValue", None)
            if fill_value is not None:
                create_options["fill_value"] = fill_value
            output_data = sample.createVariable(
                variable_name, data_var.dtype, data_var.dimensions, **create_options,
            )
            _copy_attributes(data_var, output_data)
            output_data[0, ...] = field
            for attribute in source.ncattrs():
                sample.setncattr(attribute, source.getncattr(attribute))
            sample.setncattr("source_filename", source_path.name)
            sample.setncattr("source_variable", variable_name)
            sample.setncattr("source_record_index_zero_based", record_index)
            sample.setncattr("requested_date", target_date.isoformat())
            sample.setncattr("selected_time", str(selected_time))
            sample.setncattr(
                "temporal_note",
                "Record selected by matching the requested date to the CF time coordinate.",
            )
            source_history = getattr(source, "history", "")
            extract_history = (
                f"Extracted {variable_name} at CF time {selected_time} from "
                f"{source_path.name}, record index {record_index} (zero-based)."
            )
            sample.setncattr(
                "history",
                f"{source_history}\n{extract_history}" if source_history else extract_history,
            )

    record = {
        "extracted_at_utc": datetime.now(timezone.utc).isoformat(timespec="seconds"),
        "variable": variable_name,
        "requested_date": target_date.isoformat(),
        "cf_time": str(selected_time),
        "source_file": str(source_path.resolve()),
        "source_record_index_zero_based": str(record_index),
        "destination_file": str(destination.resolve()),
        "dimensions": ",".join(output_dimensions),
        "shape": ",".join(str(size) for size in output_shape),
        "dtype": str(field.dtype),
        "units": units,
    }
    _append_log(destination.parent / "extraction_log.csv", record)
    return record


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source-dir", type=Path, required=True)
    parser.add_argument("--variable", required=True, help="NetCDF output variable name")
    parser.add_argument("--date", required=True, help="Requested CF date, YYYY-MM-DD")
    parser.add_argument("--params", type=Path, required=True, help="Map params.txt")
    parser.add_argument("--output", type=Path, required=True, help="One-record NetCDF path")
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    try:
        target_date = date.fromisoformat(args.date)
        record = extract_output_variable(
            args.source_dir, args.variable, target_date, args.params, args.output,
        )
    except (ValueError, OSError, RuntimeError) as exc:
        raise SystemExit(f"Output extraction failed: {exc}") from exc

    print("Extracted simulation output:")
    print(f"  Variable:        {record['variable']}")
    print(f"  Requested date:  {record['requested_date']}")
    print(f"  CF time:         {record['cf_time']}")
    print(f"  Source file:     {record['source_file']}")
    print(f"  Source record:   {record['source_record_index_zero_based']} (0-based)")
    print(f"  Destination:     {record['destination_file']}")
    print(f"  Data:            {record['shape']} {record['dtype']} {record['units']}")
    print(f"  Extraction log:  {args.output.parent / 'extraction_log.csv'}")


if __name__ == "__main__":
    main()
