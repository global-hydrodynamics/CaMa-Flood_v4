#!/usr/bin/env python3
"""Extract modeled daily outflw at the stations listed in the location CSV."""

from __future__ import annotations

import argparse
import csv
from datetime import date, datetime, timedelta
from pathlib import Path

import numpy as np
from netCDF4 import Dataset, num2date


YEARS = (2000, 2001)
VARIABLE = "outflw"
UNITS = "m3/s"

STATION_FIELDS = (
    "station_name", "plot_label", "longitude_deg_e", "latitude_deg_n",
    "upstream_drainage_area_km2", "map_x_1based", "map_y_1based",
    "python_array_column_0based", "python_array_row_0based", "index_language",
    "map_cell_center_longitude_deg_e", "map_cell_center_latitude_deg_n",
    "cell_assignment_method",
)


def read_stations(path: Path) -> list[dict[str, str | float | int]]:
    """Read stations and verify the explicit map/Python index conventions."""
    with path.open(newline="", encoding="utf-8") as stream:
        reader = csv.DictReader(stream)
        missing = set(STATION_FIELDS) - set(reader.fieldnames or ())
        if missing:
            raise ValueError(f"{path}: missing columns: {', '.join(sorted(missing))}")
        stations = []
        for line_number, row in enumerate(reader, start=2):
            try:
                ix = int(row["map_x_1based"])
                iy = int(row["map_y_1based"])
                column = int(row["python_array_column_0based"])
                row_index = int(row["python_array_row_0based"])
                lon = float(row["longitude_deg_e"])
                lat = float(row["latitude_deg_n"])
                cell_lon = float(row["map_cell_center_longitude_deg_e"])
                cell_lat = float(row["map_cell_center_latitude_deg_n"])
                area = float(row["upstream_drainage_area_km2"])
            except (TypeError, ValueError) as exc:
                raise ValueError(f"{path}:{line_number}: invalid station value") from exc
            if row["index_language"] != "Python":
                raise ValueError(
                    f"{path}:{line_number}: index_language must be 'Python'"
                )
            if (ix < 1 or iy < 1 or column != ix - 1 or row_index != iy - 1):
                raise ValueError(
                    f"{path}:{line_number}: map x/y and Python row/column indices disagree"
                )
            stations.append({
                "station_name": row["station_name"],
                "plot_label": row["plot_label"],
                "longitude_deg_e": lon,
                "latitude_deg_n": lat,
                "upstream_drainage_area_km2": area,
                "map_x_1based": ix,
                "map_y_1based": iy,
                "python_array_column_0based": column,
                "python_array_row_0based": row_index,
                "index_language": row["index_language"],
                "map_cell_center_longitude_deg_e": cell_lon,
                "map_cell_center_latitude_deg_n": cell_lat,
                "cell_assignment_method": row["cell_assignment_method"],
            })
    if not stations:
        raise ValueError(f"{path}: no station rows found")
    return stations


def read_annual_stations(path: Path, year: int, stations: list[dict]):
    """Extract each selected cell and convert interval-end timestamps to day labels."""
    with Dataset(path, "r") as source:
        for name in ("time", "lat", "lon", VARIABLE):
            if name not in source.variables:
                raise ValueError(f"{path}: missing variable {name!r}")
        time_var = source.variables["time"]
        data_var = source.variables[VARIABLE]
        if data_var.dimensions != ("time", "lat", "lon"):
            raise ValueError(f"{path}: unexpected {VARIABLE} dimensions {data_var.dimensions}")
        if getattr(data_var, "units", "") != UNITS:
            raise ValueError(
                f"{path}: expected {VARIABLE} units {UNITS}, "
                f"found {getattr(data_var, 'units', '')!r}"
            )
        if getattr(data_var, "cell_methods", "") != "time: mean":
            raise ValueError(f"{path}: expected daily-mean {VARIABLE} output")
        if not time_var.units:
            raise ValueError(f"{path}: time coordinate has no CF units")

        calendar = getattr(time_var, "calendar", "standard")
        timestamps = num2date(
            time_var[:], time_var.units, calendar=calendar,
            only_use_cftime_datetimes=False,
        )
        expected_count = (date(year + 1, 1, 1) - date(year, 1, 1)).days
        if len(timestamps) != expected_count:
            raise ValueError(
                f"{path}: expected {expected_count} daily records, found {len(timestamps)}"
            )
        expected_stamps = [
            datetime(year, 1, 1) + timedelta(days=index + 1)
            for index in range(expected_count)
        ]
        actual_stamps = [
            datetime(value.year, value.month, value.day, value.hour, value.minute,
                     value.second)
            for value in timestamps
        ]
        if actual_stamps != expected_stamps:
            raise ValueError(f"{path}: CF times are not daily interval-end stamps for {year}")

        lat = np.asarray(source.variables["lat"][:], dtype=float)
        lon = np.asarray(source.variables["lon"][:], dtype=float)
        values = np.ma.masked_all((len(timestamps), len(stations)), dtype=np.float32)
        data_var.set_auto_maskandscale(True)
        for station_index, station in enumerate(stations):
            row = station["python_array_row_0based"]
            column = station["python_array_column_0based"]
            if not (0 <= row < len(lat) and 0 <= column < len(lon)):
                raise ValueError(
                    f"{station['station_name']}: Python array index ({row}, {column}) "
                    f"outside grid ({len(lat)}, {len(lon)})"
                )
            if not np.isclose(lon[column], station["map_cell_center_longitude_deg_e"],
                              rtol=0, atol=1e-5):
                raise ValueError(f"{station['station_name']}: longitude cell center mismatch")
            if not np.isclose(lat[row], station["map_cell_center_latitude_deg_n"],
                              rtol=0, atol=1e-5):
                raise ValueError(f"{station['station_name']}: latitude cell center mismatch")
            values[:, station_index] = data_var[:, row, column]

        # CaMa-Flood stamps each daily mean at its interval end; report the day it represents.
        represented_dates = [value.date() - timedelta(days=1) for value in timestamps]
        expected_dates = [date(year, 1, 1) + timedelta(days=index)
                          for index in range(expected_count)]
        if represented_dates != expected_dates:
            raise ValueError(f"{path}: interval-end times do not represent each day of {year}")
        return represented_dates, values, lon, lat


def write_products(output_dir: Path, stations: list[dict], dates: list[date],
                   values: np.ma.MaskedArray, source_files: list[Path],
                   lon: np.ndarray, lat: np.ndarray) -> tuple[Path, Path]:
    """Write a long-form CSV and a time-by-station NetCDF analysis file."""
    output_dir.mkdir(parents=True, exist_ok=True)
    csv_path = output_dir / "station_outflw_daily_2000-2001.csv"
    nc_path = output_dir / "station_outflw_daily_2000-2001.nc"
    csv_fields = [
        "date", *STATION_FIELDS[:-1], "cell_assignment_method", "outflw_m3_s",
    ]
    with csv_path.open("w", newline="", encoding="utf-8") as stream:
        writer = csv.DictWriter(stream, fieldnames=csv_fields)
        writer.writeheader()
        for time_index, day in enumerate(dates):
            for station_index, station in enumerate(stations):
                discharge = values[time_index, station_index]
                row = {name: station[name] for name in STATION_FIELDS}
                row["date"] = day.isoformat()
                row["outflw_m3_s"] = (
                    "" if np.ma.is_masked(discharge) else f"{float(discharge):.9g}"
                )
                writer.writerow(row)

    with Dataset(nc_path, "w", format="NETCDF4") as output:
        output.createDimension("time", len(dates))
        output.createDimension("station", len(stations))
        time = output.createVariable("time", "i4", ("time",))
        time[:] = np.asarray([(day - date(2000, 1, 1)).days for day in dates],
                             dtype=np.int32)
        time.units = "days since 2000-01-01 00:00:00"
        time.calendar = "proleptic_gregorian"
        time.long_name = "daily interval represented by the model output"

        string_fields = ("station_name", "plot_label", "cell_assignment_method")
        for field in string_fields:
            variable = output.createVariable(field, str, ("station",))
            for station_index, station in enumerate(stations):
                variable[station_index] = str(station[field])

        numeric_fields = {
            "longitude": ("longitude_deg_e", "f8", "degrees_east"),
            "latitude": ("latitude_deg_n", "f8", "degrees_north"),
            "upstream_drainage_area": (
                "upstream_drainage_area_km2", "f8", "km2"
            ),
            "map_x": ("map_x_1based", "i4", "1-based map grid x index"),
            "map_y": ("map_y_1based", "i4", "1-based map grid y index"),
            "python_array_column": (
                "python_array_column_0based", "i4", "0-based Python array column"
            ),
            "python_array_row": (
                "python_array_row_0based", "i4", "0-based Python array row"
            ),
            "map_cell_center_longitude": (
                "map_cell_center_longitude_deg_e", "f8", "degrees_east"
            ),
            "map_cell_center_latitude": (
                "map_cell_center_latitude_deg_n", "f8", "degrees_north"
            ),
        }
        for output_name, (station_field, dtype, units) in numeric_fields.items():
            variable = output.createVariable(output_name, dtype, ("station",))
            variable[:] = [station[station_field] for station in stations]
            variable.units = units
        output.variables["map_x"].index_origin = 1
        output.variables["map_y"].index_origin = 1
        output.variables["python_array_column"].index_origin = 0
        output.variables["python_array_row"].index_origin = 0

        discharge_var = output.createVariable(
            VARIABLE, "f4", ("time", "station"), fill_value=-9999.0,
            zlib=True, complevel=4, shuffle=True,
        )
        discharge_var[:] = values.filled(-9999.0)
        discharge_var.units = UNITS
        discharge_var.long_name = "daily mean modeled discharge at selected station cells"
        discharge_var.cell_methods = "time: mean"

        output.title = "CaMa-Flood daily outflw sampled at Mekong observation locations"
        output.source_files = ", ".join(str(path.resolve()) for path in source_files)
        output.station_locations_file = "station_map_locations.csv"
        output.source_variable = VARIABLE
        output.processing = (
            "Sampled using zero-based Python row/column indices from the station CSV. "
            "Source CF timestamps are interval ends; each is labeled by the preceding "
            "represented calendar day."
        )
    return csv_path, nc_path


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source-dir", type=Path, required=True)
    parser.add_argument("--stations-csv", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()

    try:
        stations = read_stations(args.stations_csv)
        all_dates: list[date] = []
        all_values = []
        source_files = []
        reference_coordinates = None
        for year in YEARS:
            source_path = args.source_dir / f"o_{VARIABLE}{year}.nc"
            dates, values, lon, lat = read_annual_stations(source_path, year, stations)
            if reference_coordinates is None:
                reference_coordinates = (lon, lat)
            elif not (np.array_equal(lon, reference_coordinates[0])
                      and np.array_equal(lat, reference_coordinates[1])):
                raise ValueError(f"{source_path}: grid differs from the other annual file")
            all_dates.extend(dates)
            all_values.append(values)
            source_files.append(source_path)

        dates = all_dates
        values = np.ma.concatenate(all_values, axis=0)
        expected_dates = [date(2000, 1, 1) + timedelta(days=index)
                          for index in range(366 + 365)]
        if dates != expected_dates:
            raise ValueError("Combined annual files do not form a continuous 2000–2001 series")
        csv_path, nc_path = write_products(
            args.output_dir, stations, dates, values, source_files,
            reference_coordinates[0], reference_coordinates[1],
        )
    except (OSError, ValueError, RuntimeError) as exc:
        raise SystemExit(f"Station outflw extraction failed: {exc}") from exc

    print(f"Stations:     {len(stations)}")
    print(f"Dates:        {dates[0]} through {dates[-1]} ({len(dates)} days)")
    print(f"Source files: {', '.join(path.name for path in source_files)}")
    print(f"CSV:          {csv_path}")
    print(f"NetCDF:       {nc_path}")


if __name__ == "__main__":
    main()
