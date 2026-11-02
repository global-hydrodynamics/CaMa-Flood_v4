#!/usr/bin/env python3
"""Validate the structure and grid arithmetic of runoff_datasets.csv."""

import csv
import math
import re
from pathlib import Path


REGISTRY = Path(__file__).with_name("runoff_datasets.csv")
REQUIRED_COLUMNS = {
    "dataset_id",
    "description",
    "format",
    "directory",
    "prefix",
    "suffix",
    "filename_pattern",
    "nc_variable",
    "GRSIZEIN",
    "WESTIN",
    "EASTIN",
    "NORTHIN",
    "SOUTHIN",
    "grid_nx",
    "grid_ny",
    "coordinate_names",
    "longitude_convention",
    "OLAT",
    "IFRQ_INP",
    "record_interval_hours",
    "units",
    "DROFUNIT",
    "LINPDAY",
    "period",
    "data_source",
    "source_url",
    "access_notes",
    "distribution_scope",
    "configuration_status",
    "notes",
}
READY_STATUSES = {"metadata_verified", "metadata_verified_time_label_open"}


def number(row, field):
    try:
        return float(row[field])
    except (KeyError, TypeError, ValueError):
        raise ValueError(f"{row.get('dataset_id', '<unknown>')}: {field} must be numeric")


def validate():
    with REGISTRY.open(newline="", encoding="utf-8") as stream:
        reader = csv.DictReader(stream)
        columns = reader.fieldnames or []
        missing = REQUIRED_COLUMNS - set(columns)
        if missing:
            raise ValueError(f"missing required columns: {', '.join(sorted(missing))}")
        rows = list(reader)

    if not rows:
        raise ValueError("registry has no dataset rows")

    dataset_ids = [row["dataset_id"].strip() for row in rows]
    if any(not value for value in dataset_ids):
        raise ValueError("dataset_id must not be blank")
    if len(dataset_ids) != len(set(dataset_ids)):
        raise ValueError("dataset_id values must be unique")

    for row in rows:
        dataset_id = row["dataset_id"]
        if not re.fullmatch(r"[A-Za-z0-9_-]+", dataset_id):
            raise ValueError(f"{dataset_id}: dataset_id must be safe to use as a ROFFTAG")
        if None in row:
            raise ValueError(f"{dataset_id}: row has more values than the header")
        if any(value is None for value in row.values()):
            raise ValueError(f"{dataset_id}: row has fewer values than the header")
        if row["format"] not in {"binary", "NetCDF"}:
            raise ValueError(f"{dataset_id}: unsupported format {row['format']!r}")
        if row["OLAT"] not in {"NtoS", "StoN"}:
            raise ValueError(f"{dataset_id}: OLAT must be NtoS or StoN")

        resolution = number(row, "GRSIZEIN")
        west = number(row, "WESTIN")
        east = number(row, "EASTIN")
        north = number(row, "NORTHIN")
        south = number(row, "SOUTHIN")
        nx = int(number(row, "grid_nx"))
        ny = int(number(row, "grid_ny"))
        if resolution <= 0 or nx <= 0 or ny <= 0:
            raise ValueError(f"{dataset_id}: resolution and grid dimensions must be positive")
        if east <= west or north <= south:
            raise ValueError(f"{dataset_id}: grid bounds are not increasing")
        if not math.isclose((east - west) / resolution, nx, abs_tol=1e-6):
            raise ValueError(f"{dataset_id}: longitude bounds/resolution do not yield grid_nx")
        if not math.isclose((north - south) / resolution, ny, abs_tol=1e-6):
            raise ValueError(f"{dataset_id}: latitude bounds/resolution do not yield grid_ny")

        if row["configuration_status"] in READY_STATUSES:
            interval = number(row, "IFRQ_INP")
            divisor = number(row, "DROFUNIT")
            if interval <= 0 or divisor <= 0:
                raise ValueError(f"{dataset_id}: IFRQ_INP and DROFUNIT must be positive")
            if not row["units"].strip():
                raise ValueError(f"{dataset_id}: configured product needs units")
        if row["record_interval_hours"].strip():
            record_interval = number(row, "record_interval_hours")
            if record_interval <= 0:
                raise ValueError(f"{dataset_id}: record_interval_hours must be positive")
            if row["IFRQ_INP"].strip() and not math.isclose(
                record_interval, number(row, "IFRQ_INP"), abs_tol=1e-9
            ):
                raise ValueError(f"{dataset_id}: IFRQ_INP does not match the record interval")
        if row["LINPDAY"] and row["LINPDAY"] not in {".TRUE.", ".FALSE."}:
            raise ValueError(f"{dataset_id}: LINPDAY must be .TRUE., .FALSE., or blank")

    return len(rows)


if __name__ == "__main__":
    try:
        count = validate()
    except (OSError, ValueError, csv.Error) as error:
        raise SystemExit(f"Invalid runoff dataset registry: {error}")
    print(f"Runoff dataset registry valid: {count} entries.")
