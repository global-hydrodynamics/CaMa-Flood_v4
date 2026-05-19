#!/usr/bin/env python3
#==========================================================
# Download ERA5-Land runoff data from CDS as NetCDF.
#
# This script can download either:
#   - one daily file:   --file-unit daily   --day DD
#   - one monthly file: --file-unit monthly --start-day DD --end-day DD
#
# It is intended to be called by s11-download.sh.
#
# Daily example:
#   python download_era5land.py \
#       --file-unit daily \
#       --year 2000 \
#       --month 1 \
#       --day 1 \
#       --variable runoff \
#       --time-step 24 \
#       --area-nswe 90 -90 -180 180 \
#       --out ./ori_daily/ERA5-Land_runoff_20000101.nc
#
# Monthly example:
#   python download_era5land.py \
#       --file-unit monthly \
#       --year 2000 \
#       --month 1 \
#       --start-day 1 \
#       --end-day 31 \
#       --variable runoff \
#       --time-step 24 \
#       --area-nswe 90 -90 -180 180 \
#       --out ./ori_monthly/ERA5-Land_runoff_200001.nc
#
# Area convention:
# - --area-nswe is given as NORTH SOUTH WEST EAST.
# - By default, --area-nswe is interpreted as grid-center limits.
#   For a 0.1-degree grid:
#       N=23, S=22 -> CDS area north/south = 23.05 / 21.95
#   This includes latitude grid centers:
#       23.0, 22.9, ..., 22.0
# - Use --area-is edge to pass the specified values directly as CDS edges.
#
# CDS API area order is:
#   NORTH WEST SOUTH EAST
#==========================================================

from __future__ import annotations

import argparse
import calendar
import os
import sys
import time
from pathlib import Path
from typing import List

try:
    import cdsapi
except ImportError:
    cdsapi = None


DATASET = "reanalysis-era5-land"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Download daily or monthly ERA5-Land runoff data from CDS as NetCDF."
    )

    parser.add_argument(
        "--file-unit",
        choices=["daily", "monthly", "auto"],
        default="auto",
        help=(
            "Output file unit. "
            "'daily' requires --day. "
            "'monthly' requires --start-day and --end-day. "
            "'auto' infers from provided arguments. Default: auto."
        ),
    )

    parser.add_argument("--year", type=int, required=True, help="Target year, e.g. 2000")
    parser.add_argument("--month", type=int, required=True, help="Target month, 1-12")

    parser.add_argument(
        "--day",
        type=int,
        help="Target day for daily mode, 1-31.",
    )

    parser.add_argument(
        "--start-day",
        type=int,
        help="First day for monthly mode, inclusive.",
    )

    parser.add_argument(
        "--end-day",
        type=int,
        help="Last day for monthly mode, inclusive.",
    )

    parser.add_argument(
        "--variable",
        default="runoff",
        help="ERA5-Land variable name. Default: runoff",
    )

    parser.add_argument(
        "--time-step",
        type=int,
        default=1,
        help=(
            "Time interval in hours. "
            "Allowed values should divide 24 exactly, e.g. 1,2,3,4,6,8,12,24. "
            "Default: 1."
        ),
    )

    parser.add_argument(
        "--area-nswe",
        nargs=4,
        type=float,
        metavar=("NORTH", "SOUTH", "WEST", "EAST"),
        default=[90.0, -90.0, -180.0, 180.0],
        help=(
            "Download area in NORTH SOUTH WEST EAST order. "
            "By default, values are interpreted as grid-center limits."
        ),
    )

    parser.add_argument(
        "--area-is",
        choices=["center", "edge"],
        default="center",
        help=(
            "Interpretation of --area-nswe. "
            "'center': grid-center limits, expanded by half grid. "
            "'edge': CDS bounding edges, used directly. "
            "Default: center."
        ),
    )

    parser.add_argument(
        "--grid",
        type=float,
        default=0.1,
        help="Grid spacing in degrees used when --area-is center. Default: 0.1 for ERA5-Land.",
    )

    parser.add_argument(
        "--out",
        type=Path,
        required=True,
        help="Output NetCDF file path.",
    )

    parser.add_argument(
        "--overwrite",
        action="store_true",
        help="Overwrite output file if it already exists.",
    )

    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Print CDS request and exit without downloading.",
    )

    parser.add_argument(
        "--retries",
        type=int,
        default=3,
        help="Number of retry attempts if CDS request fails. Default: 3.",
    )

    parser.add_argument(
        "--sleep",
        type=int,
        default=60,
        help="Sleep seconds between retry attempts. Default: 60.",
    )

    return parser.parse_args()


def infer_file_unit(args: argparse.Namespace) -> str:
    if args.file_unit != "auto":
        return args.file_unit

    if args.day is not None and args.start_day is None and args.end_day is None:
        return "daily"

    if args.day is None and args.start_day is not None and args.end_day is not None:
        return "monthly"

    raise ValueError(
        "Could not infer --file-unit. "
        "Use --day for daily mode, or --start-day/--end-day for monthly mode."
    )


def validate_args(args: argparse.Namespace) -> str:
    if not (1 <= args.month <= 12):
        raise ValueError("--month must be between 1 and 12")

    if not (1 <= args.time_step <= 24):
        raise ValueError("--time-step must be between 1 and 24")

    if 24 % args.time_step != 0:
        raise ValueError("--time-step must divide 24 exactly")

    if args.grid <= 0:
        raise ValueError("--grid must be positive")

    file_unit = infer_file_unit(args)
    month_days = calendar.monthrange(args.year, args.month)[1]

    if file_unit == "daily":
        if args.day is None:
            raise ValueError("--day is required when --file-unit daily")
        if args.start_day is not None or args.end_day is not None:
            raise ValueError("Do not use --start-day/--end-day with --file-unit daily")
        if not (1 <= args.day <= month_days):
            raise ValueError(f"--day must be between 1 and {month_days} for this month")

    elif file_unit == "monthly":
        if args.start_day is None or args.end_day is None:
            raise ValueError("--start-day and --end-day are required when --file-unit monthly")
        if args.day is not None:
            raise ValueError("Do not use --day with --file-unit monthly")
        if not (1 <= args.start_day <= month_days):
            raise ValueError(f"--start-day must be between 1 and {month_days} for this month")
        if not (1 <= args.end_day <= month_days):
            raise ValueError(f"--end-day must be between 1 and {month_days} for this month")
        if args.end_day < args.start_day:
            raise ValueError("--end-day must be greater than or equal to --start-day")
    else:
        raise ValueError(f"Unsupported file unit: {file_unit}")

    north, south, west, east = args.area_nswe

    if north < south:
        raise ValueError("Invalid area: NORTH must be greater than or equal to SOUTH")

    if north > 90.0 or south < -90.0:
        raise ValueError("Invalid area center limits: NORTH/SOUTH must be within [-90, 90]")

    if west < -180.0 or east > 360.0:
        raise ValueError("Invalid area center limits: WEST/EAST should usually be within [-180, 360]")

    if args.retries < 1:
        raise ValueError("--retries must be at least 1")

    return file_unit


def make_times(time_step: int) -> List[str]:
    return [f"{hour:02d}:00" for hour in range(0, 24, time_step)]


def make_days(args: argparse.Namespace, file_unit: str) -> List[str]:
    if file_unit == "daily":
        assert args.day is not None
        return [f"{args.day:02d}"]

    assert args.start_day is not None
    assert args.end_day is not None
    return [f"{day:02d}" for day in range(args.start_day, args.end_day + 1)]


def center_limits_to_edges(
    north: float,
    south: float,
    west: float,
    east: float,
    grid: float,
) -> tuple[float, float, float, float]:
    """Convert center-coordinate limits to bounding box edges.

    Input/output order:
        input : north_center, south_center, west_center, east_center
        output: north_edge,   south_edge,   west_edge,   east_edge

    For a 0.1-degree grid:
        N=23, S=22 -> N_edge=23.05, S_edge=21.95
    """

    half = grid * 0.5

    north_edge = north + half
    south_edge = south - half
    west_edge = west - half
    east_edge = east + half

    # CDS area should not exceed the poles.
    # For CaMa-Flood mapping tables, 90.05/-90.05 is useful as grid-cell edges.
    # For CDS download requests, latitude outside [-90,90] should be avoided.
    north_edge = min(90.0, north_edge)
    south_edge = max(-90.0, south_edge)

    return north_edge, south_edge, west_edge, east_edge


def make_cds_area(args: argparse.Namespace) -> list[float]:
    north, south, west, east = args.area_nswe

    if args.area_is == "center":
        north, south, west, east = center_limits_to_edges(
            north=north,
            south=south,
            west=west,
            east=east,
            grid=args.grid,
        )

    # User-friendly order after possible conversion:
    #   N, S, W, E
    # CDS API order:
    #   N, W, S, E
    return [north, west, south, east]


def make_request(args: argparse.Namespace, file_unit: str) -> dict:
    yyyy = f"{args.year:04d}"
    mm = f"{args.month:02d}"

    request = {
        "variable": [args.variable],
        "year": yyyy,
        "month": mm,
        "day": make_days(args, file_unit),
        "time": make_times(args.time_step),
        "area": make_cds_area(args),
        "data_format": "netcdf",
        "download_format": "unarchived",
    }

    return request


def retrieve_with_retries(dataset: str, request: dict, target: Path, retries: int, sleep_sec: int) -> None:
    if cdsapi is None:
        raise RuntimeError("cdsapi is not installed. Install with: pip install cdsapi")

    client = cdsapi.Client()

    target.parent.mkdir(parents=True, exist_ok=True)
    tmp = target.with_suffix(target.suffix + ".part")

    if tmp.exists():
        tmp.unlink()

    last_error: Exception | None = None

    for attempt in range(1, retries + 1):
        try:
            print(f"[INFO] CDS retrieve attempt {attempt}/{retries}: {target}", flush=True)
            client.retrieve(dataset, request, str(tmp))

            if not tmp.exists():
                raise RuntimeError(f"Temporary file was not created: {tmp}")

            if tmp.stat().st_size == 0:
                raise RuntimeError(f"Downloaded file is empty: {tmp}")

            os.replace(tmp, target)
            print(f"[INFO] Download completed: {target}", flush=True)
            return

        except Exception as exc:
            last_error = exc
            print(f"[WARN] CDS retrieve failed on attempt {attempt}/{retries}: {exc}", file=sys.stderr, flush=True)

            if tmp.exists():
                try:
                    tmp.unlink()
                except OSError:
                    pass

            if attempt < retries:
                print(f"[INFO] Sleeping {sleep_sec} seconds before retry.", flush=True)
                time.sleep(sleep_sec)

    raise RuntimeError(f"CDS retrieve failed after {retries} attempts: {last_error}")


def main() -> int:
    args = parse_args()

    try:
        file_unit = validate_args(args)
        request = make_request(args, file_unit)

        print("[INFO] ERA5-Land NetCDF downloader")
        print(f"[INFO] Dataset      : {DATASET}")
        print(f"[INFO] File unit    : {file_unit}")
        print(f"[INFO] Year/month   : {args.year:04d}-{args.month:02d}")
        print(f"[INFO] Days         : {', '.join(request['day'])}")
        print(f"[INFO] Variable     : {args.variable}")
        print(f"[INFO] Times        : {', '.join(request['time'])}")
        print(f"[INFO] Area input   : {args.area_nswe}  # North, South, West, East; {args.area_is}")
        print(f"[INFO] Grid spacing : {args.grid}")
        print(f"[INFO] Area CDS     : {request['area']}  # North, West, South, East")
        print(f"[INFO] Output       : {args.out}")

        if args.out.exists() and not args.overwrite:
            print(f"[INFO] Output already exists. Skip: {args.out}")
            return 0

        if args.dry_run:
            print("[INFO] Dry run. CDS request:")
            print(request)
            return 0

        retrieve_with_retries(
            DATASET,
            request,
            args.out,
            retries=args.retries,
            sleep_sec=args.sleep,
        )

        return 0

    except Exception as exc:
        print(f"[ERROR] {exc}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
