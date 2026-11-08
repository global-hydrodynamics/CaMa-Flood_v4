#!/usr/bin/env python3
"""Convert the tracked Mekong daily float32 runoff sample to yearly NetCDF files.

The conversion uses ncgen, nccopy, and ncdump from the NetCDF command-line
utilities. Output is NetCDF-4 with lossless zlib level 3 and shuffle; runoff
values remain float32 and coordinates and CF time bounds are preserved.
"""
from __future__ import annotations

import argparse
import calendar
import math
import re
import shutil
import struct
import subprocess
import sys
import tempfile
from datetime import date, timedelta
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
SOURCE_DIR = REPO_ROOT / "inp" / "test-mek_1deg" / "runoff"
OUTPUT_DIR = Path(__file__).resolve().parent
NX = 20
NY = 30
WEST = 90.0
NORTH = 35.0
RESOLUTION = 1.0
MISSING_VALUE = -999.0
RECORD_BYTES = NX * NY * 4


def cdl_float(value: float) -> str:
    """Format a value with enough digits to recover its original float32."""
    text = f"{value:.9g}"
    return text if "." in text or "e" in text.lower() else text + ".0"


def read_year(year: int, source_dir: Path) -> tuple[list[bytes], list[str]]:
    records: list[bytes] = []
    cdl_values: list[str] = []
    day = date(year, 1, 1)
    end = date(year + 1, 1, 1)
    while day < end:
        path = source_dir / f"Roff____{day:%Y%m%d}.one"
        if not path.is_file():
            raise ValueError(f"missing source runoff file: {path}")
        raw = path.read_bytes()
        if len(raw) != RECORD_BYTES:
            raise ValueError(f"{path}: expected {RECORD_BYTES} bytes, got {len(raw)}")
        values = struct.unpack(f"<{NX * NY}f", raw)
        if not all(math.isfinite(value) for value in values):
            raise ValueError(f"{path}: contains non-finite values")
        records.append(raw)
        cdl_values.extend(cdl_float(value) for value in values)
        day += timedelta(days=1)
    return records, cdl_values


def make_cdl(year: int, values: list[str]) -> str:
    days = 366 if calendar.isleap(year) else 365
    lon = [WEST + RESOLUTION * (index + 0.5) for index in range(NX)]
    lat = [NORTH - RESOLUTION * (index + 0.5) for index in range(NY)]
    lines = [
        f"netcdf runoff{year} {{",
        "dimensions:",
        f"    time = {days} ;",
        "    bounds = 2 ;",
        f"    latitude = {NY} ;",
        f"    longitude = {NX} ;",
        "variables:",
        "    double time(time) ;",
        '        time:standard_name = "time" ;',
        '        time:long_name = "time at the center of each daily runoff interval" ;',
        f'        time:units = "days since {year}-01-01 12:00:00" ;',
        '        time:calendar = "proleptic_gregorian" ;',
        '        time:bounds = "time_bounds" ;',
        "    double time_bounds(time, bounds) ;",
        '        time_bounds:long_name = "daily runoff interval boundaries" ;',
        "    float latitude(latitude) ;",
        '        latitude:standard_name = "latitude" ;',
        '        latitude:long_name = "latitude" ;',
        '        latitude:units = "degrees_north" ;',
        '        latitude:axis = "Y" ;',
        "    float longitude(longitude) ;",
        '        longitude:standard_name = "longitude" ;',
        '        longitude:long_name = "longitude" ;',
        '        longitude:units = "degrees_east" ;',
        '        longitude:axis = "X" ;',
        "    float runoff(time, latitude, longitude) ;",
        '        runoff:long_name = "Daily surface runoff depth" ;',
        '        runoff:units = "mm day-1" ;',
        "        float runoff:missing_value = -999.0 ;",
        '        runoff:cell_methods = "time: mean" ;',
        "    :title = \"Mekong 1-degree daily runoff converted from test-mek_1deg binary\" ;",
        "    :Conventions = \"CF-1.8\" ;",
        "    :source_binary_format = \"little-endian float32, longitude varies fastest, north-to-south rows\" ;",
        "data:",
        "    time = " + ", ".join(str(index) for index in range(days)) + " ;",
    ]
    bounds = []
    for index in range(days):
        bounds.extend((f"{index - 0.5:g}", f"{index + 0.5:g}"))
    lines.append("    time_bounds = " + ", ".join(bounds) + " ;")
    lines.append("    latitude = " + ", ".join(f"{value:g}" for value in lat) + " ;")
    lines.append("    longitude = " + ", ".join(f"{value:g}" for value in lon) + " ;")
    lines.append("    runoff =")
    for offset in range(0, len(values), NX):
        suffix = "," if offset + NX < len(values) else " ;"
        lines.append("        " + ", ".join(values[offset : offset + NX]) + suffix)
    lines.append("}")
    return "\n".join(lines) + "\n"


def verify_netcdf(path: Path, expected: bytes, expected_records: int) -> None:
    ncdump = shutil.which("ncdump")
    if not ncdump:
        raise RuntimeError("ncdump is required to verify generated NetCDF values")
    result = subprocess.run(
        [ncdump, "-p", "9,17", "-v", "runoff", str(path)],
        check=True,
        text=True,
        capture_output=True,
    )
    match = re.search(r"\brunoff\s*=\s*(.*?);", result.stdout, flags=re.DOTALL)
    if not match:
        raise RuntimeError(f"could not parse runoff values from ncdump output: {path}")
    tokens = [token.strip() for token in match.group(1).replace("\n", " ").split(",") if token.strip()]
    if len(tokens) != expected_records * NX * NY:
        raise RuntimeError(
            f"{path}: expected {expected_records * NX * NY} runoff values, found {len(tokens)}"
        )
    try:
        actual = struct.pack(f"<{len(tokens)}f", *(float(token) for token in tokens))
    except ValueError as exc:
        raise RuntimeError(f"could not parse numeric runoff values from {path}: {exc}") from exc
    if actual != expected:
        raise RuntimeError(f"NetCDF runoff values do not match the source float32 records: {path}")


def convert_year(year: int, source_dir: Path, output_dir: Path, overwrite: bool) -> Path:
    ncgen = shutil.which("ncgen")
    nccopy = shutil.which("nccopy")
    if not ncgen or not nccopy:
        raise RuntimeError("ncgen and nccopy are required; install the NetCDF command-line utilities")
    output_path = output_dir / f"runoff{year}.nc"
    if output_path.exists() and not overwrite:
        raise FileExistsError(f"refusing to overwrite existing file (use --overwrite): {output_path}")

    raw_records, cdl_values = read_year(year, source_dir)
    cdl_text = make_cdl(year, cdl_values)
    output_dir.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix=f"runoff{year}-", dir=output_dir) as temp_name:
        temp_dir = Path(temp_name)
        cdl_path = temp_dir / f"runoff{year}.cdl"
        temp_nc = temp_dir / f"runoff{year}.nc"
        compressed_nc = temp_dir / f"runoff{year}.compressed.nc"
        cdl_path.write_text(cdl_text, encoding="ascii")
        subprocess.run([ncgen, "-k", "netCDF-4", "-o", str(temp_nc), str(cdl_path)], check=True)
        subprocess.run([nccopy, "-d", "3", "-s", str(temp_nc), str(compressed_nc)], check=True)
        verify_netcdf(compressed_nc, b"".join(raw_records), len(raw_records))
        compressed_nc.replace(output_path)
    return output_path


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--start-year", type=int, default=2000)
    parser.add_argument("--end-year", type=int, default=2001, help="inclusive; may be extended through 2009")
    parser.add_argument("--source-dir", type=Path, default=SOURCE_DIR)
    parser.add_argument("--output-dir", type=Path, default=OUTPUT_DIR)
    parser.add_argument("--overwrite", action="store_true", help="replace existing yearly NetCDF files")
    args = parser.parse_args()
    if args.start_year < 1 or args.end_year < args.start_year or args.end_year > 2009:
        parser.error("year range must be increasing and end no later than 2009")
    try:
        for year in range(args.start_year, args.end_year + 1):
            path = convert_year(year, args.source_dir, args.output_dir, args.overwrite)
            count = 366 if calendar.isleap(year) else 365
            print(f"[SUCCESS] {path}: {count} daily records; values verified against binary input")
    except (FileExistsError, OSError, ValueError, RuntimeError, subprocess.CalledProcessError) as exc:
        print(f"ERROR: {exc}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
