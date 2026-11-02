#!/usr/bin/env python3
"""Area-weight 1-arcmin daily binary runoff records onto a 6-arcmin grid."""

import argparse
from datetime import date, timedelta
import math
from pathlib import Path
import sys

import numpy as np

UNDEF = -9999.0
SOURCE_NX = 1500
SOURCE_NY = 1320
SOURCE_RECORDS_PER_DAY = 24
SOURCE_DTYPE = np.dtype("<f4")
EARTH_RADIUS_M = 6_371_000.0


def parse_ctl(path):
    values = {}
    options = set()
    variable = ""
    in_variables = False
    for raw_line in path.read_text(encoding="utf-8").splitlines():
        line = raw_line.strip()
        if not line or line.startswith("*"):
            continue
        parts = line.split()
        key = parts[0].lower()
        if key == "endvars":
            in_variables = False
        elif in_variables:
            # GrADS variable definitions follow the `vars N` declaration.
            variable = " ".join(parts[4:])
        elif key == "vars":
            in_variables = True
        elif key == "options":
            options.update(value.lower() for value in parts[1:])
        elif key in {"dset", "undef", "xdef", "ydef", "tdef"}:
            values[key] = parts[1:]
    return values, options, variable


def dates_inclusive(start, end):
    current = date.fromisoformat(start)
    last = date.fromisoformat(end)
    if last < current:
        raise ValueError("end date must not precede start date")
    dates = []
    while current <= last:
        dates.append(current)
        current += timedelta(days=1)
    return dates


def parse_xdef(values, key):
    try:
        count, mode, first, step = values[key]
    except (KeyError, ValueError):
        raise ValueError(f"CTL {key} must use: {key} NX linear first_center spacing")
    if mode.lower() != "linear":
        raise ValueError(f"CTL {key} must be linear")
    return int(count), float(first), float(step)


def validate_source_ctl(ctl_path, west, east, south, north, source_resolution, expected_dates):
    values, options, _ = parse_ctl(ctl_path)
    nx, xfirst, dx = parse_xdef(values, "xdef")
    ny, yfirst, dy = parse_xdef(values, "ydef")
    tdef = values.get("tdef", [])
    undef = float(values.get("undef", ["nan"])[0])

    expected_nx = round((east - west) / source_resolution)
    expected_ny = round((north - south) / source_resolution)
    if (nx, ny) != (expected_nx, expected_ny):
        raise ValueError(f"source CTL grid is {nx}x{ny}; expected {expected_nx}x{expected_ny}")
    if not math.isclose(dx, source_resolution, rel_tol=0.0, abs_tol=1e-10):
        raise ValueError(f"source x spacing {dx} does not match {source_resolution}")
    if not math.isclose(dy, source_resolution, rel_tol=0.0, abs_tol=1e-10):
        raise ValueError(f"source y spacing {dy} does not match {source_resolution}")
    if not math.isclose(xfirst - dx / 2.0, west, abs_tol=1e-8):
        raise ValueError("source CTL west edge does not match requested domain")
    if not math.isclose(yfirst - dy / 2.0, south, abs_tol=1e-8):
        raise ValueError("source CTL south edge does not match requested domain")
    if "yrev" not in options or "little_endian" not in options:
        raise ValueError("source CTL must declare options yrev and little_endian")
    if len(tdef) != 4 or int(tdef[0]) != len(expected_dates) * SOURCE_RECORDS_PER_DAY:
        raise ValueError("source CTL tdef count does not match expected hourly coverage")
    if len(tdef) == 4 and (tdef[2].lower() != "00z03sep2007" or tdef[3].lower() != "1hr"):
        raise ValueError("source CTL time origin or interval does not match the expected hourly sequence")
    variable = " ".join(values.get("var", []))
    # The source package CTL is known to carry the maintainer-identified
    # erroneous mm/day label; output metadata uses the assessed mm/hour unit.
    if "mm/day" not in variable.lower():
        raise ValueError(f"source CTL does not carry the expected original mm/day label: {variable}")
    if undef != UNDEF:
        raise ValueError(f"source CTL undefined value is {undef}; expected {UNDEF}")
    dset = values.get("dset", [""])[0]
    if "runoff_%y4%m2%d2.bin" not in dset.lower():
        raise ValueError(f"unexpected source CTL dset pattern: {dset}")
    return nx, ny, xfirst, yfirst, dx, dy, values, options


def source_snapshot(files):
    return {p: (p.stat().st_size, p.stat().st_mtime_ns) for p in files}


def grid_cell_area_rows(ny, north, resolution):
    edges = north - np.arange(ny + 1, dtype=np.float64) * resolution
    north_rad = np.deg2rad(edges[:-1])
    south_rad = np.deg2rad(edges[1:])
    delta_lon = math.radians(resolution)
    return EARTH_RADIUS_M**2 * delta_lon * (np.sin(north_rad) - np.sin(south_rad))


def write_ctl(path, west, south, nx, ny, resolution, nrecords, unit_text):
    xfirst = west + resolution / 2.0
    yfirst = south + resolution / 2.0
    path.write_text(
        f"dset ^runoff_%y4%m2%d2.bin\n"
        f"undef {UNDEF:g}\n"
        "title Japan hourly runoff, area-weighted 6-arcmin sample\n"
        "options yrev little_endian template\n"
        f"xdef {nx} linear {xfirst:.10f} {resolution:.10f}\n"
        f"ydef {ny} linear {yfirst:.10f} {resolution:.10f}\n"
        "tdef 168 linear 00Z03sep2007 1hr\n"
        "zdef 1 linear 1 1\n"
        "vars 1\n"
        f"runoff 0 99 ** runoff [{unit_text}]\n"
        "ENDVARS\n",
        encoding="ascii",
    )


def validate_output_ctl(path, nx, ny, west, south, resolution, nrecords):
    values, options, variable = parse_ctl(path)
    ox, xfirst, dx = parse_xdef(values, "xdef")
    oy, yfirst, dy = parse_xdef(values, "ydef")
    tdef = values.get("tdef", [])
    if (ox, oy) != (nx, ny):
        raise ValueError("output CTL dimensions do not match aggregated data")
    if not math.isclose(dx, resolution, abs_tol=1e-10) or not math.isclose(dy, resolution, abs_tol=1e-10):
        raise ValueError("output CTL spacing does not match 6-arcmin grid")
    if not math.isclose(xfirst, west + resolution / 2.0, abs_tol=1e-8):
        raise ValueError("output CTL x origin is incorrect")
    if not math.isclose(yfirst, south + resolution / 2.0, abs_tol=1e-8):
        raise ValueError("output CTL y origin is incorrect")
    if "yrev" not in options or "little_endian" not in options:
        raise ValueError("output CTL must describe north-to-south little-endian data")
    if len(tdef) != 4 or int(tdef[0]) != nrecords or tdef[2].lower() != "00z03sep2007" or tdef[3].lower() != "1hr":
        raise ValueError("output CTL time axis does not match the 168 hourly records")
    if float(values.get("undef", ["nan"])[0]) != UNDEF:
        raise ValueError("output CTL undefined value is incorrect")
    if "mm/hour" not in variable.lower():
        raise ValueError("output CTL must declare the maintainer-identified mm/hour unit")


def aggregate_record(source_record, row_weights, undef):
    ny_out, nx_out = row_weights.shape[0], source_record.shape[1] // 6
    blocks = source_record.reshape(ny_out, 6, nx_out, 6)
    valid = blocks != undef
    complete = valid.all(axis=(1, 3))
    weights = row_weights[:, :, None, None]
    weighted_sum = np.where(valid, blocks, 0.0).astype(np.float64)
    weighted_sum *= weights
    weighted_sum = weighted_sum.sum(axis=(1, 3))
    denominator = (row_weights.sum(axis=1) * 6.0)[:, None]
    result = (weighted_sum / denominator).astype(np.float32)
    result[~complete] = np.float32(undef)
    return result, complete


def independent_block_mean(source_record, out_y, out_x, row_weights, undef):
    total = 0.0
    weight_total = 0.0
    for sub_y in range(6):
        weight = float(row_weights[out_y, sub_y])
        for sub_x in range(6):
            value = float(source_record[out_y * 6 + sub_y, out_x * 6 + sub_x])
            if value == undef:
                return undef
            total += value * weight
            weight_total += weight
    return total / weight_total


def run(args):
    source_dir = args.source_dir.resolve(strict=True)
    output_dir = args.output_dir.resolve()
    if not source_dir.is_dir():
        raise ValueError(f"source directory is not a directory: {source_dir}")
    if source_dir == output_dir or source_dir in output_dir.parents:
        raise ValueError("output directory must not be the source directory or inside it")

    source_res = float(args.source_resolution)
    target_res = float(args.target_resolution)
    if target_res <= source_res or not math.isclose(target_res / source_res, 6.0, abs_tol=1e-8):
        raise ValueError("this workflow requires 6 source cells per target cell")
    nx = round((args.east - args.west) / target_res)
    ny = round((args.north - args.south) / target_res)
    if (nx, ny) != (250, 220):
        raise ValueError(f"target grid is {nx}x{ny}; expected 250x220")
    dates = dates_inclusive(args.start, args.end)
    if len(dates) != 7 or args.hours_per_day != SOURCE_RECORDS_PER_DAY:
        raise ValueError("this sample expects seven days with 24 hourly records per day")

    ctl_path = source_dir / "runoff.ctl"
    src_nx, src_ny, _, _, _, _, _, _ = validate_source_ctl(
        ctl_path, args.west, args.east, args.south, args.north, source_res, dates
    )
    if (src_nx, src_ny) != (1500, 1320):
        raise ValueError(f"source grid is {src_nx}x{src_ny}; expected 1500x1320")

    expected_names = [f"runoff_{d:%Y%m%d}.bin" for d in dates]
    files = sorted(source_dir.glob("runoff_*.bin"))
    actual_names = [p.name for p in files]
    if actual_names != expected_names:
        raise ValueError(f"source date/file set differs from expected dates: {actual_names}")

    source_record_bytes = src_nx * src_ny * SOURCE_DTYPE.itemsize
    daily_bytes = source_record_bytes * SOURCE_RECORDS_PER_DAY
    for path in files:
        if path.stat().st_size != daily_bytes:
            raise ValueError(f"{path.name}: expected {daily_bytes} bytes, found {path.stat().st_size}")
    before = source_snapshot(files)

    row_weights = grid_cell_area_rows(src_ny, args.north, source_res).reshape(ny, 6)
    target_row_areas = grid_cell_area_rows(ny, args.north, target_res)
    output_dir.mkdir(parents=True, exist_ok=True)
    output_record_bytes = nx * ny * SOURCE_DTYPE.itemsize
    output_day_bytes = output_record_bytes * SOURCE_RECORDS_PER_DAY
    all_relative_errors = []
    sample_records = {}
    source_value_stats = {"missing": 0, "nan": 0, "inf": 0, "negative": 0}
    print(f"[INFO] Source CTL: {ctl_path}")
    print(f"[INFO] Source grid: {src_nx} x {src_ny}, little-endian float32, NtoS")
    print(f"[INFO] Target grid: {nx} x {ny}, 0.1 degree, NtoS")
    print(f"[INFO] Dates and records: {dates[0]} to {dates[-1]}, {len(dates) * 24} hourly")
    print(f"[INFO] Missing policy: a target cell is missing if any of its 36 source cells is missing")

    for day_index, (day, path) in enumerate(zip(dates, files), start=1):
        print(f"\n[STEP {day_index}/7] Reading {path.name}")
        raw = np.fromfile(path, dtype=SOURCE_DTYPE)
        if raw.size != src_nx * src_ny * SOURCE_RECORDS_PER_DAY:
            raise ValueError(f"{path.name}: unexpected number of float32 values")
        values = raw.reshape(SOURCE_RECORDS_PER_DAY, src_ny, src_nx)
        source_value_stats["missing"] += int(np.count_nonzero(values == UNDEF))
        source_value_stats["nan"] += int(np.count_nonzero(np.isnan(values)))
        source_value_stats["inf"] += int(np.count_nonzero(np.isinf(values)))
        source_value_stats["negative"] += int(np.count_nonzero((values < 0) & (values != UNDEF)))
        if source_value_stats["nan"] or source_value_stats["inf"]:
            raise ValueError(f"{path.name}: unexpected NaN or infinity in source")

        output = np.empty((SOURCE_RECORDS_PER_DAY, ny, nx), dtype=SOURCE_DTYPE)
        for hour in range(SOURCE_RECORDS_PER_DAY):
            target, complete = aggregate_record(values[hour], row_weights, UNDEF)
            output[hour] = target

            # Compare integrated runoff over complete target cells only. This
            # also gives a well-defined comparison if a source block is masked.
            source_blocks = values[hour].reshape(ny, 6, nx, 6)
            retained = complete[:, None, :, None]
            source_common = np.where(retained, source_blocks, 0.0).astype(np.float64)
            source_row_areas = grid_cell_area_rows(src_ny, args.north, source_res).reshape(ny, 6)
            source_area_weights = source_row_areas[:, :, None, None]
            source_integral = float((source_common * source_area_weights).sum())
            target_valid = complete
            target_integral = float((target[ target_valid ].astype(np.float64) *
                                     np.broadcast_to(target_row_areas[:, None], target.shape)[target_valid]).sum())
            denominator = max(abs(source_integral), 1.0)
            rel_error = abs(target_integral - source_integral) / denominator
            all_relative_errors.append(rel_error)
            if rel_error > 2.0e-7:
                raise ValueError(f"{day} hour {hour}: area-integrated runoff mismatch {rel_error:.3e}")

            if day_index == 1 and hour in {0, 12, 23}:
                sample_records[hour] = (values[hour].copy(), target.copy())

        output_path = output_dir / f"runoff_{day:%Y%m%d}.bin"
        temporary = output_path.with_suffix(output_path.suffix + ".tmp")
        output.tofile(temporary)
        temporary.replace(output_path)
        if output_path.stat().st_size != output_day_bytes:
            raise ValueError(f"{output_path.name}: output byte size is incorrect")
        print(f"[INFO] Wrote {output_path.name}: {output_path.stat().st_size} bytes, 24 records")
        del raw, values, output

    ctl_out = output_dir / "runoff.ctl"
    write_ctl(ctl_out, args.west, args.south, nx, ny, target_res,
              len(dates) * SOURCE_RECORDS_PER_DAY,
              "mm/hour")
    validate_output_ctl(ctl_out, nx, ny, args.west, args.south, target_res,
                        len(dates) * SOURCE_RECORDS_PER_DAY)

    # Independently recalculate selected target cells with explicit scalar
    # loops, separate from the vectorized aggregation path above.
    check_points = [(0, 0), (ny // 2, nx // 2), (ny - 1, nx - 1)]
    for hour, (source, target) in sample_records.items():
        for out_y, out_x in check_points:
            expected = independent_block_mean(source, out_y, out_x, row_weights, UNDEF)
            actual = float(target[out_y, out_x])
            if not math.isclose(actual, expected, rel_tol=2.0e-7, abs_tol=1.0e-6):
                raise ValueError(f"independent block check failed at hour={hour}, y={out_y}, x={out_x}")

    after = source_snapshot(files)
    if before != after:
        raise ValueError("source file size or modification time changed during processing")
    output_files = sorted(output_dir.glob("runoff_*.bin"))
    if [p.name for p in output_files] != expected_names:
        raise ValueError("output file names do not match the requested dates")
    if any(p.stat().st_size != output_day_bytes for p in output_files):
        raise ValueError("one or more output files have an unexpected size")
    if sum(p.stat().st_size for p in output_files) != output_day_bytes * len(dates):
        raise ValueError("aggregate output byte count is incorrect")

    print(f"\n[INFO] Source value checks: {source_value_stats}")
    print(f"[INFO] Maximum relative area-integral difference: {max(all_relative_errors):.3e}")
    print(f"[INFO] Mean relative area-integral difference: {sum(all_relative_errors) / len(all_relative_errors):.3e}")
    print(f"[INFO] Independently checked 9 block means across source records at hours 00, 12, and 23.")
    print(f"[INFO] Output CTL validated: {ctl_out}")
    print(f"[SUCCESS] Generated {len(output_files)} daily files; each has 24 hourly float32 records.")
    print(f"[SUCCESS] Raw binary total: {sum(p.stat().st_size for p in output_files):,} bytes.")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source-dir", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    parser.add_argument("--west", type=float, required=True)
    parser.add_argument("--east", type=float, required=True)
    parser.add_argument("--south", type=float, required=True)
    parser.add_argument("--north", type=float, required=True)
    parser.add_argument("--source-resolution", required=True)
    parser.add_argument("--target-resolution", required=True)
    parser.add_argument("--start", required=True)
    parser.add_argument("--end", required=True)
    parser.add_argument("--hours-per-day", type=int, default=24)
    args = parser.parse_args()
    try:
        run(args)
    except (OSError, ValueError, OverflowError) as error:
        print(f"[ERROR] {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
