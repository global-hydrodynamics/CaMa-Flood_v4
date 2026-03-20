#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Extract time series from GrADS-style binary using external params.txt

Arguments:
  1. params.txt
  2. binary file path
  3. year
  4. x index (Fortran, 1-based)
  5. y index (Fortran, 1-based)

Options:
  --freq hourly|daily
  --daily-mean
"""

import sys
import argparse
import datetime as dt
import calendar
import struct
import math
from pathlib import Path

UNDEF = 1.0e20
LITTLE_ENDIAN = True
DTYPE = "f"
BYTES = 4


def parse_args():
    parser = argparse.ArgumentParser(description="Extract time series from binary")

    # ===== 順番変更ここ =====
    parser.add_argument("params_file", help="params.txt")
    parser.add_argument("binfile", help="binary file path")
    parser.add_argument("year", type=int, help="year")
    parser.add_argument("x", type=int, help="x index (Fortran, 1-based)")
    parser.add_argument("y", type=int, help="y index (Fortran, 1-based)")

    parser.add_argument("--freq", choices=["hourly", "daily"], default="hourly")
    parser.add_argument("--daily-mean", action="store_true")

    return parser.parse_args()


def read_params(fname):
    with open(fname) as f:
        lines = f.readlines()

    nx = int(lines[0].split()[0])
    ny = int(lines[1].split()[0])
    dx = float(lines[3].split()[0])
    west = float(lines[4].split()[0])
    east = float(lines[5].split()[0])
    south = float(lines[6].split()[0])
    north = float(lines[7].split()[0])

    return nx, ny, dx, west, east, south, north


def steps_in_year(year, freq):
    if freq == "hourly":
        return (366 if calendar.isleap(year) else 365) * 24
    else:
        return 366 if calendar.isleap(year) else 365


def x_to_lon(ix, west, dx):
    return west + (ix + 0.5) * dx


def y_to_lat(iy, north, dx):
    return north - (iy + 0.5) * dx


def read_value(f, ix, iy, t, nx, ny):
    offset = ((t * ny + iy) * nx + ix) * BYTES
    f.seek(offset)
    raw = f.read(BYTES)
    if len(raw) != BYTES:
        return None

    endian = "<" if LITTLE_ENDIAN else ">"
    return struct.unpack(endian + DTYPE, raw)[0]


def is_undef(val):
    return math.isclose(val, UNDEF, rel_tol=0.0,
                        abs_tol=1e-6 * max(1.0, abs(UNDEF)))


def output_hourly(f, year, ix, iy, nx, ny):
    start = dt.datetime(year, 1, 1)
    ntime = steps_in_year(year, "hourly")

    print("# time\tvalue")
    for t in range(ntime):
        time = start + dt.timedelta(hours=t)
        val = read_value(f, ix, iy, t, nx, ny)
        if val is None:
            break

        print(f"{time:%Y-%m-%dT%H:%M}\t{ 'UNDEF' if is_undef(val) else f'{val:.2f}' }")

def output_daily(f, year, ix, iy, nx, ny):
    start = dt.datetime(year, 1, 1)
    ntime = steps_in_year(year, "daily")

    print("# date\tvalue")
    for t in range(ntime):
        date = start + dt.timedelta(days=t)
        val = read_value(f, ix, iy, t, nx, ny)
        if val is None:
            break

        print(f"{time:%Y-%m-%dT%H:%M}\t{ 'UNDEF' if is_undef(val) else f'{val:.2f}' }")


def output_daily_mean(f, year, ix, iy, nx, ny):
    ndays = steps_in_year(year, "daily")
    print("# date\tdaily_mean")

    for d in range(ndays):
        date = dt.datetime(year, 1, 1) + dt.timedelta(days=d)

        vals = []
        missing = False

        for h in range(24):
            t = d * 24 + h
            val = read_value(f, ix, iy, t, nx, ny)
            if val is None:
                return
            if is_undef(val):
                missing = True
            else:
                vals.append(val)

        if missing or not vals:
            out = "UNDEF"
        else:
            out = sum(vals) / len(vals)

        print(f"{date:%Y-%m-%d}\t{out}")


def main():
    args = parse_args()

    nx, ny, dx, west, east, south, north = read_params(args.params_file)

    ix = args.x - 1
    iy = args.y - 1

    if not (0 <= ix < nx):
        sys.exit(f"x out of range: {args.x} (1-{nx})")

    if not (0 <= iy < ny):
        sys.exit(f"y out of range: {args.y} (1-{ny})")

    lon = x_to_lon(ix, west, dx)
    lat = y_to_lat(iy, north, dx)

    print(f"# params: {args.params_file}")
    print(f"# file: {args.binfile}")
    print(f"# year: {args.year}")
    print(f"# freq: {args.freq}")
    print(f"# x,y (Fortran): {args.x}, {args.y}")
    print(f"# lon,lat: {lon}, {lat}")

    if args.daily_mean:
        print("# output: daily mean")

    with open(args.binfile, "rb") as f:
        if args.freq == "hourly":
            if args.daily_mean:
                output_daily_mean(f, args.year, ix, iy, nx, ny)
            else:
                output_hourly(f, args.year, ix, iy, nx, ny)
        else:
            output_daily(f, args.year, ix, iy, nx, ny)


if __name__ == "__main__":
    main()