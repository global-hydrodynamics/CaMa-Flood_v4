#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Merge gauge list metadata and time series into one CSV.

Usage:
  python x02-merge_gauge_csv.py data/list_points.txt gauge 2000 2001 -o out.csv
"""

from __future__ import annotations

import argparse
import csv
from pathlib import Path


def parse_args():
    parser = argparse.ArgumentParser(
        description="Merge gauge list and time series into one CSV."
    )
    parser.add_argument("list_file")
    parser.add_argument("gauge_dir")
    parser.add_argument("start_year", type=int)
    parser.add_argument("end_year", type=int)
    parser.add_argument("-o", "--output", required=True)
    return parser.parse_args()


def read_station_list(list_file: str):
    stations = []

    with open(list_file, "r", encoding="utf-8") as f:
        for line in f:
            line = line.strip()

            if not line or line.startswith("#"):
                continue

            parts = line.split()
            if len(parts) < 6:
                continue

            # ヘッダ行をスキップ（Name / NAME 両対応）
            if parts[0].lower() == "name":
                continue

            stations.append({
                "Name": parts[0],
                "IX_fort": parts[1],
                "IY_fort": parts[2],
                "Lat": parts[3],
                "Lon": parts[4],
                "Uparea": parts[5],
            })

    return stations


def read_timeseries(txt_file: Path):
    data = []

    with open(txt_file, "r", encoding="utf-8") as f:
        for line in f:
            line = line.strip()

            if not line or line.startswith("#"):
                continue

            parts = line.split()
            if len(parts) < 2:
                continue

            data.append((parts[0], parts[1]))

    return data


def main():
    args = parse_args()

    if args.start_year > args.end_year:
        raise ValueError("start_year must be <= end_year")

    years = list(range(args.start_year, args.end_year + 1))

    stations = read_station_list(args.list_file)
    gauge_dir = Path(args.gauge_dir)

    station_series = {}
    global_time_order = []

    for station in stations:
        name = station["Name"]
        merged = []

        for year in years:
            txt_file = gauge_dir / f"{name}_{year}.txt"

            if not txt_file.exists():
                raise FileNotFoundError(f"Missing file: {txt_file}")

            merged.extend(read_timeseries(txt_file))

        station_series[name] = dict(merged)

        for t, _ in merged:
            if t not in global_time_order:
                global_time_order.append(t)

    with open(args.output, "w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)

        writer.writerow(["Name"] + [s["Name"] for s in stations])
        writer.writerow(["IX_fort"] + [s["IX_fort"] for s in stations])
        writer.writerow(["IY_fort"] + [s["IY_fort"] for s in stations])
        writer.writerow(["Lat"] + [s["Lat"] for s in stations])
        writer.writerow(["Lon"] + [s["Lon"] for s in stations])
        writer.writerow(["Uparea"] + [s["Uparea"] for s in stations])

        for t in global_time_order:
            row = [t]
            for station in stations:
                row.append(station_series[station["Name"]].get(t, ""))
            writer.writerow(row)


if __name__ == "__main__":
    main()