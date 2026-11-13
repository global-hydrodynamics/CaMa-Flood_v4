#!/usr/bin/env python3
"""Plot daily modeled outflw for Stung Treng and Prek Kdam."""

from __future__ import annotations

import argparse
import csv
from datetime import timedelta
from pathlib import Path

import matplotlib.dates as mdates
import matplotlib.pyplot as plt
import numpy as np
from netCDF4 import Dataset, num2date


STATION_ORDER = ("Stung Treng", "Prek Kdam")
VARIABLE = "outflw"
EXPECTED_UNITS = "m3/s"


def read_station_labels(path: Path) -> dict[str, str]:
    """Read display labels for the two requested stations from the location CSV."""
    labels = {}
    with path.open(newline="", encoding="utf-8") as stream:
        reader = csv.DictReader(stream)
        required = {"station_name", "plot_label"}
        missing = required - set(reader.fieldnames or ())
        if missing:
            raise ValueError(f"{path}: missing columns: {', '.join(sorted(missing))}")
        for row in reader:
            if row["station_name"] in STATION_ORDER:
                labels[row["station_name"]] = row["plot_label"]
    if set(labels) != set(STATION_ORDER):
        raise ValueError(f"{path}: expected {', '.join(STATION_ORDER)}")
    return labels


def read_station_series(path: Path, labels: dict[str, str]):
    """Read the extracted daily series and validate station names, dates, and units."""
    with Dataset(path, "r") as source:
        for name in ("time", "station_name", VARIABLE):
            if name not in source.variables:
                raise ValueError(f"{path}: missing variable {name!r}")
        time_var = source.variables["time"]
        flow_var = source.variables[VARIABLE]
        if flow_var.dimensions != ("time", "station"):
            raise ValueError(f"{path}: unexpected {VARIABLE} dimensions {flow_var.dimensions}")
        if getattr(flow_var, "units", "") != EXPECTED_UNITS:
            raise ValueError(f"{path}: expected {EXPECTED_UNITS}, found {getattr(flow_var, 'units', '')!r}")
        if getattr(flow_var, "cell_methods", "") != "time: mean":
            raise ValueError(f"{path}: expected daily-mean {VARIABLE}")

        names = [str(value) for value in source.variables["station_name"][:]]
        missing = [name for name in STATION_ORDER if name not in names]
        if missing:
            raise ValueError(f"{path}: missing station series: {', '.join(missing)}")
        timestamps = num2date(
            time_var[:], time_var.units,
            calendar=getattr(time_var, "calendar", "standard"),
            only_use_cftime_datetimes=False,
        )
        dates = [timestamp.date() for timestamp in timestamps]
        for previous, current in zip(dates, dates[1:]):
            if current - previous != timedelta(days=1):
                raise ValueError(f"{path}: time coordinate is not continuous daily data")
        station_flow = {}
        for station_name in STATION_ORDER:
            index = names.index(station_name)
            station_flow[station_name] = np.ma.asarray(flow_var[:, index], dtype=float)
    return dates, station_flow


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--stations", required=True, type=Path,
                        help="CSV containing selected station display labels")
    parser.add_argument("--sample", required=True, type=Path,
                        help="Extracted time-by-station daily outflw NetCDF")
    parser.add_argument("--output", required=True, type=Path, help="Output PNG path")
    parser.add_argument("--figure-width", type=float, default=8.0,
                        help="Figure width in inches")
    parser.add_argument("--figure-height", type=float, default=8.0,
                        help="Figure height in inches")
    parser.add_argument("--dpi", type=int, default=200)
    args = parser.parse_args()
    if args.figure_width <= 0 or args.figure_height <= 0 or args.dpi < 1:
        parser.error("figure dimensions and DPI must be positive")
    return args


def main() -> None:
    args = parse_args()
    labels = read_station_labels(args.stations)
    dates, flows = read_station_series(args.sample, labels)

    fig, axes = plt.subplots(2, 1, figsize=(args.figure_width, args.figure_height),
                             sharex=True)
    flow_color = "#1f5a94"
    for axis, station_name in zip(axes, STATION_ORDER):
        values = flows[station_name]
        axis.plot(dates, values, color=flow_color, linewidth=0.8)
        axis.axhline(0, color="#333333", linewidth=0.7, linestyle="--")
        axis.set_title(labels[station_name], loc="left", fontsize=11, pad=5)
        axis.set_ylabel("outflw (m³/s)")
        axis.grid(True, color="#b8b8b8", linewidth=0.45, alpha=0.65)
        axis.margins(x=0.01, y=0.08)
        valid = values.compressed()
        reverse_days = int(np.count_nonzero(valid < 0))
        print(
            f"{station_name}: {dates[0]} to {dates[-1]}, "
            f"min={valid.min():.3f}, max={valid.max():.3f} m3/s, "
            f"negative days={reverse_days}/{valid.size}"
        )

    axes[-1].set_xlabel("Date")
    axes[-1].xaxis.set_major_locator(mdates.MonthLocator(interval=3))
    axes[-1].xaxis.set_major_formatter(mdates.DateFormatter("%Y-%m"))
    fig.suptitle(
        "p02-f01b River Discharge: Hydrograph\n"
        "Negative values indicate flow opposite to the model-defined direction",
        fontsize=12, y=0.98,
    )
    fig.subplots_adjust(left=0.15, right=0.98, top=0.88, bottom=0.11, hspace=0.34)
    args.output.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(args.output, dpi=args.dpi, format="png")
    plt.close(fig)
    print(f"Wrote {args.output}")


if __name__ == "__main__":
    main()
