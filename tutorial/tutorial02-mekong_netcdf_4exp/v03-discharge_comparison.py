#!/usr/bin/env python3
"""Compare four experiment hydrographs at six Mekong model cells."""
from datetime import date, datetime, timedelta
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.dates as mdates
import matplotlib.pyplot as plt
from netCDF4 import Dataset, num2date
import numpy as np

# User settings: one continuous period and six one-based map-cell locations.
RUN_DIR = Path(__file__).resolve().parent / "out"
FIGURE_DIR = Path(__file__).resolve().parent / "figure"
START_DATE = date(2001, 5, 1)
END_DATE = date(2002, 3, 31)
YEARS = (2001, 2002)
EXPERIMENTS = {
    "mek_default": "Default",
    "mek_nobif": "No Bifurcation",
    "mek_kine": "Kinematic Wave",
    "mek_nofld": "No Floodplain",
}
EXPERIMENT_COLORS = {
    "mek_default": "#0072B2",
    "mek_nobif": "#D55E00",
    "mek_kine": "#009E73",
    "mek_nofld": "#CC79A7",
}
STATIONS = [
    ("Vientiane", 51, 69),
    ("Ubon Ratchathani (Mun)", 60, 80),
    ("Stung Treng", 64, 86),
    ("Prek Kdam (Tonle Sap)", 60, 93),
    ("My Thuan (Tien)", 64, 99),
    ("Can Tho (Hau/Bassac)", 64, 100),
]


def read_daily_file(path):
    """Read daily grid discharge, represented dates, and grid coordinates."""
    with Dataset(path) as data:
        discharge = np.ma.masked_invalid(data.variables["outflw"][:])
        time = data.variables["time"]
        dates = num2date(time[:], time.units, getattr(time, "calendar", "standard"),
                         only_use_cftime_datetimes=False)
        # CaMa-Flood stamps daily means at interval end; each represents the prior day.
        dates = [date_value.replace(hour=0) - timedelta(days=1) for date_value in dates]
        lon = data.variables["lon"][:]
        lat = data.variables["lat"][:]
        units = getattr(data.variables["outflw"], "units", "")
        if units != "m3/s":
            raise ValueError(f"{path}: expected outflw units m3/s, found {units!r}")
        if discharge.ndim != 3 or discharge.shape[1:] != (len(lat), len(lon)):
            raise ValueError(f"{path}: unexpected outflw dimensions {discharge.shape}")
        return discharge, dates, lon, lat


def read_period(experiment):
    """Combine annual files for the requested inclusive date range."""
    records = {}
    reference_coordinates = None
    for year in YEARS:
        path = RUN_DIR / experiment / f"o_outflw{year}.nc"
        flow, dates, lon, lat = read_daily_file(path)
        expected_records = 366 if year % 4 == 0 else 365
        if len(dates) != expected_records:
            raise ValueError(f"{path}: expected {expected_records} daily records, got {len(dates)}")
        if reference_coordinates is None:
            reference_coordinates = (lon, lat)
        elif not (np.array_equal(lon, reference_coordinates[0])
                  and np.array_equal(lat, reference_coordinates[1])):
            raise ValueError(f"{path}: output grid differs from the other annual files")
        for index, record_date in enumerate(dates):
            if START_DATE <= record_date.date() <= END_DATE:
                records[record_date.date()] = flow[index]

    target_dates = [date.fromordinal(day) for day in
                    range(START_DATE.toordinal(), END_DATE.toordinal() + 1)]
    missing_dates = [day for day in target_dates if day not in records]
    if missing_dates:
        raise ValueError(f"{experiment}: missing dates in requested period: {missing_dates[:3]}")
    return target_dates, [records[day] for day in target_dates], reference_coordinates


# Read the requested period from all experiments and verify shared date/grid coverage.
FIGURE_DIR.mkdir(parents=True, exist_ok=True)
series = {}
reference_dates = None
reference_coordinates = None
for experiment, label in EXPERIMENTS.items():
    dates, grids, coordinates = read_period(experiment)
    if reference_dates is None:
        reference_dates = dates
        reference_coordinates = coordinates
    elif dates != reference_dates or not (
            np.array_equal(coordinates[0], reference_coordinates[0])
            and np.array_equal(coordinates[1], reference_coordinates[1])):
        raise ValueError(f"{experiment}: dates or output grid differ from the reference")
    series[experiment] = (label, grids)

fig, axes = plt.subplots(3, 2, figsize=(15, 10), sharex=True)
for ax, (station_name, ix, iy) in zip(axes.flat, STATIONS):
    for experiment, (label, grids) in series.items():
        values = np.ma.stack([grid[iy - 1, ix - 1] for grid in grids])
        is_default = experiment == "mek_default"
        ax.plot(reference_dates, values, color=EXPERIMENT_COLORS[experiment],
                linewidth=2.4 if is_default else 1.05,
                alpha=1.0 if is_default else 0.88,
                zorder=4 if is_default else 2, label=label)
    ax.axhline(0, color="black", linewidth=0.6, zorder=1)
    ax.grid(alpha=0.2)
    ax.set_title(station_name, loc="left")
    ax.set_ylabel("Discharge (m³/s)")

for ax in axes[-1, :]:
    ax.set_xlabel("Date")
    ax.xaxis.set_major_locator(mdates.MonthLocator())
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%b\n%Y"))
axes[0, 0].set_xlim(datetime.combine(START_DATE, datetime.min.time()),
                    datetime.combine(END_DATE, datetime.min.time()))
fig.suptitle("Daily mean discharge at six Mekong model cells\n"
             "1 May 2001 – 31 March 2002", y=0.99)
handles, labels = axes[0, 0].get_legend_handles_labels()
fig.legend(handles, labels, loc="upper center", bbox_to_anchor=(0.5, 0.935),
           ncol=4, frameon=False)
fig.tight_layout(rect=(0, 0, 1, 0.90))
output_path = FIGURE_DIR / "f03-discharge_comparison.png"
fig.savefig(output_path, dpi=180, bbox_inches="tight")
plt.close(fig)
print(f"Wrote {output_path}")
