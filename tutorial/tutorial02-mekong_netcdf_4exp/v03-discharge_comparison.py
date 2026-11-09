#!/usr/bin/env python3
"""Plot Tutorial 02 station locations and compare daily discharge experiments."""
from datetime import timedelta
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.collections import LineCollection
from netCDF4 import Dataset, num2date
import numpy as np

# User settings: update these paths only if the tutorial layout changes.
ROOT = Path(__file__).resolve().parents[2]
MAP_DIR = ROOT / "map" / "mekong_15min" / "netcdf"
RUN_DIR = Path(__file__).resolve().parent / "out"
FIGURE_DIR = Path(__file__).resolve().parent / "figure"
EXPERIMENTS = {
    "mek_default": "Default",
    "mek_nobif": "No Bifurcation",
    "mek_kine": "Kinematic Wave",
    "mek_nofld": "No Floodplain",
}
YEARS = (2001, 2002, 2003)
STATIONS = [
    {"name": "Vientiane", "river": "Mekong", "cell": (51, 69),
     "station": (102.613, 17.929), "area": 304814.28, "source": "GRDC 2469072"},
    {"name": "Ubon Ratchathani", "river": "Mun", "cell": (60, 80),
     "station": (104.863, 15.221), "area": 106544.10, "source": "GRDC 2969200"},
    {"name": "Stung Treng", "river": "Mekong", "cell": (64, 86),
     "station": (105.946, 13.537), "area": 637963.19, "source": "GRDC 2569005"},
    {"name": "Prek Kdam", "river": "Tonle Sap", "cell": (60, 93),
     "station": (104.804, 11.821), "area": 87707.56, "source": "GRDC 2569020"},
    {"name": "My Thuan", "river": "Tien", "cell": (64, 99),
     "station": (105.9, 10.273), "area": 1931.65,
     "source": "GRDC 2369005; cell from 1min catmxy"},
    {"name": "Can Tho", "river": "Hau/Bassac", "cell": (64, 100),
     "station": (105.79775, 10.0580), "area": 773786.94,
     "source": "MRC WQ station H039803"},
]


def read_daily_file(path):
    """Read daily grid discharge and return interval-start dates and coordinates."""
    with Dataset(path) as data:
        discharge = data.variables["outflw"][:]
        time = data.variables["time"]
        dates = num2date(time[:], time.units, getattr(time, "calendar", "standard"),
                         only_use_cftime_datetimes=False)
        # CaMa-Flood stamps daily means at interval end; map each to its represented day.
        dates = [date.replace(hour=0) - timedelta(days=1) for date in dates]
        lon = data.variables["lon"][:]
        lat = data.variables["lat"][:]
        units = getattr(data.variables["outflw"], "units", "")
        if units != "m3/s":
            raise ValueError(f"{path}: expected outflw units m3/s, found {units!r}")
        if discharge.ndim != 3 or discharge.shape[1:] != (len(lat), len(lon)):
            raise ValueError(f"{path}: unexpected outflw dimensions {discharge.shape}")
        return np.ma.masked_invalid(discharge), dates, lon, lat


def save_station_map():
    """Show the modeled river network, station references, and sampled map cells."""
    with Dataset(MAP_DIR / "nextxy_noedge.nc") as data:
        lon = data.variables["lon"][:]
        lat = data.variables["lat"][:]
        nextx = data.variables["nextx"][:]
        nexty = data.variables["nexty"][:]
    segments = []
    for row, col in zip(*np.where((nextx > 0) & (nexty > 0))):
        target_x, target_y = int(nextx[row, col]) - 1, int(nexty[row, col]) - 1
        if target_x < len(lon) and target_y < len(lat):
            segments.append(((lon[col], lat[row]), (lon[target_x], lat[target_y])))

    fig, ax = plt.subplots(figsize=(10, 9))
    ax.add_collection(LineCollection(segments, colors="#9aa7b1", linewidths=0.45, alpha=0.75))
    for station in STATIONS:
        ix, iy = station["cell"]
        grid_lon, grid_lat = lon[ix - 1], lat[iy - 1]
        ax.scatter(grid_lon, grid_lat, s=50, marker="o", edgecolor="black", zorder=4)
        ax.annotate(station["name"], (grid_lon, grid_lat), xytext=(5, 5),
                    textcoords="offset points", fontsize=8)
        # Geographic station coordinate is a reference only; it is not the sampled cell.
        ax.scatter(*station["station"], marker="x", color="tab:red", s=28, zorder=5)
    ax.set_xlim(99, 108.5)
    ax.set_ylim(8.5, 22)
    ax.set_xlabel("Longitude (degrees east)")
    ax.set_ylabel("Latitude (degrees north)")
    ax.set_title("Tutorial 02 sampling cells (circles) and station references (red crosses)")
    ax.grid(alpha=0.2)
    fig.tight_layout()
    fig.savefig(FIGURE_DIR / "f03a-station_locations.png", dpi=180)
    plt.close(fig)


# Read the four experiments and draw one row per station and one column per year.
FIGURE_DIR.mkdir(parents=True, exist_ok=True)
save_station_map()
fig, axes = plt.subplots(len(STATIONS), len(YEARS), figsize=(16, 15), sharex="col")
colors = {"mek_default": "#1b9e77", "mek_nobif": "#d95f02",
          "mek_kine": "#7570b3", "mek_nofld": "#e7298a"}
reference_coordinates = None
for experiment, label in EXPERIMENTS.items():
    for year in YEARS:
        path = RUN_DIR / experiment / f"o_outflw{year}.nc"
        flow, dates, lon, lat = read_daily_file(path)
        if len(dates) != 365:
            raise ValueError(f"{path}: expected 365 daily records, got {len(dates)}")
        if reference_coordinates is None:
            reference_coordinates = (lon, lat)
        elif not (np.array_equal(lon, reference_coordinates[0])
                  and np.array_equal(lat, reference_coordinates[1])):
            raise ValueError(f"{path}: output grid differs from the other experiments")
        for row, station in enumerate(STATIONS):
            ix, iy = station["cell"]
            values = flow[:, iy - 1, ix - 1]
            for col, target_year in enumerate(YEARS):
                if year == target_year:
                    axes[row, col].plot(dates, values, color=colors[experiment],
                                        linewidth=1.0, label=label)

for row, station in enumerate(STATIONS):
    for col, year in enumerate(YEARS):
        ax = axes[row, col]
        ax.axhline(0, color="black", linewidth=0.6)
        ax.grid(alpha=0.2)
        if row == 0:
            ax.set_title(str(year))
        if col == 0:
            ax.set_ylabel(f"{station['name']}\n{station['river']}\nDischarge (m³/s)")
        if row == len(STATIONS) - 1:
            ax.set_xlabel("Date")
        if row == 0 and col == len(YEARS) - 1:
            ax.legend(fontsize=8, loc="upper right")
fig.suptitle("Daily mean discharge at selected Mekong model cells (2001–2003)", y=1.002)
fig.autofmt_xdate(rotation=25)
fig.tight_layout()
fig.savefig(FIGURE_DIR / "f03b-discharge_comparison.png", dpi=180, bbox_inches="tight")
plt.close(fig)
print(f"Wrote {FIGURE_DIR / 'f03a-station_locations.png'}")
print(f"Wrote {FIGURE_DIR / 'f03b-discharge_comparison.png'}")
