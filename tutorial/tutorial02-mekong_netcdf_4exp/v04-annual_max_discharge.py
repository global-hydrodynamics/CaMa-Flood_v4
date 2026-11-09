#!/usr/bin/env python3
"""Map annual maximum daily-mean discharge for Tutorial 02 experiments."""
from datetime import timedelta
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.colors import LogNorm
from netCDF4 import Dataset, num2date
import numpy as np
try:
    from cmf_colormap import get_cmap
except ModuleNotFoundError as exc:
    if exc.name != "cmf_colormap":
        raise
    raise SystemExit(
        "CMF Colormaps is not available in this Python environment. From the "
        "repository root, run './s02-python_setup.sh', activate it with "
        "'source .venv/bin/activate', and rerun this script with that Python."
    ) from exc

# User settings: annual maximum is calculated from daily mean outflw records.
TUTORIAL_DIR = Path(__file__).resolve().parent
RUN_DIR = TUTORIAL_DIR / "out"
MAP_FILE = Path(__file__).resolve().parents[2] / "map" / "mekong_15min" / "netcdf" / "nextxy_noedge.nc"
FIGURE_DIR = TUTORIAL_DIR / "figure"
YEARS = (2001, 2002, 2003)
EXPERIMENTS = {
    "mek_default": "Default",
    "mek_nobif": "No Bifurcation",
    "mek_kine": "Kinematic Wave",
    "mek_nofld": "No Floodplain",
}


def read_annual_max(path, year):
    """Return the cellwise maximum for represented daily intervals in year."""
    with Dataset(path) as data:
        flow = np.ma.masked_invalid(data.variables["outflw"][:])
        time = data.variables["time"]
        dates = num2date(time[:], time.units, getattr(time, "calendar", "standard"),
                         only_use_cftime_datetimes=False)
        represented_dates = [date.replace(hour=0) - timedelta(days=1) for date in dates]
        selected = [index for index, date in enumerate(represented_dates)
                    if date.year == year]
        if len(selected) not in (365, 366):
            raise ValueError(f"{path}: expected 365/366 daily intervals for {year}, got {len(selected)}")
        if getattr(data.variables["outflw"], "units", "") != "m3/s":
            raise ValueError(f"{path}: outflw units are not m3/s")
        return np.ma.max(flow[selected, :, :], axis=0), data.variables["lon"][:], data.variables["lat"][:]


# Use one shared positive logarithmic scale; zero and negative values are not maxima of flow magnitude.
FIGURE_DIR.mkdir(parents=True, exist_ok=True)
with Dataset(MAP_FILE) as data:
    lon = data.variables["lon"][:]
    lat = data.variables["lat"][:]
    river = (data.variables["nextx"][:] > 0) & (data.variables["nexty"][:] > 0)

maps = {}
all_positive = []
for experiment in EXPERIMENTS:
    for year in YEARS:
        maximum, out_lon, out_lat = read_annual_max(
            RUN_DIR / experiment / f"o_outflw{year}.nc", year)
        if not np.array_equal(lon, out_lon) or not np.array_equal(lat, out_lat):
            raise ValueError(f"{experiment} {year}: output coordinates differ from the map")
        maximum = np.ma.masked_where(~river | (maximum <= 0), maximum)
        maps[(experiment, year)] = maximum
        all_positive.extend(maximum.compressed().tolist())
if not all_positive:
    raise ValueError("No positive river-cell discharge values were available for plotting")

vmin = 1.0
vmax = float(np.max(all_positive))
norm = LogNorm(vmin=vmin, vmax=vmax)
cmap = get_cmap("shade_rainbow")  # Optional CMF palette example; data and norm are unchanged.
fig, axes = plt.subplots(len(YEARS), len(EXPERIMENTS), figsize=(16, 11), sharex=True, sharey=True)
for col, (experiment, label) in enumerate(EXPERIMENTS.items()):
    for row, year in enumerate(YEARS):
        ax = axes[row, col]
        mesh = ax.pcolormesh(lon, lat, maps[(experiment, year)], shading="nearest",
                             cmap=cmap, norm=norm)
        ax.set_title(label if row == 0 else "")
        if col == 0:
            ax.set_ylabel(f"{year}\nLatitude (°N)")
        if row == len(YEARS) - 1:
            ax.set_xlabel("Longitude (°E)")
        ax.set_xlim(99, 108.5)
        ax.set_ylim(8.5, 22)
fig.colorbar(mesh, ax=axes.ravel().tolist(), label="Annual maximum daily-mean discharge (m³/s)",
             shrink=0.82, pad=0.02)
fig.suptitle("Mekong annual maximum daily-mean discharge (2001–2003)", y=0.99)
fig.savefig(FIGURE_DIR / "f04-annual_max_discharge.png", dpi=180, bbox_inches="tight")
plt.close(fig)
print(f"Shared logarithmic color range: {vmin:.3g}–{vmax:.3g} m3/s")
print(f"Wrote {FIGURE_DIR / 'f04-annual_max_discharge.png'}")
