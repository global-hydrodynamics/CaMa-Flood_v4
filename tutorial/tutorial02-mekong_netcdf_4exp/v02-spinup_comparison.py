#!/usr/bin/env python3
"""Compare annual seasonal discharge at Tutorial 02 model cells."""
from datetime import datetime, timedelta
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.dates as mdates
import matplotlib.pyplot as plt
from netCDF4 import Dataset, num2date
import numpy as np

# User settings: this figure uses only the Default experiment's daily NetCDF output.
RUN_DIR = Path(__file__).resolve().parent / "out" / "mek_default"
FIGURE_DIR = Path(__file__).resolve().parent / "figure"
ANALYSIS_FILE = Path(__file__).resolve().parent / "out" / "analysis" / "v02_spinup_station_discharge.nc"
YEARS = (2000, 2001, 2002, 2003)
STATIONS = [
    ("Vientiane", 51, 69),
    ("Ubon Ratchathani (Mun)", 60, 80),
    ("Stung Treng", 64, 86),
    ("Prek Kdam (Tonle Sap)", 60, 93),
    ("My Thuan (Tien)", 64, 99),
    ("Can Tho (Hau/Bassac)", 64, 100),
]
# Saturated colors distinguish years; the thicker 2000 line marks the spin-up year.
COLORS = {2000: "#1B6CA8", 2001: "#00A6A6", 2002: "#E69F00", 2003: "#C44536"}


def read_daily_file(path):
    """Return daily means indexed by the calendar day represented by each record."""
    with Dataset(path) as data:
        values = np.ma.masked_invalid(data.variables["outflw"][:])
        time = data.variables["time"]
        dates = num2date(time[:], time.units, getattr(time, "calendar", "standard"),
                         only_use_cftime_datetimes=False)
        dates = [date.replace(hour=0) - timedelta(days=1) for date in dates]
        if getattr(data.variables["outflw"], "units", "") != "m3/s":
            raise ValueError(f"{path}: outflw units are not m3/s")
        return values, dates


# Store the station series used by the figure on a common non-leap 2001 axis.
reference_dates = [datetime(2001, 1, 1) + timedelta(days=day) for day in range(365)]
discharge = np.ma.masked_all((len(YEARS), len(STATIONS), len(reference_dates)), dtype=float)
for year_index, year in enumerate(YEARS):
    flow, dates = read_daily_file(RUN_DIR / f"o_outflw{year}.nc")
    expected_records = 366 if year == 2000 else 365
    if len(dates) != expected_records:
        raise ValueError(f"{year}: expected {expected_records} daily records, got {len(dates)}")
    for station_index, (_, ix, iy) in enumerate(STATIONS):
        for date, value in zip(dates, flow[:, iy - 1, ix - 1]):
            if year == 2000 and date.month == 2 and date.day == 29:
                continue
            month_day = date.replace(year=2001)
            day_index = (month_day - reference_dates[0]).days
            discharge[year_index, station_index, day_index] = value

if np.any(np.ma.getmaskarray(discharge)):
    raise ValueError("One or more station/day values are missing from the analysis data")

ANALYSIS_FILE.parent.mkdir(parents=True, exist_ok=True)
with Dataset(ANALYSIS_FILE, "w", format="NETCDF4") as analysis:
    analysis.createDimension("year", len(YEARS))
    analysis.createDimension("station", len(STATIONS))
    analysis.createDimension("time", len(reference_dates))
    year_var = analysis.createVariable("year", "i2", ("year",))
    year_var[:] = YEARS
    station_var = analysis.createVariable("station_name", str, ("station",))
    for index, (name, _, _) in enumerate(STATIONS):
        station_var[index] = name
    map_x_var = analysis.createVariable("map_x", "i4", ("station",))
    map_y_var = analysis.createVariable("map_y", "i4", ("station",))
    map_x_var.long_name = "one-based Mekong map grid x index"
    map_y_var.long_name = "one-based Mekong map grid y index"
    map_x_var.index_origin = 1
    map_y_var.index_origin = 1
    map_x_var[:] = [ix for _, ix, _ in STATIONS]
    map_y_var[:] = [iy for _, _, iy in STATIONS]
    time_var = analysis.createVariable("time", "i2", ("time",))
    time_var[:] = np.arange(len(reference_dates))
    time_var.units = "days since 2001-01-01 00:00:00"
    time_var.calendar = "proleptic_gregorian"
    time_var.long_name = "reference calendar day; 2000 leap day omitted"
    flow_var = analysis.createVariable(
        "outflw", "f4", ("year", "station", "time"), fill_value=-9999.0
    )
    flow_var[:] = discharge.filled(-9999.0).astype(np.float32)
    flow_var.units = "m3/s"
    flow_var.long_name = "daily mean discharge at selected model cells"
    flow_var.cell_methods = "time: mean"
    analysis.title = "Tutorial 02 Figure 2 station discharge analysis data"
    analysis.source_outputs = ", ".join(f"out/mek_default/o_outflw{year}.nc" for year in YEARS)
    analysis.processing = (
        "Extracted one-based map cells; February 29, 2000 omitted and each date "
        "mapped to its month/day on the 2001 reference calendar."
    )

# Plot from the persisted analysis dataset so the figure's data are reproducible.
with Dataset(ANALYSIS_FILE) as analysis:
    discharge = np.ma.asarray(analysis.variables["outflw"][:])
    reference_dates = [datetime(2001, 1, 1) + timedelta(days=int(day))
                       for day in analysis.variables["time"][:]]

FIGURE_DIR.mkdir(parents=True, exist_ok=True)
fig, axes = plt.subplots(len(STATIONS), 1, figsize=(13, 13), sharex=True)
for year_index, year in enumerate(YEARS):
    for station_index, (ax, (name, _, _)) in enumerate(zip(axes, STATIONS)):
        is_spinup = year == 2000
        ax.plot(reference_dates, discharge[year_index, station_index], color=COLORS[year],
                linewidth=2.3 if is_spinup else 0.9,
                label="2000 (spin-up year)" if is_spinup else str(year),
                alpha=1.0 if is_spinup else 0.78,
                zorder=3 if is_spinup else 2)
        if year_index == 0:
            ax.axhline(0, color="black", linewidth=0.5)
            ax.grid(alpha=0.2)
            ax.set_ylabel(f"{name}\n(m³/s)")
axes[0].legend(ncol=4, title="Model year", frameon=False)
axes[-1].xaxis.set_major_locator(mdates.MonthLocator())
axes[-1].xaxis.set_major_formatter(mdates.DateFormatter("%b"))
axes[-1].set_xlim(datetime(2001, 1, 1), datetime(2002, 1, 1))
fig.suptitle("Daily mean discharge by calendar day (Default experiment)\n"
             "2000 Feb 29 omitted from overlay; interannual runoff also differs", y=1.005)
fig.tight_layout()
fig.savefig(FIGURE_DIR / "f02-spinup_comparison.png", dpi=180, bbox_inches="tight")
plt.close(fig)
print(f"Wrote {FIGURE_DIR / 'f02-spinup_comparison.png'}")
