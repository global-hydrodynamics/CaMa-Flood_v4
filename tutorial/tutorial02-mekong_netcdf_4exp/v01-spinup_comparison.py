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
YEARS = (2000, 2001, 2002, 2003)
STATIONS = [
    ("Vientiane", 51, 69),
    ("Ubon Ratchathani (Mun)", 60, 80),
    ("Stung Treng", 64, 86),
    ("Prek Kdam (Tonle Sap)", 60, 93),
    ("My Thuan (Tien)", 64, 99),
    ("Can Tho (Hau/Bassac)", 64, 100),
]
COLORS = {2000: "#1b9e77", 2001: "#d95f02", 2002: "#7570b3", 2003: "#e7298a"}


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


# Plot days on a non-leap 2001 calendar axis; omit February 29 from 2000 only.
FIGURE_DIR.mkdir(parents=True, exist_ok=True)
fig, axes = plt.subplots(len(STATIONS), 1, figsize=(13, 13), sharex=True)
for year in YEARS:
    flow, dates = read_daily_file(RUN_DIR / f"o_outflw{year}.nc")
    expected_records = 366 if year == 2000 else 365
    if len(dates) != expected_records:
        raise ValueError(f"{year}: expected {expected_records} daily records, got {len(dates)}")
    for ax, (name, ix, iy) in zip(axes, STATIONS):
        day_x = []
        discharge = []
        for date, value in zip(dates, flow[:, iy - 1, ix - 1]):
            if year == 2000 and date.month == 2 and date.day == 29:
                continue
            month_day = date.replace(year=2001)
            day_x.append(month_day)
            discharge.append(value)
        ax.plot(day_x, discharge, color=COLORS[year], linewidth=0.9,
                label=str(year), alpha=0.9)
        ax.axhline(0, color="black", linewidth=0.5)
        ax.grid(alpha=0.2)
        ax.set_ylabel(f"{name}\n(m³/s)")
axes[0].legend(ncol=4, title="Model year")
axes[-1].xaxis.set_major_locator(mdates.MonthLocator())
axes[-1].xaxis.set_major_formatter(mdates.DateFormatter("%b"))
axes[-1].set_xlim(datetime(2001, 1, 1), datetime(2002, 1, 1))
fig.suptitle("Daily mean discharge by calendar day (Default experiment)\n"
             "2000 Feb 29 omitted from overlay; interannual runoff also differs", y=1.005)
fig.tight_layout()
fig.savefig(FIGURE_DIR / "spinup_comparison.png", dpi=180, bbox_inches="tight")
plt.close(fig)
print(f"Wrote {FIGURE_DIR / 'spinup_comparison.png'}")
