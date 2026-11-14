#!/usr/bin/env python3
"""Compare four experiment hydrographs at six Mekong model cells."""
import argparse
from datetime import date, datetime, timedelta
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.dates as mdates
import matplotlib.pyplot as plt
from netCDF4 import Dataset, num2date
import numpy as np
from station_csv import DEFAULT_STATION_CSV, read_station_records

# User settings: one continuous period and six one-based map-cell locations.
RUN_DIR = Path(__file__).resolve().parent / "out"
FIGURE_DIR = Path(__file__).resolve().parent / "figure"
ANALYSIS_FILE = Path(__file__).resolve().parent / "out" / "analysis" / "v03_station_discharge.nc"
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
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--stations", type=Path, default=DEFAULT_STATION_CSV,
                    help="CSV containing station names and one-based map x/y indices")
STATION_RECORDS = read_station_records(parser.parse_args().stations)
STATIONS = [
    (row["plot_label"], row["map_x_1based"], row["map_y_1based"])
    for row in STATION_RECORDS
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
    """Extract station discharge for the requested inclusive date range."""
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
                records[record_date.date()] = np.ma.asarray([
                    flow[index, iy - 1, ix - 1] for _, ix, iy in STATIONS
                ])

    target_dates = [date.fromordinal(day) for day in
                    range(START_DATE.toordinal(), END_DATE.toordinal() + 1)]
    missing_dates = [day for day in target_dates if day not in records]
    if missing_dates:
        raise ValueError(f"{experiment}: missing dates in requested period: {missing_dates[:3]}")
    station_values = np.ma.stack([records[day] for day in target_dates], axis=1)
    return target_dates, station_values, reference_coordinates


# Read the requested period from all experiments and verify shared date/grid coverage.
FIGURE_DIR.mkdir(parents=True, exist_ok=True)
series = {}
reference_dates = None
reference_coordinates = None
for experiment, label in EXPERIMENTS.items():
    dates, station_values, coordinates = read_period(experiment)
    if reference_dates is None:
        reference_dates = dates
        reference_coordinates = coordinates
    elif dates != reference_dates or not (
            np.array_equal(coordinates[0], reference_coordinates[0])
            and np.array_equal(coordinates[1], reference_coordinates[1])):
        raise ValueError(f"{experiment}: dates or output grid differ from the reference")
    series[experiment] = (label, station_values)

ANALYSIS_FILE.parent.mkdir(parents=True, exist_ok=True)
with Dataset(ANALYSIS_FILE, "w", format="NETCDF4") as analysis:
    analysis.createDimension("experiment", len(EXPERIMENTS))
    analysis.createDimension("station", len(STATIONS))
    analysis.createDimension("time", len(reference_dates))
    experiment_var = analysis.createVariable("experiment", str, ("experiment",))
    for index, name in enumerate(EXPERIMENTS):
        experiment_var[index] = name
    experiment_label_var = analysis.createVariable(
        "experiment_label", str, ("experiment",)
    )
    for index, label in enumerate(EXPERIMENTS.values()):
        experiment_label_var[index] = label
    station_var = analysis.createVariable("station_name", str, ("station",))
    plot_label_var = analysis.createVariable("station_plot_label", str, ("station",))
    map_x_var = analysis.createVariable("map_x", "i4", ("station",))
    map_y_var = analysis.createVariable("map_y", "i4", ("station",))
    map_x_var.long_name = "one-based Mekong map grid x index"
    map_y_var.long_name = "one-based Mekong map grid y index"
    map_x_var.index_origin = 1
    map_y_var.index_origin = 1
    longitude_var = analysis.createVariable("station_longitude", "f8", ("station",))
    latitude_var = analysis.createVariable("station_latitude", "f8", ("station",))
    area_var = analysis.createVariable(
        "station_upstream_drainage_area", "f8", ("station",)
    )
    longitude_var.units = "degrees_east"
    latitude_var.units = "degrees_north"
    area_var.units = "km2"
    area_var.long_name = "upstream drainage area from the Mekong 15-minute map"
    method_var = analysis.createVariable("cell_assignment_method", str, ("station",))
    for index, row in enumerate(STATION_RECORDS):
        station_var[index] = row["station_name"]
        plot_label_var[index] = row["plot_label"]
        map_x_var[index] = row["map_x_1based"]
        map_y_var[index] = row["map_y_1based"]
        longitude_var[index] = row["longitude_deg_e"]
        latitude_var[index] = row["latitude_deg_n"]
        area_var[index] = row["upstream_drainage_area_km2"]
        method_var[index] = row["cell_assignment_method"]
    time_var = analysis.createVariable("time", "i4", ("time",))
    time_var[:] = np.asarray([(value - START_DATE).days
                              for value in reference_dates], dtype=np.int32)
    time_var.units = f"days since {START_DATE.isoformat()} 00:00:00"
    time_var.calendar = "proleptic_gregorian"
    time_var.long_name = "represented daily interval date"
    flow_var = analysis.createVariable(
        "outflw", "f4", ("experiment", "station", "time"), fill_value=-9999.0
    )
    flow_var.units = "m3/s"
    flow_var.long_name = "daily mean discharge sampled at selected model cells"
    flow_var.cell_methods = "time: mean"
    for experiment_index, experiment in enumerate(EXPERIMENTS):
        flow_var[experiment_index] = series[experiment][1].filled(-9999.0).astype(np.float32)
    analysis.title = "Tutorial 02 Figure 3 station discharge analysis data"
    analysis.source_outputs = ", ".join(
        f"out/{experiment}/o_outflw{year}.nc"
        for experiment in EXPERIMENTS for year in YEARS
    )
    analysis.processing = (
        f"Daily mean outflw extracted for {START_DATE.isoformat()} through "
        f"{END_DATE.isoformat()} at the one-based map cells in station_map_locations.csv."
    )

# Read the persisted analysis data used by the plot.
with Dataset(ANALYSIS_FILE) as analysis:
    plotted_values = np.ma.asarray(analysis.variables["outflw"][:])
    reference_dates = [datetime.combine(START_DATE, datetime.min.time())
                       + timedelta(days=int(day))
                       for day in analysis.variables["time"][:]]

fig, axes = plt.subplots(3, 2, figsize=(15, 10), sharex=True)
for station_index, (ax, (station_name, _, _)) in enumerate(zip(axes.flat, STATIONS)):
    for experiment_index, (experiment, label) in enumerate(EXPERIMENTS.items()):
        values = plotted_values[experiment_index, station_index]
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
