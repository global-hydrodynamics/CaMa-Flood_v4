#!/usr/bin/env python3
"""Render six independent figures from saved model, analysis, and downscaler products."""

from __future__ import annotations

import csv
import os
from pathlib import Path
import tempfile

os.environ.setdefault("MPLCONFIGDIR", str(Path(tempfile.gettempdir()) / "cmf-matplotlib-cache"))

import matplotlib
matplotlib.use("Agg")
import matplotlib.dates as mdates
import matplotlib.pyplot as plt
from matplotlib.colors import TwoSlopeNorm
from matplotlib.patches import Rectangle
import numpy as np
import xarray as xr

ROOT = Path(__file__).resolve().parent
REPO = ROOT.parents[2]
OUT = ROOT / "out"
ANALYSIS = ROOT / "analysis"
FIGURE = ROOT / "figure"
FIGURE.mkdir(exist_ok=True)

try:
    from cmf_colormap import get_cmap
    FIELD_CMAP = get_cmap("shade_rainbow")
except ImportError:
    FIELD_CMAP = "viridis"
DIFF_CMAP = "coolwarm"
EXPERIMENT_COLORS = {"Control": "#2457A6", "Deep Channel": "#E87500"}


def load_event() -> dict[str, str]:
    with (ANALYSIS / "event_selection.csv").open(newline="") as stream:
        return next(csv.DictReader(stream))


def open_var(case: str, name: str) -> xr.DataArray:
    with xr.open_dataset(OUT / case / f"o_{name}2000.nc", decode_times=True, mask_and_scale=True) as ds:
        return ds[name].load()


def data_values(da: xr.DataArray) -> np.ndarray:
    values = np.asarray(da.values, dtype=np.float64)
    fill = da.attrs.get("_FillValue", da.encoding.get("_FillValue"))
    missing = da.attrs.get("missing_value")
    valid = np.isfinite(values)
    for sentinel in (fill, missing):
        if sentinel is not None:
            valid &= values != float(sentinel)
    return np.where(valid, values, np.nan)


def finish(fig: plt.Figure, name: str) -> None:
    fig.savefig(FIGURE / name, dpi=180, bbox_inches="tight")
    plt.close(fig)
    print(f"[SUCCESS] Wrote {FIGURE / name}")


def plot_map_change() -> None:
    src = xr.open_dataset(REPO / "map/mekong_15min/netcdf/rivhgt.nc", mask_and_scale=True)
    base = src["rivhgt"].load()
    deep = xr.open_dataset(ROOT / "map_modify/rivhgt_deep.nc", mask_and_scale=True)["rivhgt"].load()
    delta = data_values(deep) - data_values(base)
    vals = data_values(base)
    vmin, vmax = float(np.nanpercentile(vals, 1)), float(np.nanpercentile(vals, 99))
    fig, axes = plt.subplots(1, 3, figsize=(14, 4.6), constrained_layout=True)
    for ax, data, title in zip(axes[:2], (base, deep), ("Control: original rivhgt", "Deep Channel: +3 m")):
        m = ax.pcolormesh(base.lon, base.lat, data_values(data), shading="auto", cmap=FIELD_CMAP, vmin=vmin, vmax=vmax)
        ax.set_title(title); ax.set_xlabel("Longitude (°E)"); ax.set_ylabel("Latitude (°N)")
        fig.colorbar(m, ax=ax, label="River-channel depth (m)")
    m = axes[2].pcolormesh(base.lon, base.lat, delta, shading="auto", cmap=DIFF_CMAP, vmin=-3, vmax=3)
    axes[2].set_title("Deep Channel − Control"); axes[2].set_xlabel("Longitude (°E)"); axes[2].set_ylabel("Latitude (°N)")
    fig.colorbar(m, ax=axes[2], label="Change in river-channel depth (m)")
    fig.suptitle("Tutorial-local map modification (valid cells only)")
    finish(fig, "f01-rivhgt_map_change.png")


def plot_model_fields(event: dict[str, str]) -> None:
    stamp = np.datetime64(event["time_coordinate_interval_end"])
    fig, axes = plt.subplots(2, 2, figsize=(12, 9), constrained_layout=True)
    definitions = [
        ("outflw", "Interval-mean total discharge", "m³/s"),
        ("rivdph", "River-channel depth snapshot", "m"),
        ("sfcelv", "Water-surface elevation snapshot", "m"),
        ("fldfrc", "Flooded fraction snapshot", "fraction"),
    ]
    for ax, (name, title, unit) in zip(axes.flat, definitions):
        da = open_var("control", name)
        i = int(np.flatnonzero(da.time.values == stamp)[0])
        values = data_values(da.isel(time=i))
        if name == "fldfrc":
            vmin, vmax = 0.0, 1.0
        else:
            vmin, vmax = float(np.nanpercentile(values, 1)), float(np.nanpercentile(values, 99))
        m = ax.pcolormesh(da.lon, da.lat, values, shading="auto", cmap=FIELD_CMAP, vmin=vmin, vmax=vmax)
        ax.set_title(title); ax.set_xlabel("Longitude (°E)"); ax.set_ylabel("Latitude (°N)")
        fig.colorbar(m, ax=ax, label=unit)
    fig.suptitle(f"Control model fields — represented day {event['represented_day']} (interval ends {event['time_coordinate_interval_end']})")
    finish(fig, "f02-model_fields.png")


def plot_timeseries() -> None:
    with (ANALYSIS / "provisional_timeseries.csv").open(newline="") as stream:
        rows = list(csv.DictReader(stream))
    with (ANALYSIS / "provisional_sites.csv").open(newline="") as stream:
        site_rows = {row["site"]: row for row in csv.DictReader(stream)}
    sites = list(dict.fromkeys(row["site"] for row in rows))
    variables = [("outflw", "Discharge (m³/s)"), ("rivdph", "River depth (m)"), ("sfcelv", "Water-surface elevation (m)")]
    fig, axes = plt.subplots(len(sites), len(variables), figsize=(14, 10), sharex=True, constrained_layout=True)
    for r, site in enumerate(sites):
        subset = [row for row in rows if row["site"] == site]
        dates = np.array([np.datetime64(row["represented_day"]) for row in subset])
        for c, (variable, ylabel) in enumerate(variables):
            ax = axes[r, c]
            for experiment, label in (("control", "Control"), ("deep_channel", "Deep Channel")):
                values = np.array([float(row[f"{experiment}_{variable}"]) for row in subset])
                ax.plot(dates, values, lw=1.6 if label == "Control" else 1.2, color=EXPERIMENT_COLORS[label], label=label)
            if r == 0: ax.set_title(variable)
            if c == 0:
                selected = site_rows[site]
                coords = f"{float(selected['cell_lon']):.3f}°E, {float(selected['cell_lat']):.3f}°N"
                ax.set_ylabel(f"{site} ({coords})\n{ylabel}")
            ax.grid(alpha=0.25)
            if r == 0 and c == 0: ax.legend(frameon=False)
    for ax in axes[-1, :]:
        ax.set_xlabel("Month in 2000")
        ax.xaxis.set_major_locator(mdates.MonthLocator(interval=2))
        ax.xaxis.set_major_formatter(mdates.DateFormatter("%b"))
    fig.suptitle("Provisional nearest-catchment-cell comparisons — not observation validation")
    finish(fig, "f03-river_timeseries.png")


def plot_flooded_area(event: dict[str, str]) -> None:
    bbox = (104.0, 108.0, 8.0, 13.0)
    stamp = np.datetime64(event["time_coordinate_interval_end"])
    ctmare = xr.open_dataset(REPO / "map/mekong_15min/netcdf/ctmare.nc", mask_and_scale=True)["ctmare"].load()
    fig = plt.figure(figsize=(12, 11), constrained_layout=True)
    grid = fig.add_gridspec(3, 2, height_ratios=(1, 1, 0.8))
    axes = [fig.add_subplot(grid[0, 0]), fig.add_subplot(grid[0, 1]),
            fig.add_subplot(grid[1, 0]), fig.add_subplot(grid[1, 1])]
    for ax, case, metric in ((axes[0], "control", "fraction"), (axes[1], "deep_channel", "fraction"),
                             (axes[2], "control", "area"), (axes[3], "deep_channel", "area")):
        da = open_var(case, "fldfrc")
        i = int(np.flatnonzero(da.time.values == stamp)[0])
        frac = data_values(da.isel(time=i))
        if metric == "fraction":
            data, label, vmax = frac, "Flooded fraction", 1.0
        else:
            data, label, vmax = frac * data_values(ctmare) / 1.0e6, "Cell contribution (km²)", None
        if vmax is None: vmax = max(float(np.nanpercentile(data, 99)), 1e-5)
        m = ax.pcolormesh(da.lon, da.lat, data, shading="auto", cmap=FIELD_CMAP, vmin=0, vmax=vmax)
        ax.add_patch(Rectangle((bbox[0], bbox[2]), bbox[1]-bbox[0], bbox[3]-bbox[2], fill=False, ec="black", lw=1.2, ls="--"))
        ax.set_title(f"{case.replace('_',' ').title()} — {label}")
        ax.set_xlabel("Longitude (°E)"); ax.set_ylabel("Latitude (°N)")
        fig.colorbar(m, ax=ax, label=label)
    with (ANALYSIS / "provisional_flooded_area.csv").open(newline="") as stream:
        rows = list(csv.DictReader(stream))
    dates = [np.datetime64(row["represented_day"]) for row in rows]
    ax = fig.add_subplot(grid[2, :])
    for experiment, label in (("control", "Control"), ("deep_channel", "Deep Channel")):
        values = [float(row[f"{experiment}_flooded_area_km2"]) for row in rows]
        ax.plot(dates, values, label=label, color=EXPERIMENT_COLORS[label], lw=1.5)
    ax.set_ylabel("Flooded area (km²)"); ax.set_xlabel("Represented calendar day (2000)")
    ax.set_title("Provisional regional total (104–108°E, 8–13°N; cell-center selection)")
    ax.grid(alpha=0.25); ax.legend(frameon=False)
    finish(fig, "f04-flood_fraction_and_cell_area.png")


def plot_sensitivity(event: dict[str, str]) -> None:
    with xr.open_dataset(ANALYSIS / "difference_at_event.nc") as ds:
        fig, axes = plt.subplots(2, 2, figsize=(12, 9), constrained_layout=True)
        for ax, name, title in zip(axes.flat, ("outflw", "rivdph", "sfcelv", "fldfrc"),
                                   ("Interval-mean discharge", "River depth", "Water-surface elevation", "Flooded fraction")):
            data = np.asarray(ds[f"delta_{name}"].values, dtype=np.float64)
            vmax = float(np.nanpercentile(np.abs(data), 98))
            if not np.isfinite(vmax) or vmax == 0: vmax = 1.0
            m = ax.pcolormesh(ds.lon, ds.lat, data, shading="auto", cmap=DIFF_CMAP,
                              norm=TwoSlopeNorm(vmin=-vmax, vcenter=0.0, vmax=vmax))
            ax.set_title(f"{title} (Deep − Control)"); ax.set_xlabel("Longitude (°E)"); ax.set_ylabel("Latitude (°N)")
            unit = "m³/s" if name == "outflw" else ("fraction" if name == "fldfrc" else "m")
            fig.colorbar(m, ax=ax, label=unit + f" (display capped at |P98|={vmax:.3g})")
        fig.suptitle(f"Event sensitivity on represented day {event['represented_day']} — provisional point/date")
        finish(fig, "f05-event_sensitivity.png")


def plot_downscaling(event: dict[str, str]) -> None:
    bounds = (102.0, 108.0, 9.0, 15.0)
    shape = (360, 360)
    arrays = []
    for case in ("control", "deep_channel"):
        path = OUT / "downscaling" / case / "flddph_1min_provisional.bin"
        if not path.is_file():
            raise FileNotFoundError(f"Run s04-downscale_event.sh first: {path}")
        raw = np.fromfile(path, dtype="<f4")
        if raw.size != shape[0] * shape[1]:
            raise ValueError(f"{path} contains {raw.size} Float32 values; expected {shape}")
        arrays.append(raw.reshape(shape))
    valid_vals = np.concatenate([x[np.isfinite(x) & (x != -9999.0)] for x in arrays])
    vmax = float(np.nanpercentile(valid_vals, 98))
    downscale_cmap = plt.get_cmap(FIELD_CMAP).copy()
    downscale_cmap.set_bad("#f4f4f4")
    fig, axes = plt.subplots(1, 2, figsize=(11, 5), constrained_layout=True)
    for ax, values, case in zip(axes, arrays, ("Control", "Deep Channel")):
        masked = np.ma.masked_where(~np.isfinite(values) | (values == -9999.0), values)
        m = ax.imshow(masked, extent=bounds, origin="upper", cmap=downscale_cmap, vmin=0, vmax=vmax,
                      interpolation="nearest", aspect="auto")
        ax.set_title(case); ax.set_xlabel("Longitude (°E)"); ax.set_ylabel("Latitude (°N)")
        fig.colorbar(m, ax=ax, label="Downscaled inundation depth (m)")
    fig.suptitle(f"1-minute postprocessing — represented day {event['represented_day']} (102–108°E, 9–15°N)")
    finish(fig, "f06-one_minute_inundation.png")


def main() -> None:
    for required in (ANALYSIS / "event_selection.csv", ANALYSIS / "provisional_timeseries.csv",
                     ANALYSIS / "provisional_flooded_area.csv", ANALYSIS / "difference_at_event.nc"):
        if not required.is_file():
            raise FileNotFoundError(f"Run a01-analyze_outputs.py first: {required}")
    event = load_event()
    plot_map_change()
    plot_model_fields(event)
    plot_timeseries()
    plot_flooded_area(event)
    plot_sensitivity(event)
    plot_downscaling(event)


if __name__ == "__main__":
    main()
