#!/usr/bin/env python3
"""Plot one extracted CF-timestamped CaMa-Flood outflw map."""

from __future__ import annotations

import argparse
import csv
import json
from pathlib import Path
import sys

import matplotlib.pyplot as plt
import numpy as np
from matplotlib.colors import LinearSegmentedColormap, Normalize
from netCDF4 import Dataset, num2date
from importlib.resources import files

SCRIPT_DIR = Path(__file__).resolve().parent
RIVER_MAP_SCRIPT_DIR = SCRIPT_DIR.parents[1] / "river_map" / "script"
sys.path.insert(0, str(RIVER_MAP_SCRIPT_DIR))

from river_map_common import (  # noqa: E402
    add_aligned_colorbar,
    create_map_axes,
    draw_raster,
    read_geometry,
    save_panel,
)
from cmf_colormap import get_cmap  # noqa: E402

VARIABLE = "outflw"
EXPECTED_UNITS = "m3/s"


class AsymmetricSignedLogNorm(Normalize):
    """Map signed log ranges by their log spans around a linear zero band."""

    def __init__(self, vmin: float, vmax: float, linthresh: float,
                 linear_color_fraction: float) -> None:
        if not np.isfinite(linthresh) or linthresh <= 0:
            raise ValueError("LINTHRESH must be a finite positive number")
        if not (vmin < -linthresh < 0 < linthresh < vmax):
            raise ValueError(
                "CELV endpoints must extend beyond the linear interval: "
                "minimum < -LINTHRESH < 0 < LINTHRESH < maximum"
            )
        if (not np.isfinite(linear_color_fraction) or
                not 0 < linear_color_fraction < 1):
            raise ValueError("LINEAR_FRAC must be greater than 0 and less than 1")
        self.negative_log_span = float(np.log10(abs(vmin) / linthresh))
        self.positive_log_span = float(np.log10(vmax / linthresh))
        total_log_span = self.negative_log_span + self.positive_log_span
        self.linthresh = float(linthresh)
        self.linear_color_fraction = float(linear_color_fraction)
        logarithmic_fraction = 1 - linear_color_fraction
        self.negative_width = logarithmic_fraction * (
            self.negative_log_span / total_log_span
        )
        self.positive_width = logarithmic_fraction * (
            self.positive_log_span / total_log_span
        )
        self.linear_low = self.negative_width
        self.linear_high = self.negative_width + linear_color_fraction
        self.zero_position = self.negative_width + linear_color_fraction / 2
        super().__init__(vmin=vmin, vmax=vmax, clip=False)

    def __call__(self, value, clip=None):
        values, is_scalar = self.process_value(value)
        data = values.data.astype(np.float64)
        should_clip = self.clip if clip is None else clip
        if should_clip:
            data = np.clip(data, self.vmin, self.vmax)

        normalized = np.empty_like(data)
        negative = data < -self.linthresh
        central = (data >= -self.linthresh) & (data <= self.linthresh)
        positive = data > self.linthresh
        normalized[negative] = self.linear_low * (
            1 - np.log10(np.abs(data[negative]) / self.linthresh) /
            self.negative_log_span
        )
        normalized[central] = self.zero_position + (
            data[central] / self.linthresh
        ) * (self.linear_color_fraction / 2)
        normalized[positive] = self.linear_high + (
            np.log10(data[positive] / self.linthresh) /
            self.positive_log_span
        ) * self.positive_width

        mask = np.ma.getmaskarray(values) | ~np.isfinite(data)
        result = np.ma.array(normalized, mask=mask, copy=False)
        return result[0] if is_scalar else result

    def inverse(self, value):
        positions = np.asarray(value, dtype=np.float64)
        data = np.empty_like(positions)
        negative = positions < self.linear_low
        central = (positions >= self.linear_low) & (positions <= self.linear_high)
        positive = positions > self.linear_high
        negative_fraction = (self.linear_low - positions[negative]) / self.negative_width
        positive_fraction = (
            (positions[positive] - self.linear_high) / self.positive_width
        )
        data[negative] = -self.linthresh * 10 ** (
            negative_fraction * self.negative_log_span
        )
        data[central] = (
            (positions[central] - self.zero_position) * self.linthresh *
            (2 / self.linear_color_fraction)
        )
        data[positive] = self.linthresh * 10 ** (
            positive_fraction * self.positive_log_span
        )
        return data


def zero_centered_spectral_cmap(base_cmap, zero_position: float):
    """Reposition registered Spectral stops so data zero keeps the pale center."""
    definition_path = files("cmf_colormap").joinpath("data/diverge_spectral.json")
    definition = json.loads(definition_path.read_text(encoding="utf-8"))
    shifted_stops = []
    for control in definition["control_colors"]:
        old_position = float(control["position"])
        if old_position <= 0.5:
            new_position = old_position * zero_position / 0.5
        else:
            new_position = zero_position + (
                (old_position - 0.5) * (1 - zero_position) / 0.5
            )
        shifted_stops.append((new_position, base_cmap(old_position)))
    return LinearSegmentedColormap.from_list(
        "diverge_spectral_zero_centered", shifted_stops, N=base_cmap.N,
    )


def parse_color_levels(specification: str) -> np.ndarray:
    """Parse and validate comma-separated ascending signed-flow tick values."""
    try:
        levels = np.asarray(
            [float(item.strip()) for item in specification.split(",")],
            dtype=np.float64,
        )
    except ValueError as exc:
        raise ValueError("CELV must contain comma-separated numeric values") from exc
    if levels.size < 3 or not np.all(np.isfinite(levels)):
        raise ValueError("CELV must contain at least three finite numeric values")
    if np.any(np.diff(levels) <= 0):
        raise ValueError("CELV values must be strictly ascending without duplicates")
    if np.count_nonzero(levels == 0) != 1:
        raise ValueError("CELV must contain zero exactly once")
    if not np.any(levels < 0) or not np.any(levels > 0):
        raise ValueError("CELV must include at least one negative and one positive value")
    return levels


def format_color_level(value: float) -> str:
    """Format a colorbar value with grouped thousands and a typographic minus."""
    if value < 0:
        return f"−{abs(value):,g}"
    return f"{value:,g}"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--params", type=Path, required=True, help="Map params.txt")
    parser.add_argument("--sample", type=Path, required=True,
                        help="Extracted one-record NetCDF input")
    parser.add_argument("--stations", type=Path, required=True,
                        help="Observation-location CSV used to mark station coordinates")
    parser.add_argument("--output", type=Path, required=True, help="Output PNG path")
    parser.add_argument("--figure-width", type=float, default=0.0,
                        help="Complete figure width in inches; 0 is automatic")
    parser.add_argument("--figure-height", type=float, default=8.0,
                        help="Complete figure height in inches; 0 is automatic")
    parser.add_argument("--dpi", type=int, default=200)
    parser.add_argument("--celv", required=True,
                        help="Comma-separated ascending colorbar ticks; endpoints set range")
    parser.add_argument("--linthresh", type=float, required=True,
                        help="Positive half-width of the linear interval around zero")
    parser.add_argument("--linear-frac", type=float, required=True,
                        help="Colormap fraction reserved for the full linear interval")
    return parser.parse_args()


def _validate_grid(lon: np.ndarray, lat: np.ndarray, geometry) -> None:
    nx, ny, resolution, west, east, south, north = geometry
    expected_lon = west + (np.arange(nx) + 0.5) * resolution
    expected_lat = north - (np.arange(ny) + 0.5) * resolution
    if lon.shape != (nx,) or lat.shape != (ny,):
        raise ValueError(
            f"Coordinate lengths lon={lon.size}, lat={lat.size} do not match "
            f"params.txt grid {nx}x{ny}"
        )
    if not (np.all(np.diff(lon) > 0) and np.all(np.diff(lat) < 0)):
        raise ValueError("Expected longitude ascending eastward and latitude north-first")
    if not (np.allclose(lon, expected_lon, rtol=0, atol=1e-5) and
            np.allclose(lat, expected_lat, rtol=0, atol=1e-5)):
        raise ValueError("NetCDF cell centers do not match map/mekong_15min/params.txt")
    if (not np.isclose(east, west + nx * resolution) or
            not np.isclose(north, south + ny * resolution)):
        raise ValueError("params.txt dimensions, resolution, and bounds are inconsistent")


def read_display_stations(path: Path) -> list[dict[str, float | str]]:
    """Read the two station locations selected for p02-f01a from the shared CSV."""
    selected = {"Stung Treng", "Prek Kdam"}
    stations = []
    with path.open(newline="", encoding="utf-8") as stream:
        reader = csv.DictReader(stream)
        required = {"station_name", "plot_label", "longitude_deg_e", "latitude_deg_n"}
        missing = required - set(reader.fieldnames or ())
        if missing:
            raise ValueError(f"{path}: missing columns: {', '.join(sorted(missing))}")
        for row in reader:
            if row["station_name"] not in selected:
                continue
            stations.append({
                "station_name": row["station_name"],
                "plot_label": row["plot_label"],
                "longitude": float(row["longitude_deg_e"]),
                "latitude": float(row["latitude_deg_n"]),
            })
    names = {str(station["station_name"]) for station in stations}
    if names != selected:
        raise ValueError(f"{path}: expected station rows for {', '.join(sorted(selected))}")
    return stations


def draw_figure(sample_path: Path, params: Path, stations_path: Path, output: Path,
                figure_width: float, figure_height: float, dpi: int,
                color_levels: np.ndarray, linthresh: float,
                linear_fraction: float) -> None:
    geometry = read_geometry(params)
    nx, ny, _, west, east, south, north = geometry
    with Dataset(sample_path, "r") as sample:
        lon = np.asarray(sample.variables["lon"][:], dtype=np.float64)
        lat = np.asarray(sample.variables["lat"][:], dtype=np.float64)
        values = np.ma.asarray(sample.variables[VARIABLE][0, :, :], dtype=np.float64)
        fill_value = float(getattr(sample.variables[VARIABLE], "_FillValue", 1.0e20))
        cell_methods = getattr(sample.variables[VARIABLE], "cell_methods", "")
        units = getattr(sample.variables[VARIABLE], "units", "")
        time_var = sample.variables["time"]
        selected_time = num2date(
            time_var[:], time_var.units,
            calendar=getattr(time_var, "calendar", "standard"),
            only_use_cftime_datetimes=False,
        )[0]

    _validate_grid(lon, lat, geometry)
    stations = read_display_stations(stations_path)
    if values.shape != (ny, nx) or units != EXPECTED_UNITS or cell_methods != "time: mean":
        raise ValueError("Extracted sample metadata or grid shape is inconsistent")
    masked = np.ma.masked_where(
        np.ma.getmaskarray(values) | ~np.isfinite(np.asarray(values.filled(np.nan))) |
        (np.asarray(values.filled(fill_value)) == fill_value),
        values,
    )
    valid_values = np.asarray(masked.compressed(), dtype=np.float64)
    color_vmin = float(color_levels[0])
    color_vmax = float(color_levels[-1])
    below_range = valid_values < color_vmin
    above_range = valid_values > color_vmax
    under_values = valid_values[below_range]
    over_values = valid_values[above_range]

    norm = AsymmetricSignedLogNorm(
        vmin=color_vmin, vmax=color_vmax, linthresh=linthresh,
        linear_color_fraction=linear_fraction,
    )
    cmap = zero_centered_spectral_cmap(
        get_cmap("diverge_spectral"), norm.zero_position,
    )
    cmap.set_bad("white")
    title = f"p02-f01a River Discharge: Raster ({VARIABLE})\n{selected_time:%Y-%m-%d}"
    fig, ax = create_map_axes(geometry, west, east, south, north, title)
    ax.set_xlabel("Longitude")
    ax.set_ylabel("Latitude")
    image = draw_raster(ax, masked, geometry, cmap, norm)
    for station in stations:
        label_left_of_point = station["station_name"] == "Prek Kdam"
        ax.scatter(
            station["longitude"], station["latitude"], s=48,
            marker="o", facecolor="white", edgecolor="black",
            linewidth=1.1, zorder=5,
        )
        ax.annotate(
            station["plot_label"],
            (station["longitude"], station["latitude"]),
            xytext=(-6, -12) if label_left_of_point else (6, 6),
            textcoords="offset points",
            horizontalalignment="right" if label_left_of_point else "left",
            verticalalignment="top" if label_left_of_point else "bottom",
            fontsize=8,
            color="black", zorder=6,
            bbox={"facecolor": "white", "edgecolor": "none", "alpha": 0.72,
                  "pad": 1.2},
        )
    colorbar = add_aligned_colorbar(
        fig, ax, image, "Daily mean total outflow (m³/s)",
        orientation="vertical", extend="both",
    )
    colorbar.set_ticks(color_levels)
    colorbar.ax.set_yticklabels([format_color_level(value) for value in color_levels])
    save_panel(
        fig, output, dpi, figure_width=figure_width, figure_height=figure_height,
    )
    plt.close(fig)
    under_range = (
        f"{under_values.size} (range {under_values.min():.3f} to "
        f"{under_values.max():.3f} m³/s)" if under_values.size else "0"
    )
    over_range = (
        f"{over_values.size} (range {over_values.min():.3f} to "
        f"{over_values.max():.3f} m³/s)" if over_values.size else "0"
    )
    print(f"Wrote {output}; time={selected_time}; valid={valid_values.size}; "
          f"missing={nx * ny - valid_values.size}; min={valid_values.min():.3f}; "
          f"max={valid_values.max():.3f}; below {color_vmin:g}: {under_range}; "
          f"above {color_vmax:g}: {over_range}")


def main() -> None:
    args = parse_args()
    try:
        color_levels = parse_color_levels(args.celv)
        AsymmetricSignedLogNorm(
            vmin=float(color_levels[0]), vmax=float(color_levels[-1]),
            linthresh=args.linthresh,
            linear_color_fraction=args.linear_frac,
        )
    except ValueError as exc:
        raise SystemExit(f"Invalid color-scale settings: {exc}") from exc
    draw_figure(
        args.sample, args.params, args.stations, args.output,
        args.figure_width, args.figure_height, args.dpi,
        color_levels, args.linthresh, args.linear_frac,
    )


if __name__ == "__main__":
    main()
