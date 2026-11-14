#!/usr/bin/env python3
"""Compare registered diverging palettes on the p02-f01 outflw sample."""

from __future__ import annotations

import argparse
import importlib.util
import json
from pathlib import Path
import sys

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.colors import LinearSegmentedColormap
from matplotlib.cm import ScalarMappable
from netCDF4 import Dataset, num2date
from importlib.resources import files

REPO_ROOT = Path(__file__).resolve().parents[3]
OUTPUT_ANALYSIS_DIR = REPO_ROOT / "visualization" / "output_analysis"
RIVER_MAP_SCRIPT_DIR = REPO_ROOT / "visualization" / "river_map" / "script"
sys.path.insert(0, str(RIVER_MAP_SCRIPT_DIR))

from river_map_common import read_geometry  # noqa: E402
from cmf_colormap import get_cmap  # noqa: E402

P02_SCRIPT = OUTPUT_ANALYSIS_DIR / "script" / "f01a-discharge_raster-map.py"
P02_SPEC = importlib.util.spec_from_file_location("p02_f01_outflow", P02_SCRIPT)
P02_MODULE = importlib.util.module_from_spec(P02_SPEC)
P02_SPEC.loader.exec_module(P02_MODULE)

DEFAULT_SAMPLE = OUTPUT_ANALYSIS_DIR / "out_sample" / "outflw_2001-09-01.nc"
DEFAULT_PARAMS = REPO_ROOT / "map" / "mekong_15min" / "params.txt"
DEFAULT_OUTPUT = Path(__file__).resolve().parent / "diverge_outflow_comparison.png"
PALETTE_NAMES = ("diverge_spectral", "diverge_asymmetry")


def recenter_palette(name: str, zero_position: float):
    """Move a palette's stored center to the shared p02-f01 zero position."""
    cmap = get_cmap(name)
    definition = json.loads(
        files("cmf_colormap").joinpath(f"data/{name}.json").read_text(encoding="utf-8")
    )
    center = float(definition["center_position"])
    shifted_stops = []
    for control in definition["control_colors"]:
        old_position = float(control["position"])
        if old_position <= center:
            new_position = old_position * zero_position / center
        else:
            new_position = zero_position + (
                (old_position - center) * (1 - zero_position) / (1 - center)
            )
        shifted_stops.append((new_position, cmap(old_position)))
    return LinearSegmentedColormap.from_list(
        f"{name}_p02_zero_position", shifted_stops, N=cmap.N,
    )


def _format_level(value: float) -> str:
    return f"−{abs(value):,g}" if value < 0 else f"{value:,g}"


def _read_sample(path: Path, geometry):
    with Dataset(path, "r") as sample:
        for name in ("time", "lat", "lon", "outflw"):
            if name not in sample.variables:
                raise ValueError(f"{path} is missing variable {name!r}")
        variable = sample.variables["outflw"]
        if variable.dimensions != ("time", "lat", "lon") or variable.shape[0] != 1:
            raise ValueError("Expected one-record outflw(time, lat, lon) data")
        if getattr(variable, "units", "") != "m3/s":
            raise ValueError("Expected outflw units m3/s")
        nx, ny = geometry[:2]
        if variable.shape[1:] != (ny, nx):
            raise ValueError(f"outflw grid {variable.shape[1:]} does not match params.txt")
        time = sample.variables["time"]
        selected_time = num2date(
            time[:], time.units, calendar=getattr(time, "calendar", "standard"),
            only_use_cftime_datetimes=False,
        )[0]
        data = np.ma.asarray(variable[0, :, :], dtype=np.float64)
        units = getattr(variable, "units", "")
        cell_methods = getattr(variable, "cell_methods", "")
    if cell_methods != "time: mean":
        raise ValueError(f"Expected daily-mean outflw metadata, found {cell_methods!r}")
    return data, selected_time, units


def make_comparison(sample_path: Path, params_path: Path, output_path: Path,
                    celv: str, linthresh: float, linear_fraction: float,
                    dpi: int) -> None:
    levels = P02_MODULE.parse_color_levels(celv)
    norm = P02_MODULE.AsymmetricSignedLogNorm(
        float(levels[0]), float(levels[-1]), linthresh, linear_fraction,
    )
    geometry = read_geometry(params_path)
    nx, ny, _, west, east, south, north = geometry
    values, selected_time, units = _read_sample(sample_path, geometry)
    lon = west + (np.arange(nx) + 0.5) * geometry[2]
    lat = north - (np.arange(ny) + 0.5) * geometry[2]

    figure = plt.figure(figsize=(12.8, 10.0))
    grid = figure.add_gridspec(
        2, 4, height_ratios=(0.13, 0.87),
        width_ratios=(1.0, 0.055, 1.0, 0.055),
        left=0.075, right=0.965, bottom=0.12, top=0.91,
        wspace=0.30, hspace=0.26,
    )
    figure.suptitle(
        f"p02-f01 Discharge Raster — colormap comparison ({selected_time:%Y-%m-%d})",
        fontsize=13,
    )

    strip_positions = np.linspace(0.0, 1.0, 2048)[None, :]
    for column, name in zip((0, 2), PALETTE_NAMES):
        cmap = recenter_palette(name, norm.zero_position)
        swatch = figure.add_subplot(grid[0, column:column + 2])
        swatch.imshow(
            strip_positions, extent=(0, 1, 0, 1),
            cmap=cmap, vmin=0, vmax=1, aspect="auto", interpolation="nearest",
        )
        swatch.axvline(norm(0), color="#202020", linestyle="--", linewidth=1.0)
        swatch.set_yticks([])
        swatch.set_xticks(norm(np.asarray((levels[0], 0, levels[-1]))))
        swatch.set_xticklabels([_format_level(value) for value in (levels[0], 0, levels[-1])])
        swatch.set_title(name.replace("_", "-").title(), fontsize=10, pad=4)

        axis = figure.add_subplot(grid[1, column])
        colorbar_axis = figure.add_subplot(grid[1, column + 1])
        axis.imshow(
            values, origin="upper", extent=(west, east, south, north),
            cmap=cmap, norm=norm, interpolation="nearest",
        )
        midpoint_latitude = (south + north) / 2
        axis.set_aspect(1.0 / np.cos(np.deg2rad(midpoint_latitude)))
        axis.set_xlim(west, east)
        axis.set_ylim(south, north)
        axis.set_xlabel("Longitude")
        if column == 0:
            axis.set_ylabel("Latitude")
        else:
            axis.tick_params(labelleft=False)
        axis.set_title(f"Outflow ({name})", fontsize=9, pad=5)
        mappable = ScalarMappable(norm=norm, cmap=cmap)
        colorbar = figure.colorbar(
            mappable, cax=colorbar_axis, orientation="vertical", ticks=levels,
            extend="both",
        )
        colorbar.ax.set_yticklabels([_format_level(value) for value in levels], fontsize=6)

    figure.text(
        0.5, 0.035, f"Shared scale: daily mean total outflow ({units})",
        ha="center", fontsize=9,
    )

    output_path.parent.mkdir(parents=True, exist_ok=True)
    figure.savefig(output_path, dpi=dpi)
    plt.close(figure)
    print(f"Wrote {output_path}")
    print(f"Source sample: {sample_path}")
    print(
        f"Shared p02-f01 settings: range={levels[0]:g} to {levels[-1]:g} {units}; "
        f"LINTHRESH={linthresh:g}; LINEAR_FRAC={linear_fraction:g}; "
        f"normalized zero position={norm.zero_position:.6f}"
    )


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--sample", type=Path, default=DEFAULT_SAMPLE)
    parser.add_argument("--params", type=Path, default=DEFAULT_PARAMS)
    parser.add_argument("--output", type=Path, default=DEFAULT_OUTPUT)
    parser.add_argument(
        "--celv",
        default="-3000,-1000,-300,-100,-30,-10,0,10,30,100,300,1000,3000,10000,30000",
    )
    parser.add_argument("--linthresh", type=float, default=10.0)
    parser.add_argument("--linear-frac", type=float, default=0.10)
    parser.add_argument("--dpi", type=int, default=180)
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    make_comparison(
        args.sample, args.params, args.output, args.celv,
        args.linthresh, args.linear_frac, args.dpi,
    )


if __name__ == "__main__":
    main()
