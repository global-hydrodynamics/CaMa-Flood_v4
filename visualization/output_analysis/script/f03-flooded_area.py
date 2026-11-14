#!/usr/bin/env python3
"""Plot raw and fractional flooded-area fields for one existing CF-date sample."""

from __future__ import annotations

import argparse
import json
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.colors import Normalize
from netCDF4 import Dataset, num2date
import numpy as np
from PIL import Image, PngImagePlugin

import sys
SCRIPT_DIR = Path(__file__).resolve().parent
ROOT = SCRIPT_DIR.parents[2]
sys.path.insert(0, str(ROOT / "visualization" / "river_map" / "script"))
from river_map_common import read_geometry, read_map_raster  # noqa: E402

AREA_UNITS = "m2"
FRACTION_UNITS = "1"
# Keep the plotted map and colorbar at their existing physical sizes and positions.
MAP_RECT_INCHES = (1.134, 1.26, 5.519, 6.21)
COLORBAR_RECT_INCHES = (7.182, 1.26, 0.27, 6.21)
PANEL_TITLES = {
    "a": "p02-f03a Flooded Area: fldare\nRaw flooded area [km²]",
    "b": "p02-f03b Flooded Area: fldfrc\nFlooded fraction within unit catchment",
    "c": "p02-f03c Flooded Area: fldare - prmwat\nTemporary flooded fraction (prmwat excluded)",
    "d": "p02-f03d Flooded Area: prmwat / ctmare\nPermanent-water fraction within unit catchment",
}


def parse_ticks(text: str, name: str) -> np.ndarray:
    try:
        values = np.asarray([float(part.strip()) for part in text.split(",")])
    except ValueError as exc:
        raise ValueError(f"{name} must be comma-separated numeric values") from exc
    if values.size < 2 or not np.all(np.isfinite(values)) or np.any(np.diff(values) <= 0):
        raise ValueError(f"{name} must be finite and strictly ascending")
    return values


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--panel", choices=("a", "b", "c", "d"), required=True)
    parser.add_argument("--date", required=True)
    parser.add_argument("--fldare", type=Path, required=True)
    parser.add_argument("--fldfrc", type=Path, required=True)
    parser.add_argument("--ctmare", type=Path, required=True)
    parser.add_argument("--prmwat", type=Path, required=True)
    parser.add_argument("--params", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    for edge in ("west", "east", "south", "north"):
        parser.add_argument(f"--{edge}", type=float, required=True)
    parser.add_argument("--area-min", type=float, required=True)
    parser.add_argument("--area-max", type=float, required=True)
    parser.add_argument("--area-ticks", required=True)
    parser.add_argument("--area-cmap", required=True)
    parser.add_argument("--fraction-min", type=float, required=True)
    parser.add_argument("--fraction-max", type=float, required=True)
    parser.add_argument("--fraction-ticks", required=True)
    parser.add_argument("--fraction-cmap", required=True)
    parser.add_argument("--permanent-cmap", required=True)
    parser.add_argument("--zero-color", required=True)
    parser.add_argument("--title-size", type=float, required=True)
    parser.add_argument("--axis-label-size", type=float, required=True)
    parser.add_argument("--tick-label-size", type=float, required=True)
    parser.add_argument("--colorbar-label-size", type=float, required=True)
    parser.add_argument("--figure-width", type=float, required=True)
    parser.add_argument("--figure-height", type=float, required=True)
    parser.add_argument("--dpi", type=int, required=True)
    return parser.parse_args()


def read_sample(path: Path, name: str, expected_units: str, geometry):
    nx, ny, resolution, west, east, south, north = geometry
    with Dataset(path) as source:
        if name not in source.variables:
            raise ValueError(f"{path}: missing variable {name}")
        variable = source.variables[name]
        if variable.dimensions != ("time", "lat", "lon") or variable.shape != (1, ny, nx):
            raise ValueError(f"{path}:{name} must have shape (1,{ny},{nx}) in time,lat,lon order")
        if getattr(variable, "units", "") != expected_units:
            raise ValueError(f"{path}:{name} units must be {expected_units!r}")
        values = np.ma.asarray(variable[0], dtype=np.float64)
        lon = np.asarray(source.variables["lon"][:], dtype=np.float64)
        lat = np.asarray(source.variables["lat"][:], dtype=np.float64)
        time_var = source.variables["time"]
        selected = num2date(
            time_var[:], time_var.units,
            calendar=getattr(time_var, "calendar", "standard"),
            only_use_cftime_datetimes=False,
        )[0]
    expected_lon = west + (np.arange(nx) + 0.5) * resolution
    expected_lat = north - (np.arange(ny) + 0.5) * resolution
    if (not np.allclose(lon, expected_lon, rtol=0, atol=1e-5) or
            not np.allclose(lat, expected_lat, rtol=0, atol=1e-5)):
        raise ValueError(f"{path}: coordinates do not match {nx}x{ny} params.txt grid")
    return values, selected


def png_map_metadata(fig, ax, cax, output: Path, dpi: int, extent) -> None:
    """Store map axes bounds so the panel assembler aligns geographic areas."""
    fig.canvas.draw()
    width_px, height_px = (round(v * dpi) for v in fig.get_size_inches())
    scale = dpi / fig.dpi

    def bounds(axes):
        x, y, width, height = axes.get_window_extent(fig.canvas.get_renderer()).bounds
        return [round(x * scale, 5), round(height_px - (y + height) * scale, 5),
                round(width * scale, 5), round(height * scale, 5)]

    metadata = PngImagePlugin.PngInfo()
    metadata.add_text("cama_map_layout", json.dumps({
        "bounds_px": bounds(ax), "extent": list(extent),
        "colorbar_bounds_px": bounds(cax), "colorbar_visible": True,
    }))
    temporary = output.with_name(f".{output.name}.tmp.png")
    try:
        with Image.open(output) as image:
            image.save(temporary, format="PNG", pnginfo=metadata, dpi=(dpi, dpi))
        temporary.replace(output)
    finally:
        temporary.unlink(missing_ok=True)


def figure_rect(rect_in_inches: tuple[float, float, float, float],
                figure_width: float, figure_height: float) -> tuple[float, float, float, float]:
    """Convert fixed physical axes dimensions to Matplotlib figure fractions."""
    left, bottom, width, height = rect_in_inches
    return (left / figure_width, bottom / figure_height,
            width / figure_width, height / figure_height)


def draw(args: argparse.Namespace) -> None:
    if not (args.west < args.east and args.south < args.north):
        raise ValueError("map extent must be ordered")
    if args.figure_width <= 0 or args.figure_height <= 0 or args.dpi < 1:
        raise ValueError("figure dimensions and DPI must be positive")
    if not (args.area_min >= 0 and args.area_min < args.area_max):
        raise ValueError("raw area linear bounds must satisfy 0 <= minimum < maximum")
    if not (args.fraction_min < args.fraction_max):
        raise ValueError("fraction bounds must be ascending")
    area_ticks = parse_ticks(args.area_ticks, "AREA_TICKS")
    fraction_ticks = parse_ticks(args.fraction_ticks, "FRACTION_TICKS")
    if area_ticks[0] < args.area_min or area_ticks[-1] > args.area_max:
        raise ValueError("AREA_TICKS must lie within AREA_MIN and AREA_MAX")
    if fraction_ticks[0] < args.fraction_min or fraction_ticks[-1] > args.fraction_max:
        raise ValueError("FRACTION_TICKS must lie within FRACTION_MIN and FRACTION_MAX")

    geometry = read_geometry(args.params)
    fldare, selected_time = read_sample(args.fldare, "fldare", AREA_UNITS, geometry)
    fldfrc, fraction_time = read_sample(args.fldfrc, "fldfrc", FRACTION_UNITS, geometry)
    if selected_time != fraction_time or selected_time.strftime("%Y-%m-%d") != args.date:
        raise ValueError("fldare/fldfrc sample dates must match the requested date")
    lon_c, lat_c, ctmare = read_map_raster(args.ctmare, "ctmare", AREA_UNITS, geometry)
    lon_p, lat_p, prmwat = read_map_raster(args.prmwat, "prmwat", AREA_UNITS, geometry)
    del lon_c, lat_c, lon_p, lat_p

    masks = [np.ma.getmaskarray(values) | ~np.isfinite(values.filled(np.nan))
             for values in (fldare, fldfrc, ctmare, prmwat)]
    fldare_mask, fldfrc_mask, ctmare_mask, prmwat_mask = masks
    if fldare.shape != ctmare.shape or fldare.shape != prmwat.shape:
        raise ValueError("flooded-area fields and map inputs must have matching shapes")
    valid_fldare = ~fldare_mask
    if np.any(fldare.data[valid_fldare] < 0):
        raise ValueError("fldare contains negative valid area values")
    valid_ratio = (~fldare_mask & ~fldfrc_mask & ~ctmare_mask &
                   np.isfinite(ctmare.filled(np.nan)) & (ctmare.filled(0) > 0))
    fraction_from_area = fldare.filled(np.nan) / ctmare.filled(np.nan)
    diff = np.abs(fraction_from_area[valid_ratio] - fldfrc.filled(np.nan)[valid_ratio])
    print(f"{args.date}: fldare / ctmare vs fldfrc max abs difference={diff.max():.9g}")

    derived_mask = (fldare_mask | ctmare_mask | prmwat_mask |
                    ~np.isfinite(ctmare.filled(np.nan)) |
                    ~np.isfinite(prmwat.filled(np.nan)) | (ctmare.filled(0) <= 0))
    temporary_area = np.maximum(fldare.filled(np.nan) - prmwat.filled(np.nan), 0.0)
    seasonal_fraction = np.ma.array(
        temporary_area / ctmare.filled(np.nan), mask=derived_mask,
    )
    permanent_mask = (ctmare_mask | prmwat_mask |
                      ~np.isfinite(ctmare.filled(np.nan)) |
                      ~np.isfinite(prmwat.filled(np.nan)) | (ctmare.filled(0) <= 0))
    permanent_fraction = np.ma.array(
        prmwat.filled(np.nan) / ctmare.filled(np.nan), mask=permanent_mask,
    )
    raw_area_km2 = np.ma.array(fldare * 1.0e-6, mask=fldare_mask)
    values = (raw_area_km2 if args.panel == "a" else fldfrc if args.panel == "b"
              else seasonal_fraction if args.panel == "c" else permanent_fraction)
    valid_values = values.compressed()
    if valid_values.size == 0:
        raise ValueError(f"panel {args.panel} has no valid values")
    lower, upper = ((args.area_min, args.area_max) if args.panel == "a"
                    else (args.fraction_min, args.fraction_max))
    out_low = int(np.count_nonzero(valid_values < lower))
    out_high = int(np.count_nonzero(valid_values > upper))
    print(f"panel {args.panel}: valid={valid_values.size}, min={valid_values.min():.9g}, "
          f"max={valid_values.max():.9g}, below={out_low}, above={out_high}")
    if args.panel in ("c", "d"):
        fraction_field = seasonal_fraction if args.panel == "c" else permanent_fraction
        fraction_mask = derived_mask if args.panel == "c" else permanent_mask
        outside = np.argwhere((~fraction_mask) &
                              ((fraction_field.filled(np.nan) < 0) |
                               (fraction_field.filled(np.nan) > 1)))
        measure = "temporary" if args.panel == "c" else "permanent-water"
        print(f"{measure} fraction outside [0,1]: {len(outside)} cells")
        for row, col in outside[:20]:
            print(f"  lat_index={row}, lon_index={col}, value={fraction_field[row, col]:.9g}")

    extent = (args.west, args.east, args.south, args.north)
    midpoint_lat = (args.south + args.north) / 2.0
    figure = plt.figure(figsize=(args.figure_width, args.figure_height))
    ax = figure.add_axes(figure_rect(MAP_RECT_INCHES,
                                     args.figure_width, args.figure_height))
    ax.set_xlim(args.west, args.east)
    ax.set_ylim(args.south, args.north)
    ax.set_aspect(1.0 / np.cos(np.deg2rad(midpoint_lat)))
    ax.set_xlabel("Longitude", fontsize=args.axis_label_size)
    ax.set_ylabel("Latitude", fontsize=args.axis_label_size)
    ax.tick_params(axis="both", labelsize=args.tick_label_size)
    ax.set_title(PANEL_TITLES[args.panel] + f"\n{args.date}",
                 fontsize=args.title_size, pad=10)
    ax.set_facecolor("white")
    ax.grid(False)

    if args.panel == "a":
        cmap = plt.get_cmap(args.area_cmap).copy()
        cmap.set_bad("white")
        cmap.set_under("#54202b")
        cmap.set_over("#4d0011")
        positive = np.ma.array(raw_area_km2, mask=fldare_mask | (fldare.filled(0) <= 0))
        image = ax.imshow(
            positive, origin="upper", extent=geometry[3:], interpolation="nearest",
            cmap=cmap, norm=Normalize(args.area_min, args.area_max, clip=False),
            aspect=ax.get_aspect(),
        )
        zero_cells = np.ma.array(
            np.ones(fldare.shape), mask=fldare_mask | (fldare.filled(-1) != 0),
        )
        ax.imshow(zero_cells, origin="upper", extent=geometry[3:], interpolation="nearest",
                  cmap=matplotlib.colors.ListedColormap([args.zero_color]), vmin=0, vmax=1,
                  aspect=ax.get_aspect())
        mappable = image
        ticks = area_ticks
        label = "fldare: flooded area [km²] (linear; zero shown in gray)"
        extend = "both" if out_low and out_high else "min" if out_low else "max" if out_high else "neither"
    else:
        fraction_cmap_name = (args.permanent_cmap if args.panel == "d"
                              else args.fraction_cmap)
        cmap = plt.get_cmap(fraction_cmap_name).copy()
        cmap.set_bad("white")
        cmap.set_under("#4d315e")
        cmap.set_over("#173f5f")
        image = ax.imshow(
            values, origin="upper", extent=geometry[3:], interpolation="nearest",
            cmap=cmap, norm=Normalize(args.fraction_min, args.fraction_max, clip=False),
            aspect=ax.get_aspect(),
        )
        mappable = image
        ticks = fraction_ticks
        label = ("fldfrc: flooded fraction of unit catchment" if args.panel == "b"
                 else "max(fldare - prmwat, 0) / ctmare: temporary fraction"
                 if args.panel == "c" else "prmwat / ctmare: permanent-water fraction")
        extend = "both" if out_low and out_high else "min" if out_low else "max" if out_high else "neither"

    colorbar_rect = figure_rect(COLORBAR_RECT_INCHES,
                                args.figure_width, args.figure_height)
    cax = figure.add_axes(colorbar_rect)
    colorbar = figure.colorbar(mappable, cax=cax, ticks=ticks, extend=extend)
    colorbar.ax.set_position(colorbar_rect)
    colorbar.set_label(label, labelpad=12)
    colorbar.ax.tick_params(labelsize=args.tick_label_size)
    colorbar.ax.yaxis.label.set_size(args.colorbar_label_size)
    figure.patch.set_facecolor("white")
    args.output.parent.mkdir(parents=True, exist_ok=True)
    figure.savefig(args.output, dpi=args.dpi, format="png")
    png_map_metadata(figure, ax, cax, args.output, args.dpi, extent)
    plt.close(figure)


def main() -> None:
    args = parse_args()
    try:
        draw(args)
    except (OSError, ValueError, KeyError) as exc:
        raise SystemExit(f"ERROR: {exc}") from exc


if __name__ == "__main__":
    main()
