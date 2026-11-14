#!/usr/bin/env python3
"""Render daily signed discharge on the Mekong outlet-to-outlet river network."""

from __future__ import annotations

import argparse
import importlib.util
import json
import os
from pathlib import Path
import sys
import tempfile

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.cm import ScalarMappable
from matplotlib.collections import LineCollection
from matplotlib.colors import LinearSegmentedColormap
from matplotlib.lines import Line2D
from netCDF4 import Dataset, num2date
import numpy as np
from PIL import Image, PngImagePlugin

SCRIPT_DIR = Path(__file__).resolve().parent
PROJECT_DIR = SCRIPT_DIR.parent
ROOT = PROJECT_DIR.parents[1]
TUTORIAL_DIR = ROOT / "tutorial" / "tutorial02-mekong_netcdf_4exp"
sys.path.insert(0, str(ROOT / "visualization" / "river_map" / "script"))

from cmf_colormap import get_cmap
from river_map_common import add_aligned_colorbar, read_geometry, save_panel


def load_helper(path: Path, name: str):
    """Load an existing figure helper by path to reuse its data conventions."""
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"Cannot load helper: {path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


NETWORK = load_helper(TUTORIAL_DIR / "v01-network_maps.py", "p02_network_maps")
BACKGROUND = load_helper(
    TUTORIAL_DIR / "v04-annual_max_discharge.py", "p02_discharge_background",
)
RASTER = load_helper(PROJECT_DIR / "script" / "f01a-discharge_raster-map.py", "p02_raster")
MAP_ROOT = ROOT / "map" / "mekong_15min"
EXPECTED_UNITS = "m3/s"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--sample", type=Path, required=True)
    parser.add_argument("--pthflw", type=Path, help="One-day pthflw sample for panel b")
    parser.add_argument("--direction-aware", action="store_true",
                        help="Show direction markers for signed main and bifurcation flow")
    parser.add_argument("--params", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--figure-height", type=float, default=8.0)
    parser.add_argument("--dpi", type=int, default=200)
    parser.add_argument("--celv", required=True)
    parser.add_argument("--linthresh", type=float, required=True)
    parser.add_argument("--linear-frac", type=float, required=True)
    parser.add_argument("--colormap", required=True)
    parser.add_argument("--west", type=float, required=True)
    parser.add_argument("--east", type=float, required=True)
    parser.add_argument("--south", type=float, required=True)
    parser.add_argument("--north", type=float, required=True)
    parser.add_argument("--line-width-scale", type=float, required=True)
    return parser.parse_args()


def read_outflow(path: Path, geometry):
    nx, ny, resolution, west, east, south, north = geometry
    with Dataset(path) as data:
        variable = data.variables["outflw"]
        if variable.dimensions != ("time", "lat", "lon"):
            raise ValueError(f"{path}: unexpected outflw dimensions {variable.dimensions}")
        if getattr(variable, "units", "") != EXPECTED_UNITS:
            raise ValueError(f"{path}: outflw units must be {EXPECTED_UNITS}")
        if getattr(variable, "cell_methods", "") != "time: mean":
            raise ValueError(f"{path}: outflw must be daily-mean data")
        lon = np.asarray(data.variables["lon"][:], dtype=float)
        lat = np.asarray(data.variables["lat"][:], dtype=float)
        values = np.ma.asarray(variable[0], dtype=float)
        time_var = data.variables["time"]
        selected_time = num2date(
            time_var[:], time_var.units, calendar=getattr(time_var, "calendar", "standard"),
            only_use_cftime_datetimes=False,
        )[0]
    expected_lon = west + (np.arange(nx) + 0.5) * resolution
    expected_lat = north - (np.arange(ny) + 0.5) * resolution
    if values.shape != (ny, nx) or not np.allclose(lon, expected_lon) or not np.allclose(lat, expected_lat):
        raise ValueError(f"{path}: grid coordinates or shape do not match params.txt")
    return values, selected_time


def read_bifurcation_flows(path: Path, bif: dict, outlon, outlat, all_layers=False):
    """Match channel-layer or all-layer pthflw values to pathways through pth_id."""
    with Dataset(path) as data:
        variable = data.variables["pthflw"]
        if variable.dimensions != ("time", "NPTHLEV", "NPTHOUT"):
            raise ValueError(f"{path}: unexpected pthflw dimensions {variable.dimensions}")
        if getattr(variable, "units", "") != EXPECTED_UNITS:
            raise ValueError(f"{path}: pthflw units must be {EXPECTED_UNITS}")
        time_var = data.variables["time"]
        selected_time = num2date(
            time_var[:], time_var.units, calendar=getattr(time_var, "calendar", "standard"),
            only_use_cftime_datetimes=False,
        )[0]
        layer_values = np.ma.asarray(variable[0, :, :], dtype=float)
        path_ids = np.asarray(data.variables["pth_id"][:], dtype=int)
        endpoints = {name: np.asarray(data.variables[name][:], dtype=int)
                     for name in ("ix", "iy", "jx", "jy")}

    map_ids = np.asarray(bif["pth_id"], dtype=int)
    if path_ids.ndim != 1 or len(np.unique(path_ids)) != len(path_ids):
        raise ValueError(f"{path}: pth_id values must be unique")
    if set(path_ids.tolist()) != set(map_ids.tolist()):
        raise ValueError(f"{path}: pth_id values do not match bifprm.nc")
    map_index = {int(path_id): i for i, path_id in enumerate(map_ids)}
    for name in endpoints:
        expected = np.asarray([bif[name][map_index[int(pid)]] for pid in path_ids])
        if not np.array_equal(endpoints[name], expected):
            raise ValueError(f"{path}: {name} differs from bifprm.nc")

    path_widths = np.asarray(
        [bif["width"][:, map_index[int(pid)]] for pid in path_ids], dtype=float,
    ).T
    if layer_values.shape != path_widths.shape:
        raise ValueError(
            f"{path}: pthflw layer/path shape {layer_values.shape} does not match "
            f"bifprm width shape {path_widths.shape}"
        )
    if all_layers:
        # Sum signed flow across pathway layers before plotting each path's net direction.
        values = np.ma.sum(layer_values, axis=0)
        eligible_path = np.any(path_widths > 0, axis=0)
    else:
        values = layer_values[0]
        eligible_path = path_widths[0] > 0
    channel_path = path_widths[0] > 0
    selected = eligible_path & np.isfinite(np.ma.filled(values, np.nan))
    ix = endpoints["ix"] - 1
    iy = endpoints["iy"] - 1
    jx = endpoints["jx"] - 1
    jy = endpoints["jy"] - 1
    segments = np.stack((
        np.column_stack((outlon[iy, ix], outlat[iy, ix])),
        np.column_stack((outlon[jy, jx], outlat[jy, jx])),
    ), axis=1)
    valid = np.all(np.isfinite(segments), axis=(1, 2)) & np.all(segments != -9999, axis=(1, 2))
    selected &= valid
    return (segments[selected], np.asarray(values.filled(np.nan))[selected],
            channel_path[selected], selected_time)


def add_colored_links(ax, segments, values, cmap, norm, widths, linestyle="solid", zorder=3):
    valid = np.isfinite(values)
    for width in np.unique(widths[valid]):
        selected = valid & (widths == width)
        ax.add_collection(LineCollection(
            segments[selected], colors=cmap(norm(values[selected])), linewidths=width,
            linestyles=linestyle, zorder=zorder,
        ))
    return int(np.count_nonzero(valid))


def recenter_diverging_cmap(base_cmap, zero_position: float):
    """Place the registered palette's neutral midpoint at normalized flow zero."""
    old_positions = np.linspace(0.0, 1.0, 257)
    new_positions = np.where(
        old_positions <= 0.5,
        old_positions * (zero_position / 0.5),
        zero_position + (old_positions - 0.5) * ((1 - zero_position) / 0.5),
    )
    return LinearSegmentedColormap.from_list(
        f"{base_cmap.name}_zero_centered",
        list(zip(new_positions, base_cmap(old_positions))),
        N=base_cmap.N,
    )


def embed_map_layout(fig, ax, colorbar_ax, output: Path, dpi: int) -> None:
    """Record map and colorbar bounds for map-area-aligned panel assembly."""
    fig.canvas.draw()
    image_width, image_height = (round(value * dpi) for value in fig.get_size_inches())
    scale_x, scale_y = dpi / fig.dpi, dpi / fig.dpi

    def bounds(axes):
        x, y, width, height = axes.get_window_extent(fig.canvas.get_renderer()).bounds
        return [round(x * scale_x, 6),
                round(image_height - (y + height) * scale_y, 6),
                round(width * scale_x, 6), round(height * scale_y, 6)]

    metadata = PngImagePlugin.PngInfo()
    metadata.add_text("cama_map_layout", json.dumps({
        "bounds_px": bounds(ax),
        "extent": [float(value) for value in (*ax.get_xlim(), *ax.get_ylim())],
        "colorbar_bounds_px": bounds(colorbar_ax),
        "colorbar_visible": colorbar_ax.get_visible(),
    }))
    with tempfile.NamedTemporaryFile(
        prefix=f".{output.stem}-", suffix=".png", dir=output.parent, delete=False,
    ) as temporary:
        temporary_path = Path(temporary.name)
    try:
        with Image.open(output) as image:
            image.save(temporary_path, format="PNG", pnginfo=metadata, dpi=(dpi, dpi))
        os.replace(temporary_path, output)
    finally:
        temporary_path.unlink(missing_ok=True)


def draw(args: argparse.Namespace) -> None:
    extent = (args.west, args.east, args.south, args.north)
    if args.west >= args.east or args.south >= args.north:
        raise ValueError("map extent must have west < east and south < north")
    if args.line_width_scale <= 0:
        raise ValueError("line-width-scale must be positive")
    if args.direction_aware and not args.pthflw:
        raise ValueError("--direction-aware requires a pthflw sample")
    geometry = read_geometry(args.params)
    lon, lat, uparea, nextx, nexty, outlon, outlat, _, bif = NETWORK.read_map_data()
    flow_grid, selected_time = read_outflow(args.sample, geometry)
    if args.pthflw:
        bif_segments, bif_values, bif_channel_paths, bif_time = read_bifurcation_flows(
            args.pthflw, bif, outlon, outlat, all_layers=True,
        )
        if bif_time != selected_time:
            raise ValueError(f"outflw/pthflw timestamps differ: {selected_time} and {bif_time}")
    else:
        bif_segments = np.empty((0, 2, 2))
        bif_values = np.empty(0)
        bif_channel_paths = np.empty(0, dtype=bool)

    links = NETWORK.downstream_links(lon, lat, uparea, nextx, nexty)
    _, _, sr, sc, dr, dc, _, _, invalid = links
    if invalid:
        raise ValueError(f"nextxy has {invalid} out-of-grid destination links")
    outlet_valid = (
        np.isfinite(outlon) & np.isfinite(outlat) & (outlon != -9999) &
        (outlat != -9999) & (outlon >= -180) & (outlon <= 360) &
        (outlat >= -90) & (outlat <= 90)
    )
    valid = outlet_valid[sr, sc] & outlet_valid[dr, dc]
    sr, sc, dr, dc = sr[valid], sc[valid], dr[valid], dc[valid]
    main_segments = np.stack((
        np.column_stack((outlon[sr, sc], outlat[sr, sc])),
        np.column_stack((outlon[dr, dc], outlat[dr, dc])),
    ), axis=1)
    main_values = np.asarray(flow_grid.filled(np.nan))[sr, sc]
    areas = uparea[sr, sc]
    thresholds = np.quantile(np.log10(areas), [0.25, 0.5, 0.75])
    area_class = np.digitize(np.log10(areas), thresholds)
    order = np.argsort(areas, kind="stable")
    main_segments, main_values, area_class = (
        main_segments[order], main_values[order], area_class[order]
    )
    main_widths = np.asarray([0.55, 0.78, 1.08, 1.48])[area_class] * args.line_width_scale

    levels = RASTER.parse_color_levels(args.celv)
    norm = RASTER.AsymmetricSignedLogNorm(
        vmin=float(levels[0]), vmax=float(levels[-1]), linthresh=args.linthresh,
        linear_color_fraction=args.linear_frac,
    )
    cmap = recenter_diverging_cmap(get_cmap(args.colormap), norm.zero_position)
    cmap.set_bad("white")
    background, bg_extent, bg_min, bg_max, _, _ = BACKGROUND.read_elevation_background(
        BACKGROUND.ELEVATION_FILE, extent,
    )

    panel_letter = "b" if args.direction_aware else "a"
    option = "Direction-Aware" if args.direction_aware else "Default"
    title = f"p02-f02{panel_letter} Discharge Vector: {option}\n{selected_time:%Y-%m-%d}"
    fig, ax = plt.subplots(figsize=(8, 8))
    ax.set_facecolor("#FAFAFA")
    ax.imshow(background, extent=bg_extent, origin="upper", cmap="Greys",
              vmin=bg_min, vmax=bg_max, interpolation="nearest", alpha=0.60, zorder=0)
    for index in range(4):
        chosen = area_class == index
        add_colored_links(ax, main_segments[chosen], main_values[chosen], cmap, norm,
                          main_widths[chosen], zorder=3)
        if args.direction_aware:
            # Mark reverse-flow links against the stored network direction.
            reverse = chosen & (main_values < 0)
            for segment, value, width in zip(
                    main_segments[reverse], main_values[reverse], main_widths[reverse]):
                NETWORK.add_directional_arrow(
                    ax, segment[::-1], cmap(norm(value)), linewidth=width,
                    start_fraction=0.42, end_fraction=0.60, size=6.5, zorder=4,
                )
    if args.pthflw:
        bif_display_values = np.abs(bif_values) if args.direction_aware else bif_values
        bif_widths = np.full(len(bif_values), 1.35 * args.line_width_scale)
        # Keep channel paths dashed and identify overland-only paths with dots.
        for path_mask, path_style in (
            (bif_channel_paths, (0, (2.4, 1.6))),
            (~bif_channel_paths, (0, (0.7, 1.5))),
        ):
            add_colored_links(
                ax, bif_segments[path_mask], bif_display_values[path_mask], cmap, norm,
                bif_widths[path_mask], linestyle=path_style, zorder=4,
            )
        if args.direction_aware:
            # Color by magnitude; negative flow reverses the stored map direction.
            bif_width = 1.35 * args.line_width_scale
            for segment, value in zip(bif_segments, bif_values):
                flow_segment = segment if value >= 0 else segment[::-1]
                NETWORK.add_directional_arrow(
                    ax, flow_segment, cmap(norm(abs(value))), linewidth=bif_width,
                    start_fraction=0.42, end_fraction=0.60, size=7.0, zorder=5,
                )

    ax.set_xlim(extent[0], extent[1])
    ax.set_ylim(extent[2], extent[3])
    ax.set_xlabel("Longitude")
    ax.set_ylabel("Latitude")
    ax.set_title(title)
    ax.set_aspect(1.0 / np.cos(np.deg2rad((extent[2] + extent[3]) / 2)))
    ax._river_map_extent = extent
    if args.direction_aware:
        main_legend = Line2D(
            [0, 0.5, 1], [0, 0, 0], color="#333333",
            linewidth=1.1 * args.line_width_scale, marker=">", markevery=[1],
            markersize=5.0,
            label="Main-channel discharge (arrow marks reverse flow)",
        )
    else:
        main_legend = Line2D(
            [], [], color="#333333", linewidth=1.1 * args.line_width_scale,
            label="Main-channel signed discharge (outflw)",
        )
    legend_handles = [main_legend]
    if args.pthflw:
        if args.direction_aware:
            channel_bifurcation_legend = Line2D(
                [0, 0.5, 1], [0, 0, 0], color="#333333",
                linewidth=1.1 * args.line_width_scale,
                linestyle=(0, (2.4, 1.6)), marker=">", markevery=[1],
                markersize=5.0,
                label="Channel bifurcation (dashed)",
            )
            overland_bifurcation_legend = Line2D(
                [0, 0.5, 1], [0, 0, 0], color="#333333",
                linewidth=1.1 * args.line_width_scale,
                linestyle=(0, (0.7, 1.5)), marker=">", markevery=[1],
                markersize=5.0,
                label="Overland-only (dotted)",
            )
            legend_handles.extend((channel_bifurcation_legend, overland_bifurcation_legend))
        else:
            channel_bifurcation_legend = Line2D(
                [], [], color="#333333", linewidth=1.1 * args.line_width_scale,
                linestyle=(0, (2.4, 1.6)),
                label="Channel bifurcation (dashed)",
            )
            overland_bifurcation_legend = Line2D(
                [], [], color="#333333", linewidth=1.1 * args.line_width_scale,
                linestyle=(0, (0.7, 1.5)),
                label="Overland-only (dotted)",
            )
            legend_handles.extend((channel_bifurcation_legend, overland_bifurcation_legend))
    fig.legend(handles=legend_handles, loc="lower center", ncols=len(legend_handles),
               bbox_to_anchor=(0.48, 0.025), frameon=False,
               fontsize=7 if args.direction_aware else 8)
    colorbar = add_aligned_colorbar(
        fig, ax, ScalarMappable(norm=norm, cmap=cmap),
        "Daily mean discharge (m³/s)", extend="both",
    )
    colorbar.set_ticks(levels)
    colorbar.ax.set_yticklabels([RASTER.format_color_level(value) for value in levels])
    save_panel(fig, args.output, args.dpi, figure_height=args.figure_height)
    embed_map_layout(fig, ax, colorbar.ax, args.output, args.dpi)
    plt.close(fig)

    finite_main = main_values[np.isfinite(main_values)]
    finite_bif = bif_values[np.isfinite(bif_values)]
    bif_description = "all-layer-sum"
    channel_path_count = int(np.count_nonzero(bif_channel_paths))
    overland_path_count = len(bif_channel_paths) - channel_path_count
    print(f"Wrote {args.output}; time={selected_time}; main links={len(main_segments)}, "
          f"finite={finite_main.size}, range={finite_main.min():.3g}..{finite_main.max():.3g} m3/s; "
          f"{bif_description} bifurcation links={len(bif_segments)}, "
          f"finite={finite_bif.size}, layers={bif['width'].shape[0]}, "
          f"channel={channel_path_count}, overland-only={overland_path_count}")


def main() -> None:
    args = parse_args()
    try:
        if not 0 < args.linear_frac < 1:
            raise ValueError("LINEAR_FRAC must be between zero and one")
        draw(args)
    except (ValueError, OSError, RuntimeError, KeyError) as exc:
        raise SystemExit(f"Discharge-vector plotting failed: {exc}") from exc


if __name__ == "__main__":
    main()
