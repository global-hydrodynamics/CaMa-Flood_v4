#!/usr/bin/env python3
"""Scale station-to-cell link color and width by linked-cell upstream area."""

from __future__ import annotations

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.colors import LogNorm
from matplotlib.lines import Line2D
from matplotlib.patches import FancyArrowPatch, Patch
from matplotlib.ticker import FuncFormatter, LogLocator
from matplotlib.cm import ScalarMappable
import numpy as np

from station_grid_common import (
    draw_background,
    draw_station_and_cell_markers,
    parse_args,
    read_map_and_links,
    save_panel,
    set_map_axes,
)


LABEL_OFFSETS = {
    "2969013": (-32, -11),
    "2969030": (7, -11),
    "2969069": (-48, 7),
    "2969071": (-40, -12),
    "2969160": (-8, -14),
    "2969430": (-8, 7),
}


def main() -> None:
    args = parse_args(__doc__ or "Scale station-to-grid links by upstream area")
    lon, lat, uparea, stations, map_bounds = read_map_and_links(args)
    areas = np.asarray([station["area_km2"] for station in stations], dtype=float)
    low, high = float(areas.min()), float(areas.max())
    if low <= 0 or high <= low:
        raise ValueError("linked cells need distinct, positive upstream areas for log scaling")

    cmap = plt.get_cmap("plasma")
    norm = LogNorm(vmin=low, vmax=high)
    log_low, log_high = np.log10(low), np.log10(high)

    fig, ax = plt.subplots(figsize=(4.2, 8.0))
    draw_background(ax, lon, lat, uparea, map_bounds)

    draw_station_and_cell_markers(ax, stations, LABEL_OFFSETS)
    for station in stations:
        area = station["area_km2"]
        fraction = (np.log10(area) - log_low) / (log_high - log_low)
        arrow = FancyArrowPatch(
            (station["station_lon"], station["station_lat"]),
            (station["cell_lon"], station["cell_lat"]),
            arrowstyle="-|>", mutation_scale=6.5,
            linewidth=0.9 + 2.1 * fraction,
            color=cmap(norm(area)), shrinkA=2, shrinkB=2, zorder=7,
        )
        ax.add_patch(arrow)

    set_map_axes(ax, args)
    ax.set_title("p01-f02b Link style scaled by upstream area", pad=9)

    scalar = ScalarMappable(norm=norm, cmap=cmap)
    colorbar = fig.colorbar(scalar, ax=ax, pad=0.035, fraction=0.055)
    colorbar.set_label("Linked-cell upstream drainage area (km²)")
    colorbar.locator = LogLocator(base=10, subs=(1.0, 2.0, 5.0), numticks=12)
    colorbar.formatter = FuncFormatter(lambda value, _: f"{value:g}")
    colorbar.update_ticks()

    handles = [
        Line2D([], [], marker="o", linestyle="none", markersize=5,
               markerfacecolor="white", markeredgecolor="#762a83",
               label="Station coordinate"),
        Line2D([], [], marker="s", linestyle="none", markersize=5,
               markerfacecolor="#fdae61", markeredgecolor="#303030",
               label="15-minute cell center"),
        Line2D([], [], color=cmap(0.15), linewidth=1.1,
               marker=">", markersize=4, label="Smaller linked-cell area"),
        Line2D([], [], color=cmap(0.9), linewidth=3.0,
               marker=">", markersize=4, label="Larger linked-cell area"),
        Patch(facecolor="#c6dfb8", alpha=0.65,
              label="Faint background: uparea raster (log scale)"),
    ]
    ax.legend(handles=handles, loc="lower left", fontsize=6.8, framealpha=0.92)
    save_panel(fig, args.output, args.dpi)
    print(f"Station links: {len(stations)}; linked-cell uparea: {low:.1f}–{high:.1f} km²")


if __name__ == "__main__":
    main()
