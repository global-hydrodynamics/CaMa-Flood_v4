#!/usr/bin/env python3
"""Plot station coordinates and their assigned 15-minute grid-cell centers."""

from __future__ import annotations

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import FancyArrowPatch

from station_grid_common import (
    add_basic_legend,
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
    args = parse_args(__doc__ or "Draw station-to-grid-cell links")
    lon, lat, uparea, stations, map_bounds = read_map_and_links(args)

    fig, ax = plt.subplots(figsize=(4.2, 8.0))
    draw_background(ax, lon, lat, uparea, map_bounds)

    draw_station_and_cell_markers(ax, stations, LABEL_OFFSETS)
    # Draw links over the small markers so even short offsets remain visible.
    for station in stations:
        arrow = FancyArrowPatch(
            (station["station_lon"], station["station_lat"]),
            (station["cell_lon"], station["cell_lat"]),
            arrowstyle="-|>", mutation_scale=8, linewidth=1.25,
            color="#4d4d4d", shrinkA=0, shrinkB=0, zorder=7,
        )
        ax.add_patch(arrow)
    set_map_axes(ax, args)
    ax.set_title("p01-f02a Station-to-grid-cell links", pad=9)
    add_basic_legend(ax)
    save_panel(fig, args.output, args.dpi)
    print(f"Station links: {len(stations)}; map grid: {len(lon)} x {len(lat)}")


if __name__ == "__main__":
    main()
