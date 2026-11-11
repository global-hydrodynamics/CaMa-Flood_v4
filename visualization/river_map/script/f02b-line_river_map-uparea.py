#!/usr/bin/env python3
"""Plot nextxy links with source-cell upstream area encoded by color and width."""

from __future__ import annotations

import argparse
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
from matplotlib.colors import LogNorm
from matplotlib.ticker import FuncFormatter, LogLocator

from river_map_common import (
    add_aligned_colorbar,
    add_map_arguments,
    create_map_axes,
    draw_raster,
    read_geometry,
    save_panel,
)
from river_network_common import (
    count_segments_intersecting_extent,
    draw_flexible_arrows,
    read_river_network,
)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--uparea", type=Path, required=True)
    parser.add_argument("--nextxy", type=Path, required=True)
    add_map_arguments(parser)
    args = parser.parse_args()

    geometry = read_geometry(args.params)
    network = read_river_network(args.uparea, args.nextxy, geometry)
    area_km2 = network.uparea[network.source_rows, network.source_cols] / 1_000_000.0
    minimum, maximum = float(area_km2.min()), float(area_km2.max())
    norm = LogNorm(vmin=minimum, vmax=maximum)

    fig, ax = create_map_axes(
        geometry, args.west, args.east, args.south, args.north,
        "p01-f02b Source-cell upstream area",
    )
    raster = network.uparea / 1_000_000.0
    raster_valid = np.isfinite(raster) & (raster > 0)
    draw_raster(
        ax, np.ma.masked_where(~raster_valid, raster), geometry, "YlGnBu",
        LogNorm(vmin=float(raster[raster_valid].min()), vmax=float(raster[raster_valid].max())),
    ).set_alpha(0.17)

    source_lon = network.lon[network.source_cols]
    source_lat = network.lat[network.source_rows]
    delta_lon = network.lon[network.dest_cols] - source_lon
    delta_lat = network.lat[network.dest_rows] - source_lat
    log_area = np.log10(area_km2)
    scaled = (log_area - log_area.min()) / (log_area.max() - log_area.min())
    widths = 0.30 + 1.10 * scaled
    local = ~network.flexible
    local_arrows = ax.quiver(
        source_lon[local], source_lat[local], delta_lon[local], delta_lat[local],
        area_km2[local],
        angles="xy", scale_units="xy", scale=1, pivot="tail", units="inches",
        width=0.008, headwidth=7.5, headlength=6.5, headaxislength=5.8,
        cmap="plasma", norm=norm, linewidths=widths[local],
        edgecolors="face", zorder=4,
    )
    flexible_colors = plt.get_cmap("plasma")(norm(area_km2[network.flexible]))
    flexible_arrows = draw_flexible_arrows(
        ax, network, flexible_colors, widths[network.flexible], zorder=5
    )
    rendered = local_arrows.U.size + len(flexible_arrows)
    if rendered != len(network.source_rows):
        raise RuntimeError(
            f"Rendered {rendered} arrows for {len(network.source_rows)} valid nextxy links"
        )
    colorbar = add_aligned_colorbar(
        fig, ax, local_arrows, "Source-cell upstream drainage area (km²)"
    )
    colorbar.locator = LogLocator(base=10, subs=(1.0, 2.0, 5.0), numticks=18)
    colorbar.formatter = FuncFormatter(lambda value, _: f"{value:.0f}")
    colorbar.update_ticks()
    fig.tight_layout()
    save_panel(fig, args.output, args.dpi, args.max_width, args.max_height)
    plt.close(fig)

    visible, visible_local, visible_flexible = count_segments_intersecting_extent(
        network, (args.west, args.east, args.south, args.north)
    )
    print(
        f"Wrote {args.output}: {rendered} arrows ({local_arrows.U.size} straight local, "
        f"{len(flexible_arrows)} curved flexible); source-cell uparea "
        f"range {minimum:g}–{maximum:g} km²; line width range "
        f"{widths.min():.2f}–{widths.max():.2f} pt. "
        f"{visible} direct segments intersect the view ({visible_local} local, "
        f"{visible_flexible} flexible); {len(network.source_rows) - visible} "
        f"links lie outside the view."
    )


if __name__ == "__main__":
    main()
