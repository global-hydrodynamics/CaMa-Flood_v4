#!/usr/bin/env python3
"""Plot every valid nextxy connection, distinguishing local and flexible links."""

from __future__ import annotations

import argparse
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
from matplotlib.colors import LogNorm

from river_map_common import add_map_arguments, draw_raster, read_geometry
from river_network_layout import create_network_axes, save_network_panel
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
    area_km2 = network.uparea / 1_000_000.0
    valid = np.isfinite(area_km2) & (area_km2 > 0)
    fig, ax, cax = create_network_axes(
        geometry, args,
        "p01-f02a Vector River Network: Connectivity Type",
        show_colorbar=False,
    )
    draw_raster(
        ax, np.ma.masked_where(~valid, area_km2), geometry, "YlGnBu",
        LogNorm(vmin=float(area_km2[valid].min()), vmax=float(area_km2[valid].max())),
    ).set_alpha(0.17)

    source_lon = network.lon[network.source_cols]
    source_lat = network.lat[network.source_rows]
    delta_lon = network.lon[network.dest_cols] - source_lon
    delta_lat = network.lat[network.dest_rows] - source_lat
    local = ~network.flexible
    local_arrows = ax.quiver(
        source_lon[local], source_lat[local], delta_lon[local], delta_lat[local],
        angles="xy", scale_units="xy", scale=1, pivot="tail", units="inches",
        width=0.007, headwidth=7.5, headlength=6.5, headaxislength=5.8,
        color="#4778B5", alpha=0.9, zorder=3,
    )
    flexible_arrows = draw_flexible_arrows(
        ax, network, "#F28E2B", np.full(np.count_nonzero(network.flexible), 1.0)
    )
    rendered = local_arrows.U.size + len(flexible_arrows)
    expected = len(network.source_rows)
    if rendered != expected:
        raise RuntimeError(f"Rendered {rendered} arrows for {expected} valid nextxy links")

    ax.plot([], [], color="#4778B5", lw=1.4, label="Local D8 connection")
    ax.plot([], [], color="#F28E2B", lw=1.8, label="Flexible Downstream connection")
    ax.legend(loc="lower left", frameon=True, framealpha=0.95)
    save_network_panel(fig, ax, cax, args)
    plt.close(fig)

    visible, visible_local, visible_flexible = count_segments_intersecting_extent(
        network, (args.west, args.east, args.south, args.north)
    )
    print(
        f"Wrote {args.output}: {rendered} arrows ({local_arrows.U.size} local, "
        f"{len(flexible_arrows)} curved flexible); {visible} direct segments intersect the view "
        f"({visible_local} local, {visible_flexible} flexible); "
        f"{expected - visible} links lie outside the view; "
        f"{network.out_of_map} positive destinations are outside the map."
    )


if __name__ == "__main__":
    main()
