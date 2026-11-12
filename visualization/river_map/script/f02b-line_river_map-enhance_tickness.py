#!/usr/bin/env python3
"""Plot black nextxy arrows with enhanced upstream-area-based thickness."""

from __future__ import annotations

import argparse
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
from matplotlib.colors import LogNorm
from matplotlib.lines import Line2D

from river_map_common import (
    add_map_arguments,
    draw_raster,
    read_geometry,
)
from river_network_layout import create_network_axes, save_network_panel
from river_network_common import (
    count_segments_intersecting_extent,
    draw_area_arrows,
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

    fig, ax, cax = create_network_axes(
        geometry, args,
        "p01-f02b Line River Map:\nEnhance Tickness",
        show_colorbar=False,
    )
    raster = network.uparea / 1_000_000.0
    valid = np.isfinite(raster) & (raster > 0)
    draw_raster(ax, np.ma.masked_where(~valid, raster), geometry, "YlGnBu",
                LogNorm(vmin=float(raster[valid].min()), vmax=float(raster[valid].max()))
                ).set_alpha(0.17)

    rendered, widths = draw_area_arrows(ax, network, area_km2, norm)
    # Representative physical areas, drawn with the same six width classes.
    handles = []
    for area in (1_000, 10_000, 100_000):
        level = min(5, int(np.clip(norm(area), 0, 1) * 6))
        width = 0.008 + 0.024 * level / 5
        handles.append(Line2D([], [], color="black", lw=width * 72,
                              label=f"{area:,} km²"))
    ax.legend(handles=handles, title="Source-cell upstream area",
              loc="lower left", framealpha=0.95, fontsize=9, title_fontsize=9)
    save_network_panel(fig, ax, cax, args)
    plt.close(fig)

    visible, visible_local, visible_flexible = count_segments_intersecting_extent(
        network, (args.west, args.east, args.south, args.north)
    )
    print(
        f"Wrote {args.output}: {rendered} arrows ({np.count_nonzero(~network.flexible)} straight local, "
        f"{np.count_nonzero(network.flexible)} curved flexible); source-cell uparea "
        f"range {minimum:g}–{maximum:g} km²; shaft width range "
        f"{widths.min():.3f}–{widths.max():.3f} inches. "
        f"{visible} direct segments intersect the view ({visible_local} local, "
        f"{visible_flexible} flexible); {len(network.source_rows) - visible} "
        f"links lie outside the view."
    )


if __name__ == "__main__":
    main()
