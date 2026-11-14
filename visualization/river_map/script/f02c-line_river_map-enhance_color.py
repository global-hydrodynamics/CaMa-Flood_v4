#!/usr/bin/env python3
"""Plot arrowless nextxy links with upstream area encoded by color and width."""

from __future__ import annotations

import argparse
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
from matplotlib.colors import BoundaryNorm, ListedColormap, LogNorm
from cmf_colormap import get_cmap
from matplotlib.ticker import FuncFormatter, LogLocator

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
    parser.add_argument("--subcol", type=Path, required=True)
    add_map_arguments(parser)
    args = parser.parse_args()

    geometry = read_geometry(args.params)
    network = read_river_network(args.uparea, args.nextxy, geometry)
    area_km2 = network.uparea[network.source_rows, network.source_cols] / 1_000_000.0
    minimum, maximum = float(area_km2.min()), float(area_km2.max())
    norm = LogNorm(vmin=minimum, vmax=maximum)

    fig, ax, cax = create_network_axes(
        geometry, args,
        "p01-f02c Line River Map:\nEnhance Color",
        show_colorbar=True,
    )
    nx, ny, _, _, _, _, _ = geometry
    subcol = np.fromfile(args.subcol, dtype="<i4")
    if subcol.size != nx * ny:
        raise ValueError(
            f"{args.subcol} contains {subcol.size} int32 values; expected {nx * ny}"
        )
    # Fortran (NX, NY) storage has x varying fastest; reshape to plotted (NY, NX).
    subcol = subcol.reshape((ny, nx))
    valid = (subcol >= 0) & (subcol <= 10)
    gray_levels = [0.50, *np.linspace(0.45, 0.10, 10)]
    gray = ListedColormap([(level, level, level) for level in gray_levels])
    gray = gray.with_extremes(bad="white")
    draw_raster(ax, np.ma.masked_where(~valid, subcol), geometry, gray,
                BoundaryNorm(np.arange(-0.5, 11.5), gray.N))

    cmap = get_cmap("shade_rainbow")
    rendered, widths = draw_area_arrows(
        ax, network, area_km2, norm, cmap=cmap, show_heads=False,
        width_range=(0.008, 0.032),
    )
    mappable = plt.cm.ScalarMappable(norm=norm, cmap=cmap)
    colorbar = fig.colorbar(mappable, cax=cax)
    colorbar.set_label(
        "Source-cell upstream drainage area (km²)\n(color and line width)"
    )
    colorbar.locator = LogLocator(base=10, subs=(1.0, 2.0, 5.0), numticks=18)
    colorbar.formatter = FuncFormatter(lambda value, _: f"{value:.0f}")
    colorbar.update_ticks()
    save_network_panel(fig, ax, cax, args)
    plt.close(fig)

    visible, visible_local, visible_flexible = count_segments_intersecting_extent(
        network, (args.west, args.east, args.south, args.north)
    )
    print(
        f"Wrote {args.output}: {rendered} arrowless links ({np.count_nonzero(~network.flexible)} straight local, "
        f"{np.count_nonzero(network.flexible)} curved flexible); source-cell uparea "
        f"range {minimum:g}–{maximum:g} km²; shaft width range "
        f"{widths.min():.3f}–{widths.max():.3f} inches. "
        f"{visible} direct segments intersect the view ({visible_local} local, "
        f"{visible_flexible} flexible); {len(network.source_rows) - visible} "
        f"links lie outside the view."
    )


if __name__ == "__main__":
    main()
