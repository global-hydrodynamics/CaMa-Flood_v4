#!/usr/bin/env python3
"""Plot river-cell channel-top elevation on the Mekong river map."""

from __future__ import annotations

import argparse
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
from matplotlib.colors import Normalize
from matplotlib.ticker import FuncFormatter

from river_map_common import (
    add_map_arguments,
    add_aligned_colorbar,
    create_map_axes,
    draw_raster,
    read_geometry,
    read_map_raster,
    save_panel,
)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--input", type=Path, required=True, help="elevtn.nc")
    parser.add_argument("--cmap", default="terrain")
    add_map_arguments(parser)
    args = parser.parse_args()

    geometry = read_geometry(args.params)
    _, _, raw = read_map_raster(args.input, "elevtn", "m", geometry)
    elevation = raw.filled(np.nan)
    valid = np.isfinite(elevation)
    if not np.any(valid):
        raise ValueError("elevtn.nc contains no finite river-cell elevations")

    minimum, maximum = float(elevation[valid].min()), float(elevation[valid].max())
    fig, ax = create_map_axes(
        geometry, args.west, args.east, args.south, args.north,
        "p01-f01c River-cell channel-top elevation",
    )
    image = draw_raster(
        ax, np.ma.masked_where(~valid, elevation), geometry, args.cmap,
        Normalize(vmin=minimum, vmax=maximum),
    )
    colorbar = add_aligned_colorbar(
        fig, ax, image, "Channel-top elevation (m, EGM96)"
    )
    colorbar.formatter = FuncFormatter(lambda value, _: f"{value:.0f}")
    colorbar.update_ticks()
    fig.tight_layout()
    save_panel(fig, args.output, args.dpi, args.max_width, args.max_height)
    plt.close(fig)
    print(f"Wrote {args.output}; elevation range: {minimum:g}–{maximum:g} m")


if __name__ == "__main__":
    main()
