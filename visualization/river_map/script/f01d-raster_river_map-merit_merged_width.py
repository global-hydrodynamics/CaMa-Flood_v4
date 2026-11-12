#!/usr/bin/env python3
"""Plot MERIT Hydro-merged river-channel width."""

from __future__ import annotations

import argparse
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
from matplotlib.colors import LogNorm, Normalize

from river_map_common import (
    add_map_arguments,
    add_aligned_colorbar,
    add_log_colorbar,
    create_map_axes,
    draw_raster,
    read_geometry,
    read_map_raster,
    save_panel,
)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--input", type=Path, required=True, help="rivwth_merit.nc")
    parser.add_argument("--cmap", default="magma")
    parser.add_argument("--scale", choices=("log", "linear"), default="log")
    add_map_arguments(parser)
    args = parser.parse_args()

    geometry = read_geometry(args.params)
    _, _, raw = read_map_raster(args.input, "rivwth", "m", geometry)
    width = raw.filled(np.nan)
    valid = np.isfinite(width) & (width > 0)
    if not np.any(valid):
        raise ValueError("rivwth_merit.nc contains no finite positive widths")

    minimum, maximum = float(width[valid].min()), float(width[valid].max())
    fig, ax = create_map_axes(
        geometry, args.west, args.east, args.south, args.north,
        "p01-f01d Raster River Map: MERIT-Merged Width",
    )
    norm = LogNorm(vmin=minimum, vmax=maximum) if args.scale == "log" else Normalize(
        vmin=minimum, vmax=maximum
    )
    image = draw_raster(
        ax, np.ma.masked_where(~valid, width), geometry, args.cmap, norm
    )
    if args.scale == "log":
        add_log_colorbar(fig, ax, image, "River-channel width (m)")
    else:
        add_aligned_colorbar(fig, ax, image, "River-channel width (m)")
    fig.tight_layout()
    save_panel(fig, args.output, args.dpi, args.max_width, args.max_height)
    plt.close(fig)
    print(
        f"Wrote {args.output}; positive width range: {minimum:g}–{maximum:g} m "
        f"({args.scale})"
    )


if __name__ == "__main__":
    main()
