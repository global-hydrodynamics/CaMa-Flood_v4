#!/usr/bin/env python3
"""Plot upstream drainage area on the Mekong 15-minute river map."""

from __future__ import annotations

import argparse
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
from matplotlib.colors import LogNorm, Normalize
from matplotlib.ticker import FuncFormatter, LogLocator

from river_map_common import (
    add_map_arguments,
    draw_raster,
    read_geometry,
    read_map_raster,
)
from river_network_layout import (
    create_network_axes as create_panel_axes,
    save_network_panel as save_fixed_panel,
)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--input", type=Path, required=True, help="uparea.nc")
    parser.add_argument("--cmap", default="YlGnBu")
    parser.add_argument("--scale", choices=("log", "linear"), default="log")
    add_map_arguments(parser)
    args = parser.parse_args()

    geometry = read_geometry(args.params)
    _, _, raw = read_map_raster(args.input, "uparea", "m2", geometry)
    area_km2 = raw.filled(np.nan) / 1_000_000.0
    valid = np.isfinite(area_km2) & (area_km2 > 0)
    if not np.any(valid):
        raise ValueError("uparea.nc contains no finite positive drainage-area values")

    minimum, maximum = float(area_km2[valid].min()), float(area_km2[valid].max())
    norm = LogNorm(vmin=minimum, vmax=maximum) if args.scale == "log" else Normalize(
        vmin=minimum, vmax=maximum
    )
    fig, ax, cax = create_panel_axes(
        geometry, args, "p01-f01a Raster River Map:\nUpstream Area", show_colorbar=True,
    )
    image = draw_raster(
        ax, np.ma.masked_where(~valid, area_km2), geometry, args.cmap, norm
    )
    colorbar = fig.colorbar(image, cax=cax)
    colorbar.set_label("Upstream drainage area (km²)")
    if args.scale == "log":
        colorbar.locator = LogLocator(base=10, subs=(1.0, 2.0, 5.0), numticks=18)
    colorbar.formatter = FuncFormatter(lambda value, _: f"{value:.0f}")
    colorbar.update_ticks()
    save_fixed_panel(fig, ax, cax, args)
    plt.close(fig)
    print(f"Wrote {args.output}; uparea range: {minimum:g}–{maximum:g} km² ({args.scale})")


if __name__ == "__main__":
    main()
