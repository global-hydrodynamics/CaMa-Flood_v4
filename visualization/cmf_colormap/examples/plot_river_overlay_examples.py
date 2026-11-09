#!/usr/bin/env python3
"""Create a synthetic river-network comparison for Shade- and White-Rainbow."""

from __future__ import annotations

from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.colors import LightSource, LogNorm, Normalize
from matplotlib.collections import LineCollection
import numpy as np

from cmf_colormap import get_cmap

HERE = Path(__file__).resolve().parent
SEED = 20261109


def build_synthetic_network(seed: int = SEED, depth: int = 5):
    """Build a connected binary drainage tree with downstream flow accumulation."""
    generator = np.random.default_rng(seed)
    edges = []
    unit_runoff = 2.0

    def add_edge(start, end, flow):
        curvature = generator.uniform(-0.025, 0.025)
        fraction = np.linspace(0, 1, 18)
        x = start[0] * (1 - fraction) + end[0] * fraction
        x += curvature * np.sin(np.pi * fraction)
        y = start[1] * (1 - fraction) + end[1] * fraction
        edges.append({"coordinates": np.column_stack((x, y)), "flow": flow})

    def grow_upstream(parent, levels, span):
        if levels == 0:
            source = (
                parent[0] + generator.uniform(-0.04, 0.04),
                parent[1] + generator.uniform(0.07, 0.12),
            )
            add_edge(source, parent, unit_runoff)
            return 1

        child_leaves = 2 ** (levels - 1)
        for side in (-1, 1):
            child = (
                parent[0] + side * span * generator.uniform(0.72, 1.12),
                parent[1] + generator.uniform(0.09, 0.13),
            )
            add_edge(child, parent, child_leaves * unit_runoff)
            grow_upstream(child, levels - 1, span * 0.55)
        return 2**levels

    outlet_node = (0.5, 0.12)
    leaf_count = grow_upstream(outlet_node, depth, 0.18)
    outlet = (outlet_node[0] + 0.012, 0.035)
    add_edge(outlet_node, outlet, leaf_count * unit_runoff)

    error_generator = np.random.default_rng(seed + 1)
    for edge, error in zip(edges, error_generator.lognormal(np.log(18), 0.9, len(edges))):
        edge["error"] = float(np.clip(error, 0.1, 100.0))
    return edges


def build_comparison_figure():
    """Return the figure and its two line collections for deterministic tests."""
    edges = build_synthetic_network()
    segments = [edge["coordinates"] for edge in edges]
    discharge = np.asarray([edge["flow"] for edge in edges])
    error = np.asarray([edge["error"] for edge in edges])
    widths = 0.55 + 1.4 * np.log(discharge / discharge.min()) / np.log(128 / discharge.min())

    x = np.linspace(0, 1, 420)
    y = np.linspace(0, 1, 320)
    xx, yy = np.meshgrid(x, y)
    elevation = (
        0.5
        + 0.20 * np.sin(2 * np.pi * (xx + 0.12 * np.sin(2 * np.pi * yy)))
        + 0.16 * np.cos(2 * np.pi * (1.15 * yy - 0.08 * np.sin(2 * np.pi * xx)))
        + 0.12 * np.exp(-((xx - 0.28) ** 2 + (yy - 0.72) ** 2) / 0.035)
        + 0.10 * np.exp(-((xx - 0.78) ** 2 + (yy - 0.34) ** 2) / 0.025)
    )
    relief = LightSource(azdeg=315, altdeg=42).hillshade(elevation, vert_exag=1.2)
    relief = 0.5 + 0.3 * (relief - 0.5)

    figure, axes = plt.subplots(1, 2, figsize=(11, 5.1), sharex=True, sharey=True)
    figure.suptitle("Synthetic river networks over shared shaded relief", fontsize=13)
    figure.subplots_adjust(left=0.08, right=0.97, top=0.84, bottom=0.20, wspace=0.15)
    shade_cmap = get_cmap("shade_rainbow")
    white_cmap = get_cmap("white_rainbow")
    shade_norm = LogNorm(vmin=2, vmax=96)
    error_norm = Normalize(vmin=0, vmax=100)

    for ax in axes:
        ax.imshow(relief, cmap="gray", origin="lower", extent=(0, 1, 0, 1),
                  vmin=0.0, vmax=1.0, interpolation="bilinear")
        ax.set(xlim=(0, 1), ylim=(0, 1), xlabel="Synthetic east–west coordinate")
        ax.set_aspect("equal")
        ax.set_xticks([])
        ax.set_yticks([])
        for spine in ax.spines.values():
            spine.set_color("#777777")

    axes[0].set_title("(a) Shade-Rainbow — discharge", fontsize=10)
    discharge_lines = LineCollection(
        segments, cmap=shade_cmap, norm=shade_norm, linewidths=widths,
        capstyle="round", joinstyle="round",
    )
    discharge_lines.set_array(discharge)
    axes[0].add_collection(discharge_lines)
    discharge_bar = figure.colorbar(
        discharge_lines, ax=axes[0], fraction=0.046, pad=0.03,
        ticks=(2, 4, 8, 16, 32, 64),
    )
    discharge_bar.set_ticks((2, 4, 8, 16, 32, 64))
    discharge_bar.set_ticklabels(("2", "4", "8", "16", "32", "64"))
    discharge_bar.minorticks_off()
    discharge_bar.set_label("Synthetic discharge (m³/s; logarithmic)", fontsize=8)
    discharge_bar.ax.tick_params(labelsize=7)

    axes[1].set_title(
        "(b) White-Rainbow — error magnitude\n"
        "White indicates zero or negligible error, not missing data.",
        fontsize=9,
    )
    error_lines = LineCollection(
        segments, cmap=white_cmap, norm=error_norm, linewidths=widths,
        capstyle="round", joinstyle="round",
    )
    error_lines.set_array(error)
    axes[1].add_collection(error_lines)
    error_bar = figure.colorbar(error_lines, ax=axes[1], fraction=0.046, pad=0.03)
    error_bar.set_label("Synthetic nonnegative error (%)", fontsize=8)
    error_bar.ax.tick_params(labelsize=7)
    figure.text(
        0.5, 0.055,
        "Illustrative synthetic network and terrain; no observational or CaMa-Flood output data.",
        ha="center", fontsize=8, color="#444444",
    )
    return figure, discharge_lines, error_lines


def plot_river_overlay_comparison(output_path: Path | None = None) -> Path:
    """Save the paired river overlay example and return its path."""
    destination = Path(output_path) if output_path else HERE / "river_overlay_comparison.png"
    figure, _, _ = build_comparison_figure()
    figure.savefig(destination, dpi=160, bbox_inches="tight")
    plt.close(figure)
    print(f"Wrote {destination}")
    return destination


if __name__ == "__main__":
    plot_river_overlay_comparison()
