#!/usr/bin/env python3
"""Generate registry-driven CMF colormap previews."""

from __future__ import annotations

import colorsys
import json
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.colors import Normalize
import numpy as np

from cmf_colormap import get_cmap, list_colormaps
from cmf_colormap.core import _interpolate_rgb

HERE = Path(__file__).resolve().parent
DEFINITION = Path(__file__).resolve().parents[1] / "src" / "cmf_colormap" / "data" / "shade_rainbow.json"


def _gradient(cmap, count=1024):
    """Return a normalized horizontal color strip."""
    values = np.linspace(0, 1, count)[None, :]
    return cmap(Normalize(0, 1)(values))


def plot_gallery():
    """Write a compact 0–1 gallery for all registered maps."""
    names = list_colormaps()
    fig, axes = plt.subplots(len(names), 1, figsize=(9, max(2.0, 1.35 * len(names))), squeeze=False)
    for ax, name in zip(axes[:, 0], names):
        ax.imshow(_gradient(get_cmap(name)), aspect="auto", extent=(0, 1, 0, 1))
        ax.set_yticks([])
        ax.set_xlim(0, 1)
        ax.set_xticks(np.linspace(0, 1, 6))
        ax.set_ylabel(name, rotation=0, ha="right", va="center", labelpad=12)
    axes[-1, 0].set_xlabel("Normalized colormap position")
    fig.suptitle("CMF Colormaps")
    fig.tight_layout()
    path = HERE / "colormap_gallery.png"
    fig.savefig(path, dpi=180, bbox_inches="tight")
    plt.close(fig)
    print(f"Wrote {path}")


def _hsv_diagnostics(position, controls):
    """Interpolate exact RGB values and derive HSV for diagnostic displays."""
    rgb = np.asarray([_interpolate_rgb(controls, float(x)) for x in position])
    hsv = np.asarray([colorsys.rgb_to_hsv(*color) for color in rgb])
    return rgb, hsv


def plot_shade_rainbow_detail():
    """Write the detailed control, background, HSV, and log-scale reference."""
    definition = json.loads(DEFINITION.read_text(encoding="utf-8"))
    controls = definition["control_colors"]
    cmap = get_cmap("shade_rainbow")
    position = np.linspace(0, 1, 1024)
    rgb, hsv = _hsv_diagnostics(position, controls)
    figure = plt.figure(figsize=(18, 12))
    grid = figure.add_gridspec(4, 1, height_ratios=(1.55, 2.2, 1.25, 1.6), hspace=0.42)
    figure.subplots_adjust(left=0.18, right=0.99, top=0.92, bottom=0.08)

    # Panel 1: exact stored control colors, with HSV calculated only for display.
    controls_grid = grid[0].subgridspec(2, 12, height_ratios=(1.0, 1.15), hspace=0.08)
    for index, item in enumerate(controls):
        ax = figure.add_subplot(controls_grid[0, index])
        ax.set_facecolor(item["hex"])
        ax.set_xticks([])
        ax.set_yticks([])
        ax.set_title(f"{index + 1}\n{item['name']}\n{item['hex']}", fontsize=7)
        ax = figure.add_subplot(controls_grid[1, index])
        hue, saturation, value = colorsys.rgb_to_hsv(*_interpolate_rgb(controls, index / 11))
        ax.text(0.5, 0.68, f"H {hue * 360:.1f}°", ha="center", fontsize=8)
        ax.text(0.5, 0.39, f"S {saturation:.3f}", ha="center", fontsize=8)
        ax.text(0.5, 0.10, f"V {value:.3f}", ha="center", fontsize=8)
        ax.axis("off")
    # Panel 2: thick opaque gradient bars over selectable background colors.
    background_grid = grid[1].subgridspec(3, 1, hspace=0.34)
    for row, (background, label) in enumerate((("#FFFFFF", "White #FFFFFF"),
                                                ("#C0C0C0", "Light gray #C0C0C0"),
                                                ("#555555", "Dark gray #555555"))):
        ax = figure.add_subplot(background_grid[row, 0])
        ax.set_facecolor(background)
        ax.imshow(_gradient(cmap), aspect="auto", extent=(0, 1, 0.25, 0.75), interpolation="nearest")
        ax.set_xlim(0, 1)
        ax.set_ylim(0, 1)
        ax.set_yticks([])
        if row == 2:
            ax.set_xticks(np.linspace(0, 1, 12))
            ax.set_xticklabels([f"{x:.2f}" for x in np.linspace(0, 1, 12)], fontsize=7)
        else:
            ax.set_xticks([])
        ax.set_ylabel(label, rotation=0, ha="right", va="center", labelpad=15, fontsize=8)

    # Panel 3: H-only, saturation grayscale (S^3 display transform), and true V.
    diagnostics_grid = grid[2].subgridspec(3, 1, hspace=0.28)
    hue_rgb = np.asarray([colorsys.hsv_to_rgb(float(h), 1.0, 1.0) for h in hsv[:, 0]])
    for row, (strip, label) in enumerate((
        (hue_rgb[None, :, :], "Hue only (S=1, V=1)"),
        (np.repeat((hsv[:, 1] ** 3)[None, :, None], 3, axis=2), "Saturation grayscale (display = S³)"),
        (np.repeat(hsv[:, 2][None, :, None], 3, axis=2), "Value grayscale (actual V)"),
    )):
        ax = figure.add_subplot(diagnostics_grid[row, 0])
        ax.imshow(strip, aspect="auto", extent=(0, 1, 0, 1), interpolation="nearest")
        ax.set_xlim(0, 1)
        ax.set_yticks([])
        ax.set_xticks([])
        ax.set_ylabel(label, rotation=0, ha="right", va="center", labelpad=15, fontsize=8)

    # Panel 4: actual HSV curves and a log-discharge example, kept separate from the definition.
    curve_grid = grid[3].subgridspec(1, 2, width_ratios=(1.0, 1.0), wspace=0.24)
    ax = figure.add_subplot(curve_grid[0, 0])
    ax.plot(position, hsv[:, 1], label="Saturation", color="#3B6FB6")
    ax.plot(position, hsv[:, 2], label="Value", color="#D97B22")
    ax.set(xlim=(0, 1), ylim=(0, 1.02), xlabel="Normalized position", ylabel="HSV component")
    ax.grid(alpha=0.25)
    ax.legend()

    ax = figure.add_subplot(curve_grid[0, 1])
    discharge = np.geomspace(5, 1_000_000, 512)
    ax.imshow(_gradient(cmap, len(discharge)), aspect="auto",
              extent=(np.log10(discharge[0]), np.log10(discharge[-1]), 0, 1),
              interpolation="nearest")
    ticks = np.array([5, 10, 100, 1_000, 10_000, 100_000, 1_000_000])
    ax.set_xticks(np.log10(ticks), [f"{value:g}" for value in ticks])
    ax.set_yticks([])
    ax.set_xlabel("Example discharge (m³/s; logarithmic)")
    ax.set_title("Demonstration only: controls equally spaced in log value", fontsize=9)

    figure.suptitle("Shade-Rainbow v1.0 — definition and display diagnostics", fontsize=15)
    path = HERE / "shade_rainbow_detailed.png"
    figure.savefig(path, dpi=180, bbox_inches="tight")
    plt.close(figure)
    print(f"Wrote {path}")


if __name__ == "__main__":
    plot_gallery()
    plot_shade_rainbow_detail()
