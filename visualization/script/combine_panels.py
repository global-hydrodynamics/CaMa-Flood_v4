#!/usr/bin/env python3
"""Arrange rendered panel images into one complete Figure image."""

from __future__ import annotations

import argparse
import math
import os
from pathlib import Path
import tempfile

from PIL import Image, ImageColor


def arrange_panels(images: list[Image.Image], layout: str, columns: int,
                   gap: int, margin: int, background: tuple[int, int, int]
                   ) -> Image.Image:
    """Compose existing images without changing their scientific content."""
    if layout == "horizontal":
        cell_widths = [image.width for image in images]
        cell_heights = [max(image.height for image in images)]
        positions = []
        x = margin
        for image in images:
            positions.append((x, None))
            x += image.width + gap
        width = sum(cell_widths) + gap * (len(images) - 1) + 2 * margin
        height = cell_heights[0] + 2 * margin
        positions = [(x, margin + (height - 2 * margin - image.height) // 2)
                     for (x, _), image in zip(positions, images)]
    elif layout == "vertical":
        cell_widths = [max(image.width for image in images)]
        positions = []
        y = margin
        for image in images:
            positions.append((None, y))
            y += image.height + gap
        width = cell_widths[0] + 2 * margin
        height = sum(image.height for image in images) + gap * (len(images) - 1) + 2 * margin
        positions = [(margin + (width - 2 * margin - image.width) // 2, y)
                     for (_, y), image in zip(positions, images)]
    else:
        column_count = min(columns, len(images))
        row_count = math.ceil(len(images) / column_count)
        cell_widths = [0] * column_count
        cell_heights = [0] * row_count
        for index, image in enumerate(images):
            row, column = divmod(index, column_count)
            cell_widths[column] = max(cell_widths[column], image.width)
            cell_heights[row] = max(cell_heights[row], image.height)
        x_offsets = [margin]
        y_offsets = [margin]
        for cell_width in cell_widths[:-1]:
            x_offsets.append(x_offsets[-1] + cell_width + gap)
        for cell_height in cell_heights[:-1]:
            y_offsets.append(y_offsets[-1] + cell_height + gap)
        width = sum(cell_widths) + gap * (column_count - 1) + 2 * margin
        height = sum(cell_heights) + gap * (row_count - 1) + 2 * margin
        positions = []
        for index, image in enumerate(images):
            row, column = divmod(index, column_count)
            x = x_offsets[column] + (cell_widths[column] - image.width) // 2
            y = y_offsets[row] + (cell_heights[row] - image.height) // 2
            positions.append((x, y))

    canvas = Image.new("RGB", (width, height), background)
    for image, position in zip(images, positions):
        canvas.paste(image.convert("RGB"), position)
    return canvas


def fit_image(image: Image.Image, max_width: float, max_height: float,
              dpi: int) -> Image.Image:
    """Scale the complete composite proportionally to its inch limits."""
    max_pixels_width = math.floor(max_width * dpi) if max_width > 0 else image.width
    max_pixels_height = math.floor(max_height * dpi) if max_height > 0 else image.height
    scale = min(1.0, max_pixels_width / image.width, max_pixels_height / image.height)
    target = (max(1, math.floor(image.width * scale)),
              max(1, math.floor(image.height * scale)))
    if target == image.size:
        return image
    resized = image.resize(target, Image.Resampling.LANCZOS)
    image.close()
    return resized


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("inputs", nargs="+", type=Path, help="Panel PNGs in display order")
    parser.add_argument("--output", required=True, type=Path, help="Combined Figure PNG")
    parser.add_argument("--layout", choices=("horizontal", "vertical", "grid"),
                        default="horizontal")
    parser.add_argument("--columns", type=int, default=2,
                        help="Grid columns; used only with --layout grid")
    parser.add_argument("--gap", type=int, default=24, help="Space between panels in pixels")
    parser.add_argument("--margin", type=int, default=24, help="Outer margin in pixels")
    parser.add_argument("--background", default="white", help="Canvas background color")
    parser.add_argument("--max-width", type=float, default=0.0,
                        help="Maximum complete-image width in inches; 0 is unconstrained")
    parser.add_argument("--max-height", type=float, default=0.0,
                        help="Maximum complete-image height in inches; 0 is unconstrained")
    parser.add_argument("--dpi", type=int, default=200)
    args = parser.parse_args()
    if args.gap < 0 or args.margin < 0:
        parser.error("--gap and --margin must be non-negative")
    if args.layout == "grid" and args.columns < 1:
        parser.error("--columns must be positive")
    if args.dpi < 1:
        parser.error("--dpi must be positive")
    if (not math.isfinite(args.max_width) or not math.isfinite(args.max_height) or
            args.max_width < 0 or args.max_height < 0):
        parser.error("--max-width and --max-height must be finite and non-negative")
    try:
        ImageColor.getrgb(args.background)
    except ValueError as exc:
        parser.error(f"invalid --background color: {exc}")
    return args


def main() -> None:
    args = parse_args()
    args.output.parent.mkdir(parents=True, exist_ok=True)
    background = ImageColor.getrgb(args.background)[:3]
    try:
        panels = []
        for path in args.inputs:
            with Image.open(path) as source:
                source.load()
                panels.append(source.convert("RGB"))
        combined = arrange_panels(
            panels, args.layout, args.columns, args.gap, args.margin, background)
        combined = fit_image(combined, args.max_width, args.max_height, args.dpi)
        with tempfile.TemporaryDirectory(prefix=".combine-panels-",
                                         dir=args.output.parent) as temp_dir:
            temporary_output = Path(temp_dir) / "combined.png"
            combined.save(temporary_output, format="PNG", dpi=(args.dpi, args.dpi))
            os.replace(temporary_output, args.output)
        print(f"Wrote {args.output}: {combined.width} x {combined.height} px at {args.dpi} dpi")
        combined.close()
        for panel in panels:
            panel.close()
    except (OSError, ValueError) as exc:
        raise SystemExit(f"ERROR: cannot combine panel images: {exc}") from exc


if __name__ == "__main__":
    main()
