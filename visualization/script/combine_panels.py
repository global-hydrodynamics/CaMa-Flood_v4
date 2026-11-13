#!/usr/bin/env python3
"""Arrange rendered panel images into one complete Figure image."""

from __future__ import annotations

import argparse
import json
import math
import os
from pathlib import Path
import tempfile

from PIL import Image, ImageColor


def arrange_panels(images: list[Image.Image], layout: str, columns: int,
                   gap: int, margin: int, background: tuple[int, int, int],
                   map_layouts: list[dict] | None = None,
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
        if map_layouts is not None and layout != "grid":
            raise ValueError("map-area alignment is supported only for grid layouts")
        column_count = min(columns, len(images))
        row_count = math.ceil(len(images) / column_count)
        if map_layouts is not None:
            if len(map_layouts) != len(images) or any(item is None for item in map_layouts):
                raise ValueError("every panel must contain cama_map_layout PNG metadata")
            extents = [item["extent"] for item in map_layouts]
            reference_extent = extents[0]
            if any(len(extent) != 4 or any(
                    not math.isclose(value, reference, rel_tol=0, abs_tol=1e-9)
                    for value, reference in zip(extent, reference_extent))
                   for extent in extents[1:]):
                raise ValueError("map panels have different geographic extents")
            bounds = [item["bounds_px"] for item in map_layouts]
            widths = [item[2] for item in bounds]
            heights = [item[3] for item in bounds]
            if max(widths) - min(widths) > 0.1 or max(heights) - min(heights) > 0.1:
                raise ValueError("map panels have different physical map-area sizes")
            map_width, map_height = round(widths[0]), round(heights[0])
            left_margins = [0] * column_count
            right_margins = [0] * column_count
            top_margins = [0] * row_count
            bottom_margins = [0] * row_count
            for index, (image, box) in enumerate(zip(images, bounds)):
                row, column = divmod(index, column_count)
                x, y, _, _ = (round(value) for value in box)
                left_margins[column] = max(left_margins[column], x)
                right_margins[column] = max(
                    right_margins[column], image.width - x - map_width,
                )
                top_margins[row] = max(top_margins[row], y)
                bottom_margins[row] = max(
                    bottom_margins[row], image.height - y - map_height,
                )
            cell_widths = [left + map_width + right
                           for left, right in zip(left_margins, right_margins)]
            cell_heights = [top + map_height + bottom
                            for top, bottom in zip(top_margins, bottom_margins)]
            x_offsets = [margin]
            y_offsets = [margin]
            for cell_width in cell_widths[:-1]:
                x_offsets.append(x_offsets[-1] + cell_width + gap)
            for cell_height in cell_heights[:-1]:
                y_offsets.append(y_offsets[-1] + cell_height + gap)
            width = sum(cell_widths) + gap * (column_count - 1) + 2 * margin
            height = sum(cell_heights) + gap * (row_count - 1) + 2 * margin
            positions = []
            for index, box in enumerate(bounds):
                row, column = divmod(index, column_count)
                x, y = round(box[0]), round(box[1])
                positions.append((x_offsets[column] + left_margins[column] - x,
                                  y_offsets[row] + top_margins[row] - y))
        else:
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


def pad_image(image: Image.Image, figure_width: float, figure_height: float,
              dpi: int, background: tuple[int, int, int]) -> Image.Image:
    """Place the composition on an exact-size canvas without scaling panels."""
    target_width = math.floor(figure_width * dpi + 0.5) if figure_width > 0 else image.width
    target_height = math.floor(figure_height * dpi + 0.5) if figure_height > 0 else image.height
    if target_width < image.width or target_height < image.height:
        raise ValueError(
            "panel composition exceeds the requested figure dimensions; "
            "increase FIG_WIDTH/FIG_HEIGHT or reduce panel spacing"
        )
    if (target_width, target_height) == image.size:
        return image
    canvas = Image.new("RGB", (target_width, target_height), background)
    canvas.paste(image, ((target_width - image.width) // 2,
                         (target_height - image.height) // 2))
    image.close()
    return canvas


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("inputs", nargs="+", type=Path, help="Panel PNGs in display order")
    parser.add_argument("--output", required=True, type=Path, help="Combined Figure PNG")
    parser.add_argument("--layout", choices=("horizontal", "vertical", "grid"),
                        default="horizontal")
    parser.add_argument("--columns", type=int, default=2,
                        help="Grid columns; used only with --layout grid")
    parser.add_argument("--align-map-areas", action="store_true",
                        help="Align map axes from cama_map_layout PNG metadata; grid only")
    parser.add_argument("--gap", type=int, default=24, help="Space between panels in pixels")
    parser.add_argument("--margin", type=int, default=24, help="Outer margin in pixels")
    parser.add_argument("--background", default="white", help="Canvas background color")
    parser.add_argument("--figure-width", type=float, default=0.0,
                        help="Exact complete-Figure width in inches; 0 is automatic")
    parser.add_argument("--figure-height", type=float, default=0.0,
                        help="Exact complete-Figure height in inches; 0 is automatic")
    # Keep the original post-render bounds available for backward compatibility.
    parser.add_argument("--max-width", type=float, default=0.0,
                        help="Legacy maximum complete-image width in inches")
    parser.add_argument("--max-height", type=float, default=0.0,
                        help="Legacy maximum complete-image height in inches")
    parser.add_argument("--dpi", type=int, default=200)
    args = parser.parse_args()
    if args.gap < 0 or args.margin < 0:
        parser.error("--gap and --margin must be non-negative")
    if args.layout == "grid" and args.columns < 1:
        parser.error("--columns must be positive")
    if args.align_map_areas and args.layout != "grid":
        parser.error("--align-map-areas requires --layout grid")
    if args.dpi < 1:
        parser.error("--dpi must be positive")
    if (not math.isfinite(args.max_width) or not math.isfinite(args.max_height) or
            args.max_width < 0 or args.max_height < 0):
        parser.error("--max-width and --max-height must be finite and non-negative")
    if (not math.isfinite(args.figure_width) or not math.isfinite(args.figure_height) or
            args.figure_width < 0 or args.figure_height < 0):
        parser.error("--figure-width and --figure-height must be finite and non-negative")
    if ((args.figure_width > 0 or args.figure_height > 0) and
            (args.max_width > 0 or args.max_height > 0)):
        parser.error("--figure-* cannot be combined with legacy --max-* bounds")
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
        map_layouts = []
        for path in args.inputs:
            with Image.open(path) as source:
                source.load()
                raw_layout = source.info.get("cama_map_layout")
                map_layouts.append(json.loads(raw_layout) if raw_layout else None)
                panels.append(source.convert("RGB"))
        combined = arrange_panels(
            panels, args.layout, args.columns, args.gap, args.margin, background,
            map_layouts if args.align_map_areas else None)
        if args.figure_width > 0 or args.figure_height > 0:
            combined = pad_image(combined, args.figure_width, args.figure_height,
                                 args.dpi, background)
        else:
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
