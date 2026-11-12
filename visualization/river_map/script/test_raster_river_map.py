"""Verify all four f01 scientific fields, binary orientation and shared geometry."""

from __future__ import annotations

import contextlib
import csv
import io
from pathlib import Path
import runpy
import sys
import struct
import tempfile
import unittest
from unittest.mock import patch

import matplotlib
matplotlib.use("Agg")
import numpy as np
from matplotlib.colors import BoundaryNorm, LogNorm
from matplotlib.collections import QuadMesh
from netCDF4 import Dataset
from PIL import Image

import river_network_layout as layout


ROOT = Path(__file__).resolve().parents[3]
SCRIPTS = Path(__file__).resolve().parent
MAP = ROOT / "map/mekong_15min"


class RasterRiverMapTest(unittest.TestCase):
    def test_subbasin_binary_contract(self):
        reader = runpy.run_path(str(SCRIPTS / "f01d-raster_river_map-subbasin_color.py"))["read_subbasin_colors"]
        geometry = (3, 2, 1, 90, 93, 5, 7)
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "subcol.bin"
            # First Fortran row is the northern row; x increases eastward.
            payload = struct.pack("<6i", 2, 3, 4, 5, 0, -9999)
            path.write_bytes(payload)
            data = reader(path, geometry)
            np.testing.assert_array_equal(data.data, [[2, 3, 4], [5, 0, -9999]])
            np.testing.assert_array_equal(data.mask, [[False, False, False], [False, True, True]])
            for invalid in (payload[:-1], payload + b"\0", struct.pack(">6i", 2, 3, 4, 5, 0, -9999)):
                path.write_bytes(invalid)
                with self.assertRaises(ValueError):
                    reader(path, geometry)

    def test_data_and_layout(self):
        with (ROOT / "visualization/visualization_catalog.csv").open() as source:
            catalog = {row["visualization_id"]: row for row in csv.DictReader(source)}
        for mode, height, width in (("max", 6, 0), ("max", 4, 0), ("max", 6, 3),
                                    ("figure", 3.79, 0), ("figure", 0, 4),
                                    ("figure", 4, 4), ("figure", 0, 0)):
            with self.subTest(mode=mode, height=height, width=width), tempfile.TemporaryDirectory() as tmp:
                bounds = []
                palettes = {}
                for panel, option, variable in (("a", "upstream_area", "uparea"),
                                                ("b", "basin", "basin"),
                                                ("c", "basin_color", "bsncol"),
                                                ("d", "subbasin_color", "subcol")):
                    name = f"f01{panel}-raster_river_map-{option}"
                    output = Path(tmp) / (name + ".png")
                    argv = [str(SCRIPTS / (name + ".py")),
                            "--input", str(MAP / "subcol-30000.bin" if variable == "subcol"
                                           else MAP / "netcdf" / (variable + ".nc")),
                            "--params", str(MAP / "params.txt"),
                            "--west", "90", "--east", "110", "--south", "5", "--north", "35",
                            f"--{mode}-height", str(height), f"--{mode}-width", str(width),
                            "--dpi", "100", "--output", str(output)]
                    if variable == "subcol":
                        argv += ["--basin", str(MAP / "netcdf/basin.nc"),
                                 "--nextxy", str(MAP / "netcdf/nextxy_noedge.nc")]
                    original_save = layout.save_network_panel

                    def inspect(fig, ax, cax, args):
                        image = ax.images[0]
                        if variable == "subcol":
                            # Independent Fortran (NX,NY) view checks the real
                            # sample's orientation and original cell values.
                            data = np.ndarray((80, 120), dtype="<i4", buffer=args.input.read_bytes(), order="F").T
                            raw = np.ma.masked_where(data <= 0, data)
                        else:
                            with Dataset(args.input) as source:
                                raw = np.ma.asarray(source[variable][:], dtype=float)
                        expected = raw / 1_000_000 if variable == "uparea" else raw
                        np.testing.assert_array_equal(image.get_array(), expected)
                        np.testing.assert_array_equal(np.ma.getmaskarray(image.get_array()),
                                                      np.ma.getmaskarray(raw))
                        self.assertEqual(ax.get_xlim(), (90, 110))
                        self.assertEqual(ax.get_ylim(), (5, 35))
                        self.assertEqual(ax.get_aspect(), 1)
                        if variable == "uparea":
                            self.assertIsInstance(image.norm, LogNorm)
                            self.assertEqual(image.get_cmap().name, "YlGnBu")
                            self.assertEqual(image.norm.vmin, expected.min())
                            self.assertEqual(image.norm.vmax, expected.max())
                        else:
                            self.assertIsInstance(image.norm, BoundaryNorm)
                            if variable == "basin":
                                self.assertEqual(len(np.unique(image.get_array().compressed())), 414)
                                self.assertIn("categorical", cax.get_ylabel())
                                labels = [tick.get_text() for tick in cax.get_yticklabels()]
                                self.assertEqual(labels, [str(n) for n in
                                    [*range(1, 21), 30, 40, 50, 70, 100, 150, 200, 300, 414]])
                                legend = cax.collections[0]
                                ids = np.arange(1, 415)
                                np.testing.assert_array_equal(legend.get_array().ravel(), ids)
                                np.testing.assert_array_equal(
                                    legend.get_cmap()(legend.norm(ids)),
                                    image.get_cmap()(image.norm(ids)),
                                )
                                edges = legend.get_coordinates()[:, 0, 1]
                                np.testing.assert_allclose(np.diff(edges[:21]), 1)
                                self.assertTrue(np.all(np.diff(edges[20:]) < 1))
                                self.assertEqual(cax.get_yscale(), "linear")
                            else:
                                indices = np.unique(expected.compressed())
                                colors = image.get_cmap()(image.norm(indices))[:, :3]
                                self.assertEqual(len(indices), 5)
                                # Quantify useful opacity/lightness and separation
                                # for the actual neighboring-color index palette.
                                self.assertTrue(np.all(colors @ [0.2126, 0.7152, 0.0722] > 0.70))
                                distances = np.linalg.norm(colors[:, None] - colors[None, :], axis=2)
                                self.assertGreater(distances[np.triu_indices(len(colors), 1)].min(), 0.10)
                                np.testing.assert_equal(image.get_cmap()(image.norm(indices))[:, 3], 1)
                                palettes[variable] = image.get_cmap()(image.norm(np.arange(1, 11)))
                                if variable == "subcol":
                                    self.assertEqual(cax.get_ylabel(), "Subbasin display-color index (not subbasin ID)")
                                    self.assertEqual([label.get_text() for label in cax.get_yticklabels()],
                                                     ["2", "3", "4", "5", "6"])
                                    self.assertEqual(image.get_interpolation(), "nearest")
                                    rgba = image.to_rgba(image.get_array())
                                    self.assertTrue(np.all(rgba[np.ma.getmaskarray(raw)] == [1, 1, 1, 1]))
                        row = catalog[f"p01-f01{panel}"]
                        self.assertEqual(row["option_name"], option)
                        self.assertEqual(ax.get_title().replace("\n", " "),
                                         row["visualization_id"] + " " + row["title"])
                        size = original_save(fig, ax, cax, args)
                        renderer = fig.canvas.get_renderer()
                        if variable == "subcol":
                            legend = next(item for item in cax.collections if isinstance(item, QuadMesh))
                            np.testing.assert_array_equal(legend.get_array().ravel(), indices)
                            np.testing.assert_array_equal(legend.get_facecolors(), image.get_cmap()(image.norm(indices)))
                        if variable == "basin":
                            boxes = [label.get_window_extent(renderer) for label in cax.get_yticklabels()]
                            for lower, upper in zip(boxes, boxes[1:]):
                                self.assertLess(lower.y1, upper.y0)
                        np.testing.assert_allclose(ax.get_window_extent().bounds[1::2],
                                                   cax.get_window_extent().bounds[1::2], atol=1e-8)
                        for axes in (ax, cax):
                            box = axes.get_tightbbox(renderer)
                            self.assertGreaterEqual(box.x0, 0)
                            self.assertGreaterEqual(box.y0, 0)
                            self.assertLessEqual(box.x1, fig.bbox.width)
                            self.assertLessEqual(box.y1, fig.bbox.height)
                        with Image.open(output) as rendered:
                            self.assertEqual(rendered.size, size)
                        bounds.append((size, ax.get_position().bounds,
                                       ax.transData.transform([(90, 5), (110, 35)])))
                        return size

                    with patch.object(sys, "argv", argv), patch.object(
                        layout, "save_network_panel", inspect
                    ), contextlib.redirect_stdout(io.StringIO()):
                        runpy.run_path(argv[0], run_name="__main__")
                for other in bounds[1:]:
                    self.assertEqual(other[0], bounds[0][0])
                    np.testing.assert_allclose(other[1], bounds[0][1], atol=1e-10)
                    np.testing.assert_allclose(other[2], bounds[0][2], atol=1e-8)
                np.testing.assert_array_equal(palettes["bsncol"], palettes["subcol"])
                if mode == "figure":
                    if height:
                        self.assertAlmostEqual(bounds[0][0][1], height * 100, delta=1)
                    if width:
                        self.assertAlmostEqual(bounds[0][0][0], width * 100, delta=1)
                elif not width:
                    self.assertEqual(bounds[0][0][1], height * 100)
                else:
                    self.assertLessEqual(bounds[0][0][0], width * 100)


if __name__ == "__main__":
    unittest.main()
