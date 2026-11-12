"""Verify the three f01 scientific fields, soft colors and exported geometry."""

from __future__ import annotations

import contextlib
import csv
import io
from pathlib import Path
import runpy
import sys
import tempfile
import unittest
from unittest.mock import patch

import matplotlib
matplotlib.use("Agg")
import numpy as np
from matplotlib.colors import BoundaryNorm, LogNorm
from netCDF4 import Dataset
from PIL import Image

import river_network_layout as layout


ROOT = Path(__file__).resolve().parents[3]
SCRIPTS = Path(__file__).resolve().parent
MAP = ROOT / "map/mekong_15min"


class RasterRiverMapTest(unittest.TestCase):
    def test_data_and_layout(self):
        with (ROOT / "visualization/visualization_catalog.csv").open() as source:
            catalog = {row["visualization_id"]: row for row in csv.DictReader(source)}
        for height, width in ((6, 0), (4, 0), (6, 3)):
            with self.subTest(height=height, width=width), tempfile.TemporaryDirectory() as tmp:
                bounds = []
                for panel, option, variable in (("a", "upstream_area", "uparea"),
                                                ("b", "basin", "basin"),
                                                ("c", "basin_color", "bsncol")):
                    name = f"f01{panel}-raster_river_map-{option}"
                    output = Path(tmp) / (name + ".png")
                    argv = [str(SCRIPTS / (name + ".py")),
                            "--input", str(MAP / "netcdf" / (variable + ".nc")),
                            "--params", str(MAP / "params.txt"),
                            "--west", "90", "--east", "110", "--south", "5", "--north", "35",
                            "--max-height", str(height), "--max-width", str(width),
                            "--dpi", "100", "--output", str(output)]
                    original_save = layout.save_network_panel

                    def inspect(fig, ax, cax, args):
                        image = ax.images[0]
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
                        row = catalog[f"p01-f01{panel}"]
                        self.assertEqual(row["option_name"], option)
                        self.assertEqual(ax.get_title().replace("\n", " "),
                                         row["visualization_id"] + " " + row["title"])
                        size = original_save(fig, ax, cax, args)
                        renderer = fig.canvas.get_renderer()
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
                if not width:
                    self.assertEqual(bounds[0][0][1], height * 100)
                else:
                    self.assertLessEqual(bounds[0][0][0], width * 100)


if __name__ == "__main__":
    unittest.main()
