"""Check real f02 exports for equal map geometry and retained hidden space."""

from __future__ import annotations

import contextlib
import io
from pathlib import Path
import runpy
import sys
import tempfile
import unittest
from unittest.mock import patch

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
from PIL import Image

import river_network_layout as layout


ROOT = Path(__file__).resolve().parents[3]
SCRIPTS = Path(__file__).resolve().parent
MAP = ROOT / "map/mekong_15min"


class NetworkLayoutTest(unittest.TestCase):
    def test_real_panels(self):
        # Real labels, logarithmic ticks, raster and arrows catch layout drift
        # that empty axes or a test of layout constants alone would miss.
        for height, width in ((0, 0), (4, 0), (6, 0), (8, 0), (6, 4)):
            with self.subTest(height=height, width=width), tempfile.TemporaryDirectory() as tmp:
                panels = []
                for suffix in ("a-line_river_map-connectivity", "b-line_river_map-uparea"):
                    output = Path(tmp) / f"f02{suffix}.png"
                    argv = [str(SCRIPTS / f"f02{suffix}.py"),
                            "--uparea", str(MAP / "netcdf/uparea.nc"),
                            "--nextxy", str(MAP / "netcdf/nextxy_noedge.nc"),
                            "--params", str(MAP / "params.txt"),
                            "--west", "98", "--east", "110", "--south", "8", "--north", "20",
                            "--dpi", "100", "--max-height", str(height),
                            "--max-width", str(width), "--output", str(output)]
                    original_save = layout.save_network_panel

                    def inspect(fig, ax, cax, args):
                        size = original_save(fig, ax, cax, args)
                        renderer = fig.canvas.get_renderer()
                        np.testing.assert_allclose(ax.get_window_extent().bounds[1::2],
                                                   cax.get_window_extent().bounds[1::2], atol=1e-8)
                        for axes in (ax, cax) if cax.get_visible() else (ax,):
                            bounds = axes.get_tightbbox(renderer)
                            self.assertGreaterEqual(bounds.x0, 0)
                            self.assertGreaterEqual(bounds.y0, 0)
                            self.assertLessEqual(bounds.x1, fig.bbox.width)
                            self.assertLessEqual(bounds.y1, fig.bbox.height)
                            labels = [tick.label1 for tick in axes.yaxis.get_major_ticks()
                                      if tick.label1.get_visible() and
                                      axes.get_ylim()[0] <= tick.get_loc() <= axes.get_ylim()[1]]
                            boxes = sorted((label.get_window_extent(renderer) for label in labels),
                                           key=lambda box: box.y0)
                            for below, above in zip(boxes, boxes[1:]):
                                self.assertLessEqual(below.y1, above.y0)
                        with Image.open(output) as image:
                            self.assertEqual(image.size, size)
                            if not cax.get_visible():
                                x, y, w, h = cax.get_position().bounds
                                region = image.convert("RGB").crop((
                                    round(x * size[0]), round((1-y-h) * size[1]),
                                    size[0], round((1-y) * size[1])))
                                self.assertTrue(np.all(np.asarray(region) == 255))
                        panels.append((size, ax.get_position().bounds,
                                       ax.transData.transform([(98, 8), (110, 20)])))
                        print(f"height={height}, width={width}, panel={suffix[0]}, "
                              f"canvas={size}, map={ax.get_position().bounds}", file=sys.__stdout__)
                        return size

                    with patch.object(sys, "argv", argv), patch.object(
                        layout, "save_network_panel", inspect
                    ), contextlib.redirect_stdout(io.StringIO()):
                        # Export must override even a caller's tight-crop setting.
                        with plt.rc_context({"savefig.bbox": "tight"}):
                            runpy.run_path(argv[0], run_name="__main__")
                self.assertEqual(panels[0][0], panels[1][0])
                np.testing.assert_allclose(panels[0][1], panels[1][1], atol=1e-10)
                np.testing.assert_allclose(panels[0][2], panels[1][2], atol=1e-8)
                if height and not width:
                    self.assertAlmostEqual(panels[0][0][1], height * 100, delta=1)
                if width:
                    self.assertLessEqual(panels[0][0][0], width * 100)


if __name__ == "__main__":
    unittest.main()
