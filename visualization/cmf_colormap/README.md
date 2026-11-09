# CMF Colormaps

CMF Colormaps is a small, reusable Python library for palettes developed for
CaMa-Flood visualizations. The initial map, **Shade-Rainbow**, is a sequential,
multihue colormap designed to make low values visually muted and high values
more prominent. It is intended as an additional option, not a universal
replacement for existing palettes.

## Install

From a CaMa-Flood checkout:

```bash
python -m pip install -e ./visualization/cmf_colormap
```

The package can also be installed from a standalone clone of this directory:

```bash
python -m pip install -e .
```

Matplotlib is the only runtime dependency. NumPy is used by the gallery example
and is already a Matplotlib dependency.

The package is distributed under Apache License 2.0; see [`LICENSE`](LICENSE).

## Use

```python
import matplotlib.pyplot as plt
from matplotlib.colors import LogNorm

from cmf_colormap import get_cmap

cmap = get_cmap("shade_rainbow")
image = ax.pcolormesh(lon, lat, discharge, cmap=cmap,
                      norm=LogNorm(vmin=5, vmax=1_000_000, clip=True))
fig.colorbar(image, ax=ax, label="Discharge (m³/s)")
```

The package does not select a variable, unit, or normalization range. Choose a
normalization appropriate to the plotted quantity: `Normalize` for a linear
scale, `LogNorm` for positive values spanning orders of magnitude, or another
Matplotlib norm where appropriate. The `LogNorm` range above is only an example.

Available maps come from the package registry:

```python
from cmf_colormap import get_cmap, list_colormaps, register_colormaps

print(list_colormaps())  # ['shade_rainbow']
cmap = get_cmap("shade_rainbow")

# Optional global Matplotlib lookup as "cmf.shade_rainbow".
register_colormaps()
```

Importing the package does not modify Matplotlib's global registry. Registration
is explicit and safe to repeat. A pre-existing unrelated `cmf.<name>` entry is
reported as a collision. Unknown names raise `ValueError` and list registered
choices.

## Shade-Rainbow definition

The authoritative colors and metadata are in
[`src/cmf_colormap/data/shade_rainbow.json`](src/cmf_colormap/data/shade_rainbow.json).
Twelve controls are evenly spaced on normalized `[0, 1]`; adjacent RGB channel
values are interpolated piecewise-linearly in numerical sRGB values. There is no
HSV, HSL, Lab, perceptual-space, or gamma-corrected light interpolation. The
first and last control colors are the exact endpoints. HSV values in the detail
preview are derived for diagnostics only; they do not define the palette.

Matplotlib exposes finite lookup tables for colormap objects. This adapter uses
131,072 entries to keep lookup sampling fine; the JSON interpolation rule remains
the mathematical source of truth.

## Previews

The registry-driven gallery shows each available map on a normalized 0–1 axis.
The detailed Shade-Rainbow image includes control HEX/HSV diagnostics,
background comparisons, HSV component strips and curves, and an example
log-discharge axis. Its discharge values are illustrative only.

![CMF colormap gallery](examples/colormap_gallery.png)

![Shade-Rainbow detailed preview](examples/shade_rainbow_detailed.png)

Regenerate both images after changing or adding a registry entry:

```bash
python examples/plot_colormap_gallery.py
```

Install the test dependency and run the focused tests from this package
directory:

```bash
python -m pip install -e '.[test]'
python -m unittest discover -s tests -v
```

The plotting program reads definitions from package data and writes preview PNGs
beside itself. It does not maintain separate color definitions.

## Add a colormap

1. Add a JSON definition under `src/cmf_colormap/data/` with a unique name,
   version, type, interpolation rule, description, and named HEX controls.
2. Add one row to `colormaps.csv`, pointing to that JSON file.
3. Keep units and data normalization outside the definition.
4. Add focused definition/API tests and regenerate the gallery.

The implementation currently accepts `linear_srgb` definitions with at least
two named HEX controls. Add other interpolation methods only with an explicit
definition and matching tests.

## Limitations

Shade-Rainbow is not designed to be perceptually uniform: equal normalized
steps need not appear equally different. Rainbow-like transitions can also be
difficult for some color-vision deficiencies and may imply boundaries that are
not present in the data. Consider the data, background, audience, and alternative
palettes when choosing a map, and do not interpret the color sequence as a
scientific classification.

## Development and subtree synchronization

This directory is both the package location in CaMa-Flood and the root of the
standalone [`global-hydrodynamics/cmf_colormap`](https://github.com/global-hydrodynamics/cmf_colormap)
repository. CaMa-Flood integrates it with Git subtree at the prefix
`visualization/cmf_colormap`; no submodule is needed by CaMa-Flood users. The
standalone repository uses `master` as its default branch.

### Initial subtree import

The package was first developed and committed inside CaMa-Flood. For the first
publication, export its path history, rejoin the split history to the existing
CaMa-Flood branch, then push the exported commit to the empty standalone
`master` branch. `--rejoin` is important here: the initial split by itself does
not connect the standalone history to the CaMa-Flood branch for a later subtree
pull. Rejoining adds a merge commit to the CaMa-Flood branch; it does not rewrite
or replace existing commits.

From the CaMa-Flood repository:

1. Commit package changes on the approved CaMa-Flood development branch. Subtree
   operations include committed content only.
2. Add the standalone remote locally if it is not already configured:

   ```bash
   git remote add cmf-colormap git@github.com:global-hydrodynamics/cmf_colormap.git
   ```

3. Split and rejoin the subtree history, then publish that split to the
   standalone default branch. This direct push is for the initial import into
   an empty repository only; do not force-push.

   ```bash
   git subtree split --rejoin --prefix=visualization/cmf_colormap
   git subtree push --prefix=visualization/cmf_colormap cmf-colormap master
   ```

The independent repository must still be empty before the initial push. If it
already has commits or files, stop and agree on a history integration plan.
Never use `git subtree add` over the existing package directory, and do not
squash, rebase, or force-push shared history.

### Routine synchronization

Choose one repository for a change and synchronize before editing the other.
Commit all package changes first; subtree operations do not include uncommitted
files. Do not independently edit both copies and reconcile them later.

From CaMa-Flood, pull standalone changes:

```bash
git subtree pull --prefix=visualization/cmf_colormap cmf-colormap master
```

To send CaMa-Flood changes back, push the subtree split to standalone `master`:

```bash
git subtree push --prefix=visualization/cmf_colormap cmf-colormap master
```

These are non-squashed subtree operations. Synchronize before editing in the
other repository, and do not independently edit both copies. Never synchronize
with uncommitted package changes. If a pull reports conflicts, resolve them as
an ordinary merge, run the package tests, and commit the merge before
continuing. If a standalone change is based on history not yet present in the
CaMa-Flood subtree, first inspect the histories; do not force-push or use
automatic conflict resolution.

### Standalone continuous integration

The standalone repository runs the package unit tests on pushes and pull
requests. The workflow installs the package and its test dependencies from
`pyproject.toml`; it does not need CaMa-Flood model data or executables.
