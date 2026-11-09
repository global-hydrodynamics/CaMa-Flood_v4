# CaMa-Flood visualization tools

This directory contains reusable visualization components and examples. The
[`cmf_colormap/`](cmf_colormap/README.md) package provides four registered
scientific palettes. Shade-Rainbow, White-Rainbow, and Diverge-Rainbow are
primarily foreground palettes for discharge, nonnegative errors, and signed
errors or anomalies, respectively. Pale-Terrain is a subtle topographic
background. The collection follows an overlay-aware design philosophy for
layered maps of rivers, basins, floodplains, and related geospatial information.

Install it from the CaMa-Flood repository root with:

```bash
python -m pip install -e ./visualization/cmf_colormap
```

Tutorial02 demonstrates Shade-Rainbow in its annual maximum discharge figure.
The package documentation describes the intended role and limitations of each
palette. It is self-contained and can also be extracted for standalone use.
