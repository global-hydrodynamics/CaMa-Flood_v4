# CaMa-Flood Figure-Making Guideline

This guide sets shared expectations for scientific figures developed across
the CaMa-Flood repository, including tutorials and manuals. It distinguishes
requirements that protect scientific meaning and reproducibility from visual
practices that can evolve as the Gallery grows.

## Mandatory principles

### Scientific correctness

- Plot the intended variable with its verified physical meaning and units.
- Distinguish snapshots, interval means, accumulations, maxima, and other
  temporal definitions in labels, captions, or documentation when relevant.
- Handle missing values, invalid cells, and masks according to the data
  contract. Do not silently treat missing values as zero or as valid data.
- Do not alter scientific input values merely to improve appearance. Any
  aggregation, normalization, clipping, threshold, or transformation that
  affects interpretation must be explicit and justified.
- Ensure the visualization supports the stated scientific question. A
  plausible-looking image is not evidence that its interpretation is correct.

### Comparability

- Use consistent geographic extents and scales when panels are intended for
  direct comparison.
- Use common color limits for comparable quantities unless a different scale
  is necessary; clearly explain such exceptions.
- Distinguish absolute fields from anomalies, differences, ratios, or other
  derived quantities. Make the reference experiment and subtraction order
  explicit for differences.
- Make the meaning of zero and the direction of positive and negative values
  clear for signed quantities.

### Reproducibility and file ownership

- A user-facing script should generate one complete Figure. A Figure may
  contain multiple related panels; helper scripts and libraries may be used
  internally without being forced into the one-script/one-Figure interface.
- Save the default output as PNG under the owning project's `figure/`
  directory. Keep generated files with the visualization project or tutorial
  that owns them.
- Treat original model inputs as read-only in plotting workflows. Write
  transformed or derived datasets only when the project explicitly requires
  them, and document their provenance.
- Record enough information to reproduce the figure: inputs, relevant
  settings, script, and any important data transformations.

### Clear communication

- Provide an informative title or caption and label axes, units, legends, and
  colorbars where they help explain the plotted quantity.
- Use panel labels consistently when panels are referenced separately.
- Ensure lines, markers, categories, and overlaid geographic layers remain
  distinguishable at the image's intended display size.

## Recommended visual practices

These are starting points, not rigid design rules. Choose and explain a
different approach when the data or scientific question calls for it.

- Use sequential color maps for ordered nonnegative quantities, diverging
  maps for signed quantities with a meaningful center, and categorical colors
  for classes without an ordered magnitude.
- Choose normalization and limits to match the data distribution and
  scientific question. Do not select them only to make a preferred pattern
  more prominent.
- Check contrast against backgrounds and overlays, especially for river
  networks, water bodies, stations, basin boundaries, and terrain.
- Reuse a suitable palette from [`cmf_colormap/`](cmf_colormap/README.md)
  when it fits the data and background. Its design philosophy and limitations
  are documented with the package; it is one resource, not a universal
  requirement.
- Reduce clutter and unnecessary decoration. Consider color-vision
  accessibility and whether a figure remains understandable in print or
  when resized.
- Keep related panels visually consistent, while allowing a clearly stated
  exception when it improves interpretation.

Do not impose arbitrary universal values for font sizes, line widths, image
dimensions, or panel layouts. Select them for the figure's content and review
the result at its expected display size.

## Evolving the guideline

The guideline should change as contributors learn from real figures and
reviews. When a new Gallery example reveals a practice useful across projects,
consider adding it here. Keep one-off figure preferences in that project's
documentation rather than turning every local choice into a repository-wide
rule.
