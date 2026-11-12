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
- For panels intended for direct comparison, use compatible units, spatial
  extents, axis limits, and value normalization wherever the comparison
  requires them. Align corresponding axes and make panel labels unambiguous.
- Share a colorbar only when the panels use the same quantity, units, and
  color normalization. Do not let panel arrangement, resizing, or cropping
  distort geographic or other scientifically meaningful aspect ratios.

### Colorbar alignment

- For a colorbar associated with a single map or plot, its length must match
  the corresponding plotting axes.
- For vertical colorbars, align the top and bottom with the plotting area.
- For horizontal colorbars, align the left and right edges with the plotting
  area.
- Do not include titles, axis labels, or outer figure margins when determining
  the colorbar length.
- For multi-panel figures, a shared colorbar should span the intended group of
  plotting axes, rather than the entire figure canvas.
- Use layout methods that preserve this alignment when geographic aspect
  ratios, figure sizes, or output-resolution settings change.
- Verify the alignment in the saved output.

### Virtual colorbars for comparable panels

When panels compare the same geographic extent and some have a colorbar while
others do not, reserve identical colorbar space in every panel. A virtual
colorbar is reserved layout space with no visible colorbar, ticks, labels, or
frame; it must not change the meaning or normalization of the displayed data.

- Share explicit map/plot and colorbar axes geometry between the panel scripts.
  Keep map positions, dimensions, geographic extent and aspect, reserved
  colorbar space, and complete PNG canvas dimensions identical.
- Draw the real colorbar in its reserved axes. Hide those axes in panels
  without a colorbar, for example with `cbar_ax.set_visible(False)`.
- For fixed total height and automatic width, derive width from the map aspect,
  margins, and reserved colorbar space. Adding a real colorbar must not reduce
  the map height. Align its ends with the map axes and allow enough room for
  tick labels and titles. Apply any width limit to the complete layout
  proportionally.
- Preserve the full canvas during export and assembly. Do not let
  `bbox_inches="tight"`, automatic trimming, `tight_layout()`, or
  `constrained_layout` remove hidden space or move corresponding axes
  independently.
- Verify saved canvas dimensions and axes bounds in pixels, including top and
  bottom alignment, equal geographic pixel scale, invisible reserved regions,
  and uncropped, non-overlapping labels. Recheck supported height/width settings
  and the assembled Figure; visual inspection alone is insufficient.

The verified [p01-f02 implementation](river_map/README.md#virtual-colorbar-layout)
provides an example; its numeric margins are project-specific, not universal
defaults. This rule does not require adding colorbars to figures that have no
colorbar-related layout difference.

### Reproducibility and file ownership

- A user-facing script should generate one complete Figure. A Figure may
  contain multiple related panels; helper scripts and libraries may be used
  internally without being forced into the one-script/one-Figure interface.
- In a visualization project, keep the user-facing Shell entry point at the
  project root, editable input and display settings there, and Python or other
  plotting implementation under `script/`. This makes the run configuration
  easy to find while keeping plotting logic separate.
- Save the default output as PNG under the owning project's `figure/`
  directory. Git-track all project-local figure PNGs, including individual
  panels and assembled figures. Keep generated files with the visualization
  project or tutorial that owns them. Gallery publication is separate and
  requires explicit user authorization; normal figure generation must not
  copy or update Gallery images.
- Treat original model inputs as read-only in plotting workflows. Write
  transformed or derived datasets only when the project explicitly requires
  them, and document their provenance.
- Record enough information to reproduce the figure: inputs, relevant
  settings, script, and any important data transformations.

### Clear communication

- Provide an informative title or caption and label axes, units, legends, and
  colorbars where they help explain the plotted quantity.
- Begin each title shown inside a figure with its project and Figure IDs; when
  a title identifies an individual panel, include that panel ID as well. For
  example: `p01-f01a Raster River Map: Upstream Area` or
  `p01-f02a Line River Map: Connect Type`. Use human-readable
  method and option names with consistent capitalization, following the
  authoritative [naming convention](repository_development_guide.md#method-and-option-naming).
  Keep the figure identifiable outside the Gallery or project documentation.
- Use panel labels consistently when panels are referenced separately.
- Ensure lines, markers, categories, and overlaid geographic layers remain
  distinguishable at the image's intended display size.

## Recommended visual practices

These are starting points, not rigid design rules. Choose and explain a
different approach when the data or scientific question calls for it.

### Figure size and aspect ratio

When users need to control figure dimensions, user-facing Shell scripts should
offer independent `FIG_WIDTH`, `FIG_HEIGHT`, and `DPI` settings. Width and
height are the complete saved Figure dimensions in inches; `0` means automatic
sizing. Resolve the requested dimensions before final rendering and save at
the requested DPI. Include titles, labels, ticks, legends, colorbars, and
margins in the complete-Figure dimensions.

Support these sizing modes:

1. `FIG_HEIGHT > 0`, `FIG_WIDTH = 0`: keep the complete Figure height fixed and
   determine the width automatically. For map Figures, a vertical colorbar or
   legend should primarily add width, rather than reduce the map's height.
2. `FIG_WIDTH > 0`, `FIG_HEIGHT = 0`: keep the complete Figure width fixed and
   determine the height automatically. Horizontal decorations should add
   height rather than unnecessarily shrinking the plotting area.
3. Both dimensions greater than zero: fit the complete Figure in the requested
   box. Preserve scientific plotting-area aspect ratios; do not distort maps
   or stretch text.
4. Both dimensions zero: estimate readable dimensions from the map extent,
   panel arrangement, and required decorations.

Keep scientific and geographic aspect ratios. When one dimension is
automatic, use it to accommodate axes decorations while retaining the fixed
dimension. When both dimensions are fixed, keep the axes aspect correct inside
the available box. Avoid post-render raster resizing as the primary layout
method because it also scales text and line widths. Simple reusable Matplotlib
layout helpers are appropriate; do not create an elaborate sizing framework.
Pixel dimensions are rounded to the nearest pixel at the selected DPI, so
physical dimensions are exact to that output-pixel precision.

`FIG_WIDTH` and `FIG_HEIGHT` specify desired dimensions. Existing `MAX_WIDTH`
and `MAX_HEIGHT` or `--max-width` and `--max-height` settings, where retained
for compatibility, remain maximum bounding dimensions and may proportionally
resize a completed image. Do not silently reinterpret maximum bounds as exact
dimensions. New and updated Shell workflows should use `FIG_WIDTH` and
`FIG_HEIGHT`.

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
- For geographic maps, use consistent longitude and latitude major-tick
  intervals when practical. Choose intuitive intervals such as 0.5°, 1°, 2°,
  5°, or 10° for the map extent and figure size instead of relying entirely on
  automatic tick placement. Different intervals are fine when they prevent
  crowding or improve readability. Format coordinates consistently and make
  their units clear.

Do not impose arbitrary universal values for font sizes, line widths, image
dimensions, or panel layouts. Select them for the figure's content and review
the result at its expected display size.

## Evolving the guideline

The guideline should change as contributors learn from real figures and
reviews. When a new Gallery example reveals a practice useful across projects,
consider adding it here. Keep one-off figure preferences in that project's
documentation rather than turning every local choice into a repository-wide
rule.
