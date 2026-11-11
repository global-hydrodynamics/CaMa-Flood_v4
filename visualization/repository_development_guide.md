# Developing Visualizations in CaMa-Flood

This guide explains how researchers, students, and AI agents can propose,
develop, review, and share reusable CaMa-Flood visualizations. Contributors
can participate at different levels: an idea or sample image is a useful
starting point even when no code is available.

## How the visualization resources fit together

| Resource | Role |
| --- | --- |
| Visualization project | A descriptive directory grouping related plotting methods, such as river maps or hydrographs. Project names do not contain numeric IDs. |
| Figure project entry point | The user-facing Shell script at the project root. It exposes editable input and display settings and invokes the implementation. |
| Figure implementation | Python or other plotting/data-processing code under the project's `script/` directory. For the preferred multi-panel workflow, each panel has its own script and PNG; a shared utility assembles the complete Figure. |
| Project Figure (`<project>/figure/`) | Generated panel PNGs and assembled Figures. These reproducible project assets are always Git-tracked and may be updated during development. Match descriptive basenames, for example `f02a-seasonal-map.py`, `figure/f02a-seasonal-map.png`, and `figure/f02-seasonal-summary.png`. |
| Published Gallery Figure (`visualization/gallery/`) | A separately selected copy of a Project Figure. Add, replace, or remove it only when the user explicitly authorizes Gallery publication. Routine generation, review, or `Tested` status is not authorization. |
| `visualization/README.md` | The visual Gallery: approved images, figure IDs and titles, one-sentence descriptions, and links to each script and the relevant project README section. |
| Project `README.md` | User-facing documentation for the whole visualization project, with common execution instructions and a stable, descriptive section for each Figure. |
| `visualization_catalog.csv` | A discovery index with one row per panel or visualization option. It records purpose, variables, data source and preparation, method, script, status, and publication reference. |
| `figure_making_guideline.md` | Shared scientific-correctness, comparability, readability, and reproducibility principles. |
| `cmf_colormap/` | An existing reusable palette package. It may support projects, but it is not itself a required plotting framework or a Gallery project. |

The catalog is intended for human and AI discovery, including searches by
natural-language descriptions of a scientific need. It is a reference, not a
runtime dependency: tutorials may adapt a method without importing project
scripts directly.

## Visualization documentation structure

Maintain two levels of human-facing documentation. Keep
`visualization/README.md` focused on browsing approved figures. Each entry
shows the Figure ID and descriptive title, published Gallery image, one
concise description, a link to the project's user-facing Shell script, and a
link to the corresponding section of the project README. Do not put detailed
technical explanations in the Gallery.

Maintain one README per visualization project, not one document per Figure.
The project README gives a short project overview and common execution
instructions, followed by a stable, descriptive heading for every Figure. Each
Figure section explains its scientific purpose and interpretation, required
datasets/variables/units, important processing and display methods, settings
and execution, and relevant limitations; place that Figure's project-local
image immediately after its explanation. Extend the existing project README
when adding figures rather than creating individual Figure documents or a
separate project-gallery section. Keep the top-level Gallery and CSV catalog
concise; link to the project README for full explanations.

A project README should embed its own tracked figures from that project's
`figure/` directory immediately after the corresponding Figure explanation.
These project Figures are distinct from the user-authorized copies shown in
the top-level Gallery.

## Visualization Data Policy

Prefer existing Git-tracked CaMa-Flood datasets when they are scientifically
suitable. This makes examples easier to reproduce without additional downloads
or model runs. If a visualization depends on expensive preprocessing,
multi-step simulations, or large outputs, consider publishing a compact,
precomputed reference dataset with the example. Describe its source,
derivation, scope, and any access or redistribution limits.

Global and high-resolution map products may be distributed separately from the
GitHub repository. Use them when the science requires them, and state where
they are available, which products are needed, and what preparation is
required. Do not assume that a user has completed another tutorial or has its
generated outputs. In particular, general-user visualization examples must not
depend on `gosh_test/` outputs; that directory supports developer model tests
and operational verification.

For every visualization example, document its required input datasets, the
variables it uses, whether each input is Git-tracked, separately distributed,
external, or generated, and how users can obtain or prepare it. Record this
information in `visualization_catalog.csv` using `data_source`,
`required_data`, and `data_preparation`, and link detailed instructions from
the project documentation when needed.

## Propose a visualization

A proposal may be any of the following:

- a scientific visualization idea and a short explanation of why it is useful;
- an example image from a paper, report, or existing research workflow;
- a current CaMa-Flood figure that could be clearer or more informative;
- a plotting script or a partly developed example.

No working code is required to start a proposal. Include the scientific
question, the data or variables involved if known, what a reader should learn
from the figure, and any important comparisons or overlays. Treat external
images as references, not automatically as approved Gallery material; record
their source and permissions before redistribution.

## Develop a visualization

Use this sequence as a guide; review may lead back to an earlier step.

1. Search `visualization_catalog.csv` for related methods by description,
   purpose, variables, and method. Read the linked scripts and documentation
   when they exist.
2. Define the scientific question, intended audience, variables, units,
   temporal meaning, spatial extent, and comparison being shown.
3. Select an existing descriptive visualization project or create a new one
   with a project-root user entry point and `script/` and `figure/`
   subdirectories. Do not create an empty example project just to reserve an ID.
4. Assign the next available project and Figure IDs and decide whether the
   Figure needs panel IDs. IDs are catalog references and may be renumbered;
   descriptive directory and script names must stand on their own.
5. Put the user-facing Shell entry point at the project root, with editable
   input and display settings, and put Python/data/plotting implementation under
   `script/`. Give the entry point and PNG output matching descriptive
   basenames. Keep scientific inputs read-only, write working outputs to the
   owning project's `figure/`, and use PNG by default. Follow the
   [Visualization Data Policy](#visualization-data-policy) when choosing and
   documenting inputs. The Shell entry point may invoke Python or shared
   libraries; it should not duplicate complex plotting logic.
6. Generate and inspect the Figure. Check scientific semantics, data masks,
   units, scales, readability, overlays, and the intended interpretation.
   Record important settings and transformations.
7. Add or update one catalog row per panel or option. Leave unknown
   information blank rather than guessing. Use the status values below.
8. Keep every generated PNG under the project's `figure/` directory in Git,
   including individual panels and assembled Figures. Update these files when
   the scripts regenerate them; do not add broad ignore rules for project
   figures.
9. Treat Gallery publication as a separate operation. Only after the user
   explicitly authorizes it, copy the selected project Figure to
   `visualization/gallery/` with its project ID in the filename, add its entry
   to `visualization/README.md`, and fill the corresponding catalog
   `gallery_image` field. Never infer authorization from successful execution,
   visual review, or catalog status. Normal Figure scripts must not create,
   replace, or remove Gallery images.
10. Consider whether the reviewed example teaches a visual practice that
    should be added to `figure_making_guideline.md`.
11. Recheck paths, catalog rows, and links. Review the final diff and confirm
    project Figures are tracked and Gallery changes have explicit user
    authorization. Continue to exclude unrelated caches and intermediate
    computational data where appropriate.

Do not build an automated publisher or ID manager for this initial framework.
Gallery publication is a deliberate review and copy step. Updating a Gallery
image requires a deliberate replacement; rerunning a project script must
never overwrite it automatically.

## Multi-panel Figure workflow

For a Figure made from independently understandable panels, prefer one
user-facing Shell script for the complete Figure and one Python script per
panel. Keep the Shell script at the project root and panel scripts under
`script/`; save panel images and the assembled Figure under `figure/`.
This is a preferred, provisional workflow, not a requirement for every Figure.

```text
visualization/<project>/
├── f02-seasonal-summary.sh       # Settings, panel runs, assembly
├── script/
│   ├── f02a-seasonal-map.py       # Creates one panel independently
│   └── f02b-seasonal-series.py   # Creates one panel independently
└── figure/
    ├── f02a-seasonal-map.png
    ├── f02b-seasonal-series.png
    └── f02-seasonal-summary.png  # Complete Figure for review/publication
```

The Shell entry point should define shared paths and user settings, run the
panel scripts, and then call `visualization/script/combine_panels.py`. Keep
the execution order clear and allow a maintainer to rerun an individual panel
script without regenerating the others. The combiner arranges existing panel
images in horizontal, vertical, or grid layouts and manages spacing, margins,
and aspect ratio. It does not perform scientific processing or render plot
content, axes, legends, or colorbars.

Use the existing IDs consistently: a Figure such as `p01-f02` has panel IDs
`p01-f02a` and `p01-f02b`, and corresponding panel image names such as
`f02a-seasonal-map.png` and `f02b-seasonal-series.png`; the assembled image
uses the Figure-level stem, for example `f02-seasonal-summary.png`. Keep one
catalog row per panel. Each row describes that panel's purpose and variables,
while rows for the same Figure share its user-facing Shell script and its
assembled Gallery image. In the project README, explain the Figure and its
panels together, then show the assembled project-local image immediately
after that explanation; do not create a separate Markdown document per panel.

An integrated plotting script remains appropriate when panels require tightly
coordinated scientific axes, shared color scales or colorbars, or annotations
that cannot be reliably composed from independent images. Choose the
implementation that preserves scientific meaning and produces a clear Figure.

## IDs, catalog rows, and status

Use these current ID forms:

- Project: `p01`, `p02`, and so on. An ID names a project in the catalog, not
  its directory.
- Figure: `p01-f01`, `p01-f02`, and so on, numbered within the project.
- Panel: `p01-f01a`, `p01-f01b`, and so on. The `panel` column contains the
  letter (`a`, `b`, etc.).
- Single-panel or unlabeled Figure: use the Figure ID itself as
  `visualization_id`; leave `panel` empty.

There is one row per panel or option. Rows for panels in one Figure share the
project ID, Figure ID, Figure-generating script, and Gallery image. The
catalog columns are:

| Column | Meaning |
| --- | --- |
| `project_id`, `project_name` | Catalog ID and descriptive directory name. |
| `figure_id`, `panel`, `visualization_id` | Figure identity, optional panel label, and integrated Figure/panel ID. |
| `category`, `title` | Broad visualization type and human-readable panel or Figure title. |
| `description`, `purpose` | What is shown and the scientific or practical reason for showing it. |
| `variables`, `method` | Relevant CaMa-Flood variables and the plotting, normalization, overlay, or display method. |
| `script`, `gallery_image`, `documentation` | Paths relative to `visualization/`; `gallery_image` stays blank until explicit Gallery publication authorization. |
| `status` | `Proposed`, `Available`, or `Tested`, as defined below. |
| `contributor` | Contributor or project owner, when known. |
| `data_source` | Dataset provenance and availability, including whether it is tracked here, separately distributed, external, or generated. |
| `required_data` | Required files or data products and the variables needed by the visualization. |
| `data_preparation` | Where to obtain inputs and the steps needed to prepare them; link project instructions where appropriate. |

Separate multiple variable names with semicolons. Keep values as valid CSV;
quote fields that contain commas or line breaks. Catalog entries should
preserve enough natural-language description and purpose that users and AI
agents do not need to know an ID before searching.

| Status | Use when |
| --- | --- |
| `Proposed` | The method has been suggested, but a complete reusable implementation is not available. Script and Gallery paths may be blank. |
| `Available` | The script and relevant project materials exist. This does not by itself mean the result was run, reviewed, or published. |
| `Tested` | The script has been run successfully and the resulting Figure has been reviewed. |

`Tested` and Gallery publication are separate states: a reviewed Figure may
remain unpublished, and `Tested` is never publication approval. Gallery
publication requires explicit user authorization and a separate copy.
IDs may be renumbered as the repository evolves. When doing so, update catalog
rows, project documentation, Gallery filenames and links, and any current
references. Pair IDs with a natural-language description; do not use an ID as
the only reference to a method.

## Collaboration and human review

Contributors may propose a method, provide an example, develop scripts, check
the science, review the figure design, improve documentation, or maintain
reusable tools. A single person need not do all of these tasks.

Chat AI can help clarify goals, compare design options, and prepare a focused
implementation request. Agent AI can inspect the repository, implement the
approved design, run appropriate checks, and update documentation. The human
researcher remains responsible for scientific choices, interpretation, and
approval. Successful execution alone does not validate a scientific
interpretation. Follow the shared
[figure-making guideline](figure_making_guideline.md) and the repository's
[`AGENTS.md`](../AGENTS.md) when developing or changing resources.

## Relationship to tutorials

Tutorials and manuals can use the catalog to discover suitable visualization
ideas and consult the linked example or guideline. They may adapt methods to
their data and learning objectives. The catalog is a reusable reference, not
a mandatory plotting dependency; tutorials do not need to import scripts from
the Gallery or reorganize their files to match a visualization project.
