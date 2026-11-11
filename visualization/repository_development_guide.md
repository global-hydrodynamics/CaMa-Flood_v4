# Developing Visualizations in CaMa-Flood

This guide explains how researchers, students, and AI agents can propose,
develop, review, and share reusable CaMa-Flood visualizations. Contributors
can participate at different levels: an idea or sample image is a useful
starting point even when no code is available.

## How the visualization resources fit together

| Resource | Role |
| --- | --- |
| Visualization project | A descriptive directory grouping related plotting methods, such as river maps or hydrographs. Project names do not contain numeric IDs. |
| Figure-generating script | The user-facing entry point that creates one complete Figure, possibly with several panels. It lives under the project's `script/` directory. |
| Working Figure | The generated PNG output under that project's `figure/` directory. It can be regenerated and revised during development. Match its descriptive basename to the entry-point script, for example `script/f01-rivmap-uparea.sh` and `figure/f01-rivmap-uparea.png`. |
| Gallery image | An explicitly approved copy of a working Figure under `visualization/gallery/`. It is the image shown in the Gallery README and remains unchanged when a script is rerun. |
| `visualization_catalog.csv` | A discovery index with one row per panel or visualization option. It records the purpose, variables, method, script, status, and publication reference. |
| `figure_making_guideline.md` | Shared scientific-correctness, comparability, readability, and reproducibility principles. |
| `cmf_colormap/` | An existing reusable palette package. It may support projects, but it is not itself a required plotting framework or a Gallery project. |

The catalog is intended for human and AI discovery, including searches by
natural-language descriptions of a scientific need. It is a reference, not a
runtime dependency: tutorials may adapt a method without importing project
scripts directly.

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
   with `script/` and `figure/` subdirectories. Do not create an empty example
   project just to reserve an ID.
4. Assign the next available project and Figure IDs and decide whether the
   Figure needs panel IDs. IDs are catalog references and may be renumbered;
   descriptive directory and script names must stand on their own.
5. Implement the Figure-generating script under `script/`. Give the script
   and PNG output matching descriptive basenames. Keep scientific inputs
   read-only, write working outputs to the owning project's `figure/`, and use
   PNG by default. A shell entry point may invoke Python or shared libraries.
6. Generate and inspect the Figure. Check scientific semantics, data masks,
   units, scales, readability, overlays, and the intended interpretation.
   Record important settings and transformations.
7. Add or update one catalog row per panel or option. Leave unknown
   information blank rather than guessing. Use the status values below.
8. When a Figure is approved for public display, explicitly copy it from the
   project `figure/` directory to `visualization/gallery/`, prefixing the
   source filename with its project ID. For example,
   `river_map/figure/f01-uparea.png` becomes
   `gallery/p01-f01-uparea.png`.
9. Add or update the Gallery entry in `visualization/README.md` to reference
   the approved copy, not the working output. For a multi-panel Figure, the
   separate panel rows in the catalog point to that same Figure script and
   Gallery image.
10. Consider whether the reviewed example teaches a visual practice that
    should be added to `figure_making_guideline.md`.
11. Recheck paths, catalog rows, and links. Review the final diff and confirm
    that only intended project files and approved Gallery images are included.

Project owners may ignore generated working figures in Git where appropriate;
the approved Gallery copy remains a tracked deliverable. Keep source scripts,
documentation, and any intentionally shared materials under version control
according to the project's needs.

Do not build an automated publisher or ID manager for this initial framework.
Gallery publication is a deliberate review and copy step. Updating a Gallery
image requires a deliberate replacement; rerunning a project script must
never overwrite it automatically.

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
| `script`, `gallery_image`, `documentation` | Paths relative to `visualization/`; use blank values when a proposal has no implementation or publication yet. |
| `status` | `Proposed`, `Available`, or `Tested`, as defined below. |
| `contributor` | Contributor or project owner, when known. |

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
remain unpublished, and publication requires an explicit approval and copy.
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
