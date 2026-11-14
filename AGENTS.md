# CaMa-Flood v4 Repository Instructions

This repository is maintained for CaMa-Flood v4 use, maintenance, and historical reference. New CaMa-Flood v5 development takes place in [global-hydrodynamics/cmf_v5_beta](https://github.com/global-hydrodynamics/cmf_v5_beta). The root [`v5_dev/`](v5_dev/README.md) is an archive, not an active v5 work area. Do not use archived v5 instructions as authority for current v5 development.

## 1. Preserve the checkout and coordinate work

Before editing, inspect the current branch, worktree status, repository structure, and relevant source and documentation. Preserve all pre-existing changes. Do not reset, clean, stash, restore, move, or overwrite files to make the checkout convenient. Do not switch branches unless the maintainer explicitly asks.

Follow the task's stated scope. If a task is explicitly documentation-only or prohibits commits/pushes, that restriction applies. Do not broaden the scope to fix unrelated findings. Ask the maintainer before creating branches, changing public interfaces or data formats, intentionally changing numerical behavior, breaking compatibility, or making other substantial scope changes.

For shared development, confirm the intended branch and ownership with the maintainer or the task record before editing overlapping files. Do not assume archived coordination files under `v5_dev/` are current v4 project status. Do not push development commits to `master` or rewrite shared history.

## 2. Repository boundaries

- `src/` contains the v4 model source and numerical implementation.
- `adm/` contains v4 build configuration; v4 Makefiles use `adm/Mkinclude`.
- `gosh/` contains v4 build and sample-run workflows.
- `map/`, `inp/`, and `etc/` contain maps, forcing preparation, optional schemes, and related tools.
- `util/` contains supporting utilities.
- `doc/` contains v4 manuals and technical documentation.
- `visualization/` contains examples retained from v5 development. They are not certified for v4; see its [README](visualization/README.md).
- `v5_dev/` contains files archived from the former in-repository v5 development tree. Their old paths and execution assumptions may not work in this layout.

Keep v4 maintenance compatible with the existing source organization, namelists, file formats, and scripts. Do not copy v5 code into v4 or apply v5 interfaces and conventions to v4 without an explicitly approved compatibility plan. For v5 work, use the new repository and its current `AGENTS.md`.

## 3. Scientific and compatibility boundaries

Treat model equations, numerical schemes, precision, operation order, time stepping, restart contents and timing, map/forcing formats, namelist names and defaults, and established script entry points as compatibility contracts. Preserve them during documentation, formatting, and maintenance tasks. A change that may alter numerical results or compatibility requires explicit maintainer approval and an appropriately scoped task.

When a scientific or technical rationale is unclear, inspect the v4 source and authoritative v4 documentation. Distinguish confirmed behavior from interpretation. Do not invent physical explanations or silently change behavior to resolve ambiguity; report unresolved questions or request a decision if the work depends on one.

For map or forcing data, preserve provenance, licensing, dimensions, array order, coordinate conventions, units, missing values, and binary/NetCDF layout. Check the relevant map-local documentation and actual metadata before changing a reader or writer. The former v5 registry is archived under `v5_dev/variable_table/`; it is not automatically authoritative for v4 products.

## 4. Source and script conventions

Follow the local style of the v4 source file and its component documentation. Avoid broad reformatting during a focused fix. Fortran comments should clarify non-obvious purpose, units, data layout, or assumptions; do not narrate obvious syntax. Preserve existing legacy naming, formatting, and build conventions unless the task specifically changes them.

Keep shell workflows understandable as direct sequences of experiment settings and commands. Do not add validation frameworks or defensive layers without a concrete need. Keep user-facing settings and path assumptions clear, stop after fatal failures, and avoid deleting anything outside the workflow's explicitly owned output files.

Python utilities should follow the local style; new or substantially changed scripts should use clear names, focused functions, and explicit errors for inputs they consume. Do not impose v5-specific framework or setup assumptions on v4 tools.

## 5. Visualization references

The current shared visualization catalog and scripts in `visualization/` were developed for v5 workflows. They can be useful as examples but are not proof of v4 compatibility. Before adapting them, inspect the v4 data, coordinate conventions, output formats, and dependencies. Do not update or publish Gallery images unless the maintainer explicitly requests it.

## 6. Validation and reporting

Choose checks proportionate to the change. For documentation-only work, verify local links, paths, and consistency; do not run simulations unnecessarily. For source or workflow changes, run the smallest checks that cover the affected behavior and report exactly what was and was not run. Build success or file generation alone does not prove scientific correctness.

Before reporting completion, inspect the diff and worktree status. Identify files changed for the task separately from pre-existing user changes. Do not commit or push unless the task or maintainer explicitly authorizes it. Report changes, validation, remaining limitations, and unresolved technical questions.
