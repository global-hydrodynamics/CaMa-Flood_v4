# Agent Instructions: Authoring Tutorial Manuals

These evolving instructions apply to learner-facing manuals under `tutorial/`. The tutorial-specific `design/manual_design.md` defines the intended audience and teaching content. Follow the root [`AGENTS.md`](../AGENTS.md) for repository development rules.

## Ground the manual in evidence

- Inspect current scripts, source interfaces, configuration, metadata, bundled data, and generated outputs.
- Execute documented commands when feasible. Record the tested revision and environment when they affect reproducibility; do not present development-branch validation as release-package validation.
- Never invent terminal output, file listings, figures, or results. Distinguish verified behavior, design intent, interpretation, and unresolved information.
- Operational execution and file checks do not establish scientific validity.
- Keep representative stdout/stderr excerpts separate from application log files, and say which source is authoritative. Abbreviate long output explicitly.

## Make instructions easy to follow

- Explain why each major step is needed, what to run, what should appear, and how the reader can interpret the result.
- Use copyable fenced command blocks without shell prompt characters such as `$`. Label commands and observed output separately.
- Keep detailed manual-generation provenance (tested revision, environment, commands, results, limitations, and evidence sources) in a developer record under the tutorial `design/` directory, not in the learner-facing manual.
- At the start of each major command-oriented section in a beginner manual, return to the repository root with `cd "${cmf_root}"`, then enter the relevant subdirectory with a relative `cd` (for example, `cd src_main`). Show `pwd` before `ls` when inspecting a directory. Keep related commands together and avoid unnecessary directory changes within a section.
- Establish `cmf_root` once immediately after the reader confirms the repository root with `pwd`: `cmf_root="$(pwd)"`. Explain that it is a shell variable available in that same terminal session; it is not automatically available in a new terminal. Use `${cmf_root}` consistently for repository-root notation and commands.
- For Tutorial01 and future beginner tutorials in this project, use `cd "${cmf_root}"`, `cd src_main`, then `make all`; verify that `MAIN_cmf` exists or has been updated. A GOSH workflow normally launches the executable; do not tell beginners to invoke `MAIN_cmf` directly without its required namelist and working-directory context.
- Explain important filenames, settings, units, time conventions, and input/output relationships without reproducing whole specifications.
- Show before/after evidence when it helps explain a state change, such as executable creation or output generation.
- Inspect the current v5 map-tool layout before describing optional preprocessing. Do not direct users to obsolete paths such as legacy `src_param/`. Do not require map generation when a bundled sample is part of the tutorial.

## Teach progressively and maintain consistency

- Follow the manual design. Explain foundational concepts when first introduced; later tutorials may refer back to earlier material.
- Verify every command, path, filename, setting, data format, unit, and output description against the current repository and evidence.
- Do not add scientific conclusions or objectives absent from the approved design.
- Check links and Markdown structure. Preview rendered output when a suitable renderer is available and a visual check is useful.
- When intended teaching content changes, update the manual design first, then revise the manual from verified evidence.

These guidelines remain under development and should be refined only when experience shows that a rule generalizes beyond one tutorial.
