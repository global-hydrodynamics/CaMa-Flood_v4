# Agent Instructions: Authoring Tutorial Manuals

These evolving instructions implement the framework principles in the [tutorial README](README.md); the human-facing process is in [How to develop tutorials](how_to_develop_tutorials.md). They apply to learner-facing manuals under `tutorial/`. Each tutorial's `tutorial_design.md` and `design/manual_design.md` own its scientific and educational intent; manuals are derived from those specifications and verified evidence. Follow the root [`AGENTS.md`](../AGENTS.md) for repository development rules.

## Ground the manual in evidence

- Inspect current scripts, source interfaces, configuration, metadata, bundled data, and generated outputs.
- Execute documented commands when feasible. Record the tested revision and environment when they affect reproducibility; do not present development-branch validation as release-package validation.
- Never invent terminal output, file listings, figures, or results. Distinguish verified behavior, design intent, interpretation, and unresolved information.
- Operational execution and file checks do not establish scientific validity.
- Keep representative stdout/stderr excerpts separate from application log files, and say which source is authoritative. Abbreviate long output explicitly.
- After asking a beginner to inspect a directory, show a concise expected-output excerpt taken from a checked repository. Include enough actual entries to identify the directory's role; mark omitted lines and explain that environment-dependent fields may differ. Never make up filenames, permissions, sizes, or timestamps.
- In beginner tutorials, prefer directory-listing commands that visually distinguish file types when this helps learners navigate the repository. For this project, use `ls -lF` or `ls -lhF` as appropriate; explain only the relevant markers, and ensure the shown output comes from the exact command. Do not impose this convention on unrelated technical scripts where another format is more appropriate.
- For interactive setup procedures, capture real output and describe the exact prompt, available response, resulting action, and what to do when a required check fails. Do not invent prompts from source inspection alone.

## Make instructions easy to follow

- Explain why each major step is needed, what to run, what should appear, and how the reader can interpret the result.
- Use copyable fenced command blocks without shell prompt characters such as `$`. Label commands and observed output separately.
- Keep detailed manual-generation provenance (tested revision, environment, commands, results, limitations, and evidence sources) in a developer record under the tutorial `design/` directory, not in the learner-facing manual.
- At the start of each major command-oriented section in a beginner manual, establish the repository root with `cd "${cmf_root}"`, then enter a relevant subdirectory with a relative `cd` (for example, `cd src_main`). Show `pwd` before `ls` when inspecting a directory. Repeat navigation only at a new section or after the working directory has changed; do not repeat it before every command within one section.
- Establish `cmf_root` once immediately after the reader confirms the repository root with `pwd`: `cmf_root="$(pwd)"`. Explain that it is a shell variable available in that same terminal session; it is not automatically available in a new terminal. Use `${cmf_root}` consistently for repository-root notation and commands.
- For Tutorial01 and future beginner tutorials in this project, use `cd "${cmf_root}"`, `cd src_main`, then `make all`; verify that `MAIN_cmf` exists or has been updated. A GOSH workflow normally launches the executable; do not tell beginners to invoke `MAIN_cmf` directly without its required namelist and working-directory context.
- Explain important filenames, settings, units, time conventions, and input/output relationships without reproducing whole specifications.
- Introduce technical ideas in the order a beginner encounters them: locate and list the real data first, explain its role, then show the configuration that selects it. Use short actual excerpts rather than large metadata dumps.
- Prefer simple commands that teach the intended task. For example, use `cat` to demonstrate that a shell script is readable text, then recommend an available text editor; do not teach regular expressions when the goal is to inspect configuration.
- Explain interactive responses and next steps where users must make a choice. Keep executable-specific or environment-specific output excerpts concise and label omitted lines.
- Keep project naming and output isolation clear. Explain how the configured project name selects its output directory and how separate names preserve experiments; place project-specific names and examples in that tutorial's design.
- End every major instructional section in a beginner manual with a short `### Learning Check` subsection. Use one to three concise Markdown checklist items derived from the tutorial learning outcomes and Manual Design Specification. Match the checks' depth to the tutorial's explicitly stated scope; do not make them more advanced than its learning outcomes. An introductory tutorial may assess file navigation, configuration lookup, and successful execution only. Do not impose that restricted depth on later tutorials, whose stated objectives may require deeper conceptual or scientific checks. Check observable actions and conceptual relationships when appropriate; make each item answerable from that section, avoid repeating commands or introducing untaught ideas, and omit checks from introductions, appendices, and developer-only notes. If suitable checks cannot be derived, improve the specification instead of inventing quiz questions.
- Show before/after evidence when it helps explain a state change, such as executable creation or output generation.
- Inspect the current v5 map-tool layout before describing optional preprocessing. Do not direct users to obsolete paths such as legacy `src_param/`. Do not require map generation when a bundled sample is part of the tutorial.

## Teach progressively and maintain consistency

- Follow the manual design. Explain foundational concepts when first introduced; later tutorials may refer back to earlier material.
- Teach the required learner operations rather than replacing them with completed outputs. Show the provided starting file, the setting or code the learner must change, how the result is used later, and an observable completion check. Keep a complete reference solution in `design/`, outside the distributed learner package.
- Use the activity registry [`tutorial_requirements.csv`](tutorial_requirements.csv) to check that each manual activity supports an explicit tutorial learning outcome. The Manual Design must explain the activity before manual prose is finalized.
- Reference approved expected-result images from `sample_figure/`. Use `figure/` only for results generated by learners; do not rely on pre-generated files there.
- Verify every command, path, filename, setting, data format, unit, and output description against the current repository and evidence.
- Do not add scientific conclusions or objectives absent from the approved design.
- Check links and Markdown structure. Preview rendered output when a suitable renderer is available and a visual check is useful.
- When intended teaching content changes, update the manual design first, then revise the manual from verified evidence.
- Before distribution, inspect the actual package contents. Exclude all `design/` directories, completed reference scripts, generated model outputs, and finished modified inputs; retain starting materials, instructions, and `sample_figure/`. If no tutorial package assembler exists, validate an allowlisted temporary snapshot and report that the official package check is still partial.

These guidelines remain under development and should be refined only when experience shows that a rule generalizes beyond one tutorial.
