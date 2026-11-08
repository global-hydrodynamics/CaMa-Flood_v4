# Manual Authoring Guidelines

These shared guidelines apply to manuals for all tutorials under `tutorial/`. Each tutorial's `manual_design.md` defines its intended readers, topics, and content organization.

## Evidence-based documentation

- Inspect the actual scripts, source interfaces, configuration, metadata, data, and generated outputs.
- Execute documented commands when feasible. Capture representative real stdout and stderr, and inspect application log files separately.
- Never present fabricated terminal output, file listings, figures, or results as observed evidence.
- Clearly distinguish verified behavior, design intent, interpretation, and unresolved information. Do not present operational success as scientific validation.
- Record the tested repository revision and environment when they materially affect reproducibility. Do not describe development-branch validation as release-package validation.

## Clear explanations and commands

- Explain the purpose of each important step and why it is needed.
- Keep terminal commands and their observed output in separate fenced code blocks. State when long output is shortened.
- Use copyable commands, accurate paths and filenames, and appropriate Markdown code fences.
- Explain variables, filenames, units, time conventions, and output contracts when they help the intended reader.
- Avoid overwhelming readers with full compiler logs or large file listings; choose excerpts that preserve context and meaning.
- Distinguish shell output, standard error, program-specific log files, and generated metadata. Tell the reader which source is authoritative for each check.

## Progressive learning

- Follow the manual's design specification and audience assumptions.
- Explain a scientific or technical concept in detail when it first appears in the learning sequence.
- Avoid repeating already taught material; use concise reminders and links to earlier tutorials.
- Prioritize concepts newly introduced by the current tutorial.
- Do not invent learning objectives or experiment requirements that are absent from the approved manual design.

## Figures and tables

- Generate figures and result tables from actual checked outputs.
- Label experiment settings, variable names, units, time interpretation, and data source as needed to reproduce the result.
- Explain transformations such as masking, aggregation, or timestamp shifting. Do not imply that a visualization itself establishes scientific validity.

## Consistency and maintenance

- Verify commands, paths, filenames, settings, and output descriptions against the current implementation.
- Check scientific interpretations against source, metadata, or expert-reviewed specifications; raise unresolved decisions.
- Check relative Markdown links and render/read the final document for structure and clarity.
- Revisit manuals after relevant software, data, configuration, or workflow changes. Update the manual design specification when the intended teaching content changes, then regenerate affected manual content from verified evidence.
- Keep manual edits focused on the approved design and review specification and artifact diffs together.
