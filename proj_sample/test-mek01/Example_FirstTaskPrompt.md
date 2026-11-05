# Task for AI Coding Assistant
## Investigate Automated Tutorial Generation for CaMa-Flood v5

I am a student intern working on the CaMa-Flood v5 project.

I have previously followed a CaMa-Flood tutorial and performed basic simulations, but I am still learning Git, Markdown, and AI-assisted software development.

I would like your help investigating how to automatically generate a reproducible Markdown tutorial from an existing simulation project.

### Project location

The CaMa-Flood repository is available in the current working directory.

The relevant project is:

`proj_sample/test-mek01/`

It initially contains:

- `mek01-default_run.sh`: an existing sample simulation script
- `Expected_Manual.md`: an example of the documentation we would like to generate

### Overall objective

We want to generate a tutorial that describes the actual CaMa-Flood source code, model inputs, experiment settings, execution process, and model outputs.

Most technical information should be obtained directly from the repository and simulation results, rather than entered manually.

The generated tutorial should remain easy to update when the model or its configuration changes.

### Step 1 — Inspect the repository

Please investigate the repository and identify:

1. The location of the CaMa-Flood source code and its compilation instructions.
2. The river map used by `mek01-default_run.sh`.
3. The runoff forcing dataset, its format, and its directory.
4. The input mapping files required by the simulation.
5. The main configuration parameters in the simulation script.
6. The location and format of model output files.
7. Any existing variable tables, README files, metadata registries, or tools that could provide information for the tutorial.

Please read the actual files rather than assuming the repository follows a particular structure.

For each important finding, indicate which file supports it.

If a required dataset is unavailable locally, explain what is missing. Do not invent its contents.

### Step 2 — Compare with the expected manual

Read:

`proj_sample/test-mek01/Expected_Manual.md`

For each section, determine:

- What information can be collected automatically?
- What information should be written manually?
- Which existing files or scripts should be used as the source of information?
- Which information can only be obtained after running the model?
- Which parts are potentially difficult to automate?

Please present the results in a concise table.

### Step 3 — Propose a simple implementation

Suggest a minimal implementation for generating the tutorial.

For example, you may consider using a Markdown template and a Python script that collects information from the repository and inserts it into the template.

However, please compare reasonable alternatives and recommend the simplest maintainable solution.

The solution should preferably:

- Avoid unnecessary external dependencies
- Reuse existing repository metadata
- Generate file listings and short excerpts from actual files
- Extract simulation settings reliably
- Collect actual execution information and output file lists
- Avoid repeating manually maintained information
- Allow documentation to be regenerated without rerunning the simulation
- Report missing or inconsistent information rather than fabricating it

The model simulation itself should remain independent from the manual generator.

### Step 4 — Prepare an implementation plan

Please propose a step-by-step plan, starting with a small proof of concept.

A possible sequence is:

1. Generate the source code and map sections.
2. Add the runoff forcing and experiment configuration sections.
3. Add simulation logs and output file summaries.
4. Generate the complete Markdown tutorial.
5. Add optional exercises on experiment names and output variables.

For each step, identify the expected files, implementation difficulty, and how we can test whether it works.

### Important restrictions

**Do not modify any files yet.**

Do not run the simulation, download large datasets, install packages, or modify Git history without my approval.

Do not modify files outside `proj_sample/test-mek01/`.

Do not push to GitHub or create a Pull Request.

Do not assume that the expected manual is technically correct; verify its details against the current repository.

If you are uncertain about something, explain the uncertainty and suggest how to check it.

Please explain your findings in a way that helps me understand the repository and the proposed implementation.

**First, complete the investigation and provide the implementation plan. Wait for my review before writing code.**