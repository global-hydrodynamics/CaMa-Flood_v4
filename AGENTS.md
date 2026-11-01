# CaMa-Flood v4 development rules

This file is the authoritative development policy for human developers and AI coding agents. Historical README instructions and branch conventions do not override it. Keep development rules here; other documents should link here instead of restating them.

## 1. Approval and autonomous execution

Before implementation, a maintainer must approve the project, its persistent working branch, and its scope. Record that approval and its boundaries in the linked GitHub Issue/PR when one is used, or in the project plan and `docs/dev_status.md` when it is not. The record should identify the intended outcome, owner, main files/modules, and acceptance checks. An approved scope can contain many tasks and commits.

Once approved, agents may inspect files and dependencies, implement changes, run appropriate tests, fix issues found within scope, make logically organized commits, push to the designated project branch, and update development status. Do not ask again for each routine step. A task-specific restriction, such as “documentation only” or “do not commit or push,” still applies.

Obtain explicit maintainer approval before:

- Creating any additional working branch, including a replacement or temporary coordination branch.
- Substantially changing project scope.
- Merging into `master`.
- Force-pushing or rewriting shared history.
- Deleting a shared branch.
- Intentionally changing established numerical behavior.
- Breaking backward compatibility.
- Changing a public interface or distribution format.

State the concrete proposed change and its compatibility or numerical consequences when requesting approval. Continue independent work already within scope while a decision is pending. Testing does not authorize an intentional numerical change or a scope expansion. Report out-of-scope findings in the project issue when one exists, or in the project status/completion report; do not silently fix them.

For multi-stage work coordinated by the maintainer, ChatGPT, and Codex, use the reusable review and handoff protocol in [`docs/ai_development_workflow.md`](docs/ai_development_workflow.md).

## 2. One project, one persistent working branch

Use one registered working branch per project across tasks, commits, developers, and AI sessions. Do not create a branch for every task or session. Historical branches need not be renamed or reorganized.

Before making changes:

1. Inspect the repository root, current branch, worktree changes, and remotes.
2. Read the current authoritative registry from `origin/master:docs/dev_status.md`, the linked project issue if one exists, and the relevant source files. For substantial development, check existing GitHub Issues and open Pull Requests for overlapping work. Fetch `origin` when available to refresh remote-tracking refs; fetching must not switch branches or alter working files.
3. Confirm the current branch exactly matches the project's registered working branch and that the assignment is within approved scope.
4. Check overlapping file/module ownership and current work claims in the project issue if one exists, or in the project status record and maintainer coordination.

If the branch does not match, registration/approval is missing, or records disagree, ask for clarification before edits. Do not automatically create or switch branches, even when the expected branch already exists. If the remote cannot be checked, disclose the limitation and resolve coordination uncertainty before edits to shared or overlapping work.

A maintainer may explicitly authorize a bounded documentation/bootstrap task on the current branch before registration. Treat that as an exception for that task only; it does not approve a persistent project branch or future implementation. The initial creation of these two policy documents is such a documentation task and carries no authorization to commit or push.

Preserve other contributors' work. Never reset, clean, stash, overwrite, or commit unrelated changes to make the checkout convenient. Stage only intended files or hunks. Before pushing, check the remote project branch for intervening changes. Fast-forward a clean checkout when appropriate; if histories diverge, coordinate integration with the owner without rebasing shared commits, force-pushing, or switching branches automatically. Use explicit push destinations and never push development commits directly to `master`.

## 3. Lightweight coordination and status synchronization

### Authority

- The copy of `docs/dev_status.md` on `origin/master` is the authoritative project registry and coordination-dashboard snapshot. Copies on project branches are proposals or snapshots, not independent authorities.
- An Issue supplements coordination; it cannot silently override an approved branch or expand scope. Registry changes require maintainer approval and publication.

### GitHub Issues for Major Development Tasks

Creating a GitHub Issue is recommended but optional for substantial development tasks. When created, briefly describe the purpose and scope, planned changes and potentially affected components, responsible developer and working branch if known, and dependencies or potential conflicts. Link it from the project row and use it for assignments, blockers, and progress.

Before substantial development, check existing Issues and open Pull Requests for overlapping work. Use Issues for coordination and progress tracking; maintain technical specifications and implementation details in repository documentation. The project, branch, and scope still require maintainer approval. Record that approval in an Issue/PR when used, or in the repository project plan and `docs/dev_status.md` when no Issue/PR is created.

Do not introduce a custom coordination service, bot, or separate branch for status management.

### Updating and publishing status

1. For a material change in progress, ownership, affected modules, conflicts, or next actions, update the project issue if one exists. Keep a concise current summary there; use comments for work claims and decisions. If no issue exists, update the branch-local project status proposal or appropriate project document. Do not log every command or coding session.
2. If the dashboard row needs changing, propose a minimal row update on the already approved project branch or provide the exact proposed row in the issue, when one exists, if a file change is inappropriate. Check the current `origin/master` row first. Do not replace the dashboard with a stale branch copy.
3. The maintainer reconciles proposals against current rows and publishes approved dashboard updates to `master`. This can be a maintainer-authored documentation-only commit, or part of an explicitly approved PR merge from the existing project branch. A status update must not pull unfinished implementation into `master`. Agents do not directly push to `master` or create coordination branches.
4. The maintainer records the publication commit in the project issue when one exists; otherwise the published dashboard is the record. Subsequent sessions read the registry from `origin/master` even if the local project-branch copy is older. Never merge or cherry-pick another project's implementation just to obtain status Markdown.

Publish when a project is registered, ownership/branch/scope boundaries change, a significant conflict or blocker appears, or a project reaches a milestone or completion. Between publications, the issue is the latest progress source when used; otherwise consult the current project status and plan. Branch and owner changes are not effective merely because they appear in an unmerged proposal.

### Parallel work

The project owner coordinates task assignments. Before editing, identify the task, assigned files/modules, and any overlap in the project issue if one exists, or in the status/plan otherwise; check for an existing claim. Resolve overlaps with the owner before editing the same files. GitHub comments are coordination records, not an atomic lock, so conflicting or simultaneous claims need explicit resolution.

Use separate checkouts/worktrees of the same approved branch when concurrent work requires isolation, subject to the execution environment's permissions. Do not share one mutable checkout between concurrent writers. Worktree creation must not create another branch. Coordinate commits and pushes so each writer incorporates the published branch head before publishing. A separate project that needs its own branch requires explicit approval and registration.

## 4. Preserve development knowledge

When a maintainer confirms an important technical specification, record the confirmed decision, rationale, and constraints in the appropriate technical documentation. Distinguish verified facts, confirmed design decisions, assumptions, and unresolved questions. Link relevant source, tests, Issues, or Pull Requests where useful. Update an existing authoritative document instead of creating duplicate or contradictory explanations. Keep this policy and `docs/dev_status.md` concise; link to detailed technical documentation.

Do not treat informal discussion as a permanent design decision unless the maintainer confirms it. Before asking a technical question, consult the relevant documentation and source code. Ask only when the answer cannot be reliably established there or a new design decision is required.


### Maintainer questions and decision management

Investigate technical questions using documentation, source code, and available test data first. Do not spend excessive time investigating undocumented scientific knowledge or design decisions. Classify unresolved questions as:

- **Blocking:** needed to proceed safely, affects architecture, or could cause substantial rework. Ask promptly before affected work proceeds; explain the decision and consequences.
- **Important:** affects a specification or design choice, while independent work can continue. Record and raise at the next appropriate milestone.
- **Documentation:** background, provenance, or clarification that does not affect current implementation. Group for review at the end of the task or phase.

Record non-blocking questions in the relevant technical document under **Open Questions**, explaining why each answer matters and what work can continue. Group related questions and avoid repeated interruptions. Continue unrelated work while a blocking decision is pending. When the maintainer answers, record confirmed facts and rationale, update or remove resolved questions, and do not ask again unless new evidence reveals a contradiction.

At completion of a substantial task or development stage, run the brief **Knowledge-gap review** in [`docs/ai_development_workflow.md`](docs/ai_development_workflow.md). Record material questions before resolving them, including source references and why the answer matters. At handoff, list unresolved scientific or architectural questions; unrelated work need not wait for non-blocking answers. Link resolved questions to their discovery trigger, evidence, interpretation, and formalization. Record a completed review with no material new gaps; never invent questions or findings.

## 5. Technical boundaries for CaMa-Flood

Preserve the existing architecture unless the approved task justifies changing it:

- `src/`: numerical core; `cmf_drv_*` lifecycle and advancement, `cmf_ctrl_*` orchestration/I/O, `cmf_calc_*` calculations, and `yos_cmf_*` shared state.
- `adm/` and `gosh/`: build configuration and execution workflows.
- `map/`, `inp/`, `etc/`, and `util/`: preprocessing, forcing preparation, optional schemes, analysis, and utilities.
- `doc/`: existing user and technical documentation. `docs/dev_status.md` is the coordination dashboard; do not move or duplicate `doc/` to create a new documentation hierarchy.

Prefer small, independently reviewable changes. Existing Fortran, namelists, Makefiles, and shell scripts are valid mechanisms. Do not introduce frameworks, dependencies, configuration layers, or broad rewrites without a concrete need in the approved scope.

Treat the following as established contracts: public driver/coupler routines, namelist names and defaults, script entry points, paths expected by workflows, variable names/units/meaning, binary layouts, NetCDF conventions, restart precision and contents, and distribution formats. Preserve legacy aliases unless a compatibility change has been approved.

For numerical refactoring, preserve precision kinds, arithmetic and reduction order, limiters, adaptive time stepping, state-update order, and coupling/restart timing. Storage is intentionally double precision in important paths even when other calculations use single precision. Moving a call can change the numerical method. Do not mix numerical changes with cosmetic cleanup.

For optional schemes, identify initialization, state ownership, outer/adaptive time-step hooks, forcing, output, restart, and unsupported combinations. Test disabled-scheme behavior as well as enabled behavior where data permit. Do not assume compile-time flags and runtime switches are interchangeable.

Preserve the separation between source development and downloadable data-rich distribution packages. Do not add large datasets, generated model outputs, build products, credentials, or local environment paths to commits. New test fixtures need a clear purpose, size, provenance, and redistribution permission.

## Shell Script Design Principles

The following are recommended, not mandatory, practices for community shell contributions: keep scripts human-readable as automated terminal-command sequences; make scientific settings easy to find; prefer direct executable calls; avoid unnecessary defensive boilerplate; and place validation and detailed errors primarily in the program that reads the data. Use recognizable progress headings, stop on fatal errors, report clear success or error summaries, and return nonzero status for fatal program errors where practical. These recommendations do not apply retroactively to community or historical scripts.

For the current maintainer-led v5 modernization, AI agents should use these practices by default in all new or refactored scripts under `map/tools/script_region/`, `script_params/`, and `script_analysis/`, including their map-local examples. Group commands into meaningful blocks with blank lines and short comments. Keep user settings near the top, then mark the transition to implementation with the three-line `User settings end here` separator; leave a blank line before and after major separators. Keep error traps distinct from processing logic and add an `End of error handling.` separator after signal traps where a trap block exists.

Separate logically distinct operations with a blank line, even when a block contains only one or two commands. Keep the commands belonging to one processing step together. Use three-line comment separators for major sections and simple whitespace for smaller transitions. For visible workflows, use accurate `[STEP n/N]`, `[SUCCESS]`, and `[ERROR]` messages. Start step and success banners with a newline, leave a blank line between major stages, and keep useful program output visible without reformatting it. Preserve the original command order and avoid extra comments, decorative separators, or abstractions added only for appearance. These conventions apply to future v5 scripts as well as readability updates to existing scripts. Keep `set -eu`, simple argument handling and loops, and safe cleanup; do not sacrifice correctness or reproducibility for brevity. See [`docs/ai_development_workflow.md`](docs/ai_development_workflow.md) for the agent defaults and workflow-message convention.

Within `script_analysis/`, keep `a00` as a simple copy utility that discovers and copies every regular executable named `analysis_*` from the shared code directory; do not hard-code the executable list or compile from this script. Treat `a01`–`aXX` as command-line examples: show the working directory and print the exact, safely quoted command immediately before invoking the corresponding local analysis executable. Keep executable calls visible as direct `./analysis_*` commands where the program's working directory allows; respect program-specific working-directory requirements and explain any necessary path exception in the example and its README.

## Map metadata maintenance

When adding or changing map-related readers, writers, preprocessing products, or model map interfaces, inspect and update [`map/map_variables.csv`](map/map_variables.csv) and the responsible module README. Keep verified facts distinct from provisional fields, and record distribution role separately from packages where a product was observed; see [`map/map_metadata_spec.md`](map/map_metadata_spec.md).

The maintainer manually controls the map registry's workflow-status column, currently named `workflow_stage` in `map/map_variables.csv`. Codex may read it but must not edit its values, including when adding or revising registry rows. If the status appears stale or a new row needs a status, leave it unchanged or blank and report the suggested update to the maintainer. Preserve the existing column values during CSV edits and do not infer or set `default` from file presence.

## Shared Fortran Module Naming (`map/tools/code/`)

Name shared Fortran module source files in `map/tools/code/` with the `mod_` prefix (for example, `mod_map_color.F90`). This convention is limited to shared module source files in this directory; it does not apply to main-model files under `src/`, programs, or files in other directories. A source filename rename does not require renaming its internal Fortran module or procedures unless a task explicitly approves that interface change.

## Fortran Source Readability

Fortran declarations are a human-readable inventory of a program's data structures. Prioritize human understanding over compactness: group declarations by scientific or computational purpose, keep closely related values together (for example, `nx, ny` or `ix, iy`), and use separate declarations for variables with different meanings even when this adds lines. Use short comments and blank lines between groups. Add concise inline notes for variable roles, physical units, and important classifications when established by code or maintainer decisions; do not guess at unknown meaning. Align declarations reasonably. Use concise comments for major calculations and assumptions, not trivial operations. Long programs should use numbered headings such as `[1]`, `[2]`, and `[3]` with visible separators (`! ================================================`) to mark major stages. Keep scientific expressions and decision rules visible, and avoid helper abstractions that hide them. These are community recommendations, not strict formatting requirements for every contributor. In the current maintainer-led v5 modernization, apply them by default while preserving numerical behavior and keeping formatting changes focused.

## Cross-project research logging for the v5 case study

Research logging is a long-term activity across the v4-to-v5 transition, not a feature owned by one project or branch. Preserve the common [`research/`](research/README.md) history across projects and branch transitions; no dedicated research Issue is required. Record significant discussions, approvals, instructions, implementations, knowledge discoveries, human interventions, failures, and validations, distinguishing each stage. The study's approved v5 dimensions are knowledge accessibility, reproducibility, extensibility, and governance; see [`research/study_protocol.md`](research/study_protocol.md). Do not log routine commands or every AI turn, invent unavailable history/metrics, commit raw private transcripts, or claim scientific correctness from execution alone. Have the maintainer review AI-generated discussion summaries, and run `python3 research/validate_records.py` after changing the record tables.

## 6. Validation and completion

Choose checks proportionate to the change. Inspect test recipes: some existing `test` targets compile executables without running them. Do not report compilation as test execution.

Use three levels as applicable:

1. Build/unit checks: affected compiler/preprocessor paths, shell syntax, existing small numerical/I/O tests, and synthetic fixtures. For build changes, check clean and incremental builds and parallel builds when claimed to be supported.
2. Small-region regression: an explicitly identified dataset and baseline, key hydrological variables, conservation, optional schemes, and continuous versus restart execution. A dedicated approved regional regression dataset is not yet established; do not assume sample archives constitute a validated suite.
3. Full-package integration: representative distribution experiments on suitable laboratory systems, including longer runs and MPI/OpenMP where relevant. These checks need not run on GitHub Actions.

Distinguish bitwise identity under fixed compiler/library/precision/parallel settings from numerical agreement with documented variable-specific absolute and relative tolerances. Check shapes, times, missing values, non-finite values, and conservation separately. Never invent tolerances, regenerate a baseline to hide a regression, or claim numerical equivalence without evidence. Baseline selection and intentional behavior changes require maintainer agreement.

If required data or tooling are missing, state which checks were not run and why; do not claim full validation. For documentation-only work, verify paths, authority, consistency, and the changed-file list without running model simulations.

Before committing or reporting completion, inspect the diff and status for scope and unrelated changes. Commit coherent changes with meaningful messages; include supporting validation in the issue/PR or completion report. Report what changed, checks actually run and results, remaining limitations, and next actions. Update the coordination records for material changes. Completion of an implementation task does not authorize merging into `master` or deleting its persistent branch.
