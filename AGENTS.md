# CaMa-Flood v4 development rules

This file is the authoritative development policy for human developers and AI coding agents. Historical README instructions and branch conventions do not override it. Keep development rules here; other documents should link here instead of restating them.

## 1. Approval and autonomous execution

Before implementation, a maintainer must approve the project, its persistent working branch, and its scope. Record that approval in the project's GitHub issue or pull request and register the project in `docs/dev_status.md`. Approval should identify the intended outcome, boundaries, owner, main files/modules, and acceptance checks. An approved scope can contain many tasks and commits.

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

State the concrete proposed change and its compatibility or numerical consequences when requesting approval. Continue independent work already within scope while a decision is pending. Testing does not authorize an intentional numerical change or a scope expansion. Report out-of-scope findings in the project issue; do not silently fix them.

## 2. One project, one persistent working branch

Use one registered working branch per project across tasks, commits, developers, and AI sessions. Do not create a branch for every task or session. Historical branches need not be renamed or reorganized.

Before making changes:

1. Inspect the repository root, current branch, worktree changes, and remotes.
2. Read the current authoritative registry from `origin/master:docs/dev_status.md`, the approved project issue, and the relevant source files. Fetch `origin` when available to refresh remote-tracking refs; fetching must not switch branches or alter working files.
3. Confirm the current branch exactly matches the project's registered working branch and that the assignment is within approved scope.
4. Check overlapping file/module ownership and current work claims in the project issue.

If the branch does not match, registration/approval is missing, or records disagree, ask for clarification before edits. Do not automatically create or switch branches, even when the expected branch already exists. If the remote cannot be checked, disclose the limitation and resolve coordination uncertainty before edits to shared or overlapping work.

A maintainer may explicitly authorize a bounded documentation/bootstrap task on the current branch before registration. Treat that as an exception for that task only; it does not approve a persistent project branch or future implementation. The initial creation of these two policy documents is such a documentation task and carries no authorization to commit or push.

Preserve other contributors' work. Never reset, clean, stash, overwrite, or commit unrelated changes to make the checkout convenient. Stage only intended files or hunks. Before pushing, check the remote project branch for intervening changes. Fast-forward a clean checkout when appropriate; if histories diverge, coordinate integration with the owner without rebasing shared commits, force-pushing, or switching branches automatically. Use explicit push destinations and never push development commits directly to `master`.

## 3. Lightweight coordination and status synchronization

### Authority

- The copy of `docs/dev_status.md` on `origin/master` is the authoritative project registry and coordination-dashboard snapshot. Copies on project branches are proposals or snapshots, not independent authorities.
- Each registered project has one GitHub issue, linked from its row. The issue holds scope approval, current assignments, active work claims, blockers, conflicts, and the latest progress between dashboard publications.
- The issue supplements current progress; it cannot silently override an approved branch or expand scope. Registry changes require maintainer approval and publication.

Use the existing GitHub issue/PR workflow. Do not introduce a custom coordination service, bot, or separate branch for status management.

### Updating and publishing status

1. For a material change in progress, ownership, affected modules, conflicts, or next actions, update the project issue. Keep a concise current summary in the issue body; use comments for work claims and decisions. Do not log every command or coding session.
2. If the dashboard row needs changing, propose a minimal row update on the already approved project branch or provide the exact proposed row in the issue when a file change is inappropriate. Check the current `origin/master` row first. Do not replace the dashboard with a stale branch copy.
3. The maintainer reconciles proposals against current rows and publishes approved dashboard updates to `master`. This can be a maintainer-authored documentation-only commit, or part of an explicitly approved PR merge from the existing project branch. A status update must not pull unfinished implementation into `master`. Agents do not directly push to `master` or create coordination branches.
4. The maintainer records the publication commit in the project issue. Subsequent sessions read the registry from `origin/master` even if the local project-branch copy is older. Never merge or cherry-pick another project's implementation just to obtain status Markdown.

Publish when a project is registered, ownership/branch/scope boundaries change, a significant conflict or blocker appears, or a project reaches a milestone or completion. Between publications, the issue is the latest progress source. Branch and owner changes are not effective merely because they appear in an unmerged proposal.

### Parallel work

The project owner coordinates task assignments. Before editing, identify the task, assigned files/modules, and any overlap in the issue; check for an existing claim. Resolve overlaps with the owner before editing the same files. GitHub comments are coordination records, not an atomic lock, so conflicting or simultaneous claims need explicit resolution.

Use separate checkouts/worktrees of the same approved branch when concurrent work requires isolation, subject to the execution environment's permissions. Do not share one mutable checkout between concurrent writers. Worktree creation must not create another branch. Coordinate commits and pushes so each writer incorporates the published branch head before publishing. A separate project that needs its own branch requires explicit approval and registration.

## 4. Technical boundaries for CaMa-Flood

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

## 5. Validation and completion

Choose checks proportionate to the change. Inspect test recipes: some existing `test` targets compile executables without running them. Do not report compilation as test execution.

Use three levels as applicable:

1. Build/unit checks: affected compiler/preprocessor paths, shell syntax, existing small numerical/I/O tests, and synthetic fixtures. For build changes, check clean and incremental builds and parallel builds when claimed to be supported.
2. Small-region regression: an explicitly identified dataset and baseline, key hydrological variables, conservation, optional schemes, and continuous versus restart execution. A dedicated approved regional regression dataset is not yet established; do not assume sample archives constitute a validated suite.
3. Full-package integration: representative distribution experiments on suitable laboratory systems, including longer runs and MPI/OpenMP where relevant. These checks need not run on GitHub Actions.

Distinguish bitwise identity under fixed compiler/library/precision/parallel settings from numerical agreement with documented variable-specific absolute and relative tolerances. Check shapes, times, missing values, non-finite values, and conservation separately. Never invent tolerances, regenerate a baseline to hide a regression, or claim numerical equivalence without evidence. Baseline selection and intentional behavior changes require maintainer agreement.

If required data or tooling are missing, state which checks were not run and why; do not claim full validation. For documentation-only work, verify paths, authority, consistency, and the changed-file list without running model simulations.

Before committing or reporting completion, inspect the diff and status for scope and unrelated changes. Commit coherent changes with meaningful messages; include supporting validation in the issue/PR or completion report. Report what changed, checks actually run and results, remaining limitations, and next actions. Update the coordination records for material changes. Completion of an implementation task does not authorize merging into `master` or deleting its persistent branch.
