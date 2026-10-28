# Comparing modernization stages

## Reference commits

| State | Reference | Meaning |
| --- | --- | --- |
| Pre-documentation | `research-baseline-pre-document` → `1c7b667d8f23d31a0fb913d7650f969016d704a5` | Last commit before the first verified documentation-foundation commit. |
| Post-documentation / pre-map-tools | `11d3fcdfe99885b689a0930b12c2bd24136f87b9` | Merge of documentation PR #121. |
| Map-tools audit snapshot | `39dc3df7c092bdbd1825cd9e3063d0f669dbe871` | Current branch snapshot during this audit; not a final stage tag. |
| v4.30 source candidate | tag `v4.30` → `2b07b5bf3a826ca4422476f264e30fa531b59983` | Release-source candidate; separate from the pre-documentation snapshot. |

The exact branch name `document` is not recoverable from current Git/GitHub refs. The narrative labels stages by the verifiable commits and PRs rather than inventing a branch mapping.

## Safe checkout and structural comparison

Use separate worktrees so the active development checkout remains untouched:

```bash
git worktree add --detach ../cmf-pre-document research-baseline-pre-document
git worktree add --detach ../cmf-post-document 11d3fcdfe99885b689a0930b12c2bd24136f87b9
git worktree add --detach ../cmf-map-tools 39dc3df7c092bdbd1825cd9e3063d0f669dbe871

git diff --stat research-baseline-pre-document..11d3fcdfe99885b689a0930b12c2bd24136f87b9
git diff --name-status research-baseline-pre-document..11d3fcdfe99885b689a0930b12c2bd24136f87b9
git diff --stat 11d3fcdfe99885b689a0930b12c2bd24136f87b9..39dc3df7c092bdbd1825cd9e3063d0f669dbe871
```

Remove only the temporary worktrees after experiments with `git worktree remove <path>`. The examples use detached references and do not create branches. A separate clean clone is also suitable.

## What can be compared now, and what needs more evidence

| Comparison | Status | Needed before a defensible result |
| --- | --- | --- |
| Git file/tree changes from pre-document to post-document and current map-tools snapshot | Possible now from the preserved Git commits and tag | Interpret change by category; file counts alone are not an outcome measure. |
| Documentation navigation, metadata coverage, and recorded build/test entry points | Possible now as repository inspection | A task rubric and archived prompts are needed to measure whether users/agents can find and correctly apply the knowledge. |
| Mekong sample binary/CTL integrity and category-11 converter behavior | Rerun successfully during this audit; see `VR-0001` and `VR-0002` | These do not establish model simulation reproducibility, optional-scheme support, or numerical correctness. |
| Clean install and standard-run completion at pre-document baseline | Not yet reproducibly established | Exact compiler/libraries, map and runoff datasets, checksums, configuration, and expected outputs. Baseline tracked tree lacks the required global binary maps and runoff series. |
| Task completion / assistance comparison across v4 and v5 | Not yet comparable | Maintainer-reviewed task prompts, matched starting conditions, assistance policy, criteria, archived discussion evidence, and any participant/ethics process. |
| Numerical equivalence across old and modernized code | Not established | Matching data, parameterization, build flags, reference outputs, and pre-agreed variable-specific comparison criteria/tolerances. |

## Evaluation sequence

1. **Repository and documentation:** compare directory listings, authoritative guides, metadata schemas, and broken/stale references. Counts can describe scale but do not measure accessibility by themselves.
2. **Knowledge-access tasks:** define a concrete question about a variable, workflow, or module interface; require source-backed answers and record correctness, time only if actually measured, unresolved gaps, and maintainer interventions. Historical ChatGPT summaries are not available in this repository yet.
3. **Installation and execution:** define a clean-environment task and freeze the compiler, libraries, configuration, dataset identities/checksums, exact commands, and outputs. The pre-document baseline's standard runner requires map and runoff inputs not tracked at that revision.
4. **Extensibility tasks:** choose a bounded feature request and record required files/modules, interface changes, approvals, implementation evidence, and checks. Do not compare completion time unless the protocol, starting state, and assistance are comparable.
5. **Governance tasks:** trace a scoped change through proposal, scientific review, approval, validation, and integration using public records. Do not infer motivations absent a discussion summary.
6. **Scientific/numerical comparison:** use the same well-identified inputs and execution settings where compatible. Define variable-specific acceptance criteria and tolerances before running; separate bitwise reproducibility from numerical agreement. No reference simulation outputs or approved tolerance suite are established by this task.

Keep baseline files immutable. If a legacy build needs adaptation, put the adapter outside the baseline tree, cite its own commit, and document any effects on comparability.

## Proposed stage tags (not created)

- `research-stage-document` → candidate `11d3fcdfe99885b689a0930b12c2bd24136f87b9` (PR #121 merge). This is the verifiable completion point for the `docs/reorganization` PR; confirm whether it is the intended “document” stage before creating a stage tag.
- `research-stage-map-tools` → target to be selected only after the open `feature/map-data-tools` project reaches a maintainer-confirmed completion boundary. `39dc3df` is an interim snapshot, not a final tag target.

Only the pre-documentation baseline receives a permanent tag in this task.
