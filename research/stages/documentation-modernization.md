# Documentation modernization stage

## Verified boundary and sequence

- **Foundation start:** `680bf133ac487f760fed99f547e6a960706d9342`, `2026-10-25T11:54:44+09:00`, parent `1c7b667d8f23d31a0fb913d7650f969016d704a5`.
- **First branch change:** `677d7eb7cc6ed8945e6f5f7390124feb6d48ec61`, `2026-10-25T12:05:20+09:00`, first commit on the verified `docs/reorganization` branch.
- **Branch head:** `948402eaf333aea19fd459e680cca508e52ae9e3`, `2026-10-25T16:58:31+09:00`.
- **Merge/completion checkpoint:** `11d3fcdfe99885b689a0930b12c2bd24136f87b9`, `2026-10-25T16:59:21+09:00`, PR #121.
- **Elapsed commit-time span:** about five hours from the first foundation commit to the merge checkpoint; this is a timestamp difference, not measured labor time.

The commit `680bf13` added `AGENTS.md` and `docs/dev_status.md`. PR #121 used it as its base and merged six branch commits under the GitHub head name `docs/reorganization`. A literal branch named `document` is not present in local/remote refs and was not found as a PR head. This stage record therefore avoids equating those names without confirmation.

## Objectives and changes visible in the repository

The maintainer's current task description frames this stage as organizing the repository, exposing developer knowledge, and establishing agent/development rules. The committed changes show:

- Root `README.md` rewrite and navigation for users/developers, including the 2025 CaMa-Flood review citation.
- `AGENTS.md` and `docs/dev_status.md` as development policy and coordination documents.
- Component guides for `adm/`, `src/`, `gosh/`, `map/`, `inp/`, and `etc/`; updates to existing utility manuals and READMEs.
- An Intel `ifx` build-configuration example and clearer shell-script option/input behavior, including `gosh/options_full.sh`, `map/src/src_param/s02-generate_inpmat.sh`, and the ERA5 conversion script.
- Documentation for the downscaling, levee-parameter, N-year flood-depth, HPC, result-mapping, and runoff-preset workflows.

PR #121 reports 24 changed files, 1,736 additions, and 296 deletions. The diff contains documentation, shell scripts, and a compiler include; it contains no Fortran source changes. The PR metadata has no description, comments, review timeline entries, commit statuses, check runs, or Actions runs. Consequently this reconstruction can verify the merged file changes, but cannot substantiate historical test results, time spent, detailed alternatives, or the motivation for individual choices.

## Outcomes, evidence, and remaining limitations

The merge provides a reproducible post-documentation checkpoint at `11d3fcd`. The changes establish navigation and written development rules; they do not by themselves demonstrate improved user comprehension, reproducibility, extensibility, or governance. Those properties require task-based evaluation defined in [`../study_protocol.md`](../study_protocol.md).

No historical unit/model test result was found in PR #121 or its commit checks. This task did not rerun historical builds. The only current tests rerun for the separate map-tools stage are listed in [that stage record](map-data-tools.md); they must not be attributed to this documentation stage.

The exact old `document` branch identity and the reasoning documented in ChatGPT conversations remain unresolved. The user-provided ChatGPT topic list is a lead for a later maintainer-reviewed summary, not evidence of a particular chronology or quotation. See [`KG-0001`](../records/knowledge_gaps/KG-0001.md) and [`chatgpt_archive_queue.md`](../records/discussions/chatgpt_archive_queue.md).

## Key references

- [Foundation commit](https://github.com/global-hydrodynamics/CaMa-Flood_v4/commit/680bf133ac487f760fed99f547e6a960706d9342)
- [Documentation PR #121](https://github.com/global-hydrodynamics/CaMa-Flood_v4/pull/121)
- [PR head commit](https://github.com/global-hydrodynamics/CaMa-Flood_v4/commit/948402eaf333aea19fd459e680cca508e52ae9e3)
- [Merge commit](https://github.com/global-hydrodynamics/CaMa-Flood_v4/commit/11d3fcdfe99885b689a0930b12c2bd24136f87b9)
