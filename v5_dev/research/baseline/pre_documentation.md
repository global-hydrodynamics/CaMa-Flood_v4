# Pre-documentation modernization baseline

## Selected commit and tag

- **Tag:** `research-baseline-pre-document`
- **Target commit:** `1c7b667d8f23d31a0fb913d7650f969016d704a5`
- **Commit:** merge of PR #119, `Merge pull request #119 from dtokuda/heat-budget-stability`
- **Committer timestamp:** `2026-10-07T12:35:39+09:00`
- **Identified:** 2026-10-28
- **Selection basis:** the first verified systematic documentation-foundation commit is `680bf133ac487f760fed99f547e6a960706d9342` (`Added agents.md`, `2026-10-25T11:54:44+09:00`). Its sole parent is `1c7b667`. `git diff 1c7b667 680bf13` shows only the addition of `AGENTS.md` and `docs/dev_status.md`; neither file exists at the selected parent. The later documentation reorganization PR #121 used `680bf13` as its base and merged its `docs/reorganization` head into `11d3fcdfe99885b689a0930b12c2bd24136f87b9`.
- **Branch-name qualification:** no local or remote `document` branch reference was present, and GitHub branch/PR searches found no PR with head branch `document`. The verified PR is titled “Docs/reorganization” and uses branch `docs/reorganization`. This record uses “documentation stage” for the evidenced sequence; it does not assert that the user's historical label `document` was the literal name of that deleted branch.

The boundary is exact at the commit level: `1c7b667` is the immediate parent of the first verified documentation-foundation commit. The missing branch ref affects the historical branch name, not the parent relationship. This is a **pre-documentation modernization snapshot**, not a clean or original CaMa-Flood v4.30 release.

## Repository state captured

The baseline tree has 413 tracked paths and the top-level `README.md`, `doc/`, `adm/`, `gosh/`, `inp/`, `map/`, `src/`, `etc/`, and `util/` directories. It already contains user manuals and implementation notes, HPC/validation documentation, source-level test programs under `src/common/test`, `src/io/test`, `src/mod/test`, `src/phys/test`, and `src/heatlink/test`, and validation utilities under `etc/validation/`. “Pre-documentation” therefore means before this systematic modernization effort, not undocumented or without tests.

At this revision, `adm/Mkinclude` selects `gfortran -fopenmp` and `-O3` for its shown macOS setup; NetCDF is optional through a commented compile option. There is no root Python dependency manifest, lockfile, or frozen environment. The sample runner `gosh/test1-glb_15min.sh` expects external map files under `map/glb_15min/`, an interpolation matrix and dimension file under the map directory, and runoff files under `inp/test_1deg/runoff/`. No GrADS map binaries are tracked at this baseline. Two NetCDF files are tracked under the ERA5 download example, but they do not supply the full standard global simulation inputs.

The source tree includes test code and workflows, but their presence does not establish that they were run successfully at this historical revision. PR #121 has no PR body, comments, review timeline entries, commit statuses, check runs, or Actions runs in the inspected GitHub records. No historical test report was found for the documentation stage.

## Relationship to other baselines and stages

The repository's `v4.30` source tag points to `2b07b5bf3a826ca4422476f264e30fa531b59983`, dated 2026-03-12. From that tag to this baseline, Git reports 108 commits and 180 changed files (82,704 insertions, 256 deletions), including subsequent model development and tests. Thus:

- `v4.30` is the separate release-source candidate; its identity with a downloadable package remains unconfirmed in [`v4_baseline.md`](v4_baseline.md).
- `research-baseline-pre-document` is the last verified source state immediately before the systematic documentation foundation began.
- `11d3fcd` is the merged post-documentation / pre-map-tools checkpoint.
- `39dc3df` is the current map-tools-stage audit snapshot, not a completion boundary; that project remains open.

The selected commit is suitable for comparisons specifically asking what changed across the documentation stage. It should not be described as the original v4.30 implementation for a v4-to-v5 numerical comparison; use the release/package baseline separately.

## Retrieval and preservation

The annotated tag points at the historical commit itself; no source tree or historical code was modified. Retrieve it in a separate worktree without switching the current checkout:

```bash
git fetch origin --tags
git worktree add --detach ../CaMa-Flood_v4-pre-document research-baseline-pre-document
# Inspect or run an experiment only inside that separate worktree.
git worktree remove ../CaMa-Flood_v4-pre-document
```

Alternatively, clone the repository and check out the tag there. Do not apply environment fixes or adapters to the tagged tree; place experimental adapters separately and record them.

## Evidence and remaining limits

- Initial commit and parent: [680bf13](https://github.com/global-hydrodynamics/CaMa-Flood_v4/commit/680bf133ac487f760fed99f547e6a960706d9342) and [1c7b667](https://github.com/global-hydrodynamics/CaMa-Flood_v4/commit/1c7b667d8f23d31a0fb913d7650f969016d704a5).
- Documentation PR: [#121](https://github.com/global-hydrodynamics/CaMa-Flood_v4/pull/121), base `680bf13`, head `948402e`, merge commit `11d3fcd`.
- No exact downloadable v4.30 package name/checksum, map package checksum, compiler-version matrix, or reference simulation output has been frozen here.
- Source preservation alone does not make the historical model reproducible. Required map/forcing data, libraries, compiler details, and appropriate reference outputs must still be identified for each experiment.
- The exact association between the maintainer's label `document` and the observable `docs/reorganization` PR remains unverified; see [`KG-0001`](../records/knowledge_gaps/KG-0001.md).
