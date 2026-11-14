# Manual Design — Tutorial02: Mekong Four-Experiment Comparison

**Audience:** learners who have completed a basic model run and can edit a shell script. The learner manual is a future task; this document defines its instructional sequence and package requirements.

## Learning sequence and required learner actions

| Section | Learner operation | Starting material | Learner-created artifact | Observable completion |
| --- | --- | --- | --- | --- |
| Inspect the default experiment | Read `mek_default.sh` and identify its project name and model options. | Working Default script. | None yet. | Correctly identify `PROJ`, `LPTHOUT`, `LKINE`, and output location. |
| Create a No Bifurcation experiment | Copy the Default script, give it a unique name and `PROJ`, and set `LPTHOUT` false while preserving the other approved settings. | `mek_default.sh`. | Learner-created `mek_nobif.sh`. | Script has a distinct project output path and only the intended scheme change. |
| Create a Kinematic Wave experiment | Copy the Default script and edit the required routing and adaptive timestep settings; use a new project name. | `mek_default.sh`. | Learner-created `mek_kine.sh`. | Script carries the approved `mek_kine` settings and isolated output path. |
| Create a No Floodplain experiment | Copy the Kinematic Wave configuration and disable floodplain storage representation; use a new project name. | Learner-created `mek_kine.sh`. | Learner-created `mek_nofld.sh`. | Script carries the approved `mek_nofld` settings and isolated output path. |
| Run and compare | Run all four experiments, then run the provided visualization workflow. | Default plus three learner-created scripts and shared sample inputs. | Four independent output directories and figures under `figure/`. | All annual runs finish; comparison figures exist and correspond to the four named configurations. |

The three completed alternative scripts remain in `design/reference_mek_*.sh` for developer use and are excluded from the learner package. The design preserves all four scientific configurations and existing verified results. It does not claim that simulated differences validate observations or that the 2000 spin-up is sufficient.

## Teaching guidance

Introduce the experiment table and each switch before asking learners to edit it. The exercise is configuration literacy: duplicate a known-good run definition, change only approved settings, isolate outputs with `PROJ`, and compare the resulting model behavior. Do not ask learners to invent scientific settings or predict the sign of the response.

The provided visualization scripts require all four experiments, so the normal sequence must make the three absent variants before visualization. Explain annual NetCDF Runoff and Restart handoffs at the level needed to run the supplied Default; the tutorial does not require learners to rewrite the year loop.

## Package and developer requirements

- Include `mek_default.sh`, visualization scripts, the learner README, and approved figures under `sample_figure/`.
- Exclude the three completed alternative GOSH scripts, `design/`, `out/`, `nohup.out`, and generated images under `figure/`.
- Keep `figure/` present without generated PNGs at package start.
- Provide a developer reset under `design/`; it must confirm before deleting only named experiment outputs, learner-created variant scripts, and generated figures.
- No full Tutorial02 learner manual exists yet. Package reproducibility is therefore partial until that manual is authored and followed in a clean staging copy.

## Learning Checks to include in the future manual

- Can the learner identify which settings distinguish the four configurations?
- Does each copied script use a unique `PROJ` so outputs remain isolated?
- Can the learner locate the generated comparison figures and identify which configurations they compare?
