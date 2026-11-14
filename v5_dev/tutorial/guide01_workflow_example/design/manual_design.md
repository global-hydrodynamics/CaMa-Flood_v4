# Manual Design — Guide01 Workflow Example

**Audience:** tutorial authors and model users familiar with editing scripts, but new to this sensitivity workflow. The learner-facing workflow is deliberately incomplete at the start; completed reference files remain in `design/` for developers and are not part of a learner package.

## Learning sequence

| Stage | Learner operation | Starting material | Artifact to create | Completion evidence |
| --- | --- | --- | --- | --- |
| Prepare the map parameter | Inspect `s01-prepare_map.py` and change `RIVHGT_INCREMENT_M` from `0.0` to `3.0`. Run the script. | Editable map-preparation script and shared `rivhgt.nc`. | `map_modify/rivhgt_deep.nc`. | Reported dimensions, valid/missing counts, unchanged grid/mask, and configured increment. |
| Create an experiment configuration | Copy `run-experiment.sh`, assign a new `PROJ`, and change `RIVHGT` to the tutorial-local map. | Working Control GOSH configuration. | Learner-created `run-deep-channel.sh`. | Namelist uses the created map and writes to a different project directory from Control. |
| Run and inspect | Run Control and the learner-created Deep Channel configuration, then run the supplied analyses and downscaler. | Shared inputs, map variant, and two independent configurations. | Model outputs, analysis products, and figures. | Both runs complete; analysis products and figures appear under the tutorial-owned output paths. |

The only intended scientific experiment difference remains the approved `+3 m` change to valid river-depth cells. Learners are not asked to change the scientific setting or infer an expected response. Their transferable skills are editing a map-generation setting, preserving shared inputs, and constructing an isolated GOSH experiment by changing `PROJ` and an input-path setting.

## Package state

Include the editable learner scripts and approved example images under `sample_figure/`. The starting package has no `map_modify/rivhgt_deep.nc`, no learner-created Deep Channel runner, no model outputs or analysis tables, and no generated figures under `figure/`. The complete map, runner, test, and analysis references are kept under `design/` in the GitHub repository. Generated outputs and modified map data are ignored by Git.

No separate tutorial package assembler exists. A temporary learner snapshot can verify the intended file set, but a complete learner-follow-through has not been performed after this intentional incompleteness change; package reproducibility remains partial.

## Future Learning Checks

- Can the learner identify the `rivhgt` increment and produce a separate map file without modifying the shared map?
- Can the learner explain which `PROJ` and `RIVHGT` settings distinguish the copied experiment?
- Can the learner locate the two run directories and the generated comparison figures?
