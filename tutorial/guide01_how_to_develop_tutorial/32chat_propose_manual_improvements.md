# Step 32 — Human Feedback to Chat AI: Improve the Tutorial Manual

This document is an example of feedback written by a researcher to Chat AI after reviewing the initial learner manual. The purpose is to ask Chat AI to examine the concerns, discuss how to improve the teaching approach, and prepare an appropriate Codex instruction. It is **not** a direct instruction to Codex, and the requested changes have not yet been implemented.

## Example message from the researcher to Chat AI

I reviewed the initial tutorial manual and found that Sections 3 and 4 need more concrete, code-level explanations. The tutorial should help users learn how to modify CaMa-Flood themselves, rather than simply run scripts prepared for them.

### Section 3 — Create the Deep Channel map

Please expand this section so that users can understand the origin of the default `rivhgt` parameter and how to modify it independently in future experiments.

In particular:

- Explain **where and how the default `rivhgt` map is generated** in the CaMa-Flood map-preparation workflow. Identify the relevant script(s) under `map/tools/script_params/` and the part of the code that determines river-channel depth.
- Show **which calculation or code section would need to change** to produce a river-depth map increased by 3 m. Distinguish changing the original parameter-generation formula from modifying an existing `rivhgt` NetCDF map.
- Explain why this tutorial deliberately **does not modify the shared `map/` products**. Instead, it generates its experiment-specific map in `workflow_example/map_modify/`, preserving the original map as a read-only baseline.
- Include and explain the actual contents of `s01-prepare_map.py` (or the relevant essential code sections if the complete script is too long). Compare its operation explicitly with the original parameter-generation scripts and formulas. Readers should understand *what differs*, *why*, and *what remains unchanged*.
- Explain how to verify that the new map contains `rivhgt + 3 m` only at valid cells while preserving the missing-data mask and the original file.

The goal is not merely to tell users which command to run. They should understand enough to design their own map-parameter modification afterward.

### Section 4 — Run the two simulations

Users are probably not yet familiar with how GOSH scripts select map parameters. Please make the relevant changes much more explicit.

- Show **the exact part of the GOSH script or generated namelist** where the river-depth map is specified, including `CRIVHGT` and how its file path is resolved.
- Explain precisely which settings are shared between Control and Deep Channel and which setting changes. Include a short, concrete before-and-after code comparison.
- I would prefer a hands-on exercise in which the learner **copies the Control runner (`s02`) to create the Deep Channel runner (`s03`) and edits the appropriate map-path setting** rather than receiving both finished scripts without explanation. Consider whether this can be implemented cleanly using the current `run-experiment.sh` interface and repository conventions.
- If the actual implementation uses a different runner structure, first inspect it and propose the smallest educationally useful change. Do not invent filenames or imply that `s02` and `s03` already exist if they do not.
- Explain how learners can verify that both experiments use otherwise identical settings and that the Deep Channel run reads the newly generated map.

### Requested next step

Please review these suggestions against the Tutorial Design, Manual Design, current README, and actual map-generation and experiment-runner code. Identify any misunderstandings or technical constraints, and recommend how to turn Sections 3 and 4 into practical, self-contained learning exercises.

Then prepare an **English prompt for Codex** to revise the tutorial manual and, only where necessary, make minimal supporting script changes. Preserve the approved scientific experiment and the shared read-only map. Do not start a broad redesign of the analysis or figures at this stage.

I would like to discuss your proposed improvements before sending the resulting prompt to Codex.
