# Step 15 — Reviewing Agent-Generated Designs with Chat AI

**Document type:** `chat` — summary of a researcher–Chat AI design discussion (not a verbatim transcript)  
**Example:** Guide01 `workflow_example`, river-channel-depth sensitivity tutorial  
**Stage:** After repository-informed Tutorial Design and Manual Design drafts; before implementation

## Purpose of this example

This step illustrates an important part of AI-assisted scientific tutorial development: **do not proceed directly from an Agent AI-generated design to implementation**. The researcher should review the design, discuss concerns with Chat AI, and revise the specification when the proposed workflow could have unintended effects.

Here, the concern was not the scientific experiment itself. It was **where the tutorial would create files and how those files would affect shared CaMa-Flood repository resources**.

## 1. What the researcher reviewed

The researcher examined the repository-informed documents prepared with Agent AI (Codex):

- `workflow_example/tutorial_design.md` — experimental settings, proposed scripts, data layout, analysis, and validation.
- The repository-informed Manual Design — how learners would prepare inputs, run the experiment, inspect outputs, and interpret results.

The experimental objective was already defined: compare Control and Deep Channel runs using the Mekong 15-minute map, changing only valid `rivhgt` values by **+3.0 m** in the Deep Channel case.

## 2. Concern discovered during review

The Tutorial Design proposed producing a modified NetCDF file, `rivhgt_deep.nc`, under the shared directory:

`map/mekong_15min/netcdf/`

The design also contained a proposed `workflow_example/s01-prepare_rivhgt.sh` script while other passages described map preparation under `map/mekong_15min/`. The proposed location and responsibility for generated files were therefore not completely consistent.

This raised a practical question: **Should a self-contained tutorial write its experiment-specific products into a shared map directory?**

Potential disadvantages included:

- Leaving tutorial-specific products outside the tutorial directory.
- Making it harder to identify which tutorial created a file.
- Creating opportunities for conflicts with other experiments or tutorials.
- Making cleanup and reproducibility less straightforward.

The existing baseline `rivhgt.nc` was never intended to be overwritten. The concern was about the location and ownership of the *new* file, not about the scientific +3 m modification.

## 3. Options discussed with Chat AI

The discussion compared three approaches:

| Option | Approach | Main consideration |
|---|---|---|
| A | Generate `rivhgt_deep.nc` in the shared `map/mekong_15min/netcdf/` directory | Follows the original map-data location but leaves tutorial-generated content in a shared directory. |
| B | Generate the file inside the tutorial and create a symbolic link from the shared map directory | Keeps the data local but still changes the shared directory and introduces link-management concerns. |
| C | Generate the file inside the tutorial and have the model reference it directly | Makes file ownership explicit and avoids shared-directory changes, provided the model supports the path. |

**Decision for Guide01 and Tutorial03: Option C.**

Use a `map_modify/` directory inside each tutorial to contain the modified map product. Keep preparation and run scripts at the tutorial root, consistent with the Tutorial02-style workflow.

Proposed layout for Guide01:

```text
tutorial/guide01_how_to_develop_tutorial/workflow_example/
├── s01-prepare_map.sh          # Proposed script; exact name to be confirmed
├── s02-run_control.sh          # Proposed
├── s03-run_deep_channel.sh     # Proposed
├── map_modify/
│   └── rivhgt_deep.nc          # Generated, tutorial-owned input
├── out/
├── analysis/
├── figure/
└── design/
```

The future independent Tutorial03 should use its own `map_modify/rivhgt_deep.nc`, rather than share the Guide01-generated product.

The Control simulation uses the unchanged shared `map/mekong_15min/netcdf/rivhgt.nc`. The Deep Channel simulation uses its own tutorial-local modified NetCDF file through `CRIVHGT`. Direct-path support and path resolution must be verified in the model code before implementation; they were not demonstrated by this discussion alone.

## 4. General design principle extracted from the discussion

The researcher noted that the concern extends beyond Guide01. It should be documented at the **`tutorial/README.md` level**, where it applies to all CaMa-Flood tutorials, rather than only in the Guide01 README.

**Proposed common principle:**

> When designing a tutorial, prefer keeping newly generated inputs and outputs within its own directory. Minimize unintended changes to shared repository resources. This is a design guideline, not an absolute prohibition; each tutorial may adopt a justified alternative.

For example, a future tutorial demonstrating how to activate a levee scheme might reasonably need to add levee-parameter maps under a shared map directory. The correct approach depends on that tutorial's scientific and technical goals.

When designing an exception, document what external files are affected, why the change is needed, how ownership is identified, how existing files are protected, and how the change can be reproduced or reversed.

## 5. Requested updates to the design documents

After the discussion, the researcher prepared an instruction for Codex to update the relevant documentation consistently:

1. Add the general guideline to `tutorial/README.md`.
2. Update `workflow_example/tutorial_design.md` so the modified map is generated under `workflow_example/map_modify/`, not under the shared map directory.
3. Update the repository-reviewed Manual Design to explain which inputs are shared read-only files and which products are tutorial-owned.
4. Verify that `CRIVHGT` can directly reference the tutorial-local NetCDF file, including path resolution from the run script.
5. Check Guide01's README and related descriptions for obsolete paths or contradictions.
6. Preserve the historical Chat AI and Agent AI drafts to show how the design evolved.

**Status clarification:** The discussion resulted in a decision and a Codex update prompt. This summary does **not** claim that Codex has executed that prompt or that updated repository files and direct-path support have already been verified. Those are implementation/review outcomes to check separately.

## 6. What did and did not change

**Changed in the proposed design:**

- Ownership and storage location of the generated `rivhgt_deep.nc`.
- Intended direct-reference mechanism from the Deep Channel configuration.
- Documentation policy for tutorial-generated files and external repository impacts.

**Unchanged:**

- The +3.0 m river-depth perturbation and preservation of missing values.
- Control versus Deep Channel scientific comparison.
- Model configuration, forcing, period, analysis objectives, and manual learning goals.
- The requirement to preserve the original shared map.

## 7. Lesson for Guide01 readers

**Review Agent AI outputs as design proposals, not as automatically approved specifications.** A technically plausible workflow may still create unnecessary dependencies, leave files in surprising places, or complicate future maintenance.

A productive review conversation can follow this pattern:

1. Read the Agent AI-generated design and identify an assumption or possible side effect.
2. Ask Chat AI to explain the consequences and compare alternatives.
3. Make a human decision based on the tutorial's purpose and repository conventions.
4. Turn that decision into a precise Codex instruction.
5. Verify the resulting documentation and, later, the actual implementation.

This example demonstrates that **scientific experiment settings can remain fixed while software organization and reproducibility improve through human–AI design review**.
