# Chat 04 — Prompt to Refine the Tutorial Design Using Codex

**Purpose:** Example prompt for asking Codex to inspect the actual CaMa-Flood repository and refine a Chat AI-generated tutorial design **before implementation**.

**Input:** [`03draft_first_tutorial_design.md`](03draft_first_tutorial_design.md)  
**Intended output:** `tutorial/tutorial03-rivhgt_modification/tutorial_design.md` (revised design, not implemented code)

> This is an example of the **next instruction to Codex**, not a claim that Codex has already carried out the investigation. It follows the initial request (`chat01`), design discussion (`chat02`), and post-discussion draft (`03draft`).

---

## Prompt to send to Codex

### Task: Review the repository and refine the Tutorial03 design document

We have drafted a design for **Tutorial03: River Channel Depth Modification and Flood Inundation Analysis** using Chat AI. The draft captures the learning objectives and researcher-approved scientific choices, but the details have **not yet been checked against the actual CaMa-Flood v5 code, datasets, and established tutorial conventions**.

Your task is to **investigate the current repository and produce an improved, implementation-ready tutorial design document**. **Do not implement the tutorial yet.**

#### 1. Read the design and identify the established decisions

Start with:

- `tutorial/guide01_how_to_develop_tutorial/03draft_first_tutorial_design.md`
- `tutorial/guide01_how_to_develop_tutorial/02chat_discuss_tutorial_design.md` (for decision rationale)
- `tutorial/tutorial02-mekong_netcdf_4exp/tutorial_design.md` and the current Tutorial02 scripts, as relevant

Treat the following as **researcher decisions**, not suggestions to optimize or silently replace:

- Use the Mekong 15-minute map at `map/mekong_15min/`.
- Prepare a new river-depth map parameter in the **map directory**, not in the tutorial directory.
- Compare **Control** and **Deep Channel**, using Tutorial02 `mek_default` physics, numerical choices, runoff, and initial-state treatment for both.
- The only intended experimental parameter change is **+3.0 m to `rivhgt` for all valid river cells**; leave original files unchanged.
- Simulate **calendar year 2000 only** (including leap day), with **daily NetCDF** outputs for `outflw`, `rivdph`, `sfcelv`, and `fldfrc`.
- Keep **map preparation → simulation → data analysis → visualization** as separate stages.
- Explain and demonstrate `fldare = fldfrc × ctmare`; use `fldfrc` for comparisons of cellwise inundation and flooded area for regional totals.
- Include a regional inundated-area time series in the lower Mekong, sensitivity figures, and a `prmwat` visualization example.
- Demonstrate **1-minute inundation downscaling** for **both experiments on the same date**, selected from the **maximum daily `sfcelv` near Phnom Penh in the Control experiment in 2000**.

The proposed physical responses (greater discharge peaks, altered water depth and surface elevation, reduced inundation) are **hypotheses to examine**, not required or guaranteed outcomes.

#### 2. Investigate the actual repository before choosing implementation details

Inspect the **current working tree and branch**; do not rely on filenames or descriptions from an earlier conversation without verification.

Investigate at least these areas:

**Map parameters and processing**

- The actual path, NetCDF schema, dimensions, variable name, units, fill values, mask conventions, and meaning of `rivhgt` (including the `rivpht` typo in the initial request).
- How `rivhgt` is selected in GOSH (including the correct `CRIVHGT` / map filename convention).
- Which cells are valid river cells, and how to preserve original values and metadata safely.
- Existing map processing tools, conventions, variable registries, README files, and whether an appropriate place for the new parameter-generation script already exists.
- The meanings and available data for `elevtn`, `ctmare`, high-resolution map products, and `prmwat`.

**Experiment configuration and execution**

- Tutorial02 `mek_default` GOSH configuration, numerical settings, time period, forcing paths, build/executable requirements, and output conventions.
- How to reproduce its 2000 Control run without introducing unintended differences or modifying Tutorial02.
- How a second GOSH experiment selects the modified `rivhgt` file without copying the entire map.
- Existing script naming, execution order, output paths, repeatability, and Git-ignore conventions.

**Output and analysis**

- `variable_table/output_variables.csv` and actual model output code for `outflw`, `rivdph`, `sfcelv`, `fldfrc`; verify units, temporal averaging vs snapshots, time coordinates, and missing-value conventions.
- The definition and units of `ctmare`, and whether the intended `fldfrc × ctmare` calculation matches model diagnostics and the area interpretation.
- Existing approaches for extracting a time series at a point, choosing a Phnom Penh cell, and selecting an area mask for regional aggregation.
- How to separate reusable analysis outputs from plotting so figure regeneration does not recompute expensive processing.

**One-minute downscaling and permanent water**

- Find the **actual existing** CaMa-Flood downscaling tools and their documented usage; do not invent an independent downscaling algorithm.
- Identify required high-resolution files, input variables, temporal conventions, output format, resource needs, and existing examples.
- Verify how `prmwat` is defined, what its units and coverage mean, and how it can be displayed without double counting or misinterpreting permanent water.
- Check whether using the Control peak-`sfcelv` day as a downscaling input is feasible with daily snapshots and the available tools.

Use the repository as the source of truth for **implementation facts**, but keep researcher-approved **scientific choices** unchanged unless you explicitly flag a contradiction.

#### 3. Revise the design document, not the code

Produce a revised English Markdown design at:

`tutorial/tutorial03-rivhgt_modification/tutorial_design.md`

If this file already exists, inspect it and preserve any useful, current material. Do not overwrite unrelated work without reviewing the diff. Create the directory/document only if needed.

The revised document should be concrete enough for a student (Clara) to implement with Codex assistance. Include:

1. Purpose, prerequisites, and learning outcomes.
2. Explicit Control vs Deep Channel settings and the +3 m map transformation.
3. Verified map data paths and the proposed parameter-preparation workflow.
4. Simulation steps, verified GOSH settings, required daily outputs, and proposed filenames.
5. A separate analysis stage: variable statistics, time series, `fldfrc` vs `fldare`, regional flooded-area aggregation, and event-date selection.
6. One-minute downscaling workflow and permanent-water interpretation, based on available tools.
7. A proposed figure plan and the scientific question each figure teaches.
8. Expected directory layout, script order, generated artifacts, and Git-tracking policy.
9. Testing and validation criteria for each stage, including unit checks and scientific interpretation.
10. **Open decisions, unavailable inputs, risks, and questions for the researcher**.

Prefer existing CaMa-Flood naming conventions over inventing new ones. Suggest specific filenames where the conventions support them, but distinguish these proposals from established files.

Design a **small, clear beginner workflow**: do not create unnecessary abstractions or frameworks. Avoid duplicating tools already available in `map/`, `tutorial/`, or `tools/`.

#### 4. Distinguish facts, proposals, and unresolved questions

This distinction is essential:

- **Verified:** Supported by inspected repository code, registry, documentation, or data.
- **Proposed:** An implementable recommendation based on verified conventions, but not yet approved.
- **TBD / researcher decision needed:** A scientific or pedagogical choice that the repository cannot determine.
- **Blocked / not verified:** A required file, tool, or behavior that could not be inspected or tested.

Do **not** quietly choose a Phnom Penh reference cell, a map bounding box, a lower-Mekong aggregation region, or a policy for using `prmwat` quantitatively if these remain researcher decisions. Offer concrete options and explain trade-offs, then leave them open for review.

If repository findings contradict the draft, report the discrepancy clearly; do not change the scientific goal or fabricate a workaround.

Provide file paths and relevant function, variable, or configuration names supporting important technical claims. Do not invent successful tests or simulation outputs.

#### 5. Make the draft teachable

The design should guide an introductory user through **why** each step matters, not merely list commands. For example:

- Why preserve the original `rivhgt` map?
- Why must the two experiments differ only in river depth?
- Why can `rivdph` increase while `sfcelv` decreases?
- Why plot `fldfrc` on a map but sum `fldfrc × ctmare` for regional flooded area?
- Why select the event date from Control and use that same date for both runs?
- What can and cannot be concluded from a one-year, potentially cold-start experiment?

Clara will use this design to develop scripts and, later, a **separate user-facing tutorial manual**. Do not mistake the developer design document for the final beginner README.

#### 6. Deliver a review summary

In your response, provide:

- Files and code locations investigated.
- The revised design-document path.
- Major corrections or additions compared with `03draft_first_tutorial_design.md`.
- A compact list of **researcher decisions still needed**, prioritized by whether they block implementation.
- Missing data, tool uncertainties, or validations that could not be completed.
- A proposed sequence of **small implementation tasks** for a later Codex session, without executing them now.

**Do not run model simulations, generate large datasets or figures, modify scientific source code, or implement the tutorial in this task. Do not commit or push automatically.** Leave the revised design document for researcher review.

---

## Teaching note: Why this step is separate

The initial Chat AI design establishes **what the tutorial should teach**. Codex then inspects the real repository to determine **how the intended workflow can be implemented**. The researcher reviews contradictions and open scientific choices **before** requesting code changes. This prevents a plausible-looking but technically incorrect design from becoming the implementation specification.
