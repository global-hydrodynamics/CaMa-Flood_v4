# Step 11 — Chat AI Discussion: Reviewing Tutorial Design and Planning the User Manual

**Guide:** `tutorial/guide01_how_to_develop_tutorial/`  
**Example:** `workflow_example/` — river-channel depth modification and flood-inundation analysis  
**Record type:** `chat` — summary of an actual researcher–Chat AI exchange, not a verbatim transcript  
**Status:** Manual-planning discussion; no tutorial implementation or scientific validation was performed in this step.

## 1. Context and inputs

Following the initial Chat AI design discussion (Steps 01–03), the researcher prepared an Agent AI prompt (Step 04) and used Codex to inspect the CaMa-Flood repository. The repository-informed tutorial specification was then provided to Chat AI for review and for discussion of how the *learner-facing manual* should be designed.

The reviewed document was titled **“Workflow Example — River-Depth Sensitivity and Flood-Inundation Analysis”** (`tutorial_design.md`, also supplied as the Step 06 revised design). It described a controlled experiment using the Mekong 15-minute map: the original `rivhgt` (Control) versus valid-cell `rivhgt + 3.0 m` (Deep Channel), with otherwise matched Tutorial02 `mek_default` settings, daily NetCDF output for calendar year 2000, analysis of four model outputs, and one-minute inundation downscaling.

Crucially, the document described a **repository-informed design, not an implemented or validated experiment**. The scripts, runs, figures, and derived analysis products had not yet been produced.

## 2. Researcher's request to Chat AI

The researcher asked Chat AI to:

> Review the completed Tutorial Design and consider how the tutorial manual should be designed.

After discussing the review and a proposed structure, the researcher requested:

> Draft the manual design document.

The discussion therefore had two connected goals: (1) check that the implementation-oriented design gives a defensible basis for a teaching exercise, and (2) translate that design into a **manual specification** rather than immediately writing an unverified, runnable README.

## 3. Main findings from the Tutorial Design review

### 3.1 Scientific relationships that should become learning objectives

The reviewed design tied the river-map parameter to model diagnostics:

- `channel-bed elevation = elevtn - rivhgt`
- `sfcelv = elevtn - rivhgt + rivdph`

Increasing `rivhgt` by 3 m lowers the model's channel-bed elevation by 3 m while keeping the channel-top elevation unchanged. A change in `rivdph` (water depth relative to the bed) does not have the same interpretation as a change in `sfcelv` (absolute water-surface elevation).

**Manual implication:** Rather than merely showing four plots, ask learners whether channel deepening increases channel depth, water-surface elevation, or both, and compare their predictions with the actual model outputs. No direction or sign of model response should be predetermined.

### 3.2 Distinct output definitions and temporal semantics

The specified daily NetCDF variables are:

| Variable | Meaning | Daily record semantics |
|---|---|---|
| `outflw` | Total river/floodplain discharge (m³/s) | Interval mean |
| `rivdph` | Channel water depth (m) | Snapshot |
| `sfcelv` | Water-surface elevation (m) | Snapshot |
| `fldfrc` | Flooded fraction of a unit catchment | Snapshot |

**Manual implication:** Teach variable definitions and the difference between a daily mean and a daily snapshot before interpreting peaks or differences. The selected Phnom Penh event is the peak of the **Control daily `sfcelv` snapshots**, not the peak of a daily-mean water level.

### 3.3 Flooded fraction is not flooded area

The specification defines cell flooded area as `fldfrc × ctmare`, with `ctmare` representing unit-catchment area in m². Regional flooded area is the sum over an explicitly chosen set of catchments, converted to km². `fldfrc` is useful for comparing fractions between cells, while flooded area is appropriate for regional aggregation.

**Manual implication:** Place a comparison of fractional and physical area before the regional flooded-area time series. Ask why two catchments with the same `fldfrc` can contribute different areas.

### 3.4 Downscaling is postprocessing, not a fine-grid hydrodynamic simulation

The existing tributary-aware one-minute downscaler accepts Binary coarse-grid floodplain-depth input. The revised design proposes an adapter that reads the selected NetCDF `rivdph` time slice and static `rivhgt`, computes `flddph = max(rivdph - rivhgt, 0)` for the specified physics, and passes it to the existing algorithm. The legacy Binary workflow should remain functional.

The existing coarse `prmwat` is a per-catchment permanent-water area, not a one-minute pixel mask, and is not an input to this downscaler.

**Manual implication:** Explain the physical meaning and limitations of downscaling, while keeping low-level NetCDF/Fortran implementation details out of the main learner-facing workflow. Compare both experiments on the **same Control-selected event date** and with the same spatial extent and plotting scales.

### 3.5 Uncertainties and checks still required

The discussion highlighted several points that must not be passed off as completed results:

- A shared initialization *procedure* does not necessarily imply identical physical initial storage or water level when channel geometry changes; the one-year, no-prior-restart experiment may retain cold-start effects.
- The Phnom Penh reference grid cell, one-minute crop, regional catchment mask, other observation points, and some plotting dates/statistics remain to be approved.
- The NetCDF downscaler input adaptation and regression checks are not yet implemented or executed.
- Structural test success and model sensitivity are not observational validation.

The researcher did not resolve those choices in this exchange; the manual design keeps them explicitly open.

## 4. Agreed direction for the manual

The proposed distinction is:

> **Tutorial Design** specifies what the developer must implement. **Tutorial Manual** guides what the learner should do, observe, and understand.

Chat AI recommended a **question-driven manual**, with a repeatable instructional sequence:

**Question → Concept → Run → Inspect → Interpret → Checkpoint**

Each practical chapter should pose a question, give enough scientific context, provide an actual tested operation, identify expected files or plots, and prompt the learner to interpret the outcome. It should not be a simple transcription of the implementation design or a list of shell commands.

The initial learner-facing manual should be **one navigable `workflow_example/README.md`**, linked to existing setup instructions and Tutorial02 rather than duplicating them. It should only be finalized after scripts, output products, and figures exist and have been checked.

## 5. Proposed manual chapters

The discussion led to a nine-chapter learner workflow:

| Chapter | Main educational question / activity |
|---|---|
| 1. Introduction | What might a deeper river channel change? Form hypotheses without assuming the answer. |
| 2. Preparation | Which map, runoff, input matrix, model, and environment are needed? |
| 3. Modify River Depth | How can `rivhgt` be increased safely without corrupting the original map or missing mask? |
| 4. Run Two Simulations | How do matched Control and Deep Channel experiments isolate the intended parameter change? |
| 5. Inspect Model Outputs | What do `outflw`, `rivdph`, `sfcelv`, and `fldfrc` mean, including their time semantics? |
| 6. Compare River Dynamics | How do channel depth and absolute water-surface elevation respond differently? |
| 7. Flooded Fraction and Area | How do `fldfrc`, cell flooded area, and regional flooded-area totals differ? |
| 8. Spatial Sensitivity and Downscaling | Where does the response occur, and what does one-minute postprocessing add? |
| 9. Discussion and Exercises | What conclusions are justified, and what remain limitations or possible extensions? |

The emphasis was on a coherent sequence of scientific questions, not merely a sequence of scripts.

## 6. Figure plan discussed

The six figures in the implementation design were retained, with educational purposes assigned:

1. **Map parameter modification:** Original `rivhgt`, modified `rivhgt`, and valid-cell difference; establish exactly what changed.
2. **Four model outputs:** Spatial fields of `outflw`, `rivdph`, `sfcelv`, and `fldfrc`; teach their distinct physical meanings and units.
3. **River-dynamics time series:** Matched Control/Deep Channel discharge, channel depth, and surface elevation; distinguish depth from elevation.
4. **Flooded fraction versus flooded area:** Contrast `fldfrc` and `fldfrc × ctmare`, then aggregate area over the documented region.
5. **Spatial sensitivity:** Deep Channel minus Control differences; visualize actual response without assuming its sign.
6. **One-minute inundation:** Show both experiments on the same Control-selected Phnom Penh peak day using the same crop and plotting scale.

Figure captions should specify time, units, experiment comparison, and relevant limitations. Plot scripts should regenerate figures from saved analysis products rather than rerunning the model invisibly. No actual figure content or direction of change was verified in this conversation.

## 7. Manual authoring and validation approach

The discussion recommended postponing the **final** user manual until implementation has produced reliable evidence. The proposed sequence was:

1. Resolve or document researcher-controlled choices: reference grid cell, analysis region, downscaling crop, and representative dates/locations.
2. Implement and check the map-parameter change and two matched model runs.
3. Implement analysis products and the initial figures.
4. Implement and test NetCDF input for the existing downscaler, including compatibility with the Binary path.
5. Write the user-facing README from commands actually executed and files actually generated.
6. Let a beginner or student follow the initial manual and record difficulties and scientific questions.

The first working version should be **scientifically defensible, reproducible, and understandable, but not necessarily cosmetically polished**. This is the purpose of Guide01's `workflow_example`: illustrate what the structured AI-assisted workflow produces *before detailed manual tuning*.

## 8. Outcome of this discussion

Based on the review and the subsequent request to draft a manual design, Chat AI prepared an English Markdown draft titled:

**`tutorial03_manual_design_draft.md` — “Guide01 Workflow Example — Tutorial Manual Design (Draft)”**

The draft formalized:

- The central learner question and learning outcomes.
- The nine proposed learner-facing chapters.
- The `Question → Concept → Run → Inspect → Interpret → Checkpoint` teaching pattern.
- The six-figure educational contract.
- Rules for writing from verified scripts and outputs.
- Open choices and acceptance criteria for the eventual manual.

The intended final user manual is **`tutorial/guide01_how_to_develop_tutorial/workflow_example/README.md`**. The manual-design draft is a separate planning artifact, not the runnable user manual itself.

## 9. Why this step belongs in Guide01

This record illustrates a separate development activity between *repository-informed technical design* and *implementation*: reviewing the technical specification from the learner's perspective and deliberately designing the manual's teaching logic.

It shows that Chat AI can help turn a technical specification into educational questions and a draft chapter/figure plan, while the human researcher retains responsibility for scientific interpretation and unresolved choices. Agent AI can later implement and test the workflow, but neither a plausible design nor successfully executing code alone establishes the scientific validity of the tutorial.

**Next artifact:** The manual-design draft can be preserved as the next numbered `draft` file directly under the Guide01 directory, with its final filename assigned according to the chronological Guide01 workflow. This record is **Step 11** and does not imply that the manual has already been implemented.
