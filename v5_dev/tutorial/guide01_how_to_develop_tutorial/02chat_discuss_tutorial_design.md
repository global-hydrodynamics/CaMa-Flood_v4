# Chat 02 — Discussing the Tutorial Design with AI

**Example project:** CaMa-Flood v5, `tutorial03-rivhgt_modification`  
**Purpose:** Illustrate how a researcher and Chat AI turn an initial, partly open-ended tutorial idea into a more concrete design.  
**Source:** Condensed and translated from the actual conversation following [`01chat_ask_rough_tutorial_design.md`](01chat_ask_rough_tutorial_design.md). This is a **dialogue summary**, not a verbatim transcript or a finalized implementation specification.

## 1. What happened after the initial request?

The researcher first asked ChatGPT to draft a tutorial about modifying river-channel depth, running two simulations, analyzing several output variables, and visualizing flooding and downscaling results. Importantly, the researcher instructed the AI to **ask questions rather than arbitrarily resolve ambiguities**.

ChatGPT drafted a structured tutorial design with learning objectives, map preparation, simulations, data analysis, visualization, reproducibility checks, and a list of pending decisions. It then asked the researcher to clarify key design choices.

## 2. AI questions and researcher decisions

The table below summarizes the substantive design dialogue. The first four questions were presented as multiple-choice design decisions; the researcher subsequently specified the numeric depth change and the event-selection method.

| Topic | Question or uncertainty raised by Chat AI | Researcher's response | Resulting design decision |
|---|---|---|---|
| River-depth modification method | Should the channel depth be multiplied, increased by a constant, or modified only in a selected region? | Add a constant depth to **all valid river-channel cells**. | Use an additive change, not a multiplicative or geographically selective change. |
| Reference experiment | Should Control use the default configuration from Tutorial01, Tutorial02, or another setup? | Use the **`mek_default` physical configuration from Tutorial02**. | Both Tutorial03 experiments share Tutorial02 Default physics; the intended parameter difference is channel depth only. |
| Output frequency | Should the model write daily or hourly NetCDF output? | **Daily NetCDF**, as in Tutorial02. | Write `outflw`, `rivdph`, `sfcelv`, and `fldfrc` at the same daily output interval. |
| Downscaling resolution | Should inundation downscaling use 1-minute, 15-second, or another resolution? | Start with **1-minute resolution**. | Use existing CaMa-Flood 1-minute downscaling functionality, subject to inspection of the actual tools and inputs. |
| Magnitude of depth increase | How many meters should be added? An initial draft left the increment as `Δh` pending a researcher decision. | **Add 3 m uniformly**; the researcher expects this to produce a substantial response. | `rivhgt_deep = rivhgt_control + 3.0 m` for valid river-channel cells. Preserve the original file and invalid-cell masks. |
| Downscaling date | Which date should be used to compare the two experiments? | Use **the date of maximum water-surface elevation in 2000 near Phnom Penh in the Control experiment**. | Identify the Control peak day from the local `sfcelv` time series; downscale **both experiments on that same day**. |

### Example: from an open question to an explicit specification

**Initial intention:** “Prepare a deeper river-channel parameter.”

**AI follow-up:** “Should channel depth be multiplied by a factor, increased by a fixed depth, or changed only in selected areas?”

**Researcher decision (first round):** “Add a constant depth to all river channels.”

**Remaining ambiguity:** The constant had not been specified.

**Researcher decision (second round):** “Add 3 m.”

**Implementable specification:** Increase `rivhgt` by exactly 3.0 m in every valid river-channel cell while preserving the original data, dimensions, coordinates, metadata, and missing values.

This illustrates why design clarification can take more than one round. A method can be decided while a numerical parameter remains open.

## 3. Scientific reasoning discussed during refinement

The AI also highlighted distinctions that should inform the eventual tutorial rather than be treated merely as programming choices:

- **River depth versus water-surface elevation:** Deepening the channel changes its bottom elevation and storage/conveyance behavior. `rivdph` may increase while `sfcelv` decreases. These are distinct quantities and their responses must be checked from simulation results.
- **Sensitivity hypotheses are not guaranteed outcomes:** The researcher expects deeper channels to produce larger discharge peaks and less flooding, but the figures should demonstrate what the simulations actually show rather than encode a predetermined conclusion.
- **Flooded area versus flooded fraction:** `fldare = fldfrc × ctmare`. The dimensionless `fldfrc` is often more meaningful for comparing inundation between grid cells, while `fldfrc × ctmare` is needed to calculate a regional flooded-area total.
- **Fixed comparison date:** Selecting the peak-water-level day using the Control simulation and then comparing the two experiments on that same date avoids comparing two different events. The local water-level peak is not necessarily the date of maximum basin-wide inundation.
- **Workflow separation:** Prepare the modified map under `map/mekong_15min/`, run the experiments under Tutorial03, produce derived analysis datasets, and only then generate the figures. Changing a visualization should not require rerunning the model.
- **Initial conditions:** A single simulation year (2000) may retain cold-start effects. The exercise is a controlled one-year sensitivity demonstration, not proof of equilibrium sensitivity.

## 4. Design decisions already fixed by the conversation

By the end of these exchanges, the following design was agreed:

- Tutorial directory: `tutorial/tutorial03-rivhgt_modification/`.
- Parameter preparation is performed separately in `map/mekong_15min/`.
- Two experiments: Control and Deep Channel.
- Both use the Tutorial02 `mek_default` physical settings and the same forcing and simulation period.
- Simulation year: **2000 only**, including leap day.
- The only intended scientific parameter difference is `rivhgt`, increased by **3.0 m** in the Deep Channel experiment.
- Daily NetCDF outputs: `outflw`, `rivdph`, `sfcelv`, and `fldfrc`.
- Derived analysis comes **before** visualization and is a separate stage.
- Include spatial and time-series sensitivity analyses, a lower-Mekong flooded-area aggregation, permanent-water (`prmwat`) interpretation, and a downscaling example.
- Downscaling target: **1 minute**, on the Control experiment's 2000 peak-`sfcelv` day near Phnom Penh, using the same date for both experiments.

## 5. Questions not yet resolved

The conversation **did not** establish all implementation details. These should remain explicitly open rather than be guessed by an AI coding agent:

1. **Phnom Penh reference cell:** Which exact model cell, station, or allocation rule defines “near Phnom Penh” for choosing the peak-water-level date?
2. **Downscaling map extent:** What geographic box around Phnom Penh should be displayed?
3. **Regional inundation aggregation:** What is the exact lower-Mekong/Delta analysis region, and how are catchments that intersect its boundary handled?
4. **Permanent water:** Should `prmwat` be used only as a visualization layer, or should the analysis distinguish temporary inundation from permanent water quantitatively? Its actual representation and use in the downscaling workflow must be checked.
5. **Temporal summary plots:** Which dates and statistics, beyond the defined peak day and annual maximum discharge, best illustrate `rivdph`, `sfcelv`, and `fldfrc` responses?
6. **Implementation details:** The names and locations of the modified map product, analysis artifacts, and appropriate existing downscaling tools must be determined against the current repository.

One terminology point should also be checked against the code: the researcher's first request mentioned `rivpht`, while the subsequent design used **`rivhgt`** as the river-channel depth map variable. Do not silently invent a separate `rivpht` workflow.

## 6. Lessons for a tutorial author

This example illustrates a practical **human–AI design process**:

1. **Start with the educational and scientific goals.** The first prompt can be informal and incomplete.
2. **Ask AI to organize the plan**, including learning outcomes, experimental setup, analysis, figures, validation, and unresolved questions.
3. **Have AI expose choices rather than make silent assumptions.** Ask for options and their scientific or technical consequences.
4. **Let the researcher decide scientific parameters and interpretations.** Here, the researcher chose the +3 m perturbation, the baseline model, the output cadence, and the event definition.
5. **Keep a visible list of open issues.** A draft is not an implementation-ready specification until the critical decisions are resolved or explicitly delegated for investigation.
6. **Only then ask a coding agent to inspect the repository and implement the design.** Successful execution and scientific correctness both require verification.

**Key principle:** *AI helps make decisions explicit and organizes implementation work; the researcher remains responsible for the scientific objectives, parameter choices, and interpretation.*

## 7. Next step in this example

Use the agreed decisions to update `tutorial03-rivhgt_modification/tutorial_design.md`. Resolve or document the remaining choices, then ask Codex to inspect the existing map-generation, model-output, analysis, and downscaling tools before writing new implementations.

The design document and the eventual beginner-facing tutorial README serve different purposes: the former records **what and why to build**, while the latter explains **how to run and interpret an actually verified workflow**.
