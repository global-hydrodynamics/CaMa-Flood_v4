# Step 31 — Review and Refine the Workflow Example

**Guide01: How to Develop CaMa-Flood Tutorials with AI Assistance**  
**Phase:** Human review, iterative improvement, and learning from the process  
**Target:** `tutorial/guide01_how_to_develop_tutorial/workflow_example/`

## 1. Purpose

The initial AI-assisted development cycle has produced a runnable example: a modified river-depth map, two simulations for 2000, analysis products, one-minute inundation postprocessing, six figures, and an initial learner manual (to be written or completed in the preceding step).

**Your task is to review and improve the scientific and educational quality of that first version.** The goal is *not* to build an entirely new tutorial. Instead, identify weaknesses, discuss possible improvements with Chat AI, request focused changes from Codex, and verify whether those changes worked.

This phase is intentionally iterative:

1. **Read and inspect:** Follow the learner manual and examine the generated figures, analysis outputs, and relevant design documents.
2. **Identify a problem:** Is a scientific assumption questionable, a calculation unclear, a figure misleading, or an instruction difficult to follow?
3. **Discuss with Chat AI:** Explain the problem and share the relevant figure or documentation. Ask what could be improved, why, and what should be tested.
4. **Make a focused plan:** Choose one improvement at a time. Distinguish scientific decisions requiring the researcher's approval from implementation details.
5. **Ask Codex to implement:** Have Chat AI help draft a specific Codex prompt stating affected files, expected behavior, and validation requirements.
6. **Inspect the result:** Re-run only the necessary processing stage, compare before/after outputs, and check that existing functionality was not broken.
7. **Update documentation:** Ask Codex to revise the affected `tutorial_design.md`, reviewed manual-design document, and `README.md` as appropriate. Keep the historical `NNchat`, `NNdraft`, `NNprompt`, and `NNreport` records unchanged.
8. **Record what was difficult:** Write down uncertainties, mistakes, AI assistance, and lessons learned.

Repeat until the tutorial is scientifically sound, reproducible, and understandable to a new learner. Not every issue must be resolved by one person or in one session.

## 2. Start with a learner's review

Read the current `workflow_example/README.md` as if you had never used this example before. Check whether the commands can be followed from the stated working directory, whether every input and output has a clear purpose, and whether each figure answers a meaningful question. Refer to:

- `workflow_example/tutorial_design.md` — scientific and implementation requirements.
- The current reviewed manual-design document under `workflow_example/design/` — learning objectives and explanation sequence.
- `tutorial/README.md` and Guide01's authoring guidance — common tutorial conventions.
- `22implementation_record.md` and the Step 23 agent report — actual execution records and provisional choices.
- `workflow_example/figure/f01-*.png` through `f06-*.png` — initial results.

The original first version is valuable evidence. **Do not silently replace it with a polished version without recording what changed and why.** Check whether a snapshot or Git revision is needed before substantial edits.

## 3. Suggested improvement tasks

The examples below are a starting point, **not an approved list of scientific conclusions**. They are based on visible limitations in the initial six figures and on unresolved decisions in the design documents. Review the source scripts and saved numerical results before accepting any interpretation.

| Priority | Example task | What looks unclear or weak | Possible approach and acceptance check |
|---|---|---|---|
| Easy / start here | **Figure 1: comparable colors** | The original and +3 m river-depth maps should be directly comparable, but the current visual presentation makes the comparison less clear. | Inspect the plotting code. Apply the same physical color limits and colormap to the two absolute-depth panels. Keep the difference panel separate. Confirm its valid cells show +3 m and that missing cells are masked. Rebuild Fig. 1 only. |
| Easy–medium | **Figure 2: explain and improve the maps** | Large geographic extent and variable-dependent ranges hide details; readers may mistake all daily outputs for daily averages. | Check units and snapshot/interval-mean semantics. Consider a regional inset, clearer legends, or an explanatory caption without changing scientific calculations. Verify the represented day versus the interval-end timestamp. |
| Medium | **Figure 3: explain river response** | Depth, water-surface elevation, and discharge change differently at different locations. The lesson is important but not obvious to a beginner. | Check the actual reference-cell coordinates and provisional site selection. Use `sfcelv = elevtn - rivhgt + rivdph` to explain the difference between river depth and surface elevation. Review whether the plotted sites are representative; ask for researcher approval before treating them as final. |
| Medium–hard | **Figure 4: regional flooded area** | A rectangular region selected by cell centers is a provisional aggregation, not a definitive Mekong Delta boundary. The upper maps also provide similar-looking information. | Confirm `fldfrc × ctmare`, its units, and which catchments are included. Compare sensible region definitions and boundary treatments. Discuss the scientific implications with the researcher. Update Fig. 4 and its explanation only after agreeing on a rule. |
| Medium | **Figure 5: sensitivity visualization** | Some differences are spatially localized, and color scales are clipped at percentiles; readers may confuse clipping with numerical limits. | Inspect the plotted quantity, date, and sign (`Deep Channel − Control`). Consider consistent zero-centered diverging colors and explicit annotations for clipping. Check for saturated pixels, mask mismatches, and misleading scale comparisons. |
| Medium–hard | **Figure 6: interpret the downscaling** | Control and Deep Channel maps look very similar at the chosen color scale, so a reader cannot easily judge the actual difference. | Check numerical differences between outputs: maximum/mean absolute difference, number of affected pixels, and optionally a difference map or informative zoom. Confirm both use the same event, crop, and high-resolution map. Distinguish a real small difference from poor visualization. |
| Cross-cutting | **Manual usability** | A first manual can contain implicit prerequisites, unexplained scripts, or captions that state what is plotted without teaching its meaning. | Attempt a clean walkthrough. Note missing commands, unclear concepts, broken links, and expected outputs that are not explained. Ask Codex to correct only the verified documentation problems. |

**Recommended first exercise:** Improve Figure 1's comparison scales and its manual explanation. This is bounded, easy to verify, and demonstrates the full Chat AI → Codex → review → documentation cycle. More scientific choices, such as a regional flooded-area mask, can follow after discussion with the researcher.

## 4. Scientific questions requiring special care

The initial results are from **two cold-start simulations for calendar year 2000**, not observational validation. The only intended scientific input change is `rivhgt + 3.0 m` in valid cells. Keep these conditions intact unless the researcher explicitly approves a new experiment.

- **Temporal meaning:** `outflw` is a daily interval mean, while `rivdph`, `sfcelv`, and `fldfrc` are daily snapshots. Stored time labels mark the *end* of the represented interval. Do not confuse a time coordinate with the displayed calendar day.
- **Channel geometry:** Increasing `rivhgt` lowers the calculated channel bed; it does not mechanically raise the absolute water surface. At the same grid cell, with unchanged `elevtn`, `Δsfcelv = Δrivdph − 3 m` for the two experiments.
- **Flooded area:** The regional sum is `Σ(fldfrc × ctmare)`, converted from m² to km². Results depend on the definition of the spatial mask and how boundary catchments are treated.
- **Provisional selections:** Phnom Penh and other reference cells, the event date derived from the Control series, the analysis region, and the downscaling crop were provisional at initial implementation. Verify and seek approval before presenting them as scientifically selected final choices.
- **Downscaling:** The initial implementation uses a Python adapter to prepare Binary floodplain-depth input from NetCDF; the existing Fortran downscaler was not rewritten to read NetCDF directly. Downscaled one-minute fields provide spatial detail, **not** a new one-minute hydrodynamic simulation or independent validation.
- **Physical interpretation:** A response that is plausible does not prove its underlying mechanism. Avoid claiming that storage, wave propagation, or floodplain exchange caused a particular change unless the analysis supports that conclusion.

If a suspected error affects numerical validity (rather than style), prioritize diagnosis before revising the figure or manual. Distinguish **incorrect calculation**, **unverified assumption**, **misleading display**, and **unclear explanation**.

## 5. How to use Chat AI and Codex

### Discuss the issue with Chat AI

Provide the relevant image, the manual passage, and (if needed) the design or analysis summary. Describe what is confusing, not only what you want changed. Useful questions include:

- What scientific question should this figure answer?
- Is the problem with the calculation, the selected data, the visualization, or the explanation?
- What evidence would distinguish competing interpretations?
- Which changes require the researcher's scientific approval?
- How can we formulate a small, testable Codex task?

### Delegate a focused change to Codex

Ask Chat AI to prepare an English Codex prompt that identifies:

- The exact existing scripts, products, and documentation to inspect.
- The smallest requested change and what must remain unchanged.
- Which processing stages need to run again (prefer not to rerun simulations for a plotting-only change).
- A concrete check showing that the improvement worked.
- Which working design documents and learner-manual sections must be synchronized.

After Codex reports completion, **review the result yourself**. A successful command is not proof of scientific accuracy, educational clarity, or an appropriate visualization.

## 6. Keep documents synchronized

There are three distinct working documents, with different roles:

| Document | When to update it |
|---|---|
| `workflow_example/tutorial_design.md` | When approved experiment settings, analysis definitions, datasets, or implementation requirements change. |
| Reviewed Manual Design in `workflow_example/design/` | When the learning objectives, chapter sequence, figure purposes, or teaching approach change. |
| `workflow_example/README.md` | When tested commands, actual outputs, figure descriptions, interpretation, or troubleshooting information change. |

Do not edit documents reflexively: a colormap-only change may require only its figure caption, while a different regional mask may require changes to all three. Always check consistency after a substantive revision.

Keep tutorial-generated outputs within `workflow_example/` wherever practical. The shared `map/mekong_15min/` and other repository inputs are read-only for this example. Exceptions to the general tutorial-local principle must be justified and documented before implementation.

## 7. Record difficulties and learning outcomes

**This is part of the assignment, not optional housekeeping.** Guide01 is itself being evaluated as a way to teach AI-assisted scientific development. We want to know which steps a new contributor can perform independently and which steps remain confusing.

For each substantial improvement task, keep a short note in a refinement log, for example `design/refinement_log.md` (or a separate file if this better matches the repository convention):

```markdown
### Improvement: [short name]
- **Problem noticed:** What was unclear, incorrect, or difficult?
- **Why it matters:** Scientific accuracy, reproducibility, visualization, or learning?
- **What I tried:** Discussions with Chat AI, Codex requests, tests, and changes.
- **Evidence:** Figure, script, test result, or comparison before/after.
- **Hardest part:** Understanding the science, deciding what to change,
  prompting AI, debugging, or checking the outcome?
- **Result:** Improved / partly improved / unresolved.
- **What I learned and still do not understand:** 1–3 sentences.
- **Guide01 improvement suggestion:** What instruction or example would
  have helped a beginner?
```

Keep it brief and candid. An unresolved question or failed attempt is useful evidence; there is no need to make every attempt look successful. If you need a scientific decision, record the question and ask the researcher instead of hiding it in a default setting.

## 8. Expected deliverables

By the end of this refinement phase, aim to provide:

1. **A more reliable and understandable workflow example:** improved analysis/figures where justified, and an updated learner-facing manual.
2. **Consistent working documents:** update `tutorial_design.md`, the reviewed Manual Design, and the manual wherever changes affect their content.
3. **A short change record:** what improved, what was tested, and which provisional scientific decisions remain open.
4. **A learner's evaluation of Guide01:** what was easy, what was difficult, how useful Chat AI and Codex were, and what guidance should be added for future students.

Do not attempt to resolve every issue at once. One well-documented improvement cycle is more valuable than several unverified cosmetic edits. The researcher retains responsibility for approving scientific choices and final interpretations.

**Central lesson:** AI can produce a functioning first implementation quickly, but reviewing the science, improving how results are communicated, and documenting remaining uncertainty are essential parts of scientific software development.
