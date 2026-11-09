# Task: Review and Refine the Tutorial Manual Design

## Repository and input

Repository: `global-hydrodynamics/CaMa-Flood_v4`  
Working branch: `feature/gosh-refactor`

Attached input document:

`12draft_chat_generated_manual_design.md`

Primary reference:

`tutorial/guide01_how_to_develop_tutorial/workflow_example/tutorial_design.md`

Output file:

`tutorial/guide01_how_to_develop_tutorial/workflow_example/design/manual_draft.md`

## 1. Background

We are developing Guide01, a practical guide demonstrating how to create scientific tutorials and user manuals with assistance from Chat AI and Agent AI.

The `workflow_example/` directory uses a simplified river-channel-depth sensitivity experiment as a worked example.

The development process so far is:

1. The researcher discussed the initial scientific and educational objectives with Chat AI.
2. Chat AI produced an initial Tutorial Design.
3. Codex inspected the repository and refined the technical specifications.
4. The researcher and Chat AI reviewed the revised Tutorial Design and discussed how to structure a learner-oriented manual.
5. Chat AI produced `12draft_chat_generated_manual_design.md`.

Your task is to review this manual-design draft against the actual repository and the current Tutorial Design, then produce a technically consistent and educationally effective revised manual-design document.

## 2. Review the existing materials

Before editing, inspect:

- The attached `12draft_chat_generated_manual_design.md`.
- `workflow_example/tutorial_design.md`.
- `workflow_example/README.md`, if present.
- The Guide01 README and existing development records.
- Tutorial02's README, scripts, figures, and directory conventions.
- Relevant CaMa-Flood map, GOSH, output-variable, analysis, visualization, and downscaling implementations.

Check whether the proposed manual structure is consistent with the actual model workflow and existing repository conventions.

Do not assume that a script or dataset exists simply because it is mentioned in a draft.

## 3. Review the scientific and technical consistency

Pay particular attention to:

- The Control and Deep Channel experiments, with `rivhgt + 3.0 m` as the only intended scientific input difference.
- The relationship among `rivhgt`, `rivdph`, channel-bed elevation, and `sfcelv`.
- Daily mean `outflw` versus daily snapshot `rivdph`, `sfcelv`, and `fldfrc`.
- The distinction between flooded fraction (`fldfrc`) and flooded area (`fldfrc × ctmare`).
- The one-year 2000 experiment, including leap-day handling and cold-start limitations.
- Selection of a common downscaling date from the Control experiment's peak `sfcelv` near Phnom Penh.
- The existing binary downscaling implementation and the planned NetCDF input extension.
- The role and limitations of `prmwat`.

If the attached Manual Design conflicts with the current Tutorial Design, identify and explain the discrepancy.

**Treat `tutorial_design.md` as the authoritative scientific and implementation specification.**

Do not silently change approved experimental conditions.

## 4. Improve the educational structure

The manual should be designed for new CaMa-Flood users, including students with limited experience in scientific model development.

Preserve the question-driven learning approach.

Each practical section should, where appropriate, follow:

**Question → Concept → Run → Inspect → Interpret → Checkpoint**

The manual should teach users not only how to execute scripts but also how to understand the model response.

Review whether:

1. The learning objectives are clear.
2. The order of chapters follows a reproducible workflow.
3. Every command or analysis activity serves an educational purpose.
4. Each proposed figure answers a specific scientific question.
5. The distinctions among model variables are explained at appropriate stages.
6. Users are encouraged to form hypotheses before viewing results.
7. Scientific interpretation and model limitations are communicated accurately.
8. The manual remains manageable for a beginner rather than becoming a comprehensive technical reference.

Keep the proposed six-figure structure where appropriate, but recommend adjustments if needed for better scientific or educational clarity.

## 5. Distinguish verified information from planned implementation

The workflow example has not yet been fully implemented.

Therefore:

- Do not invent working commands, file names, model results, or figures.
- Do not present proposed scripts as existing files.
- Do not claim simulations or downscaling have been validated.
- Use clear placeholders or TODO notes where actual implementation details are still required.
- Identify information that can only be added to the final manual after implementation and testing.

The revised document should be a **manual design specification**, not the finished user-facing manual.

## 6. Produce the revised Manual Design

Create or overwrite:

`tutorial/guide01_how_to_develop_tutorial/workflow_example/design/manual_draft.md`

Write the document in clear English.

It should include:

1. Purpose, audience, and learning objectives.
2. Relationship to the scientific `tutorial_design.md`.
3. Educational principles and the question-driven chapter format.
4. Proposed manual chapter structure.
5. Learning questions, practical activities, expected observations, and interpretation points for each chapter.
6. Figure specifications and their educational purposes.
7. Requirements for documenting scripts, inputs, outputs, and reproducibility.
8. Scientific limitations and interpretation cautions.
9. Requirements for validating the finished manual.
10. Open questions and information needed before final manual writing.

Make the document sufficiently specific to guide the subsequent Codex implementation and manual-writing tasks.

However, avoid unnecessarily prescribing implementation details already governed by `tutorial_design.md`.

## 7. Preserve the development history

Do not modify:

`12draft_chat_generated_manual_design.md`

This file preserves the Chat AI-generated draft before repository review.

The revised `manual_draft.md` should reflect the results of your repository investigation.

Do not modify the scientific `tutorial_design.md` unless explicitly instructed. Report any issues that require changes to it.

## 8. Report the changes

After creating the revised document, summarize:

- Which repository files and conventions you inspected.
- Important corrections or clarifications made to the Chat AI draft.
- Educational improvements to the proposed manual structure.
- Any unresolved scientific or technical issues.
- Information that must be supplied after the implementation and test runs.

This summary will be used as part of the Guide01 development history.

## 9. Scope restrictions

Do not:

- Implement simulation, analysis, or visualization scripts.
- Modify CaMa-Flood model code.
- Run the numerical experiments.
- Implement the NetCDF downscaling extension.
- Generate figures.
- Write the final learner-facing README.
- Modify the original Step 12 draft.
- Commit or push automatically.

This task is limited to repository-informed review and revision of the Manual Design.
