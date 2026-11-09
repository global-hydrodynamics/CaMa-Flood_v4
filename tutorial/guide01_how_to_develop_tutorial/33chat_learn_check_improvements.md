# Step 33 — Discuss Improvements to the Learning Checks with Chat AI

The current manual includes learning checks such as:

> - Which inputs are shared repository data, and which map file will this example generate?
> - Why should the shared `rivhgt.nc` remain unchanged?

These questions are too technical and focus too heavily on file management. File ownership and reproducibility are important, but they should not dominate the learning checks.

Please review the **Tutorial Design**, **Manual Design**, and current learner manual, and reconsider the learning checks in light of the tutorial's intended scientific and educational objectives.

I would prefer questions that assess whether learners understand, for example:

- How are river-map parameters created or modified?
- How can a modified map parameter be selected in a GOSH simulation script?
- How can two simulations be configured and compared to isolate the effects of changing a parameter?
- What physical changes result from increasing `rivhgt`, and how should changes in river depth, water-surface elevation, discharge, and inundation be interpreted?

Please consider which questions belong at each stage of the tutorial. The checks should test **conceptual understanding and the ability to conduct and interpret a parameter-sensitivity experiment**, rather than simply whether learners remember file paths or implementation details.

After reviewing the documents, propose a coherent set of improved learning checks and prepare an **English prompt for Codex** to update the learner manual accordingly. Preserve essential technical and reproducibility guidance in the main explanations, even if it is removed from the learning checks.
