# Chat 01: Request for a Tutorial Design Draft

The following is an English translation of the researcher's initial request to ChatGPT for a draft of Tutorial03's design document. It preserves the exploratory nature of the original request; details not specified at this stage are intentionally left open.

---

I would like to prepare a rough tutorial design document for Tutorial03 before handing it over to Clara.

Let's design `tutorial03-rivhgt_modification`.

## Theme

**Try modifying the river map yourself.**

- Prepare an input parameter dataset with deeper river channels by modifying the river-channel depth parameter (`rivhgt`).
- Use that modified parameter dataset as model input.
- Simulate only the year 2000.
- Increase the output variables to include `outflw`, `rivdph`, `sfcelv`, and `fldfrc`.
- Provide examples of how to visualize each variable. In particular, I want to demonstrate why visualizing the flooded fraction (`fldfrc`) is more meaningful in this context than plotting flooded area directly, since `fldare = fldfrc × ctmare`.
- Show visualization using permanent-water data (`prmwat`).
- Show how to aggregate flooded area over a region, such as the lower Mekong Delta, to produce a time series.
- Conduct a sensitivity analysis of river-channel depth. The expectation is that making channels deeper will increase peak discharge and channel water depth while reducing inundation.
- Also demonstrate the downscaling functionality as a trial.

## Workflow

- Prepare the modified parameter dataset directly under `map/mekong_15min/`.
- Run two experiments.
- Separate the analysis stage, in which derived data are prepared, from the visualization stage.
- Perform visualization as the final step.

Please prepare a tutorial design document based on these goals.

**If anything is ambiguous, ask me rather than making decisions on your own.**
