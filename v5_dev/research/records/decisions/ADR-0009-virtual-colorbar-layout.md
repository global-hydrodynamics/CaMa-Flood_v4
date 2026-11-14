# ADR-0009: Reserve virtual colorbars for comparable panel layouts

- Status: accepted
- Date: 2026-11-12
- Event ID: EV-0439
- Decision owner: Dai Yamazaki (maintainer)
- Working branch: `manual-automation`

## Context and decision

The p01-f02 experiment in commit `7fdca7dd` produced identical 968x968-pixel
map rectangles on identical canvases despite only one panel displaying a
colorbar. [VR-0136](../validation_runs/VR-0136.md) records the exported geometry,
five sizing scenarios, and unchanged scientific drawing behavior. The
maintainer confirmed the aligned height and explicitly requested adding the
virtual-colorbar approach to the common visualization rules.

Comparable geographic panels with different colorbar visibility must reserve
identical map and colorbar geometry and preserve the full exported canvas.
Hidden colorbar axes have no visible ticks, labels, or frame. Fixed-height
sizing must include the reserved space without shrinking map height; verify
pixel geometry, label clearance, and assembly. Numeric margins stay local to
each project. The authoritative rule is in
[the figure-making guideline](../../../visualization/figure_making_guideline.md#virtual-colorbars-for-comparable-panels).

## Scope and verification

Only the shared guideline, f02 explanatory text/docstring, coordination status,
and this decision record change. The historical experimental report remains
unchanged. No plotting logic, scientific behavior, generated PNG, combiner,
Gallery, or unrelated visualization project changes.

Checked local links, `git diff --check`, and research record validation. No
figure regeneration is needed for these prose-only changes. The brief
knowledge-gap review found no new material scientific or architectural gap.
