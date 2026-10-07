# Water-temperature and ice heat-budget stability and diagnostics

## Stabilization

When nearly all water leaves a cell, dividing residual heat by residual water volume can produce extreme temperatures through roundoff. Reconstruct the temperature using nonnegative weights for retained and incoming water. Retain the original temperature when there is no inflow. This reconstruction does not modify hydraulic storage or discharge.

Retain the previous temperature when liquid volume is at or below the existing `STO_IGNORE` threshold. During dry conditions this is a remembered temperature, not a heat source for ice. After rewetting, determine the temperature from actual inflowing or melted water and its sensible heat.

Pass liquid sensible heat directly between freezing and melting operations, avoiding repeated conversion to temperature at very small volumes. Atmospheric heating can still melt ice in a dry cell. Ice transport also avoids dividing by liquid volumes at or below `STO_IGNORE`.

Initialize water sensible-heat and ice export limiters to one. Divide only when requested export exceeds availability. This avoids unnecessary quotient overflow for tiny positive transport without rounding all such fluxes to zero.

In detailed mode, record the difference between expected energy and the energy represented by the retained temperature as signed unapplied heat. Do not carry it forward, redistribute it, or reinject it into model state. An unchanged dry cell does not repeatedly accumulate the same previously unapplied heat.

## Monitoring settings

```fortran
&NHEATLINK
    LICE = .TRUE.
    LHEAT_DIAG = .FALSE.
    HEAT_LOG_FILE = 'HEAT-LINK.log'
/
```

`LHEAT_DIAG` defaults to `.FALSE.`. Set it to `.TRUE.` for the full heat-budget audit, including every internal advection step. `HEAT_LOG_FILE` defaults to `HEAT-LINK.log` in the run directory; a custom relative or absolute path is accepted. Its parent directory must exist. Each run replaces this log. An empty path, an inaccessible path or an already-open file (including the CaMa log) is an error. With `LHEATLINK = .FALSE.`, no heat log is opened.

| Check or operation | Always active | Additional work with `LHEAT_DIAG = .TRUE.` |
|---|---|---|
| Dry-state handling, mixing reconstruction and bounded export limiters | All physical safeguards | No change to physical state |
| Existing invalid-input and phase/storage consistency checks | Errors remain fatal | No change |
| End-of-outer-update water temperature | Finite-value check; wet/dry minima, maxima, both cell indices and volume at the maximum; warning above 350 K | Same checks |
| Calendar timestamps and heatlink errors | Separate heat log | Same log |
| Interval heat closure and independent extrema | Not evaluated | Advection, local and combined outer update: ΔE, Q, U, residuals, ratios and extrema |
| Cause-specific unapplied heat and cumulative closure | Not aggregated | Cause-specific totals, process closure and a cumulative domain ledger |
| Transport/phase budget summaries | Not aggregated | Cell and domain maximum-error summaries |

The switch skips diagnostic snapshots, compensated domain reductions, cause scans and their log records; it is not just an output filter. The physical phase kernel still calculates its existing residual outputs for `RIVICE_*` output requests, independently of this switch. Minimal mode does **not** establish heat-budget closure: use detailed mode when auditing conservation. Neither mode redistributes or reinjects unapplied heat.

## Module responsibilities

- `heat_residual_mod` defines both types and operations for cause-specific unapplied-heat totals and the domain heat ledger accumulated since this run started. These are supporting diagnostics.
- `heat_step_monitor_mod` defines both types and operations for interval snapshots, compensated sums, state increments and independent extrema. Interval residuals avoid relying on subtraction of large annual totals.
- `heatlink_diagnostics_mod` connects those two diagnostic modules to canonical CaMa storage and read-only river temperature/ice arrays. It owns diagnostic state, assembles exchanges and writes the logs.
- `heatlink_log_mod` owns the separate log file and formats calendar markers supplied by the CaMa driver state.
- `heatlink_river_mod` retains physical state, forcing, solver order and output coupling, with calls to begin, record and finish the diagnostic intervals.

The first two modules are separated by diagnostic responsibility and time scale, not by separating type definitions from their use. Their numerical routines can be tested independently of the river driver.

## Heat-budget definitions

Represented energy consists of water sensible heat relative to the melting point and the latent energy of ice at the melting point. Ice-surface temperature is a massless diagnostic; it does not represent sensible heat stored inside ice.

- `change [J]`: represented energy increment ΔE during the interval.
- `net input [J]`: net external heat input Q during the same interval. Internal transport is excluded.
- `absolute exchange [J]`: absolute external heat exchange S during the interval. For local heating, sum the absolute **net input per cell**, not the absolute values of every radiation and turbulent flux component.
- `unapplied: signed [J]`: signed unapplied heat U. Positive means unapplied heating; negative means unapplied cooling.
- `unapplied: absolute [J]`: absolute unapplied heat without cancellation between its positive and negative contributions.
- `raw[J] = ΔE − Q`: residual before accounting for unapplied heat.
- `adjusted[J] = ΔE − Q + U`: residual after accounting for unapplied heat in the diagnostic ledger.
- `storage scale [J]`: an energy-storage scale in which sensible heat and ice latent energy do not cancel.

Compute ΔE directly from cellwise volume, temperature and ice increments. Subtracting two domain energy totals is not the primary diagnostic. Use `JPRD` Neumaier compensated sums. The comparison fields `large-total comparison: change [J]` and `adjusted [J]` retain the large-total subtraction.

Record `raw/S`, `adjusted/S`, absolute unapplied heat/S and `adjusted/storage_scale`. Flag zero-denominator and unrepresentable ratios as invalid and exclude them from ratio extrema. The log displays `undefined` for such a ratio, rather than a misleading zero. Energy and ratio extrema are selected independently and may occur at different times.

## Reading the log

Heatlink-specific messages go to `HEAT_LOG_FILE` (normally `HEAT-LINK.log`), while CaMa and shared input/output messages remain in `log_CaMa.txt`. Existing source/procedure identifiers are retained in lifecycle and error messages. Each diagnostic physical process has a bracketed heading. Results are indented by two spaces per level, with labels and units on each line. Machine record prefixes are no longer emitted.

The log header defines both temperature groups, whether detailed monitoring is enabled or disabled. `wet water temperature` covers cells with end-of-update liquid-water volume greater than `STO_IGNORE`; `dry water temperature` covers cells at or below that threshold. Volumes are in m³. Dry or near-dry cells retain a remembered temperature; it is not a heat source. Ice volume is not used for this classification. In both monitoring modes, each nonempty group reports its minimum and maximum temperature and both one-based river-vector cell indices after every outer update. Ties use the first cell in vector order.

| Heading or item | Meaning |
|---|---|
| `YYYY/MM/DD HH:MM  step = ...  begin/end` | Model calendar and CaMa outer step counter, before advection and after the completed local update |
| `[advection]` | Every internal advection interval, dry/reconstruction unapplied heat and transport closure summaries |
| `[local heat budget]` | Each local (vertical heat exchange and phase-change) update, dry/ice/floor unapplied heat and wet/dry water temperatures |
| `[combined step]` | Combined advection and local budget over one outer update; one hour in the validation runs |
| `[cumulative heat budget]` | Domain ledger since this run started; supporting information, not the primary conservation test |
| `extrema over this run` | Per-process minimum/maximum of each metric, interval index and the complete corresponding record at shutdown |
| `closure since run start` | Auxiliary process closure; overlaps the domain residual and must not be added to it |

For example, the start of a detailed interval is written as follows (numbers shortened here):

```text
2000/01/01 00:00  step = 1  begin
[advection]
  interval = 1; end [s] = 3.27272737E+02; duration [s] = 3.27272737E+02
  energy [J]: change = 1.09894276E+16; net input = 1.09894276E+16; absolute exchange = 1.09894276E+16
  unapplied [J]: signed = -7.05345297E+01; absolute = 2.56328193E+04
  residual [J]: raw = 2.36000000E+02; adjusted = 1.65465470E+02
```

The `begin` calendar is recorded before hydraulic advection. The timestamp before the local heat-budget update has no trailing label (for example, `2000/01/01 01:00  step = 1`). It and the `end` calendar use the same `JYYYYMMDD` and `JHHMM` as `CMF::DRV_ADVANCE END` in the CaMa log. These are model dates, not wall-clock timestamps. Day/year rollover is retained, and each completed outer update flushes the log.

`end [s]` accumulates internal time steps from the start of this run; `duration [s]` is the interval length. These distinguish adaptive internal intervals within the formatted calendar markers. Allow for accumulated internal-step roundoff when converting extrema times back to dates. Extrema are temporal extrema of interval **domain** budgets, not spatial extrema of cellwise errors. Restart runs start new diagnostic accumulations.

Existing parsers of the former machine-prefixed records must be updated to read the process headings and labelled fields. Previously generated logs are not rewritten. For a quick inspection:

```sh
less /path/to/run/HEAT-LINK.log
rg -n '^\[(advection|local heat budget|combined step)\]|extrema over this run' /path/to/run/HEAT-LINK.log
```

Detailed monitoring evaluates and logs every internal transport interval, requiring extra memory, arithmetic and log I/O. It is disabled by default. It never corrects physical state using diagnostic values. Runtime comparisons should use identical compiler options, forcing, time intervals, output settings and OpenMP configuration; concurrent annual runs alone do not isolate monitoring cost.

With `LICE = .FALSE.`, unapplied cooling caused by the melting-point floor remains in `raw` and U. A small `adjusted` residual does not mean that cooling was physically applied. With `LICE = .TRUE.`, evaluate dry-state and other unapplied heat separately as well. These domain diagnostics do not rule out cancellation between cells or account for heat fluxes never computed because of dry-state checks.

## Diagnostic reduction performance

Large domains are split into fixed 4,096-cell blocks. OpenMP evaluates independent blocks when there are at least 32,768 cells. The primary interval sums use Neumaier compensation inside each block and during the ordered final merge; both the leading sum and its correction are merged without first rounding them together. Block boundaries and merge order do not depend on the thread count. Small domains follow the same grouping without parallel workers.

The auxiliary large-total comparison is computed during the same pass as the cellwise increment, eliminating additional domain scans. Its uncompensated domain sums now use block grouping, so its roundoff can differ from the earlier serial comparison. Cause-specific cumulative sums also use fixed blocks and retain unique affected-cell counts incrementally. Those supporting totals may differ in roundoff; they remain separate from the primary compensated interval audit.

Cellwise transport diagnostics are filled in the existing parallel reconstruction loop. No diagnostic value changes the physical update. Every internal advection interval, local update, combined interval and independent extremum is still monitored in detailed mode; the speedup does not sample or skip intervals.
