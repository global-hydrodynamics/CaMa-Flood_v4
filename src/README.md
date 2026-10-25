# Fortran source architecture

This guide describes the organization that is present in `src/`. It is a map for developers reading or extending the Fortran code; it does not define a new coding standard. For project rules, compatibility boundaries, and required approvals, see the root [AGENTS.md](../AGENTS.md). The root [README](../README.md) and [CaMa-Flood guideline](../doc/CaMa-Flood_Guideline.md) introduce the model and repository; the versioned [main manual](../doc/Manual_CaMa-Flood_v420.docx) gives the fuller user and scientific description.

## Architectural principle: reusable modules, adaptable drivers

CaMa-Flood's computational foundation is a set of reusable **CTRL** and **CALC** modules. CTRL modules manage configuration, initialization, simulation control, input/output, and related operations; CALC modules implement numerical calculations. Driver (**DRV**) code assembles and coordinates the modules needed by a particular execution environment.

The `MAIN_cmf` program and `CMF_DRV_*` routines in the main source tree are the standard driver path for stand-alone execution. They are not the only possible driver, nor a mandatory entry point for every external integration. A host application may supply its own driver and use the CTRL/CALC modules and shared state appropriate to its coupling. The modules are reusable, but they are not completely independent: they have `USE` dependencies, shared mutable state, compile-time conditions, and assumptions about initialization and update order.

```text
Stand-alone MAIN_cmf ── standard driver ──┐
ILS MAIN_cmf_ici ────── ICI driver + selected standard driver calls ─┤
Other host ──────────── host-specific driver ────────────────────────┤
                                                                     ▼
                                           reusable CaMa-Flood CTRL / CALC modules
                                                             │
                                                             ▼
                                               shared model state and map data
```

The ILS path in [`ICI/`](ICI/Makefile) is a concrete example of a host-specific executable and controller. It uses the common model calculations and state while coordinating ICI forcing, output, timing, and optional lake exchange. In this checkout it also calls the standard `CMF_DRV_*` procedures; it demonstrates an alternative outer driver arrangement, not a completely separate reimplementation of all model lifecycle routines. The diagram describes the intended architecture; the source-specific details and dependencies are described below. Some modules are compiled conditionally, and not every subdirectory is part of every executable.

## Execution flow

The standard stand-alone entry point is [`MAIN_cmf.F90`](MAIN_cmf.F90). It owns the file-driven outer loop and delegates model setup and time integration to `CMF_DRV_INPUT`, `CMF_DRV_INIT`, `CMF_DRV_ADVANCE`, and `CMF_DRV_END`. The normal standard-driver call sequence is:

```text
MAIN_cmf
  ├─ CMF_DRV_INPUT       read/check configuration and prepare logging
  ├─ CMF_DRV_INIT        initialize time, maps, state, forcing and enabled features
  ├─ stand-alone loop
  │    ├─ CMF_FORCING_GET / CMF_FORCING_PUT at forcing update times
  │    ├─ optional tracer forcing and optional heatlink input update
  │    ├─ CMF_DRV_ADVANCE(steps)
  │    │    └─ per outer step: time/boundaries → physics → diagnostics/output
  │    │                    → restart handling → advance model clock
  │    └─ optional sediment forcing and heatlink post-step work
  ├─ CMF_DRV_END         close initialized standard I/O components
  └─ optional MPI finalization
```

This is a call-flow overview, not a guarantee that each optional call is present in every preprocessed executable. The `heatlink` blocks are guarded by a compile-time definition and a runtime namelist switch. Sediment and MPI code also have compile-time guards. `MAIN_cmf.F90` owns the stand-alone outer loop and its file-based forcing interaction; its calls to `CMF_DRV_*` are the arrangement chosen for this executable, not a rule that every host must call the same set of procedures.

### Driver and calculation layers

| Layer | Main source | Responsibility |
| --- | --- | --- |
| Stand-alone program | [`MAIN_cmf.F90`](MAIN_cmf.F90) | Selects the file-driven execution path, reads forcing at its cadence, calls the model driver, and surrounds it with optional component work. |
| Standard lifecycle driver routines | [`cmf_drv_control_mod.F90`](cmf_drv_control_mod.F90) | `CMF_DRV_INPUT`, `CMF_DRV_INIT`, and `CMF_DRV_END` provide the configuration, ordered setup, and shutdown sequence used by the stand-alone driver. `CMF_DRV_INIT` has an optional integer `LECMF2LAKEC` argument used in the source for a lake-coupling case. |
| Standard time-advance routine | [`cmf_drv_advance_mod.F90`](cmf_drv_advance_mod.F90) | `CMF_DRV_ADVANCE(KSTEPS)` advances one or more outer model steps, updates optional boundaries/inflows and tracer state, calls physics, and coordinates output and restart events. A host driver may call it, as ICI does, or organize other module calls according to its own integration. |
| Execution control | `cmf_ctrl_*_mod.F90` | Reads settings, initializes state and data, schedules the numerical routines, and coordinates I/O and optional features. |
| Numerical calculations | `cmf_calc_*_mod.F90`, `cmf_opt_*_mod.F90` | Calculates flows, storage and diagnostics, including alternate calculation paths selected by existing settings. |
| Shared state | `yos_cmf_*.F90` | Holds model configuration, map geometry, time, prognostic state, and diagnostics in module variables used across procedures. |

`CMF_DRV_INPUT` reads the configuration groups through their controller modules and calls `CMF_CONFIG_CHECK`. `CMF_DRV_INIT` initializes the model clock, river map and topography, then the enabled boundary, forcing, upstream inflow, output, state, restart, reservoir, tracer, sediment, and other components in a defined order. The actual sequence is visible in [`cmf_drv_control_mod.F90`](cmf_drv_control_mod.F90); individual components are conditional on their settings or preprocessor definitions.

For each outer step, `CMF_DRV_ADVANCE` gets the next model time and applies enabled boundary and upstream-inflow updates. `CMF_PHYSICS_ADVANCE` in [`cmf_ctrl_physics_mod.F90`](cmf_ctrl_physics_mod.F90) is the main hydrodynamic orchestration routine. It determines the internal integration interval, performs the configured flow/storage calculations, and accumulates adaptive-step diagnostics. The current control flow is: calculate stage; select adaptive subdivision if enabled; reset internal-step diagnostics; calculate river flow; apply enabled reservoir and bifurcation operations; derive inflow and water-balance adjustments; save previous-step values; advance storage; run enabled lake/heatlink hooks; recalculate stage; update water-balance and average/maximum diagnostics; then restore the outer-step `DT` and finalize diagnostics. The exact order includes optional operations and must be read from the routine before changing it. After physics, the driver handles scheduled output and restart work and advances the clock.

The model-clock and boundary/inflow roles are separate from calculations. `cmf_ctrl_time_mod.F90` manages calendar and step counters; `cmf_ctrl_boundary_mod.F90` updates the sea-level boundary when enabled; `cmf_ctrl_upsinf_mod.F90` manages upstream inflow when enabled. `cmf_ctrl_forcing_mod.F90` reads and interpolates the stand-alone runoff forcing. `cmf_ctrl_maps_mod.F90` reads map/topography inputs and prepares the river-vector structures used by calculations.

Output and restart are coordinated through `cmf_ctrl_output_mod.F90` and `cmf_ctrl_restart_mod.F90`. They support binary paths and compile-time-selected NetCDF paths (`UseCDF_CMF`) and write model state using existing file conventions. Tracer output/restart is in `cmf_ctrl_tracer_mod.F90`; reservoirs, levees, and sediment have additional controllers. `CMF_DRV_END` closes initialized forcing, output, boundary, upstream-inflow, and tracer-output components. Not every allocated state array has a general deallocation path in the stand-alone lifecycle.

## File and module naming

Names are conventions found in the current tree, not a requirement to rename older files. Most legacy core files use uppercase module and procedure identifiers even when their filenames are lowercase. Newer supporting components often use lowercase identifiers. The source also preserves historical names and exceptions.

| Pattern | Usual responsibility | Examples and where related work is found |
| --- | --- | --- |
| `MAIN_*.F90` | An executable program and its outer loop. | `MAIN_cmf.F90` is the stand-alone entry point. `ICI/MAIN_cmf_ici.F90` is a distinct host-coupled entry point. |
| `cmf_drv_*_mod.F90` | Model lifecycle and time-advance driver procedures used by the stand-alone program. | `cmf_drv_control_mod.F90` and `cmf_drv_advance_mod.F90`; lifecycle-level additions belong here when they concern setup, stepping, or finalization rather than a particular calculation. |
| `cmf_ctrl_*_mod.F90` | Configuration, initialization, coordination, input/output, and optional feature control. | Examples include `cmf_ctrl_forcing_mod.F90`, `cmf_ctrl_maps_mod.F90`, `cmf_ctrl_restart_mod.F90`, and `cmf_ctrl_physics_mod.F90`. |
| `cmf_calc_*_mod.F90` | A numerical or diagnostic calculation. | `cmf_calc_outflw_mod.F90`, `cmf_calc_stonxt_mod.F90`, `cmf_calc_fldstg_mod.F90`, and `cmf_calc_diag_mod.F90`. |
| `cmf_opt_*_mod.F90` | Alternate or optimized calculation path. | `cmf_opt_outflw_mod.F90` includes kinematic and mixed-flow alternatives called by physics control. |
| `yos_cmf_*` | Shared CaMa-Flood state, often module-level and saved for model lifetime. | `yos_cmf_input.F90`, `yos_cmf_map.F90`, `yos_cmf_time.F90`, `yos_cmf_prog.F90`, and `yos_cmf_diag.F90`. `yos` is a historical prefix. |
| `parkind1.F90` | Shared integer and real kind parameters. | `PARKIND1` defines `JPIM`, `JPRB`, `JPRD`, and related kinds. |
| `*_mod.F90` / `*_mod.f90` | A Fortran module, with the suffix/case reflecting the source's history and preprocessing needs. | Both suffix variants exist. A filename alone does not tell you the build flags or whether a feature is active. |

Additional recurring names include `CMF_*_NMLIST` for namelist-reading routines, `*_INIT`/`*_END` for lifecycle procedures, and `*_WRITE`/`*_GET`/`*_PUT` for I/O or data transfer. These are useful search hints; they do not imply that every component implements the same full lifecycle.

## Source directory map

| Directory | Responsibility and notable contents | Build and status in this repository |
| --- | --- | --- |
| `src/` root | Stand-alone main, driver, core controllers/calculations, and shared `yos_cmf_*` state. | `src/Makefile` builds the stand-alone target using `adm/Mkinclude`. Optional library subdirectories are selected with `EXT_LIBS`; the default include leaves it empty. |
| [`common/`](common/Makefile) | General utilities and reusable support modules: arrays and containers, dates/time, text, binary and NetCDF helpers, map frames, numeric utilities, and linear algebra. | Builds `libcommon.a`; tests are under `common/test` and the `test` target compiles executables. It is used by the core and supporting libraries; the exact linked subset depends on the build. |
| [`mod/`](mod/Makefile) | Map/grid and input dimension conversion support, including `glob_mod`, `camaframe_mod`, `inpmat_mod`, and `dim_converter`. | Builds `libmod.a`, includes core/common module directories, and has synthetic mapping checks through `test-input-mapping`. Used by configurable I/O and heatlink paths. |
| [`io/`](io/Makefile) | Configurable input, output, and restart support used by the heatlink input/output path; includes namelist readers and writer/configuration classes. | Builds `libio.a` against common, mod, and core module files; `test-input-slice` runs a Python-driven synthetic fixture test. This is separate from the legacy core controller I/O. |
| [`phys/`](phys/Makefile) | Physical-process calculation modules such as water, heat flux/budget, ice cover, and physical constants. | Builds `libphys.a`; it is not listed among the default core objects in `src/Makefile`. Heatlink uses its physical routines. Tests are under `phys/test`; its `test` target compiles executables. |
| [`heatlink/`](heatlink/README.md) | Optional river water-temperature and surface-ice calculations, their coupling to CaMa state, configuration, diagnostics, and restart/output adapters. `heatlink_river_mod` is the bridge to core arrays. | Separately builds `libheatlink.a` and lists common, mod, io, phys, and core include paths; exact libraries are configured in `adm/Mkinclude*`. `-Dheatlink` exposes driver hooks, while `LHEATLINK` controls runtime activation. Tests are under `heatlink/test`; its `test` target compiles executables. |
| [`sediment/`](sediment/Makefile) | Sediment state, parameter and flow calculations, forcing, output, restart, and controller modules. | The separate Makefile builds a sediment-enabled executable with `-Dsediment`, using core source via `vpath`. `MAIN_cmf.F90` and `cmf_drv_advance_mod.F90` include compile-time hooks. No test directory or component README is present; dependencies/configuration are specific to this build path. |
| [`ICI/`](ICI/Makefile) | Specialized ICI/lake-coupling executable, controller, shared ICI state, and lake-inflow calculation. | Uses a separate Makefile and host dependencies such as `ici_api`, `palmtime`, and MPI, and expects an external build tree. No tests or component README are present. It is not a generic `EXT_LIBS` component. |

There is no general dynamic plug-in loader in this layout. Extensions are connected through Fortran `USE` dependencies, build object/library lists, compile-time preprocessing, runtime switches where implemented, and explicit calls in drivers or controllers. Different extensions use different combinations of these mechanisms.

### Core controller landmarks

| Module | Main responsibility visible in the source |
| --- | --- |
| `cmf_ctrl_nmlist_mod.F90` | Reads core configuration and checks selected option combinations. |
| `cmf_ctrl_maps_mod.F90` | Reads river-network and topography inputs; prepares map and vector state. |
| `cmf_ctrl_forcing_mod.F90` | Initializes, reads, and puts runoff forcing into model state; supports compile-time-dependent input paths. |
| `cmf_ctrl_physics_mod.F90` | Coordinates adaptive step selection and the hydrodynamic update sequence. |
| `cmf_ctrl_time_mod.F90` | Initializes and advances the model calendar and step counters. |
| `cmf_ctrl_output_mod.F90` / `cmf_ctrl_restart_mod.F90` | Writes scheduled output and checkpoint state using the repository's supported formats. |
| `cmf_ctrl_boundary_mod.F90` / `cmf_ctrl_upsinf_mod.F90` | Manage optional time-varying sea-level boundary and upstream inflow data. |
| `cmf_ctrl_tracer_mod.F90`, `cmf_ctrl_damout_mod.F90`, `cmf_ctrl_levee_mod.F90` | Coordinate optional tracer, reservoir, and levee functionality. |
| `cmf_ctrl_mpi_mod.F90` | Provides MPI initialization, finalization, and communication helpers when the relevant MPI path is compiled. |

The principal calculation modules separate flow, storage, and diagnostics: `cmf_calc_outflw_mod.F90` calculates river/floodplain flow, `cmf_calc_pthout_mod.F90` handles bifurcation path flow, `cmf_calc_stonxt_mod.F90` advances storage, `cmf_calc_fldstg_mod.F90` derives stage/depth state, and `cmf_calc_diag_mod.F90` accumulates diagnostics. These modules read and update shared arrays; the controller determines when they execute.

## Shared state, kinds, and data flow

The core uses module variables as the shared model state. `yos_cmf_input` contains configuration and shared input-related values; `yos_cmf_map` contains dimensions, river-network and map fields; `yos_cmf_time` contains model clock state; `yos_cmf_prog` owns prognostic quantities; and `yos_cmf_diag` owns diagnostics. `cmf_ctrl_vars_mod.F90` allocates and initializes many prognostic and diagnostic arrays. Controllers and calculations import the values they use with `USE ... ONLY` in many, but not all, places.

Arrays are commonly allocated during initialization and retained in saved module state. A number of river-vector quantities are declared with a second singleton dimension, for example `(NSEQMAX,1)`, while map data may use grid dimensions. `cmf_utils_mod.F90` provides conversions between vector and map representations. Follow the actual shape and indexing convention of the field being changed; similarly named fields do not necessarily share layouts.

`parkind1.F90` defines the project kinds. `JPRB` changes precision when `SinglePrec_CMF` is enabled; `JPRD` remains a double-precision kind and is used for important storage and budget state. For example, `P2RIVSTO` and `P2FLDSTO` in `yos_cmf_prog.F90` use `JPRD`, while many flow and diagnostic fields use `JPRB`. Do not infer a field's precision from its name or the global switch alone.

Input options are read through Fortran namelists, generally by `CMF_*_NMLIST` procedures and related component readers. Compile-time options in `adm/Mkinclude*` control preprocessing and compiler selection; namelist switches control runtime behavior only where the code provides such a switch. A runtime switch cannot enable code removed at preprocessing time.

Important definitions in the checked-in build includes include `UseCDF_CMF` (NetCDF-related branches), `UseMPI_CMF` (MPI code), `SinglePrec_CMF` (the `JPRB` kind switch), `NoAtom_CMF` (an alternate atomic-update path), `heatlink` (heatlink driver hooks), and the `ILS` conditional used for ICI lake coupling. These names are consumed in particular source/build paths; some specialized Makefiles define options themselves. Consult the active include file, Makefile, and preprocessor branches before assuming an option applies to all components.

## General coding practices observed

This section describes patterns in representative current source files. It is not a prescriptive style guide; modules vary by age, purpose, and author.

### Names, layout, and procedure structure

- The legacy core commonly uses uppercase identifiers for modules, procedures, constants, and many local names. Several newer libraries, notably `common/`, `io/`, `phys/`, and `heatlink/`, use lowercase names. Mixed case and historical spellings occur.
- Core sources commonly use two-space indentation inside control blocks, banner comments made from `!` and separator lines, and inline comments describing model quantities or steps. Supporting libraries more often use lowercase identifiers and deeper indentation. There is no single formatting layout across `src/`.
- Long argument lists and declarations use `&` continuation. `.F90` sources commonly contain `#ifdef` / `#endif` branches; OpenMP directives use sentinel comments such as `!$OMP` or `!$`.
- Large controller and calculation procedures exist alongside smaller modules and testable kernels. Some newer modules declare `private` and list their public procedures; many legacy core modules leave module entities accessible by default. Do not assume uniform encapsulation.

### Declarations, state, and errors

- `IMPLICIT NONE` is common in modules and procedures. Many procedures declare kind-qualified values via `PARKIND1`, but historical literals and declarations are not perfectly uniform.
- `INTENT` is explicit in many newer helper interfaces and some driver/MPI arguments. A number of legacy core routine arguments omit it. This is an observed variation, not a claim that omitted `INTENT` is desirable.
- Module-level `SAVE` variables and allocatable arrays are common for long-lived model state. Initialization and ownership often live in `cmf_ctrl_vars_mod.F90`, `yos_cmf_*` modules, or the relevant extension controller. There is no universal state-container abstraction.
- Diagnostics commonly go through the shared `LOGNAM` unit with `WRITE`; lower-level helpers may return status or use component-specific loggers. Error handling varies by path and includes logged messages, checks, and termination in some cases. Review the surrounding routine rather than assuming a uniform error API.
- Array allocation is generally explicit. Allocation, initialization values, use by optional code, and restart reads/writes must be considered together when introducing persistent state.

### Numerical and parallel behavior

- Precision is expressed through the project kind parameters rather than a single fixed real kind. `JPRB` configuration and the persistent `JPRD` storage fields affect memory, arithmetic, and interfaces.
- The hydrodynamic controller uses an adaptive internal time step. In the core, `cmf_ctrl_physics_mod.F90` coordinates the step selection and repeated calculations; `cmf_ctrl_time_mod.F90` updates outer-step calendar state. The sequence of state updates and diagnostics is part of the implemented method.
- OpenMP is represented by compiler directives in source, including parallel loops, reductions, `SIMD`, `THREADPRIVATE`, and guarded atomics. MPI code is generally enclosed by `UseMPI_CMF`; `cmf_ctrl_mpi_mod.F90` also has an `IFS_CMF` path using `MPL_MODULE`. These paths depend on compiler and host configuration.
- The `NoAtom_CMF` conditional changes use of atomic updates in flow calculations. Parallel reductions, atomics, kind selection, and calculation order can affect floating-point results. Reproducibility should be assessed under the same compiler, flags, precision, and parallel configuration; source inspection alone does not establish bitwise identity.
- Optional processes have ordering constraints. For example, heatlink captures/advection-updates water-related state around core storage changes, while sediment is called at a configured interval. Moving such a hook or changing a shared field can affect coupled state, restart consistency, and numerical results.

## Integration with External Modelling Systems

CaMa-Flood is used both as a stand-alone model and as a component in larger modelling systems, including ECMWF's IFS and the MIROC climate model. The reusable CTRL/CALC modules are the computational foundation; a driver adapts their use to the host. The source contains a standard stand-alone driver and a separate ICI/ILS executable path. This structure supports host-specific orchestration, but the repository alone does not establish the exact API contract or build requirements of every external host.

### Standard driver and ICI/ILS example

The standard path pairs [`MAIN_cmf.F90`](MAIN_cmf.F90) with the routines in `CMF_DRV_CONTROL_MOD` and `CMF_DRV_ADVANCE_MOD`. The main program owns file-based runoff acquisition and its outer loop; the standard driver routines initialize and advance model components. This is one concrete assembly of the shared controllers and calculations.

[`ICI/MAIN_cmf_ici.F90`](ICI/MAIN_cmf_ici.F90) is a second program entry point for the ICI-based ILS coupling path. Its visible order is:

```text
CMF_ICI_INPUT → CMF_DRV_INPUT → CMF_DRV_INIT → CMF_ICI_INIT
loop: CMF_ICI_FORCING_GET → CMF_DRV_ADVANCE(1) → CMF_ICI_OUTPUT
finish: CMF_DRV_END → CMF_ICI_END
```

The ICI program therefore replaces the stand-alone main loop's forcing/output orchestration with ICI-managed calls, while reusing the standard model initialization and advance routines. `CMF_CTRL_ICI_MOD` in [`ICI/cmf_ctrl_ici_mod.F90`](ICI/cmf_ctrl_ici_mod.F90) manages ICI setup, grid mapping, clock setup, lake state, forcing transfer, output transfer, and cleanup. It imports core state and map modules (`YOS_CMF_INPUT`, `YOS_CMF_MAP`, and `YOS_CMF_TIME`), ICI-specific state from `YOS_CMF_ICI`, and host modules `ici_api` and `palmtime`; it also uses MPI. `CMF_CALC_LAKEIN_MOD` in [`ICI/cmf_calc_lakein_mod.F90`](ICI/cmf_calc_lakein_mod.F90) implements the lake-to-river exchange calculation against shared CaMa storage and diagnostics. The core physics controller calls this calculation only under the `ILS` preprocessing branch and when the lake-inflow switch is enabled. `ICI/yos_cmf_ici.F90` owns the ICI-specific lake fraction and inflow arrays.

This example also shows that “alternative driver” does not mean every model service is duplicated: the ICI program has its own host-facing setup and loop, but explicitly reuses `CMF_DRV_*` and the core physics execution. Its Makefile builds a specialized executable and expects external ICI/host libraries and build files. It is evidence of one implementation, not a general driver framework or a prescribed template.

### Interfaces visible in this source tree

These module procedures are used by the standard path and are visible to other compiled callers:

| Procedure | Observed role |
| --- | --- |
| `CMF_DRV_INPUT()` | Read core namelists, initialize logging/configuration, and check configuration. |
| `CMF_DRV_INIT(LECMF2LAKEC)` | Initialize the model. `LECMF2LAKEC` is an optional integer input in the current source; it is referenced as a lake-coupling option. |
| `CMF_DRV_ADVANCE(KSTEPS)` | Advance the internal outer-step loop for the requested number of steps. `KSTEPS` is an integer argument. |
| `CMF_DRV_END()` | Close the standard driver's initialized I/O and component resources. |

`CMF_DRV_ADVANCE` uses forcing and model state already made available through the existing module variables and/or update routines; its argument is not a runoff array. In the stand-alone path, `MAIN_cmf` calls `CMF_FORCING_GET` and `CMF_FORCING_PUT` before advancing. The ICI entry point uses its own forcing/output controller around the same driver calls. These are source-level observations, not a complete external coupling specification or a claim that `CMF_DRV_*` is the only supported public interface.

### Integration considerations

The architecture exposes several things a host integration must account for:

- **Initialization order:** configuration precedes `CMF_DRV_INIT`; map, time, model state, forcing, and enabled schemes are initialized in an established order. A host must identify which responsibilities remain with CaMa and which are supplied externally.
- **Shared state:** model state is held in Fortran modules, including saved mutable variables. Independent simultaneous model instances are not demonstrated by this source layout.
- **Time stepping and forcing:** `CMF_DRV_ADVANCE` may run multiple outer steps, and physics may subdivide them with adaptive steps. External forcing cadence and calendar synchronization must match the chosen call sequence.
- **Precision and build:** `SinglePrec_CMF`, compiler flags, preprocessor definitions, NetCDF/MPI/OpenMP libraries, and host-specific modules affect the built interface and runtime. Rebuild dependent modules together when kind or preprocessor settings change.
- **State/restart compatibility:** shared state, update order, and restart serialization are coupled. Changes to arrays, units, time hooks, or output/restart code can require coordinated host and file-format changes.
- **Parallel ownership:** the source includes standalone MPI and an `IFS_CMF` branch in `cmf_ctrl_mpi_mod.F90`; the specialized `ICI/` path uses its own coupler and host modules. Their actual linking, rank ownership, and lifecycle must be checked in the relevant external build.

The repository therefore verifies that `CMF_DRV_*` entry points and conditional host-related paths exist. It does **not** provide enough evidence here to specify all IFS or MIROC call sequences, symbol conventions, threading guarantees, supported precision modes, restart exchange, or release compatibility. In particular, no MIROC-specific integration contract is documented by these files. Verify those requirements against the maintained host-side integration source and build configuration before making interface claims.

Changes to driver arguments, module variables, initialization order, time-step scheduling, precision kinds, calculation order, and restart or output layouts are integration-sensitive. Their compatibility impact should be assessed before editing; the authoritative approval and change rules are in [AGENTS.md](../AGENTS.md).

### Developing a host-specific driver

There is no formal plugin loader or single required custom-driver interface in this source tree. A host integration can start by identifying the CTRL/CALC routines and state it needs, then implement the host-facing setup, forcing/state exchange, timing loop, output exchange, and finalization that fit that host. It may reuse suitable standard driver routines, call selected controllers/calculations directly where appropriate, or provide additional host-specific control modules. A separate host entry point can coexist with `MAIN_cmf`, as `MAIN_cmf_ici` does; external coupling does not inherently require changing the standard main program or driver. Whether a shared CTRL/CALC change is needed depends on the host requirements and interfaces available. The ICI/ILS example is not a mandatory recipe.

Before wiring a custom driver, trace the interfaces and ordering assumptions in the modules it will use:

- Which routine initializes configuration, the map, geometry, shared arrays, forcing, and optional schemes, and in what order?
- At what point must the host provide runoff, boundaries, or other forcing, and how do host time and CaMa's outer/adaptive internal steps stay synchronized?
- Which CTRL routine owns each state update, and which CALC routines mutate that state or depend on prior calculations? CTRL and CALC modules have real dependencies and are not interchangeable layers.
- Which output, diagnostic, restart, and finalization responsibilities remain in the host versus the selected CaMa modules?
- How are MPI communicators/rank ownership, precision kinds, preprocessor flags, and required external libraries configured for the host build?

Check these against the actual host-side integration source as well as this repository. The public driver procedures documented above are facts about this code path; they do not define a universal API for all host systems.

## Adding functionality within the existing layout

These are placement and investigation hints based on current code, not new mandatory procedures.

| Change | Existing place to investigate | Questions to resolve in the current code |
| --- | --- | --- |
| Hydrodynamic calculation | `cmf_calc_*_mod.F90`, or `cmf_opt_*_mod.F90` for an existing alternate path; connect through `cmf_ctrl_physics_mod.F90`. | At what point in adaptive stepping does it run? Which state and diagnostic arrays does it read/write? Does its order affect fluxes, conservation, or parallel updates? |
| Optional physical scheme | Relevant component directory and `cmf_ctrl_physics_mod.F90` / `cmf_drv_control_mod.F90` hooks. Heatlink is a current example; sediment has its own guarded path. | How are compile-time and runtime switches combined? What are initialization, forcing, per-step, output, and finalization hooks? Which state, precision, time interval, and restart records are involved? |
| Persistent state variable | The relevant `yos_cmf_*` state module or extension-owned state module; initialization often belongs in `cmf_ctrl_vars_mod.F90` or the owning component controller. | Who allocates and initializes it? What are its dimensions, units, kind, MPI ownership, disabled-feature behavior, and restart lifecycle? |
| Configuration option | Existing namelist declarations/readers and `CMF_CONFIG_CHECK`; compile-time selections are in `adm/Mkinclude*`. | Does it change build shape or runtime behavior? What is the default? Which combinations are invalid? Is the option represented in stand-alone and host paths? |
| Input/output | Core `cmf_ctrl_forcing_mod.F90`, `cmf_ctrl_output_mod.F90`, `cmf_ctrl_restart_mod.F90`, or the relevant `io/`/component modules. | Does the field use legacy binary or NetCDF code, configurable I/O, or extension-specific files? Are units, ordering, missing values, and restart compatibility preserved? |
| Tests | Tests and Makefiles under `common/test`, `mod/test`, `io/test`, `phys/test`, and `heatlink/test`. | Does the target merely build a test executable, or run it? Is there a synthetic integration runner? A full model regression needs an identified dataset and baseline. |

The directory Makefiles and [`src/Makefile`](Makefile) are the source of truth for current object and library membership. A module being present under `src/` does not mean it is included in the default stand-alone build. The `test` targets in `common/`, `phys/`, and `heatlink/` build test programs; separate targets such as `mod/test-input-mapping` and `io/test-input-slice` invoke synthetic test runners. Check the recipe before describing a target as executed or a feature as tested.

## Related documentation

- [Repository overview and citations](../README.md)
- [Authoritative development rules](../AGENTS.md)
- [User guideline](../doc/CaMa-Flood_Guideline.md)
- [Main v4.20 manual](../doc/Manual_CaMa-Flood_v420.docx)
- [Reservoir operation manual](../doc/Manual_ReservoirOperation_v420.docx)
- [Levee method note](../doc/levee.md) and [levee parameter manual](../doc/LeveeParameter.docx)
- [Upstream inflow scheme manual](../doc/Mannual_UpstreamInflowScheme.docx)
- [High-performance computing note](../doc/Note_HighPerformanceComputing.docx)
- [Heatlink component README](heatlink/README.md)

Use the detailed manuals for scientific methods, user workflows, and file/configuration details. This source guide should remain an architectural map rather than a duplicate of those manuals.
