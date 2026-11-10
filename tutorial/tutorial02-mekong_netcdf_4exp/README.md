# Tutorial 02 — Mekong four-experiment comparison

The learner starting point intentionally provides only the working Default
experiment. Copy and edit `mek_default.sh` to create independent No
Bifurcation, Kinematic Wave, and No Floodplain scripts; give each a distinct
`PROJ` so their outputs remain separate. The completed alternatives are
developer references under `design/` in the GitHub development repository and
are excluded from learner packages. The full learner manual is not yet
available; see [`design/manual_design.md`](design/manual_design.md) for its
planned activity sequence. The experiment's scientific settings and evidence
are documented in [`tutorial_design.md`](tutorial_design.md).

Tutorial 02 compares the four Mekong model configurations described in
[`tutorial_design.md`](tutorial_design.md). Figure 4 uses the bundled
`cmf_colormap` package for its annual-maximum discharge comparison. The repository
`.venv` is recommended, but using an existing Python environment is also
supported when its dependencies and `cmf_colormap` are available.

### Recommended: repository-local environment

From the CaMa-Flood repository root, prepare and activate `.venv`:

```bash
./s02-python_setup.sh
source .venv/bin/activate
```

Then run the visualization workflow with that environment:

```bash
bash tutorial/tutorial02-mekong_netcdf_4exp/v00-visualize_all.sh
```

### Existing Python environment

Advanced users can keep their shared venv, Conda, or HPC-managed Python. From
the repository root, install the bundled package into the chosen interpreter:

```bash
python -m pip install --no-deps ./visualization/cmf_colormap
PYTHON=python bash tutorial/tutorial02-mekong_netcdf_4exp/v00-visualize_all.sh
```

Use the same Python for installation and execution; `python` and `python3` may
resolve to different interpreters. `--no-deps` does not install Matplotlib,
NumPy, NetCDF4, or other missing dependencies. The setup script only modifies
the repository `.venv`; it does not manage existing environments.

The visualization workflow expects annual model outputs from all four
experiments and the shared map data. Create the three alternative scripts and
run them before generating comparisons. Approved expected-result figures are
stored in `sample_figure/`; generated figures belong in `figure/`, which starts
without generated images. If `s02-python_setup.sh` reports that no interactive
terminal is available, rerun it from a terminal to approve setup. For detailed
verification and troubleshooting, see the
[Python environment guide](../../docs/python_environment.md).

There is no dedicated tutorial package assembler yet. The legacy v4
`doc/make_package.sh` is not used for tutorial distribution; `design/` and
completed alternatives must be excluded by any future tutorial packaging
workflow.

## Figure 4 — annual maximum discharge

`v04-annual_max_discharge.py` compares 2001 in a 3 × 4 layout. Row 1 maps the
computational-grid `max_t |Q(t)|`; Row 2 shows the same annual maximum absolute
daily-mean discharge on geographic river-network connections; and Row 3 shows
main-channel differences between adjacent model configurations. From left to
right, Row 3 shows `Default − No Bifurcation`,
`No Bifurcation − Kinematic Wave`, and
`Kinematic Wave − No Floodplain`, titled **Impact of bifurcation scheme**,
**Impact of local inertial equation**, and **Impact of floodplain activation**,
respectively. Each panel is centered between the two configuration panels it
compares. Columns are Default, No Bifurcation,
Kinematic Wave, and No Floodplain. Read the difference sequence from right to
left to follow the requested progression from No Floodplain toward Default.
Rows 2 and 3 use a grayscale background from
`map/mekong_15min/1min_nc/1min.elevtn.nc`; elevation is transformed with
`log1p` and clipped to the local 2nd–98th percentiles. The darker gray range
makes lowland relief more visible while keeping the background subdued.
In the network and difference rows, bifurcation lines are drawn first; main
channel segments are layered from smaller to larger upstream contributing area,
so the main channels remain visible at confluences.

Rows 1 and 2 use `shade_rainbow` with a shared positive logarithmic scale, each
with its own colorbar labeled at 1, 2, 5, 10, 20, 50, 100, 200, 500, 1,000,
2,000, 5,000, 10,000, 20,000, and 50,000 m³/s. Zero is shown separately in
gray because it cannot appear on a logarithmic axis. Row 3 uses `diverge_rainbow`
and a symmetric color scale
around zero with log-like levels at ±10, ±30, ±100, ±300, ±1,000, ±3,000,
and ±10,000 m³/s. Values outside ±10,000 use the end colors, shown by the
colorbar extensions. Each segment's annual maximum is calculated independently,
so maxima need not occur on the same day; absolute-value colors do not encode flow direction.
Zero discharge is gray in the absolute rows, and positive values below 1 m³/s
use the explicitly marked under-range.

Main-channel connections use the source cell's `outflw`. Default additionally
shows channel-bifurcation paths from `pthflw`, matched to the map's `bifprm.nc`
by `pth_id` and endpoint indices. `LPTHOUT` is enabled only for Default. The
other three configurations disable bifurcation routing, so their physical
network has no bifurcation paths; those panels show main-channel connections
only, not missing diagnostic data. Row 3 compares only main-channel discharge.
Thin gray dashed bifurcation lines there are positional references only; no
bifurcation-flow differences are calculated or implied.

Default runs now write both `o_outflwYYYY.nc` and `o_pthflwYYYY.nc`; the other
experiment scripts continue to request only `outflw`. If an older Default run
does not contain `o_pthflw2001.nc`, rerun the Default experiment from the
repository root:

```bash
bash tutorial/tutorial02-mekong_netcdf_4exp/mek_default.sh
```

This regenerates Default's 2000–2003 annual runs, including the Restart handoffs,
and replaces that experiment's existing local outputs. It does not rerun the
other experiments. Then regenerate Figure 4 with the standard visualization
workflow or run `v04-annual_max_discharge.py` directly.
