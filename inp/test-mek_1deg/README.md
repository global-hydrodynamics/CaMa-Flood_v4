# Mekong 1-degree ELSE_GPCC runoff sample

This sample contains daily runoff for the Mekong map domain, extracted without rescaling from the long-term global 1-degree ELSE_GPCC product. The maintainer identifies the source as the ELSE_GPCC runoff product associated with Kim et al. (2009), “Role of rivers in the seasonal variations of terrestrial water storage over global basins,” [doi:10.1029/2009GL039006](https://doi.org/10.1029/2009GL039006). The maintainer designates this derived sample for redistribution in the GitHub repository and CaMa-Flood distribution package; this does not include the original global ELSE_GPCC files.

The original `ELSE_GPCC/Roff/` files are an internal laboratory dataset and are not part of this Git repository. In this checkout, `../ELSE_GPCC/Roff/` is a configurable source path (currently a local ignored link), and `../ELSE_GPCC/Roff.ctl` is the tracked grid metadata. The source CTL describes a global 360 × 180 grid at 1 degree, little-endian float32, `NtoS` row order (`yrev`), runoff in `mm/day`, and `-999` as undefined. The CTL declares 7,671 records beginning 1981-01-01, but the available source file names span 1979-01-01 to 2011-01-01; the CTL time count is therefore not used to establish file coverage. The extraction script checks every requested daily file by date and size.

## Extraction

The defaults in [`s01-extract_ELSE_GPCC.sh`](s01-extract_ELSE_GPCC.sh) are fixed to the Mekong map sample bounds and period:

- Domain edges: 90–110°E, 5–35°N, at 1 degree (20 × 30 cells).
- Inclusive dates: 1980-01-01 through 2010-12-31 (11,323 daily records, including leap days as present in the source date sequence).
- Source: `../ELSE_GPCC/Roff/`, configurable near the top of the shell script. Relative source paths are resolved from the script's directory, not the caller's working directory.

Run the script by path from any current directory, for example:

```sh
/path/to/CaMa-Flood_v4/inp/test-mek_1deg/s01-extract_ELSE_GPCC.sh
```

It invokes the adjacent standard-library-only [`extract_ELSE_GPCC.py`](extract_ELSE_GPCC.py) helper. The helper reads the global CTL geometry, verifies the 360 × 180 1-degree little-endian grid and `yrev` layout, calculates the regional window (zero-based columns 270:290 and rows 55:85), checks all source dates and 259,200-byte input sizes, and copies the selected float32 row slices unchanged. It preserves the source order, `mm/day` values, and `-999` sentinel. It does not write to the source directory or substitute values. It checks each output's 2,400-byte size and exact byte equality with the corresponding source window, checks that output values contain no unexpected NaN or infinity, and verifies the final date set and GrADS CTL.

Outputs are written beside the scripts:

```text
inp/test-mek_1deg/
├── README.md
├── s01-extract_ELSE_GPCC.sh
├── extract_ELSE_GPCC.py
├── runoff.ctl
└── runoff/
    ├── Roff____19800101.one
    ├── Roff____19800102.one
    └── ...
```

Expected output is 11,323 files × 2,400 bytes = 27,175,200 bytes (about 25.9 MiB), plus the CTL. The complete `test-mek_1deg/` directory, including all generated `.one` files, the CTL, extraction script, helper, and README, is tracked in Git and included in the distribution package. The original global ELSE_GPCC data remain external and excluded from Git and package contents.

The output CTL uses `Roff____YYYYMMDD.one`, `float32`, little-endian, `undef -999`, daily `tdef`, and `yrev`, with cell centers at 90.5–109.5°E and 5.5–34.5°N. Its `tdef` count is derived from the requested dates, not copied from the source CTL.

## CaMa-Flood configuration

For the Mekong sample map, generate an input matrix for this runoff grid with `ROFFTAG="test-mek_1deg"` (matching the registry `dataset_id`) and the following settings in `map/mekong_15min/script_params/s03-generate_inpmat.sh`:

```sh
GRSIZEIN=1
WESTIN=90
EASTIN=110
NORTHIN=35
SOUTHIN=5
OLAT="NtoS"
```

This creates `inpdim_test-mek_1deg.txt` and `inpmat_test-mek_1deg.bin` for the selected map. The matrix is map-specific; these settings describe the runoff grid. The matching daily binary forcing configuration is:

```text
LINPCDF = .FALSE.
CROFFREQ = "DAY"
LINTERP = .TRUE.
IFRQ_INP = 24
DROFUNIT = 86400000
CROFDIR = inp/test-mek_1deg/runoff
CROFPRE = Roff____
CROFSUF = .one
CINPMAT = map/mekong_15min/inpmat_test-mek_1deg.bin
```

The `mm/day` unit and 24-hour interval yield `DROFUNIT=86400000` for conversion to `m/s`, consistent with the existing CaMa-Flood forcing reader and the `test_1deg` run configuration. No model simulation is run by the extraction script. File-format and source-window checks do not establish independent scientific validity of the runoff product.

## Redistribution status

The original global ELSE_GPCC source dataset remains external and is not Git-tracked. The derived regional Mekong sample is approved for redistribution and tracked in full in GitHub and the CaMa-Flood distribution package.

The 2000–2001 daily fields also have a yearly NetCDF representation in [`../test-mek_1deg_nc/`](../test-mek_1deg_nc/). It is a value-preserving format conversion of these same source records; see that directory's README for its CF time metadata, regeneration command, and validation limits.
