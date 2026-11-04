# Variable tables

This directory contains shared, machine-readable specifications for variables and data structures used by CaMa-Flood v5. These tables describe model and file contracts across datasets; they are not dataset catalogs.

- [`map_variables.csv`](map_variables.csv) documents map variables and products, including their meanings, units, types, dimensions, record layouts, missing values, and processing roles. See [`map_metadata_spec.md`](map_metadata_spec.md) for its schema and design notes.
- [`output_variables.csv`](output_variables.csv) documents model output names, meanings, units, internal variables, file precision, dimensions, temporal semantics, and output conditions.
- [`restart_variables.csv`](restart_variables.csv) documents Restart state names, meanings, units, internal and file precision, Binary record order, conditional state, and Storage-only behavior. Binary record positions after river and floodplain storage depend on `LSTOONLY` and the enabled groundwater, reservoir, and levee schemes; see the [Restart design and format notes](../src_main/README.md#normal-and-storage-only-restart-design).

Each table has its own schema and purpose; combining them into one CSV is not intended. Dataset registries describe specific products, sources, coverage, licensing, and access. For example, [`inp/runoff_datasets.csv`](../inp/runoff_datasets.csv) remains the registry for runoff datasets and is not part of this directory.

A separate `input_variables.csv` may be considered if future work needs a shared inventory for multiple time-varying model inputs. No such table is defined here yet.

For `output_variables.csv`, `alias_of` is the authoritative relationship between compatibility names and canonical outputs. An alias row may leave descriptive metadata such as `long_name`, `units`, `temporal_type`, and `missing_value` blank; runtime output initialization and the output CTL generator inherit those fields from the canonical row. The runtime reader uses `CMF_BASE` and `COUTVAR_TABLE` in `&NOUTPUT`: the latter defaults to `${CMF_BASE}/variable_table/output_variables.csv` and can point to another registry. Keep the alias's required-option condition consistent with the canonical output when one applies. The `runoff`/`rofsfc` pair and `runoffsub`/`rofsub` pair select identical model arrays while preserving the requested output name.
