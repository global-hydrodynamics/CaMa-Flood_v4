# Additional tools and specialized workflows

`etc/` contains additional CaMa-Flood tools, model-extension examples, and
specialized workflows for preparing data, analyzing results, validation,
visualization, and particular computing environments.

These components are generally not required for the standard `gosh/` examples.
Their roles and requirements differ; the directory is not a single installation
or execution workflow.

## Directory guide

| Directory | Purpose | Documentation or entry point |
| --- | --- | --- |
| `downscale_flddph/` | Postprocessing: downscales simulated flood depth and flood duration using higher-resolution map layers. | [Guide](downscale_flddph/ReadMe.md) |
| `levee_params/` | Preprocessing and specialized runs: prepares levee parameters, estimates heights through simulation cycles, and supports levee simulations and downscaling. | [Levee preparation guide](../doc/levee.md) |
| `n-year_flood_depth/` | Postprocessing: fits distributions to annual maxima and estimates flood depth for selected return periods, with downscaling and plotting. | [Main workflow](n-year_flood_depth/script/main.sh); [user guideline](../doc/CaMa-Flood_Guideline.md#44-flood-hazard-map) |
| `old_levee/` | Historical levee-parameter, simulation, analysis, and downscaling examples. Check their parameter conventions against the model version before reuse. | [Parameter example](old_levee/x01-levee_param_simple.sh); [simulation example](old_levee/s01-simulation_sample_param.sh) |
| `options_HPC/` | Computing-environment workflows: MPI partition preparation and run examples, vector-processor options, and floating-point reproducibility notes. | [Guide](options_HPC/README.md) |
| `result_mapping/` | Visualization: plots spatial model output from plain-binary or NetCDF files. | [Binary example](result_mapping/s01-mapping_plot_binary.sh); [NetCDF example](result_mapping/s02-mapping_plot_netcdf.sh) |
| `runoff_preset/` | Preprocessing: prepares daily and long-term runoff climatologies and modifies runoff near coastal cells for forcing/parameter-preparation workflows. | [Climatology example](runoff_preset/s01-daily_climatology.sh); [coastal adjustment](runoff_preset/s02-mod_coast.sh) |
| `sealev_boundary/` | Specialized boundary-condition workflow: prepares GTSM sea-level inputs and demonstrates a Mozambique simulation with time-varying sea level. | [Preparation notebook](sealev_boundary/preprocess_gtsm.ipynb); [run example](sealev_boundary/test5-moz_06min_sealev.sh) |
| `sediment/` | Model-extension example: runs the sediment-transport configuration using the separate executable built under `src/sediment/`. | [Run script](sediment/run_sediment.sh) |
| `storage_to_floodstage/` | Postprocessing: diagnoses river and floodplain depths, flooded area, and related fields from saved total water storage and map parameters, with a levee option. | [Conversion script](storage_to_floodstage/s01-conv_storge2fldstg.sh) |
| `traser_v422_test/` | Tracer-extension example and Japanese notes for v4.22. Review this version-specific configuration before adapting it to another model version. | [Run script](traser_v422_test/trace.sh); [notes (Word)](traser_v422_test/CaMaトレーサーv422メモ.docx) |
| `upstream_inflow/` | Preprocessing and specialized runs: masks upstream catchments, prepares inflow time series, and demonstrates prescribed upstream inflow. | [Manual (Word)](../doc/Mannual_UpstreamInflowScheme.docx); [run example](upstream_inflow/g02-upst_inflow.sh) |
| `validation/` | Validation: compares discharge, water-surface elevation, and flood-water extent outputs with observation data. | [Guide](validation/ReadMe.md) |

The links identify existing guides or representative entry points. Read the
selected component's files before choosing which steps to run.

## Usage and maintenance notes

- Dependencies, compiler settings, Python environments, and configuration
  requirements vary by component.
- Some workflows require additional maps, forcing, observations, or other
  datasets. Check the files supplied with your package or obtain the inputs
  separately when using a GitHub clone.
- Review paths, working-directory assumptions, input/output formats, and cleanup
  behavior in the component documentation or source before execution.
- A component's presence here does not enable it in a standard simulation.
  Model extensions may need build options, runtime settings, and additional
  inputs; older examples also need review against the selected source version.

## Related documentation

- [Model overview and distribution information](../README.md)
- [Fortran source architecture and extensions](../src/README.md)
- [Standard build and execution examples](../gosh/README.md)
- [Build configuration](../adm/README.md)
- [River maps and preprocessing](../map/README.md)
- [Runoff forcing preparation](../inp/README.md)
