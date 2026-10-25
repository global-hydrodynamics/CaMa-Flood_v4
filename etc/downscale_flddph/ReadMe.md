# Flood depth downscale
Written by Dai Yamazaki

For scientific methods and detailed requirements, see [Note_Downscaling.docx](../../doc/Note_Downscaling.docx).

Run these examples from `etc/downscale_flddph/` in the GitHub checkout or extracted package. Build the Fortran tools with `make -C src` using the active `adm/Mkinclude`. Provide the selected river map, its high-resolution supporting layers, and simulation outputs; these data must be supplied by the package or obtained/generated separately. Edit the map/output paths, target domain, period, and resolution in the selected shell script before execution.


## Contents
### s01-downscale_flddph.sh
Sample script for downscaling
- Global 15min simulation
- Downscale to 1min resolution using high-res topo data  (map/glb_15min/1min)
- Target: 2000 Sep

Note: 
Advanced downscaling option which consider tributary inundation is used as default.
See [`t01-downscale.sh`](t01-downscale.sh) for the selected downscaling executable.


### s11-downscale_duration.sh
Sample script to downscale flood duration. 
- Global 15min simulation
- First calculate the depth-day relationshio at each 15min grid, and then downscale the duration at 1min resolution.
- Target: 2000.

Both examples prepare `map` and `out` links and replace generated work/output directories. Review their paths and cleanup commands before running.
