# Environment inventory

No complete baseline execution environment has been frozen. The following is a one-time capture for validation rerun during the 2026-10-28 history audit; it is not a reproducibility claim for the 2026-10-07 baseline.

| Field | Recorded value | Evidence/reference |
| --- | --- | --- |
| Environment ID | `AUDIT-macos-arm64-2026-10-28` | This research audit |
| Collection date | `2026-10-28` (date precision) | Current environment inspection |
| OS/architecture | Darwin 23.3.0, arm64 | `uname -srm` |
| Python | Python 3.13.3 | `python3 --version` |
| Fortran compiler | GNU Fortran 14.2.0 (Homebrew GCC 14.2.0_1) | `gfortran --version` |
| NetCDF | 4.6.2 | `nf-config --version` |
| MPI/OpenMP | Not separately inventoried; tests ran the Fortran NetCDF smoke test with the local compiler; no model simulation was run | Test commands in `VR-0002` |
| Source revision | `39dc3df7c092bdbd1825cd9e3063d0f669dbe871`; research-only worktree changes were present | `git rev-parse HEAD`; stage record |
| Dataset | Git-tracked `map/mekong_15min/` as included at source revision; no separate checksum recorded | `map/tests/validate_mekong_sample.py` |
| Dependencies | Python/Fortran/NetCDF dependencies available in the running environment; exact Python package versions not captured | Converter test output; no lockfile snapshot |
| Limitations | This environment is not the baseline build environment; no compiler or runtime was frozen for the v4 baseline | Pre-document baseline record |

## Additional capture: 2026-10-30 map-tools CTL tests

| Field | Recorded value | Evidence/reference |
| --- | --- | --- |
| Environment ID | `AUDIT-macos-arm64-2026-10-30` | This research validation |
| Collection date | `2026-10-30` (date precision) | Local environment inspection |
| OS/architecture | Darwin 23.3.0, arm64 | `uname -srm` |
| Python | Python 3.13.3 | `python3 --version` |
| Fortran compiler | GNU Fortran 14.2.0 (Homebrew GCC 14.2.0_1) | `gfortran --version` |
| GrADS | OpenGrADS 2.2.1.oga.1 is installed; no successful data-query smoke result was established in this run | `grads -v`; attempted batch commands did not produce query output |
| NetCDF | 4.6.2 | `nf-config --version` |
| Source revision | `c2b1aac25e297607fb4adba2023b5cb6fb5326a8`; documentation, registry, generator, and test changes were uncommitted; existing Mekong CTL edits/deletions were preserved | `git rev-parse HEAD`; `git status --short` |
| Dataset | Git-tracked Mekong map sample; CTL integration generated only in temporary directories | `map/tests/validate_mekong_sample.py`; `map/tools/tests/test_mekong_ctl_integration.py` |
| Limitations | No map generation, full model simulation, network validation, or scientific comparison. The Fortran compiler emitted existing int32-to-int16 narrowing warnings in `region_combine_hires.F90`. | Validation record `VR-0018` |
