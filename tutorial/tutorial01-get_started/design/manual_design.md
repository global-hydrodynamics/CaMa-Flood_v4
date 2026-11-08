# Manual Design — Tutorial01: Get Started

**Audience:** first-time CaMa-Flood users comfortable copying terminal commands, but unfamiliar with the v5 repository, Fortran builds, river maps, runoff forcing, and GOSH experiments.

The experiment contract is [`../tutorial_design.md`](../tutorial_design.md). Shared evidence and writing instructions are in [`../../agent_manual_authoring.md`](../../agent_manual_authoring.md).

## Learning outcomes

Tutorial01 checks introductory navigation and workflow skills only. Readers should be able to:

- Obtain the Tutorial01 files from GitHub, using the temporary development branch before integration and default `master` afterward.
- Locate the repository root and the main source, map, input, and tutorial directories.
- Run the setup scripts and follow their displayed choices without accidentally replacing an existing configuration.
- Build or update `MAIN_cmf` from `src_main/` with `make all`.
- Locate the map, runoff, and input-mapping files named by the GOSH settings.
- Find `PROJ`, input paths, and output paths in the GOSH script.
- Run Tutorial01 and locate its model output and log.

These outcomes do not require learners to explain physical map variables, model parameters, numerical methods, hydrodynamic processes, or the mathematical details of input mapping. The manual may give brief context for terms needed to follow the steps, but Learning Checks should not assess those topics.

The supported official v5 package for Tutorial01 is not yet available. Direct users to wait for its release if they prefer the official distribution; older official packages are not a substitute for the v5 tutorial workflow.

## Manual-wide conventions

- Begin by explaining that the v5 development checkout is the tutorial environment and that a successful run does not validate scientific accuracy.
- Immediately after showing the repository root with `pwd`, set `cmf_root="$(pwd)"` in the same shell session and explain that a new terminal must define it again.
- At the start of each major command section, establish the root with `cd "${cmf_root}"`. Then enter a relevant subdirectory with a relative path when needed. Use `pwd` before listing a directory. Within one major section, do not repeat root-navigation commands before every related command.
- For directory inspection, show a concise **Expected output** block based on real output. Include representative file names and properties; if entries are omitted, mark the excerpt as abbreviated. Explain that owners, dates, and some sizes vary by checkout or platform.
- Use `ls -lF` for beginner directory listings, and `ls -lhF` when human-readable sizes help. Explain that `/` marks directories and `*` normally marks executable files; these suffixes are display markers, not filename characters. Use output captured from the exact command shown.
- Use the five-part instructional pattern—Purpose, Commands, Expected output, Explanation, Required action—where it helps the reader. Interactive setup sections combine the last three under **Output and required actions**. Do not force the pattern onto trivial steps.
- End every major instructional section with one or two short **Learning Check** checklist items. Derive them from the introductory learning outcomes; they must be directly answerable from that section and check basic navigation, file/settings lookup, or successful workflow completion. Do not assess scientific mechanisms, model physics, or numerical methods. Do not add checks to introductions or developer-only notes.
- Introduce data progressively: locate and list files, explain what they represent, then show the settings that select them.
- Keep command blocks short and literal. Avoid advanced shell pipelines when `cat`, `ls`, or opening the file in an editor teaches the intended concept more directly.
- Put terminal output in separate blocks from instructions. Identify representative environment-dependent details and do not invent prompts or output.

## Required demonstrations by section

| Section | Learner should understand | Inspect and demonstrate | Expected output required? | Learner action and successful completion | Learning Check |
| --- | --- | --- | --- | --- | --- |
| 0. Obtain CaMa-Flood | GitHub is the Tutorial01 route. Use `feature/gosh-refactor` until its files are integrated; use default `master` after integration. The supported official v5 package is a future release. | Give the temporary branch clone command and GitHub **Code → Download ZIP** instructions for the same branch. Show the unqualified clone command as the post-integration method. Link the official website briefly and state that users should wait for the supported v5 package rather than use an older one. | No terminal output required. Label each Git command by when it applies; don't imply the current `master` clone runs the tutorial. | Clone or download/extract the current branch, place it in a suitable directory, and locate its root. | Can obtain the current Tutorial01 branch and identify the root; can state that `master` is the standard route after integration. |
| 1. Find the repository root | The checkout directory is the repository root; `ls -lF` visually marks directories and executable scripts. | Run `pwd`; assign `cmf_root="$(pwd)"`; inspect the root with `ls -lF`. | Yes: abbreviated actual `ls -lF` output with `/` and `*` markers. | Keep using the same terminal session for later commands. Explain that suffixes mark file types and are not part of filenames. | Can locate `src_main/`, `map/`, and `inp/`, and identify directory and executable markers. |
| 2. Prepare the environment | The setup scripts display checks and ask before writing local configuration. Python setup is optional for this tutorial. | Run both setup scripts and show representative checks and exact prompts. | Yes: setup results, prompts, and resulting actions. | Follow the checks; protect an existing `config/Mkinclude` before copying the generated file. | Can recognize successful setup checks and the configuration file involved. |
| 3. Build the model | The model executable is located in `src_main/`. | Enter `src_main/`, run `make all`, then list `MAIN_cmf`. | Yes: successful build completion and executable listing. | Run the build command and confirm `MAIN_cmf` is present. | Can locate `MAIN_cmf` after `make all`. |
| 4. Read the experiment settings | The specification describes the experiment; the GOSH script contains its settings. | Inspect `tutorial_design.md` and the standalone script using `cat` and a text editor. | Yes: visible indicators from the two files. | Find a setting in the script without modifying it. | Can locate the GOSH script and find a setting shown in the specification. |
| 5. Locate river-map files | The GOSH script selects map files by directory and filename. | List actual map files and show `MAPDIR`, `NEXTXY`, and `RIVWTH`. | Yes: real filenames and representative sizes. | Match the selected file paths to files in the map directory. | Can identify the map directory and the selected connectivity and width files. |
| 6. Locate runoff files | The runoff directory contains dated files referenced by the script. | List January 1, January 2, and February 9 files; show dataset, directory, and filename settings. | Yes: actual filenames and representative sizes. | Match a listed runoff file to the script's path/name settings. | Can find the daily runoff files and corresponding GOSH settings. |
| 7. Locate input-mapping files | The dimension descriptor and matrix are separate files referenced by GOSH settings. | List and display the descriptor/matrix pair; show `INPDIM`, `INPMAT`, and `LINTERP`. | Yes: real sizes and descriptor excerpt. | Match each GOSH variable to the corresponding file. | Can locate both files and identify their settings. |
| 8. Run the experiment | The sample writes results under the directory selected using `PROJ`. | Show `PROJ`, `RUNDIR`, run command, success/status, and output listing. | Yes: captured run excerpt and result files. | Run from the documented directory and locate the selected run directory. | Can recognize successful completion and locate the run directory. |
| 9. Locate results | The output directory contains model outputs and logs. | List project outputs and show the model's normal completion message. | Yes: representative result files and completion evidence. | Locate the discharge output and model log. | Can find `outflw2000.bin` and `log_CaMa.txt`. |

## Experiment facts that the manual may summarize

The settings table should state: project `tutorial01-get_started`; map `map/mekong_15min`; runoff `inp/test-mek_1deg`; mapping descriptor `inpdim_test-mek_1deg.txt` and matrix `inpmat_test-mek_1deg.bin`; 2000-01-01 00:00 through 2000-02-10 00:00 exclusive; `DT=3600` seconds with adaptive stepping; non-MPI and one OpenMP thread; daily Binary outputs `outflw`, `storge`, `sfcelv`, `flddph`, and `fldfrc`; project output under `tutorial/tutorial01-get_started/out/${PROJ}/`.

The manual should not repeat the complete experiment specification, reproduce the whole shell script, introduce visualization or observational comparison, or claim that the simulated values are scientifically validated.
