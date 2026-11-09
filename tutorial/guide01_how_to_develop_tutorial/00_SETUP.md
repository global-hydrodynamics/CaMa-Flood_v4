# Guide01 Setup: Prepare a Tutorial-Development Workspace

This guide prepares a local CaMa-Flood checkout for following Guide01 and
developing tutorials with Chat AI and Agent AI. It is written for contributors
who may be new to Git branches, repository permissions, or AI coding tools.

> **Current checkout note:** In the repository state checked for this guide,
> Guide01 and its setup scripts are on `feature/gosh-refactor`; the default
> `master` branch does not yet contain them. Use the branch-specific clone
> command below until the project confirms that these files have been
> integrated into the default branch. This branch status can change.

## 1. What you need

### Accounts and development tools

| Need | Required? | Purpose |
| --- | --- | --- |
| Chat AI | Recommended for Guide01 | Discuss scientific and teaching goals, compare design choices, review results, and prepare focused implementation requests. Any suitable service may be used; a particular paid plan is not prescribed here. |
| Agent AI with local workspace access | Required to reproduce the coding-agent parts of Guide01; optional for a discussion-only exercise | Inspect repository files and, when authorized, edit them and run local commands. Codex is one example. Features and permission controls depend on the application. |
| Git | Required for the branch-based development workflow | Clone the repository, inspect changes, and keep work on a separate branch. |
| GitHub account | Not required to clone this public repository; generally needed to create a fork, push your work, or open a pull request | The repository is public. Remote contribution still requires authentication and permission to write to the chosen remote. |
| Fortran compiler, GNU Make, and NetCDF-Fortran | Required to build and run the model workflows in this Guide | The standard v5 workstation setup uses GNU Fortran, OpenMP, and NetCDF-Fortran. The setup helper checks these tools. Windows users should use WSL2 for the shell workflows described by the repository. |
| Python 3.10 or newer | Required for the repository's Python tools and Guide01 analysis/visualization | Guide01 uses packages including NumPy, NetCDF4, xarray, pandas, and Matplotlib. The repository setup installs its listed Python requirements and the bundled `cmf_colormap` package into a local virtual environment. |
| Sample map, runoff, and input-matrix data | Required for the example runs | The development branch includes the small Mekong samples used by Tutorial01 and the Guide01 workflow example. This source repository is not a substitute for the separate data-rich distribution package or all production datasets. |

You do not need to give an AI your password, GitHub token, SSH private key, or
other credentials. Authenticate with GitHub through your normal Git or
application workflow. Local file access and remote GitHub authentication are
separate: an Agent AI can inspect and edit a local checkout without having
permission to push it to GitHub.

## 2. Get the repository and create a work branch

The commands below use a `work` folder under your home directory as an
example. Choose another location if you prefer; the repository scripts do not
require a fixed machine-specific path.

```bash
mkdir -p ~/work
cd ~/work
git clone --branch feature/gosh-refactor --single-branch \
  https://github.com/global-hydrodynamics/CaMa-Flood_v4.git
cd CaMa-Flood_v4
```

`git clone` downloads the selected branch into a new `CaMa-Flood_v4/`
directory. Cloning this public repository does not require a GitHub account.
If Git reports that the branch is unavailable, check the repository's current
branch and tutorial-integration status before switching to another branch.

Create your own branch before editing. A branch is a separate line of work
that keeps your changes apart from the shared base branch:

```bash
git switch -c guide01-my-work
git status --short --branch
git branch --show-current
```

The second command should show your new branch. Check the status whenever you
start or finish a task. If the checkout already contains work, read its status
and ask its owner before changing or cleaning anything; do not discard
unfamiliar changes.

### Contributing when you cannot push to the project repository

You can still develop locally without upstream write permission. To publish
changes, create a GitHub fork, clone your fork, and push your personal branch
there. A pull request can then propose the changes to the project repository.
If your fork does not contain `feature/gosh-refactor`, fetch that branch from
the project repository and base your personal branch on it. GitHub's fork and
pull-request help explains the web workflow; repository owners decide whether
and when to merge contributions. See GitHub's guides to [forking a
repository](https://docs.github.com/en/pull-requests/how-tos/work-with-forks/fork-a-repo)
and [creating a pull request from a
fork](https://docs.github.com/en/pull-requests/how-tos/create-pull-requests/creating-a-pull-request-from-a-fork).

### A small Git command reference

Run repository-specific commands from the cloned repository root unless a
step says otherwise.

```bash
git status --short --branch       # Show the branch and changed files.
git diff                          # Review changes to tracked files.
git add tutorial/path/to/file.md  # Stage only the file you intend to include.
git diff --cached                 # Review exactly what is staged.
git commit -m "Describe the change" # Save the staged change locally.
git push -u origin guide01-my-work  # Publish your branch, if authorized.
```

`git add` stages selected changes for the next commit; it does not publish
them. `git commit` records them in your local repository. `git push` sends the
branch to the remote and requires suitable authentication and write
permission. Review both `git status` and the diff before staging or committing.
For a broader introduction, see the [Git documentation](https://git-scm.com/book/en/v2).

## 3. Give Agent AI access to the local checkout

Open or select the cloned `CaMa-Flood_v4/` directory as the Agent AI's
workspace. Grant only the local file access and command execution needed for
the task. For a documentation task, read/write access to the relevant
documentation and permission to run read-only checks may be enough. A model
build needs permission to run the local compiler and Make; it does not by
itself require GitHub write access.

Permission mechanisms vary by application and execution environment. Confirm
the Agent AI is in the intended repository before asking it to work. From the
repository root, a safe read-only first request is:

> Please inspect this CaMa-Flood checkout without modifying any files. Report
> the repository root, active branch, `git status --short --branch`, and the
> locations of `tutorial/`, `src_main/`, `map/`, and `inp/`. Do not run builds,
> simulations, or Git write commands.

Check that the reported branch and paths agree with your terminal. For later
tasks, specify the files or directories the Agent AI may change, the expected
result, and whether builds or tests are authorized. Ask it to preserve
unrelated changes and to report commands and results. Do not grant broad
permissions just for convenience.

Local filesystem permission lets an agent inspect or modify files in the
workspace. GitHub authentication authorizes remote operations such as pushing
or creating pull requests. One does not imply the other. Keep credentials out
of prompts and repository files.

## 4. Give Chat AI enough repository context

Chat AI can help clarify the research question, learning objectives,
alternative designs, and how to review results. Start with the public
repository URL and the branch or commit you are using. For example:

> I am developing a CaMa-Flood tutorial from the
> `feature/gosh-refactor` branch of
> https://github.com/global-hydrodynamics/CaMa-Flood_v4. Please review the
> repository purpose and structure, `tutorial/`, Guide01's authoring approach,
> and the relevant existing tutorials. Help me identify the scientific and
> educational decisions that need researcher input before implementation.

Whether Chat AI can inspect a public repository directly depends on the
product and its enabled integrations. A public URL does not give it access to
your uncommitted local changes. It also may be looking at another branch or
an older commit. When precision matters, provide the branch name and commit
identifier, attach or paste the relevant current files, and share the Agent
AI's concise inspection or test report. Do not assume that Chat AI sees files
edited locally by Codex or another agent.

If direct repository access is unavailable, give Chat AI only the context
needed for the question: relevant excerpts, the design document, current
configuration, a figure, and/or a report. Remove secrets and private data
before sharing.

## 5. Prepare and verify the local development environment

The repository keeps v5's active compiler configuration separate from the
legacy v4 configuration. The active `config/Mkinclude` is machine-specific
and ignored by Git. Do not overwrite an existing configuration without
reviewing it.

### Fortran and NetCDF-Fortran

From the repository root, run the diagnostic and setup helper:

```bash
./s01-setup_fortran.sh
```

It checks GNU Fortran, GNU Make, OpenMP, and NetCDF-Fortran. In an interactive
terminal it can offer to write a suggested root-level `Mkinclude`; it does
not activate or replace `config/Mkinclude` or legacy `adm/Mkinclude`. Review
the generated file, then copy it only if there is no active v5 configuration
and you intend to use it:

```bash
if [ ! -e config/Mkinclude ]; then
    cp Mkinclude config/Mkinclude
else
    printf '%s\n' 'config/Mkinclude already exists; review it before replacing it.'
fi
```

If the helper cannot verify a requirement, follow the [v5 build
guide](../../config/README.md) for supported configuration templates and
manual setup. Do not copy v4's `adm/Mkinclude` into the v5 configuration.

The standard model build is:

```bash
make -C src_main all
```

This builds the standard v5 executable. The Guide01 workflow example uses a
separate NetCDF-enabled executable at `out/bin/MAIN_cmf_netcdf`. After
preparing `config/Mkinclude`, build it with:

```bash
bash gosh_sample/compile_netcdf.sh
```

This requires GNU Make, GNU Fortran, and the NetCDF-Fortran `nf-config` tool.
The wrapper builds in a temporary directory and installs the executable at
the path above. See [model build
instructions](../../src_main/README.md#netcdf-enabled-build) for details.

### Python

From the repository root, create or update the recommended isolated Python
environment:

```bash
./s02-python_setup.sh
source .venv/bin/activate
```

The setup helper asks before changing `.venv/`. It installs `requirements.txt`
and the bundled `cmf_colormap` package there, not into system Python. If it is
run without an interactive terminal, it only diagnoses the environment. The
repository's [Python environment guide](../../docs/python_environment.md)
covers existing venv, Conda, and managed environments.

Check the interpreter, core packages, and colormap package in the activated
environment:

```bash
python -c "import sys; print(sys.executable)"
python -c "import matplotlib, netCDF4, numpy, pandas, xarray; from cmf_colormap import get_cmap; print(get_cmap('shade_rainbow').name)"
```

The second command should print `shade_rainbow` if the packages used by the
Guide01 analysis and visualization are importable. If a dependency is missing,
check that the setup and the script use the same Python interpreter.

### Confirm the example data and run a small documented workflow

The current development branch includes the Mekong map, runoff sample, and
input matrix used by the examples. From the repository root, check their
presence:

```bash
test -f map/mekong_15min/params.txt
test -f inp/test-mek_1deg_nc/runoff2000.nc
test -f map/mekong_15min/inpmat_test-mek_1deg.bin
test -f map/mekong_15min/inpdim_test-mek_1deg.txt
```

These checks confirm that expected files are present; they do not validate
scientific correctness. For a first model smoke test, follow the tested
40-day [Tutorial01 manual](../tutorial01-get_started/manual_tutorial01-get_started.md):
build with `make -C src_main all`, then run the script from its own directory:

```bash
cd tutorial/tutorial01-get_started
./tutorial01-get_started.sh
```

The sample script keeps its generated run products in the tutorial's ignored
`out/` directory. It is a short operational check, not a scientific
validation. Guide01's worked river-depth example uses NetCDF inputs and
outputs, Python analysis, and one-minute downscaling; after setup, follow its
[learner manual](workflow_example/manual_workflow_example.md) for those
additional steps. The repository root [README](../../README.md) distinguishes
these development samples from the official data-rich distribution package.

### Readiness checklist

- [ ] I can identify the repository root and see my personal working branch.
- [ ] `git status --short --branch` reports the branch and any local changes.
- [ ] Agent AI can inspect the intended checkout and explain its status; any
      edit or command permissions are limited to the approved task.
- [ ] Chat AI has the relevant branch/commit and design context, or I know how
      to provide the needed files and reports directly.
- [ ] The v5 compiler configuration is available, and I can build the model.
- [ ] The Python interpreter can import the packages required by my chosen
      workflow.
- [ ] Required sample data exist, and I can run an appropriate short example
      before developing a new tutorial.

## 6. Troubleshooting

| Symptom | Safe first check |
| --- | --- |
| `git` is not found | Run `git --version`. Install Git using the instructions for your operating system, then reopen the terminal. |
| GitHub asks for authentication while cloning the public repository | Confirm the clone URL is the public HTTPS URL above. A GitHub account is not needed just to clone public content. |
| Push is rejected | Check `git status --short --branch` and `git remote -v`. You may lack write access to the project remote; use a fork and pull request rather than changing permissions or credentials. |
| Agent AI reports the wrong project or branch | Ask it to report `pwd`, `git rev-parse --show-toplevel`, and `git branch --show-current`. Select the cloned repository root as its workspace. |
| Agent AI can read but not edit or run a command | Review that application's workspace permissions and grant only the specific local access needed. Remote GitHub authentication is a separate setting. |
| Chat AI discusses an outdated file | Provide the branch, commit hash from `git rev-parse --short HEAD`, and current file contents or Agent AI findings. Public repository access may not include local edits. |
| Local changes do not appear on GitHub | `git status` shows whether files are staged or committed; `git push` publishes a committed branch only when authorized. |
| `gfortran`, `make`, or `nf-config` is missing | Run `./s01-setup_fortran.sh` from the repository root for diagnostics, then consult the [v5 build guide](../../config/README.md) for your OS and library installation. |
| Python import fails | Activate `.venv/`, check `python -c "import sys; print(sys.executable)"`, and follow the [Python environment guide](../../docs/python_environment.md). |
| A sample dataset is missing | Confirm the checkout branch and the paths in Section 5. The source repository and the official distribution package have different contents; do not substitute unrelated data silently. |

## 7. How the AI-assisted workflow uses this setup

The prepared workspace supports an iterative collaboration:

**Human researcher → Chat AI discussion and design → Agent AI repository
investigation and implementation → Human/Chat AI review → Agent AI revisions**

The human researcher remains responsible for scientific decisions,
interpretation, and approval. Chat AI helps formulate the problem and
evaluate options. Agent AI checks the actual checkout and implements the
approved task. Share relevant design files, prompts, test summaries, and
figures between the two AI roles; include branch or commit information when
their conclusions depend on the exact source. Guide01's numbered records show
this process in a real example. You can follow its phases from the
[Guide01 overview](README.md) without reading every historical exchange.
