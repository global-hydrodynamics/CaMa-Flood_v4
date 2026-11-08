#!/usr/bin/env bash
# Diagnose Python and optionally create/update the repository-local environment.
set -eu

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
VENV_DIR="$SCRIPT_DIR/.venv"
REQUIREMENTS="$SCRIPT_DIR/requirements.txt"

fail() {
    printf '[ERROR] %s\n' "$1" >&2
    exit 1
}

check_python() {
    "$1" -c 'import sys; raise SystemExit(0 if sys.version_info.major == 3 and sys.version_info >= (3, 10) else 1)' \
        >/dev/null 2>&1
}

check_venv_tools() {
    "$1" -c 'import ensurepip, venv' >/dev/null 2>&1
}

describe_origin() {
    local python_path="$1"
    local base_prefix
    local stdlib_dir
    local externally_managed

    if ! base_prefix=$("$python_path" -c 'import sys; print(sys.base_prefix)' 2>/dev/null); then
        printf 'Origin not identified; system-managed: Unknown\n'
        return
    fi
    if ! stdlib_dir=$("$python_path" -c 'import sysconfig; print(sysconfig.get_path("stdlib"))' 2>/dev/null); then
        printf 'Origin not identified; system-managed: Unknown\n'
        return
    fi
    if [[ -f "$stdlib_dir/EXTERNALLY-MANAGED" ]]; then
        externally_managed="Yes (PEP 668)"
    else
        externally_managed="Unknown"
    fi

    if command -v conda >/dev/null 2>&1 && [[ -d "$base_prefix/conda-meta" ]]; then
        if [[ "${CONDA_EXE:-}" == *miniforge* || "${CONDA_EXE:-}" == *Miniforge* ]]; then
            printf 'Conda/Miniforge; system-managed: No\n'
        else
            printf 'Conda environment; system-managed: No\n'
        fi
        return
    fi

    if [[ -n "$BREW_PREFIX" && -n "$BREW_PYTHON" && "$base_prefix" == "$BREW_PREFIX"/* ]]; then
        printf 'Homebrew Python; system-managed: No\n'
        return
    fi

    if [[ -n "$PYENV_ROOT_PATH" && "$base_prefix" == "$PYENV_ROOT_PATH"/versions/* ]]; then
        printf 'pyenv Python; system-managed: No\n'
        return
    fi

    if [[ "$externally_managed" == "Yes (PEP 668)" ]]; then
        printf 'Origin not identified; system-managed: Yes (PEP 668)\n'
    else
        printf 'Origin not identified; system-managed: Unknown\n'
    fi
}

print_python_candidate() {
    local candidate="$1"
    local executable
    local version
    local origin
    local suitability

    if ! command -v "$candidate" >/dev/null 2>&1; then
        return
    fi
    executable=$(command -v "$candidate")
    if [[ -n "${SEEN_PYTHON_PATHS:-}" && "|$SEEN_PYTHON_PATHS|" == *"|$executable|"* ]]; then
        return
    fi
    SEEN_PYTHON_PATHS="${SEEN_PYTHON_PATHS:-}|$executable"
    version=$("$candidate" --version 2>&1 || printf 'version check failed')
    origin=$(describe_origin "$candidate")

    suitability="Python 3.10+ and venv tools unavailable"
    if check_python "$candidate" && check_venv_tools "$candidate"; then
        suitability="suitable for a local .venv"
        if [[ -z "$FIRST_SUITABLE_PYTHON" ]]; then
            FIRST_SUITABLE_PYTHON="$candidate"
        fi
    elif check_python "$candidate"; then
        suitability="Python 3.10+, but venv/ensurepip unavailable"
    elif "$candidate" -c 'import sys; raise SystemExit(0 if sys.version_info.major == 3 else 1)' \
        >/dev/null 2>&1; then
        suitability="Python is older than 3.10"
    else
        suitability="not a usable Python 3 interpreter"
    fi
    printf '  [INFO] %-12s %-22s %s\n' "$candidate" "$version" "$executable"
    printf '         %s; %s\n' "$origin" "$suitability"
}

if [[ "$(uname -s 2>/dev/null || true)" == "Darwin" ]]; then
    PYTHON_SYSTEM="macOS"
elif [[ "$(uname -s 2>/dev/null || true)" == "Linux" ]]; then
    if [[ -r /proc/version ]] && grep -qi microsoft /proc/version; then
        PYTHON_SYSTEM="Linux / WSL"
    else
        PYTHON_SYSTEM="Linux"
    fi
else
    PYTHON_SYSTEM="$(uname -s 2>/dev/null || printf 'Unknown')"
fi

printf '==================================================\n'
printf 'CaMa-Flood Python Setup\n'
printf '==================================================\n\n'
printf 'System: %s\n\n' "$PYTHON_SYSTEM"
if [[ "$PYTHON_SYSTEM" == MINGW* || "$PYTHON_SYSTEM" == MSYS* || "$PYTHON_SYSTEM" == CYGWIN* ]]; then
    printf '[ERROR] Use WSL2 with Ubuntu for these Python setup scripts:\n'
    printf '  https://learn.microsoft.com/windows/wsl/install\n'
    exit 1
fi
printf 'Available Python interpreters:\n'
SEEN_PYTHON_PATHS=""
FIRST_SUITABLE_PYTHON=""
BREW_PREFIX=""
BREW_PYTHON=""
PYENV_ROOT_PATH=""
if command -v brew >/dev/null 2>&1; then
    BREW_PREFIX=$(brew --prefix 2>/dev/null || true)
    BREW_PYTHON=$(brew list --versions 2>/dev/null | grep -E '^python(@[^[:space:]]+)?[[:space:]]' || true)
fi
if command -v pyenv >/dev/null 2>&1; then
    PYENV_ROOT_PATH=$(pyenv root 2>/dev/null || true)
fi
for candidate in python3 python3.14 python3.13 python3.12 python3.11 python3.10 python; do
    print_python_candidate "$candidate"
done
if [[ -n "${PYTHON:-}" ]]; then
    print_python_candidate "$PYTHON"
fi

VENV_PYTHON=""
VENV_STATE="not present"
if [[ -L "$VENV_DIR" ]]; then
    VENV_STATE="invalid: .venv is a symbolic link"
elif [[ -d "$VENV_DIR/conda-meta" ]]; then
    VENV_STATE="invalid: .venv is a Conda environment"
elif [[ -e "$VENV_DIR" ]]; then
    VENV_PYTHON="$VENV_DIR/bin/python"
    if [[ ! -x "$VENV_PYTHON" ]]; then
        VENV_STATE="invalid: .venv/bin/python is unavailable"
    elif ! "$VENV_PYTHON" -c 'import sys; raise SystemExit(0 if sys.prefix != sys.base_prefix else 1)' \
        >/dev/null 2>&1; then
        VENV_STATE="invalid: .venv is not isolated"
    elif ! check_python "$VENV_PYTHON"; then
        VENV_STATE="invalid: .venv Python is older than 3.10"
    elif ! check_venv_tools "$VENV_PYTHON"; then
        VENV_STATE="invalid: venv or ensurepip tools are unavailable"
    elif ! "$VENV_PYTHON" -m pip --version >/dev/null 2>&1; then
        VENV_STATE="invalid: pip is unavailable in .venv"
    else
        VENV_STATE="valid and reusable"
    fi
fi
printf '\nRepository environment:\n'
if [[ "$VENV_STATE" == "valid and reusable" ]]; then
    printf '  [OK] %s (%s)\n' "$VENV_DIR" "$("$VENV_PYTHON" --version 2>&1)"
else
    printf '  [--] %s\n' "$VENV_STATE"
fi

if [[ ! -f "$REQUIREMENTS" ]]; then
    fail "requirements.txt was not found beside this script."
fi

if [[ "$VENV_STATE" == invalid:* ]]; then
    fail "$VENV_STATE. Move the existing environment aside manually before retrying."
fi

if [[ "$VENV_STATE" != "valid and reusable" ]]; then
    if [[ -n "${PYTHON:-}" ]]; then
        if ! command -v "$PYTHON" >/dev/null 2>&1; then
            printf '\n[--] PYTHON override was not found: %s\n' "$PYTHON"
            SELECTED_PYTHON=""
        elif ! check_python "$PYTHON" || ! check_venv_tools "$PYTHON"; then
            printf '\n[--] PYTHON override needs Python 3.10+, venv, and ensurepip: %s\n' "$PYTHON"
            SELECTED_PYTHON=""
        else
            SELECTED_PYTHON="$PYTHON"
        fi
    else
        SELECTED_PYTHON="$FIRST_SUITABLE_PYTHON"
    fi
else
    SELECTED_PYTHON=""
fi

if [[ "$VENV_STATE" != "valid and reusable" && -z "$SELECTED_PYTHON" ]]; then
    printf '\n[ERROR] No suitable Python 3.10+ interpreter with venv and ensurepip was found.\n' >&2
    printf 'Install a user-managed Python environment such as Miniforge, then rerun this script.\n' >&2
    printf 'Official instructions: https://github.com/conda-forge/miniforge\n' >&2
    exit 1
fi

if [[ "$VENV_STATE" == "valid and reusable" ]]; then
    printf '\nSelected action: reuse the existing isolated .venv and install/update requirements.\n'
else
    printf '\nSelected interpreter for .venv: %s (%s)\n' \
        "$SELECTED_PYTHON" "$(command -v "$SELECTED_PYTHON")"
    printf 'Packages will be installed only inside .venv, never into the system interpreter.\n'
fi

if [[ ! -t 0 || ! -t 1 ]]; then
    printf '\nNo interactive terminal detected; diagnosis only. Rerun in a terminal to create/update .venv.\n'
    exit 0
fi

printf '\nCreate/update .venv and install requirements? [Y/n]: '
IFS= read -r answer || answer=""
case "$answer" in
    ""|Y|y|YES|yes|Yes) ;;
    *) printf 'Python environment was not changed.\n'; exit 0 ;;
esac

if [[ "$VENV_STATE" != "valid and reusable" ]]; then
    printf '\n[STEP 1/2] Create the repository-local virtual environment.\n'
    if ! "$SELECTED_PYTHON" -m venv "$VENV_DIR"; then
        fail "could not create .venv with $SELECTED_PYTHON. Confirm its venv/ensurepip support."
    fi
    VENV_PYTHON="$VENV_DIR/bin/python"
else
    printf '\n[STEP 1/2] Reuse the validated repository-local virtual environment.\n'
fi

if ! "$VENV_PYTHON" -m pip --version >/dev/null 2>&1; then
    fail "pip is unavailable in .venv; recreate it with a Python that includes ensurepip."
fi

printf '\n[STEP 2/2] Install requirements into .venv.\n'
if ! "$VENV_PYTHON" -m pip install -r "$REQUIREMENTS"; then
    fail "dependency installation failed. Check network access and platform package prerequisites, then rerun."
fi

if ! "$VENV_PYTHON" -c 'import pandas, xarray, netCDF4' >/dev/null 2>&1; then
    fail "required imports pandas, xarray, and netCDF4 are unavailable after installation."
fi

printf '\n[SUCCESS] Python environment ready.\n\n'
printf 'Activate the environment:\n\n  source .venv/bin/activate\n'
