#!/usr/bin/env bash
# Create or update the repository-local Python environment.
set -eu

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VENV_DIR="$SCRIPT_DIR/.venv"
REQUIREMENTS="$SCRIPT_DIR/requirements.txt"

fail() {
  printf 'Python setup error: %s\n' "$1" >&2
  exit 1
}

check_python() {
  "$1" -c 'import sys; raise SystemExit(0 if sys.version_info.major == 3 and sys.version_info >= (3, 10) else 1)' >/dev/null 2>&1
}

if [ "$(uname -s)" != "Darwin" ] && [ "$(uname -s)" != "Linux" ]; then
  fail "this script supports macOS and Linux only."
fi

if [ ! -f "$REQUIREMENTS" ]; then
  fail "requirements.txt was not found beside this script."
fi

if [ -L "$VENV_DIR" ]; then
  fail ".venv is a symbolic link; remove the link and rerun so packages cannot be installed into another environment."
elif [ -d "$VENV_DIR/conda-meta" ]; then
  fail ".venv points to a Conda environment; use a fresh repository-local virtual environment."
elif [ -d "$VENV_DIR" ]; then
  VENV_PYTHON="$VENV_DIR/bin/python"
  if [ ! -x "$VENV_PYTHON" ]; then
    fail "$VENV_DIR exists but is not a usable virtual environment; move it aside and rerun."
  fi
  if ! "$VENV_PYTHON" -c 'import sys; raise SystemExit(0 if sys.prefix != sys.base_prefix else 1)' >/dev/null 2>&1; then
    fail "$VENV_DIR is not an isolated virtual environment; move it aside and rerun."
  fi
  if ! check_python "$VENV_PYTHON"; then
    fail "the existing .venv does not use Python 3.10 or newer; move it aside and rerun with a suitable Python 3 interpreter."
  fi
else
  SELECTED_PYTHON=""
  if [ -n "${PYTHON:-}" ]; then
    if ! command -v "$PYTHON" >/dev/null 2>&1; then
      fail "the PYTHON executable '$PYTHON' was not found."
    fi
    if ! check_python "$PYTHON"; then
      fail "PYTHON must point to Python 3.10 or newer."
    fi
    SELECTED_PYTHON="$PYTHON"
  else
    for candidate in python3 python3.14 python3.13 python3.12 python3.11 python3.10; do
      if command -v "$candidate" >/dev/null 2>&1 && check_python "$candidate"; then
        SELECTED_PYTHON="$candidate"
        break
      fi
    done
  fi
  if [ -z "$SELECTED_PYTHON" ]; then
    fail "Python 3.10 or newer was not found. Install it or set PYTHON to its executable."
  fi

  printf 'Creating isolated environment with %s...\n' "$SELECTED_PYTHON"
  if ! "$SELECTED_PYTHON" -m venv "$VENV_DIR"; then
    fail "could not create .venv with $SELECTED_PYTHON. Confirm that its venv support is installed."
  fi
  VENV_PYTHON="$VENV_DIR/bin/python"
fi

if ! "$VENV_PYTHON" -m pip --version >/dev/null 2>&1; then
  fail "pip is unavailable in .venv; recreate the environment with a Python installation that includes ensurepip."
fi

printf 'Installing repository Python requirements into .venv...\n'
if ! "$VENV_PYTHON" -m pip install -r "$REQUIREMENTS"; then
  fail "dependency installation failed. Check network access and platform package prerequisites, then rerun."
fi

printf '\nPython environment is ready. Activate it with:\n  source .venv/bin/activate\n'
