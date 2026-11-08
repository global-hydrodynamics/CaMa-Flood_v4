#!/bin/sh
# Diagnose v5 build requirements and optionally write a recommended Mkinclude.
set -eu

SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
TMP_ROOT=${TMPDIR:-/tmp}
TMP_DIR=$(mktemp -d "$TMP_ROOT/cmf-fortran-setup.XXXXXX") || {
    printf '[ERROR] Could not create a temporary directory for compiler checks.\n' >&2
    exit 1
}
STAGED_CONFIG=""

cleanup() {
    if [ -n "$STAGED_CONFIG" ]; then
        rm -f "$STAGED_CONFIG"
    fi
    rm -rf "$TMP_DIR"
}

trap cleanup 0
trap 'exit 129' HUP
trap 'exit 130' INT
trap 'exit 143' TERM

OS_NAME=$(uname -s 2>/dev/null || printf 'Unknown')
CPU_ARCH=$(uname -m 2>/dev/null || printf 'Unknown')
SYSTEM_NAME=$OS_NAME
NATIVE_WINDOWS=no
case "$OS_NAME" in
    Darwin)
        SYSTEM_NAME="macOS"
        case "$CPU_ARCH" in
            arm64|aarch64) SYSTEM_NAME="macOS / Apple Silicon" ;;
            x86_64) SYSTEM_NAME="macOS / Intel" ;;
        esac
        ;;
    Linux)
        if [ -r /proc/version ] && grep -qi microsoft /proc/version; then
            SYSTEM_NAME="Linux / WSL"
        else
            SYSTEM_NAME="Linux"
        fi
        ;;
    MINGW*|MSYS*|CYGWIN*)
        SYSTEM_NAME="Windows shell ($OS_NAME)"
        NATIVE_WINDOWS=yes
        ;;
esac

PACKAGE_MANAGER="Not detected"
if command -v brew >/dev/null 2>&1; then
    PACKAGE_MANAGER="Homebrew"
elif command -v apt-get >/dev/null 2>&1; then
    PACKAGE_MANAGER="APT"
elif command -v dnf >/dev/null 2>&1; then
    PACKAGE_MANAGER="DNF"
elif command -v yum >/dev/null 2>&1; then
    PACKAGE_MANAGER="YUM"
fi

MAKE_READY=no
MAKE_INFO="not found"
if command -v make >/dev/null 2>&1; then
    MAKE_INFO=$(make --version 2>/dev/null | sed -n '1p' || printf 'version unavailable')
    case "$MAKE_INFO" in
        *"GNU Make"*) MAKE_READY=yes ;;
        *) MAKE_INFO="not GNU Make ($(command -v make))" ;;
    esac
fi

GFORTRAN_READY=no
GFORTRAN_INFO="not found"
GFORTRAN_PATH=""
if command -v gfortran >/dev/null 2>&1; then
    GFORTRAN_PATH=$(command -v gfortran)
    GFORTRAN_INFO=$(gfortran --version 2>/dev/null | sed -n '1p' || printf '%s' "$GFORTRAN_PATH")
    cat > "$TMP_DIR/basic.f90" <<'EOF'
program compiler_check
  print *, "Fortran compiler check"
end program compiler_check
EOF
    if gfortran "$TMP_DIR/basic.f90" -o "$TMP_DIR/basic" \
        >"$TMP_DIR/basic.log" 2>&1; then
        GFORTRAN_READY=yes
    else
        GFORTRAN_INFO="$GFORTRAN_INFO (compile/link test failed)"
    fi
fi

OPENMP_READY=no
OPENMP_INFO="not available"
if [ "$GFORTRAN_READY" = yes ]; then
    cat > "$TMP_DIR/openmp.f90" <<'EOF'
program openmp_check
  use omp_lib
  print *, omp_get_max_threads()
end program openmp_check
EOF
    if gfortran -fopenmp "$TMP_DIR/openmp.f90" -o "$TMP_DIR/openmp" \
        >"$TMP_DIR/openmp.log" 2>&1 && "$TMP_DIR/openmp" >/dev/null 2>&1; then
        OPENMP_READY=yes
        OPENMP_INFO="-fopenmp compile, link, and run passed"
    else
        OPENMP_INFO="-fopenmp compile/link test failed"
    fi
fi

NETCDF_READY=no
NETCDF_INFO="nf-config not found"
NETCDF_FFLAGS=""
NETCDF_FLIBS=""
NETCDF_CLIBS=""
if [ "$GFORTRAN_READY" = yes ] && command -v nf-config >/dev/null 2>&1; then
    NETCDF_INFO=$(nf-config --version 2>/dev/null || printf 'version unavailable')
    if NETCDF_FFLAGS=$(nf-config --fflags 2>/dev/null) && \
       NETCDF_FLIBS=$(nf-config --flibs 2>/dev/null); then
        if command -v nc-config >/dev/null 2>&1; then
            NETCDF_CLIBS=$(nc-config --libs 2>/dev/null | sed 's/-lnetcdf//g')
        fi
        cat > "$TMP_DIR/netcdf.f90" <<'EOF'
program netcdf_check
  use netcdf
  implicit none
  integer :: ncid, dimid, varid, status
  real(kind=8) :: input_value(1), output_value(1)

  input_value(1) = 123.25_8
  status = nf90_create("netcdf_check.nc", NF90_CLOBBER, ncid)
  if (status /= NF90_NOERR) then
    print *, nf90_strerror(status)
    stop 1
  endif
  status = nf90_def_dim(ncid, "sample", 1, dimid)
  if (status /= NF90_NOERR) then
    print *, nf90_strerror(status)
    stop 2
  endif
  status = nf90_def_var(ncid, "value", NF90_DOUBLE, (/dimid/), varid)
  if (status /= NF90_NOERR) then
    print *, nf90_strerror(status)
    stop 3
  endif
  status = nf90_enddef(ncid)
  if (status /= NF90_NOERR) then
    print *, nf90_strerror(status)
    stop 4
  endif
  status = nf90_put_var(ncid, varid, input_value)
  if (status /= NF90_NOERR) then
    print *, nf90_strerror(status)
    stop 5
  endif
  status = nf90_get_var(ncid, varid, output_value)
  if (status /= NF90_NOERR) then
    print *, nf90_strerror(status)
    stop 6
  endif
  status = nf90_close(ncid)
  if (status /= NF90_NOERR) then
    print *, nf90_strerror(status)
    stop 7
  endif
  if (output_value(1) /= input_value(1)) stop 8
end program netcdf_check
EOF
        # nf-config returns compiler flags as a word-separated command-line string.
        if (cd "$TMP_DIR" && gfortran $NETCDF_FFLAGS netcdf.f90 \
            $NETCDF_FLIBS $NETCDF_CLIBS -o netcdf_check >netcdf.log 2>&1 && \
            ./netcdf_check >>netcdf.log 2>&1); then
            NETCDF_READY=yes
        else
            NETCDF_INFO="$NETCDF_INFO (NF90_DOUBLE read/write test failed)"
        fi
    else
        NETCDF_INFO="$NETCDF_INFO (nf-config flags unavailable)"
    fi
fi

MPI_READY=no
MPI_INFO="MPI Fortran wrapper not found"
MPI_WRAPPER=""
if [ "$GFORTRAN_READY" = yes ]; then
    cat > "$TMP_DIR/mpi.f90" <<'EOF'
program mpi_check
  use mpi
  implicit none
  integer :: ierr
  call MPI_Init(ierr)
  call MPI_Finalize(ierr)
end program mpi_check
EOF
    for candidate in mpifort mpif90 mpiifx mpiifort; do
        if command -v "$candidate" >/dev/null 2>&1; then
            if "$candidate" "$TMP_DIR/mpi.f90" -o "$TMP_DIR/mpi_check" \
                >"$TMP_DIR/mpi.log" 2>&1; then
                MPI_READY=yes
                MPI_WRAPPER=$(command -v "$candidate")
                MPI_INFO="$candidate compile/link test passed"
                break
            fi
            MPI_INFO="$candidate found but MPI compile/link test failed"
        fi
    done
fi

PYTHON_READY=no
PYTHON_INFO="Python 3 not found"
if command -v python3 >/dev/null 2>&1; then
    PYTHON_INFO=$(python3 --version 2>&1)
    if python3 -c 'import sys; raise SystemExit(0 if sys.version_info >= (3, 10) else 1)' \
        >/dev/null 2>&1; then
        PYTHON_READY=yes
    else
        PYTHON_INFO="$PYTHON_INFO (requires Python 3.10+)"
    fi
fi

print_component() {
    label=$1
    ready=$2
    detail=$3
    if [ "$ready" = yes ]; then
        printf '  [OK] %-18s %s\n' "$label" "$detail"
    else
        printf '  [--] %-18s %s\n' "$label" "$detail"
    fi
}

printf '==================================================\n'
printf 'CaMa-Flood Fortran Setup\n'
printf '==================================================\n\n'
printf 'System: %s (%s)\n' "$SYSTEM_NAME" "$CPU_ARCH"
printf 'Package manager: %s\n\n' "$PACKAGE_MANAGER"
printf 'Installed components:\n'
print_component "GNU Make" "$MAKE_READY" "$MAKE_INFO"
print_component "GNU Fortran" "$GFORTRAN_READY" "$GFORTRAN_INFO"
print_component "OpenMP" "$OPENMP_READY" "$OPENMP_INFO"
print_component "NetCDF-Fortran" "$NETCDF_READY" "$NETCDF_INFO"
print_component "MPI Fortran" "$MPI_READY" "$MPI_INFO"
if [ -n "$GFORTRAN_PATH" ]; then
    printf '    gfortran path: %s\n' "$GFORTRAN_PATH"
fi
if [ -n "$MPI_WRAPPER" ]; then
    printf '    MPI wrapper:  %s\n' "$MPI_WRAPPER"
fi
if [ "$PYTHON_READY" = yes ]; then
    printf '    Python:       %s (%s)\n' "$PYTHON_INFO" "$(command -v python3)"
fi
if [ "$GFORTRAN_READY" != yes ] && [ -s "$TMP_DIR/basic.log" ]; then
    printf '    Compiler check detail: %s\n' "$(sed -n '1p' "$TMP_DIR/basic.log")"
fi
if [ "$OPENMP_READY" != yes ] && [ -s "$TMP_DIR/openmp.log" ]; then
    printf '    OpenMP check detail: %s\n' "$(sed -n '1p' "$TMP_DIR/openmp.log")"
fi
if [ "$NETCDF_READY" != yes ] && [ -s "$TMP_DIR/netcdf.log" ]; then
    printf '    NetCDF check detail: %s\n' "$(sed -n '1p' "$TMP_DIR/netcdf.log")"
fi
if [ "$MPI_READY" != yes ] && [ -s "$TMP_DIR/mpi.log" ]; then
    printf '    MPI check detail: %s\n' "$(sed -n '1p' "$TMP_DIR/mpi.log")"
fi

printf '\nSample build readiness (not a claim that simulations have run):\n'
if [ "$GFORTRAN_READY" = yes ] && [ "$MAKE_READY" = yes ] && \
   [ "$PYTHON_READY" = yes ]; then
    printf '  [READY] mek01 - Compiler, GNU Make, and CTL-tool Python requirements available.\n'
else
    printf '  [--]    mek01 - Requires GNU Fortran, GNU Make, and Python 3.10+ for CTL tools.\n'
fi
if [ "$GFORTRAN_READY" = yes ] && [ "$MAKE_READY" = yes ] && \
   [ "$NETCDF_READY" = yes ]; then
    printf '  [READY] mek02 - NetCDF simulation build requirements available.\n'
else
    printf '  [--]    mek02 - Requires working GNU Fortran, GNU Make, and NetCDF-Fortran.\n'
fi
if [ "$GFORTRAN_READY" = yes ] && [ "$MAKE_READY" = yes ] && \
   [ "$MPI_READY" = yes ]; then
    printf '  [READY] MPI tests - MPI Fortran compile/link requirements available.\n'
else
    printf '  [--]    MPI tests - Requires a working MPI Fortran wrapper.\n'
fi
printf '  These labels describe compiler/library readiness only.\n'

MEK01_DATA=yes
for file in \
    map/mekong_15min/params.txt map/mekong_15min/nextxy_noedge.bin \
    map/mekong_15min/ctmare.bin map/mekong_15min/elevtn.bin \
    map/mekong_15min/nxtdst.bin map/mekong_15min/rivlen.bin \
    map/mekong_15min/fldhgt.bin map/mekong_15min/rivwth_merit.bin \
    map/mekong_15min/rivhgt.bin map/mekong_15min/rivman.bin \
    map/mekong_15min/inpdim_test-mek_1deg.txt \
    map/mekong_15min/inpmat_test-mek_1deg.bin \
    inp/test-mek_1deg/runoff/Roff____20000101.one \
    inp/test-mek_1deg/runoff/Roff____20000209.one; do
    if [ ! -f "$SCRIPT_DIR/$file" ]; then
        MEK01_DATA=no
    fi
done
if [ -f "$SCRIPT_DIR/map/mekong_15min/bifprm.csv" ]; then
    :
elif [ -f "$SCRIPT_DIR/map/mekong_15min/bifprm.txt" ]; then
    :
else
    MEK01_DATA=no
fi

MEK02_DATA=yes
for file in \
    map/mekong_15min/params.txt map/mekong_15min/inpdim_test-mek_1deg.txt \
    map/mekong_15min/inpmat_test-mek_1deg.bin \
    map/mekong_15min/netcdf/nextxy_noedge.nc \
    map/mekong_15min/netcdf/ctmare.nc map/mekong_15min/netcdf/elevtn.nc \
    map/mekong_15min/netcdf/nxtdst.nc map/mekong_15min/netcdf/rivlen.nc \
    map/mekong_15min/netcdf/fldhgt.nc map/mekong_15min/netcdf/rivwth_merit.nc \
    map/mekong_15min/netcdf/rivhgt.nc map/mekong_15min/netcdf/rivman.nc \
    map/mekong_15min/netcdf/bifprm.nc \
    inp/test-mek_1deg_nc/runoff2000.nc inp/test-mek_1deg_nc/runoff2001.nc; do
    if [ ! -f "$SCRIPT_DIR/$file" ]; then
        MEK02_DATA=no
    fi
done
printf '\nMekong sample inputs:\n'
if [ "$MEK01_DATA" = yes ]; then
    printf '  [OK] mek01 Binary inputs found.\n'
else
    printf '  [--] mek01 inputs are incomplete in this checkout.\n'
fi
if [ "$MEK02_DATA" = yes ]; then
    printf '  [OK] mek02 NetCDF and Binary inputs found.\n'
else
    printf '  [--] mek02 inputs are incomplete in this checkout.\n'
fi

recommendations_needed=no
if [ "$MAKE_READY" != yes ] || [ "$GFORTRAN_READY" != yes ]; then
    recommendations_needed=yes
fi
if [ "$NETCDF_READY" != yes ] || [ "$OPENMP_READY" != yes ]; then
    recommendations_needed=yes
fi
if [ "$recommendations_needed" = yes ]; then
    printf '\nRecommended installation guidance (nothing will be installed automatically):\n'
    case "$OS_NAME:$PACKAGE_MANAGER" in
        Darwin:Homebrew)
            if [ "$GFORTRAN_READY" != yes ] || [ "$NETCDF_READY" != yes ] || \
               [ "$OPENMP_READY" != yes ]; then
                printf '  brew install gcc netcdf-fortran\n'
            fi
            if [ "$MAKE_READY" != yes ]; then
                printf '  brew install make\n'
            fi
            ;;
        Darwin:*)
            printf '  Install Homebrew from https://brew.sh, then run: brew install gcc netcdf-fortran\n'
            printf '  Install Apple command-line tools for make: xcode-select --install\n'
            ;;
        Linux:APT)
            if [ -n "${MODULEPATH:-}" ]; then
                printf '  HPC modules detected: check module avail and site-supported compiler/NetCDF modules first.\n'
            fi
            if [ "$NETCDF_READY" = yes ]; then
                printf '  sudo apt install gfortran make\n'
            else
                printf '  sudo apt install gfortran make libnetcdff-dev\n'
            fi
            ;;
        Linux:DNF|Linux:YUM)
            printf '  Install GNU Fortran, GNU Make, and NetCDF-Fortran development packages\n'
            printf '  from enabled repositories; package names vary by distribution.\n'
            printf '  On Rocky/RHEL or HPC, check site repositories and environment modules first.\n'
            ;;
        Linux:*)
            printf '  Install GNU Fortran, GNU Make, and (for mek02) NetCDF-Fortran.\n'
            if [ -n "${MODULEPATH:-}" ]; then
                printf '  HPC modules detected: try module avail and load the site-supported toolchain.\n'
            fi
            ;;
    esac
fi
if [ "$NATIVE_WINDOWS" = yes ]; then
    printf '\nWindows users: use WSL2 with Ubuntu for this build workflow:\n'
    printf '  https://learn.microsoft.com/windows/wsl/install\n'
fi
if [ "$MPI_READY" != yes ]; then
    case "$OS_NAME:$PACKAGE_MANAGER" in
        Darwin:Homebrew) printf 'If MPI is needed: brew install open-mpi\n' ;;
        Darwin:*) printf 'If MPI is needed: install Homebrew, then brew install open-mpi\n' ;;
        Linux:APT) printf 'If MPI is needed: sudo apt install openmpi-bin libopenmpi-dev\n' ;;
        *) printf 'If MPI is needed: use the MPI package or environment module supported by your system.\n' ;;
    esac
fi

if [ "$GFORTRAN_READY" != yes ] || [ "$MAKE_READY" != yes ]; then
    printf '\n[ERROR] Basic Fortran build requirements are not ready; no Mkinclude was generated.\n' >&2
    exit 1
fi
if [ "$NATIVE_WINDOWS" = yes ]; then
    printf '\n[ERROR] Native Windows shells are not a supported build environment; use WSL2.\n' >&2
    exit 1
fi

if [ "$NETCDF_READY" != yes ]; then
    printf '\n[ERROR] Working NetCDF-Fortran is required for the standard v5 configuration.\n' >&2
    printf 'Install NetCDF-Fortran, ensure nf-config is on PATH, and rerun this script.\n' >&2
    printf 'No configuration was generated; existing files were left unchanged.\n' >&2
    exit 1
fi

printf '\nRecommended configuration:\n'
OMP_CONFIG="OpenMP unavailable"
if [ "$OPENMP_READY" = yes ]; then
    OMP_CONFIG="OpenMP enabled"
fi
printf '  Compiler: GNU Fortran (%s)\n' "$OMP_CONFIG"
if [ "$NETCDF_READY" = yes ]; then
    printf '  NetCDF: Enabled (NF90_DOUBLE read/write test passed)\n'
else
    printf '  NetCDF: Required for the standard v5 configuration; check failed\n'
fi
printf '  MPI: Disabled by default\n'
printf '  Precision: SinglePrec_CMF, matching standard GNU templates\n'

if [ ! -t 0 ] || [ ! -t 1 ]; then
    printf '\nNo interactive terminal detected; diagnosis only. Run again in a terminal to generate ./Mkinclude.\n'
    exit 0
fi

printf '\nGenerate ./Mkinclude? [Y/n]: '
IFS= read -r answer || answer=""
case "$answer" in
    ""|Y|y|YES|yes|Yes) ;;
    *) printf 'No configuration file was written.\n'; exit 0 ;;
esac

TARGET_CONFIG="$SCRIPT_DIR/Mkinclude"
if [ -e "$TARGET_CONFIG" ]; then
    printf './Mkinclude already exists. Overwrite it? [y/N]: '
    IFS= read -r answer || answer=""
    case "$answer" in
        Y|y|YES|yes|Yes) ;;
        *) printf 'Existing ./Mkinclude was kept unchanged.\n'; exit 0 ;;
    esac
fi

STAGED_CONFIG=$(mktemp "$SCRIPT_DIR/.Mkinclude.XXXXXX") || {
    printf '[ERROR] Could not create a temporary configuration file.\n' >&2
    exit 1
}
write_recommended_config() {
    cat <<'EOF'
# Recommended CaMa-Flood v5 configuration for GNU Fortran on macOS/Linux.
# Review this machine-specific staging file before activating it in config/.
FC = gfortran
EOF
    if [ "$OPENMP_READY" = yes ]; then
        printf 'FCMP = gfortran -fopenmp\n'
    else
        printf 'FCMP = gfortran\n'
    fi
    cat <<'EOF'
FFLAGS = -O3 -Wall -cpp -ffree-line-length-none -fimplicit-none -ftree-vectorize
LFLAGS =

# NetCDF-Fortran flags come from the active installation.
NF_CONFIG ?= nf-config
NC_CONFIG ?= nc-config
NF_FFLAGS ?= $(shell $(NF_CONFIG) --fflags 2>/dev/null)
NF_FLIBS ?= $(shell $(NF_CONFIG) --flibs 2>/dev/null)
NC_LIBS ?= $(shell $(NC_CONFIG) --libs 2>/dev/null | sed 's/-lnetcdf//g')
INC ?= $(NF_FFLAGS)
LIB ?= $(NF_FLIBS) $(NC_LIBS)

# Preprocessing options
# DMPI=-DUseMPI_CMF        : activate MPI parallelization
# DCDF=-DUseCDF_CMF        : activate NetCDF input/output
# DSINGLE=-DSinglePrec_CMF : activate Single Precision Mode
# DHEATLINK=-Dheatlink     : activate river water temperature calculation
#DMPI = -DUseMPI_CMF
DCDF = -DUseCDF_CMF
DSINGLE = -DSinglePrec_CMF
#DHEATLINK=-Dheatlink
CFLAGS = $(DMPI) $(DCDF) $(DSINGLE) $(DHEATLINK)
EOF
    :
}

if write_recommended_config > "$STAGED_CONFIG"; then
    :
else
    printf '[ERROR] Could not write the recommended configuration.\n' >&2
    exit 1
fi
chmod 644 "$STAGED_CONFIG"
mv -f "$STAGED_CONFIG" "$TARGET_CONFIG" || {
    printf '[ERROR] Could not install ./Mkinclude.\n' >&2
    exit 1
}
STAGED_CONFIG=""

printf '\n[SUCCESS] Generated ./Mkinclude\n\n'
printf 'To activate this configuration:\n\n'
if [ -e "$SCRIPT_DIR/config/Mkinclude" ]; then
    printf 'Existing config/Mkinclude was not changed. Back it up before choosing to replace it.\n'
else
    printf '  cp Mkinclude config/Mkinclude\n'
fi
printf '\nLegacy v4 builds continue to use adm/Mkinclude.\n'
