#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Compute CaMa-Flood levee-related topographic and storage parameters.

Inputs:
  --params   : params.txt  (nx, ny, NLFP, etc.)
  --grdare   : GRDARE [m^2]
  --rivlen   : RIVLEN [m]
  --rivwth   : RIVWTH [m]
  --rivhgt   : RIVHGT [m] (sea cells are -9999)
  --fldhgt   : FLDHGT(i) [m], i=1..NLFP
  --levfrc   : LEVFRC [-]  (fractional levee position 0–1 from river to floodplain edge)
  --levhgt   : LEVHGT [m]  (levee crown height above river bed)

Outputs:
  --out-levdst      : LEVDST [m]     (horizontal distance from river to levee)
  --out-levbashgt   : LEVBASHGT [m]  (ground elevation at levee base; height above river bed)
  --out-levphyhgt   : LEVPHYHGT [m]  (physical levee height = LEVHGT - LEVBASHGT)
  --out-levhgt-mod  : LEVHGT_MOD [m] (LEVHGT with non-physical levees set to -9999)
  --out-levbassto   : LEVBASSTO [m3] (storage at levee base, river side only)
  --out-levtopsto   : LEVTOPSTO [m3] (storage at levee crown, river side only)
  --out-levfilsto   : LEVFILSTO [m3] (storage at levee crown, both river and protected side)

Notes:
  - We assume RIVSTOMAX = 0 for all cells (in CaMa-Flood this can be non-zero).
  - Cells with:
      * no levee (LEVHGT <= 0 or LEVPHYHGT <= 0), or
      * sea (RIVHGT == -9999)
    are set to -9999 in all outputs.
"""

import numpy as np
from pathlib import Path
import argparse


# ----------------- Basic I/O helpers ----------------- #

def read_params(param_path: Path):
    """Read nx, ny, NLFP from params.txt."""
    with param_path.open("r") as f:
        lines = [ln.strip() for ln in f if ln.strip()]

    nx = int(lines[0].split()[0])
    ny = int(lines[1].split()[0])
    nlfp = int(lines[2].split()[0])

    return nx, ny, nlfp


def read_1d_field(bin_path: Path, ncell: int, dtype=np.float32):
    """Read a GrADS binary field (nx*ny) as a 1D vector."""
    data = np.fromfile(bin_path, dtype=dtype)
    if data.size != ncell:
        raise ValueError(
            f"Size mismatch for {bin_path}: expected {ncell}, got {data.size}"
        )
    return data


def read_fldhgt(bin_path: Path, ncell: int, nlfp: int, dtype=np.float32):
    """
    Read fldhgt.bin and return array with shape (ncell, nlfp).

    Assumed layout in file: layer by layer
      [layer1(nx*ny), layer2(nx*ny), ..., layerNLFP(nx*ny)]
    """
    data = np.fromfile(bin_path, dtype=dtype)
    if data.size != ncell * nlfp:
        raise ValueError(
            f"Size mismatch for {bin_path}: expected {ncell * nlfp}, got {data.size}"
        )

    data = data.reshape((nlfp, ncell))  # (nlfp, ncell)
    data = data.transpose(1, 0)         # (ncell, nlfp)

    return data


# ----------------- Core computation ----------------- #

def compute_levee_params(
    grdare: np.ndarray,    # [ncell]  grid area [m^2]
    rivlen: np.ndarray,    # [ncell]  river length [m]
    rivwth: np.ndarray,    # [ncell]  river width [m]
    rivstomax: np.ndarray, # [ncell]  river storage at reference stage [m3] (assumed 0 here)
    fldhgt: np.ndarray,    # [ncell, NLFP] floodplain profile node heights [m]
    levfrc: np.ndarray,    # [ncell]  fractional levee position (0–1)
    levhgt: np.ndarray,    # [ncell]  levee crown height [m]
    large_value: float = 1.0e18,
):
    """
    Compute levee parameters following the logic of CaMa-Flood CMF_LEVEE_INIT and SET_FLDSTG:

      - LEVBASHGT : ground elevation at levee base
      - LEVDST    : distance from river to levee
      - LEVBASSTO : storage at levee base (river side only)
      - LEVTOPSTO : storage at levee crown (river side only)
      - LEVFILSTO : storage at levee crown (river + protected side)

    Cells with LEVHGT <= 0 are treated as "no levee" (set to large_value).
    """

    grdare = np.asarray(grdare, dtype=float)
    rivlen = np.asarray(rivlen, dtype=float)
    rivwth = np.asarray(rivwth, dtype=float)
    rivstomax = np.asarray(rivstomax, dtype=float)
    fldhgt = np.asarray(fldhgt, dtype=float)
    levfrc = np.asarray(levfrc, dtype=float)
    levhgt = np.asarray(levhgt, dtype=float)

    ncell, nlfp = fldhgt.shape

    # Outputs
    levbashgt = np.full(ncell, large_value, dtype=float)
    levdst    = np.full(ncell, large_value, dtype=float)
    levbassto = np.full(ncell, large_value, dtype=float)
    levtopsto = np.full(ncell, large_value, dtype=float)
    levfilsto = np.full(ncell, large_value, dtype=float)

    # Only cells with LEVHGT > 0 are candidates for levees
    has_levee = levhgt > 0.0

    # Clip LEVFRC to [0, 1]
    levfrc = np.clip(levfrc, 0.0, 1.0)

    # Floodplain width increment: DWTHINC = GRDARE / RIVLEN / NLFP
    with np.errstate(divide="ignore", invalid="ignore"):
        dwth_inc = np.where(rivlen > 0.0, grdare / rivlen / float(nlfp), 0.0)

    # Compute FLDSTOMAX and FLDGRD (equivalent to SET_FLDSTG)
    fldstomax = np.zeros((ncell, nlfp), dtype=float)
    fldgrd    = np.zeros((ncell, nlfp), dtype=float)

    dstopre = rivstomax.copy()  # initial storage = RIVSTOMAX
    dhgtpre = np.zeros(ncell, dtype=float)

    for i in range(nlfp):
        # Height difference across this layer
        if i == 0:
            dh = fldhgt[:, 0]  # FLDHGT(1) - 0
        else:
            dh = fldhgt[:, i] - fldhgt[:, i - 1]

        # Storage increment: river + floodplain
        dstonow = rivlen * (rivwth + dwth_inc * (i + 0.5)) * dh

        fldstomax[:, i] = dstopre + dstonow

        # Gradient: FLDGRD = (height difference) / DWTHINC
        with np.errstate(divide="ignore", invalid="ignore"):
            fldgrd[:, i] = np.where(dwth_inc > 0.0, dh / dwth_inc, 0.0)

        # Update for next layer
        dstopre = fldstomax[:, i]
        dhgtpre = fldhgt[:, i]

    # Per-cell levee parameter computation
    idx = np.where(has_levee)[0]

    for k in idx:
        length = rivlen[k]
        width  = rivwth[k]
        h_lev  = levhgt[k]
        frac   = levfrc[k]
        dh_profile    = fldhgt[k, :]   # length nlfp
        slope_profile = fldgrd[k, :]
        dw_inc        = dwth_inc[k]
        sto_profile   = fldstomax[k, :]

        if length <= 0.0 or dw_inc <= 0.0:
            # invalid cell; leave as large_value
            continue

        # LEVDST = LEVFRC * DWTHINC * NLFP = LEVFRC * GRDARE / RIVLEN
        levdst_k = frac * dw_inc * nlfp
        levdst[k] = levdst_k

        # ILEV = INT(frac * NLFP) + 1 (1..NLFP+1)
        ilev = int(np.floor(frac * nlfp)) + 1

        # ===== [1] storage at levee base (river side only) and levee top (river side only) =====
        dsto_fil = rivstomax[k]  # initial storage
        dhgtpre  = 0.0
        dwth_fil = 0.0

        if ilev >= 2:
            # Up to layer ILEV-1
            dsto_fil = sto_profile[ilev - 2]   # FLDSTOMAX(ILEV-1)
            dhgtpre  = dh_profile[ilev - 2]    # FLDHGT(ILEV-1)
            dwth_fil = dw_inc * (ilev - 1)

        if ilev <= nlfp:
            # Levee lies within floodplain layer ILEV
            dwth_add = levdst_k - dwth_fil
            dhgtnow  = dwth_add * slope_profile[ilev - 1]  # height above lower node
            levbashgt_k = dhgtnow + dhgtpre                # levee base height

            # Enforce LEVHGT >= LEVBASHGT for storage calculations (as in Fortran)
            h_lev_use = max(h_lev, levbashgt_k)

            dsto_add = (dwth_add * 0.5 + dwth_fil + width) * dhgtnow * length
            levbassto_k = dsto_fil + dsto_add

            dhgtdif = h_lev_use - levbashgt_k
            levtopsto_k = levbassto_k + (levdst_k + width) * dhgtdif * length

        else:
            # Levee lies at floodplain edge (ILEV = NLFP+1)
            levbashgt_k = dhgtpre            # height at outer floodplain node
            h_lev_use   = max(h_lev, levbashgt_k)

            levbassto_k = dsto_fil

            dhgtdif = h_lev_use - levbashgt_k
            levtopsto_k = levbassto_k + (levdst_k + width) * dhgtdif * length

        levbashgt[k] = levbashgt_k
        levbassto[k] = levbassto_k
        levtopsto[k] = levtopsto_k

        # ===== [2] storage at levee top including protected side (LEVFILSTO) =====
        i_layer = 1
        dsto_fil = rivstomax[k]
        dwth_fil = width      # initial width = river width
        dhgtpre  = 0.0

        # Find which layer the levee top belongs to
        while i_layer <= nlfp and h_lev_use > dh_profile[i_layer - 1]:
            dsto_fil = sto_profile[i_layer - 1]   # FLDSTOMAX(i_layer)
            dwth_fil = dwth_fil + dw_inc
            dhgtpre  = dh_profile[i_layer - 1]    # FLDHGT(i_layer)
            i_layer += 1
            if i_layer > nlfp:
                break

        if i_layer <= nlfp:
            # Levee top is inside layer i_layer
            dhgtnow = h_lev_use - dhgtpre
            slope_here = slope_profile[i_layer - 1]
            if slope_here > 0.0:
                dwth_add = dhgtnow / slope_here
            else:
                dwth_add = 0.0

            dsto_add = (dwth_add * 0.5 + dwth_fil) * dhgtnow * length
            levfilsto_k = dsto_fil + dsto_add
        else:
            # Levee top is higher than the highest floodplain node
            dhgtnow = h_lev_use - dhgtpre
            dsto_add = dwth_fil * dhgtnow * length
            levfilsto_k = dsto_fil + dsto_add

        levfilsto[k] = levfilsto_k

    return levbashgt, levdst, levbassto, levtopsto, levfilsto


# ----------------- Main ----------------- #

def main():
    parser = argparse.ArgumentParser(
        description="Compute CaMa-Flood levee topography and storage parameters."
    )

    # Input files
    parser.add_argument("--params", type=str, default="map/params.txt",
                        help="Path to params.txt (contains nx, ny, NLFP).")
    parser.add_argument("--grdare", type=str, default="map/grdare.bin",
                        help="GrADS binary for GRDARE (grid area).")
    parser.add_argument("--rivlen", type=str, default="map/rivlen.bin",
                        help="GrADS binary for RIVLEN (river length).")
    parser.add_argument("--rivwth", type=str, default="map/rivwth.bin",
                        help="GrADS binary for RIVWTH (river width).")
    parser.add_argument("--rivhgt", type=str, default="map/rivhgt.bin",
                        help="GrADS binary for RIVHGT (river bed elevation, sea=-9999).")
    parser.add_argument("--fldhgt", type=str, default="map/fldhgt.bin",
                        help="GrADS binary for FLDHGT (floodplain node heights, layered).")
    parser.add_argument("--levfrc", type=str, default="map/levfrc.bin",
                        help="GrADS binary for LEVFRC (fractional levee position).")
    parser.add_argument("--levhgt", type=str, default="map/levhgt.bin",
                        help="GrADS binary for LEVHGT (levee crown height).")

    # Output files
    parser.add_argument("--out-levdst",      type=str, default="map/levdst.bin",
                        help="Output GrADS binary for LEVDST.")
    parser.add_argument("--out-levbashgt",   type=str, default="map/levbashgt.bin",
                        help="Output GrADS binary for LEVBASHGT.")
    parser.add_argument("--out-levphyhgt",   type=str, default="map/levphyhgt.bin",
                        help="Output GrADS binary for LEVPHYHGT.")
    parser.add_argument("--out-levhgt-mod",  type=str, default="map/levhgt_mod.bin",
                        help="Output GrADS binary for LEVHGT_MOD.")
    parser.add_argument("--out-levbassto",   type=str, default="map/levbassto.bin",
                        help="Output GrADS binary for LEVBASSTO.")
    parser.add_argument("--out-levtopsto",   type=str, default="map/levtopsto.bin",
                        help="Output GrADS binary for LEVTOPSTO.")
    parser.add_argument("--out-levfilsto",   type=str, default="map/levfilsto.bin",
                        help="Output GrADS binary for LEVFILSTO.")

    parser.add_argument(
        "--float-endian",
        type=str,
        default="native",
        choices=["native", "little", "big"],
        help="Endianness for GrADS float32 binary.",
    )

    args = parser.parse_args()

    # dtype with endianness
    if args.float_endian == "native":
        dt = np.float32
    elif args.float_endian == "little":
        dt = "<f4"
    else:  # "big"
        dt = ">f4"

    # 1. Read params
    nx, ny, nlfp = read_params(Path(args.params))
    ncell = nx * ny
    print(f"[INFO] nx={nx}, ny={ny}, NLFP={nlfp}, ncell={ncell}")

    # 2. Read inputs
    grdare = read_1d_field(Path(args.grdare), ncell, dtype=dt)
    rivlen = read_1d_field(Path(args.rivlen), ncell, dtype=dt)
    rivwth = read_1d_field(Path(args.rivwth), ncell, dtype=dt)
    rivhgt = read_1d_field(Path(args.rivhgt), ncell, dtype=dt)
    fldhgt = read_fldhgt(Path(args.fldhgt), ncell, nlfp, dtype=dt)
    levfrc = read_1d_field(Path(args.levfrc), ncell, dtype=dt)
    levhgt = read_1d_field(Path(args.levhgt), ncell, dtype=dt)

    # Assume RIVSTOMAX = 0 (can be extended to read from file if needed)
    rivstomax = np.zeros(ncell, dtype=float)

    # 3. Compute levee parameters
    large_value = 1.0e18
    levbashgt, levdst, levbassto, levtopsto, levfilsto = compute_levee_params(
        grdare, rivlen, rivwth, rivstomax, fldhgt, levfrc, levhgt,
        large_value=large_value
    )

    # 4. Handle cells with no levee and sea cells
    #    - "no levee": LEVBASHGT is large_value
    #    - sea: RIVHGT == -9999
    no_levee_mask = levbashgt >= large_value * 0.5
    sea_mask = (rivhgt == -9999.0)

    undefined_mask = no_levee_mask | sea_mask

    # First, set all outputs for those cells to -9999 as a placeholder
    for arr in (levbashgt, levdst, levbassto, levtopsto, levfilsto):
        arr[undefined_mask] = -9999.0

    # 5. Physical levee height: LEVPHYHGT = LEVHGT - LEVBASHGT
    #    - For undefined cells, set NaN for now; then we will convert to -9999.
    levphyhgt = np.full(ncell, np.nan, dtype=float)
    valid_for_phys = ~undefined_mask

    levphyhgt[valid_for_phys] = (
        levhgt[valid_for_phys].astype(float) - levbashgt[valid_for_phys].astype(float)
    )

    # 6. Cells with LEVPHYHGT <= 0 are treated as "no levee"
    neg_mask = np.zeros(ncell, dtype=bool)
    neg_mask[valid_for_phys] = levphyhgt[valid_for_phys] <= 0.0

    # Final "no levee" mask
    final_no_levee = undefined_mask | neg_mask

    # For these, set all outputs to -9999
    for arr in (levbashgt, levdst, levbassto, levtopsto, levfilsto, levphyhgt):
        arr[final_no_levee] = -9999.0

    # LEVHGT_MOD: original LEVHGT except "no levee" cells become -9999
    levhgt_mod = levhgt.astype(float)
    levhgt_mod[final_no_levee] = -9999.0

    # Ensure no NaN remains in LEVPHYHGT
    levphyhgt[np.isnan(levphyhgt)] = -9999.0

    # 7. Write outputs (float32)
    out_dir = Path(args.out_levbashgt).parent
    out_dir.mkdir(parents=True, exist_ok=True)

    np.asarray(levbashgt,  dtype=np.float32).tofile(args.out_levbashgt)
    np.asarray(levdst,     dtype=np.float32).tofile(args.out_levdst)
    np.asarray(levphyhgt,  dtype=np.float32).tofile(args.out_levphyhgt)
    np.asarray(levhgt_mod, dtype=np.float32).tofile(args.out_levhgt_mod)
    np.asarray(levbassto,  dtype=np.float32).tofile(args.out_levbassto)
    np.asarray(levtopsto,  dtype=np.float32).tofile(args.out_levtopsto)
    np.asarray(levfilsto,  dtype=np.float32).tofile(args.out_levfilsto)

    print("[INFO] Finished. Wrote:")
    print("  LEVDST     ->", args.out_levdst)
    print("  LEVBASHGT  ->", args.out_levbashgt)
    print("  LEVPHYHGT  ->", args.out_levphyhgt)
    print("  LEVHGT_MOD ->", args.out_levhgt_mod)
    print("  LEVBASSTO  ->", args.out_levbassto)
    print("  LEVTOPSTO  ->", args.out_levtopsto)
    print("  LEVFILSTO  ->", args.out_levfilsto)


if __name__ == "__main__":
    main()
