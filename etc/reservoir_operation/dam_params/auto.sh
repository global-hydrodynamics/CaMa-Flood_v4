#!/bin/sh
# =============================================================
# Sample script to allocate GRanD data to CaMa-Flood river map
#
# Pre-requirements
# 1) Download GRanD, GRSAD, ReGeom data
# 2) Execute naturalizrd (no dam) simulation using the same river map
#
# Written by Risa Hanazaki & Dai Yamazaki
#===========================================================

# Link Required Data
./s00-link.sh

# Allocate GRanD dam on CaMa river map
./s01-calc_damloc.sh

exit 0
