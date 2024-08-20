#!/bin/sh

# set link to map directory

MAP="../../map/glb_15min"
ln -s $MAP map

./src/calc_levparam

