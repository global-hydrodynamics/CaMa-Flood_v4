#!/bin/sh

# link output directory

ln -sf ../../out/levee_test       out_lev
ln -sf ../../out/test1-glb_15min  out_ori

mkdir -p data
mkdir -p fig

# some analysis on river depth & flood fraction

./src/analysis_output

###########

# Downscale flood depth for Mississippi

WEST=-93
EAST=-86
SOUTH=29
NORTH=35
HIRES="3sec"
#HIRES="1min"

# define levee protected pixels 
echo "./src/define_protect_pix  $WEST $EAST $SOUTH $NORTH $HIRES"
./src/define_protect_pix  $WEST $EAST $SOUTH $NORTH $HIRES

# downscale flood depth with levee

IDAY=62  ## day of year for downscale:

FFLDDPH="./out_lev/flddph2001.bin"
FLEVDPH="./out_lev/levdph2001.bin"
FFLOOD="./data/flood_lev.bin"
FDPH="./data/dph_lev.bin"
FFIG="./fig/flood_lev.jpg"

echo "./src/downscale_flddph_levee  $WEST $EAST $SOUTH $NORTH $HIRES $FFLDDPH $FLEVDPH $FFLOOD $IDAY"
./src/downscale_flddph_levee  $WEST $EAST $SOUTH $NORTH $HIRES $FFLDDPH $FLEVDPH $FFLOOD $IDAY

echo "./src/conv_flood_levee  $WEST $EAST $SOUTH $NORTH $FFLOOD $FDPH $HIRES 10"
./src/conv_flood_levee        $WEST $EAST $SOUTH $NORTH $FFLOOD $FDPH $HIRES 10

echo "draw_flddph.py $WEST $EAST $SOUTH $NORTH $FDPH $HIRES 10"
python draw_flddph.py $WEST $EAST $SOUTH $NORTH $FDPH $HIRES 10

mv flddph.jpg $FFIG


######

IDAY=62  ## day of year for downscale:

FFLDDPH="./out_ori/flddph2001.bin"
FLEVDPH="NONE"
FFLOOD="./data/flood_ori.bin"
FDPH="./data/dph_ori.bin"
FFIG="./fig/flood_ori.jpg"

echo "./src/downscale_flddph_levee  $WEST $EAST $SOUTH $NORTH $HIRES $FFLDDPH $FLEVDPH $FFLOOD $IDAY"
./src/downscale_flddph_levee  $WEST $EAST $SOUTH $NORTH $HIRES $FFLDDPH $FLEVDPH $FFLOOD $IDAY

echo "./src/conv_flood_levee  $WEST $EAST $SOUTH $NORTH $FFLOOD $FDPH $HIRES 10"
./src/conv_flood_levee        $WEST $EAST $SOUTH $NORTH $FFLOOD $FDPH $HIRES 10

echo "draw_flddph.py $WEST $EAST $SOUTH $NORTH $FDPH $HIRES 10"
python draw_flddph.py $WEST $EAST $SOUTH $NORTH $FDPH $HIRES 10

mv flddph.jpg $FFIG

