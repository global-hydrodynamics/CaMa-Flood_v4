#!/bin/sh

# link output directory

mkdir -p downscale
mkdir -p fig_down

###########

# Downscale flood depth for Mississippi

WEST=-100
EAST=-80
SOUTH=30
NORTH=45
#HIRES="3sec"
HIRES="1min"

LEVFRC="map/levfrc.bin"

FFLDDPH="./levee_out/flddph2000.bin"
FLEVDPH="./levee_out/levdph2000.bin"
FFLOOD="./downscale/flood_lev.bin"
FDPH="./downscale/dph_lev.bin"
FFIG="./fig_down/flood_lev.jpg"

############

rm -f dph_lev.bin
rm -f flood_lev.bin
rm -f hand.bin
rm -f protect_pix.bin

# define levee protected pixels 
echo "./src/define_protect_pix  $WEST $EAST $SOUTH $NORTH $HIRES $LEVFRC"
./src/define_protect_pix  $WEST $EAST $SOUTH $NORTH $HIRES  $LEVFRC

# downscale flood depth with levee

IDAY=62  ## day of year for downscale:

echo "./src/downscale_flddph_levee  $WEST $EAST $SOUTH $NORTH $HIRES $FFLDDPH $FLEVDPH $FFLOOD $IDAY"
./src/downscale_flddph_levee  $WEST $EAST $SOUTH $NORTH $HIRES $FFLDDPH $FLEVDPH $FFLOOD $IDAY

echo "./src/conv_flood_levee  $WEST $EAST $SOUTH $NORTH $FFLOOD $FDPH $HIRES 10"
./src/conv_flood_levee        $WEST $EAST $SOUTH $NORTH $FFLOOD $FDPH $HIRES 10

echo "./src/draw_flddph.py $WEST $EAST $SOUTH $NORTH $FDPH $HIRES 10"
python ./src/draw_flddph.py $WEST $EAST $SOUTH $NORTH $FDPH $HIRES 10

mv flddph.jpg $FFIG

