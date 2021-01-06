#!/bin/sh

WESN_G=`./gradsinfo`
echo $WESN_G

mkdir -p tmpctl
cd tmpctl
rm -f *.ctl

##### nextxy, lon-lat 2 layers
../wrte_ctl_map nextxy.bin nextxy.ctl int4 xy                $WESN_G
../wrte_ctl_map downxy.bin downxy.ctl int4 xy                $WESN_G

../wrte_ctl_map nextxy_noedge.bin nextxy_noedge.ctl int4 xy  $WESN_G
../wrte_ctl_map lsmask.bin lsmask.ctl int4 x                 $WESN_G

../wrte_ctl_map lonlat.bin lonlat.ctl real xy                $WESN_G

#### topography 
../wrte_ctl_map elevtn.bin elevtn.ctl real x                 $WESN_G
../wrte_ctl_map grdare.bin grdare.ctl real x                 $WESN_G
../wrte_ctl_map ctmare.bin ctmare.ctl real x                 $WESN_G

../wrte_ctl_map nxtdst.bin nxtdst.ctl real x                 $WESN_G
../wrte_ctl_map width.bin  width.ctl  real x                 $WESN_G
../wrte_ctl_map rivlen.bin rivlen.ctl real x                 $WESN_G
../wrte_ctl_map outupa.bin outupa.ctl real x                 $WESN_G
../wrte_ctl_map uparea.bin uparea.ctl real x                 $WESN_G

../wrte_ctl_map rivwth.bin rivwth.ctl real x                 $WESN_G
../wrte_ctl_map rivhgt.bin rivhgt.ctl real x                 $WESN_G

../wrte_ctl_map fldhgt.bin fldhgt.ctl fldp x                 $WESN_G

../wrte_ctl_map catmpx.bin catmpx.ctl int4 x                 $WESN_G
../wrte_ctl_map outfix.bin outfix.ctl int4 x                 $WESN_G

../wrte_ctl_map rivseq.bin rivseq.ctl int4 x                 $WESN_G
../wrte_ctl_map upgrid.bin upgrid.ctl int4 x                 $WESN_G

../wrte_ctl_map uparea_grid.bin uparea_grid.ctl real x       $WESN_G
../wrte_ctl_map nxtdst_grid.bin nxtdst_grid.ctl real x       $WESN_G
../wrte_ctl_map rivlen_grid.bin rivlen_grid.ctl real x       $WESN_G

../wrte_ctl_map basin.bin  basin.ctl  int4 x                 $WESN_G
../wrte_ctl_map bsncol.bin bsncol.ctl int4 x                 $WESN_G

# CaMa-Flood
../wrte_ctl_map rivwth_gwdlr.bin rivwth_gwdlr.ctl real x     $WESN_G
../wrte_ctl_map rivhgt_pth.bin   rivhgt_pth.ctl   real x     $WESN_G
../wrte_ctl_map rivhgt_inf.bin   rivhgt_inf.ctl   real x     $WESN_G
../wrte_ctl_map outclm.bin       outclm.ctl       real x     $WESN_G

mv *.ctl ../../

cd ..
rm -rf tmpctl