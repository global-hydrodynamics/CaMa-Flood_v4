#!/bin/sh

TAG=`date '+%Y%m%d'`
VER="v400"
BASE=`pwd`
DIR="CaMa-Flood_${VER}_${TAG}"

mkdir $DIR
cd    $DIR
PKGBASE=`pwd`

cd $PKGBASE

#export COPYFILE_DISABLE=1
#tar czvf ${DIR}.tar.gz --exclude='.DS_Store' ${DIR}
#exit

##### Adm #####
cp -r ${BASE}/adm .
cd adm
  pwd
  rm -f ./.*  2> /dev/null
cd $PKGBASE

##### Util #####
cp -r ${BASE}/util .
cd util
  pwd
  rm -f ./.*  2> /dev/null
cd $PKGBASE

##### Src #####
mkdir -p src
cd src
  pwd
  cp ${BASE}/src/Makefile .
  cp ${BASE}/src/*.F90    .
  rm -f ./.*  2> /dev/null

  cp -r ${BASE}/src/MOJ   .
  rm -f ./MOJ/.*          2> /dev/null

cd $PKGBASE

##### Map #####
mkdir -p map
cd map
  pwd
  cp -r ${BASE}/map/data           .
  rm -f ./data/.*   2> /dev/null


  mkdir -p src
  cd src
    mkdir src_param
    cd src_param
      pwd
      cp ${BASE}/map/src/src_param/Makefile .
      cp ${BASE}/map/src/src_param/*.F90    .
      cp ${BASE}/map/src/src_param/*.sh     .
      rm -f ./.*  2> /dev/null
    cd ..

    mkdir src_region
    cd src_region
      pwd
      cp ${BASE}/map/src/src_region/Makefile .
      cp ${BASE}/map/src/src_region/*.F90    .
      cp ${BASE}/map/src/src_region/*.sh     .
      cp ${BASE}/map/src/src_region/*.txt    .
      rm -f ./.*  2> /dev/null
    cd ..
  cd ..

  MAPDIR='glb_15min'
  echo map/$MAPDIR
  cp -r ${BASE}/map/$MAPDIR .
  rm -f ./${MAPDIR}/.*               2> /dev/null
  rm -f ./${MAPDIR}/*/.*             2> /dev/null

  MAPDIR='conus_06min'
  echo map/$MAPDIR
  cp -r ${BASE}/map/$MAPDIR .
  rm -f ./${MAPDIR}/.*               2> /dev/null
  rm -f ./${MAPDIR}/*/.*             2> /dev/null

  MAPDIR='tej_01min'
  echo map/$MAPDIR
  cp -r ${BASE}/map/$MAPDIR .
  rm -f ./${MAPDIR}/.*               2> /dev/null
  rm -f ./${MAPDIR}/*/.*             2> /dev/null
cd $PKGBASE

##### gosh #####
mkdir -p gosh
cd gosh
  pwd
  cp ${BASE}/gosh/compile.sh    .
  cp ${BASE}/gosh/test*.sh      .
  rm -f ./.*                         2> /dev/null
cd $PKGBASE

##### Input #####
mkdir -p inp
cd inp
  pwd

  ROFF="test_1deg"
  echo inp/$ROFF
  cp -r ${BASE}/inp/$ROFF .
  rm -f ./${ROFF}/.*             2> /dev/null

  ROFF="test_15min_nc"
  echo inp/$ROFF
  cp -r ${BASE}/inp/$ROFF .
  rm -f ./${ROFF}/.*             2> /dev/null

  ROFF="test_jpn_1hr"
  echo inp/$ROFF
  cp -r ${BASE}/inp/$ROFF .
  rm -f ./${ROFF}/.*             2> /dev/null
cd $PKGBASE

##### Output #####
mkdir -p out
cd out
  pwd
#  cp -r ${BASE}/out/src      .
#  rm -f .*
cd $PKGBASE

##### etc #####
mkdir -p etc
cd etc
  pwd

  PROJ="downscale_flddph"
  mkdir $PROJ
  cd $PROJ
    cp ${BASE}/etc/$PROJ/*.sh      .
    cp ${BASE}/etc/$PROJ/*.py      .
    cp -r ${BASE}/etc/$PROJ/src    .
    cd src
    make clean
    cd ..
    rm -f ./.*                 2> /dev/null
    rm -f ./src/.*             2> /dev/null
  cd ..

  PROJ="flood_duration"
  mkdir $PROJ
  cd $PROJ
    cp ${BASE}/etc/$PROJ/*.sh      .
    cp ${BASE}/etc/$PROJ/*.py      .
    cp -r ${BASE}/etc/$PROJ/src    .
    cd src
    make clean
    cd ..
    rm -f ./.*                 2> /dev/null
    rm -f ./src/.*             2> /dev/null
  cd ..

  PROJ="runoff_preset"
  mkdir $PROJ
  cd $PROJ
    cp ${BASE}/etc/$PROJ/*.sh      .
    cp ${BASE}/etc/$PROJ/*.py      .
    cp -r ${BASE}/etc/$PROJ/src    .
    cd src
    make clean
    cd ..
    rm -f ./.*                 2> /dev/null
    rm -f ./src/.*             2> /dev/null
  cd ..

cd $PKGBASE
###

cp -r ${BASE}/doc             .
rm -f ./doc/.*                 2> /dev/null

cp ${BASE}/archive_pkg.sh         .
cp ${BASE}/archive_src.sh         .

#===========

cd ${BASE}

export COPYFILE_DISABLE=1
tar czvf ${DIR}.tar.gz --exclude='.DS_Store' ${DIR}


