#!/bin/sh
# Archiving source codes, scripts, and minimum data for test simulation.
# To be used in working directory, before executing test simulations.
# Make sure only required files are included.


TAG=`date '+%Y%m%d'`
VER="v401"
BASE=`pwd`
DIR="CaMa-Flood_${VER}_${TAG}"

mkdir $DIR
cd    $DIR
PKGBASE=`pwd`

cd $PKGBASE

#export COPYFILE_DISABLE=1
#tar czvf ${DIR}.tar.gz --exclude='.DS_Store' ${DIR}
#exit

##### Dirs treated simply #####
cp -r ${BASE}/adm  .
cp -r ${BASE}/doc  .
cp -r ${BASE}/etc  .
cp -r ${BASE}/gosh .
cp -r ${BASE}/src  .
cp -r ${BASE}/util .
mkdir -p out

cp ${BASE}/LICENSE     .
cp ${BASE}/README.md   .
cp ${BASE}/versions.md .
cp ${BASE}/archive_src .
cp ${BASE}/archive_pkg .
cp ${BASE}/.gitignore  .

##### Map #####
mkdir -p map
cd ${PKGBASE}/map
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

##### Input #####
mkdir -p inp
cd ${PKGBASE}/inp
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

#===========

cd ${BASE}

export COPYFILE_DISABLE=1
tar czvf ${DIR}.tar.gz --exclude='.DS_Store' ${DIR}


