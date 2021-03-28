#!/bin/sh

TAG=`date '+%Y%m%d'`
VER="v401"
BASE=`pwd`
DIR="cmf_${VER}_src"

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

cd $PKGBASE

##### gosh #####
mkdir -p gosh
cd gosh
  pwd
  cp ${BASE}/gosh/compile.sh    .
  cp ${BASE}/gosh/test*.sh      .
  rm -f ./.*                         2> /dev/null
cd $PKGBASE

##### etc #####
mkdir -p etc
cd etc
  pwd

  PROJ="downscale_flddph"
  cd $PKGBASE/etc/
  mkdir $PROJ
  cd $PROJ
    cp ${BASE}/etc/$PROJ/*.sh      .
    cp ${BASE}/etc/$PROJ/*.py      .
    cp ${BASE}/etc/$PROJ/*.md .
    cp -r ${BASE}/etc/$PROJ/src    .
    cd src
    make clean
    cd ..
    rm -f ./.*                 2> /dev/null
    rm -f ./src/.*             2> /dev/null
  cd ..

  PROJ="runoff_preset"
  cd $PKGBASE/etc/
  mkdir $PROJ
  cd $PROJ
    cp ${BASE}/etc/$PROJ/*.sh      .
    cp ${BASE}/etc/$PROJ/*.py      .
    cp ${BASE}/etc/$PROJ/*.md .
    cp -r ${BASE}/etc/$PROJ/src    .
      cd src
      make clean
      cd ..
    rm -f ./.*                 2> /dev/null
    rm -f ./src/.*             2> /dev/null
  cd ..

 PROJ="n-year_flood_depth"
  cd $PKGBASE/etc/
  mkdir $PROJ
  cd $PROJ
    cp -r ${BASE}/etc/$PROJ/script .
      cd script 
        cd src
        make clean
        cd ..
      rm -f ./.*                 2> /dev/null
      rm -f ./src/.*             2> /dev/null
    cd ..
  cd ..


 PROJ="reservoir_operation"
  cd $PKGBASE/etc/
  mkdir $PROJ
  cd $PROJ
    cp ${BASE}/etc/$PROJ/*.sh .
    cp ${BASE}/etc/$PROJ/*.py .
    cp ${BASE}/etc/$PROJ/*.md .
    cp -r ${BASE}/etc/$PROJ/sample_data .
    cp -r ${BASE}/etc/$PROJ/dam_params  .

    rm -f ./.*                 2> /dev/null
  cd ..

 PROJ="validation"
  cd $PKGBASE/etc/
  mkdir $PROJ
  cd $PROJ
    cp ${BASE}/etc/$PROJ/*.sh .
    cp ${BASE}/etc/$PROJ/*.py .
    cp ${BASE}/etc/$PROJ/*.md .
    cp -r ${BASE}/etc/$PROJ/src .

    cd src
    make clean
    cd ..
    rm -f ./.*                 2> /dev/null
    rm -f ./src/.*             2> /dev/null

    cd ..
    rm -f ./.*                 2> /dev/null
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


