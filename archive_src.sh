#!/bin/sh
# Archiving source code/script 
# To be used in GitHub branch dir. Only codes are archivd.
# Make sure only required files are included in Git Repo.

TAG=`date '+%Y%m%d'`
VER="v404"
BASE=`pwd`
DIR="cmf_${VER}_src"

mkdir $DIR
cd    $DIR
PKGBASE=`pwd`

cd $PKGBASE

#export COPYFILE_DISABLE=1
#tar czvf ${DIR}.tar.gz --exclude='.DS_Store' ${DIR}
#exit

cp -r ${BASE}/adm  .
cp -r ${BASE}/doc  .
cp -r ${BASE}/etc  .
cp -r ${BASE}/gosh .
cp -r ${BASE}/map  .
cp -r ${BASE}/out  .
cp -r ${BASE}/src  .
cp -r ${BASE}/util .

cp ${BASE}/LICENSE        .
cp ${BASE}/README.md      .
cp ${BASE}/Versions.md    .
cp ${BASE}/archive_src.sh .
cp ${BASE}/.gitignore     .

#===========

cd ${BASE}

export COPYFILE_DISABLE=1
tar czvf ${DIR}.tar.gz --exclude='.DS_Store' ${DIR}


