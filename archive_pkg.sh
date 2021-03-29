#!/bin/sh
# Archiving source code/script 
# To be used in GitHub branch dir. Only codes are archivd.
# Make sure only required files are included in Git Repo.

TAG=`date '+%Y%m%d'`
VER="v401"
BASE=`pwd`
DIR="cmf_${VER}_pkg"

export COPYFILE_DISABLE=1
tar czvf ${DIR}_${TAG}.tar.gz --exclude='.DS_Store' ${DIR}


