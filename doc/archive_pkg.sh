#!/bin/sh
# Archiving source code/script 
# To be used in GitHub branch dir. Only codes are archivd.
# Make sure only required files are included in Git Repo.

TAG=`date '+%Y%m%d'`
VER="v406"
BASE=`pwd`
PKG="cmf_${VER}_pkg"

mv ${PKG} ${PKG}_${TAG}

export COPYFILE_DISABLE=1
tar czvf ${PKG}_${TAG}.tar.gz --exclude='.DS_Store' ${PKG}_${TAG}

mv ${PKG}_${TAG} ${PKG} 


