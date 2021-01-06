#!/bin/sh 

clean=$1       # 1th argument in script : if == "no" then clean before compiling

#CAMADIR="/Users/yamadai/work/CaMa-Flood"
CAMADIR=`pwd`/..
BASE=$CAMADIR  # code location

libs="util map/src/src_param map/src/src_region src"
for lib in $libs
do 
  cd $BASE/$lib
  if [ "$clean" = "no" ];then
    echo "*********** $lib **********"
    make -B all
  elif [ "$clean" = "clean" ];then
    make clean
  else
    make clean
    echo "*********** $lib **********"
    make all
  fi
done 

if [ "$clean" != "clean" ];then
  cd $BASE/src/
  make MAIN_cmf
  if [ -r MAIN_cmf ]; then
   echo "Compilation OK!  Executable created: $BASE/src/MAIN_cmf"
  else
   echo "### ERROR in compike ###"
   echo "Problems during Compilation. Executable not created."
   echo "### ### ### ### ###"
  fi
fi

