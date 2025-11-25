#!/bin/sh
FILE_DOCX=$1
FILE_MD=$2

LUA="./style-cmf.lua"
MEDIA_DIR="media_${FILE_MD}"

pandoc $FILE_DOCX -t gfm --reference-doc=$TEMP_DOCX --lua-filter=$LUA --extract-media=$MEDIA_DIR -o $FILE_MD 
