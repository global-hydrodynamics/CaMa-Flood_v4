#!/bin/sh
FILE_MD=$1
FILE_DOCX=$2

LUA="./style-cmf.lua"
TEMP_DOCX="WordStyleTemplate.docx"

pandoc $FILE_DOCX -t gfm --reference-doc=$TEMP_DOCX --lua-filter=$LUA --extract-media=media -o $FILE_MD 