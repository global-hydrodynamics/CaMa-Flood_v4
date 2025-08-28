include_dirs := 
local_objects := \
text_lib.o \
funit_lib.o \
bin_lib.o \
nc_lib.o \
mapframe_lib.o \
util_lib.o \
LU.o \
layer_lib.o
#local_objects := \
text_lib.o \
const_lib.o \
funit_lib.o \
bin_lib.o \
gt_lib.o \
nc_lib.o \
mapframe_lib.o \
util_lib.o \
LU.o \
layer_lib.o

$(eval $(call make-library, $(subdirectory), $(local_objects), $(include_dirs)))
