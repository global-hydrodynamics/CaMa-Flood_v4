# Shared v5 map-tool build. Executables and compiler intermediates live here.
CODE_DIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
include $(CODE_DIR)/../../../adm/Mkinclude

REGION_NAMES := cut_domain cut_bifway set_map combine_hires generate_inpmat
REGION_BINS := $(addprefix $(CODE_DIR)/region_,$(REGION_NAMES))
PARAM_SOURCES := $(wildcard $(CODE_DIR)/param_*.F90)
PARAM_BINS := $(patsubst %.F90,%,$(PARAM_SOURCES))

.PHONY: all region params param clean
all: region params

region: $(REGION_BINS)

# Parameter programs will be added under param_*.F90 in Phase B2.
ifeq ($(strip $(PARAM_SOURCES)),)
params:
	@echo "No param_*.F90 sources are available yet (planned for Phase B2)."
else
params: $(PARAM_BINS)
endif

param: params

$(CODE_DIR)/region_%: $(CODE_DIR)/region_%.F90
	cd "$(CODE_DIR)" && $(FC) $(FFLAGS) $(LFLAGS) $(INC) $(LIB) "$(@F).F90" -o "$(@F)" $(LIB)

$(CODE_DIR)/param_%: $(CODE_DIR)/param_%.F90
	cd "$(CODE_DIR)" && $(FC) $(FFLAGS) $(LFLAGS) $(INC) $(LIB) "$(@F).F90" -o "$(@F)" $(LIB)

clean:
	cd "$(CODE_DIR)" && $(RM) $(notdir $(REGION_BINS)) $(notdir $(PARAM_BINS)) *.o *.mod *.pyc *.d *.s
	cd "$(CODE_DIR)" && /bin/rm -rf __pycache__
