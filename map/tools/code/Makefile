# Shared v5 map-tool build. Executables and compiler intermediates live here.
CODE_DIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
include $(CODE_DIR)/../../../adm/Mkinclude

REGION_NAMES := cut_domain cut_bifway set_map combine_hires default_inpmat
REGION_BINS := $(addprefix $(CODE_DIR)/region_,$(REGION_NAMES))
PARAM_SOURCES := $(wildcard $(CODE_DIR)/param_*.F90)
PARAM_BINS := $(patsubst %.F90,%,$(PARAM_SOURCES))
# The wildcard includes analysis_mpi_region_color and other standalone diagnostics.
# Region and basin display coloring share mod_map_color.o.
ANALYSIS_SOURCES := $(wildcard $(CODE_DIR)/analysis_*.F90)
ANALYSIS_BINS := $(patsubst %.F90,%,$(ANALYSIS_SOURCES))
COLOR_MODULE_OBJECT := $(CODE_DIR)/mod_map_color.o

.PHONY: all region params param analysis clean
all: region params analysis

region: $(REGION_BINS)

# Parameter programs under param_*.F90 are built together by the shared target.
ifeq ($(strip $(PARAM_SOURCES)),)
params:
	@echo "No param_*.F90 sources are available yet (planned for Phase B2)."
else
params: $(PARAM_BINS)
endif

param: params

ifeq ($(strip $(ANALYSIS_SOURCES)),)
analysis:
	@echo "No analysis_*.F90 sources are available yet."
else
analysis: $(ANALYSIS_BINS)
endif

$(CODE_DIR)/region_%: $(CODE_DIR)/region_%.F90
	cd "$(CODE_DIR)" && $(FC) $(FFLAGS) $(LFLAGS) $(INC) $(LIB) "$(@F).F90" -o "$(@F)" $(LIB)

$(CODE_DIR)/param_%: $(CODE_DIR)/param_%.F90
	cd "$(CODE_DIR)" && $(FC) $(FFLAGS) $(LFLAGS) $(INC) $(LIB) "$(@F).F90" -o "$(@F)" $(LIB)

$(COLOR_MODULE_OBJECT): $(CODE_DIR)/mod_map_color.F90
	cd "$(CODE_DIR)" && $(FC) $(FFLAGS) $(INC) -c mod_map_color.F90 -o mod_map_color.o

$(CODE_DIR)/param_bif_set_basin: $(CODE_DIR)/param_bif_set_basin.F90 $(COLOR_MODULE_OBJECT)
	cd "$(CODE_DIR)" && $(FC) $(FFLAGS) $(LFLAGS) $(INC) $(LIB) mod_map_color.o param_bif_set_basin.F90 -o param_bif_set_basin $(LIB)

$(CODE_DIR)/param_mpi_set_basin: $(CODE_DIR)/param_mpi_set_basin.F90 $(COLOR_MODULE_OBJECT)
	cd "$(CODE_DIR)" && $(FC) $(FFLAGS) $(LFLAGS) $(INC) $(LIB) mod_map_color.o param_mpi_set_basin.F90 -o param_mpi_set_basin $(LIB)

$(CODE_DIR)/analysis_mpi_region_color: $(CODE_DIR)/analysis_mpi_region_color.F90 $(COLOR_MODULE_OBJECT)
	cd "$(CODE_DIR)" && $(FC) $(FFLAGS) $(LFLAGS) $(INC) $(LIB) mod_map_color.o analysis_mpi_region_color.F90 -o analysis_mpi_region_color $(LIB)

$(CODE_DIR)/analysis_%: $(CODE_DIR)/analysis_%.F90
	cd "$(CODE_DIR)" && $(FC) $(FFLAGS) $(LFLAGS) $(INC) $(LIB) "$(@F).F90" -o "$(@F)" $(LIB)

clean:
	cd "$(CODE_DIR)" && $(RM) $(notdir $(REGION_BINS)) $(notdir $(PARAM_BINS)) $(notdir $(ANALYSIS_BINS)) *.o *.mod *.pyc *.d *.s
	cd "$(CODE_DIR)" && /bin/rm -rf __pycache__
