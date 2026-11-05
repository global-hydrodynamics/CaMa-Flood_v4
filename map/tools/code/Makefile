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
# Remove stale executables from the pre-Phase-C analysis naming convention.
RETIRED_ANALYSIS_BINS := analysis_get_rivinfo analysis_get_upbasin analysis_get_subbasin analysis_get_vecmap
ALLOC_SOURCES := $(wildcard $(CODE_DIR)/allocate_*.F90)
ALLOC_BINS := $(patsubst %.F90,%,$(ALLOC_SOURCES))
COLOR_MODULE_OBJECT := $(CODE_DIR)/mod_map_color.o
BIFPRM_CSV_MODULE_OBJECT := $(CODE_DIR)/mod_bifprm_csv.o
ALLOCATION_MODULE_OBJECT := $(CODE_DIR)/mod_allocation_map_io.o

.PHONY: all region params param analysis allocate clean
all: region params analysis allocate

region: $(REGION_BINS)

# Parameter programs under param_*.F90, including param_water_calc_prmwat,
# are discovered automatically and built together by the shared target.
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

allocate: $(ALLOC_BINS)

$(ALLOCATION_MODULE_OBJECT): $(CODE_DIR)/mod_allocation_map_io.F90
	cd "$(CODE_DIR)" && $(FC) $(FFLAGS) $(INC) -c mod_allocation_map_io.F90 -o mod_allocation_map_io.o

$(CODE_DIR)/allocate_%: $(CODE_DIR)/allocate_%.F90 $(ALLOCATION_MODULE_OBJECT)
	cd "$(CODE_DIR)" && $(FC) $(FFLAGS) $(LFLAGS) $(INC) $(LIB) mod_allocation_map_io.o "$(@F).F90" -o "$(@F)" $(LIB)

$(COLOR_MODULE_OBJECT): $(CODE_DIR)/mod_map_color.F90
	cd "$(CODE_DIR)" && $(FC) $(FFLAGS) $(INC) -c mod_map_color.F90 -o mod_map_color.o

$(BIFPRM_CSV_MODULE_OBJECT): $(CODE_DIR)/mod_bifprm_csv.F90
	cd "$(CODE_DIR)" && $(FC) $(FFLAGS) $(INC) -c mod_bifprm_csv.F90 -o mod_bifprm_csv.o

$(CODE_DIR)/param_bif_set_basin: $(CODE_DIR)/param_bif_set_basin.F90 $(COLOR_MODULE_OBJECT) $(BIFPRM_CSV_MODULE_OBJECT)
	cd "$(CODE_DIR)" && $(FC) $(FFLAGS) $(LFLAGS) $(INC) $(LIB) mod_map_color.o mod_bifprm_csv.o param_bif_set_basin.F90 -o param_bif_set_basin $(LIB)

$(CODE_DIR)/param_mpi_set_basin: $(CODE_DIR)/param_mpi_set_basin.F90 $(COLOR_MODULE_OBJECT) $(BIFPRM_CSV_MODULE_OBJECT)
	cd "$(CODE_DIR)" && $(FC) $(FFLAGS) $(LFLAGS) $(INC) $(LIB) mod_map_color.o mod_bifprm_csv.o param_mpi_set_basin.F90 -o param_mpi_set_basin $(LIB)

$(CODE_DIR)/analysis_check_mpi_bifurcation: $(CODE_DIR)/analysis_check_mpi_bifurcation.F90 $(BIFPRM_CSV_MODULE_OBJECT)
	cd "$(CODE_DIR)" && $(FC) $(FFLAGS) $(LFLAGS) $(INC) $(LIB) mod_bifprm_csv.o analysis_check_mpi_bifurcation.F90 -o analysis_check_mpi_bifurcation $(LIB)

$(CODE_DIR)/analysis_mpi_region_color: $(CODE_DIR)/analysis_mpi_region_color.F90 $(COLOR_MODULE_OBJECT)
	cd "$(CODE_DIR)" && $(FC) $(FFLAGS) $(LFLAGS) $(INC) $(LIB) mod_map_color.o analysis_mpi_region_color.F90 -o analysis_mpi_region_color $(LIB)

$(CODE_DIR)/analysis_%: $(CODE_DIR)/analysis_%.F90
	cd "$(CODE_DIR)" && $(FC) $(FFLAGS) $(LFLAGS) $(INC) $(LIB) "$(@F).F90" -o "$(@F)" $(LIB)

clean:
	cd "$(CODE_DIR)" && $(RM) $(notdir $(REGION_BINS)) $(notdir $(PARAM_BINS)) $(notdir $(ANALYSIS_BINS)) $(notdir $(ALLOC_BINS)) $(RETIRED_ANALYSIS_BINS) *.o *.mod *.pyc *.d *.s
	cd "$(CODE_DIR)" && /bin/rm -rf __pycache__
