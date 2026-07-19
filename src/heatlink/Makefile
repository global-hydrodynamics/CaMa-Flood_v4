TOP := $(abspath ../..)
MKINCLUDE ?= $(TOP)/adm/Mkinclude
include $(MKINCLUDE)

LIBNAME = libheatlink.a

TOPDIR ?= ..
MODDIRS = \
	-I$(TOPDIR)/common \
	-I$(TOPDIR)/mod \
	-I$(TOPDIR)/phys \
	-I$(TOPDIR)/io \
	-I$(TOPDIR)

INC  += $(MODDIRS)
MODS += $(MODDIRS)

OBJS = \
	topo_mod.o \
	thermo_mod.o \
	water_storage_adapter_mod.o \
	heatlink_input_adapter_mod.o \
	river_water_advection_mod.o \
	river_ice_advection_mod.o \
	heatlink_river_mod.o \
	river_water_advection_coupler.o

TESTS = \
	test_water_storage_adapter \
	test_heatlink_input_adapter \
	test_river_water_advection \
	test_river_water_advection_boundaries \
	test_river_ice_advection \
	test_river_ice_advection_boundaries \
	test_river_water_ice_advection_energy
TEST_EXES = $(TESTS)
RIVER_WATER_TESTS = \
	test_river_water_advection \
	test_river_water_advection_boundaries
TOPOLOGY_TESTS = \
	$(RIVER_WATER_TESTS) \
	test_river_ice_advection \
	test_river_ice_advection_boundaries \
	test_river_water_ice_advection_energy

.SUFFIXES: .o .F90 .f90
%.o: %.F90
	$(FCMP) -c $(FFLAGS) $(INC) $(MODS) $(CFLAGS) $< -o $@
%.o: %.f90
	$(FCMP) -c $(FFLAGS) $(INC) $(MODS) $(CFLAGS) $< -o $@

.PHONY: all clean test
all: $(LIBNAME)
$(LIBNAME): $(OBJS)
	@rm -f $@
	ar -rv $@ $(OBJS)

heatlink_river_mod.o: water_storage_adapter_mod.o heatlink_input_adapter_mod.o \
	river_water_advection_mod.o river_ice_advection_mod.o
river_water_advection_coupler.o: heatlink_river_mod.o

$(TOPOLOGY_TESTS): %: test/%.f90 $(LIBNAME) ../yos_cmf_map.o
	$(FCMP) $(FFLAGS) $(INC) $(MODS) $(CFLAGS) $< ../yos_cmf_map.o \
	    -L. -lheatlink -L../phys -lphys -L../common -lcommon \
	    $(LDFLAGS) $(LIBS) -o $@
	@install_name_tool -delete_rpath $(NETCDFDIR)/lib $@ 2>/dev/null || true

test: $(TEST_EXES)

%: test/%.f90 $(LIBNAME)
	$(FCMP) $(FFLAGS) $(INC) $(MODS) $(CFLAGS) $< \
	    -L. -lheatlink -L../phys -lphys -L../common -lcommon \
	    $(LDFLAGS) $(LIBS) -o $@
	@install_name_tool -delete_rpath $(NETCDFDIR)/lib $@ 2>/dev/null || true

clean:
	${RM} -rf *.o *.mod *.a *~ *.s *.optrpt *.dSYM $(TEST_EXES)
