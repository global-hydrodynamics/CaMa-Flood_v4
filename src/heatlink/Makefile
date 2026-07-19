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
	heatlink_river_mod.o

TESTS = \
	test_water_storage_adapter \
	test_heatlink_input_adapter
TEST_EXES = $(TESTS)

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

heatlink_river_mod.o: water_storage_adapter_mod.o heatlink_input_adapter_mod.o

test: $(TEST_EXES)

%: test/%.f90 $(LIBNAME)
	$(FCMP) $(FFLAGS) $(INC) $(MODS) $(CFLAGS) $< \
	    -L. -lheatlink -L../phys -lphys -L../common -lcommon \
	    $(LDFLAGS) $(LIBS) -o $@
	@install_name_tool -delete_rpath $(NETCDFDIR)/lib $@ 2>/dev/null || true

clean:
	${RM} -rf *.o *.mod *.a *~ *.s *.optrpt *.dSYM $(TEST_EXES)
