MKINCLUDE ?= ../../adm/Mkinclude
include $(MKINCLUDE)

libname = libphys.a

TOPDIR ?= ..
MODDIRS = \
    -I$(TOPDIR)/common \
    -I$(TOPDIR)/mod \
    -I$(TOPDIR)

INC  += $(MODDIRS)
MODS += $(MODDIRS)

LIBOBJS = \
	phys_const_mod.o \
	water_mod.o \
	channel_mod.o \
	heat_flux_mod.o \
	heat_budget_mod.o
#eddy_mod.o
#sed_mod

TESTS = \
	test_heat_budget
TEST_EXES = $(TESTS)
.SUFFIXES: .o .F90 .f90
%.o: %.F90
	$(FCMP) -c $(FFLAGS) $(INC) $(MODS) $*.F90 -o $*.o
%.o: %.f90
	$(FCMP) -c $(FFLAGS) $(INC) $(MODS) $*.f90 -o $*.o

$(libname): $(LIBOBJS)
	ar rcs $@ $^

.PHONY: all clean test
all: $(libname)

test: $(TEST_EXES)

%: test/%.f90 $(libname)
	$(FCMP) $(FFLAGS) $(INC) $(MODS) $(CFLAGS) $< \
	    -L. -lphys -L../common -lcommon \
	    $(LDFLAGS) $(LIBS) -o $@
	@install_name_tool -delete_rpath $(NETCDFDIR)/lib $@ 2>/dev/null || true

clean:
	$(RM) -rf *.o *.mod *.a *~ *.s *.optrpt *.dSYM $(TEST_EXES)
