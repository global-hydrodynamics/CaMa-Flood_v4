include ../../adm/Mkinclude

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
.SUFFIXES: .o .F90 .f90
%.o: %.F90
	$(FCMP) -c $(FFLAGS) $(INC) $(MODS) $*.F90 -o $*.o
%.o: %.f90
	$(FCMP) -c $(FFLAGS) $(INC) $(MODS) $*.f90 -o $*.o

$(libname): $(LIBOBJS)
	ar rcs $@ $^

.PHONY: all clean
all: $(libname)

clean:
	$(RM) -rf *.o *.mod *.a *~ *.s *.optrpt *.dSYM $(TEST_EXES)
