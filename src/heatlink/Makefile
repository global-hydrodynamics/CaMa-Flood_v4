TOP := $(abspath ../..)
include $(TOP)/adm/Mkinclude

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
	heatlink_river_mod.o

.SUFFIXES: .o .F90 .f90
%.o: %.F90
	$(FCMP) -c $(FFLAGS) $(INC) $(MODS) $(CFLAGS) $< -o $@
%.o: %.f90
	$(FCMP) -c $(FFLAGS) $(INC) $(MODS) $(CFLAGS) $< -o $@

.PHONY: all clean
all: $(LIBNAME)
$(LIBNAME): $(OBJS)
	@rm -f $@
	ar -rv $@ $(OBJS)

clean:
	${RM} -f *.o *.mod *.a *~ *.s *.optrpt *.dSYM
