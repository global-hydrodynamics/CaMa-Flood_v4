include ../../adm/Mkinclude

libname = libio.a

TOPDIR ?= ..
MODDIRS = \
    -I$(TOPDIR)/common \
    -I$(TOPDIR)/mod \
    -I$(TOPDIR)

INC  += $(MODDIRS)
MODS += $(MODDIRS)

#ifneq ($(strip $(LIBDIR)),)
#LDFLAGS += -L$(LIBDIR)
#endif

LIBOBJS = \
    io_namelist_mod.o \
	output_conf_class.o \
	output_writer_class.o \
	output_mod.o \
	input_conf_class.o \
	input_mod.o

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