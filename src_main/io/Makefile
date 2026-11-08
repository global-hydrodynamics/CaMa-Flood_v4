CONFIG_FILE := $(abspath ../../config/Mkinclude)
ifeq ($(wildcard $(CONFIG_FILE)),)
$(error CaMa-Flood v5 configuration missing: $(CONFIG_FILE). Review config/Mkinclude_gfortran or run ./s01-setup_fortran.sh, then copy ./Mkinclude to config/Mkinclude)
endif
include $(CONFIG_FILE)

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
	input_mod.o \
	restart_mod.o

.SUFFIXES: .o .F90 .f90
%.o: %.F90
	$(FCMP) $(FFLAGS) $(FPPFLAG) $(FCMODFLAG) . -c $(INCDIRS) $(INC) $(MODS) $(CFLAGS) $*.F90 -o $*.o
%.o: %.f90
	$(FCMP) $(FFLAGS) $(FPPFLAG) $(FCMODFLAG) . -c $(INCDIRS) $(INC) $(MODS) $(CFLAGS) $*.f90 -o $*.o

$(libname): $(LIBOBJS)
	ar rcs $@ $^

.PHONY: all clean 
all: $(libname)

clean:
	$(RM) -rf *.o *.mod *.a *~ *.s *.optrpt *.dSYM $(TEST_EXES)

# Compile and run the production input path with disposable synthetic fixtures.
PYTHON ?= python3
.PHONY: test-input-slice
test-input-slice:
	$(PYTHON) test/run_input_slice_tests.py --fc "$(FCMP)" $(INPUT_SLICE_TEST_FLAGS)
