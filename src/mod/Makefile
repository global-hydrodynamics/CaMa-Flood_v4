TOP := $(abspath ../..)
include $(TOP)/adm/Mkinclude

LIBNAME = libmod.a

TOPDIR ?= ..
MODDIRS = \
	-I$(TOPDIR)/common \
	-I$(TOPDIR)

INC  += $(MODDIRS)
MODS += $(MODDIRS)

OBJS = \
	glob_mod.o \
	camaframe_mod.o \
	inpmat_mod.o \
	dim_converter.o

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

# Synthetic fixtures and compilation are isolated in a temporary directory.
.PHONY: test-input-mapping
test-input-mapping:
	python3 test/run_input_mapping_tests.py --fc "$(FCMP)" $(INPUT_MAPPING_TEST_FLAGS)
