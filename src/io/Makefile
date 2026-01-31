include ../../adm/Mkinclude

libname = libio.a

TOPDIR ?= ..
MODDIRS = \
    -I$(TOPDIR)/lib \
    -I$(TOPDIR)/mod \
    -I$(TOPDIR)

INC  += $(MODDIRS)
MODS += $(MODDIRS)

ifneq ($(strip $(LIBDIR)),)
LDFLAGS += -L$(LIBDIR)
endif

LIBOBJS = \
	key_table_class.o \
    ranked_array_class.o \
	accumulated_array_class.o \
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

#------------------------------------------------------------
# tests
#------------------------------------------------------------
TESTS = \
	test_numeric_utils \
	test_key_table \
	test_ranked_array
	
TEST_SRCS = $(addprefix test/,$(addsuffix .f90,$(TESTS)))
TEST_EXES = $(TESTS)

.PHONY: all clean test
all: $(libname)

test: $(TEST_EXES)

%: test/%.f90 $(libname)
	$(FCMP) $(FFLAGS) $(INC) $(MODS) $< \
	    -L. -lio \
	    -L$(TOPDIR)/lib -llib \
	    -L$(TOPDIR)/mod -lmod \
	    $(LDFLAGS) $(LIBS) -o $@
	@install_name_tool -delete_rpath $(NETCDFDIR)/lib $@ 2>/dev/null || true

clean:
	$(RM) -rf *.o *.mod *.a *~ *.s *.optrpt *.dSYM $(TEST_EXES)