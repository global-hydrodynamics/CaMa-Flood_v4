include ../../adm/Mkinclude

LIBNAME = libcommon.a
TOPDIR ?= ..
MODDIRS = \
    -I$(TOPDIR)

INC  += $(MODDIRS)
MODS += $(MODDIRS)

OBJS = \
	const_mod.o \
	text_mod.o \
	funit_mod.o \
	numeric_utils_mod.o \
	array_mod.o \
	key_table_class.o \
    ranked_array_class.o \
	accumulated_array_class.o \
	time_mod.o \
	time_recorder_class.o \
	bin_mod.o \
	nc_mod.o \
	mapframe_mod.o \
	util_mod.o \
	LU.o
#layer_mod.o

array_mod.o: FFLAGS += -Wno-uninitialized -Wno-maybe-uninitialized

.SUFFIXES: .o .F90 .f90
%.o: %.F90
	$(FCMP) -c $(FFLAGS) $(INC) $(MODS) $*.F90 -o $*.o
%.o: %.f90
	$(FCMP) -c $(FFLAGS) $(INC) $(MODS) $*.f90 -o $*.o

$(libname): $(OBJS)
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
all: $(LIBNAME)
$(LIBNAME): $(OBJS)
	@rm -f $@
	ar -rv $@ $(OBJS)

clean:
	$(RM) -rf *.o *.mod *.a *~ *.s *.optrpt *.dSYM $(TEST_EXES)

test: $(TEST_EXES)

%: test/%.f90 $(LIBNAME)
	$(FCMP) $(FFLAGS) $(INC) $(MODS) $< \
	    -L. -lcommon \
	    $(LDFLAGS) $(LIBS) -o $@
	@install_name_tool -delete_rpath $(NETCDFDIR)/lib $@ 2>/dev/null || true
