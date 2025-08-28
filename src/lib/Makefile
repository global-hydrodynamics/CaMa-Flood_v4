include ../../adm/Mkinclude

libname = liblib.a

LIBOBJS = \
text_lib.o \
funit_lib.o \
bin_lib.o \
nc_lib.o \
mapframe_lib.o \
util_lib.o \
LU.o \
layer_lib.o

.SUFFIXES: .o .F90 .f90
%.o: %.F90
	$(FCMP) -c ${FFLAGS} ${INC} $(MODS) $*.F90 -o $*.o
%.o: %.f90
	$(FCMP) -c ${FFLAGS} ${INC} $(MODS) $*.f90 -o $*.o

$(libname): $(LIBOBJS)
	ar rcs $@ $^

.PHONY: all clean
all: $(libname)

clean:
	${RM} -f *.o *.mod *.a *~ *.s *.optrpt *.dSYM