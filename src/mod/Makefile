TOP := $(abspath ../..)
include $(TOP)/adm/Mkinclude

LIBNAME = libmod.a

OBJS = \
	glob_mod.o \
	camaframe_mod.o \
	inpmat_mod.o \
	dim_converter.o

.PHONY: all clean
all: $(LIBNAME)
$(LIBNAME): $(OBJS)
	@rm -f $@
	ar -rv $@ $(OBJS)

clean:
	${RM} -f *.o *.mod *.a *~ *.s *.optrpt *.dSYM