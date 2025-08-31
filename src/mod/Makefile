TOP := $(abspath ../..)
include $(TOP)/adm/Mkinclude

LIBNAME = libmod.a

OBJS = \
	glob_mod.o

.PHONY: all clean
all: $(LIBNAME)
$(LIBNAME): $(OBJS)
	@rm -f $@
	ar -rv $@ $(OBJS)

clean:
	${RM} -f *.o *.mod *.a *~ *.s *.optrpt *.dSYM