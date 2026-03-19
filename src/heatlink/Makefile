TOP := $(abspath ../..)
include $(TOP)/adm/Mkinclude

LIBNAME = libheatlink.a

OBJS = \
	heatlink_river_mod.o

.PHONY: all clean
all: $(LIBNAME)
$(LIBNAME): $(OBJS)
	@rm -f $@
	ar -rv $@ $(OBJS)

clean:
	${RM} -f *.o *.mod *.a *~ *.s *.optrpt *.dSYM