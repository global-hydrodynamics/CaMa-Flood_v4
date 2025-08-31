include ../../adm/Mkinclude

LIBNAME = liblib.a

OBJS = \
	text_lib.o \
	funit_lib.o \
	array_lib.o \
	time_recorder_class.o \
	bin_lib.o \
	nc_lib.o \
	mapframe_lib.o \
	util_lib.o \
	LU.o \
	layer_lib.o

.PHONY: all clean
all: $(LIBNAME)
$(LIBNAME): $(OBJS)
	@rm -f $@
	ar -rv $@ $(OBJS)

clean:
	@find . -maxdepth 1 -name "*.o" -delete
	@find . -maxdepth 1 -name "*.mod" -delete
	@rm -f liblib.a