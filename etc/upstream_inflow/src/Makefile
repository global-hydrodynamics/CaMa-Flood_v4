##################################################
include   ../../../adm/Mkinclude
##################################################
TARGETS=\
get_mask_upbasin \

all: $(TARGETS)

clean:
	$(RM) -rf *.o *.s core *~ *trace *.mod *.dSYN $(TARGETS)

.SUFFIXES : .F90
.F90:
	$(FC) $(FFLAGS) $(LFLAG) $(CFLAGS) $(INC) $(LIB) $^ -o $@ $(LIB)

