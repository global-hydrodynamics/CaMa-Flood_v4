##################################################
include   ../../../adm/Mkinclude
##################################################
TARGETS=\
igetday \
day_clim \
ave_clim \
mod_runoff

all: $(TARGETS)

clean:
	$(RM) -rf *.o *.s core *~ *trace *.mod *.dSYN $(TARGETS)

.SUFFIXES : .F90
.F90:
	$(FC) $(FFLAGS) $(LFLAG) $(INC) $^ -o $@

