##################################################
include   ../../../adm/Mkinclude
##################################################
TARGETS=\
igetday \
conv_slp \
conv_flood \
calc_depth-duration \
downscale_fldprd

all: $(TARGETS)

clean:
	$(RM) -rf *.o *.s core *~ *trace *.mod *.dSYN $(TARGETS)

.SUFFIXES : .F90
.F90:
	$(FC) $(FFLAGS) $(LFLAG) $(INC) $^ -o $@

