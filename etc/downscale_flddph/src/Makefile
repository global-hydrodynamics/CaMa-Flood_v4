##################################################
include   ../../../adm/Mkinclude
##################################################
TARGETS=\
igetday \
conv_slp \
conv_slp_dur \
conv_flood \
downscale_flddph \
downscale_fldprd \
calc_depth-duration

all: $(TARGETS)

clean:
	$(RM) -rf *.o *.s core *~ *trace *.mod *.dSYN $(TARGETS)

.SUFFIXES : .F90
.F90:
	$(FC) $(FFLAGS) $(LFLAG) $(INC) $^ -o $@

