##################################################
include   ../../../adm/Mkinclude
##################################################
TARGETS=\
analysis_output \
calc_levparam \
conv_flood_levee \
define_protect_pix \
downscale_flddph_levee \

all: $(TARGETS)

clean:
	$(RM) -rf *.o *.s core *~ *trace *.mod *.dSYN $(TARGETS)

.SUFFIXES : .F90
.F90:
	$(FC) $(FFLAGS) $(LFLAG) $(INC) $^ -o $@

