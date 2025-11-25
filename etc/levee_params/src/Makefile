##################################################
include   ../../../adm/Mkinclude
##################################################
TARGETS=\
calc_levfrc \
conv_flood_levee \
define_protect_pix \
downscale_flddph_levee \


all: $(TARGETS)

clean:
	$(RM) -rf *.o *.s core *~ *trace *.mod *.dSYN $(TARGETS)

.SUFFIXES : .F90
.F90:
	$(FC) $(FFLAGS) $(LFLAG) $(INC) $^ -o $@

