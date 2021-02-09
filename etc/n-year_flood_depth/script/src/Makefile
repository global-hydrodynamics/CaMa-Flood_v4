##################################################
include   ../../../../adm/Mkinclude
#include   /cluster/data6/x.zhou/CaMa_v395b/GLB_15min/adm/Mkinclude
##################################################
TARGETS=\
igetday \
conv_slp \
conv_flood \
get_ele \
downscale_nflddph  

all: $(TARGETS)

clean:
	$(RM) -rf *.o *.s core *~ *trace *.mod *.dSYN $(TARGETS)

.SUFFIXES : .F90
.F90:
	$(FC) $(FFLAGS) $(LFLAG) $(INC) $^ -o $@

