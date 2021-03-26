##################################################
include   /cluster/data5/hanazaki/CaMa-Flood_v396a_20200514/adm/Mkinclude
##################################################
TARGETS=\
get_rivinfo_glb

all: $(TARGETS)

clean:
	$(RM) -rf *.o *.s core *~ *trace *.mod *.dSYN $(TARGETS)

.SUFFIXES : .F90
.F90:
	$(FC) $(FFLAGS) $(LFLAG) $(INC) $^ -o $@

