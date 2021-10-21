##################################################
include   ../../../adm/Mkinclude
##################################################
TARGETS=\
generate_inpmat \
calc_outclm \
calc_rivwth \
set_gwdlr \
set_bifparam \
wrte_ctl_map \
get_rivinfo \
get_vecmap \
get_upbasin \
get_subbasin \
set_bif_basin \
set_mpi_region

all: $(TARGETS)

clean:
	$(RM) -rf *.o *.s core *~ *trace *.mod *.dSYN $(TARGETS)

.SUFFIXES : .F90
.F90:
	$(FC) $(FFLAGS) $(LFLAG) $(INC) $^ -o $@

