##################################################
include   ../../../adm/Mkinclude
##################################################
TARGETS=\
cut_domain \
combine_hires \
generate_inpmat \
set_map \
cut_bifway \
gradsinfo \
wrte_ctl_map \
wrte_ctl_hires \
set_name

all: $(TARGETS)

clean:
	$(RM) -rf *.o *.s core *~ *trace *.mod *.dSYN $(TARGETS)

.SUFFIXES : .F90
.F90:
	$(FC) $(FFLAGS) $(LFLAG) $(INC) $^ -o $@

##################################################

