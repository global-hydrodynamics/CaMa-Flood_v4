#==========================================================
#* Makefile for stand-alone CaMa-Flood 
#
# (C) D.Yamazaki & E. Dutra  (U-Tokyo/FCUL)  Aug 2019
#
# Licensed under the Apache License, Version 2.0 (the "License");
#   You may not use this file except in compliance with the License.
#   You may obtain a copy of the License at: http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is 
#  distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
# See the License for the specific language governing permissions and limitations under the License.
#==========================================================
include   ../adm/Mkinclude

#=============================
#*** module
YOS_OBJS = \
	parkind1.o      \
	yos_cmf_input.o \
	yos_cmf_time.o  \
	yos_cmf_map.o   \
	yos_cmf_prog.o  \
	yos_cmf_diag.o  \
	cmf_utils_mod.o

CALC_OBJS = \
	cmf_calc_outflw_mod.o \
	cmf_calc_pthout_mod.o \
	cmf_calc_fldstg_mod.o \
	cmf_calc_stonxt_mod.o \
	cmf_calc_diag_mod.o   \
	cmf_opt_outflw_mod.o

CTRL_OBJS = \
	cmf_ctrl_mpi_mod.o      \
	cmf_ctrl_damout_mod.o   \
	cmf_ctrl_tracer_mod.o   \
	cmf_ctrl_levee_mod.o    \
	cmf_ctrl_forcing_mod.o  \
	cmf_ctrl_boundary_mod.o \
	cmf_ctrl_output_mod.o   \
	cmf_ctrl_restart_mod.o  \
	cmf_ctrl_physics_mod.o  \
	cmf_ctrl_time_mod.o     \
	cmf_ctrl_maps_mod.o     \
	cmf_ctrl_vars_mod.o     \
	cmf_ctrl_nmlist_mod.o

DRV_OBJS = \
	cmf_drv_control_mod.o   \
	cmf_drv_advance_mod.o 

OBJECTS = $(YOS_OBJS) $(CALC_OBJS) $(CTRL_OBJS) $(DRV_OBJS)

INC += -Ilib

#=============================
#*** suffix rule
.SUFFIXES : .o .F90
.F90.o:
	$(FCMP) ${FFLAGS} -c ${INC} $(MODS) ${CFLAGS} -c $<
.f90.o:
	$(FCMP) ${FFLAGS} -c ${INC} $(MODS) ${CFLAGS} -c $<

#=============================
EXT_LIBS := $(strip $(EXT_LIBS))
EXT_LIB_TARGETS := $(foreach d,$(EXT_LIBS),$(d)/lib$(d).a)
EXT_LIB_AR      := $(EXT_LIB_TARGETS)
$(EXT_LIB_TARGETS): $(YOS_OBJS)
	@d=$(@D); \
	$(MAKE) -C $$d all

#=============================
#*** build steps
$(EXT_LIB_TARGETS):
	@d=$(@D); \
	f=$(@F); \
	$(MAKE) -C $$d all

yos: $(YOS_OBJS)
ext-libs: yos $(EXT_LIB_TARGETS)
calc: ext-libs $(CALC_OBJS)
ctrl: calc $(CTRL_OBJS)
drv: ctrl $(DRV_OBJS)
src: drv
	ar -rv libcama.a $(OBJECTS)

#*** for main program
TARGET = MAIN_cmf
$(TARGET): $(OBJECTS) src $(TARGET).o $(EXT_LIB_AR)
	$(FCMP) $(FFLAGS) $(LFLAGS) $@.o $(OBJECTS) $(EXT_LIB_AR) -o $@ $(LIB)

# bug in MacOSX libcama.a does not work
#	$(FCMP) $(FFLAGS) $(LFLAGS) $@.o libcama.a -o $@ $(LIB)

#=============================
#*** general rule
all: $(TARGET)
clean:
	${RM} -rf *.o core *~ *trace temp* *.mod *.a *.s *.optrpt *.dSYM $(TARGET)
	@for d in $(EXT_LIBS); do \
		if [ -d $$d ]; then \
			$(MAKE) -C $$d clean; \
		fi; \
	done
