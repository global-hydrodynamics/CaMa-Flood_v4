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
OBJECTS=\
parkind1.o \
yos_cmf_input.o   \
yos_cmf_time.o    \
yos_cmf_map.o     \
yos_cmf_prog.o    \
yos_cmf_diag.o    \
cmf_utils_mod.o   \
\
cmf_calc_outflw_mod.o   \
cmf_calc_pthout_mod.o   \
cmf_calc_fldstg_mod.o   \
cmf_calc_stonxt_mod.o   \
cmf_calc_diag_mod.o     \
cmf_opt_outflw_mod.o    \
\
cmf_ctrl_damout_mod.o \
cmf_ctrl_forcing_mod.o  \
cmf_ctrl_boundary_mod.o \
cmf_ctrl_output_mod.o   \
cmf_ctrl_restart_mod.o  \
cmf_ctrl_physics_mod.o  \
cmf_ctrl_time_mod.o     \
cmf_ctrl_maps_mod.o     \
cmf_ctrl_vars_mod.o     \
cmf_ctrl_nmlist_mod.o   \
\
cmf_drv_control_mod.o   \
cmf_drv_advance_mod.o 

#=============================
#*** suffix rule
.SUFFIXES : .o .F90
.F90.o:
	$(FCMP) ${FFLAGS} -c ${INC} $(MODS) ${CFLAGS} -c $<
src: $(OBJECTS)
	ar -rv libcama.a $(OBJECTS)

#=============================
#*** for main program
TARGET = MAIN_cmf
$(TARGET): $(OBJECTS) src $(TARGET).o
	$(FCMP) $(FFLAGS) $(LFLAGS) $@.o $(OBJECTS) -o $@ $(LIB)

# bug in MacOSX libcama.a does not work
#	$(FCMP) $(FFLAGS) $(LFLAGS) $@.o libcama.a -o $@ $(LIB)

#=============================
#*** general rule
all: $(TARGET)
clean:
	${RM} -rf *.o core *~ *trace temp* *.mod *.a *.s *.dSYM
cleanall:
	${RM} -rf *.o core *~ *trace temp* *.mod *.a *.s *.dSYM $(TARGET)

