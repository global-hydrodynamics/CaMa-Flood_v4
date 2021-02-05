#!/bin/bash


#*** PBS setting when needed
#PBS -N RPA
#PBS -q E20
#PBS -l select=1:ncpus=20:mem=50gb
#PBS -j oe
#PBS -m ea
#PBS -V

# 
cd $PBS_O_WORKDIR
# calculate N-year flddph by converting storage to flddph
# the distrition is working on the storage rather than the rivdph
# But since we cannot get the flddph directly from the parameters, 
# the storage at N-year period should be estimated first and then 
# to be converted to N-year flddph

##### Settings #################################

VAR='rivdph storge' # variable in CaMa-Flood

# Where the CaMa-Flood is 
CAMA_FOLDER="../../"

# If there are multiple simulations.
GLBNAMES="e2o_anu  e2o_cnrs  e2o_jrc  e2o_nerc  e2o_ecmwf  e2o_univk  e2o_univu"

# The Experiment name for the region 
# Note that the downscaling for the global region is infesible at 3sec. 
# Mekong
EXPNAME='Mekong' # name of hazard map
WEST=102 # target downscale domain
EAST=108
SOUTH=9
NORTH=15

YEARS=1980	# start year 
YEARE=2014	# end year
#

RES=3sec # downscale resolution (3sec, 15sec, 1min), a hires map should exist in mapdir
NGRID=1  
MAXDPH=11

#
# *Exponential (EXP)
# *Gamma (GAM)
# *Generalised Extreme Value (GEV)
# *Generalised Logistic (GLO)
# *Generalised Normal (GNO)
# *Generalised Pareto (GPA)
# *Gumbel (GUM)
# *Kappa (KAP)
# *Normal (NOR)
# *Pearson III (PE3)
# *Wakeby (WAK)
# *Weibull (WEI)

# define the fitting functions 
FUNC="GEV GAM PE3 GUM WEI WAK"

# define the return period
RPS="0.001 0.005 0.01 0.02 0.1 0.2 0.25 0.4 0.5 0.6 0.75 0.8 0.9 0.98 0.99 0.995 0.999"

# calculate the paramters for distribution fitting  
./s01-n-year_para_estimate.sh  $VAR $CAMA_FOLDER $GLBNAMES $FUNC $YEARS $YEARE $RES

# calculate the flood water depth at different return period (RP)
./s02-n-year_flddph_estimate.sh  $VAR $CAMA_FOLDER $GLBNAMES $FUNC $RPS $YEARS $YEARE $RES $EXPNAME $WEST $EAST $SOUTH $NORTH $NGRID $MAXDPH


