#!/bin/sh
# Estimate the parameters for each combination of runoff, variable and fitting distribution
# ./s01-n-year_para_estimate.sh  $CAMA_FOLDER $YEARS $YEARE $RES
CAMA_FOLDER=$1
MAPDIR=$2
YEARS=$3
YEARE=$4
RES=$5

echo "@@@@@ s01-n-year_para_estimate.sh @@@@@"

VARS=`cat vars.txt`
GLBNAMES=`cat glbnames.txt`
FUNCS=`cat funcs.txt`
RPS=`cat rps.txt`

for VAR in $VARS
do
    for GLBNAME in $GLBNAMES
    do
        echo "\n\n\n####### $GLBNAME $VAR ######"

        INPDIR=$CAMA_FOLDER'/out/'${GLBNAME} # input directory

        # '' > no normorlization
        # "
        norm=''

        # define the output folder
        if [ $VAR = 'rivdph' ] ; then
            OUTDIR="./../result"$norm"/"${GLBNAME}"/"
        else
            OUTDIR="./../result"$norm"/"${GLBNAME}"/STO2DPH"
        fi

        mkdir -p $OUTDIR
        ln -snf $MAPDIR ${OUTDIR}/map
        ln -snf $INPDIR ${OUTDIR}/inp

        # calculate the x,y information from the map parameters
        XSIZE=$(head -n 1 "${MAPDIR}/params.txt" | awk '{print $1}') # xsize of input data
        YSIZE=$(head -n 2 "${MAPDIR}/params.txt" | tail -n 1 | awk '{print $1}') # ysize of input data

        echo "\nYEARS=${YEARS}, YEARE=${YEARE}, YSIZE=${YSIZE}, XSIZE=${XSIZE}"
        echo "INPDIR=${INPDIR}"
        echo "MAPDIR=${MAPDIR}"


        ##### Main calculation ##########################

        echo '\n### calculate annual maximum value ###' $OUTDIR $VAR
        echo "python ./src/annual_max.py $YEARS $YEARE $YSIZE $XSIZE $OUTDIR $VAR"
        mkdir -p ${OUTDIR}/amax
        python ./src/annual_max.py $YEARS $YEARE $YSIZE $XSIZE $OUTDIR $VAR

        ####
        for fun in $FUNCS
        do 
            echo '\n### calculate and store the parameter, also the statistics for the fitting###'

            if [ $VAR = 'storge' ] ; then
                paranum=`ls './../result/'$GLBNAME/STO2DPH/para/$fun* 2> /dev/null | grep $fun | wc -l `
            else
                paranum=`ls './../result/'$GLBNAME/para/$fun* 2> /dev/null | grep $fun | wc -l `
            fi

            echo $paranum

            if [ $paranum -gt 0 ]; then
                echo "$GLBNAME $fun exists"
                echo "1 python ./src/calc_distributions.py $YEARS $YEARE $YSIZE $XSIZE $OUTDIR $VAR $fun $norm"
                python ./src/calc_distributions.py $YEARS $YEARE $YSIZE $XSIZE $OUTDIR $VAR $fun $norm #&

            else
                echo $GLBNAME $fun 
                mkdir -p ${OUTDIR}/para
                echo "2 python ./src/calc_distributions.py $YEARS $YEARE $YSIZE $XSIZE $OUTDIR $VAR $fun $norm"
                python ./src/calc_distributions.py $YEARS $YEARE $YSIZE $XSIZE $OUTDIR $VAR $fun $norm #&

            # controlling of the parallization.
            #NUM1=`ps aux | grep calc_distributions  | wc -l | awk '{print $1}'`
            #while [ $NUM1 -gt 18 ];
            #do
            #  sleep 30
            #  NUM1=`ps aux | grep calc_distributions  | wc -l | awk '{print $1}'`
            #done
            fi 

        done

    done

done
