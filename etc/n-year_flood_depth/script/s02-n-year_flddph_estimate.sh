#!/bin/sh
# calculate the flddph at different return period using the parameters estimated for the various fitting distribution.
# ./s02-n-year_flddph_estimate.sh $CAMA_FOLDER $YEARS $YEARE $RES $EXPNAME $WEST $EAST $SOUTH $NORTH $NGRID $MAXDPH

CAMA_FOLDER=$1
MAPDIR=$2

YEARS=$3
YEARE=$4
RES=$5
EXPNAME=$6
NGRID=$7
MAXDPH=$8

echo "@@@@@ s02-n-year_flddph_estimate.sh @@@@@"


VARS=`cat vars.txt`
GLBNAMES=`cat glbnames.txt`
FUNCS=`cat funcs.txt`
RPS=`cat rps.txt`
DOMAIN=`cat domain.txt`

for VAR in $VARS
do 

    for GLBNAME in $GLBNAMES
    do
        INPDIR=$CAMA_FOLDER'/out/'${GLBNAME} # input directory

        EXPNAME=$EXPNAME-$RES

        norm=''

        #OUTDIR="./${EXPNAME}" # output directory
        #OUTDIR="./out"
        if [ $VAR = 'rivdph' ] ; then
            OUTDIR="./../result"$norm"/"${GLBNAME}"/"
        else
            OUTDIR="./../result"$norm"/"${GLBNAME}"/STO2DPH"
        fi


        mkdir -p $OUTDIR
        ln -snf $MAPDIR ${OUTDIR}/map
        ln -snf $MAPDIR map
        ln -snf $INPDIR ${OUTDIR}/inp

        ##### Read x/y size of input map ################
        XSIZE=$(head -n 1 "${MAPDIR}/params.txt" | awk '{print $1}') # xsize of input data
        YSIZE=$(head -n 2 "${MAPDIR}/params.txt" | tail -n 1 | awk '{print $1}') # ysize of input data

        echo "\nYEARS=${YEARS}, YEARE=${YEARE}, YSIZE=${YSIZE}, XSIZE=${XSIZE}"
        echo "INPDIR=${INPDIR}"
        echo "MAPDIR=${MAPDIR}"

        ##### Main calculation ##########################

        for RP in $RPS
        do
            ####
            for fun in $FUNCS
            do 
                echo $GLBNAME $RP $fun
                if [ "$VAR" = "storge" ] ; then
                    echo '\n\n### calculate n-year flood storage with the parameters'
                    mkdir -p ${OUTDIR}/Nyear_storge
                    python ./src/rp2storge_dis.py $YEARS $YEARE $YSIZE $XSIZE $OUTDIR $VAR $RP $fun

                    echo '\n\n### convert the storge to rivdph and flddph'
                    mkdir -p ${OUTDIR}/Nyear_flddph
                    python ./src/sto2dph.py $YEARS $YEARE $YSIZE $XSIZE $OUTDIR $RP $fun

                    FLDDPH="${OUTDIR}/Nyear_flddph/sto2flddph_RP${RP}_${fun}.bin"

                else

                    echo '\n\n### calculate n-year flood depth (flddph) with the parameters'
                    mkdir -p ${OUTDIR}/Nyear_flddph
                    python ./src/rp2flddph_dis.py $YEARS $YEARE $YSIZE $XSIZE $OUTDIR $VAR $RP $fun

                    FLDDPH="${OUTDIR}/Nyear_flddph/flddph_RP${RP}_${fun}.bin"

                fi 

                #echo '\n### downscale flood depth ###'
                mkdir -p ${OUTDIR}/${EXPNAME}
                mkdir -p ${OUTDIR}/${EXPNAME}/downscaled_flddph

                FFLOOD="${OUTDIR}/${EXPNAME}/downscaled_flddph/${EXPNAME}_RP${RP}_${RES}_${fun}.bin"
                FFLOODM="${OUTDIR}/${EXPNAME}/downscaled_flddph/${EXPNAME}_RP${RP}_${RES}_mask_${fun}.bin"
                echo "\n\n### starting downscale_nflddph"
                ./src/downscale_nflddph $DOMAIN $RES $FLDDPH $FFLOOD $FFLOODM $OUTDIR

                echo '\n\n### print the downscaled maps'
                # visualization using PyThon
                mkdir -p ${OUTDIR}/${EXPNAME}/floodrisk_figure
                ./t02-draw_flddph.sh $DOMAIN $RES $NGRID $MAXDPH $OUTDIR/${EXPNAME} $FFLOOD $RP $fun 
            done 

        done

    done
done

rm -f map
rm -f ele.bin
