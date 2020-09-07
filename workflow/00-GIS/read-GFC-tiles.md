
```sh

cd $WORKDIR
conda deactivate
export VRS=GFC-2019-v1.7
export VAR=gain
for ARCHS in $(ls $GISDB/extra-gisdata/sensores/Landsat/$VRS/*${VAR}*070W.tif)
do
  export ARCH=$ARCHS
  export LOC=$(basename $ARCH | sed -e s/.tif// -e s/_$VAR// -e s/_$VRS//)
  nohup grass -c $ARCH $GISDB/tmp/$LOC --exec bash $SCRIPTDIR/workflow/00-GIS/read-GFC-tiles.sh > nohup-${LOC}.out & 
done

export ARCH=/opt/gisdb/extra-gisdata/sensores/Landsat/GFC-2019-v1.7/Hansen_GFC-2019-v1.7_gain_10N_070W.tif
export ARCH=/opt/gisdb/extra-gisdata/sensores/Landsat/GFC-2019-v1.7/Hansen_GFC-2019-v1.7_gain_10S_070W.tif
#empty
/opt/gisdb/extra-gisdata/sensores/Landsat/GFC-2019-v1.7/Hansen_GFC-2019-v1.7_gain_80N_180W.tif



  grass $GISDB/tmp/$LOC/PERMANENT --exec r.proj location=IVC mapset=PERMANENT dbase=$GISDB input=IVC_NS_v7 output=IVC
  case ${?} in
    0) {
      grass $GISDB/tmp/$LOC/PERMANENT --exec r.proj location=IVC mapset=PERMANENT dbase=$GISDB input=MCD12Q1.bsq output=MCD12Q1.bsq

      grass $GISDB/tmp/$LOC/PERMANENT --exec r.external input=$ARCH output=$VAR
      for VAR2 in lossyear treecover2000
      do
        grass $GISDB/tmp/$LOC/PERMANENT --exec r.external input=$(echo $ARCH | sed s/$VAR/$VAR2/) output=$VAR2
      done
        grass $GISDB/tmp/$LOC/PERMANENT --exec r.stats -acn IVC,MCD12Q1.bsq,treecover2000,lossyear,gain > ${LOC}-IVC-MCD12Q1-lossgain.tab
    } ;;
    *) {
      echo "outside IVC distribution"
    }
  esac


```
