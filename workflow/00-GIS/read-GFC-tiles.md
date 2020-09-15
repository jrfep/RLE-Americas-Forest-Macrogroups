
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


```
