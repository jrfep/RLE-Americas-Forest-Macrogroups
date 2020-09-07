#!/bin/bash
export MCDG = $@
g.region -dp
g.region vect=$MCDG -p
export VRS=GFC-2019-v1.7
mkdir -p $WORKDIR/$VRS

eval `g.region -g`
eval `g.region -bg`

for VAR in gain lossyear treecover2000
do
  if [ ! -e $WORKDIR/$VRS/${VRS}.${MCDG}.${VAR}.tif ]
  then
    gdalwarp -t_srs "+proj=robin +lon_0=-80 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" -te $w $s $e $n -co "COMPRESS=LZW" $GISDB/extra-gisdata/sensores/Landsat/index_${VRS}_${VAR}.vrt $WORKDIR/$VRS/${VRS}.${MCDG}.${VAR}.tif
  fi
    r.in.gdal input=$WORKDIR/$VRS/${VRS}.${MCDG}.${VAR}.tif output=${VAR}
done


r.mapcalc expression="MCD12Q1.bsq=if(MCD12Q1.2001@PERMANENT<6,1,0) + if(MCD12Q1.2002@PERMANENT<6,1,0) +if(MCD12Q1.2003@PERMANENT<6,1,0) +if(MCD12Q1.2004@PERMANENT<6,1,0) +if(MCD12Q1.2005@PERMANENT<6,1,0) +if(MCD12Q1.2006@PERMANENT<6,1,0) +if(MCD12Q1.2007@PERMANENT<6,1,0) +if(MCD12Q1.2008@PERMANENT<6,1,0) +if(MCD12Q1.2009@PERMANENT<6,1,0) +if(MCD12Q1.2010@PERMANENT<6,1,0) +if(MCD12Q1.2011@PERMANENT<6,1,0) +if(MCD12Q1.2012@PERMANENT<6,1,0) +if(MCD12Q1.2013@PERMANENT<6,1,0) +if(MCD12Q1.2014@PERMANENT<6,1,0) +if(MCD12Q1.2015@PERMANENT<6,1,0) +if(MCD12Q1.2016@PERMANENT<6,1,0) +if(MCD12Q1.2017@PERMANENT<6,1,0) +if(MCD12Q1.2018@PERMANENT<6,1,0) +if(MCD12Q1.2019@PERMANENT<6,1,0)"


g.region rast=treecover2000 -p
r.stats -acn ${MCDG},MCD12Q1.bsq,treecover2000,lossyear,gain > ${MCDG}-MCD12Q1-GFC-lossgain.tab
