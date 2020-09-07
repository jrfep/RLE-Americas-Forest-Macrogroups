# Data sources

## NatureServe IVC maps
 The original files of the potential distribution of the Macrogroups in geotiff format were delivered by NatureServe.

 http://hub.arcgis.com/datasets/b25fa8f7673749fc85e0ba7980374c5f
 http://hub.arcgis.com/datasets/Natureserve::southamerica-ivc-macrogroups-potential-natureserve-v1

 Extraction: files were encoded in .lpk format, I extracted them using the 7z extraction command

```sh
cp $GISDATA/ecosistemas/NatureServe/*potential*tif.lpk $WORKDIR
cp $NSDATA/NorthAmerica_Caribbean_IVC_MacroGroups_potential_NatureServe_v5_270m_tif.lpk $WORKDIR
cd $WORKDIR
7z x SouthAmerica_IVC_MacroGroups_potential_NatureServe_v7_270m_tif.lpk
7z x NorthAmerica_Caribbean_IVC_MacroGroups_potential_NatureServe_v5_270m_tif.lpk
rm *lpk
```

 The layer package contains different files, raster are located in folder commondata/raster_data/

 Original projections: files differed in the original projection, the South America file also includes a table with raster attributes

```sh
gdalinfo commondata/raster_data/NorthAmerica_Caribbean_IVC_MacroGroups_potential_NatureServe_v5_270m.tif
gdalinfo commondata/raster_data/SouthAmerica_IVC_MacroGroups_potential_NatureServe_v7_270m.tif | less
```

 Reprojection: In order to combine both layers in one file with a common projection, I used gdalwarp. Create option COMPRESS=LZW allows for lossless data compression.  I chose the robin projection for the whole continent. Output file is IVC_NS_v7_270m_robin.tif


```sh
gdalwarp -co "COMPRESS=LZW" -t_srs '+proj=robin +lon_0=-80 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0' commondata/raster_data/NorthAmerica_Caribbean_IVC_MacroGroups_potential_NatureServe_v5_270m.tif commondata/raster_data/SouthAmerica_IVC_MacroGroups_potential_NatureServe_v7_270m.tif IVC_NS_v7_270m_robin.tif
rm -r commondata/ esriinfo/ v10 v103/
```

# Create the GRASS GIS location

```sh
conda deactivate
cd $WORKDIR
grass -c IVC_NS_v7_270m_robin.tif $GISDB/IVC
r.in.gdal input="IVC_NS_v7_270m_robin.tif" output="IVC_NS_v7" memory=300 offset=0 num_digits=0
exit
```

```sh

conda deactivate
cd $WORKDIR
#for k in $(psql -At -d IUCN -c "select value,mg_key from ivc_americas where formation_ IN ('1.A.1','1.A.2','1.A.3','1.A.4','1.A.5','1.B.1','1.B.2','1.B.3') order by ivc_format,mg_key")
nohup grass $GISDB/IVC/PERMANENT --exec bash $SCRIPTDIR/workflow/00-GIS/split-macrogroup-mapsets.sh "'1.A.5'" &
nohup grass $GISDB/IVC/PERMANENT --exec bash $SCRIPTDIR/workflow/00-GIS/split-macrogroup-mapsets.sh "'1.A.4'" &
nohup grass $GISDB/IVC/PERMANENT --exec bash $SCRIPTDIR/workflow/00-GIS/split-macrogroup-mapsets.sh "'1.B.1'" > nohupB1.out&
nohup grass $GISDB/IVC/PERMANENT --exec bash $SCRIPTDIR/workflow/00-GIS/split-macrogroup-mapsets.sh "'1.B.2'" > nohupB2.out&
nohup grass $GISDB/IVC/PERMANENT --exec bash $SCRIPTDIR/workflow/00-GIS/split-macrogroup-mapsets.sh "'1.A.1'" > nohupA1.out&
nohup grass $GISDB/IVC/PERMANENT --exec bash $SCRIPTDIR/workflow/00-GIS/split-macrogroup-mapsets.sh "'1.B.3'" > nohupB3.out&

nohup grass $GISDB/IVC/PERMANENT --exec bash $SCRIPTDIR/workflow/00-GIS/split-macrogroup-mapsets.sh "'1.A.2'" > nohupA2.out&

nohup grass $GISDB/IVC/PERMANENT --exec bash $SCRIPTDIR/workflow/00-GIS/split-macrogroup-mapsets.sh "'1.A.3'" > nohupA3.out&

du -sch $GISDB/IVC/*


```

Data in Virtual Raster Tilesets (VRT).

```sh
cd $WORKDIR
conda deactivate
grass $GISDB/IVC/PERMANENT
g.region -dp

eval `g.region -g`
eval `g.region -bg`


export VAR=MCD12Q1
export VRS=006
mkdir -p $WORKDIR/$VAR.$VRS/

for YEAR in $(seq 2001 2019)
do
  export FECHA=${YEAR}.01.01
  gdalwarp -t_srs "+proj=robin +lon_0=-80 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" -te $w $s $e $n -srcnodata 255 -dstnodata 255 -of VRT $GISDATA/sensores/Modis/MCD12Q1.006/index_${VAR}_${VRS}_${FECHA}_LC_Type1.vrt $WORKDIR/$VAR.$VRS/${VAR}.${YEAR}.vrt

  # if [ ! -e $WORKDIR/$VAR.$VRS/${VAR}.${YEAR}.tif ]
  # then
  #   gdalwarp -t_srs "+proj=robin +lon_0=-80 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" -te $w $s $e $n -srcnodata 255 -dstnodata 255 -co "COMPRESS=LZW" $GISDATA/sensores/Modis/MCD12Q1.006/index_${VAR}_${VRS}_${FECHA}_LC_Type1.vrt $WORKDIR/$VAR.$VRS/${VAR}.${YEAR}.tif
  #   fi
done


export VRS=GFC-2019-v1.7
mkdir -p $WORKDIR/$VRS
export VAR=gain

for VAR in gain lossyear treecover2000
do
  if [ ! -e $WORKDIR/$VRS/${VRS}.${VAR}.vrt ]
  then
    gdalwarp -t_srs "+proj=robin +lon_0=-80 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" -te $w $s $e $n -of VRT $GISDB/extra-gisdata/sensores/Landsat/index_${VRS}_${VAR}.vrt $WORKDIR/$VRS/${VRS}.${VAR}.vrt
  fi
done

exit

```


Using `r.external` works very well for Modis, but does not show much progress with GFC files
```sh
cd $WORKDIR
conda deactivate
grass $GISDB/IVC/PERMANENT
g.region -dp

export YEAR=2007
for YEAR in $(seq 2001 2019)
do
  r.external input=MCD12Q1.006/MCD12Q1.${YEAR}.vrt output=MCD12Q1.${YEAR}
done

```


Using `r.in.gdal` is also very slow at this large scale

```sh

export VRS=GFC-2019-v1.7
for VAR in gain lossyear treecover2000
do
  r.in.gdal input=$WORKDIR/$VRS/${VRS}.${VAR}.vrt output=${VAR}
done

r.in.gdal input=$WORKDIR/$VRS/${VRS}.${VAR}.tif output=${VAR}
r.in.gdal input=$WORKDIR/$VAR.$VRS/${VAR}.${YEAR}.tif output=${VAR}.${YEAR}


```

```sh
# create lat and long raster
 r.mapcalc expression="lons=x()"
 r.mapcalc expression="lats=y()"

# cross IVC with lat and long and extract extremes for bounding box
##r.statistics cover=lons base=IVC_NS_v7 method=min,max
r.stats.zonal base=IVC_NS_v7 cover=lons method=min output=tmp001
r.stats.zonal base=IVC_NS_v7 cover=lons method=max output=tmp002
r.stats.zonal base=IVC_NS_v7 cover=lats method=min output=tmp003
r.stats.zonal base=IVC_NS_v7 cover=lats method=max output=tmp004

r.stats -ai IVC_NS_v7,tmp001,tmp002,tmp003,tmp004 output=MG_latlon_range.txt
```
