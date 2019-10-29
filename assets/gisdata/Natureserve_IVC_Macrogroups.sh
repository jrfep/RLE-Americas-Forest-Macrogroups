## The original files of the potential distribution of the Macrogroups in geotiff format were delivered by NatureServe.
## Data set description in:
## http://hub.arcgis.com/datasets/b25fa8f7673749fc85e0ba7980374c5f
## http://hub.arcgis.com/datasets/Natureserve::southamerica-ivc-macrogroups-potential-natureserve-v1

cp $GISDATA/ecosistemas/NatureServe/*potential*tif.lpk $WORKDIR
cp $NSDATA/NorthAmerica_Caribbean_IVC_MacroGroups_potential_NatureServe_v5_270m_tif.lpk $WORKDIR
cd $WORKDIR
## Extraction: files were encoded in .lpk format, I extracted them using the 7z extraction command
7z x SouthAmerica_IVC_MacroGroups_potential_NatureServe_v7_270m_tif.lpk
7z x NorthAmerica_Caribbean_IVC_MacroGroups_potential_NatureServe_v5_270m_tif.lpk

rm *lpk

## The layer package contains different files, raster are located in folder commondata/raster_data/

## Original projections: files differed in the original projection
gdalinfo commondata/raster_data/NorthAmerica_Caribbean_IVC_MacroGroups_potential_NatureServe_v5_270m.tif
## the South America file also includes a table with raster attributes
gdalinfo commondata/raster_data/SouthAmerica_IVC_MacroGroups_potential_NatureServe_v7_270m.tif | less

## Reprojection: In order to combine both layers in one file with a common projection, I used gdalwarp
## create option COMPRESS=LZW allows for lossless data compression
##  I chose the robin projection for the whole continent
gdalwarp -co "COMPRESS=LZW" -t_srs '+proj=robin +lon_0=-80 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0' commondata/raster_data/NorthAmerica_Caribbean_IVC_MacroGroups_potential_NatureServe_v5_270m.tif commondata/raster_data/SouthAmerica_IVC_MacroGroups_potential_NatureServe_v7_270m.tif IVC_NS_v7_270m_robin.tif
## output file is IVC_NS_v7_270m_robin.tif

rm -r commondata/ esriinfo/ v10 v103/

## generate shapefile version...
##gdal_polygonize.py IVC_NS_v7_270m_robin.tif IVC_NS_v7_270m_robin_polygons.shp
