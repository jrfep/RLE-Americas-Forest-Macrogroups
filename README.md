# RLE-Americas-Forest-Macrogroups
Application of the IUCN Red List of Ecosystems to the forest macrogroups of the Americas

This repository replicates and documents the analysis in **Ferrer-Paris et al. (2019)** for the assessment of tropical and temperate forest ecosystems in the Americas region.

## Repository structure

### assets: Original data sources
Folder with meta-data and documentation of data sources used.

### env: programming environment variables
Sets up environment variables before running the workflows and scripts

### inc: useful scripts
Scripts for specific tasks

### workflow
These folders describe complete workflows for analysis or documentation

### documentation
Documentation of the assessment in xml data format.

### results
Summary of overall results in tabular format, figures and other outputs

## Citation
Please refer to the original reference:
Ferrer‐Paris, JR, Zager, I, Keith, DA, et al. An ecosystem risk assessment of temperate and tropical forests of the Americas with an outlook on future conservation strategies. Conservation Letters. 2019; 12:e12623. https://doi.org/10.1111/conl.12623


```sh
conda activate geotest
cd /opt/gisdb/extra-gisdata/sensores/GFC-2019-v1.7/
python
from osgeo import gdal,ogr,osr
raster=r'Hansen_GFC-2019-v1.7_lossyear_10S_080W.tif'
ds=gdal.Open(raster)

gt=ds.GetGeoTransform()
cols = ds.RasterXSize
rows = ds.RasterYSize

ulx, xres, xskew, uly, yskew, yres  = ds.GetGeoTransform()
lrx = ulx + (ds.RasterXSize * xres)
lry = uly + (ds.RasterYSize * yres)
```
