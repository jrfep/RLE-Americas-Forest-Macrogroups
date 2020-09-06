International Vegetation Classification
IVC or EcoVeg
https://www.natureserve.org/conservation-tools/projects/international-vegetation-classification


```sh
mkdir -p $GISDATA/ecosistemas/NatureServe/
cd $GISDATA/ecosistemas/NatureServe/
```

## North America and South America

Data set description in:
http://hub.arcgis.com/datasets/b25fa8f7673749fc85e0ba7980374c5f
http://hub.arcgis.com/datasets/Natureserve::southamerica-ivc-macrogroups-potential-natureserve-v1

 The original files of the potential distribution of the Macrogroups in geotiff format were delivered by NatureServe, files were encoded in .lpk format, I extracted them using the 7z extraction command.

```sh
cd $WORKDIR
cp $GISDATA/ecosistemas/NatureServe/*potential*tif.lpk $WORKDIR
7z x SouthAmerica_IVC_MacroGroups_potential_NatureServe_v7_270m_tif.lpk
7z x NorthAmerica_Caribbean_IVC_MacroGroups_potential_NatureServe_v5_270m_tif.lpk

rm *lpk


mkdir -p $WORKDIR
cd $WORKDIR

cp $GISDATA/ecosistemas/Natureserve/IUCN/SAM/SouthAmerica_IVC_MacroGroups_potential_NatureServe_v7_270m_tif.lpk .
cp $GISDATA/ecosistemas/Natureserve/IUCN/NAC/NorthAmerica_Caribbean_IVC_MacroGroups_potential_NatureServe_v5_270m_tif.lpk .

7z x SouthAmerica_IVC_MacroGroups_potential_NatureServe_v7_270m_tif.lpk
7z x NorthAmerica_Caribbean_IVC_MacroGroups_potential_NatureServe_v5_270m_tif.lpk


gdalwarp -co "COMPRESS=LZW" -t_srs '+proj=robin +lon_0=-80 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0' commondata/raster_data/NorthAmerica_Caribbean_IVC_MacroGroups_potential_NatureServe_v5_270m.tif commondata/raster_data/SouthAmerica_IVC_MacroGroups_potential_NatureServe_v7_270m.tif IVC_NS_v7_270m_robin.tif

rm *lpk

```
The layer package contains different files, raster are located in folder `commondata/raster_data/`. Original projections: files differed in the original projection, the South America file also includes a table with raster attributes

```sh
gdalinfo commondata/raster_data/NorthAmerica_Caribbean_IVC_MacroGroups_potential_NatureServe_v5_270m.tif

gdalinfo commondata/raster_data/SouthAmerica_IVC_MacroGroups_potential_NatureServe_v7_270m.tif | less
```
Reprojection: In order to combine both layers in one file with a common projection, I used gdalwarp create option COMPRESS=LZW allows for lossless data compression   I chose the robin projection for the whole continent. output file is IVC_NS_v7_270m_robin.tif

```sh

gdalwarp -co "COMPRESS=LZW" -t_srs '+proj=robin +lon_0=-80 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0' commondata/raster_data/NorthAmerica_Caribbean_IVC_MacroGroups_potential_NatureServe_v5_270m.tif commondata/raster_data/SouthAmerica_IVC_MacroGroups_potential_NatureServe_v7_270m.tif IVC_NS_v7_270m_robin.tif


rm -r commondata/ esriinfo/ v10 v103/

## generate shapefile version...
##gdal_polygonize.py IVC_NS_v7_270m_robin.tif IVC_NS_v7_270m_robin_polygons.shp
```



```
 mdb-tables -d ";" gisdata/ecosistemas/Natureserve/IUCN/Docs/IUCN\ data.accdb
mdb-export gisdata/ecosistemas/Natureserve/IUCN/Docs/IUCN\ data.accdb "Ecosystem" | head

```


```R
##R --vanilla
require(dplyr)
require("RPostgreSQL")
require(foreign)
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "IUCN",
                 host = "localhost", port = 5432,
                 user = "jferrer")



work.dir <- Sys.getenv("WORKDIR")
setwd(work.dir)
## make sure to have the NatureServe files extracted here
## inc/gdal/merge_input_to_IVC_raster.sh

tmp1 <- read.dbf("commondata/raster_data/NorthAmerica_Caribbean_IVC_MacroGroups_potential_NatureServe_v5_270m.tif.vat.dbf")
tmp2 <- read.dbf("commondata/raster_data/SouthAmerica_IVC_MacroGroups_potential_NatureServe_v7_270m.tif.vat.dbf")
total.cols <- colnames(tmp1)[colnames(tmp1) %in% colnames(tmp2)]
tmp <- rbind(tmp1[,total.cols],
  tmp2[,total.cols])

dim(tmp)
unique(tmp$mg_key)
with(tmp,aggregate(Count,list(Value,mg_key),sum))

## https://stackoverflow.com/questions/39181208/how-to-group-by-all-but-one-columns
## https://datacarpentry.org/R-genomics/04-dplyr.html
## https://www3.nd.edu/~steve/computing_with_data/24_dplyr/dplyr.html

IVC_eco = tmp %>%
  filter(!is.na(mg_key)) %>%
    group_by_at(names(tmp)[-grep("Count", names(tmp))]) %>%
      summarise(total = sum(Count, na.rm = TRUE))

##apply(IVC_eco,2,function(x) max(nchar(as.character(x)),na.rm=T))

dbWriteTable(con,"tmptable3",IVC_eco,overwrite=T,row.names = FALSE)
qry <- "INSERT INTO ivc_americas SELECT * FROM tmptable3 ON CONFLICT (mg_key) DO NOTHING"
dbSendQuery(con,qry)
qry <- "DROP TABLE tmptable3"
dbSendQuery(con,qry)

## qry <- "SELECT macrogroup, mg_key,total from ivc_americas where ivc_class like '%Forest%'"
##  rs <- dbSendQuery(con,qry)
##  rslts <- dbFetch(rs)
##  dbClearResult(rs)

dbDisconnect(con)
```
