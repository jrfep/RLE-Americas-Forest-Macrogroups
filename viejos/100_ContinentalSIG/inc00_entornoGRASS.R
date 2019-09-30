##localizaciones en grass:
## 1 SAM
## 2 NAC
## 3 ModisSAM
## 4 ModisNAC
## 5 HansenSAM
## 6 HansenNAC

##~~~~~~
## 1 SAM
##^^^^^^
## generar a partir de mapa
## SouthAmerica_IVC_MacroGroups_existing_NatureServe_v7_270m_tif
## ver: inc10_location_SAM.R

##~~~~~~
## 2 NAC
##^^^^^^
## generar a partir de mapa
## NorthAmerica_Caribbean_IVC_MacroGroups_existing_NatureServe_v5_270m
##7z x ../NAC/NorthAmerica_Caribbean_IVC_MacroGroups_potential_NatureServe_v5_270m_tif.lpk
##7z x ../NAC/NorthAmerica_Caribbean_IVC_MacroGroups_existing_NatureServe_v5_270m_tif.lpk

##r.in.gdal input=/media/user3/DATOS/00_LRE/gisdata/Natureserve/IUCN/NAC/commondata/raster_data/NorthAmerica_Caribbean_IVC_MacroGroups_potential_NatureServe_v5_270m.tif output=NorthAmerica_Caribbean_IVC_MacroGroups_potential_NatureServe_v5_270m location=NAC

## ver: inc11_location_NAC.R

##~~~~~~
## 3 ModisSAM
##^^^^^^
## ver: inc12_location_ModisSAM.R

##~~~~~~
## 4 ModisNAC
##^^^^^^
## ver: inc13_location_ModisNAC.R

##~~~~~~
## 5 HansenSAM
##^^^^^^
##r.in.gdal --overwrite input=/media/mapoteca/Elements/gisdata/sensores/Landsat/GFC2015/Hansen_GFC2015_treecover2000_SAM.tif output=GFC2000 location=HansenSAM
## ver: inc14_location_HansenSAM.R

##~~~~~~
## 5 HansenNAC
##^^^^^^
## ver: inc15_location_HansenNAC.R


##grass -text ~/mi.gis/ModisSAM/PERMANENT/
system("v.in.ogr --overwrite  dsn=/opt/gisdata/Morrone/Lowenberg_Neto_2015.shp output=Morrone1 snap=1e-13")
system("v.in.ogr --overwrite  dsn=/opt/gisdata/Morrone/Lowenberg_Neto_2014.shp output=Morrone2 snap=1e-13")
GEnS.tab <- read.dbf("/media/mapoteca/Elements/gisdata/climod/GEnS/GEnS_SAM.dbf")



