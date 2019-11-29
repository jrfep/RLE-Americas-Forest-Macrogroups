conda deactivate

grass $GISDB/IVC/PERMANENT

g.mapset -c macrogroups

r.mapcalc 'M652=if(IVC_NS_v7==652,1,null())'
r.to.vect -v input=M652@macrogroups output=M652@macrogroups type=area

## fix in PostgreSQL to get the right SRID
## grant access to public.spatial_ref_sys

## this does not allow multi features
##v.out.postgis input=M652@macrogroups output="PG:host=localhost user=jferrer dbname=rle_gis_db" options="GEOMETRY_NAME=wkb_geometry,SRID=90814" output_layer=ivc.macrogroups_import

## this one does but is then too slow to make transformations in postgis
##v.out.ogr -m input=M652@macrogroups type=area output="PG:host=localhost user=jferrer dbname=rle_gis_db" format=PostgreSQL output_layer=ivc.macrogroups_import

v.out.ogr -m input=M652@macrogroups type=area output=M652.shp format=ESRI_Shapefile

mkdir WGS84
ogr2ogr  -t_srs EPSG:4326 WGS84/M652.shp M652.shp 
