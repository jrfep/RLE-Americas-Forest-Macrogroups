conda deactivate
cd $WORKDIR
grass -c IVC_NS_v7_270m_robin.tif $GISDB/IVC
r.in.gdal input="IVC_NS_v7_270m_robin.tif" output="IVC_NS_v7" memory=300 offset=0 num_digits=0

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
