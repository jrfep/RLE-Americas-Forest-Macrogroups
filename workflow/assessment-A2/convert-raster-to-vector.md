
```sh
conda deactivate

grass $GISDB/IVC/PERMANENT

g.mapset -c macrogroups

#psql -At -d IUCN -c "select ivc_format,mg_key,mg_hierarc from ivc_americas where formation_ IN ('1.A.1','1.A.2','1.A.3','1.A.4','1.A.5','1.B.1','1.B.2','1.B.3') order by ivc_format,mg_key"

psql -At -d IUCN -c "select divsion_c,mg_key from ivc_americas where formation_ IN ('1.A.1','1.A.2','1.A.3','1.A.4','1.A.5','1.B.1','1.B.2','1.B.3') order by ivc_format,mg_key"

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

#slow
g.region -pd
r.stats -ac M652
#fast
g.region vect=M652@macrogroups -p
r.stats -ac M652

eval `g.region -g`
eval `g.region -bg`

export VRS=GFC-2019-v1.7
export VAR=treecover2000
mkdir -p $WORKDIR/$VRS

for VAR in gain lossyear treecover2000
do
  if [ ! -e $WORKDIR/$VRS/${VRS}.M652.${VAR}.tif ]
  then
    gdalwarp -t_srs "+proj=robin +lon_0=-80 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" -te $w $s $e $n -co "COMPRESS=LZW" $GISDB/extra-gisdata/sensores/Landsat/index_${VRS}_${VAR}.vrt $WORKDIR/$VRS/${VRS}.M652.${VAR}.tif
    r.in.gdal input=$WORKDIR/$VRS/${VRS}.M652.${VAR}.tif output=${VAR}
  fi
done

export VAR=MCD12Q1
export VRS=006
mkdir -p $WORKDIR/$VAR.$VRS/

for YEAR in $(seq 2001 2019)
do
  export FECHA=${YEAR}.01.01

  if [ ! -e $WORKDIR/$VAR.$VRS/${VAR}.M652.${YEAR}.tif ]
  then
    gdalwarp -t_srs "+proj=robin +lon_0=-80 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" -te $w $s $e $n -co "COMPRESS=LZW" $GISDATA/sensores/Modis/MCD12Q1.006/index_${VAR}_${VRS}_${FECHA}_LC_Type1.vrt $WORKDIR/$VAR.$VRS/${VAR}.M652.${YEAR}.tif
    r.in.gdal input=$WORKDIR/$VAR.$VRS/${VAR}.M652.${YEAR}.tif output=${VAR}.${YEAR}
  fi
done

for YEAR in $(seq 2001 2019)
do
  r.mapcalc expression="bsq.${YEAR}=if(MCD12Q1.${YEAR}@macrogroups<6,1,0)"
done

g.region rast=treecover2000 -p
r.stats -acn M652,treecover2000,lossyear,gain > GFC-lossgain.tab

for YEAR in $(seq 2001 2019)
do
  r.stats -acn M652,MCD12Q1.${YEAR}@macrogroups,treecover2000,lossyear,gain > MCD12Q1-${YEAR}-GFC-lossgain.tab
done

r.stats -acn M652,bsq.2001,bsq.2002,bsq.2003,bsq.2004,bsq.2005,bsq.2006,bsq.2007,bsq.2008,bsq.2009,bsq.2010,bsq.2011,bsq.2012,bsq.2013,bsq.2014,bsq.2015,bsq.2016,bsq.2017,bsq.2018,bsq.2019 > MCD12Q1.tab
```

```r
# R --vanilla
require(dplyr)
tmp1 <- read.table("GFC-lossgain.tab",col.names=c('code','treecover2000','lossyear','gain','area','cells'))
tmp2 <- read.table("MCD12Q1.tab",col.names=c('code',sprintf('bsq.%s',2001:2019),'area','cells'))
tmp4 <- read.table("MCD12Q1-2001-GFC-lossgain.tab", col.names=c('code','MCD12Q1','treecover2000','lossyear','gain','area','cells'))

## they are all in the same resolution
all.equal(sum(tmp1$cells),sum(tmp2$cells),sum(tmp4$cells))

sum(subset(tmp2,bsq.2001>0)$area)/1e6
sum(subset(tmp2,bsq.2009>0)$area)/1e6


tmp4 %>% group_by(MCD12Q1) %>% summarise(mu=sum(treecover2000,area)/1e6)

tmp4 %>% group_by(MCD12Q1) %>% summarise(mu=weighted.mean(treecover2000,area))


##tt <- with(tmp4,tapply((treecover2000 * area)/1e6,list(lossyear,gain),sum))

dts <- data.frame(year=2000,grp='all',area=with(tmp4,sum((treecover2000/100) * area)/1e6),stringsAsFactors=F)

for (yy in 1:19) {
  dts <- rbind(dts,
  data.frame(year=2000+yy,grp='all',area=with(subset(tmp4,lossyear == 0 | lossyear > yy),sum((treecover2000/100) * area)/1e6)))
  dts <- rbind(dts,
  data.frame(year=2000+yy,grp='all',area=with(subset(tmp4,lossyear == 0 | lossyear > yy | gain %in% 1),sum((treecover2000/100) * area)/1e6)))
}


for (k in 2001:2019) {
  tmp4 <- read.table(sprintf("MCD12Q1-%s-GFC-lossgain.tab",k), col.names=c('code','MCD12Q1','treecover2000','lossyear','gain','area','cells'))
  dts <- rbind(dts,data.frame(year=2000,grp=k,area=with(subset(tmp4,MCD12Q1 %in% 1:5),sum((treecover2000/100) * area)/1e6)))

  for (yy in 1:19) {
    dts <- rbind(dts,
    data.frame(year=2000+yy,grp=k,area=with(subset(tmp4,(MCD12Q1 %in% 1:5) & (lossyear == 0 | lossyear > yy)),sum((treecover2000/100) * area)/1e6)))
    dts <- rbind(dts,
    data.frame(year=2000+yy,grp=k,area=with(subset(tmp4,(MCD12Q1 %in% 1:5) & (lossyear == 0 | lossyear > yy | gain %in% 1)),sum((treecover2000/100) * area)/1e6)))
  }

}




mdl1 <- glm(area~year,subse(dts,grp %in% 'all'),family=quasipoisson)
prd1 <- predict(mdl1,data.frame(year=c(2050)),se.fit=T)

prd.x <- exp(prd1$fit + (prd1$se.fit*1.645)) # 90% CI
prd.n <- exp(prd1$fit - (prd1$se.fit*1.645)) # 90% CI

1-(prd.x/dts[1,"area"])
1-(prd.n/dts[1,"area"])
prd1 <- predict(mdl1,data.frame(year=c(2001,2050)),se.fit=T)

prd.x <- exp(prd1$fit + (prd1$se.fit*1.645)) # 90% CI
prd.n <- exp(prd1$fit - (prd1$se.fit*1.645)) # 90% CI

1-(prd.x[2]/prd.n[1])
1-(prd.n[2]/prd.x[1])

mdl2 <- glm(area~year,subset(dts,!grp %in% 'all'),family=quasipoisson)
prd2 <- predict(mdl2,data.frame(year=c(2001,2050)),se.fit=T)

prd.x <- exp(prd2$fit + (prd2$se.fit*1.645)) # 90% CI
prd.n <- exp(prd2$fit - (prd2$se.fit*1.645)) # 90% CI

1-(prd.x[2]/prd.n[1])
1-(prd.n[2]/prd.x[1])
```


## generate shapefile version...
With gdal calc and gdal polygonize can take a long time to complete
```sh
##gdal_polygonize.py IVC_NS_v7_270m_robin.tif IVC_NS_v7_270m_robin_polygons.shp

for MCDG in 563 572
do
  gdal_calc.py -A IVC_NS_v7_270m_robin.tif --outfile=M${MCDG}.tif --calc="(A==${MCDG})*1" --format=GTiff --type=Byte --creation-option="NBITS=1" --creation-option="COMPRESS=DEFLATE" --NoDataValue=0
  gdal_polygonize.py M${MCDG}.tif M${MCDG}.shp -f "ESRI shapefile"
done

```
