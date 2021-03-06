#!/bin/bash


for k in $(psql -At -d IUCN -c "select value,mg_key from ivc_americas where formation_ IN ($@) order by ivc_format,mg_key")
do
  export MCDG=$(echo $k | cut -d"|" -f 2)
  export CDG=$(echo $k | cut -d"|" -f 1)
  g.mapset -c $MCDG
  g.region rast=IVC_NS_v7
  if [ $(g.list type=raster pattern=$MCDG | wc -l) == 1 ]
  then
     echo "$MCDG raster listo"
  else
    r.mapcalc ${MCDG}'=if(IVC_NS_v7=='${CDG}',1,null())'
  fi
  if   [ $(g.list type=vector pattern=$MCDG | wc -l) == 1 ]
  then
    echo "$MCDG vector listo"
  else
     r.to.vect -v input=${MCDG} output=${MCDG} type=area
  fi
  g.region vect=${MCDG}
done
