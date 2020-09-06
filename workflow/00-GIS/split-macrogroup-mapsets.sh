#!/bin/bash

#for k in $(psql -At -d IUCN -c "select value,mg_key from ivc_americas where formation_ IN ('1.A.1','1.A.2','1.A.3','1.A.4','1.A.5','1.B.1','1.B.2','1.B.3') order by ivc_format,mg_key")
for k in $(psql -At -d IUCN -c "select value,mg_key from ivc_americas where formation_ IN ($@) order by ivc_format,mg_key")
do
  export MCDG=$(echo $k | cut -d"|" -f 2)
  export CDG=$(echo $k | cut -d"|" -f 1)
  g.mapset -c $MCDG
  g.region rast=IVC_NS_v7
  r.mapcalc '${MCDG}=if(IVC_NS_v7==${CDG},1,null())'
  r.to.vect -v input=${MCDG} output=${MCDG} type=area
  g.region vect=${MCDG}
done
