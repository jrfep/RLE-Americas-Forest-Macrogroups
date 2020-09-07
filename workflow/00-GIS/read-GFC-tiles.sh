#!/usr/bash

r.proj location=IVC mapset=PERMANENT dbase=$GISDB input=IVC_NS_v7 output=IVC
case ${?} in
  0) {
    r.proj location=IVC mapset=PERMANENT dbase=$GISDB input=MCD12Q1.bsq output=MCD12Q1.bsq
    r.external input=$ARCH output=$VAR
    for VAR2 in lossyear treecover2000
    do
      r.external input=$(echo $ARCH | sed s/$VAR/$VAR2/) output=$VAR2
    done
    r.stats -acn IVC,MCD12Q1.bsq,treecover2000,lossyear,gain > ${LOC}-IVC-MCD12Q1-lossgain.tab
    } ;;
  *) {
    echo "outside IVC distribution"
  }
esac
