cd $SCRIPTDIR/documentation/xml
for k in $(ls *xml)
do
  sed -f $SCRIPTDIR/inc/sed/special-characters -i .bkp $k
done
