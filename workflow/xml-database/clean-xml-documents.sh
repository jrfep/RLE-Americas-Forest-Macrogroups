cd $SCRIPTDIR/documentation/xml
for k in $(ls *xml)
do
  sed -f $SCRIPTDIR/inc/sed/special-characters -i .bkp $k
done

tar -cjvf RA_Forest_Macrogroups.tar.bz2 RA*xml
zip RA_Forest_Macrogroups.zip RA_Forest_Macrogroups*xml
