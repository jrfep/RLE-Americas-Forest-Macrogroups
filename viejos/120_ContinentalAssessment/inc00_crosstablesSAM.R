##grass -text ~/mi.gis/SAM/mapoteca/
## aquí es grass 7.0.3
##grass -text /media/user3/DATOS/00_LRE/mi.gis/SAM/mapoteca/ 

##R --vanilla
setwd("~/tmp")

system("mkdir calculos_201712")

##for (k in paste("modisfc",2000:2012,sep=""))
##for (k in paste("GFC",2000:2012,sep=""))
##ADVERTENCIA: Please update the usage of <r.stats>: option <fs> has been
##             renamed to <separator>
for (k in paste("bsq",2001:2012,sep=""))
    system(sprintf("r.stats -a -c input=SouthAmerica_IVC_MacroGroups_potential_NatureServe_v7_270m,Paises,AOO,%s output=calculos_201712/SAM_Paises_AOO_%s.tab separator=space",k,k))

for (k in sprintf("Anthromes_%s00",17:20))
    system(sprintf("r.stats -a -c input=SouthAmerica_IVC_MacroGroups_potential_NatureServe_v7_270m,Paises,AOO,%s output=calculos_201712/SAM_Paises_AOO_%s.tab separator=space",k,k))
