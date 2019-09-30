##grass -text ~/mi.gis/NAC/mapoteca/
##R --vanilla
setwd("~/tmp")

system("mkdir calculos_201712")

##for (k in paste("modisfc",2000:2012,sep=""))
##for (k in paste("GFC",2000:2012,sep=""))
for (k in paste("bsq",2001:2012,sep=""))
    system(sprintf("r.stats -a -c input=NorthAmerica_Caribbean_IVC_MacroGroups_potential_NatureServe_v5_270m,Paises,AOO,%s output=calculos_201712/NAC_Paises_AOO_%s.tab fs=space",k,k))

for (k in sprintf("Anthromes_%s00",17:20))
    system(sprintf("r.stats -a -c input=NorthAmerica_Caribbean_IVC_MacroGroups_potential_NatureServe_v5_270m,Paises,AOO,%s output=calculos_201712/NAC_Paises_AOO_%s.tab fs=space",k,k))

    system(sprintf("r.stats -a -c input=NorthAmerica_Caribbean_IVC_MacroGroups_potential_NatureServe_v5_270m,Paises,Anthromes_1700,Anthromes_1800,Anthromes_1900,Anthromes_2000 output=calculos_201712/NAC_Paises_Anthromes_Changes.tab fs=space"))

for (k in c("RS","GWSchange")[2])
    system(sprintf("r.stats -a -c input=NorthAmerica_Caribbean_IVC_MacroGroups_potential_NatureServe_v5_270m,Paises,%s output=calculos_201712/NAC_Paises_%s.tab fs=space",k,k))





##generar shapefiles

system("g.mapset -c Macrogrupos")
system("g.mapsets Macrogrupos,mapoteca,PERMANENT")

mi.path <- "~/Provita/doc/"
if (!file.exists(mi.path))
    mi.path <- "~/Provita_JRFP/doc/"
if (!file.exists(mi.path))
    mi.path <- "~/Dropbox/Provita/doc/"


(load(sprintf("%s/20170724_ResultadosParaPublicacion.rda",gsub("doc","Rdata",mi.path))))
if (!exists("tipologia")) 
    tipologia <- read.csv(sprintf("%s/IVC/EcoVeg_typology_hierarchy 30 Jan 30 2015.csv",gsub("doc","data",mi.path)))

tipologia$grp <-  substr(tipologia$Division.Code,1,1)
tipologia$sgrp <-  substr(tipologia$Division.Code,1,3)
tipologia$format <- substr(tipologia$Division.Code,1,5)

cdgs <- levels(rsm$mcdg)


for (frmt in c("1.A.1","1.A.2","1.A.3","1.A.4","1.A.5","1.B.1","1.B.2","1.B.3")) {
    cat(sprintf("%s ::",frmt))
    for (mcdg in cdgs[cdgs %in% subset(tipologia,format %in% frmt & macrogroup_key != "")$macrogroup_key]) {
        cat(sprintf(": %s :",mcdg))

        output.dir <- sprintf("/home/jferrer/Provita/output/MGs/%s/%s",frmt,mcdg)
        system(sprintf("mkdir -p %s",output.dir))
        cdg <- as.numeric(gsub("M","",mcdg))
        gc()

        tmp <- read.table("calculos_201712/NAC_Paises_GWSchange.tab")
        tmp <- subset(tmp,V1 %in% cdg)
        ls <- unique(as.character(tmp$V2))
        ls <- ls[!ls %in% "*"] 

        ## usamos umbral de 25 porque en la creación de la capa bsq.20XX aplicamos este umbral para zonas de mosaico boscoso

        for (l in ls) {
            system(sprintf("r.mapcalc 'tmp=if(NorthAmerica_Caribbean_IVC_MacroGroups_potential_NatureServe_v5_270m@PERMANENT==%s & Paises==%s & bsq2012>24,1,null())'",cdg,l))
            
            if (strsplit(system("r.info tmp@Macrogrupos -r",intern=T),"=")[[2]][2] == "1") {
                system(sprintf("r.to.vect --overwrite input=tmp@Macrogrupos output=%s_%s feature=area",mcdg,l))
                system(sprintf("v.hull input=%1$s_%2$s@Macrogrupos output=%1$s_%2$s_EOO",mcdg,l))
                ##if (file.exists(sprintf("%4$s/%1$s_%3$s_EOO.shp",mcdg,frmt,l,output.dir)))
                ##  system(sprintf("rm %4$s/%1$s_%3$s_EOO.*",mcdg,frmt,l,output.dir))
                system(sprintf("v.out.ogr -c type=area input=%1$s_%3$s_EOO dsn=%4$s olayer=%1$s_%3$s_EOO format=ESRI_Shapefile",mcdg,frmt,l,output.dir))
            }
        }
    }
}
