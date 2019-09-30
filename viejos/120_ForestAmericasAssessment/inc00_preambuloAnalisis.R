setwd("~/tmp")
##mi.rda <- "~/Rdata/Provita"

(load(sprintf("%s/20170724_ResultadosParaPublicacion.rda",gsub("doc","Rdata",mi.path))))
##confTest <- read.csv("~/Dropbox/MS/02_RondaN/FEE_RLE/20171130_TableAppendixA1_confidenceTests.csv",stringsAsFactors=F)

IUCNclr <- rgb(c(0,255,255,255,173,0,128,255),
               c(0,0,165,255,255,128,128,255),
               c(0,0,0,0,47,0,128,255),maxColorValue=255)
names(IUCNclr) <- c("CO","CR","EN","VU","NT","LC","DD","NE")

colsIUCN <- rgb(c(255,255,255,0),c(0,165,255,128),c(0,0,0,0),max=255)
##(load(sprintf("%s/NS_hexagonos_aug.rda",mi.rda)))

##datos paises 
##adm0 <- shapefile("/opt/gisdata/vectorial/DIVA/countries.shp")
paises <- read.dbf(sprintf("%s/ModisSAM/PERMANENT/dbf/Paises.dbf",mi.gis.dir))
## para el paso de cambio climático
TMWB <- shapefile(sprintf("%s/vectorial/TMworldborders/TM_WORLD_BORDERS-0.3.shp",mi.gisdata.dir))

##datos de los MGs

allMGs <- read.csv(sprintf("%s/IVC/TablaOriginalMacrogrupos.csv",gsub("doc","data",mi.path)),as.is=T)
rslts.NS <- read.xls(sprintf("%s/Natureserve/IUCN/SAM/South_America_Results_IUCN_RLE_April2015.xlsx",mi.gisdata.dir))

##rslts <- read.xls("~/Descargas/Tabla Bosques_NS.xlsx",sheet=6,as.is=T,encoding="utf8")
##tipologia <- read.csv(sprintf("%s/Natureserve/IUCN/EcoVeg_typology_hierarchy 30 Jan 30 2015.csv",mi.gisdata.dir))
if (!exists("tipologia")) 
    tipologia <- read.csv(sprintf("%s/IVC/EcoVeg_typology_hierarchy 30 Jan 30 2015.csv",gsub("doc","data",mi.path)))


defMG <- read.ods(sprintf("%s/IVC/TablaEspeciesMG.ods",gsub("doc","data",mi.path)),stringsAsFactors=F)
defMG$MG <- trim(gsub("\\.","",defMG$MG))
refs <-  aggregate(defMG[,5:6],list(MG=defMG$MG),sum,na.rm=T)

tipologia$grp <-  substr(tipologia$Division.Code,1,1)
tipologia$sgrp <-  substr(tipologia$Division.Code,1,3)
tipologia$format <- substr(tipologia$Division.Code,1,5)

if (!exists("GEnS.tab")) {
    GEnS.tab <- read.dbf(sprintf("%s/GEnS/GEnS_v3.dbf",gsub("doc","data",mi.path)))
    ecoregs <- read.dbf(sprintf("%s/biogeografia/WWF/official/wwf_terr_ecos.dbf",mi.gisdata.dir))
    GEnS.df <- data.frame(GEnZ=levels(GEnS.tab$GEnZname),
                          temp=c(0,0,1,1,2,1,2,3,3,3,4,4,5,5,5,6,6,6),
                          humd=c(0,0,1,1,1,3,3,4,5,2,3,5,3,4,6,6,5,2))
    plot(hclust(dist(GEnS.df[,c("temp","humd")])),labels=GEnS.df$GEnZ)
    
    GEnS.mtz <- as.matrix(dist(GEnS.df[,c("temp","humd")]))
    rownames(GEnS.mtz) <- GEnS.df$GEnZ
    colnames(GEnS.mtz) <- GEnS.df$GEnZ
    GEnS.df$clust <- cutree(hclust(dist(GEnS.df[,c("temp","humd")])),h=3)
}

if (!exists("legend.Anthromes")) {
    legend.Anthromes <- read.table(sprintf("%s/biomas/Anthromes/v2.0/anthromes_2_GeoTIFF/leyenda.txt",mi.gisdata.dir),header=T)
    legend.Anthromes$nombre <- c("Urban","Mixed settlements",
                                 "Rice villages","Irrigated villages","Rainfed villages", "Pastoral villages",
                                 "Residential irrigated croplands",
                                 "Residential rainfed croplands",
                                 "Populated croplands",
                             "Remote croplands",
                                 "Residential rangelands",
                                 "Populated rangelands",
                                 "Remote rangelands",
                                 "Residential woodlands",
                                 "Populated woodlands",
                                 "Remote woodlands",
                                 "Inhabitated treeless & barren lands",
                                 "Wild woodlands",
                             "Wild treeless & barren lands")
    
    legend.Anthromes$groups <- c(rep("Dense settlements",2),
                                 rep("Villages",4),
                                 rep("Croplands",4),
                                 rep("Rangelands",3),
                                 rep("Seminatural",4),
                                 rep("Wildlands",2))
}

## por algún motivo este paso tiene una duración de procesamiento muy variable
validationtables <- sprintf("%s/MGforestValidationTables.rda",gsub("doc","Rdata",mi.path))
if (file.exists(validationtables))
    load(validationtables)
if(!exists("Pat.table")) {
    Pat.table <- read.xls(sprintf("%s/IVC/Americas_Forests.xlsx",gsub("doc","data",mi.path)))
    Josse.table <- read.xls(sprintf("%s/IVC/Americas_Forests.xlsx",gsub("doc","data",mi.path)),sheet=2)
    Pat.table$mcdg <- sprintf("M%03d",Pat.table$MG_cd)
    save(file=validationtables,
         Pat.table,Josse.table)
}


(load(file=sprintf("%s/data_RF_MGs.rda",gsub("doc","Rdata",mi.path))))
if (!exists("xys.ps")) {
    xys <- spTransform(rnd,TMWB@proj4string)
    xys.ps <- over(xys,TMWB)
    save(file=sprintf("%s/data_RF_MGs.rda",gsub("doc","Rdata",mi.path)),rnd,ccli,ecos,xys.ps,xys)

}
