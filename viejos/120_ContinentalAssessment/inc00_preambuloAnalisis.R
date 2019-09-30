setwd("~/tmp")
mi.rda <- "~/Rdata/Provita"

colsIUCN <- rgb(c(255,255,255,0),c(0,165,255,128),c(0,0,0,0),max=255)
(load(sprintf("%s/NS_hexagonos_aug.rda",mi.rda)))

##datos paises (¿cual se necesita de los dos?)
adm0 <- shapefile("/opt/gisdata/vectorial/DIVA/countries.shp")
paises <- read.dbf("/home/jferrer/mi.gis/ModisSAM/PERMANENT/dbf/Paises.dbf")

##datos de los MGs

allMGs <- read.csv("/home/jferrer/Provita/data/IVC/TablaOriginalMacrogrupos.csv",as.is=T)
rslts.NS <- read.xls("/opt/gisdata/Natureserve/IUCN/SAM/South_America_Results_IUCN_RLE_April2015.xlsx")

##rslts <- read.xls("~/Descargas/Tabla Bosques_NS.xlsx",sheet=6,as.is=T,encoding="utf8")
if (!exists("tipologia")) 
    tipologia <- read.csv("/opt/gisdata/Natureserve/IUCN/EcoVeg_typology_hierarchy 30 Jan 30 2015.csv")


defMG <- read.ods("~/Provita/data/IVC/TablaEspeciesMG.ods",stringsAsFactors=F)
defMG$MG <- trim(gsub("\\.","",defMG$MG))
refs <-  aggregate(defMG[,5:6],list(MG=defMG$MG),sum,na.rm=T)

tipologia$grp <-  substr(tipologia$Division.Code,1,1)
tipologia$sgrp <-  substr(tipologia$Division.Code,1,3)

    
tipologia$grp <-  substr(tipologia$Division.Code,1,1)
tipologia$sgrp <-  substr(tipologia$Division.Code,1,3)
tipologia$format <- substr(tipologia$Division.Code,1,5)

if (!exists("GEnS.tab")) {
    GEnS.tab <- read.dbf(sprintf("~/Provita/data/GEnS/GEnS_v3.dbf"))
    ecoregs <- read.dbf(sprintf("/opt/gisdata/vectorial/WWF/official/wwf_terr_ecos.dbf"))
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
    legend.Anthromes <- read.table("/opt/gisdata/biomas/Anthromes/v2.0/anthromes_2_GeoTIFF/leyenda.txt",header=T)
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
##if(!exists("Pat.table")) {
##    Pat.table <- read.xls("/home/jferrer/Provita/data/IVC/Americas_Forests.xlsx")
##    Josse.table <- read.xls("/home/jferrer/Provita/data/IVC/Americas_Forests.xlsx",sheet=2)
## Pat.table$mcdg <- sprintf("M%03d",Pat.table$MG_cd)
##}
