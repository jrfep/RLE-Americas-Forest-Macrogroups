################
## Macrogroups of the Americas
###############

###############
## INPUTS
################

## Many documents...
## Typology

if (!exists("tipologia")) 
    tipologia <- read.csv("/opt/gisdata/Natureserve/IUCN/EcoVeg_typology_hierarchy 30 Jan 30 2015.csv")
tipologia$grp <-  substr(tipologia$Division.Code,1,1)
tipologia$sgrp <-  substr(tipologia$Division.Code,1,3)
tipologia$frmt <-  substr(tipologia$Division.Code,1,5)

## Macrogroup concepts
##rsm.MG <- read.xls(sprintf("%sIVC/TablaBosquesNS.xlsx",dbdata.dir),sheet=1,stringsAsFactors=F)
rsm.MG1 <- read.csv(sprintf("%sIVC/TablaBosquesNS.csv",dbdata.dir),stringsAsFactors=F)
rsm.MG2 <- read.csv(sprintf("%sIVC/TablaBosquesNS2.csv",dbdata.dir),stringsAsFactors=F)
rsm.MG3 <- read.csv(sprintf("%sIVC/TablaBosquesNS3.csv",dbdata.dir),stringsAsFactors=F)
rsm.MG4 <- read.csv(sprintf("%sIVC/TablaBosquesNS4.csv",dbdata.dir),stringsAsFactors=F)
rsm.MG5 <- read.csv(sprintf("%sIVC/TablaBosquesNS5.csv",dbdata.dir),stringsAsFactors=F)

sort(unique(trim(c(rsm.MG1$Codigo,rsm.MG2$Codigo,rsm.MG3$Codigo,rsm.MG4$Codigo,rsm.MG5$Codigo))))

## Spatial data


lista.paises <- read.dbf("/opt/gisdata/vectorial/DIVA/countries.dbf",as.is=T)
lista.ecoreg <- read.dbf("/opt/gisdata/vectorial/WWF/official/wwf_terr_ecos.dbf",as.is=T)


## Analysis description

## Results of analysis

confTest <- read.csv("~/Dropbox/MS/02_RondaN/FEE_RLE/20171130_TableAppendixA1_confidenceTests.csv",stringsAsFactors=F)

##datos de los MGs

##allMGs <- read.xls("/home/jferrer/Descargas/Tabla Original Macrogrupos.xlsx",as.is=T)
##allMGs <- read.csv("/media/jferrer/Elements/Provita/data/TablaOriginalMacrogrupos.csv",as.is=T)
rslts.NS <- read.xls("/opt/gisdata/Natureserve/IUCN/SAM/South_America_Results_IUCN_RLE_April2015.xlsx")
##rslts <- read.xls("~/Descargas/Tabla Bosques_NS.xlsx",sheet=6,as.is=T,encoding="utf8")

##defMG <- read.ods("/media/jferrer/Elements/Provita/data/TablaEspeciesMG.ods",stringsAsFactors=F)
##defMG$MG <- trim(gsub("\\.","",defMG$MG))
##refs <-  aggregate(defMG[,5:6],list(MG=defMG$MG),sum,na.rm=T)


    
##table(droplevels(subset(tipologia,grepl("^1",Division.Code))$formation))
##table(droplevels(tipologia$grp))
##table(tipologia$sgrp)

##otros datos
IUCN.subclass <- c("1.5 Subtropical/Tropical Dry Forest",
                   "1.6 Subtropical/Tropical Moist Lowland Forest",
                   "1.9 Subtropical/Tropical Moist Montane Forest",
                   "1.8 Subtropical/Tropical Swamp Forest",
                   "1.7 Subtropical/Tropical Mangrove Forest Vegetation Above High Tide Level",
                   "1.4 Temperate Forest",
                   "1.4 Temperate Forest",
                   "1.4 Temperate Forest")
names(IUCN.subclass) <- c("1.A.1","1.A.2","1.A.3","1.A.4","1.A.5",
                          "1.B.1","1.B.2","1.B.3")
                   


path <- "~/Provita/000_BDRLE"

cdgs <- confTest$Macrogroup.code


source(sprintf("%s/xml01/inc/Macrogroups/00_Macrogroups.R",path))
source(sprintf("%s/xml01/inc/Macrogroups/01_CaseStudies.R",path))

