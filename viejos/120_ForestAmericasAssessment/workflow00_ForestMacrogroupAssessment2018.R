## versión revisada diciembre 2017; valido en enero 2018

## preambulo en grass
## inc00_crosstablesSAM.R
## inc00_crosstablesNAC.R

##R --vanilla
setwd("~/tmp")
require(foreign)
require(gdata)
require(vegan)
require(xtable)
require(rgeos)
require(rgdal)
require(plotrix)
 require(raster)
require(ROpenOffice)
##require(spgrass6)
require(redlistr)

hoy <- format(Sys.time(), "%Y%m%d")
##g6 <- gmeta()

mi.path <- "~/Provita/doc/"
if (!file.exists(mi.path))
    mi.path <- "~/Provita_JRFP/doc/"
if (!file.exists(mi.path))
    mi.path <- "~/Dropbox/Provita/doc/"

mi.gis.dir <- "/home/jferrer/mi.gis"
if (!file.exists(mi.gis.dir))
    mi.gis.dir <- "/media/user3/DATOS/00_LRE/mi.gis/"
mi.gisdata.dir <- "/opt/gisdata"
if (!file.exists(mi.gisdata.dir))
    mi.gisdata.dir <- "/media/user3/DATOS/00_LRE/gisdata/"


mi.dir <- "000_RLE"
mi.dir <- "001_ForestAmericasAssessment"
mi.dir <- "120_ForestAmericasAssessment"

###########
## preliminary code and functions
###########
source(sprintf("%s/%s/inc00_preambuloAnalisis.R",mi.path,mi.dir))
source(sprintf("%s/%s/inc30_funciones.R",mi.path,mi.dir))

IUCN.cats <- c("Data Deficient","Collapsed","Critically Endangered","Endangered","Vulnerable","Near Threatened","Least Concern","Not Evaluated")
names(IUCN.cats) <- c("DD","CO","CR","EN","VU","NT","LC","NE")

## selección de macrogrupos
##tds.cdg <- sort(sprintf("M%03d",unique(r2$mg)))
##cdgs <- tds.cdg
##cdgs <- tds.cdg[tds.cdg %in% unique(drop.levels(subset(tipologia,grp==1 & !format %in% c("1.B.4","1.B.5") & macrogroup_key!="")$macrogroup_key))]

cdgs <- levels(rsm$mcdg)


###############
## These scripts sets up indicator measures and apply spatial and functional criteria for each Macrogroup
################
source(sprintf("%s/%s/sesion01_prepararEvaluacion.R",mi.path,mi.dir))
source(sprintf("%s/%s/inc25_spatialindicators_categories.R",mi.path,mi.dir))
source(sprintf("%s/%s/inc26_climateensemble_forest.R",mi.path,mi.dir))


## not yet ready...
##source(sprintf("%s/%s/inc70_tablaGlobal.R",mi.path,mi.dir))


###############
## This code and sourced scripts reassign criteria under scenarios of action measured in area-cost the base-line is calculated for cost=0
################

todos <- actuales <- posibles <- data.frame()
##confTest <- read.csv("~/Dropbox/MS/02_RondaN/FEE_RLE/20171130_TableAppendixA1_confidenceTests.csv",stringsAsFactors=F)
##excluir  <- subset(confTest,!Included.in.analysis)$Macrogroup.code
excluir <- c("M562","M573","M600","M614","M618","M650","M013",
              "M653","M655","M656","M659","M028","M154","M664")

for (j in unique(b.pres$pais)) {
    jj <- j
    if (j %in% "Virgin Islands")
        jj <- "United States Virgin Islands"
    if (j %in% "Turks and Caicas Islands")
        jj <- "Turks and Caicos Islands"
    if (j %in% c("Aruba","Curaçao","Bonaire, Saint Eustatius and Saba"))
        jj <- "Netherlands Antilles"
    mms <- unique(c(as.character(subset(b.pres,pais %in% j)$mcdg),as.character(subset(b.hist,pais %in% j)$mcdg),as.character(subset(d.pres,pais %in% j)$mcdg),as.character(subset(d.hist,pais %in% j)$mcdg),as.character(subset(w.pres,pais %in% j)$mcdg),as.character(subset(AOOs,pais %in% j)$mcdg,subset(c.futr,pais %in% jj)$mcdg)))

    for (mm in mms) {
        source(sprintf("%s/%s/inc80_scenarios_spatialindicators.R",mi.path,mi.dir))
    }
}

actuales <- subset(todos,costo==0)

table(subset(actuales,oacats %in% "CR")$oabounds)
table(subset(actuales,oacats %in% "EN")$oabounds)
table(subset(actuales,oacats %in% "VU")$oabounds)
table(subset(actuales,oacats %in% "NT")$oabounds)
table(subset(actuales,oacats %in% "LC")$oabounds)


###############
## This is a consitency check for the distribution of macrogroups (was necessary to debug old code)
################


for (mm in sort(unique(as.character(actuales$mcdg)))) {
    cat(sprintf("\n(%s) %s:",mm,
                subset(tipologia,macrogroup_key %in% mm)$macrogroup_name))
    cat(paste(unique(subset(actuales,mcdg %in% mm)$pais),collapse=","))
    
}

##Sospechosos
##(M013) Eastern North American Ruderal Forest:United States,Mexico ## debería ser Madrean... 


###############
## This is the sourced code for the summary table
## (check changes with previous version)
################
referencia <- "20180113"
referencia <- "20180317"
source(sprintf("%s/%s/sesion02_resumenResultados.R",mi.path,mi.dir))

table(unlist(strsplit(subset(resumen.global,oacats %in% "EN")$tcats,", ")))
 table(unlist(strsplit(subset(resumen.global,oacats %in% "CR")$tcats,", ")))




## comparar con resultados viejos...
##resumen.global <- read.csv(sprintf("%s/MGs/%s_resumen_resultados.csv",gsub("doc","output",mi.path),"20180115"))
##table(subset(resumen.global, mcdg %in% subset(dts,cf1>1 & p2051>1)$mcdg)$A2a)
##table(subset(resumen.global, mcdg %in% subset(dts,cf1<1 & p2051<1)$mcdg)$A2a)
##table(subset(resumen.global, mcdg %in% subset(dts,cf1>1 & p2051<1)$mcdg)$A2a)
##table(subset(resumen.global, mcdg %in% subset(dts,cf1<1 & p2051>1)$mcdg)$A2a)

###############
## This code outputs summaries in plain text for inclusion in the manuscript
################

source(sprintf("%s/%s/sesion03_summaryResults.R",mi.path,mi.dir))

###############
## This code generates Figure showing historic rate of change
## (Fig. 1)
################

source(sprintf("%s/%s/sesion04_FiguresHistoricDeforestation.R",mi.path,mi.dir))


###############
## This code generates Figures showing decline/mean severity for each criterion
## (Figs. 2 and 3)
################

source(sprintf("%s/%s/sesion04_FiguresCriteria.R",mi.path,mi.dir))

source(sprintf("%s/%s/sesion04_ResumenResultados.R",mi.path,mi.dir))


###############
## This code generates a color table for the final category map
## Map output through grass (Fig. 4)
################

source(sprintf("%s/%s/sesion04_ColorTableMap.R",mi.path,mi.dir))

###############
## This code outputs summaries in plain text for inclusion in the manuscript
################

source(sprintf("%s/%s/sesion04_ResumenResultados.R",mi.path,mi.dir))


##########
## This source performs the cost eficiency analysis
################

set.seed("32452598")
 dev.off()
layout(matrix(1:25,ncol=5))
source(sprintf("%s/%s/sesion05_costEfficiencyCalculation.R",mi.path,mi.dir))

##########
## ... and this produces the corresponding figure
################

source(sprintf("%s/%s/sesion06_figureCostEfficiency.R",mi.path,mi.dir))

table(apply(tt,1,which.max))
##"restore.forest"     "halt.deforestation" "protect.restricted"
##18                       6                     9
##"climate.adaptation" "water.protection"   "resource.use"
##5                        1 (Honduras)         6
##"over.explotation" 
##1 (Guatemala)

CAR[!CAR %in% acciones$pais]
SAM[!SAM %in% acciones$pais]
NAM[!NAM %in% acciones$pais]
CAM[!CAM %in% acciones$pais]



############
## threat scores by formation, country: continental vs country level
## tabla resultados: Threat scores por pais 
############

source(sprintf("%s/%s/sesion07_threatScores.R",mi.path,mi.dir))

subset(actuales,grepl("D",tcats) & pais %in% "global")
