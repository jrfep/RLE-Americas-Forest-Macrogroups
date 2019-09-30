################
## El Salvador
###############

###############
## INPUTS
################

## Publication in pdf, supporting information (xls, doc, tif)
el.salvador <- subset(RLEDB[[2]],reference_code %in% "Crespin_RLE_ElSalvador_2015")


ES.2 <- subset(Jess.DB,Reference_code %in% "Crespin_RLE_ElSalvador_2015")
##R0 <- raster("~/Documentos/Mendeley Desktop/Crespin, Simonetti/2015/Austral Ecology/Crespin, Simonetti_2015_Predicting ecosystem collapse Spatial factors that influence risks to tropical ecosystems.tif") ## no real spatial information

ES.info <- read.ods("~/Provita/data/RLEDB/CaseStudyDetails_Crespin2015.ods",stringsAsFactor=F)[[1]]

path <- "~/Provita/000_BDRLE"
source(sprintf("%s/xml01/inc/ElSalvador/00_ElSalvador_ecosystems.R",path))
source(sprintf("%s/xml01/inc/ElSalvador/01_ElSalvador_CaseStudies.R",path))

