################
## Costa Rica
###############

###############
## INPUTS
################

## Informe en Pdf

##información extraida del informe
costa.rica <- subset(RLEDB[[2]],reference_code %in% "Herrera_RLE_CostaRica_2015")
CR.2 <- subset(HQ.DB,Reference_code %in% "Herrera_RLE_CostaRica_2015")

CR.info <- read.ods("~/Provita/data/RLEDB/CaseStudyDetails_Herrera2015.ods",stringsAsFactor=F)


path <- "~/Dropbox/Provita/doc/000_BDRLE"
path <- "~/Provita/000_BDRLE"
source(sprintf("%s/xml01/inc/CostaRica/00_costaRica_ecosystems.R",path))
source(sprintf("%s/xml01/inc/CostaRica/01_costaRica_CaseStudies.R",path))
