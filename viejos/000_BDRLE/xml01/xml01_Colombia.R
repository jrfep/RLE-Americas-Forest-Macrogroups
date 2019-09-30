################
## Colombia
###############
colombia <- shapefile("~/respaldoAmericasDataFiles/EcoOri_12052015_2014_TodosCriterios.shp")

path <- "~/Dropbox/Provita/doc/000_BDRLE"
source(sprintf("%s/xml01_Colombia00_ecosystems.R",path))
source(sprintf("%s/xml01_Colombia01_CaseStudies.R",path))
