##R --vanilla
setwd("~/tmp/Provita")
require(foreign)
require(gdata)
require(vegan)
require(xtable)
require(rgeos)
require(rgdal)
require(XML)
require(RColorBrewer)

hoy <- format(Sys.time(), "%Y%m%d")

mi.path <- "~/doc/"
mi.path <- "~/Dropbox/Provita/doc/"
mi.path <- "~/Provita_JRFP/doc/"
mi.dir <- "000_BDRLE"


mi.arch <- "Document1_RLE_XMLguide"
titulo <- "Report1_RLE_XMLdevelopment"

xml.db <-  "~/Provita_JRFP/xml/"
cdg.doc <- sprintf("Provita.RLE.2017.%s",1) ## colocar código aquí...


options(width=80)
##system(sprintf("rm %s.*",mi.arch))
Sweave(file=paste(mi.path,mi.dir,"/",mi.arch,".Rnw",sep=""),eps=F)
##Stangle(file=paste(mi.path,mi.dir,"/",mi.arch,".Rnw",sep=""))
tools::texi2dvi(paste(mi.arch,".tex",sep=""), pdf=TRUE)

##system(sprintf("evince %s.pdf &",mi.arch))
##system(sprintf("mv %s.pdf %s/%s/%s_%s.pdf",mi.arch,mi.path,mi.dir,hoy,titulo))

