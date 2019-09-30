##~/bin/basex/bin/basexgui &

##R --vanilla
require(ROpenOffice)
require(raster)
require(gdata)
require(foreign)
setwd("~/tmp/Provita")

dbdata.dir <- "~/Provita/data/RLEDB/"

RLEDB <- read.ods(sprintf("%s/RLE assessment summary tables.ods",dbdata.dir))

HQ.DB <-read.xls(sprintf("%s/RLE_Last_Version_database_HQ_November2017.xlsx",dbdata.dir),sheet=2,stringsAsFactors=F)

Jess.DB <-read.xls(sprintf("%s/RLE_database_16Nov2017_Jess.xlsx",dbdata.dir),sheet=1,stringsAsFactors=F,fileEncoding="latin1")


##output.dir <- "~/Dropbox/Provita/xml/RLE_test1"
output.dir <- "~/Provita/xml/RLE_Neotropics"

##### considerar estas referencias
##http://www.iucnredlist.org/technical-documents/classification-schemes/habitats-classification-scheme-ver3
##http://s3.amazonaws.com/iucnredlist-newcms/staging/public/attachments/3125/dec_2012_guidance_habitats_classification_scheme.pdf

##http://www.iucnredlist.org/technical-documents/classification-schemes/threats-classification-scheme
##http://s3.amazonaws.com/iucnredlist-newcms/staging/public/attachments/3127/dec_2012_guidance_threats_classification_scheme.pdf

########
## paso 1 ## generar xml para Macrogrupos, Chile (1 y 2), Colombia (1 y 2), Mexico, Costa Rica, Nicaragua, Venezuela (1 y 2), Brasil, Casos de Estudio Latinoamericanos
########

########
## chile
## Provita/000_BDRLE/xml01/xml01_Chile.R #(1 y 2)
## costa rica
## Provita/000_BDRLE/xml01/xml01_CostaRica.R 
## el salvador
## Provita/000_BDRLE/xml01/xml01_ElSalvador.R 


## colombia
## Provita/000_BDRLE/xml01/xml01_Colombia.R #(1 y 2)
## venezuela
## Provita/000_BDRLE/xml01/xml01_Venezuela.R #(1 y 2)

########
## paso 2 ## 
########
