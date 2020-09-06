##R --vanilla
require(dplyr)
require("RPostgreSQL")
require(foreign)
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "IUCN",
                 host = "localhost", port = 5432,
                 user = "jferrer")



work.dir <- Sys.getenv("WORKDIR")
setwd(work.dir)
## make sure to have the NatureServe files extracted here
## inc/gdal/merge_input_to_IVC_raster.sh

tmp1 <- read.dbf("commondata/raster_data/NorthAmerica_Caribbean_IVC_MacroGroups_potential_NatureServe_v5_270m.tif.vat.dbf")
tmp2 <- read.dbf("commondata/raster_data/SouthAmerica_IVC_MacroGroups_potential_NatureServe_v7_270m.tif.vat.dbf")
total.cols <- colnames(tmp1)[colnames(tmp1) %in% colnames(tmp2)]
tmp <- rbind(tmp1[,total.cols],
  tmp2[,total.cols])

dim(tmp)
unique(tmp$mg_key)
with(tmp,aggregate(Count,list(Value,mg_key),sum))

## https://stackoverflow.com/questions/39181208/how-to-group-by-all-but-one-columns
## https://datacarpentry.org/R-genomics/04-dplyr.html
## https://www3.nd.edu/~steve/computing_with_data/24_dplyr/dplyr.html

IVC_eco = tmp %>%
  filter(!is.na(mg_key)) %>%
    group_by_at(names(tmp)[-grep("Count", names(tmp))]) %>%
      summarise(total = sum(Count, na.rm = TRUE))

##apply(IVC_eco,2,function(x) max(nchar(as.character(x)),na.rm=T))

dbWriteTable(con,"tmptable3",IVC_eco,overwrite=T,row.names = FALSE)
qry <- "INSERT INTO ivc_americas SELECT * FROM tmptable3 ON CONFLICT (mg_key) DO NOTHING"
dbSendQuery(con,qry)
qry <- "DROP TABLE tmptable3"
dbSendQuery(con,qry)

## qry <- "SELECT macrogroup, mg_key,total from ivc_americas where ivc_class like '%Forest%'"
##  rs <- dbSendQuery(con,qry)
##  rslts <- dbFetch(rs)
##  dbClearResult(rs)

dbDisconnect(con)
