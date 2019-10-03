#! R --vanilla
require(raster)
## working directory and path to scripts
script.dir <- Sys.getenv("SCRIPTDIR")
work.dir <- Sys.getenv("WORKDIR")
gis.data <- Sys.getenv("GISDATA")
setwd(work.dir)

rAll <- raster("~/tmp/IUCN/IVC_NS_v7_270m_robin.tif")
r0 <- rAll
mcdg <- values(rAll) %in% 134
 plot(mcdg)
