
## leer tabla bsq
for (k in 2001:2012) {
    if (!file.exists(sprintf("%1$s/Paises_AOO_bsq%2$s",output.dir,k))) {
        system(sprintf("grep ^%1$s calculos_201712/SAM_Paises_AOO_bsq%2$s.tab > %3$s/Paises_AOO_bsq%2$s",cdg,k,output.dir))
        system(sprintf("grep ^%1$s calculos_201712/NAC_Paises_AOO_bsq%2$s.tab >> %3$s/Paises_AOO_bsq%2$s",cdg,k,output.dir))
    }
    tmp2 <- read.table(sprintf("%s/Paises_AOO_bsq%s",output.dir,k))
    tmp2 <- subset(tmp2,V1 %in% cdg)
    assign(sprintf("bsq.%s",k),
           tmp2)
    rm(tmp2)
    gc()
}
## leer tabla ant
for (k in 17:20) {
    if (!file.exists(sprintf("%1$s/Paises_AOO_ant%2$s00",output.dir,k))) {
        system(sprintf("grep ^%1$s calculos_201712/SAM_Paises_AOO_Anthromes_%2$s00.tab > %3$s/Paises_AOO_ant%2$s00",cdg,k,output.dir))
        system(sprintf("grep ^%1$s calculos_201712/NAC_Paises_AOO_Anthromes_%2$s00.tab >> %3$s/Paises_AOO_ant%2$s00",cdg,k,output.dir))
        gc()
    }
    tmp2 <- read.table(sprintf("%s/Paises_AOO_ant%s00",output.dir,k))
    tmp2 <- subset(tmp2,V1 %in% cdg)
    assign(sprintf("ant.%s00",k),
           tmp2)
    rm(tmp2)
    gc()
}

##leer tabla RR
k <- "RS"
if (!file.exists(sprintf("%1$s/Paises_%2$s",output.dir,k))) {
    system(sprintf("grep ^%1$s calculos_201712/SAM_Paises_%2$s.tab > %3$s/Paises_%2$s",cdg,k,output.dir))
    system(sprintf("grep ^%1$s calculos_201712/NAC_Paises_%2$s.tab >> %3$s/Paises_%2$s",cdg,k,output.dir))
}
tmp2 <- read.table(sprintf("%s/Paises_%s",output.dir,k))
tmp2 <- subset(tmp2,!(V2 %in% "*") & !(V3 %in% "*"))
tmp2 <- subset(tmp2,V1 %in% cdg)
assign(sprintf("caz.%s",k),
       tmp2)
rm(tmp2)
gc()

##leer tabla GWS
k <- "GWSchange"
if (!file.exists(sprintf("%1$s/Paises_%2$s",output.dir,k))) {
    system(sprintf("grep ^%1$s calculos_201712/SAM_Paises_%2$s.tab > %3$s/Paises_%2$s",cdg,k,output.dir))
    system(sprintf("grep ^%1$s calculos_201712/NAC_Paises_%2$s.tab >> %3$s/Paises_%2$s",cdg,k,output.dir))
}
tmp2 <- read.table(sprintf("%s/Paises_%s",output.dir,k))
tmp2 <- subset(tmp2,!(V2 %in% "*") & !(V3 %in% "*"))
tmp2 <- subset(tmp2,V1 %in% cdg)
gws <- tmp2
rm(tmp2)
gc()


##leer tabla cambios en Anthromes
k <- "Anthromes_Changes"
if (!file.exists(sprintf("%1$s/Paises_%2$s",output.dir,k))) {
    system(sprintf("grep ^%1$s calculos_201712/SAM_Paises_%2$s.tab > %3$s/Paises_%2$s",cdg,k,output.dir))
    system(sprintf("grep ^%1$s calculos_201712/NAC_Paises_%2$s.tab >> %3$s/Paises_%2$s",cdg,k,output.dir))
}
tmp2 <- read.table(sprintf("%s/Paises_%s",output.dir,k))
tmp2 <- subset(tmp2,!(V2 %in% "*") & !(V3 %in% "*"))
tmp2 <- subset(tmp2,V1 %in% cdg)

antchange <- tmp2
rm(tmp2)
gc()


##shapefiles
lst.shps <- dir(output.dir,"shp",full.names=T)
for (arch in lst.shps) {
##    shp <- shapefile(arch)
##    shp.ll <- spTransform(shp,TMWB@proj4string)
##    over(shp
}
