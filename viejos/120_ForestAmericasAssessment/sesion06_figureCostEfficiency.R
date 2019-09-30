paises.robin <- spTransform(subset(TMWB,NAME %in% c(NAM,CAM,CAR,SAM)),"+proj=robin +lon_0=-80 +x_0=0 +y_0=0 +no_defs +a=6378137 +rf=298.257223563 +towgs84=0.000,0.000,0.000 +to_meter=1")

xy.r <- project(as.matrix(paises.robin@data[,c("LON","LAT")]),"+proj=robin +lon_0=-80 +x_0=0 +y_0=0 +no_defs +a=6378137 +rf=298.257223563 +towgs84=0.000,0.000,0.000 +to_meter=1")

paises.robin@data[,c("LON","LAT")] <- xy.r


tt <- acciones[acciones$pais %in% c(NAM,CAM,CAR,SAM),
               c(grep("costo.[A-D]",colnames(acciones)))]
tt <- tt/acciones[acciones$pais %in% c(NAM,CAM,CAR,SAM),"costo"]
rownames(tt) <- acciones[acciones$pais %in% c(NAM,CAM,CAR,SAM),"pais"]

rownames(tt) <- gsub("Caicas","Caicos",rownames(tt))
rownames(tt) <- gsub("Bonaire, Saint Eustatius and Saba","Bonaire",rownames(tt))


tt$restore.forest <- with(tt,apply(data.frame(costo.A1,costo.A3),1,max))
##with(tt,costo.A1+costo.A3) ## ¿suma o máximo?
tt$halt.deforestation <- with(tt,costo.A2a)
tt$protect.restricted <- with(tt,apply(data.frame(costo.B1,costo.B2,costo.B3),1,max))
##with(tt,costo.B1+costo.B2+costo.B3)## ¿suma o máximo?
tt$climate.adaptation <- tt$costo.C2a
tt$water.protection <- tt$costo.C2b
tt$resource.use <-  with(tt,apply(data.frame(costo.D1,costo.D3),1,max))
##with(tt,costo.D1+costo.D3)## ¿suma o máximo?
tt$over.explotation <- tt$costo.D2b
tt <- tt[,!grepl("costo",colnames(tt))]

##locs <- paises.robin@data[match(rownames(tt),paises.robin@data$NAME),c("NAME","LON","LAT")]
locs <- read.csv(sprintf("%s/IVC/CoordenadasMapas.csv",gsub("doc","data",mi.path)),sep=";",dec=",")
locs <- locs[match(rownames(tt),locs$Name),]

##xy.r <- project(as.matrix(locs[,c("lon","lat")]),"+proj=robin +lon_0=-80 +x_0=0 +y_0=0 +no_defs +a=6378137 +rf=298.257223563 +towgs84=0.000,0.000,0.000 +to_meter=1")
##locs[,c("lon","lat")] <- xy.r
##xy.r <- project(as.matrix(locs[,c("x","y")]),"+proj=robin +lon_0=-80 +x_0=0 +y_0=0 +no_defs +a=6378137 +rf=298.257223563 +towgs84=0.000,0.000,0.000 +to_meter=1")
##locs[,c("x","y")] <- xy.r


clrs <- rev(RColorBrewer::brewer.pal(9,"PiYG"))[c(1:3,6:9)]
clrs <- rev(RColorBrewer::brewer.pal(7,"Accent"))[c(1,7,4,6,3,5,2)]
par(mar=c(0,0,0,0))
plot(subset(paises.robin,NAME %in% c(CAM,NAM,CAR,SAM)),
     xlim=c(-4600000,5000000),ylim=c(-5900000, 6400000))
segments(locs$lon,locs$lat,locs$x,locs$y,lty=2,col="grey22")
inset.x <- 3329862
inset.y <- 5389647
for (k in 1:nrow(tt)) {
    ccs <- rainbow(ncol(tt))[tt[k,]>0]
    ccs <- clrs[tt[k,]>0]
    y <- tt[k,tt[k,]>0]
    if (!is.na(locs[k,"x"])) {
        floating.pie(locs[k,"x"],locs[k,"y"],t(tt[k,]),radius=3.5e5,col=ccs)
    } else {
        if (sum(tt[k,])>0) {
            nmbr <- gsub(" the "," ",gsub("Saint","St",gsub("Islands","Is",gsub(" and "," & ",rownames(tt[k,])))))
            floating.pie(inset.x,inset.y,t(tt[k,]),radius=2e5,col=ccs)
            text(inset.x,inset.y-3e5,nmbr,cex=.74,xpd=NA)
            if (nmbr %in% "St Lucia") {
                inset.x <-4738464 
                inset.y <-2392516 
            } else {
                inset.y <- inset.y-6e5
            }
        }
    }
        
}
legend(-3882179,-1559952,fill=clrs,
       c("Forest restoration","Halting deforestation","Protecting restricted types","Climate adaptation","Water protection","Manage resource use","Control over-exploitation"),
       ##sapply(gsub("\\."," ",gsub("costo.","",colnames(tt))),function(x) paste(toupper(substr(x,1,1)),substr(x,2,20),sep="")),
       ncol=1,inset=-.05,xpd=T)
dev.copy(pdf,file=sprintf("%s/MS/20180319_Figure6.pdf",sub("doc","etc",mi.path)),width=10,height=15)
dev.off()
