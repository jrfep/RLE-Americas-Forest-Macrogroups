tt <- acciones[acciones$pais %in% SAM,
               c(grep("costo.[A-D]",colnames(acciones)))]
tt <- tt/acciones[acciones$pais %in% SAM,"costo"]
rownames(tt) <- acciones[acciones$pais %in% SAM,"pais"]


tt$restore.forest <- with(tt,apply(data.frame(costo.A1,costo.A3),1,max))
##with(tt,costo.A1+costo.A3) ## ¿suma o máximo?
tt$halt.deforestation <- with(tt,costo.A2a)
tt$protect.restricted <- with(tt,costo.B1+costo.B2+costo.B3) ## ¿suma o máximo?
tt$climate.adaptation <- tt$costo.C2a
tt$water.protection <- tt$costo.C2b
tt$resource.use <- with(tt,costo.D1+costo.D3) ## ¿suma o máximo?
tt$over.explotation <- tt$costo.D2b

tt <- tt[,!grepl("costo",colnames(tt))]

##stars(tt,draw.segments=T,col.segments=rainbow(ncol(tt)),locations= TMWB@data[match(rownames(tt),TMWB@data$NAME),c("LON","LAT")],len=5,add=T)
## probar si tiene más sentido así

##stars(tt,draw.segments=T,col.segments=rainbow(ncol(tt)),locations= paises.robin@data[match(rownames(tt),paises.robin@data$NAME),c("LON","LAT")],len=5,add=T)
## probar si tiene más sentido así
##plot(subset(paises.robin,NAME %in% c(CAM,NAM,CAR,SAM)),xlim=c(-90,-60))
##segments(locs$lon,locs$lat,locs$x,locs$y,lty=2,col="grey22")
##stars(sqrt(tt),draw.segments=T,col.segments=rainbow(ncol(tt)),locations=locs[,c("x","y")] ,len=5e4,scale=F,add=T,label="")
##legend("bottomleft",fill=rainbow(ncol(tt)),gsub("costo.","",colnames(tt)),ncol=1,inset=-.05,xpd=T)

colSums(tt>0.5)
colSums(tt[rownames(tt) %in% CAR,])
colSums(tt[rownames(tt) %in% CAM[-7],])
colSums(tt[rownames(tt) %in% SAM,])
colSums(tt[rownames(tt) %in% c(NAM,CAM[7]),])
table(rowSums(tt>0))

rowSums(tt[,1:3])/rowSums(tt)
table(rowSums(tt[,1:3])/rowSums(tt)>.8)
tt[(rowSums(tt[,1:3],na.rm=T)/rowSums(tt,na.rm=T)>.8),]

tipologia[match(resumen.global[grep("C2a",resumen.global$tcats),"mcdg"],tipologia$macrogroup_key),]
tipologia[match(resumen.global[grep("C2b",resumen.global$tcats),"mcdg"],tipologia$macrogroup_key),"macrogroup_name"]


ss <- subset(actuales,pais %in% "Honduras")
ss$nombre <- tipologia[match(ss$mcdg,tipologia$macrogroup_key),"macrogroup_name"]
subset(ss,oacats %in% "EN")

ss <- subset(actuales,pais %in% "Venezuela")
ss$nombre <- tipologia[match(ss$mcdg,tipologia$macrogroup_key),"macrogroup_name"]
ss <- subset(actuales,pais %in% "Colombia")
ss$nombre <- tipologia[match(ss$mcdg,tipologia$macrogroup_key),"macrogroup_name"]

subset(ss,oacats %in% c("CR","EN"))
## problem: why these in Venezuela? similar in Colombia (all MG<100 except the mangroves)
##Central & Appalachian Floodplain Forest
##                Southern Mesic Mixed Broadleaf Forest
##                               Longleaf Pine Woodland
subset(ss,tcats %in% c("C2b"))


ss <- subset(actuales,pais %in% "Chile")
ss$nombre <- tipologia[match(ss$mcdg,tipologia$macrogroup_key),"macrogroup_name"]

subset(ss,oacats %in% c("CR","EN"))
## problem: why these in Venezuela?
##Central & Appalachian Floodplain Forest
##                Southern Mesic Mixed Broadleaf Forest
##                               Longleaf Pine Woodland
subset(ss,tcats %in% c("C2b"))



