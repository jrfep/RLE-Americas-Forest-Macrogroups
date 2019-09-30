

### for the whole country, threat score, total area

CAR <- as.character(subset(paises,UNREG1 %in% "Caribbean")$NAME)
SAM <- as.character(subset(paises,UNREG1 %in% "South America")$NAME)
NAM <- as.character(subset(paises,UNREG1 %in% "Northern America")$NAME)
CAM <- as.character(subset(paises,UNREG1 %in% "Central America")$NAME)

acciones <- data.frame()
for (j in unique(todos$pais)) {
    ss <- subset(todos,pais %in% j & costo==0)
    kk <- ss[!duplicated(ss$mcdg),]

    boots <- data.frame(pais=j,Ts= weighted.mean(c(NA,5:0,NA),table(factor(kk$oacats,levels=names(IUCN.cats))),na.rm=T),
                        t(colSums(kk[,grep("costo",colnames(kk))],na.rm=T)))

    ss <- subset(todos,pais %in% j)
    ss <- ss[!duplicated(ss[,c("mcdg","oacats")]),]

    for (i in 1:10000) {
        kk <- ss[  sample(1:nrow(ss)),]
        kk <- kk[!duplicated(kk$mcdg),]
        
        boots <- rbind(boots,data.frame(pais=j,Ts= weighted.mean(c(NA,5:0,NA),table(factor(kk$oacats,levels=names(IUCN.cats))),na.rm=T),
                                        t(colSums(kk[,grep("costo",colnames(kk))],na.rm=T))))

    }
    boots$costo.relativo <- boots$costo/subset(paises,NAME %in% j)$SQKM
    boots$Ts.relativo <- 1-(boots$Ts/boots$Ts[1])
    if (j %in% c("Uruguay","French Guiana","Anguilla","Aruba",
                 "Jamaica", "Saint Barthélemy", "Saint Martin",
                 "Sint Maarten")) {
        min.costo <- 0.0000045
    } else {
        min.costo <- 0.0045
    }
        boots <- subset(boots,costo.relativo>min.costo & costo.relativo < 0.5 & Ts.relativo>0)
    if (nrow(boots)>0) {
        plot(costo.relativo~Ts.relativo,boots,main=j)
        points(costo.relativo~Ts.relativo,boots[which.min(boots$costo.relativo/boots$Ts.relativo),],col=2,cex=2,pch=19)
        
        acciones <- rbind(acciones,boots[which.min(boots$costo.relativo/boots$Ts.relativo),])
    }

}
round(acciones$costo.relativo,3)
summary(acciones$costo.relativo,3)

## actions (maybe use http://cmp-openstandards.org/using-os/tools/actions-taxonomy/)
##A1,A3  =>reforest historical losses 2.3 Habitat & Natural Process Restoration
##A2a,A2b halt current deforestation 1.2 Resource & Habitat Protection
##B1,B2,B3 => protect restricted ecosystems 1.2 Resource & Habitat Protection
##C2a => adapt to climate change 
##C2b => protect water resources
##D1,D3 => diminish natural resource use
##D2 => diminish overexplotaition of animal resources
##mosaicplot(acciones[,c(grep("costo.[A-D]",colnames(acciones)))])
