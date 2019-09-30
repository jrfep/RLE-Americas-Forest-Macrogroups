
actuales$oamin <- unname(ifelse(is.na(sapply(actuales$oabounds,
                                             function(x) strsplit(x," -- ")[[1]][1])),
                                actuales$oacat,sapply(actuales$oabounds,function(x) strsplit(x," -- ")[[1]][1])))
actuales$oamax <- unname(ifelse(is.na(sapply(actuales$oabounds,
                                             function(x) strsplit(x," -- ")[[1]][2])),
                                actuales$oacat,sapply(actuales$oabounds,function(x) strsplit(x," -- ")[[1]][2])))


########
##tabla resultados: Threat scores por pais
#########
tabla.resultados <- with(actuales,tapply(mcdg,list(pais,factor(oacats,levels=names(IUCN.cats))),function(x) length(unique(x))))
tabla.resultados[is.na(tabla.resultados)] <- 0
Ts <- c()
for (k in 1:nrow(tabla.resultados))
    Ts <- c(Ts,weighted.mean(c(NA,5:0,NA),tabla.resultados[k,],na.rm=T))
Ts.pais <- data.frame(tabla.resultados,Ts)

tabla.resultados <- with(actuales,tapply(mcdg,list(pais,factor(oamin,levels=names(IUCN.cats))),function(x) length(unique(x))))
tabla.resultados[is.na(tabla.resultados)] <- 0
Ts <- c()
for (k in 1:nrow(tabla.resultados))
    Ts <- c(Ts,weighted.mean(c(NA,5:0,NA),tabla.resultados[k,],na.rm=T))
Ts.pais <- cbind(Ts.pais,Ts.n=Ts)

tabla.resultados <- with(actuales,tapply(mcdg,list(pais,factor(oamax,levels=names(IUCN.cats))),function(x) length(unique(x))))
tabla.resultados[is.na(tabla.resultados)] <- 0
Ts <- c()
for (k in 1:nrow(tabla.resultados))
    Ts <- c(Ts,weighted.mean(c(NA,5:0,NA),tabla.resultados[k,],na.rm=T))
Ts.pais <- cbind(Ts.pais,Ts.x=Ts)

Ts.pais$Tg.x <- Ts.pais$Tg.n <- Ts.pais$Tg <- NA
for (p in rownames(Ts.pais)) {
    slc <- subset(actuales,pais %in% p)$mcdg
    ss <- subset(actuales,mcdg %in% slc & pais %in% "global")
    tabla.resultados <- with(ss,tapply(mcdg,list(factor(oacats,levels=names(IUCN.cats))),function(x) length(unique(x))))
    tabla.resultados[is.na(tabla.resultados)] <- 0
    Ts.pais[p,"Tg"] <-     weighted.mean(c(NA,5:0,NA),tabla.resultados,na.rm=T)
    
    tabla.resultados <- with(ss,tapply(mcdg,list(factor(oamin,levels=names(IUCN.cats))),function(x) length(unique(x))))
    tabla.resultados[is.na(tabla.resultados)] <- 0
    Ts.pais[p,"Tg.n"] <-     weighted.mean(c(NA,5:0,NA),tabla.resultados,na.rm=T)
    
    tabla.resultados <- with(ss,tapply(mcdg,list(factor(oamax,levels=names(IUCN.cats))),function(x) length(unique(x))))
    tabla.resultados[is.na(tabla.resultados)] <- 0
    Ts.pais[p,"Tg.x"] <-     weighted.mean(c(NA,5:0,NA),tabla.resultados,na.rm=T)


}
Ts.pais <- Ts.pais[!rownames(Ts.pais) %in% "global",]




layout(1)
plot(Ts.pais$Ts,Ts.pais$Tg,xlim=c(0,5),ylim=c(0,5))
segments(Ts.pais$Ts.n,Ts.pais$Tg,
         Ts.pais$Ts.x,Ts.pais$Tg)
segments(Ts.pais$Ts,Ts.pais$Tg.n,
         Ts.pais$Ts,Ts.pais$Tg.x)
abline(0,1)

Ts.pais$grp[rownames(Ts.pais) %in% CAM] <- "CAM" 
Ts.pais$grp[rownames(Ts.pais) %in% CAR] <- "CAR" 
Ts.pais$grp[rownames(Ts.pais) %in% SAM] <- "SAM" 
Ts.pais$grp[rownames(Ts.pais) %in% c("Mexico",NAM)] <- "NAM" 

Ts.pais$grp <- factor(Ts.pais$grp,levels=c("SAM","CAR","CAM","NAM"))

layout(1)
par(mar=c(5,8,.5,2),yaxs="i")
oo <- order(Ts.pais$grp,Ts.pais$Tg)
yy <- 1:nrow(Ts.pais) + as.numeric(Ts.pais$grp[oo])

plot(Ts.pais$Tg[oo],yy,xlim=c(0,4),ylim=c(1,56),axes=F,pch=19,xlab="Threat score",ylab="")


axis(2,yy,
     gsub("Caicas","Caicos",gsub(", St Eustatius & Saba","",gsub(" the "," ", gsub("Islands","Is",gsub("Saint ","St ",gsub(" and "," & ",rownames(Ts.pais)[oo])))))),
     las=2,cex.axis=.65)

segments(Ts.pais$Tg.n[oo],yy,
         Ts.pais$Tg.x[oo],yy)
points(Ts.pais$Ts[oo],yy,xlim=c(0,5),col=1,pch=1,lwd=2)
##segments(Ts.pais$Ts.n[oo],yy,
##         Ts.pais$Ts.x[oo],yy,col="pink")

abline(h=c(15,44,52))
axis(4,c(7.5,29.5,48,54),c("South America","Caribbean","Central","North"),srt=90,tick=F,line=-1,cex.axis=.75)
axis(1)
box()
dev.copy(pdf,file=sprintf("~/Provita/etc/MS/%s_Figure5.pdf",hoy),width=4,height=6)
dev.off()




layout(1)
par(mar=c(5,8,.5,.5),yaxs="i")
oo <- order(Ts.pais$grp,Ts.pais$Ts)
yy <- 1:nrow(Ts.pais) + as.numeric(Ts.pais$grp[oo])

plot(Ts.pais$Tg[oo],yy,xlim=c(0,4),ylim=c(1,56),axes=F,pch=NA,xlab="Threat score",ylab="")
axis(2,yy,
     gsub("Caicas","Caicos",gsub(", St Eustatius & Saba","",gsub(" the "," ", gsub("Islands","Is",gsub("Saint ","St ",gsub(" and "," & ",rownames(Ts.pais)[oo])))))),
     las=2,cex.axis=.65)
points(Ts.pais$Ts[oo],yy,xlim=c(0,5),col=2,pch=19)
segments(Ts.pais$Ts.n[oo],yy,
         Ts.pais$Ts.x[oo],yy,col="pink")

points(acciones$Ts[match(rownames(Ts.pais)[oo],acciones$pais)],yy)
axis(1)
box()
abline(h=c(15,44,52))

Ts.pais$Ts.opt <- acciones$Ts[match(rownames(Ts.pais),acciones$pais)]

dev.copy(pdf,file=sprintf("~/Provita/etc/MS/%s_Figure5A.pdf",hoy),width=4,height=6)
dev.off()

print(cor.test(Ts.pais$Ts,Ts.pais$Tg,method="kendall"))

print(cor.test(Ts.pais$Ts,Ts.pais$Ts.opt,method="kendall"))

print(t.test(Ts.pais$Ts,Ts.pais$Ts.opt,mu=0.5,paired=T,alternative="g",var.equal=T))

Ts <- data.frame()
for (kk in unique(resumen.global$frmt)) {
    ss <- resumen.global$frmt %in% kk
    Ts <- rbind(Ts,
                data.frame(formation=kk,
                           Ts.m=weighted.mean(c(5,4,3,2,1,0),table(factor(cp[ss],levels=names(IUCNclr)))[1:6],na.rm=T),
                           Ts.n=weighted.mean(c(5,4,3,2,1,0),table(factor(cn[ss],levels=names(IUCNclr)))[1:6],na.rm=T),
                           Ts.x=weighted.mean(c(5,4,3,2,1,0),table(factor(cx[ss],levels=names(IUCNclr)))[1:6],na.rm=T)))
}


