################
## Fig 1: Historical rates of decline
#################

## changes per formation, division and macrogroup


################
## Fig 2: Recent and current rates of decline
#################

xx <- merge(subset(b.hist,pais %in% "global"),subset(b.pres,pais %in% "global"),by="mcdg")
xx$p2062[xx$p2062>1] <- 1.05
plot(p2062~p1950, xx,type="n",xlim=c(0,1),ylim=c(0,1.1),xlab="Proportion of woodland area loss between 1950 and 2000 (estimated)",ylab="Proportion of area of forest cover to be lost between 2012 and 2062 (projected) ")
rect(0,0,0.7,1.1,col=IUCNclr["VU"],border=NA)
rect(0,0,1,0.7,col=IUCNclr["VU"],border=NA)
rect(0,0,0.5,1.1,col=IUCNclr["EN"],border=NA)
rect(0,0,1,0.5,col=IUCNclr["EN"],border=NA)
rect(0,0,0.2,1.1,col=IUCNclr["CR"],border=NA)
rect(0,0,1,0.2,col=IUCNclr["CR"],border=NA)
points(p2062~p1950, xx)

dev.copy(pdf,file="~/Provita/etc/MS/Figure2_draft.pdf",width=8,height=8)
dev.off()

################
## Fig 3: Mean severity for each formation
#################


##boxplot(d.pres$mean.severity~d.pres$D2b)
##boxplot(w.pres$mean.severity~w.pres$C2b)
##boxplot(d.hist$mean.severity.D3~d.hist$D3,varwidth=T)
##boxplot(d.hist$mean.severity.D1~d.hist$D1)

kk <- subset(w.pres,pais %in% "global" & !is.na(mean.severity))
kk$format <- tipologia$format[match(kk$mcdg,tipologia$macrogroup_key)]##,
##                    levels=unique(grep("1.[AB].",tipologia$format,value=T))[1:8])
ll <- subset(c.futr,pais %in% "global")
ll$format <- factor(tipologia$format[match(ll$mcdg,tipologia$macrogroup_key)])##,
##                    levels=unique(grep("1.[AB].",tipologia$format,value=T))[1:8])

mm <- subset(d.pres,pais %in% "global")
mm$format <- factor(tipologia$format[match(mm$mcdg,tipologia$macrogroup_key)],
                    levels=unique(grep("1.[AB].",tipologia$format,value=T))[1:8])

nn <- subset(d.hist,pais %in% "global")
nn$format <- factor(tipologia$format[match(nn$mcdg,tipologia$macrogroup_key)],
                    levels=unique(grep("1.[AB].",tipologia$format,value=T))[1:8])

layout(matrix(c(1,2,3,3,4,4,5,5),ncol=2,byrow=T))
barplot(kk$mean.severity,ylim=c(0,100),col=IUCNclr[match(kk$C2b,names(IUCNclr))],names.arg=kk$mcdg,main="Current Land Surface Water change (C2b)",las=2,ylab="Mean severity (%)")

boxplot(mean.severity~format,ll,varwidth=T,ylim=c(0,100),main="Future Climate change (C2a)",ylab="Mean severity (%)")##,las=2,col=match(ll$sc,c("26","45","60","85")))

barplot(nn$mean.severity.D1,ylim=c(0,100),col=IUCNclr[match(nn$D1,names(IUCNclr))],names.arg=nn$mcdg,main="Past increase (1900-2000) in human use intensity (D1)",las=2,ylab="Mean severity (%)")

barplot(mm$mean.severity,ylim=c(0,100),col=IUCNclr[match(mm$D2b,names(IUCNclr))],names.arg=mm$mcdg,main="Current risk of defaunation (D2b)",las=2,ylab="Mean severity (%)")


barplot(nn$mean.severity.D3,ylim=c(0,100),col=IUCNclr[match(nn$D3,names(IUCNclr))],names.arg=nn$mcdg,main="Historical (1700-2000) increase in human use intensity (D3)",las=2,ylab="Mean severity (%)")

dev.copy(pdf,file="~/Provita/etc/MS/Figure3_draft.pdf",width=15,height=10)
dev.off()
################
## Fig 4: Map with overall category
#################


################
## Fig 5: threat score per country
#################


################
## Fig 6: 
#################


tt <- acciones[acciones$pais %in% c(NAM,CAM,CAR,SAM),
               c(grep("costo.[A-D]",colnames(acciones)))]
tt <- tt/acciones[acciones$pais %in% c(NAM,CAM,CAR,SAM),"costo"]
rownames(tt) <- acciones[acciones$pais %in% c(NAM,CAM,CAR,SAM),"pais"]


tt$restore.forest <- with(tt,costo.A1+costo.A3)
tt$halt.deforestation <- with(tt,costo.A2a+costo.A2b)
tt$protect.restricted <- with(tt,costo.B1+costo.B2+costo.B3)
tt$climate.adaptation <- tt$costo.C2a
tt$water.protection <- tt$costo.C2b
tt$resource.use <- with(tt,costo.D1+costo.D3)
tt$over.explotation <- tt$costo.D2b
tt <- tt[,!grepl("costo",colnames(tt))]

##locs <- TMWB@data[match(rownames(tt),TMWB@data$NAME),c("NAME","LON","LAT")]
locs <- read.csv(sprintf("%s/IVC/CoordenadasMapas.csv",gsub("doc","data",mi.path)),sep=";",dec=",")
locs <- locs[match(rownames(tt),locs$Name),]

##stars(tt,draw.segments=T,col.segments=rainbow(ncol(tt)),locations= TMWB@data[match(rownames(tt),TMWB@data$NAME),c("LON","LAT")],len=5,add=T)
## probar si tiene más sentido así
plot(subset(TMWB,NAME %in% c(CAM,NAM,CAR,SAM)),xlim=c(-90,-60))
segments(locs$lon,locs$lat,locs$x,locs$y,lty=2,col="grey22")
stars(sqrt(tt),draw.segments=T,col.segments=rainbow(ncol(tt)),locations=locs[,c("x","y")] ,len=5,scale=F,add=T,label="")
legend("bottomleft",fill=rainbow(ncol(tt)),gsub("costo.","",colnames(tt)),ncol=1,inset=-.05,xpd=T)


clrs <- rev(RColorBrewer::brewer.pal(9,"PiYG"))[c(1:3,6:9)]
plot(subset(TMWB,NAME %in% c(CAM,NAM,CAR,SAM)),xlim=c(-90,-60))
segments(locs$lon,locs$lat,locs$x,locs$y,lty=2,col="grey22")
inset.x <- 0
inset.y <- 70
for (k in 1:nrow(tt)) {
    ccs <- rainbow(ncol(tt))[tt[k,]>0]
    ccs <- clrs[tt[k,]>0]
    y <- tt[k,tt[k,]>0]
    if (!is.na(locs[k,"x"])) {
        floating.pie(locs[k,"x"],locs[k,"y"],t(tt[k,]),radius=2.8,col=ccs)
    } else {
        floating.pie(inset.x,inset.y,t(tt[k,]),radius=2.8,col=ccs)
        text(inset.x,inset.y-5,rownames(tt[k,]))
        inset.y <- inset.y-10
    }
        
}
legend("bottomleft",fill=clrs,gsub("costo.","",colnames(tt)),ncol=1,inset=-.05,xpd=T)
dev.copy(pdf,file="~/Provita/etc/MS/Figure6_draft.pdf",width=15,height=10)
dev.off()

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
