################
## Fig 1: Historical rates of decline
#################

## changes per formation, division and macrogroup


################
## Fig 2: Recent and current rates of decline
#################

## see sesion04_FiguresCriteria.R for current version of figure

barplot(xx$p1950,
        col=IUCNclr[as.character(xx$A1)],ylim=c(0,1))
barplot(xx$p2051,col=IUCNclr[as.character(xx$A2a)],ylim=c(0,1))

barplot(xx$p1750,col=IUCNclr[as.character(xx$A3)],ylim=c(0,1))


barplot(b.hist$p1950)

layout(matrix(1:8,ncol=4,byrow=T))
par(mar=c(0,1,1,1),oma=c(5,4,1,1))
xx <- merge(subset(b.hist,pais %in% "global"),subset(dts,pais %in% "global"),by="mcdg")
##xx$p2051[xx$p2051>1] <- 1.05


for (k in unique(resumen.global$frmt)) {
    if (k %in% "1.A.5")
        par(mar=c(1,1,0,1))
    xs <- subset(xx,mcdg %in% subset(resumen.global,frmt %in% k)$mcdg)
    plot(p2051~p1950, xs,type="n",xlim=c(0,1),ylim=c(0,1.5),xlab="",ylab="",axes=F)

    rect(0,0,0.7,1,col=IUCNclr["VU"],border=NA)
    rect(0,0,1,0.7,col=IUCNclr["VU"],border=NA)
    rect(0,0,0.5,1,col=IUCNclr["EN"],border=NA)
    rect(0,0,1,0.5,col=IUCNclr["EN"],border=NA)
    rect(0,0,0.2,1,col=IUCNclr["CR"],border=NA)
    rect(0,0,1,0.2,col=IUCNclr["CR"],border=NA)
    with(subset(xs,!(A1 %in% c("DD","NE")) & !(A2a %in% c("DD","NE")) & p2051<1.5),segments(p1950,n2051,p1950,x2051))
    with(subset(xs,!(A1 %in% c("DD","NE")) & !(A2a %in% c("DD","NE")) & p2051<1.5),segments(n1950,p2051,x1950,p2051))
    points(p2051~p1950, subset(xs,!A1 %in% c("DD","NE")))
    if (any(xs$p2051>1.5))
        arrows(xs$p1950[xs$p2051>1.5],1.47,xs$p1950[xs$p2051>1.5],1.53,length=0.07,angle=45)

    if (k %in% c("1.A.1","1.A.5"))
        axis(2)
    if (k %in% c("1.A.4","1.B.3"))
        axis(4)
    if (k %in% c("1.A.5","1.B.2"))
        axis(1)
    if (k %in% c("1.A.2","1.A.4"))
        axis(3)

}
    mtext("Proportion of woodland area loss between 1950 and 2000 (estimated)",side=1,line=3,outer=T)
   mtext("Proportion of area of forest cover to be lost between 2001 and 2051 (projected)",side=2,line=2,outer=T)
    
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
subset(tipologia, macrogroup_key %in% subset(resumen.global,oacats %in% "CR")$mcdg)$macrogroup_name

## revisar aquí
## checking https://www1.usgs.gov/csas/nvcs/nvcsGetUnitDetails?elementGlobalId=899634 for north america macrogroups


Comanchian Forest & Woodland
Southern Rocky Mountain Two-needle Pinyon - One-seed Juniper Woodland
Central & Appalachian Swamp Forest     

Parana Humid Forest
Northern Andean Montane & Upper Montane Humid Forest              

Espinal Floodplain Forest

## need closer look:
## Northern Great plain Woodlands: mostly grassland, historical change?
## 

## confirming evidence:
## Western Ecuador Humid Forest -- Dodson & Gentry 1991, Sierra et al 2002
## Guayaquil Flooded & Swamp Forest -- Dodson & Gentry 1991, Sierra et al 2002

## revisar si es "Central Midwest Oak Forest, Woodland & Savanna" o "North Central Oak - Hardwood & Pine Forest" -- Hak & Comer 2017
## Parana Floodplain Forest -- Agostinho et al 2004
## Tumbes Guayaquil Seasonal Dry Forest -- Escribano-Avila et al 2017, Sierra et al 2002
## Caatinga Seasonal Dry Forest  -- Dryflor 2017                           
## Parana Seasonal Dry Forest -- Dryflor 2017



################
## Fig 5: threat score per country
#################


################
## Fig 6: 
#################

## ver sesion10_costoefectividad.R
