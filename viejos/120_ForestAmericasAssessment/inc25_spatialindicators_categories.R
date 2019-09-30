########################
## Comparar resultados julio 2017 con los de diciembre 2017
##########################

###### Historicos
dts1 <- b.hist[ b.hist$pais %in% "global",c("mcdg","p1750","n1750","x1750")]

dts2 <- hist.decline[ hist.decline$ext %in% "global" & hist.decline$con %in% "todo",c("mcdg","prd.min",  "prd.mean",   "prd.max")]
dts <- merge(dts1,dts2,by="mcdg")

plot(I(1-p1750)~prd.mean,dts)
## diferencias más grandes M154 (NE) y M662 (NT) no parece ser de importancia

###### Presentes

pres.decline$pais <- as.character(paises$NAME[match(pres.decline$ext,paises$OBJECTID)])

pres.decline$pais[pres.decline$ext %in% "global"] <- "global"
##dts1 <- b.pres[ b.pres$pais %in% "global",c("mcdg","p2051","n2051","x2051")]

dts2 <- pres.decline[ pres.decline$con %in% "todo",c("mcdg","pais","cfmin",  "cf1",   "cfmax")]
dts <- merge(b.pres,dts2,by=c("pais","mcdg"),all.x=T)



dts$A2a <- cut(dts$p2062,breaks=c(-Inf,0,.2,.5,.7,.67,Inf),label=c("DD","CR","EN","VU","NT","LC"))
dts$A2a.min <- cut(apply(dts[,c("p2062","n2062","x2062","cfmin","cf1","cfmax")],1,max),breaks=c(0,.2,.5,.7,.67,Inf),label=c("CR","EN","VU","NT","LC"))
dts$A2a.max <- cut(apply(dts[,c("p2062","n2062","x2062","cfmin","cf1","cfmax")],1,min),breaks=c(0,.2,.5,.7,.67,Inf),label=c("CR","EN","VU","NT","LC"))
dts$A2a <- factor(dts$A2a,levels=names(IUCN.cats))
##dts$A2a[dts$k2062] <- "CO"
    

dts$A2a[dts$A2a %in% c("CO","CR","EN","VU") & dts$p.glm>0.05] <- "NT"
dts$A2a.min[dts$A2a %in% c("NT") & dts$p.glm>0.05] <- "NT"
dts$A2a.max[dts$A2a %in% c("NT") & dts$p.glm>0.05] <- "NT"

dts$A2a[dts$A2a.min %in% "LC" & dts$A2a.max %in% "CR"] <- "DD"

##dts$A2b <- cut(dts$p2051,breaks=c(-Inf,0,.2,.5,.7,.67,Inf),label=c("DD","CR","EN","VU","NT","LC"))
##dts$A2b.min <- cut(dts$n2051,breaks=c(0,.2,.5,.7,.67,Inf),label=c("CR","EN","VU","NT","LC"))
##dts$A2b.max <- cut(dts$x2051,breaks=c(0,.2,.5,.7,.67,Inf),label=c("CR","EN","VU","NT","LC"))
##dts$A2b <- factor(dts$A2b,levels=names(IUCN.cats))

##dts$A2b[dts$k2051] <- "CO"


##dts$A2b[dts$A2b %in% c("CO","CR","EN","VU") & dts$p.glm>0.05] <- "NT"

##dts$A2b[dts$A2b.min %in% "CR" & dts$A2b.max %in% "LC"] <- "DD"


b.hist$A1 <- cut(b.hist$p1950,breaks=c(-Inf,0,.2,.5,.7,.67,Inf),label=c("DD","CR","EN","VU","NT","LC"))
b.hist$A1.min <- cut(b.hist$n1950,breaks=c(0,.2,.5,.7,.67,Inf),label=c("CR","EN","VU","NT","LC"))
b.hist$A1.max <- cut(b.hist$x1950,breaks=c(0,.2,.5,.7,.67,Inf),label=c("CR","EN","VU","NT","LC"))
b.hist$A1 <- factor(b.hist$A1,levels=names(IUCN.cats))
##b.hist$A1[b.hist$k1950] <- "CO"

## no recuerdo porque quité esto, tal vez por ser una curva descriptiva (solo cuatro puntos, muchos parámetros) no importa que no sea significativa?
##no influye en categoría final, pero si cambia la tabla
##b.hist$A1[b.hist$A1 %in% c("CR","EN","VU") & b.hist$p1.glm>0.05 & b.hist$p2.glm>0.05] <- "NT"

b.hist$A1[b.hist$A1.min %in% "CR" & b.hist$A1.max %in% "LC"] <- "DD"


b.hist$A3 <- cut(b.hist$p1750,breaks=c(-Inf,0,.1,.3,.5,.45,Inf),label=c("DD","CR","EN","VU","NT","LC"))
b.hist$A3.min <- cut(b.hist$n1750,breaks=c(0,.1,.3,.5,.45,Inf),label=c("CR","EN","VU","NT","LC"))
b.hist$A3.max <- cut(b.hist$x1750,breaks=c(0,.1,.3,.5,.45,Inf),label=c("CR","EN","VU","NT","LC"))
b.hist$A3 <- factor(b.hist$A3,levels=names(IUCN.cats))
##b.hist$A3[b.hist$k1950] <- "CO"


##b.hist$A3[b.hist$A3 %in% c("CR","EN","VU") & b.hist$p1.glm>0.05 & b.hist$p2.glm>0.05] <- "NT"

b.hist$A3[b.hist$A3.min %in% "CR" & b.hist$A3.max %in% "LC"] <- "DD"


AOOs$B1 <- cut(AOOs$EOO,breaks=c(-Inf,0,2000,20000,50000,55000,Inf),labels=c("CO","CR","EN","VU","NT","LC"))
AOOs$B1 <- factor(AOOs$B1,levels=c("CO","CR","EN","VU","NT","LC","DD"))
AOOs$B1[AOOs$EOO>0 & !AOOs$ai] <- "LC"
AOOs$B1[is.na(AOOs$EOO)] <- "DD"


AOOs$B2 <- cut(AOOs$AOO.c,breaks=c(-Inf,0,2,20,50,55,Inf),labels=c("CO","CR","EN","VU","NT","LC"))
AOOs$B2 <- factor(AOOs$B2,levels=c("CO","CR","EN","VU","NT","LC","DD"))
AOOs$B2[!is.na(AOOs$AOO.c) & AOOs$AOO.c>0 & !AOOs$ai] <- "LC"
AOOs$B2[AOOs$AOO.c %in% 0 & !AOOs$ai] <- "NT"
AOOs$B2[is.na(AOOs$AOO.c)] <- "DD"


AOOs$B3 <- ifelse(AOOs$AOO<5,"VU","LC") 
AOOs$B3[is.na(AOOs$AOO)] <- "DD"

AOOs$costo.a.LC[AOOs$B2 %in% c("DD","LC")] <- 0
AOOs$costo.e.LC[AOOs$B1 %in% c("DD","LC")] <- 0
AOOs$costo.a.VU[AOOs$B2 %in% c("DD","LC")] <- 0
AOOs$costo.e.VU[AOOs$B1 %in% c("DD","LC")] <- 0
AOOs$costo.a.EN[AOOs$B2 %in% c("DD","LC")] <- 0
AOOs$costo.e.EN[AOOs$B1 %in% c("DD","LC")] <- 0





plot(p2051~cf1,subset(dts,p2051<10))
abline(a=0,b=1,col=2)

table(dts$cf1>1, dts$p2051>1)

plot(p2051~cf1,subset(dts,p2051<1 & cf1<1))
abline(a=0,b=1,col=2)

## considerar las discrepancias como "data deficient", debido a que no tenemos datos de evaluación en campo para validar dirección del cambio
##table(cut(dts$cf1,c(0,.2,.5,.7,Inf),label=c("CR","EN","VU","LC")),
##      cut(dts$p2051,c(0,.2,.5,.7,Inf),label=c("CR","EN","VU","LC")))
