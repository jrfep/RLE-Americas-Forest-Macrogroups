plot(subset(paises.robin,NAME %in% SAM))
stars(sqrt(tt),draw.segments=T,col.segments=rainbow(ncol(tt)),locations= paises.robin@data[match(rownames(tt),paises.robin@data$NAME),c("LON","LAT")],len=5e5,scale=F,add=T)
legend("bottomleft",fill=rainbow(ncol(tt)),gsub("costo.","",colnames(tt)),ncol=1,inset=-.05,xpd=T)


tt <- acciones[acciones$pais %in% CAR & acciones$pais %in% paises.robin@data$NAME,
               c(grep("costo.[A-D]",colnames(acciones)))]
tt <- tt/acciones[acciones$pais %in% CAR & acciones$pais %in% paises.robin@data$NAME,"costo"]
rownames(tt) <- acciones[acciones$pais %in% CAR & acciones$pais %in% paises.robin@data$NAME,"pais"]
##tt <- tt[rowSums(tt)>0,colSums(tt)>0]
stars(tt,draw.segments=T,col.segments=rainbow(12),locations= paises.robin@data[match(rownames(tt),paises.robin@data$NAME),c("LON","LAT")],len=2e5)
legend("bottomleft",fill=rainbow(12),gsub("costo.","",colnames(tt)),ncol=3,inset=-.05,xpd=T)

tt <- acciones[acciones$pais %in% CAM & acciones$pais %in% paises.robin@data$NAME,
               c(grep("costo.[A-D]",colnames(acciones)))]
tt <- tt/acciones[acciones$pais %in% CAM & acciones$pais %in% paises.robin@data$NAME,"costo"]
rownames(tt) <- acciones[acciones$pais %in% CAM & acciones$pais %in% paises.robin@data$NAME,"pais"]
##tt <- tt[rowSums(tt)>0,colSums(tt)>0]
stars(tt,draw.segments=T,col.segments=rainbow(12),locations= paises.robin@data[match(rownames(tt),paises.robin@data$NAME),c("LON","LAT")],len=1.3)
legend("bottomleft",fill=rainbow(12),gsub("costo.","",colnames(tt)),ncol=3,inset=-.05,xpd=T)



stars(tt,draw.segments=T,col.segments=rainbow(12))


ss <- subset(todos,pais=="Colombia")
with(ss,tapply(costo,list(droplevels(mcdg),factor(oacats,levels=names(IUCN.cats))),min,na.rm=T))

ss <- ss[order(ss$costo),]
kk <- ss[!duplicated(ss$mcdg),]




## alternativas...
table(actuales$oacats,actuales$opciones)
table(posibles$proteger>0)
table(posibles$restaurar>0)
table(posibles$emision>25)

acciones <- data.frame()
ss <- subset(todos,pais %in% "Venezuela" & catorig !="LC")
ss <- subset(todos,pais %in% "Colombia" & catorig !="LC")
for (j in unique(todos$pais)) {
    ss <- subset(todos,pais %in% j & catorig !="LC")
    ss$mejora <- ss$catorig != ss$oacats
    rsm <- summary(glm(mejora~I(proteger>0)+I(reforestar>0)+I(fauna>0)+I(hidrico>0)+I(restaurar>0)+I(emision>25),data=ss,family=binomial(logit)))
    
    acciones <- rbind(acciones,
                      data.frame(pais=j,
                                 var=rownames(rsm$coefficients)[-1],
                                 signo=sign(rsm$coefficients[-1,1]),
                                 p=rsm$coefficients[-1,4]<0.05))
}

tt <- 
    with(acciones,tapply(p,list(pais,var),sum))
colnames(tt) <- c("emision","proteger","reforestar","restaurar","hidrico","fauna")

all(rownames(tt) %in% c(CAR,SAM,NAM,CAM))

## controlling climate change emisions do not have a significant effect on overall category

## climate change, fauna and hidric not significant,
## several countries with no improvement
## combination of protect and reforest,
## restoration of remaining forest is important for Antigua and Barbuda, DR and Turk and Caicos...
tt[rownames(tt) %in% CAR,]
## climate change not significant
## protect and restoreation needed overall, reforesting in El salvador and mexico, hidric in CR, dfaunation Honduras and Panama
tt[rownames(tt) %in% CAM,]
## Mexico and USA need combinations of protection, reforestation and restauration, Canada no significant change
tt[rownames(tt) %in% NAM,]
## Andean countries can improve with reforestation programs 
## in Southern countries (Bolivia, Brazil, Paraguay, Argentina, Chile) restoration + defaunation + water regime are more important than just reforesting
## no significant actions in the Guiana shield or Uruguay
tt[rownames(tt) %in% SAM,]







    rsm <- summary(lm(Ts~proteger+reforestar+fauna+hidrico+restaurar+adaptar+emision,data=boots))
    
    acciones <- rbind(acciones,
                      data.frame(pais=j,
                                 var=rownames(rsm$coefficients)[-1],
                                 cff=rsm$coefficients[-1,1],
                                 p=rsm$coefficients[-1,4]))
}

tt <- t1 <- 
    with(acciones,tapply(cff,list(pais,var),function(x) round(x,3)))
t2 <- 
    with(acciones,tapply(p,list(pais,var),function(x) round(x,3)))

tt[t2<0.05] <- 0

## clearly reforestation has the larger effect but most country require some additional action, 
tt[rownames(tt) %in% CAR,]
table(apply(tt[rownames(tt) %in% c(CAR),],1,which.min))
apply(tt[rownames(tt) %in% c(CAR),],2,sum,na.rm=T)

## similar contributions of protection and restoration, also water  management but no effect of fauna protection or climate change
tt[rownames(tt) %in% CAM,]
tt[rownames(tt) %in% NAM,]
table(apply(tt[rownames(tt) %in% c(CAM,NAM),],1,which.min))
apply(tt[rownames(tt) %in% c(CAM,NAM),],2,sum,na.rm=T)

## mostly a combination of protection, restoration and refoerestation, slightly higher effect of effective protection, most coutnries benefit from water management, climate emission important for Peru and Chile, The Guayana Shield countries are relatively safe
tt[rownames(tt) %in% SAM,]
table(apply(tt[rownames(tt) %in% c(SAM),],1,which.min))
apply(tt[rownames(tt) %in% c(SAM),],2,sum,na.rm=T)


apply(tt[rownames(tt) %in% c(SAM),][c(1:5),],2,sum,na.rm=T)
apply(tt[rownames(tt) %in% c(SAM),][c(6:8),],2,sum,na.rm=T)
apply(tt[rownames(tt) %in% c(SAM),][c(9:13),],2,sum,na.rm=T)



subset(paises,NAME=="Venezuela")

ss <- subset(actuales,pais %in% "Venezuela")
ss <- subset(actuales,pais == 188)
ss <- subset(actuales,pais == 256)
mtz <- with(ss,tapply(oacats,list(droplevels(mcdg),droplevels(escenario)),unique))
cost.p <- with(ss,tapply(proteger,list(droplevels(mcdg),droplevels(escenario)),sum))
cost.r <- with(ss,tapply(reforestar,list(droplevels(mcdg),droplevels(escenario)),sum))
cost.f <- with(ss,tapply(fauna,list(droplevels(mcdg),droplevels(escenario)),sum))
cost.w <- with(ss,tapply(hidrico,list(droplevels(mcdg),droplevels(escenario)),sum))
cost.p[is.na(cost.p)] <- 0
cost.r[is.na(cost.r)] <- 0
cost.f[is.na(cost.f)] <- 0
cost.w[is.na(cost.w)] <- 0

Ts <- weighted.mean(c(0,6:1,0),table(factor(mtz[,1],levels=names(IUCN.cats))))
for (k in 2:ncol(mtz)) {
    mtz[is.na(mtz[,k]),k] <- mtz[is.na(mtz[,k]),1]
    Ts <- c(Ts,weighted.mean(c(0,6:1,0),table(factor(mtz[,k],levels=names(IUCN.cats)))))
}

Cp <- colSums(cost.p)
Cr <- colSums(cost.r)
Cf <- colSums(cost.f)
Cw <- colSums(cost.w)
slc <- c(T,Ts[-1]<Ts[1])

plot(Ts~Cp,subset=slc)
plot(Ts~Cr,subset=slc)
plot(Ts~Cf,subset=slc)
summary(glm(Ts~Cr+Cp+Cf+Cw,subset=slc,family=quasipoisson(log)))



ss <- subset(actuales,pais == 54)

subset(ss,oacat %in% "CR")
## escenarios alternativos:
## detener deforestación en los más críticos, detener deforestación en los menos costosos -> criterios A2a y A2b
## reforestar más críticos, reforestar menos costosos -> criterios A1, A3, B2 y B3
## cambio climático ...
## disminuir defaunacion...
## manejo de agua
## manejo de fuego

##tabla: MGs por categorias y treat score por pais, luego con varios escenarios

## mapas: areas donde proteger el bosque puede mejorar el treat score,
## areas donde es prioritario restaurar/reforestar
## areas donde es necesario complementar proteccion/reforestacion con otras acciones
