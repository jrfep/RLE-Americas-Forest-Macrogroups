## usamos umbral de 25 porque en la creación de la capa bsq.20XX aplicamos este umbral para zonas de mosaico boscoso

country.year <- data.frame()
for (k in 2001:2012) {
    tt <- with(subset(get(sprintf("bsq.%s",k)),!(V2 %in% "*") & V1 %in% cdg & V4 %in% 25:100),
               tapply(V5,list(factor(V2,levels=unique(bsq.2001$V2))),function(x) sum(x/1e6,na.rm=T)))
    
    country.year <- rbind(country.year,tt)
    colnames(country.year) <- names(tt)
}
country.year[is.na(country.year)] <- 0
country.year <- country.year[,!colnames(country.year) %in% "*",drop=F]

country.cntr <- data.frame()
for (k in 17:20) {
    tt <- with(subset(get(sprintf("ant.%s00",k)),!(V2 %in% "*") & V1 %in% cdg & V4 %in% legend.Anthromes$value[grep("woodland",legend.Anthromes$nombre)]),
               tapply(V5,list(factor(V2,levels=unique(ant.1700$V2))),function(x) sum(x/1e6,na.rm=T)))
    
    country.cntr <- rbind(country.cntr,tt)
    colnames(country.cntr) <- names(tt)
}
country.cntr[is.na(country.cntr)] <- 0
country.cntr <- country.cntr[,!colnames(country.cntr) %in% "*",drop=FALSE]

country.cell <- with(subset(bsq.2012,!(V2 %in% "*") & V1 %in% cdg & V4 %in% 25:100),
                     tapply(V5,list(V2,V3),function(x) sum(x/1e6,na.rm=T)))
country.cell <- country.cell[!rownames(country.cell) %in% "*",,drop=FALSE]

a1 <- rowSums(country.cell>1,na.rm=T)
a0 <- rowSums(country.cell>0,na.rm=T)

### global

bsq <- rowSums(country.year,na.rm=T)
yr <- (2001:2012)
glm01 <-  glm(bsq~yr,family=quasipoisson(log))

puntos <- xys[ecos %in% cdg ,]### para calculo EOO...???
if (length(puntos)>2) {
    cH <- rgeos::gConvexHull(puntos)
    mi.eoo <- area(cH)/1e6
    costo.eoo <- c(ifelse(mi.eoo>50000,0,50000-mi.eoo),
                   ifelse(mi.eoo>20000,0,20000-mi.eoo),
                   ifelse(mi.eoo>2000,0,2000-mi.eoo))
} else {
    mi.eoo <- NA
    costo.eoo <- rep(NA,3)
}
if(file.exists(sprintf("%s/%s_EOO.shp",output.dir,mcdg))) {
    shp <- shapefile(sprintf("%s/%s_EOO.shp",output.dir,mcdg))
    mi.eoo <- area(shp)/1e6
    costo.eoo <- c(ifelse(mi.eoo>50000,0,50000-mi.eoo),
                   ifelse(mi.eoo>20000,0,20000-mi.eoo),
                   ifelse(mi.eoo>2000,0,2000-mi.eoo))  
} 

## para calculo de escenarios

celdas <- rev(sort(colSums(country.cell,na.rm=T)))[1:51]
actual <- c(sum(celdas[1:51],na.rm=T),sum(celdas[1:21],na.rm=T),sum(celdas[1:3],na.rm=T))
meta <- c(sum(ifelse(!is.na(celdas) & celdas>1,celdas,1)[1:51],na.rm=T),sum(ifelse(!is.na(celdas) & celdas>1,celdas,1)[1:21],na.rm=T),sum(ifelse(!is.na(celdas) & celdas>1,celdas,1)[1:3],na.rm=T))

  if (sum(a0)>0) {
  
    AOOs <- rbind(AOOs,
                  data.frame(mcdg,pais="global",
                            EOO=mi.eoo,
                             AOO.c=sum(a1),AOO=sum(a0),ai=(coef(glm01 )[[2]] <0 & summary(glm01 )$coefficients[2,4] <0.05),costo.a.LC=meta[1]-actual[1],costo.a.VU=meta[2]-actual[2],costo.a.EN=meta[3]-actual[3],costo.e.LC=costo.eoo[1],costo.e.VU=costo.eoo[2],costo.e.EN=costo.eoo[3]))
    } else {
        AOOs <- rbind(AOOs,
                      data.frame(mcdg,pais="global",EOO=mi.eoo,AOO.c=NA,AOO=NA,ai=NA,costo.a.LC=NA,costo.a.VU=NA,costo.a.EN=NA,costo.e.LC=costo.eoo[1],costo.e.VU=costo.eoo[2],costo.e.EN=costo.eoo[3]))

    }
    prd1 <- try(predict(glm01,newdata=data.frame(yr=c(2001,2012,2051,2062)),type="link",se.fit=T))

    ##http://www.stat.yale.edu/Courses/1997-98/101/confint.htm
    ## intervalos de confianza basados en la distribución z
    ## 95% 1.96
    ## 90% 1.645

    if (any(class(prd1) %in% "try-error")) {
        b.pres <- 
            rbind(b.pres,
                  data.frame(mcdg,pais="global",
                             min.bosque=min(bsq),
                             mean.bosque=mean(bsq),
                             max.bosque=max(bsq),
                             PRD=coef(glm01 )[[2]],
                             PRD.se=summary(glm01 )$coefficients[2,2],
                             p.glm=summary(glm01 )$coefficients[2,4],
                             k2051=any(bsq<1),
                             k2062=any(bsq<1),
                             p2051=NA,n2051=NA,x2051=NA,
                             p2062=NA,n2062=NA,x2062=NA,
                             costo.2051.LC=NA,
                             costo.2051.VU=NA,
                             costo.2051.EN=NA,
                             costo.2062.LC=NA,
                             costo.2062.VU=NA,
                             costo.2062.EN=NA))
           
    } else {
        b.pres <- 
            rbind(b.pres,
                  data.frame(mcdg,pais="global",
                             min.bosque=min(bsq),
                             mean.bosque=mean(bsq),
                             max.bosque=max(bsq),
                             PRD=coef(glm01 )[[2]],
                             PRD.se=summary(glm01 )$coefficients[2,2],
                             p.glm=summary(glm01 )$coefficients[2,4],
                             k2051=any(bsq<1),
                             k2062=any(bsq<1),
                             p2051=exp(prd1$fit[3])/exp(prd1$fit[1]),
                             
                             n2051=exp(prd1$fit[3]-prd1$se.fit[3]*1.645)/
                                 exp(prd1$fit[1]-prd1$se.fit[1]*1.645),
                             x2051=exp(prd1$fit[3]+prd1$se.fit[3]*1.645)/
                                 exp(prd1$fit[1]+prd1$se.fit[1]*1.645),
                             p2062=exp(prd1$fit[4])/exp(prd1$fit[2]),
                             n2062=exp(prd1$fit[4]-prd1$se.fit[4]*1.645)/
                                 exp(prd1$fit[2]-prd1$se.fit[2]*1.645),
                             x2062=exp(prd1$fit[4]+prd1$se.fit[4]*1.645)/
                                 exp(prd1$fit[2]+prd1$se.fit[2]*1.645),
                             costo.2051.LC=max(c(0,(exp(prd1$fit[1])*.7)-exp(prd1$fit[3]))),
                             costo.2051.VU=max(c(0,(exp(prd1$fit[1])*.5)-exp(prd1$fit[3]))),
                             costo.2051.EN=max(c(0,(exp(prd1$fit[1])*.2)-exp(prd1$fit[3]))),
                             costo.2062.LC=max(c(0,(exp(prd1$fit[2])*.7)-exp(prd1$fit[4]))),
                             costo.2062.VU=max(c(0,(exp(prd1$fit[2])*.5)-exp(prd1$fit[4]))),
                             costo.2062.EN=max(c(0,(exp(prd1$fit[2])*.2)-exp(prd1$fit[4])))
                             ))
    }


    bsq <- rowSums(country.cntr)
    yr <- (17:20)*100
    glm00 <-  glm(bsq~1,family=quasipoisson(log))
    glm01 <-  glm(bsq~yr,family=quasipoisson(log))
    glm02 <-  glm(bsq~poly(yr,2),family=quasipoisson(log))
    ##tiene que ser type link y luego transformar para evitar los valores negativos
    
    prd1 <- predict(glm02,newdata=data.frame(yr=c(1700,1950,2000)),type="link",se.fit=T)
    b.hist <- 
        rbind(b.hist,
              data.frame(mcdg,pais="global",
                         min.bosque=min(bsq),
                         mean.bosque=mean(bsq),
                         max.bosque=max(bsq),
                         p0.glm=deviance(glm00),
                         p1.glm=deviance(glm01),
                         p2.glm=deviance(glm02),
                         k1950=any(bsq<1),
                         k1750=any(bsq<1),
                         p1950=exp(prd1$fit[3])/exp(prd1$fit[2]),
                         
                         n1950=exp(prd1$fit[3]-prd1$se.fit[3]*1.645)/
                             exp(prd1$fit[2]-prd1$se.fit[2]*1.645),
                         x1950=exp(prd1$fit[3]+prd1$se.fit[3]*1.645)/
                             exp(prd1$fit[2]+prd1$se.fit[2]*1.645),
                         p1750=exp(prd1$fit[3])/exp(prd1$fit[1]),
                         n1750=exp(prd1$fit[3]-prd1$se.fit[3]*1.645)/
                             exp(prd1$fit[1]-prd1$se.fit[1]*1.645),
                         x1750=exp(prd1$fit[3]+prd1$se.fit[3]*1.645)/
                             exp(prd1$fit[1]+prd1$se.fit[1]*1.645),
                         costo.1950.LC=max(c(0,(exp(prd1$fit[2])*.7)-exp(prd1$fit[3]))),
                         costo.1950.VU=max(c(0,(exp(prd1$fit[2])*.5)-exp(prd1$fit[3]))),
                         costo.1950.EN=max(c(0,(exp(prd1$fit[2])*.2)-exp(prd1$fit[3]))),
                         costo.1750.LC=max(c(0,(exp(prd1$fit[1])*.5)-exp(prd1$fit[3]))),
                         costo.1750.VU=max(c(0,(exp(prd1$fit[1])*.3)-exp(prd1$fit[3]))),
                         costo.1750.EN=max(c(0,(exp(prd1$fit[1])*.1)-exp(prd1$fit[3])))
                         ))

save(file=sprintf("%s/spatialindicators.rda",output.dir),country.cell,country.year,country.cntr,glm01,glm02)


### por pais
for (l in colnames(country.year)) {
    bsq <- country.year[,l] 
    yr <- (2001:2012)
    glm01 <-  glm(bsq~yr,family=quasipoisson(log))
    ##tiene que ser type link y luego transformar para evitar los valores negativos

        puntos <- xys[ecos %in% cdg & xys.ps$NAME %in% as.character(paises$NAME[paises$OBJECTID %in% l]),]### para calculo EOO...???
        if (length(puntos)>2) {
            cH <- rgeos::gConvexHull(puntos)
            mi.eoo <- area(cH)/1e6
            costo.eoo <- c(ifelse(mi.eoo>50000,0,50000-mi.eoo),
                           ifelse(mi.eoo>20000,0,20000-mi.eoo),
                           ifelse(mi.eoo>2000,0,2000-mi.eoo))
        } else {
            mi.eoo <- NA
            costo.eoo <- rep(NA,3)
        }
    if(file.exists(sprintf("%s/%s_%s_EOO.shp",output.dir,mcdg,l))) {
        shp <- shapefile(sprintf("%s/%s_%s_EOO.shp",output.dir,mcdg,l))
        mi.eoo <- area(shp)/1e6
        costo.eoo <- c(ifelse(mi.eoo>50000,0,50000-mi.eoo),
                       ifelse(mi.eoo>20000,0,20000-mi.eoo),
                       ifelse(mi.eoo>2000,0,2000-mi.eoo))
     
    } 
    
    if (l %in% names(a0) & any(a0[l]>0)) {
        ## para calculo de escenarios

        celdas <- rev(sort(country.cell[l,!is.na(country.cell[l,])]))[1:51]
        actual <- c(sum(celdas[1:51],na.rm=T),sum(celdas[1:21],na.rm=T),sum(celdas[1:3],na.rm=T))
        meta <- c(sum(ifelse(!is.na(celdas) & celdas>1,celdas,1)[1:51],na.rm=T),sum(ifelse(!is.na(celdas) & celdas>1,celdas,1)[1:21],na.rm=T),sum(ifelse(!is.na(celdas) & celdas>1,celdas,1)[1:3],na.rm=T))

        AOOs <- rbind(AOOs,
                      data.frame(mcdg,pais=as.character(paises[match(l,paises$OBJECTID),"NAME"]),
                                 EOO=mi.eoo,
                                 AOO.c=a1[l],AOO=a0[l],ai=(coef(glm01 )[[2]] <0 & summary(glm01 )$coefficients[2,4] <0.05),costo.a.LC=meta[1]-actual[1],costo.a.VU=meta[2]-actual[2],costo.a.EN=meta[3]-actual[3],costo.e.LC=costo.eoo[1],costo.e.VU=costo.eoo[2],costo.e.EN=costo.eoo[3]))
    } else {
        AOOs <- rbind(AOOs,
                      data.frame(mcdg,pais=as.character(paises[match(l,paises$OBJECTID),"NAME"]),EOO=mi.eoo,AOO.c=NA,AOO=NA,ai=NA,costo.a.LC=NA,costo.a.VU=NA,costo.a.EN=NA,costo.e.LC=costo.eoo[1],costo.e.VU=costo.eoo[2],costo.e.EN=costo.eoo[3]))

    }
    prd1 <- try(predict(glm01,newdata=data.frame(yr=c(2001,2012,2051,2062)),type="link",se.fit=T))

    ##http://www.stat.yale.edu/Courses/1997-98/101/confint.htm
    ## intervalos de confianza basados en la distribución z
    ## 95% 1.96
    ## 90% 1.645

    if (any(class(prd1) %in% "try-error")) {
        b.pres <- 
            rbind(b.pres,
                  data.frame(mcdg,pais=as.character(paises[match(l,paises$OBJECTID),"NAME"]),
                             min.bosque=min(bsq),
                             mean.bosque=mean(bsq),
                             max.bosque=max(bsq),
                             PRD=coef(glm01 )[[2]],
                             PRD.se=summary(glm01 )$coefficients[2,2],
                             p.glm=summary(glm01 )$coefficients[2,4],
                             k2051=any(bsq<1),
                             k2062=any(bsq<1),
                             p2051=NA,n2051=NA,x2051=NA,
                             p2062=NA,n2062=NA,x2062=NA,
                             costo.2051.LC=NA,
                             costo.2051.VU=NA,
                             costo.2051.EN=NA,
                             costo.2062.LC=NA,
                             costo.2062.VU=NA,
                             costo.2062.EN=NA))
           
    } else {
        b.pres <- 
            rbind(b.pres,
                  data.frame(mcdg,pais=as.character(paises[match(l,paises$OBJECTID),"NAME"]),
                             min.bosque=min(bsq),
                             mean.bosque=mean(bsq),
                             max.bosque=max(bsq),
                             PRD=coef(glm01 )[[2]],
                             PRD.se=summary(glm01 )$coefficients[2,2],
                             p.glm=summary(glm01 )$coefficients[2,4],
                             k2051=any(bsq<1),
                             k2062=any(bsq<1),
                             p2051=exp(prd1$fit[3])/exp(prd1$fit[1]),
                             
                             n2051=exp(prd1$fit[3]-prd1$se.fit[3]*1.645)/
                                 exp(prd1$fit[1]-prd1$se.fit[1]*1.645),
                             x2051=exp(prd1$fit[3]+prd1$se.fit[3]*1.645)/
                                 exp(prd1$fit[1]+prd1$se.fit[1]*1.645),
                             p2062=exp(prd1$fit[4])/exp(prd1$fit[2]),
                             n2062=exp(prd1$fit[4]-prd1$se.fit[4]*1.645)/
                                 exp(prd1$fit[2]-prd1$se.fit[2]*1.645),
                             x2062=exp(prd1$fit[4]+prd1$se.fit[4]*1.645)/
                                 exp(prd1$fit[2]+prd1$se.fit[2]*1.645),
                             costo.2051.LC=max(c(0,(exp(prd1$fit[1])*.7)-exp(prd1$fit[3]))),
                             costo.2051.VU=max(c(0,(exp(prd1$fit[1])*.5)-exp(prd1$fit[3]))),
                             costo.2051.EN=max(c(0,(exp(prd1$fit[1])*.2)-exp(prd1$fit[3]))),
                             costo.2062.LC=max(c(0,(exp(prd1$fit[2])*.7)-exp(prd1$fit[4]))),
                             costo.2062.VU=max(c(0,(exp(prd1$fit[2])*.5)-exp(prd1$fit[4]))),
                             costo.2062.EN=max(c(0,(exp(prd1$fit[2])*.2)-exp(prd1$fit[4])))
                             ))
    }
}

for (l in colnames(country.cntr)) {
    bsq <- country.cntr[,l] 
    yr <- (17:20)*100
    glm00 <-  glm(bsq~1,family=quasipoisson(log))
    glm01 <-  glm(bsq~yr,family=quasipoisson(log))
    glm02 <-  glm(bsq~poly(yr,2),family=quasipoisson(log))
    ##tiene que ser type link y luego transformar para evitar los valores negativos
    
    prd1 <- predict(glm02,newdata=data.frame(yr=c(1700,1950,2000)),type="link",se.fit=T)
    b.hist <- 
        rbind(b.hist,
              data.frame(mcdg,pais=as.character(paises[match(l,paises$OBJECTID),"NAME"]),
                         min.bosque=min(bsq),
                         mean.bosque=mean(bsq),
                         max.bosque=max(bsq),
                         p0.glm=deviance(glm00),
                         p1.glm=deviance(glm01),
                         p2.glm=deviance(glm02),
                         k1950=any(bsq<1),
                         k1750=any(bsq<1),
                         p1950=exp(prd1$fit[3])/exp(prd1$fit[2]),
                         
                         n1950=exp(prd1$fit[3]-prd1$se.fit[3]*1.645)/
                             exp(prd1$fit[2]-prd1$se.fit[2]*1.645),
                         x1950=exp(prd1$fit[3]+prd1$se.fit[3]*1.645)/
                             exp(prd1$fit[2]+prd1$se.fit[2]*1.645),
                         p1750=exp(prd1$fit[3])/exp(prd1$fit[1]),
                         n1750=exp(prd1$fit[3]-prd1$se.fit[3]*1.645)/
                             exp(prd1$fit[1]-prd1$se.fit[1]*1.645),
                         x1750=exp(prd1$fit[3]+prd1$se.fit[3]*1.645)/
                             exp(prd1$fit[1]+prd1$se.fit[1]*1.645),
                         costo.1950.LC=max(c(0,(exp(prd1$fit[2])*.7)-exp(prd1$fit[3]))),
                         costo.1950.VU=max(c(0,(exp(prd1$fit[2])*.5)-exp(prd1$fit[3]))),
                         costo.1950.EN=max(c(0,(exp(prd1$fit[2])*.2)-exp(prd1$fit[3]))),
                         costo.1750.LC=max(c(0,(exp(prd1$fit[1])*.5)-exp(prd1$fit[3]))),
                         costo.1750.VU=max(c(0,(exp(prd1$fit[1])*.3)-exp(prd1$fit[3]))),
                         costo.1750.EN=max(c(0,(exp(prd1$fit[1])*.1)-exp(prd1$fit[3])))
                         ))
}


