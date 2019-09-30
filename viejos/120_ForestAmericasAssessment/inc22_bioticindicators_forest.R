##criterion D2b
thresholds <- c(0,30,50,80,100)

## excluimos estos de este análisis 
no.tropicales <- subset(paises,UNREG1 %in% c("Caribbean","Northern America"))[,c("OBJECTID","NAME")]

for (j in c("global",as.character(unique(caz.RS$V2)))) {
    if (as.character(j) %in% no.tropicales$OBJECTID | frmt %in% c("1.B.1","1.B.2","1.B.3")) {
        d.pres <- rbind(d.pres,
                        data.frame(mcdg,pais=as.character(paises[match(as.numeric(j),paises$OBJECTID),"NAME"]),
                                   mean.severity=NA,
                                   costo.LC=NA,
                                   costo.VU=NA,
                                   costo.EN=NA,
                                   D2b="NE"))

    } else {
        if (  j %in% "global") {
            sz <- subset(caz.RS, !(V3 %in% "*"))
        } else {
            sz <- subset(caz.RS,V2 %in% as.character(j) & !(V3 %in% "*"))
        }
        
        max.ext <- sum(sz$V4[as.numeric(as.character(sz$V3)) %in% 0:100],na.rm=T)/1e6
        deg.ext <- c()
        deg.sev <- 1:100 
        
        for (k in deg.sev) 
            deg.ext <- c(deg.ext,sum(sz$V4[as.numeric(as.character(sz$V3))<=k],na.rm=T))

        ##        costo.def <- abs(max(0,sum(cumsum(rep(max(deg.ext)/100,100))-deg.ext)/1e6))
        
        deg.ext <- 100-deg.ext*100/max(deg.ext)
        
        plot(deg.ext,deg.sev,ylab="Relative Severity",xlab="Extent",xlim=c(0,100),ylim=c(0,100),type="n")
        rect(thresholds[5],thresholds[4],thresholds[4],thresholds[5],col=IUCNclr["CR"])
        rect(thresholds[5],thresholds[3],thresholds[4],thresholds[4],col=IUCNclr["EN"])
        rect(thresholds[5],thresholds[2],thresholds[4],thresholds[3],col=IUCNclr["VU"])
        rect(thresholds[4],thresholds[4],thresholds[3],thresholds[5],col=IUCNclr["EN"])
        rect(thresholds[4],thresholds[3],thresholds[3],thresholds[4],col=IUCNclr["VU"])
        rect(thresholds[3],thresholds[4],thresholds[2],thresholds[5],col=IUCNclr["VU"])
        lines(deg.ext,deg.sev)  
        lines(1:100,100:1)
        deg.sev-(100:1)

        mean.severity <- weighted.mean(deg.ext,deg.sev)
        ocat <- ifelse(mean.severity>30,"NT","LC")
        
        res.tab <- table(cut(deg.ext,thresholds,include.lowest=T),
                         cut(deg.sev,thresholds,include.lowest=T))
        if (res.tab[4,2]>0 | res.tab[3,3]>0 | res.tab[2,4]>0) {
            ocat <- "VU"
        }
       if (res.tab[4,3]>0 | res.tab[3,4]>0)
            ocat <- "EN"
        if (res.tab[4,4]>0) {
            ocat <- "CR"
        }


        costo.LC <- max.ext*sum(max(0,max(deg.ext[deg.sev>thresholds[4]])-thresholds[2]),
                                max(0,max(deg.ext[deg.sev>thresholds[3]])-thresholds[3]),
                                max(0,max(deg.ext[deg.sev>thresholds[2]])-thresholds[4]))/100
        costo.VU <- max.ext*sum(max(0,max(deg.ext[deg.sev>thresholds[4]])-thresholds[3]),
                                max(0,max(deg.ext[deg.sev>thresholds[3]])-thresholds[4]))/100
       costo.EN <- max.ext*(max(0,max(deg.ext[deg.sev>thresholds[4]])-thresholds[4]))/100

        d.pres <- rbind(d.pres,
                        data.frame(mcdg,pais=ifelse(j %in% "global",j,as.character(paises[match(as.numeric(j),paises$OBJECTID),"NAME"])),
                                   mean.severity,
                                   costo.LC,
                                   costo.VU,
                                   costo.EN,
                                   D2b=ocat))
    }
}


##D1 y D3 basado en Anthromes

    mtz <- matrix(c(0,0,0,0,
                    25,0,0,0,
                    50,25,0,0,
                    75,50,25,0),byrow=T,ncol=4,
                  dimnames=list(c("51","52","53","61"),c("51","52","53","61")))

## matriz de severidad relativa
for (j in c("global",as.character(unique(antchange$V2)))) {
    if (j %in% "global") {
        ac <- subset(antchange, !(V3 %in% "*"))
    } else {
        ac <- subset(antchange,V2 %in% as.character(j) & !(V3 %in% "*"))
    } 
    
    ac$anterior <- factor(ifelse(ac$V3 %in% c("51","52","53","61"),ac$V3,NA),levels=c("51","52","53","61"))
    ac$actual <- factor(ifelse(ac$V6 %in% c("51","52","53","61"),ac$V6,NA),levels=c("51","52","53","61"))
    ext <- tapply(ac$V7/1e6,list(ac$anterior,ac$actual),sum)
    ext[is.na(ext)] <- 0
    
    if (sum(ext)>0) {
        mean.severity.D3 <- weighted.mean(mtz,ext,na.rm=T)
        catD3 <- ifelse(mean.severity.D3>50,"NT","LC")
        if(ext[4,1]/sum(ext)>.5) { 
            catD3 <- "VU"
        }
        if((ext[4,1]+ext[3,1]+ext[4,2])/sum(ext)>.7) {
            catD3 <- "VU"
        }
        if((ext[4,1]+ext[3,1]+ext[4,2]+ext[4,3]+ext[3,2]+ext[2,1])/sum(ext)>.9) {
            catD3 <- "VU"
        }
        if(ext[4,1]/sum(ext)>.7) {
            catD3 <- "EN"
        }

        if((ext[4,1]+ext[3,1]+ext[4,2])/sum(ext)>.9) {
            catD3 <- "EN"
        }
        if(ext[4,1]/sum(ext)>.9) {
            catD3 <- "CR"
        }

        ##revisar costos aquí
        costoD3.LC <- sum(max(0,ext[4,1]-(0.5*sum(ext))),
                        max(0,(ext[4,1]+ext[3,1]+ext[4,2])-(0.7*sum(ext))),
                        max(0,(ext[4,1]+ext[3,1]+ext[4,2]+ext[4,3]+ext[3,2]+ext[2,1])-(0.9*sum(ext))))
        costoD3.VU <- sum(max(0,ext[4,1]-(0.7*sum(ext))),
                        max(0,(ext[4,1]+ext[3,1]+ext[4,2])-(0.9*sum(ext))))
        costoD3.EN <- max(0,ext[4,1]-(0.9*sum(ext)))


    } else {
        mean.severity.D3 <- NA
        catD3 <- "DD"
        costoD3.LC <- costoD3.VU <- costoD3.EN <- 0
    }

    ac$anterior <- factor(ifelse(ac$V5 %in% c("51","52","53","61"),ac$V5,NA),levels=c("51","52","53","61"))

    ext <- tapply(ac$V7/1e6,list(ac$anterior,ac$actual),sum)
    ext[is.na(ext)] <- 0
    
    if (sum(ext)>0) {
        mean.severity.D1 <- weighted.mean(mtz,ext,na.rm=T)
        catD1 <- ifelse(mean.severity.D1>30,"NT","LC")
        
        if(ext[4,1]/sum(ext)>.3) {
            catD1 <- "VU"
        }
        
        if((ext[4,1]+ext[3,1]+ext[4,2])/sum(ext)>.5) {
            catD1 <- "VU"
        }
        if((ext[4,1]+ext[3,1]+ext[4,2]+ext[4,3]+ext[3,2]+ext[2,1])/sum(ext)>.8) {
            catD1 <- "VU"
        }
        if(ext[4,1]/sum(ext)>.5) {
            catD1 <- "EN"
        }
        if((ext[4,1]+ext[3,1]+ext[4,2])/sum(ext)>.8) {
            catD1 <- "EN"
        }

        if(ext[4,1]/sum(ext)>.8) {
            catD1 <- "CR"
        }

        ##revisar costos aquí
        costoD1.LC <- sum(max(0,ext[4,1]-(0.3*sum(ext))),
                        max(0,(ext[4,1]+ext[3,1]+ext[4,2])-(0.5*sum(ext))),
                        max(0,(ext[4,1]+ext[3,1]+ext[4,2]+ext[4,3]+ext[3,2]+ext[2,1])-(0.8*sum(ext))))
        costoD1.VU <- sum(max(0,ext[4,1]-(0.5*sum(ext))),
                        max(0,(ext[4,1]+ext[3,1]+ext[4,2])-(0.8*sum(ext))))
        costoD1.EN <- max(0,ext[4,1]-(0.8*sum(ext)))

    } else {
        mean.severity.D1 <- NA
        catD1 <- "DD"
        costoD1.LC <- costoD1.VU <- costoD1.EN <- 0
    }

    d.hist <- rbind(d.hist,
                    data.frame(mcdg,
                               pais=ifelse(j %in% "global",j,as.character(paises[match(as.numeric(j),paises$OBJECTID),"NAME"])),
                               mean.severity.D1,
                               costoD1.LC,
                               costoD1.VU,
                               costoD1.EN,
                               D1=catD1,
                               mean.severity.D3,
                               costoD3.LC,
                               costoD3.VU,
                               costoD3.EN,
                               D3=catD3))

}
