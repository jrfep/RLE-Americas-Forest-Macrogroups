##criterion C2b
thresholds <- c(0,30,50,80,100)
for (j in c("global",as.character(unique(gws$V2)))) {
        if (j %in% "global") {
            sz <- subset(gws, V1 %in% cdg & !(V3 %in% "*"))
        } else {
            sz <- subset(gws,V1 %in% cdg & V2 %in% j & !(V3 %in% "*"))
        }

    if (frmt %in% c("1.A.4","1.A.5","1.B.3")) {
        OD <- 100 - as.numeric(as.character(sz$V3)) ## observed decline
        MD <- 100 - 15
        sz$RS <- ifelse(OD>MD,100,ifelse(OD<0,0,OD)*100/MD)
        
        deg.ext <- c()
        deg.sev <- 1:100 
        
        for (k in deg.sev) 
            deg.ext <- c(deg.ext,sum(sz$V4[as.numeric(as.character(sz$RS))<=k],na.rm=T))

        ##costo.def <- abs(max(0,sum(cumsum(rep(max(deg.ext)/100,100))-deg.ext)/1e6))
        
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
        ##        deg.sev-(100:1)
        
        mean.severity <- weighted.mean(deg.ext,deg.sev)
        ocat <- ifelse(mean.severity>30,"NT","LC")
        
        res.tab <- table(cut(deg.ext,thresholds,include.lowest=T),
                         cut(deg.sev,thresholds,include.lowest=T))
        if (res.tab[4,2]>0 | res.tab[3,3]>0 | res.tab[2,4]>0)
            ocat <- "VU"
        if (res.tab[4,3]>0 | res.tab[3,4]>0)
            ocat <- "EN"
        if (res.tab[4,4]>0)
            ocat <- "CR"

        costo.LC <- max.ext*sum(max(0,max(deg.ext[deg.sev>thresholds[4]])-thresholds[2]),
                                max(0,max(deg.ext[deg.sev>thresholds[3]])-thresholds[3]),
                                max(0,max(deg.ext[deg.sev>thresholds[2]])-thresholds[4]))/100
        costo.VU <- max.ext*sum(max(0,max(deg.ext[deg.sev>thresholds[4]])-thresholds[3]),
                                max(0,max(deg.ext[deg.sev>thresholds[3]])-thresholds[4]))/100
        costo.EN <- max.ext*(max(0,max(deg.ext[deg.sev>thresholds[4]])-thresholds[4]))/100
        
        w.pres <- rbind(w.pres,
                        data.frame(mcdg,
                                   pais=ifelse(j %in% "global",j,as.character(paises[match(j,paises$OBJECTID),"NAME"])),
                                   mean.severity,
                                   costo.LC,
                                   costo.VU,
                                   costo.EN,
                                   C2b=ocat))
        
    } else {
        w.pres <- rbind(w.pres,
                        data.frame(mcdg,pais=ifelse(j %in% "global",j,as.character(paises[match(j,paises$OBJECTID),"NAME"])),
                                   mean.severity=NA,
                                   costo.LC=NA,
                                   costo.VU=NA,
                                   costo.EN=NA,
                                   C2b="NE"))

    } 

}



##cambio climático

thresholds <- c(0,30,50,80,100)

if (file.exists(sprintf("%s/resultadosRF.rda",output.dir))) {
    load(sprintf("%s/resultadosRF.rda",output.dir))
    
    if (length(ls(pattern="^RS\\."))>0) {
        dts <- data.frame()
        for (k in ls(pattern="^RS\\.")) {
            cat(sprintf("\n%s.",k))
            rtmp <- get(k)
            area.cell <- values(area(rtmp))
            
            lss <- c("global",unique(values(rpaises * !is.na(rtmp))))
            lss <- lss[! lss %in%  c(NA,"0")]
            
            for (l in lss) {
                cat(sprintf("%s.",l))
                
                if (l %in% "global") {
                    sev.vals <- values(rtmp)[values(rpaises) %in% as.numeric(lss[-1])]
                } else {
                    sev.vals <- values(rtmp)[values(rpaises) %in% as.numeric(l)]
                }
                
                RSE <- ecdf(-1*sev.vals)
                mean.severity <- mean(sev.vals,na.rm=T)  

                Cat <- ifelse(mean.severity>30,"NT","LC")
                if (RSE(-1*thresholds[4])>thresholds[2]/100) {
                    Cat <- "VU"
                }
                if (RSE(-1*thresholds[3])>thresholds[3]/100) {
                    Cat <- "VU"             
                }
                if (RSE(-1*thresholds[2])>thresholds[4]/100) {
                    Cat <- "VU"
                }
                if (RSE(-1*thresholds[4])>thresholds[3]/100) {
                    Cat <- "EN"
                }
                if (RSE(-1*thresholds[3])>thresholds[4]/100) {
                    Cat <- "EN"
                    
                }
                if (RSE(-1*thresholds[4])>thresholds[4]/100) {
                    Cat <- "CR"
                }

                
                costo.LC <- sum(max(0,(RSE(-1*thresholds[4])-(thresholds[2]/100))*sum(area.cell[values(rpaises) %in% l],na.rm=T)),
                                max(0,(RSE(-1*thresholds[3])-(thresholds[3]/100))*sum(area.cell[values(rpaises) %in% l],na.rm=T)),
                                max(0,(RSE(-1*thresholds[2])-(thresholds[4]/100))*sum(area.cell[values(rpaises) %in% l],na.rm=T)))

                costo.VU <- sum(max(0,(RSE(-1*thresholds[4])-(thresholds[3]/100))*sum(area.cell[values(rpaises) %in% l],na.rm=T)),
                                max(0,(RSE(-1*thresholds[3])-(thresholds[4]/100))*sum(area.cell[values(rpaises) %in% l],na.rm=T)))
                
                costo.EN <- max(0,(RSE(-1*thresholds[4])-(thresholds[4]/100))*sum(area.cell[values(rpaises) %in% l],na.rm=T))

                ##qry <- (RelativeSeverityPlot(rpaises %in% l,rtmp,"C2a",do.plot=F))
                
                dts <- rbind(dts,data.frame(mcdg,pais=ifelse(l %in% "global",l,TMWB@data[as.numeric(l),"NAME"]),
                                            md=strsplit(k,"\\.")[[1]][2],
                                            sc=strsplit(k,"\\.")[[1]][3],
                                            costo.LC,
                                            costo.VU,
                                            costo.EN,
                                            mean.severity=mean.severity,
                                            C2a=Cat))
            }
            rm(rtmp,costo.LC,costo.VU,costo.EN,Cat,mean.severity,RSE,area.cell,sev.vals)

        }
        
        c.futr <- rbind(c.futr,dts[order(dts$pais,dts$sc),])
        
        rm(list=ls(pattern="^RS."))
        gc()
    }
}

