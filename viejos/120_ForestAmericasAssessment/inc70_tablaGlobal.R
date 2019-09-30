j <- jj <- "global"
tmp <- data.frame()
for (mm in cdgs) {
    if (mm %in% excluir) {
        tmp <- rbind(tmp,data.frame(mcdg=mm,
                                    A1="NE",A2a="NE",A2b="NE",A3="NE",
                                    B1="NE",B2="NE",B3="NE",
                                    C2a="NE",C2b="NE",
                                    D1="NE",D2b="NE",D3="NE",
                                    A1.min="NE",A1.max="NE",
                                    A2a.min="NE",A2a.max="NE",
                                    A2b.min="NE",A2b.max="NE",
                                    A3.min="NE",A3.max="NE",
                                    C2a.min="NE",C2a.max="NE"       ))


    } else {
        sp <- subset(b.pres,pais %in% j & mcdg %in% mm)
        sh <- subset(b.hist,pais %in% j & mcdg %in% mm)
        sa <- subset(AOOs,pais %in% j & mcdg %in% mm)
        sz <- subset(d.pres,pais %in% j & mcdg %in% mm)
        sg <- subset(d.hist,pais %in% j & mcdg %in% mm)
        sw <- subset(w.pres,pais %in% j & mcdg %in% mm)
        mc <- subset(cli.rsm,pais %in% jj & mcdg %in% mm)
        
        tmp <- rbind(tmp,data.frame(mcdg=mm,
                                    A1=ifelse(nrow(sh==1),as.character(sh$A1),"NE"),
                                    A2a=ifelse(nrow(sp==1),as.character(sp$A2a),"NE"),
                                    A2b=ifelse(nrow(sp==1),as.character(sp$A2b),"NE"),
                                    A3=ifelse(nrow(sh==1),as.character(sh$A3),"NE"),
                                    
                                    
                                    B1=ifelse(nrow(sa)==1,as.character(sa$B1),"NE"),
                                    B2=ifelse(nrow(sa)==1,as.character(sa$B2),"NE"),
                                    B3=ifelse(nrow(sa)==1,as.character(sa$B3),"NE"),
                                    
                                    C2a=ifelse(nrow(mc)==1,as.character(mc$C2a),"NE"),
                                    C2b=ifelse(nrow(sw)==1,as.character(sw$C2b),"NE"),
                                    D1=ifelse(nrow(sg)==1,as.character(sg$D1),"NE"),
                                    D2b=ifelse(nrow(sz)==1,as.character(sz$D2b),"NE"),
                                    D3=ifelse(nrow(sg)==1,as.character(sg$D3),"NE"),
                                    A1.min=ifelse(nrow(sh==1),as.character(sh$A1.max),"NE"),
                                    A1.max=ifelse(nrow(sh==1),as.character(sh$A1.min),"NE"),
                                    A2a.min=ifelse(nrow(sp==1),as.character(sp$A2a.max),"NE"),
                                    A2a.max=ifelse(nrow(sp==1),as.character(sp$A2a.min),"NE"),
                                    A2b.min=ifelse(nrow(sp==1),as.character(sp$A2b.max),"NE"),
                                    A2b.max=ifelse(nrow(sp==1),as.character(sp$A2b.min),"NE"),
                                    A3.min=ifelse(nrow(sh==1),as.character(sh$A3.max),"NE"),
                                    A3.max=ifelse(nrow(sh==1),as.character(sh$A3.min),"NE"),
                                    
                                    C2a.min=ifelse(nrow(mc)==1,strsplit(as.character(mc$bounds),"--")[[1]][1],"NE"),
                                    C2a.max=ifelse(nrow(mc)==1,strsplit(as.character(mc$bounds),"--")[[1]][2],"NE")))
        
        
    }
}

    ## cambios en la categoria final según los cambios en las categorias de cada criterio
    for (d in 1:nrow(tmp)) {
        for (cc in rev(names(IUCN.cats))[2:7]) {
            tcat <- c()
            for (kk in 2:13) {
                if (tmp[d,kk] %in% cc) {
                    tmp$oacats[d] <- cc
                    tcat <- c(tcat,colnames(tmp)[kk])
                }
            }
            if (length(tcat)>0)
                tmp$tcats[d] <- paste(tcat,collapse=", ")            
        }
    }

tmp$tcats[tmp$oacats %in% "LC"] <- NA

for (d in 1:nrow(tmp)) {
    altcats <- c()
    slc <- colnames(tmp)[2:13]
    for (cc in rev(names(IUCN.cats))[2:7]) {
        for (kk in c(2:13)) {
                if (tmp[d,kk] %in% cc) {
                    tmp$oacats[d] <- cc
                }
            }
        }
    }


    todos <- rbind(subset(tmp,!duplicated(oacats)),todos)
    ## de aquí para abajo falta revisar...
}
}
