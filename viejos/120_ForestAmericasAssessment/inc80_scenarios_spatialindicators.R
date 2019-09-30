if (mm %in% excluir) {

    tmp <- data.frame(pais=j,mcdg=mm,
                      costo=0,
                      A1="NE",A2a="NE",##A2b="NE",
                      A3="NE",
                      B1="NE",B2="NE",B3="NE",
                      C2a="NE",C2b="NE",
                      D1="NE",D2b="NE",D3="NE",
                      costo.A1=NA,costo.A2a=NA,
                      ##costo.A2b=NA,
                      costo.A3=NA,
                      costo.B1=NA,costo.B2=NA,costo.B3=NA,
                      costo.C2a=NA,costo.C2b=NA,
                      costo.D1=NA,costo.D2b=NA,costo.D3=NA,
                      bounds.A1=NA,bounds.A2a=NA,
                      ##bounds.A2b=NA,
                      bounds.A3=NA,
                      bounds.C2a=NA,
                      oacats="NE",oabounds=NA,tcats=NA       )

    todos <- rbind(todos,tmp)

} else {
    sp <- subset(dts,pais %in% j & mcdg %in% mm)
    sh <- subset(b.hist,pais %in% j & mcdg %in% mm)
    sa <- subset(AOOs,pais %in% j & mcdg %in% mm)
    sz <- subset(d.pres,pais %in% j & mcdg %in% mm)
    sg <- subset(d.hist,pais %in% j & mcdg %in% mm)
    sw <- subset(w.pres,pais %in% j & mcdg %in% mm)
    mc <- subset(cli.rsm,pais %in% jj & mcdg %in% mm)
    
    meta.reforestar <- ceiling(sort(unique(c(0,sa$costo.a.LC,sa$costo.a.VU,sa$costo.a.EN,sh$costo.1750.LC,sh$costo.1750.VU,sh$costo.1750.EN,sh$costo.1950.LC,sh$costo.1950.VU,sh$costo.1950.EN))))
    meta.proteger <- ceiling(sort(unique(c(0,sa$costo.e.LC,sa$costo.e.VU,sa$costo.e.EN,sp$costo.2051.LC,sp$costo.2051.VU,sp$costo.2051.EN,sp$costo.2062.LC,sp$costo.2062.VU,sp$costo.2062.EN))))
    meta.fauna <- ceiling(sort(unique(c(0,sz$costo.LC,sz$costo.VU,sz$costo.EN))))
    meta.restaurar <- ceiling(sort(unique(c(0,sg$costoD1.LC,sg$costoD1.VU,sg$costoD1.EN,sg$costoD3.LC,sg$costoD3.VU,sg$costoD3.EN))))
    meta.hidrico <- ceiling(sort(unique(c(0,sw$costo.LC,sw$costo.VU,sw$costo.EN))))
    meta.adaptar <- ceiling(sort(unique(c(0,mc$costo.LC,mc$costo.VU,mc$costo.EN))))

    metas <- sort(unique(c(meta.reforestar,meta.proteger,meta.fauna,meta.restaurar,meta.hidrico,meta.adaptar)))

    
    ##análisis de todos los subcriterios, pero falta considerar cambio climático (incertidumbre por usar varios modelos/escenarios)...
    
    tmp <- data.frame()
    for (meta in metas) {
        tmp <- rbind(tmp,
                     data.frame(pais=j,mcdg=mm,
                      costo=meta,
                      A1=mejorar.Cat(sh,"A1",meta,"costo.1950.LC","costo.1950.VU","costo.1950.EN")$Cat,
                      A2a=mejorar.Cat(sp,"A2a",meta,"costo.2062.LC","costo.2062.VU","costo.2062.EN")$Cat,
                      ##A2b=mejorar.Cat(sp,"A2b",meta,"costo.2051.LC","costo.2051.VU","costo.2051.EN")$Cat,
                      A3=mejorar.Cat(sh,"A3",meta,"costo.1750.LC","costo.1750.VU","costo.1750.EN")$Cat,
                      B1=mejorar.Cat(sa,"B1",meta,"costo.e.LC","costo.e.VU","costo.e.EN")$Cat,
                      B2=mejorar.Cat(sa,"B2",meta,"costo.a.LC","costo.a.VU","costo.a.EN")$Cat,
                      B3=mejorar.Cat(sa,"B3",meta,"costo.a.LC","costo.a.VU","costo.a.EN")$Cat,
                      C2a=   mejorar.Cat(mc,"C2a",meta,"costo.LC","costo.VU","costo.EN")$Cat,
                      C2b=mejorar.Cat(sw,"C2b",meta,"costo.LC","costo.VU","costo.EN")$Cat,
                      D1=mejorar.Cat(sg,"D1",meta,"costoD1.LC","costoD1.VU","costoD1.EN")$Cat,
                      D2b=mejorar.Cat(sz,"D2b",meta,"costo.LC","costo.VU","costo.EN")$Cat,

                      D3=mejorar.Cat(sg,"D3",meta,"costoD3.LC","costoD3.VU","costoD3.EN")$Cat,
                      costo.A1=mejorar.Cat(sh,"A1",meta,"costo.1950.LC","costo.1950.VU","costo.1950.EN")$costo,
                      costo.A2a=mejorar.Cat(sp,"A2a",meta,"costo.2062.LC","costo.2062.VU","costo.2062.EN")$costo,
                      ##costo.A2b=mejorar.Cat(sp,"A2b",meta,"costo.2051.LC","costo.2051.VU","costo.2051.EN")$costo,
                      costo.A3=mejorar.Cat(sh,"A3",meta,"costo.1750.LC","costo.1750.VU","costo.1750.EN")$costo,
                      costo.B1=mejorar.Cat(sa,"B1",meta,"costo.e.LC","costo.e.VU","costo.e.EN")$costo,
                      costo.B2=mejorar.Cat(sa,"B2",meta,"costo.a.LC","costo.a.VU","costo.a.EN")$costo,
                      costo.B3=mejorar.Cat(sa,"B3",meta,"costo.a.LC","costo.a.VU","costo.a.EN")$costo,
                      costo.C2a=mejorar.Cat(mc,"C2a",meta,"costo.LC","costo.VU","costo.EN")$costo,
                      costo.C2b=mejorar.Cat(sw,"C2b",meta,"costo.LC","costo.VU","costo.EN")$costo,
                      costo.D1=mejorar.Cat(sg,"D1",meta,"costoD1.LC","costoD1.VU","costoD1.EN")$costo,
                      costo.D2b=mejorar.Cat(sz,"D2b",meta,"costo.LC","costo.VU","costo.EN")$costo,

                                costo.D3=mejorar.Cat(sg,"D3",meta,"costoD3.LC","costoD3.VU","costoD3.EN")$costo,
                                bounds.A1=mejorar.Cat(sh,"A1",meta,"costo.1950.LC","costo.1950.VU","costo.1950.EN","A1.max","A1.min")$bounds,
                                ## esto lo cambiamos en marzo 2018:
                                bounds.A2a=mejorar.Cat(sp,"A2a",meta,"costo.2062.LC","costo.2062.VU","costo.2062.EN","A2a.min","A2a.max")$bounds,
                                ## esto lo quitamos en marzo 2018:
                                ##bounds.A2b=mejorar.Cat(sp,"A2b",meta,"costo.2051.LC","costo.2051.VU","costo.2051.EN","A2b.max","A2b.min")$bounds,
                                bounds.A3=mejorar.Cat(sh,"A3",meta,"costo.1750.LC","costo.1750.VU","costo.1750.EN","A3.max","A3.min")$bounds,
                                bounds.C2a=ifelse(length(mc$bounds)==0,NA,as.character(mc$bounds))
))
    }

    ## cambios en la categoria final según los cambios en las categorias de cada criterio
    for (d in 1:nrow(tmp)) {

        ##### esto lo mejoramos en marzo 2018:
        todas.0 <- as.character(unname(unlist(tmp[d,c("A1", "A2a", "A3", "B1", "B2", "B3", "C2a", "C2b", "D1", "D2b", "D3"),drop=T])))
        todas <- unique(todas.0[!todas.0 %in% c("DD","NE")])
        final.cat <- todas[which.min(match(todas,names(IUCN.cats)))]
        final.crit <- paste(c("A1", "A2a", "A3", "B1", "B2", "B3", "C2a", "C2b", "D1", "D2b", "D3")[todas.0 %in% final.cat],collapse=", ")

        if (is.na(tmp[d,"bounds.A1"])) {
            a1s <- tmp[d,"A1"]
        } else {
            qry <- match(strsplit(as.character(tmp[d,"bounds.A1"])," -- ")[[1]],names(IUCN.cats))
            a1s <- names(IUCN.cats)[qry[1]:qry[2]]
        }
        if (is.na(tmp[d,"bounds.A2a"])) {
            a2s <- tmp[d,"A2a"]
        } else {
            qry <- match(strsplit(as.character(tmp[d,"bounds.A2a"])," -- ")[[1]],names(IUCN.cats))
            a2s <- names(IUCN.cats)[qry[1]:qry[2]]
        }
        if (is.na(tmp[d,"bounds.A3"])) {
            a3s <- tmp[d,"A3"]
        } else {
            qry <- match(strsplit(as.character(tmp[d,"bounds.A3"])," -- ")[[1]],names(IUCN.cats))
            a3s <- names(IUCN.cats)[qry[1]:qry[2]]
        }
        if (is.na(tmp[d,"bounds.C2a"])) {
            c2s <- tmp[d,"C2a"]
        } else {
            qry <- match(strsplit(as.character(tmp[d,"bounds.C2a"]),"--")[[1]],names(IUCN.cats))
            if (length(qry)>1) {
                c2s <- names(IUCN.cats)[qry[1]:qry[2]]
            } else {
                c2s <- names(IUCN.cats)[qry]
            }
        }

final.comb <- c()
        for (k1 in a1s) {
            for (k2 in a2s) {
                for (k3 in a3s) {
                    for (k4 in c2s) {
                        combinaciones <- as.character(unname(unlist(tmp[d,c("A1", "A2a", "A3", "B1", "B2", "B3", "C2a", "C2b", "D1", "D2b", "D3"),drop=T])))
                        combinaciones[1] <- k1
                        combinaciones[2] <- k2
                        combinaciones[3] <- k3
                        combinaciones[7] <- k4
                        todas <- unique(combinaciones[!combinaciones %in% c("DD","NE")])
                        final.comb <- c(final.comb,todas[which.min(match(todas,names(IUCN.cats)))])

                    }
                }
            }
        }

        final.bounds <- paste(unique(names(IUCN.cats)[rev(range(match(   final.comb,names(IUCN.cats))))]),collapse=" -- ")
        
        tmp$oacats[d] <- final.cat
        tmp$oabounds[d] <- ifelse(final.bounds != final.cat,final.bounds,NA)
        tmp$tcats[d] <- ifelse(final.cat %in% c("LC","DD","NE"),NA,final.crit)
     }       

    todos <- rbind(subset(tmp,!duplicated(oacats)),todos)

}
