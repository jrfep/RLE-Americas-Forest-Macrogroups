## esta es la función para incorporar cambios en las categorias segun la inversión en conservación...
mejorar.Cat <- function(x,crit,meta,costo1,costo2,costo3,crit.min=NA,crit.max=NA) {
    bounds <- NA
    costo <- 0
    if (is.na(crit.min))
        crit.min <- crit
    if (is.na(crit.max))
        crit.max <- crit
    
    if (nrow(x)<1) {
        Cat <- "DD"
    } else {
        if (is.na(x[,crit])) {
            Cat <- "DD"
        } else {
            if (x[,crit] %in% c("NE","LC","DD")) {
                Cat <- x[,crit]
            } else {
                if (x[,crit] %in% c("NT","VU","EN","CR","CO")) {
                    if(!is.na(x[,costo1]) & x[,costo1]<meta) {
                        Cat <- "LC"
                        costo <- x[,costo1]
                    } else {
                        Cat <- x[,crit]
                    }
                }
                if (x[,crit] %in% c("EN","CR","CO")) {
                    if(!is.na(x[,costo2]) & x[,costo2]<meta) {
                        Cat <- "VU"
                        costo <- x[,costo2]
                    } else {
                        Cat <- x[,crit]
                    }
                }
                if (x[,crit] %in% c("CR","CO")) {
                    if(!is.na(x[,costo3]) & x[,costo3]<meta) {
                        Cat <- "EN"
                        costo <- x[,costo3]
                    } else {
                        Cat <- x[,crit]
                    }
                }
            }
        }
    }
    if ((!Cat %in% c("NE","DD")) & costo == 0) {
        if(!is.na(x[,crit.min]) & !is.na(x[,crit.max])) {
            if (x[,crit.min]!=x[,crit.max]) {
                bounds <- sprintf("%s -- %s",x[,crit.min],x[,crit.max])
            }
        }
    }

    return(list(Cat=as.character(Cat),costo=costo, bounds=bounds))
}
