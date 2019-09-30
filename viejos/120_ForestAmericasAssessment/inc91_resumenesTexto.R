length(unique(actuales$mcdg))
length(unique(actuales$pais))
table(actuales$A1,useNA="always")
table(actuales$A2a,useNA="always")
table(actuales$A2b,useNA="always")
table(actuales$A3,useNA="always")
table(actuales$B2,useNA="always")
table(actuales$B3,useNA="always")
table(actuales$C2a,useNA="always")
table(actuales$C2b,useNA="always")
table(actuales$D1,useNA="always")
table(actuales$D2b,useNA="always")
table(actuales$D3,useNA="always")
table(actuales$oacats,useNA="always")


## summaries per formation:

tabla.resultados <- with(subset(actuales,pais %in% "global"),
                         tapply(mcdg,
                                list(tipologia$format[match(mcdg,tipologia$macrogroup_key)],
                                     factor(oacats,levels=names(IUCN.cats))),
                                function(x) length(unique(x))))
tabla.resultados[is.na(tabla.resultados)] <- 0
Ts <- c()
for (k in 1:nrow(tabla.resultados))
    Ts <- c(Ts,weighted.mean(c(NA,5:0,NA),tabla.resultados[k,],na.rm=T))
cbind(tabla.resultados,Ts)

