
resumen.resultados <- data.frame()
for (subcriterion in c("A1","A2a",##"A2b",
                       "A3","B1","B2","B3",
                       "C2a",
                       "C2b",
                       "D1","D2b","D3","oacats")) {
    resumen.resultados <- rbind(resumen.resultados,
                                data.frame(subcriterion,
                                           DD=sum(actuales[,subcriterion] %in% c("DD")),
                                           CO=sum(actuales[,subcriterion] %in% c("CO")),
                                           CR=sum(actuales[,subcriterion] %in% c("CR")),
                                           EN=sum(actuales[,subcriterion] %in% c("EN")),
                                           VU=sum(actuales[,subcriterion] %in% c("VU")),
                                           NT=sum(actuales[,subcriterion] %in% c("NT")),
                                           LC=sum(actuales[,subcriterion] %in% c("LC")),
                                           NE=sum(actuales[,subcriterion] %in% c("NE")),
                                           amenazados=sum(actuales[,subcriterion] %in% c("VU","EN","CR")),
                                           total=sum(!actuales[,subcriterion] %in% c("DD","NE")),
                                           evaluados=sum(!actuales[,subcriterion] %in% c("NE"))))
}

resumen.resultados


resumen.global <- subset(actuales,pais %in% "global")[,c("mcdg",
                                                         "A1","bounds.A1",
                                                         "A2a","bounds.A2a",
                                                         "A3","bounds.A3",
                                                         "B1","B2","B3",
                                                         "C2a","bounds.C2a",
                                                         "C2b","D1","D2b","D3","oacats","oabounds","tcats")]

resumen.global$frmt <- tipologia$format[match(resumen.global$mcdg,tipologia$macrogroup_key)]
resumen.global$dvs <- tipologia$Division.Code[match(resumen.global$mcdg,tipologia$macrogroup_key)]
resumen.global$name <- tipologia$macrogroup_name[match(resumen.global$mcdg,tipologia$macrogroup_key)]
resumen.global <- resumen.global[with(resumen.global,order(as.character(dvs),as.character(mcdg))),
               c("dvs","frmt","mcdg","name",
                 "A1","bounds.A1",
                 "A2a","bounds.A2a",
                 "A3","bounds.A3",
                 "B1","B2","B3",
                 "C2a","bounds.C2a",
                 "C2b","D1","D2b","D3","oacats","oabounds","tcats"
                 )]


## hay un error de asignación aquí, debe ser NE no DD
resumen.global[resumen.global$frmt %in% c("1.B.1","1.B.2","1.B.3"),"D2b"] <- "NE"
resumen.global$tcats[resumen.global$oacats %in% c("DD","NE","LC")]
resumen.global$tcats[resumen.global$oacats %in% c("DD","NE","LC")] <- NA

subset(resumen.global,dvs %in% "1.A.1.Ea")
resumen.global[resumen.global$frmt %in% c("1.B.1","1.B.2","1.B.3"),"D2b"]


with(subset(resumen.global,oacats %in% "CR"),sprintf("%s %s %s (%s)",mcdg,name,oacats,oabounds))

##write.csv(file=sprintf("%s/MGs/%s_resumen_resultados.csv",gsub("doc","output",mi.path),hoy),resumen.global)

## chequear diferencias
t1 <- resumen.global##read.csv(file=sprintf("%s/MGs/%s_resumen_resultados.csv",gsub("doc","output",mi.path),hoy))
t2 <- read.csv(file=sprintf("%s/MGs/%s_resumen_resultados.csv",gsub("doc","output",mi.path),referencia))
 table(t1$oacats,t2$oacats)

       cat(sprintf("Chequeo de consistencia con versión del %s : ",referencia))
for (k in colnames(t1)) {
    if (!k %in% colnames(t2)) {
        cat(sprintf("falta %s; \n",k))
    } else {
        viejo <- t2[,k]
        if (is.factor(t1[,k])) {
            nuevo <- droplevels(t1[,k])
        } else {
            nuevo <- t1[,k]
          if (is.factor(t2[,k])) {
              viejo <- as.character(t2[,k])
          }
        }
        if (all(nuevo==viejo,na.rm=T)) {
            ##cat(sprintf("%s bien;",k))
            cat(sprintf("*",k))
        } else {
            cat(sprintf("diferencia en %s; \n",k))
            
        }
    }
}
       cat(sprintf(": Fin \n",k))



    
## tropicales amenazados por A1 y A2a, templados por A3, sin embargo no es significativo
chisq.test(table(substr(resumen.global$frmt,1,3),grepl("[A-D]2",resumen.global$tcats)))


subset(resumen.global,oacats %in% "CR")


