
cat(sprintf("\nAccording to IUCN RLE criterion A, %0.1f to %0.1f %% of the macrogroups would eligible for listing as threatened on the basis of historical or past rates of declines, and %0.1f %% by current and future rates of decline.\n",
        sum(resumen.global$A1 %in% c("VU","EN","CR"))*100/sum(!(resumen.global$A1 %in% c("DD","NE"))),
        sum(resumen.global$A3 %in% c("VU","EN","CR"))*100/sum(!(resumen.global$A3 %in% c("DD","NE"))),
        sum(resumen.global$A2a %in% c("VU","EN","CR"))*100/sum(!(resumen.global$A2a %in% c("DD","NE")))))

cat(sprintf("\nFor Tropical and Temperate Flooded & Swamp Forest as well as Mangroves, the mean relative severity in loss of surface water was around %0.1f +/- %0.1f, with minor differences between formations. Mean severity of degradation of climatic conditions was around %0.1f +/- %0.1f, but these values were only estimated for a subset of macrogroups with suitable models (see Methods) and were very variable between different climate change model and scenarios (Fig 3a and 3b). According to IUCN RLE criterion C, %s of %s macrogroups assessed may be eligible for threatened status due to current changes in water conditions, and %s of %s assessed may be eligible for threatened status due to predicted future changes in climatic conditions (WebTable 1).\n",
        mean(kk$mean.severity),
        sd(kk$mean.severity),
        mean(ll$x[,2]),
        sd(ll$x[,2]),
        sum(resumen.global$C2b %in% c("VU","EN","CR")),
        sum(!resumen.global$C2b %in% c("NE","DD")),
        sum(resumen.global$C2a %in% c("VU","EN","CR")),
        sum(!resumen.global$C2a %in% c("NE","DD"))))

nn$grupo <- nn$mcdg %in% subset(resumen.global,!frmt %in% c("1.A.1","1,A,2","1.A.3","1.A.4","1.A.5"))$mcdg
with(nn,t.test( mean.severity.D3~grupo))
with(nn,t.test( mean.severity.D1~grupo))

        
cat(sprintf("\nFor historical changes in intensity of human use (between 1700 and 2000), the inferred mean severity is larger in temperate than in tropical or warm temperate forest formations (%0.1f +/- %0.1f and %0.1f +/- %0.1f, respectively), but this pattern reverts in the last century with higher values in tropical than in temperate forest formations (%0.1f +/- %0.1f and %0.1f +/- %0.1f respectively) (Fig 3c and 3e). For inferred current values of defaunation in tropical forest formations the mean severity remains similarly high and variable (%0.1f +/- %0.1f), with slightly higher values in Seasonal dry tropical Forest (Fig 3d). According to IUCN RLE criterion D, between %0.1f and %0.1f %% of the macrogroups would be eligible for threatened status on the basis of historical or past rates of declines, and %0.1f%% by current rates of decline (WebTable 1).\n",
        mean(subset(nn,mcdg %in% subset(resumen.global,!frmt %in% c("1.A.1","1,A,2","1.A.3","1.A.4","1.A.5"))$mcdg)[,"mean.severity.D3"]),
        sd(subset(nn,mcdg %in% subset(resumen.global,!frmt %in% c("1.A.1","1,A,2","1.A.3","1.A.4","1.A.5"))$mcdg)[,"mean.severity.D3"]),
        mean(subset(nn,mcdg %in% subset(resumen.global,frmt %in% c("1.A.1","1,A,2","1.A.3","1.A.4","1.A.5"))$mcdg)[,"mean.severity.D3"]),
        sd(subset(nn,mcdg %in% subset(resumen.global,frmt %in% c("1.A.1","1,A,2","1.A.3","1.A.4","1.A.5"))$mcdg)[,"mean.severity.D3"]),

        mean(subset(nn,mcdg %in% subset(resumen.global,frmt %in% c("1.A.1","1,A,2","1.A.3","1.A.4","1.A.5"))$mcdg)[,"mean.severity.D1"]),
        sd(subset(nn,mcdg %in% subset(resumen.global,frmt %in% c("1.A.1","1,A,2","1.A.3","1.A.4","1.A.5"))$mcdg)[,"mean.severity.D1"]),
        mean(subset(nn,mcdg %in% subset(resumen.global,!frmt %in% c("1.A.1","1,A,2","1.A.3","1.A.4","1.A.5"))$mcdg)[,"mean.severity.D1"]),
        sd(subset(nn,mcdg %in% subset(resumen.global,!frmt %in% c("1.A.1","1,A,2","1.A.3","1.A.4","1.A.5"))$mcdg)[,"mean.severity.D1"]),


        mean(mm$mean.severity),
        sd(mm$mean.severity),
        sum(resumen.global$D3 %in% c("VU","EN","CR"))*100/sum(!resumen.global$D3 %in% c("NE","DD")),
        sum(resumen.global$D1 %in% c("VU","EN","CR"))*100/sum(!resumen.global$D1 %in% c("NE","DD")),
        sum(resumen.global$D2b %in% c("VU","EN","CR"))*100/sum(!resumen.global$D1 %in% c("NE","DD"))))

cat(sprintf("\nIntegrating all of the criteria, approximately %s %% (%s out of %s evaluated) macrogroups are eligible for threatened listing and they represent up to %0.1f %% of the current tropical and temperate forest area of the continent (Fig. 4). The overall risk category is determined mainly by the spatial criteria A and B (%0.1f %% of macrogroups), with lower contributions of functional criteria C and D (%0.1f and %0.1f %% respectively), or by a combination of spatial and functional criteria (%0.1f %%). A similar number of macrogroups is threatened by historical threats (%0.1f %% under subcriteria A1, A3, D1, D3), to that threatened by current and future threats (%0.1f %% under A2b, C2a, C2b, D2b), and %0.1f %% are threatened by both.  \n",
       sum(resumen.global$oacats %in% c("CR","EN","VU"))/
            sum(!resumen.global$oacats %in% c("NE","DD")),
            sum(resumen.global$oacats %in% c("CR","EN","VU")),
            sum(!resumen.global$oacats %in% c("NE","DD")),
            sum(total.cntr[4,sprintf("M%0.3d",as.numeric(colnames(total.cntr))) %in% subset(resumen.global,oacats %in% c("VU","EN","CR"))$mcdg])*100/sum(total.cntr[4,],na.rm=T),
            sum(grepl("[AB]",resumen.global$tcats) & !(grepl("[CD]",resumen.global$tcats)))*100/sum(!is.na(resumen.global$tcats)),
            sum(grepl("[C]",resumen.global$tcats) & !(grepl("[ABD]",resumen.global$tcats)))*100/sum(!is.na(resumen.global$tcats)),
            sum(grepl("[D]",resumen.global$tcats) & !(grepl("[ABC]",resumen.global$tcats)))*100/sum(!is.na(resumen.global$tcats)),
            sum(grepl("[CD]",resumen.global$tcats) & (grepl("[AB]",resumen.global$tcats)))*100/sum(!is.na(resumen.global$tcats)),
            sum(grepl("[13]",resumen.global$tcats) & !(grepl("2",resumen.global$tcats)))*100/sum(!is.na(resumen.global$tcats)),
            sum(grepl("[2]",resumen.global$tcats) & !(grepl("[13]",resumen.global$tcats)))*100/sum(!is.na(resumen.global$tcats)),
            sum(grepl("[2]",resumen.global$tcats) & (grepl("[13]",resumen.global$tcats)))*100/sum(!is.na(resumen.global$tcats))

 ))


 table(grepl("[C]",resumen.global$tcats),grepl("[D]",resumen.global$tcats),grepl("[AB]",resumen.global$tcats))

## mencionar si esto cambie entre global y nacional

with(subset(actuales,!is.na(tcats)),table(grepl("A",tcats),pais %in% "global"))
with(subset(actuales,!is.na(tcats)),table(grepl("B",tcats),pais %in% "global"))
with(subset(actuales,!is.na(tcats)),table(grepl("C",tcats),pais %in% "global"))
with(subset(actuales,!is.na(tcats)),table(grepl("D",tcats),pais %in% "global"))

with(subset(actuales,!is.na(tcats)),table(grepl("A",tcats)+grepl("B",tcats)+grepl("C",tcats)+grepl("D",tcats),pais %in% "global"))

 with(subset(actuales,!is.na(tcats)),table(grepl("A",tcats)+grepl("B",tcats)+grepl("C",tcats)+grepl("D",tcats),pais %in% "global"))

table(grepl("[CD]",actuales$tcats),grepl("[AB]",actuales$tcats),actuales$pais %in% "global")
## c(288,98,54)/sum(c(288,98,54))
## c(65,19,21)/sum(c(65,19,21))

## similar proportion of single or multiple causes of threat
chisq.test(with(subset(actuales,!is.na(tcats)),table((grepl("A",tcats)+grepl("B",tcats)+grepl("C",tcats)+grepl("D",tcats))>1,pais %in% "global")))

##similar proportion of functional vs. spatial
chisq.test(with(subset(actuales,!is.na(tcats)),table((grepl("C",tcats)+grepl("D",tcats))>0,pais %in% "global")))

chisq.test(with(subset(actuales,!is.na(tcats)),table((grepl("A",tcats)+grepl("B",tcats))>0,pais %in% "global")))

chisq.test(with(subset(actuales,!is.na(tcats)),table((grepl("1",tcats)+grepl("2",tcats))>0,pais %in% "global")))

##Entonces hacemos un calculo conjunto:

x <- with(subset(actuales,!is.na(tcats)),
          table(grepl("[CD]",tcats),grepl("[AB]",tcats)))
x/sum(x)

x <- with(subset(actuales,!is.na(tcats)),
          table(grepl("A[13]",tcats) | grepl("D[13]",tcats),
                grepl("[2]",tcats) | grepl("B",tcats)))
x/sum(x)

c(153,215,72)/440
c(42,45,18)/105

with(actuales, tapply(oacats %in% c("VU","EN","CR"),list(mcdg,pais),sum))
mtz <- with(actuales, tapply(oacats %in% c("VU","EN","CR"),list(mcdg,pais),sum))

boxplot(rowSums(mtz[,-ncol(mtz)]!=0,na.rm=T)/ rowSums(!is.na(mtz[,-ncol(mtz)]))~mtz[,"global"])

comp.cats <- data.frame()
for (j in unique(actuales$mcdg)) {
    gcat <- subset(actuales,mcdg %in% j & pais %in% "global")$oacats
    gcat.n <- strsplit(subset(actuales,mcdg %in% j & pais %in% "global")$oabounds," -- ")[[1]][1]
    gcat.x <- strsplit(subset(actuales,mcdg %in% j & pais %in% "global")$oabounds," -- ")[[1]][2]
    gcat.n <- ifelse(is.na(gcat.n),gcat,gcat.n)
    gcat.x <- ifelse(is.na(gcat.x),gcat,gcat.x)
    ccat <- subset(actuales,mcdg %in% j & !pais %in% "global")$oacats
    comp.cats <- rbind(comp.cats,
                       data.frame(mcdg=j,
                                  country=match(ccat,names(IUCN.cats)),
                                  global=match(gcat,names(IUCN.cats)),
                                  global.min=match(gcat.n,names(IUCN.cats)),
                                  global.max=match(gcat.x,names(IUCN.cats)))
                       )

}
 with(comp.cats,table(country > 5,global<6))
       
 with(comp.cats,table(country <6,global>5))


table(with(comp.cats,country<global))
