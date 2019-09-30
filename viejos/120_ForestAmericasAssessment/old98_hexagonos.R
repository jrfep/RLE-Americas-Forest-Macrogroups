## current distribution
## (potential distribution crossed with current forest cover)



## version basada en hexagonos
shp.dir <- "~/Rdata/Provita/MGshp"
(load(sprintf("%s/%s.rda",shp.dir,mcdg)))



## chequear versiones actualizadas de cada carpeta:

mi.rda <- "~/Dropbox/Provita/Rdata"
chk.dir <- "~/Rdata/Provita/MGcheck3"

spp.dir <- "~/Rdata/Provita/MGspp"
bsq.dir <- "~/Rdata/Provita/MGbsq.20170219"



mpl <- 1.6
Final.rda <- sprintf("%s/20170724_ResultadosParaPublicacion.rda",mi.rda)
##(load(Final.rda))

source(sprintf("%s/%s/inc72_aplicarFiltros.R",mi.path,mi.dir))

with(subset(tipologia,macrogroup_key %in% tds.cdg),table(sgrp))
table(tipologia$subclass)


source(sprintf("%s/%s/inc71_agruparPorRegiones.R",mi.path,mi.dir))

mean(rsm$BZ.valid/rsm$total>.64)

## 

## read table from raster crossing
source(sprintf("%s/%s/inc73_Calculos_AOO.R",mi.path,mi.dir))

source(sprintf("%s/%s/inc74_rsmA3_v2.R",mi.path,mi.dir))
source(sprintf("%s/%s/inc75_A3_270m.R",mi.path,mi.dir))



## ¿donde esta validos...?
##ss <- !rsm.v6$mcdg %in% descartados
##table(sprintf("%s (%s--%s)",rsm.v6$A3.mean[ss],rsm.v6$A3.min[ss],rsm.v6$A3.max[ss]))
##table(A3.dir[ss],A3.mean[ss])


    
    for (g in c("26","45","60","85")) {
        if (nrow(mc)<1) {
            mic2a <- "DD"
        } else {
            if (nrow(subset(mc,sc==g))<1) {
                mic2a <- "DD"
            } else {
                if (mc$costo.cli<b) {
                    mic2a <- "LC"
                } else {
                    tab.cli <- table(factor(subset(mc,sc==g)$C2a,levels=names(IUCN.cats)))
                    tab.cli <- tab.cli[tab.cli>0]
                    mic2a <- names(which.max(tab.cli))
                }
            }
        }
    

    
                                dts <- rbind(dts,tmp)
    }
                            r <- r+1
                        }
                        q <- q+1
                    }
                    p <- p+1
                }
                o <- o+1
            }
            n <- n+1
        }
        m <- m+1
    }    



dts$catorig <-     subset(dts,escenario %in% "p 0 r 0 f 0 h 0 s 0 a 0 c 60")$oacats
    todos <- rbind(todos,dts)

    seleccion <- !(dts$oacat %in% dts$catorig)
    tmp <- dts[seleccion,]

    n <- 0
    for (k in unique(tmp$oacats)) {
        pss <- subset(tmp,oacats %in% k)
        pss <- pss[which.min(rowSums(pss[,c("proteger","reforestar","fauna","hidrico","restaurar","emision","adaptar")])),]
        n <- n+1
        posibles <- rbind(posibles,pss)
    }
    tmp <- subset(dts,escenario %in% "p 0 r 0 f 0 h 0 s 0 a 0 c 60")
    tmp$opciones <- n
    actuales <- rbind(actuales,tmp)
}


source(sprintf("%s/%s/inc76_rsmValidos.R",mi.path,mi.dir))

TablaA1 <- data.frame(Formation=NA,
                      Division=NA,
                      mcdg=rsm$mcdg,
                      Name=NA,
                      valid.local=NA,
                      valid.5km=NA,
                      expert.conf=NA,
                      consistent.Bioclimate=rsm$BZ.valid/rsm$total,
                      consistent.Ecoregion=rsm$ER.valid/rsm$total,
                      consistent.CharacteristicBiota=rsm$SP.valid/rsm$total,
                      overlap=rsm$OV.valid/rsm$total,
                      consistent.ForestCover=NA,
                      overall.Valid=NA)

TablaA1$valid.local <- dts[match(TablaA1$mcdg,dts$mcdg),"valNS1"]/100
TablaA1$valid.5km <- dts[match(TablaA1$mcdg,dts$mcdg),"valNS5"]/100
TablaA1$expert.conf <- dts[match(TablaA1$mcdg,dts$mcdg),"conf"] ## this include USNVC conf and Carmen Josse conf.
TablaA1$overall.Valid <- TablaA1$mcdg %in% validos
TablaA1$consistent.ForestCover <- dts[match(TablaA1$mcdg,dts$mcdg),"Ant17"]
TablaA1$Formation <- dts[match(TablaA1$mcdg,dts$mcdg),"Formation..N.8."]
TablaA1$Name <- dts[match(TablaA1$mcdg,dts$mcdg),"IVC.Macrogroup.Code.and.Name."]
TablaA1$Division <- dts[match(TablaA1$mcdg,dts$mcdg),"Division..N..42."]

##
##write.csv2(TablaA1,file="TablaA1.csv")


match(Pat.table$mcdg,tipologia$macrogroup_key)

tipologia[match(Pat.table$mcdg,tipologia$macrogroup_key),"format"] ==  substr(Pat.table$IVC.Macrogroup.Code.and.Name.,1,5)

with(subset(tipologia,macrogroup_key %in% tds.cdg),aggregate(data.frame(format,Division.Code,macrogroup_key),list(sgrp),function(x) length(unique(x))))

write.csv(file="ListaMacrogruposNeotropicales.csv",subset(tipologia,macrogroup_key %in% tds.cdg)[,c("sgrp","format","Division.Code","macrogroup_key","macrogroup_name")])



## diferencias dramáticas
##with(dts,plot(100-remain*100/total~NSloss,ylim=c(0,100),xlim=c(0,100)))
##abline(a=0,b=1)


## muy malo
hist(dts$valNS1)
mean(dts$valNS1>43,na.rm=T)

## regular
hist(dts$valNS5)
mean(dts$valNS5>71.5,na.rm=T)

##
clrs.UICN <- rgb(c(0,255,255,255,173,0,128,255),
                 c(0,0,165,255,255,128,128,255),
                 c(0,0,0,0,47,0,128,255),maxColorValue=255)
names(clrs.UICN) <- c("CO","CR","EN","VU","NT","LC","DD","NE")

##
tbl <- aggregate(data.frame(a1700=rsm.v6$a1700.1,a2000=rsm.v6$a2000.1),
                 by=list(formation=tipologia$format[match(rsm.v6$mcdg,tipologia$macrogroup_key)]),sum,na.rm=T)
tbl$loss <- tbl$a2000*100/tbl$a1700

format.names <- as.character(tipologia$formation[tipologia$formation !="" & tipologia$format %in% tbl$formation])
names(format.names) <- substr(format.names,1,5)
format.names <- substr(format.names,7,100)
rownames(tbl) <- format.names[tbl$formation]


source(sprintf("%s/%s/inc76_plausibleBounds.R",mi.path,mi.dir))
source(sprintf("%s/%s/inc77_TreatScore.R",mi.path,mi.dir))

##validos
range(with(subset(rsm,mcdg %in% validos),valido/total))
median(with(subset(rsm,mcdg %in% validos),valido/total))
t.test(subset(d.todo,mcdg %in% validos)$prd.mean,subset(d.valido,mcdg %in% validos)$prd.mean)


length(validos)
length(descartados)
## Figura 1 mejorada según comentarios Carlos
mpl <- 1.7
##svg(file="FigA4_HistoricDivision.svg",width=9*mpl,height=4*mpl)
source(sprintf("%s/%s/inc78_FigA4.R",mi.path,mi.dir))
##rsvg-convert -f pdf -o Fig3.pdf Fig3_ThreatCountries.svg 

##dev.off()

mpl <- 7
##svg(file="Fig1_Historic.svg",width=(9/4)*mpl,height=mpl)
##source(sprintf("%s/%s/inc78_Fig1.R",mi.path,mi.dir))
source(sprintf("%s/%s/inc78_Fig1_v2.R",mi.path,mi.dir))

##dev.off()

##Figura 2
mpl <- 1.7
##svg(file="Fig2_HistPres.svg",width=5*mpl,height=7*mpl)    
source(sprintf("%s/%s/inc78_Fig2.R",mi.path,mi.dir))
##dev.off()

##Figura 3
mpl <- 6/7
##svg(file="Fig3_ThreatCountries.svg",width=5*mpl,height=7*mpl)    
source(sprintf("%s/%s/inc78_Fig3.R",mi.path,mi.dir))
##dev.off()

### para el mapa
##source(sprintf("%s/%s/inc78_Mapas.R",mi.path,mi.dir))

##Tablas Apendices
##source(sprintf("%s/%s/inc78_tablasApendices.R",mi.path,mi.dir))

## kml output for David
system("mkdir SAMvct")
system("mkdir NACvct")
prb1 <-  read.table("tabNAC/Anthromes_1700_NACv5pot.txt",stringsAsFactor=F)
 
for (mm in c("M004","M015","M028","M281","M565","M595","M618","M653")[8]) {
    if (mm %in%    sprintf("M%03d",as.numeric(prb1$V1))) { 
        system(sprintf("r.mapcalc 'tmp=if(NorthAmerica_Caribbean_IVC_MacroGroups_potential_NatureServe_v5_270m==%s,1,null())'",as.numeric(gsub("M","",mm))))
        system("r.to.vect --overwrite input=tmp@mapoteca output=tmp001 feature=area")
        system(sprintf("v.out.ogr input=tmp001@mapoteca dsn=NACvct/%1$s_%2$s.kml olayer=%1$s format=KML",mm,gsub("\\(","",gsub("\\)","",gsub("&","_",gsub(" ","_",subset(tipologia,macrogroup_key %in% mm)$macrogroup_name))))))
    }
}


## number of Macrogroup with field validation data
table(Pat.table[as.numeric(gsub("%","",Pat.table$X..Agreement.Local))>0,"mcdg"] %in% cdgs)
table(Pat.table[as.numeric(gsub("%","",Pat.table$X..Agreement.within.5km2))>0,"mcdg"] %in% cdgs)

## number of Macrogroup with expert confidence data
table(as.character(Josse.table[Josse.table[,"Very.High"] %in% 1 | Josse.table[,"High"] %in% 1 | Josse.table[,"Medium"] %in% 1,"X.17"]) %in% cdgs)





##colorblind selection from
## http://www.somersault1824.com/tips-for-designing-scientific-figures-for-color-blind-readers/
## https://betterfigures.org/2015/06/23/picking-a-colour-scale-for-scientific-graphics/
## http://bconnelly.net/2013/10/creating-colorblind-friendly-figures/
## http://colorbrewer2.org/#type=sequential&scheme=YlGnBu&n=4
    
colores1 <- c(rgb(255, 109, 182, max=255),
              rgb(255, 255, 109,max=255),
              rgb(36, 255, 36,max=255),
              rgb(109, 182, 255,max=255))

colores2 <- rgb(0,seq(0,1,length=4),.3)
plot(rep(0,4),1:4,col=colores1,pch=19,cex=5,ylim=c(0,4),xlim=c(0,4))


points(1:4,rep(0,4),col=colores2,pch=19,cex=5)



##revisar
##M650 southern chaco floodplain f&w
##M660 mexican interior riparian forest
##require(gdata)
##Jess.table <- read.xls("~/Descargas/RLE_assessments_2.xlsx")

source(sprintf("%s/%s/inc78_FigurasPresentacion.R",mi.path,mi.dir))


GWS <- data.frame()
for (k in cdgs) {
    ##GWS
    d1 <- data.frame()
    for (arch in sprintf("~/tmp/tab%s/GWSchange/%s.txt",c("SAM","NAC"),k)) {
        if (file.exists(arch)) {
            d1 <- unique(rbind(d1,read.table(arch,stringsAsFactors=F)))
        }
    }
    d1 <- subset(d1,(!V1 %in% "*") & !V2 %in% c(253,254,255,"*"))
    if (nrow(d1)>0) {
        prb <- with(d1,
                    aggregate(V3,list(cut(as.numeric(V2),c(-1,95,105,200))),sum))
        ##Reciente vs pasado
        GWS <- rbind(GWS,data.frame(k,WS=sum(prb[2:3,2])/sum(prb[2:1,2],na.rm=T)))
        
    }
}

gws.ss <- subset(GWS,k %in% subset(tipologia,format %in% c("1.A.4","1.B.3"))$macrogroup_key)

subset(tipologia, macrogroup_key %in% "M649")

prb <- with(prueba,aggregate(V3,list(as.numeric(V2)),sum))


table(cut(gws.ss$WS,c(0,.20,.50,.70,.77,Inf),label=c("CR","EN","VU","NT","LC")))


##cloud forest
tipologia[grep("Venezuelan Coastal",tipologia$macrogroup_name),]
plot(raster("cld/cru_ts_3_10.1901.2009.cld_1901_12.asc"))
