## versión revisada diciembre 2017

## preambulo en grass
## inc00_crosstablesSAM.R
## inc00_crosstablesNAC.R

##R --vanilla
setwd("~/tmp")
require(foreign)
require(gdata)
require(vegan)
require(xtable)
require(rgeos)
require(rgdal)
require(plotrix)
 require(raster)
require(ROpenOffice)
##require(spgrass6)

hoy <- format(Sys.time(), "%Y%m%d")
##g6 <- gmeta()

mi.path <- "~/Provita/doc/"
##mi.path <- "~/Dropbox/Provita/doc/"
mi.dir <- "000_RLE"
mi.dir <- "001_ForestAmericasAssessment"


source(sprintf("%s/%s/inc00_preambuloAnalisis.R",mi.path,mi.dir))

## selección de macrogrupos
tds.cdg <- sort(sprintf("M%03d",unique(r2$mg)))
cdgs <- tds.cdg
cdgs <- tds.cdg[tds.cdg %in% unique(drop.levels(subset(tipologia,grp==1 & !format %in% c("1.B.4","1.B.5") & macrogroup_key!="")$macrogroup_key))]


## un ejemplo del bosque seco de la Guajira
frmt <- "1.A.1"
mcdg <- "M563"
output.dir <- sprintf("~/output/Provita/MGs/%s/%s",frmt,mcdg)
cdg <- as.numeric(gsub("M","",mcdg))
gc()
## leer tabla bsq
for (k in 2001:2012) {
    print(k)
    system(sprintf("grep ^%1$s calculos_201712/SAM_Paises_AOO_bsq%2$s.tab > %3$s/Paises_AOO_bsq%2$s",cdg,k,output.dir))
    system(sprintf("grep ^%1$s calculos_201712/NAC_Paises_AOO_bsq%2$s.tab >> %3$s/Paises_AOO_bsq%2$s",cdg,k,output.dir))
    gc()
}
## leer tabla ant
for (k in 17:20) {
    print(k)
    system(sprintf("grep ^%1$s calculos_201712/SAM_Paises_AOO_Anthromes_%2$s00.tab > %3$s/Paises_AOO_ant%2$s00",cdg,k,output.dir))
    system(sprintf("grep ^%1$s calculos_201712/NAC_Paises_AOO_Anthromes_%2$s00.tab >> %3$s/Paises_AOO_ant%2$s00",cdg,k,output.dir))
    gc()
}

for (k in 2001:2012) {
    tmp2 <- read.table(sprintf("%s/Paises_AOO_bsq%s",output.dir,k))
    assign(sprintf("bsq.%s",k),
           tmp2)
}
for (k in 17:20) {
    tmp2 <- read.table(sprintf("%s/Paises_AOO_ant%s00",output.dir,k))
    assign(sprintf("ant.%s00",k),
           tmp2)
}
    
b.pres <- b.hist <- A00s <- data.frame()
if (cdg %in% bsq.2012$V1) {

    country.year <- data.frame()
    for (k in 2001:2012) {
        tt <- with(subset(get(sprintf("bsq.%s",k)),!(V2 %in% "*") & V1 %in% cdg & V4 %in% 40:100),
                   tapply(V5,list(V2),function(x) sum(x/1e6,na.rm=T)))
        
        country.year <- rbind(country.year,tt)
        colnames(country.year) <- names(tt)
    }
    country.year <- country.year[,colSums(is.na(country.year))==0]

    country.cntr <- data.frame()
    for (k in 17:20) {
        tt <- with(subset(get(sprintf("ant.%s00",k)),!(V2 %in% "*") & V1 %in% cdg & V4 %in% legend.Anthromes$value[grep("woodland",legend.Anthromes$nombre)]),
                   tapply(V5,list(V2),function(x) sum(x/1e6,na.rm=T)))
        
        country.cntr <- rbind(country.cntr,tt)
        colnames(country.cntr) <- names(tt)
    }
    country.cntr <- country.cntr[,colSums(is.na(country.cntr))==0]

    country.cell <- with(subset(bsq.2012,!(V2 %in% "*") & V1 %in% cdg & V4 %in% 40:100),
               tapply(V5,list(V2,V3),function(x) sum(x/1e6,na.rm=T)))
    
    a1 <- rowSums(country.cell>1,na.rm=T)
    a0 <- rowSums(country.cell>0,na.rm=T)
    if (any(a1>0))
        A00s <- rbind(A00s,
                      data.frame(mcdg,pais=names(a1)[a1>0],A00=a1[a1>0],min=1))
    
    if (any(a0>0))
        A00s <- rbind(A00s,
                      data.frame(mcdg,pais=names(a0)[a0>0],A00=a0[a0>0],min=0))
    
    
    for (l in colnames(country.year)) {
        bsq <- country.year[,l] 
        yr <- (2001:2012)
        glm01 <-  glm(bsq~yr,family=quasipoisson(log))
        ##tiene que ser type link y luego transformar para evitar los valores negativos
        
        prd1 <- predict(glm01,newdata=data.frame(yr=c(2001,2051,2062)),type="link",se.fit=T)
        b.pres <- 
            rbind(b.pres,
                      data.frame(mcdg,pais=l,
                                 p2051=exp(prd1$fit[2])/exp(prd1$fit[1]),

                                 n2051=exp(prd1$fit[2]-prd1$se.fit[2]*2)/
                                     exp(prd1$fit[1]-prd1$se.fit[1]*2),
                                 x2051=exp(prd1$fit[2]+prd1$se.fit[2]*2)/
                                     exp(prd1$fit[1]+prd1$se.fit[1]*2),
                                 p2062=exp(prd1$fit[3])/exp(prd1$fit[1]),
                                 n2062=exp(prd1$fit[3]-prd1$se.fit[3]*2)/
                                     exp(prd1$fit[1]-prd1$se.fit[1]*2),
                                 x2062=exp(prd1$fit[3]+prd1$se.fit[3]*2)/
                                     exp(prd1$fit[1]+prd1$se.fit[1]*2)))
    }

    for (l in colnames(country.cntr)) {
        bsq <- country.cntr[,l] 
        yr <- (17:20)*100
        glm02 <-  glm(bsq~poly(yr,2),family=quasipoisson(log))
        ##tiene que ser type link y luego transformar para evitar los valores negativos
        
        prd1 <- predict(glm02,newdata=data.frame(yr=c(1700,1950,2000)),type="link",se.fit=T)
        b.hist <- 
            rbind(b.hist,
                      data.frame(mcdg,pais=l,
                                 p1950=exp(prd1$fit[3])/exp(prd1$fit[2]),

                                 n1950=exp(prd1$fit[3]-prd1$se.fit[3]*2)/
                                     exp(prd1$fit[2]-prd1$se.fit[2]*2),
                                 x1950=exp(prd1$fit[3]+prd1$se.fit[3]*2)/
                                     exp(prd1$fit[2]+prd1$se.fit[2]*2),
                                 p2000=exp(prd1$fit[3])/exp(prd1$fit[1]),
                                 n2000=exp(prd1$fit[3]-prd1$se.fit[3]*2)/
                                     exp(prd1$fit[1]-prd1$se.fit[1]*2),
                                 x2000=exp(prd1$fit[3]+prd1$se.fit[3]*2)/
                                     exp(prd1$fit[1]+prd1$se.fit[1]*2)))
    }


}
























    tmp1 <- read.table(sprintf("",k))
    tmp2 <- read.table(sprintf("calculos_201712/NAC_Paises_AOO_bsq%s.tab",k))
    assign(sprintf("bsq.%s",k),
           unique(rbind(tmp1,tmp2)))
    rm(tmp1,tmp2)
    save(file=sprintf("%s/%s_tabla_bsq.rda",mi.rda,hoy),list=ls(pattern="^bsq"))
    gc()


## leer tabla Anthromes
for (k in 17:20) {
    tmp1 <- read.table(sprintf("calculos_201712/SAM_Paises_AOO_Anthromes_%s00.tab",k))
    tmp2 <- read.table(sprintf("calculos_201712/NAC_Paises_AOO_Anthromes_%s00.tab",k))
    assign(sprintf("ant.%s00",k),
           unique(rbind(tmp1,tmp2)))
    gc()
}


prds <- A00s <- data.frame()
for (mcdg in cdgs) {
    cdg <- as.numeric(gsub("M","",mcdg))
    if (cdg %in% rslt.2012$V1) {
        tt <- with(subset(rslt.2012,!(V2 %in% "*") & V1 %in% cdg & V4 %in% 40:100),
                   tapply(V5,list(V2,V3),function(x) sum(x/1e6,na.rm=T)))
        
        a1 <- rowSums(tt>1,na.rm=T)
        a0 <- rowSums(tt>0,na.rm=T)
        if (any(a1>0))
            A00s <- rbind(A00s,
                          data.frame(mcdg,pais=names(a1)[a1>0],A00=a1[a1>0],min=1))
        
        if (any(a0>0))
        A00s <- rbind(A00s,
                      data.frame(mcdg,pais=names(a0)[a0>0],A00=a0[a0>0],min=0))

        
        tts <- data.frame()
        for (k in 2001:2012) {
            tt <- with(subset(get(sprintf("rslt.%s",k)),!(V2 %in% "*") & V1 %in% cdg & V4 %in% 40:100),
                       tapply(V5,list(V2),function(x) sum(x/1e6,na.rm=T)))
            
            tts <- rbind(tts,tt)
            colnames(tts) <- names(tt)
        }
        tts <- tts[,colSums(is.na(tts))==0]
        for (l in colnames(tts)) {
            bsq <- tts[,l] 
            yr <- (2001:2012)
            mdl1 <- glm(bsq~yr,gaussian("log"))
            ##tiene que ser type link y luego transformar para evitar los valores negativos
            
            prd1 <- predict(mdl1,newdata=data.frame(yr=c(2001,2051,2062)),type="link",se.fit=T)
            prds <- 
                rbind(prds,
                      data.frame(mcdg,pais=l,
                                 p2051=exp(prd1$fit[2])/exp(prd1$fit[1]),

                                 n2051=exp(prd1$fit[2]-prd1$se.fit[2]*2)/
                                     exp(prd1$fit[1]-prd1$se.fit[1]*2),
                                 x2051=exp(prd1$fit[2]+prd1$se.fit[2]*2)/
                                     exp(prd1$fit[1]+prd1$se.fit[1]*2),
                                 p2062=exp(prd1$fit[3])/exp(prd1$fit[1]),
                                 n2062=exp(prd1$fit[3]-prd1$se.fit[3]*2)/
                                     exp(prd1$fit[1]-prd1$se.fit[1]*2),
                                 x2062=exp(prd1$fit[3]+prd1$se.fit[3]*2)/
                                     exp(prd1$fit[1]+prd1$se.fit[1]*2)))
        }
    }
}


table(cut(prds$p2051,breaks=c(0,.3,.5,.7,Inf),label=c("CR","EN","VU","LC")))

table(cut(prds$p2062,breaks=c(0,.3,.5,.7,Inf),label=c("CR","EN","VU","LC")))


    rslt <- read.table("calculos_201712/SAM_Paises_AOO_modisfc2012.tab")
rslt.h2012 <- read.table("calculos_201712/SAM_Paises_AOO_GFC2012.tab")

head(rslt)
tt <- with(subset(rslt,V1 %in% "563" & V4 %in% c("25","100")),
           tapply(V5,list(V2,V3),function(x) sum(x/1e6,na.rm=T)))

rowSums(tt>1,na.rm=T)
head(rslt.h2012)
tt <- with(subset(rslt.h2012,V1 %in% "563" & V4 %in% 40:100),
           tapply(V5,list(V2,V3),function(x) sum(x/1e6,na.rm=T)))

rowSums(tt>1,na.rm=T)
head(rslt.b2012)

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
