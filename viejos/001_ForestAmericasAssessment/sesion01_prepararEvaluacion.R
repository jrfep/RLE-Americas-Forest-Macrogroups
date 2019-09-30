## versión revisada diciembre 2017; valido en enero 2018

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
require(redlistr)

hoy <- format(Sys.time(), "%Y%m%d")
##g6 <- gmeta()

mi.path <- "~/Provita/doc/"
if (!file.exists(mi.path))
    mi.path <- "~/Provita_JRFP/doc/"
if (!file.exists(mi.path))
    mi.path <- "~/Dropbox/Provita/doc/"

mi.gis.dir <- "/home/jferrer/mi.gis"
if (!file.exists(mi.gis.dir))
    mi.gis.dir <- "/media/user3/DATOS/00_LRE/mi.gis/"
mi.gisdata.dir <- "/opt/gisdata"
if (!file.exists(mi.gisdata.dir))
    mi.gisdata.dir <- "/media/user3/DATOS/00_LRE/gisdata/"


mi.dir <- "000_RLE"
mi.dir <- "001_ForestAmericasAssessment"

source(sprintf("%s/%s/inc00_preambuloAnalisis.R",mi.path,mi.dir))

IUCN.cats <- c("Data Deficient","Collapsed","Critically Endangered","Endangered","Vulnerable","Near Threatened","Least Concern","Not Evaluated")
names(IUCN.cats) <- c("DD","CO","CR","EN","VU","NT","LC","NE")

## selección de macrogrupos
##tds.cdg <- sort(sprintf("M%03d",unique(r2$mg)))
##cdgs <- tds.cdg
##cdgs <- tds.cdg[tds.cdg %in% unique(drop.levels(subset(tipologia,grp==1 & !format %in% c("1.B.4","1.B.5") & macrogroup_key!="")$macrogroup_key))]

cdgs <- levels(rsm$mcdg)


## un ejemplo del bosque seco de la Guajira
frmt <- "1.A.1"
mcdg <- "M563"
output.dir <- sprintf("%s/MGs/%s/%s",gsub("doc","output",mi.path),frmt,mcdg)
cdg <- as.numeric(gsub("M","",mcdg))
gc()
## source("~/mi.git/redlistr/R/RelativeSeverity_functions.R")


c.futr <- w.pres <- data.frame()
d.hist <- d.pres <- data.frame()
b.pres <- b.hist <- AOOs <- data.frame()
load(sprintf("%s/%s_tablas_Mgs.rda",
             gsub("doc","Rdata",mi.path),"20180113"))

for (frmt in c("1.A.1","1.A.2","1.A.3","1.A.4","1.A.5","1.B.1","1.B.2","1.B.3")) {
    cat(sprintf("%s ::",frmt))
    for (mcdg in cdgs[cdgs %in% subset(tipologia,format %in% frmt & macrogroup_key != "")$macrogroup_key]) {
        cat(sprintf(": %s :",mcdg))

        output.dir <- sprintf("%s/MGs/%s/%s",gsub("doc","output",mi.path),frmt,mcdg)

        system(sprintf("mkdir -p %s",output.dir))
        cdg <- as.numeric(gsub("M","",mcdg))
        gc()
        
        source(sprintf("%s/%s/inc10_readTabs.R",mi.path,mi.dir))
         cat(sprintf("."))

        ## se redefinieron los costos para incorporar diferentes escalas (LC, VU, EN) en cada criterio... *~*
        if (!mcdg %in% b.pres$mcdg) {
            source(sprintf("%s/%s/inc20_spatialindicators_forest.R",mi.path,mi.dir))
            cat(sprintf("."))
        }
       if (!mcdg %in% w.pres$mcdg) {
         source(sprintf("%s/%s/inc21_environmentalindicators_forest.R",mi.path,mi.dir))
         cat(sprintf("."))
       }
         if (!mcdg %in% d.pres$mcdg) {
             source(sprintf("%s/%s/inc22_bioticindicators_forest.R",mi.path,mi.dir))
         cat(sprintf("."))
         }
        save(file=sprintf("%s/%s_tablas_Mgs.rda",
                          gsub("doc","Rdata",mi.path),hoy),c.futr,w.pres,d.hist,
             d.pres,b.pres,b.hist,AOOs )
    }
    cat(sprintf(":\n",frmt))
}

source(sprintf("%s/%s/inc25_spatialindicators_categories.R",mi.path,mi.dir))

source(sprintf("%s/%s/inc26_climateensemble_forest.R",mi.path,mi.dir))

source(sprintf("%s/%s/inc30_funciones.R",mi.path,mi.dir))

## not yet ready...
##source(sprintf("%s/%s/inc70_tablaGlobal.R",mi.path,mi.dir))




todos <- actuales <- posibles <- data.frame()
##confTest <- read.csv("~/Dropbox/MS/02_RondaN/FEE_RLE/20171130_TableAppendixA1_confidenceTests.csv",stringsAsFactors=F)
##excluir  <- subset(confTest,!Included.in.analysis)$Macrogroup.code
excluir <- c("M562","M573","M600","M614","M618","M650","M013",
              "M653","M655","M656","M659","M028","M154","M664")

for (j in unique(b.pres$pais)) {
    jj <- j
    if (j %in% "Virgin Islands")
        jj <- "United States Virgin Islands"
    if (j %in% "Turks and Caicas Islands")
        jj <- "Turks and Caicos Islands"
    if (j %in% c("Aruba","Curaçao","Bonaire, Saint Eustatius and Saba"))
        jj <- "Netherlands Antilles"
    mms <- unique(c(as.character(subset(b.pres,pais %in% j)$mcdg),as.character(subset(b.hist,pais %in% j)$mcdg),as.character(subset(d.pres,pais %in% j)$mcdg),as.character(subset(d.hist,pais %in% j)$mcdg),as.character(subset(w.pres,pais %in% j)$mcdg),as.character(subset(AOOs,pais %in% j)$mcdg,subset(c.futr,pais %in% jj)$mcdg)))

    for (mm in mms) {
        source(sprintf("%s/%s/inc80_scenarios_spatialindicators.R",mi.path,mi.dir))
    }
}

actuales <- subset(todos,costo==0)

for (mm in sort(unique(as.character(actuales$mcdg)))) {
    cat(sprintf("\n(%s) %s:",mm,
                subset(tipologia,macrogroup_key %in% mm)$macrogroup_name))
    cat(paste(unique(subset(actuales,mcdg %in% mm)$pais),collapse=","))
    
}

##Sospechosos
##(M013) Eastern North American Ruderal Forest:United States,Mexico ## debería ser otro Macrogrupo en Chile?

resumen.resultados <- data.frame()
for (subcriterion in c("A1","A2a","A2b","A3","B1","B2","B3",
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
                                       "A1","A2a","A2b","A3","B1","B2","B3",
                                       "C2a","C2b","D1","D2b","D3","oacats","tcats")]

resumen.global$frmt <- tipologia$format[match(resumen.global$mcdg,tipologia$macrogroup_key)]
resumen.global$dvs <- tipologia$Division.Code[match(resumen.global$mcdg,tipologia$macrogroup_key)]
resumen.global <- resumen.global[with(resumen.global,order(as.character(dvs),as.character(mcdg))),
               c("dvs","frmt","mcdg",
                 "A1","A2a","A2b","A3","B1","B2","B3",
                 "C2a","C2b","D1","D2b","D3","oacats","tcats")]

## hay un error de asignación aquí, debe ser NE no DD
resumen.global[resumen.global$frmt %in% c("1.B.1","1.B.2","1.B.3"),"D2b"] <- "NE"
resumen.global$tcats[resumen.global$oacats %in% c("DD","NE","LC")]
resumen.global$tcats[resumen.global$oacats %in% c("DD","NE","LC")] <- NA

subset(resumen.global,dvs %in% "1.A.1.Ea")
resumen.global[resumen.global$frmt %in% c("1.B.1","1.B.2","1.B.3"),"D2b"]

##write.csv(file=sprintf("%s/MGs/%s_resumen_resultados.csv",gsub("doc","output",mi.path),hoy),resumen.global)



plot(p1950~p1750,subset(b.hist,pais %in% "global"))
summary(with(subset(b.hist,pais %in% "global"),p1950/p1750))

mean(grepl("A1",resumen.global$tcats) | grepl("A3",resumen.global$tcats))
mean(grepl("A1",resumen.global$tcats) | grepl("A2a",resumen.global$tcats)| grepl("A2b",resumen.global$tcats)| grepl("A3",resumen.global$tcats))
mean(grepl("C2a",resumen.global$tcats)| grepl("C2b",resumen.global$tcats))
mean(grepl("D1",resumen.global$tcats) | grepl("D2b",resumen.global$tcats)| grepl("D3",resumen.global$tcats))

mean(grepl("A2a",resumen.global$tcats) | grepl("A2b",resumen.global$tcats))

with(subset(b.pres,pais %in% "global"),summary((max.bosque-min.bosque)/mean.bosque))
with(subset(b.pres,pais %in% "global"),table(p.glm<0.05,sign(PRD)))
median(with(subset(b.pres,pais %in% "global" & p.glm<0.05 & sign(PRD) <0),PRD))

mean(grepl("A2a",resumen.global$tcats) | grepl("A2b",resumen.global$tcats))
sum(resumen.global$A2b %in% c("CO","CR","EN","VU"))/sum(!resumen.global$A2b %in% c("NE"))

sum(resumen.global$C2b %in% c("CO","CR","EN","VU"))/sum(!resumen.global$C2b %in% c("DD","NE"))
sum(resumen.global$C2a %in% c("CO","CR","EN","VU"))/sum(!resumen.global$C2b %in% c("NE"))
sum(resumen.global$C2b %in% c("CO","CR","EN","VU"))
sum(resumen.global$C2a %in% c("CO","CR","EN","VU"))

sum(resumen.global$D1 %in% c("CO","CR","EN","VU"))/sum(!resumen.global$D1 %in% c("NE"))

mad(subset(w.pres,pais %in% "global")$mean.severity,na.rm=T)
mean(subset(w.pres,pais %in% "global" & mcdg %in% subset(tipologia,format %in% c("1.A.4"))$macrogroup_key)$mean.severity,na.rm=T)
mean(subset(w.pres,pais %in% "global" & mcdg %in% subset(tipologia,format %in% c("1.A.5"))$macrogroup_key)$mean.severity,na.rm=T)
mean(subset(w.pres,pais %in% "global" & mcdg %in% subset(tipologia,format %in% c("1.B.3"))$macrogroup_key)$mean.severity,na.rm=T)

median(subset(c.futr,pais %in% "global")$mean.severity,na.rm=T)
mad(subset(c.futr,pais %in% "global")$mean.severity,na.rm=T)
summary(lm(mean.severity~md+sc,data=subset(c.futr,pais %in% "global")))
with(subset(c.futr,pais %in% "global"),
     aggregate(mean.severity,list(sc),median))

median(subset(d.pres,pais %in% "global")$mean.severity,na.rm=T)
with(subset(d.hist,pais %in% "global"),
     aggregate(mean.severity.D3,list(tipologia$format[match(mcdg,tipologia$macrogroup_key)]),median))

median(subset(d.pres,pais %in% "global")$mean.severity,na.rm=T)
with(subset(d.hist,pais %in% "global"),
     aggregate(mean.severity.D1,list(tipologia$format[match(mcdg,tipologia$macrogroup_key)]),median))

with(subset(d.hist,pais %in% "global"),
      aggregate(mean.severity.D3,list(tipologia$format[match(mcdg,tipologia$macrogroup_key)]),mad,na.rm=T))

with(subset(d.pres,pais %in% "global"),
      aggregate(mean.severity,list(tipologia$format[match(mcdg,tipologia$macrogroup_key)]),median,na.rm=T))

median(subset(d.pres,pais %in% "global" & tipologia$format[match(mcdg,tipologia$macrogroup_key)] %in% c("1.A.2","1.A.3","1.A.4","1.A.5"))$mean.severity,na.rm=T)


median(subset(d.hist,pais %in% "global" & tipologia$format[match(mcdg,tipologia$macrogroup_key)] %in% c("1.A.1","1.A.2","1.A.3","1.A.4","1.A.5","1.B.2"))$mean.severity.D3,na.rm=T)

median(subset(d.hist,pais %in% "global" & tipologia$format[match(mcdg,tipologia$macrogroup_key)] %in% c("1.A.1","1.A.2","1.A.3","1.A.4","1.A.5"))$mean.severity.D1,na.rm=T)
median(subset(d.hist,pais %in% "global" & tipologia$format[match(mcdg,tipologia$macrogroup_key)] %in% c("1.B.1","1.B.3"))$mean.severity.D3,na.rm=T)

median(subset(d.hist,pais %in% "global" & tipologia$format[match(mcdg,tipologia$macrogroup_key)] %in% c("1.A.1","1.A.2","1.A.3","1.A.4","1.A.5","1.B.2"))$mean.severity.D3,na.rm=T)
median(subset(d.hist,pais %in% "global" & tipologia$format[match(mcdg,tipologia$macrogroup_key)] %in% c("1.B.1","1.B.3"))$mean.severity.D3,na.rm=T)

with(resumen.global,table(oacats %in% c("CR","EN","VU"),grepl("2",tcats) , (grepl("3",tcats) |grepl("1",tcats))))




ss <- subset(b.hist,pais %in% "global")
ss$frmt <- tipologia$format[match(ss$mcdg,tipologia$macrogroup_key)]
with(ss, aggregate(max.bosque,list(frmt),sum,na.rm=T))

tt <- with(ss, aggregate(data.frame(max.bosque,min.bosque),list(frmt),sum,na.rm=T))
tt$decline <- 100-round(tt$min.bosque*100/tt$max.bosque,1)


ss$dvs <- tipologia$Division.Code[match(ss$mcdg,tipologia$macrogroup_key)]

tt <- with(ss, aggregate(data.frame(max.bosque,min.bosque),list(frmt,dvs),sum,na.rm=T))
tt$decline <- 100-round(tt$min.bosque*100/tt$max.bosque,1)


sum(actuales$C2b %in% c("VU","EN","CR")) / sum(!actuales$C2b %in% c("DD","NE"))

##tabla resultados
tabla.resultados <- with(actuales,tapply(mcdg,list(pais,factor(oacats,levels=names(IUCN.cats))),function(x) length(unique(x))))
tabla.resultados[is.na(tabla.resultados)] <- 0
Ts <- c()
for (k in 1:nrow(tabla.resultados))
    Ts <- c(Ts,weighted.mean(c(NA,5:0,NA),tabla.resultados[k,],na.rm=T))
cbind(tabla.resultados,Ts)





### for the whole country, threat score, total area

CAR <- as.character(subset(paises,UNREG1 %in% "Caribbean")$NAME)
SAM <- as.character(subset(paises,UNREG1 %in% "South America")$NAME)
NAM <- as.character(subset(paises,UNREG1 %in% "Northern America")$NAME)
CAM <- as.character(subset(paises,UNREG1 %in% "Central America")$NAME)

acciones <- data.frame()
for (j in unique(todos$pais)) {
    ss <- subset(todos,pais %in% j & costo==0)
    kk <- ss[!duplicated(ss$mcdg),]

    boots <- data.frame(pais=j,Ts= weighted.mean(c(NA,5:0,NA),table(factor(kk$oacats,levels=names(IUCN.cats))),na.rm=T),
                        t(colSums(kk[,grep("costo",colnames(kk))],na.rm=T)))

    ss <- subset(todos,pais %in% j)
    ss <- ss[!duplicated(ss[,c("mcdg","oacats")]),]

    for (i in 1:1000) {
        kk <- ss[  sample(1:nrow(ss)),]
        kk <- kk[!duplicated(kk$mcdg),]
        
        boots <- rbind(boots,data.frame(pais=j,Ts= weighted.mean(c(NA,5:0,NA),table(factor(kk$oacats,levels=names(IUCN.cats))),na.rm=T),
                                        t(colSums(kk[,grep("costo",colnames(kk))],na.rm=T))))

    }
    boots$costo.relativo <- boots$costo/subset(paises,NAME %in% j)$SQKM
    boots$Ts.relativo <- 1-(boots$Ts/boots$Ts[1])
    boots <- subset(boots,costo.relativo>0.0045 & costo.relativo < 0.5 & Ts.relativo>0)
    if (nrow(boots)>0) {
        plot(costo.relativo~Ts.relativo,boots)
        points(costo.relativo~Ts.relativo,boots[which.min(boots$costo.relativo/boots$Ts.relativo),],col=2,cex=2,pch=19)
        
        acciones <- rbind(acciones,boots[which.min(boots$costo.relativo/boots$Ts.relativo),])
    }

}
round(acciones$costo.relativo,3)
summary(acciones$costo.relativo,3)

## actions (maybe use http://cmp-openstandards.org/using-os/tools/actions-taxonomy/)
##A1,A3  =>reforest historical losses 2.3 Habitat & Natural Process Restoration
##A2a,A2b halt current deforestation 1.2 Resource & Habitat Protection
##B1,B2,B3 => protect restricted ecosystems 1.2 Resource & Habitat Protection
##C2a => adapt to climate change 
##C2b => protect water resources
##D1,D3 => diminish natural resource use
##D2 => diminish overexplotaition of animal resources
##mosaicplot(acciones[,c(grep("costo.[A-D]",colnames(acciones)))])



tt <- acciones[acciones$pais %in% SAM,
               c(grep("costo.[A-D]",colnames(acciones)))]
tt <- tt/acciones[acciones$pais %in% SAM,"costo"]
rownames(tt) <- acciones[acciones$pais %in% SAM,"pais"]


tt$restore.forest <- with(tt,costo.A1+costo.A3)
tt$halt.deforestation <- with(tt,costo.A2a+costo.A2b)
tt$protect.restricted <- with(tt,costo.B1+costo.B2+costo.B3)
tt$climate.adaptation <- tt$costo.C2a
tt$water.protection <- tt$costo.C2b
tt$resource.use <- with(tt,costo.D1+costo.D3)
tt$over.explotation <- tt$costo.D2b
tt <- tt[,!grepl("costo",colnames(tt))]

##stars(tt,draw.segments=T,col.segments=rainbow(ncol(tt)),locations= TMWB@data[match(rownames(tt),TMWB@data$NAME),c("LON","LAT")],len=5,add=T)
## probar si tiene más sentido así
plot(subset(TMWB,NAME %in% SAM))
stars(sqrt(tt),draw.segments=T,col.segments=rainbow(ncol(tt)),locations= TMWB@data[match(rownames(tt),TMWB@data$NAME),c("LON","LAT")],len=5,scale=F,add=T)
legend("bottomleft",fill=rainbow(ncol(tt)),gsub("costo.","",colnames(tt)),ncol=1,inset=-.05,xpd=T)


tt <- acciones[acciones$pais %in% CAR & acciones$pais %in% TMWB@data$NAME,
               c(grep("costo.[A-D]",colnames(acciones)))]
tt <- tt/acciones[acciones$pais %in% CAR & acciones$pais %in% TMWB@data$NAME,"costo"]
rownames(tt) <- acciones[acciones$pais %in% CAR & acciones$pais %in% TMWB@data$NAME,"pais"]
##tt <- tt[rowSums(tt)>0,colSums(tt)>0]
stars(tt,draw.segments=T,col.segments=rainbow(12),locations= TMWB@data[match(rownames(tt),TMWB@data$NAME),c("LON","LAT")],len=3)
legend("bottomleft",fill=rainbow(12),gsub("costo.","",colnames(tt)),ncol=3,inset=-.05,xpd=T)

tt <- acciones[acciones$pais %in% CAM & acciones$pais %in% TMWB@data$NAME,
               c(grep("costo.[A-D]",colnames(acciones)))]
tt <- tt/acciones[acciones$pais %in% CAM & acciones$pais %in% TMWB@data$NAME,"costo"]
rownames(tt) <- acciones[acciones$pais %in% CAM & acciones$pais %in% TMWB@data$NAME,"pais"]
##tt <- tt[rowSums(tt)>0,colSums(tt)>0]
stars(tt,draw.segments=T,col.segments=rainbow(12),locations= TMWB@data[match(rownames(tt),TMWB@data$NAME),c("LON","LAT")],len=1.3)
legend("bottomleft",fill=rainbow(12),gsub("costo.","",colnames(tt)),ncol=3,inset=-.05,xpd=T)



stars(tt,draw.segments=T,col.segments=rainbow(12))


ss <- subset(todos,pais=="Colombia")
with(ss,tapply(costo,list(droplevels(mcdg),factor(oacats,levels=names(IUCN.cats))),min,na.rm=T))

ss <- ss[order(ss$costo),]
kk <- ss[!duplicated(ss$mcdg),]




## alternativas...
table(actuales$oacats,actuales$opciones)
table(posibles$proteger>0)
table(posibles$restaurar>0)
table(posibles$emision>25)

acciones <- data.frame()
ss <- subset(todos,pais %in% "Venezuela" & catorig !="LC")
ss <- subset(todos,pais %in% "Colombia" & catorig !="LC")
for (j in unique(todos$pais)) {
    ss <- subset(todos,pais %in% j & catorig !="LC")
    ss$mejora <- ss$catorig != ss$oacats
    rsm <- summary(glm(mejora~I(proteger>0)+I(reforestar>0)+I(fauna>0)+I(hidrico>0)+I(restaurar>0)+I(emision>25),data=ss,family=binomial(logit)))
    
    acciones <- rbind(acciones,
                      data.frame(pais=j,
                                 var=rownames(rsm$coefficients)[-1],
                                 signo=sign(rsm$coefficients[-1,1]),
                                 p=rsm$coefficients[-1,4]<0.05))
}

tt <- 
    with(acciones,tapply(p,list(pais,var),sum))
colnames(tt) <- c("emision","proteger","reforestar","restaurar","hidrico","fauna")

all(rownames(tt) %in% c(CAR,SAM,NAM,CAM))

## controlling climate change emisions do not have a significant effect on overall category

## climate change, fauna and hidric not significant,
## several countries with no improvement
## combination of protect and reforest,
## restoration of remaining forest is important for Antigua and Barbuda, DR and Turk and Caicos...
tt[rownames(tt) %in% CAR,]
## climate change not significant
## protect and restoreation needed overall, reforesting in El salvador and mexico, hidric in CR, dfaunation Honduras and Panama
tt[rownames(tt) %in% CAM,]
## Mexico and USA need combinations of protection, reforestation and restauration, Canada no significant change
tt[rownames(tt) %in% NAM,]
## Andean countries can improve with reforestation programs 
## in Southern countries (Bolivia, Brazil, Paraguay, Argentina, Chile) restoration + defaunation + water regime are more important than just reforesting
## no significant actions in the Guiana shield or Uruguay
tt[rownames(tt) %in% SAM,]







    rsm <- summary(lm(Ts~proteger+reforestar+fauna+hidrico+restaurar+adaptar+emision,data=boots))
    
    acciones <- rbind(acciones,
                      data.frame(pais=j,
                                 var=rownames(rsm$coefficients)[-1],
                                 cff=rsm$coefficients[-1,1],
                                 p=rsm$coefficients[-1,4]))
}

tt <- t1 <- 
    with(acciones,tapply(cff,list(pais,var),function(x) round(x,3)))
t2 <- 
    with(acciones,tapply(p,list(pais,var),function(x) round(x,3)))

tt[t2<0.05] <- 0

## clearly reforestation has the larger effect but most country require some additional action, 
tt[rownames(tt) %in% CAR,]
table(apply(tt[rownames(tt) %in% c(CAR),],1,which.min))
apply(tt[rownames(tt) %in% c(CAR),],2,sum,na.rm=T)

## similar contributions of protection and restoration, also water  management but no effect of fauna protection or climate change
tt[rownames(tt) %in% CAM,]
tt[rownames(tt) %in% NAM,]
table(apply(tt[rownames(tt) %in% c(CAM,NAM),],1,which.min))
apply(tt[rownames(tt) %in% c(CAM,NAM),],2,sum,na.rm=T)

## mostly a combination of protection, restoration and refoerestation, slightly higher effect of effective protection, most coutnries benefit from water management, climate emission important for Peru and Chile, The Guayana Shield countries are relatively safe
tt[rownames(tt) %in% SAM,]
table(apply(tt[rownames(tt) %in% c(SAM),],1,which.min))
apply(tt[rownames(tt) %in% c(SAM),],2,sum,na.rm=T)


apply(tt[rownames(tt) %in% c(SAM),][c(1:5),],2,sum,na.rm=T)
apply(tt[rownames(tt) %in% c(SAM),][c(6:8),],2,sum,na.rm=T)
apply(tt[rownames(tt) %in% c(SAM),][c(9:13),],2,sum,na.rm=T)


subset(paises,NAME=="Venezuela")

ss <- subset(actuales,pais %in% "Venezuela")
ss <- subset(actuales,pais == 188)
ss <- subset(actuales,pais == 256)
mtz <- with(ss,tapply(oacats,list(droplevels(mcdg),droplevels(escenario)),unique))
cost.p <- with(ss,tapply(proteger,list(droplevels(mcdg),droplevels(escenario)),sum))
cost.r <- with(ss,tapply(reforestar,list(droplevels(mcdg),droplevels(escenario)),sum))
cost.f <- with(ss,tapply(fauna,list(droplevels(mcdg),droplevels(escenario)),sum))
cost.w <- with(ss,tapply(hidrico,list(droplevels(mcdg),droplevels(escenario)),sum))
cost.p[is.na(cost.p)] <- 0
cost.r[is.na(cost.r)] <- 0
cost.f[is.na(cost.f)] <- 0
cost.w[is.na(cost.w)] <- 0

Ts <- weighted.mean(c(0,6:1,0),table(factor(mtz[,1],levels=names(IUCN.cats))))
for (k in 2:ncol(mtz)) {
    mtz[is.na(mtz[,k]),k] <- mtz[is.na(mtz[,k]),1]
    Ts <- c(Ts,weighted.mean(c(0,6:1,0),table(factor(mtz[,k],levels=names(IUCN.cats)))))
}

Cp <- colSums(cost.p)
Cr <- colSums(cost.r)
Cf <- colSums(cost.f)
Cw <- colSums(cost.w)
slc <- c(T,Ts[-1]<Ts[1])

plot(Ts~Cp,subset=slc)
plot(Ts~Cr,subset=slc)
plot(Ts~Cf,subset=slc)
summary(glm(Ts~Cr+Cp+Cf+Cw,subset=slc,family=quasipoisson(log)))



ss <- subset(actuales,pais == 54)

subset(ss,oacat %in% "CR")
## escenarios alternativos:
## detener deforestación en los más críticos, detener deforestación en los menos costosos -> criterios A2a y A2b
## reforestar más críticos, reforestar menos costosos -> criterios A1, A3, B2 y B3
## cambio climático ...
## disminuir defaunacion...
## manejo de agua
## manejo de fuego

##tabla: MGs por categorias y treat score por pais, luego con varios escenarios

## mapas: areas donde proteger el bosque puede mejorar el treat score,
## areas donde es prioritario restaurar/reforestar
## areas donde es necesario complementar proteccion/reforestacion con otras acciones


subset(b.pres,pais==54)
subset(b.hist,pais==54)

sh <- subset(b.hist,pais==54 & mcdg %in% "M563")
sp <- subset(b.pres,pais==54 & mcdg %in% "M563" )

        

critA =  ## Criterion A
    newXMLNode("criterion",attrs=list(name="A",version="RLE 2.0"),
               children=c(
                   newXMLNode("key-indicator-variable","forest cover"),
                   newXMLNode("key-indicator-variable","woodland distribution"),
                   newXMLNode("indicator-data","remote sensor"),
                   newXMLNode("indicator-data","cartographic reconstruction"),
                   newXMLNode("data-source","Modis"),
                   newXMLNode("data-source","GFC2015"),
                   newXMLNode("data-source","Anthromes"),
                   newXMLNode("method-of-measurement","Spatio-temporal analysis")))

        source(sprintf("%s/%s/inc30_subcritA1.R",mi.path,mi.dir))
        source(sprintf("%s/%s/inc31_subcritA2a.R",mi.path,mi.dir))
        source(sprintf("%s/%s/inc32_subcritA2b.R",mi.path,mi.dir))
        source(sprintf("%s/%s/inc33_subcritA3.R",mi.path,mi.dir))


critA =  ## Criterion A
    newXMLNode("criterion",attrs=list(name="B",version="RLE 2.0"),
               children=c(
                   newXMLNode("key-indicator-variable","forest cover"),
                   newXMLNode("indicator-data","remote sensor"),
                   newXMLNode("data-source","Modis"),
                   newXMLNode("data-source","GFC2015"),
                   newXMLNode("method-of-measurement","Spatial analysis")))



##########
##












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
