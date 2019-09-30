plot(p1950~p1750,subset(b.hist,pais %in% "global"))
summary(with(subset(b.hist,pais %in% "global"),p1950/p1750))


cat(sprintf("Criterion A determines the overall threat category for %0.1f%% of the Macrogroups.\n",mean(grepl("A1",resumen.global$tcats) | grepl("A2a",resumen.global$tcats)| grepl("A3",resumen.global$tcats))*100))

cat(sprintf("Criterion C determines the overall threat category for %0.1f%% of the Macrogroups.\n",mean(grepl("C1",resumen.global$tcats) | grepl("C2",resumen.global$tcats)| grepl("C3",resumen.global$tcats))*100))

cat(sprintf("Criterion D determines the overall threat category for %0.1f%% of the Macrogroups.\n",mean(grepl("D1",resumen.global$tcats) | grepl("D2",resumen.global$tcats)| grepl("D3",resumen.global$tcats))*100))

cat(sprintf("Subcriteria A1 and A3 determine the overall threat category for %0.1f%% of the Macrogroups.\n",mean(grepl("A1",resumen.global$tcats) | grepl("A3",resumen.global$tcats))*100))
cat(sprintf("Subcriterion A2 determine the overall threat category for %0.1f%% of the Macrogroups.\n",mean(grepl("A2a",resumen.global$tcats) | grepl("A2b",resumen.global$tcats))*100))


with(subset(b.pres,pais %in% "global"),summary((max.bosque-min.bosque)/mean.bosque))
with(subset(b.pres,pais %in% "global"),table(p.glm<0.05,sign(PRD)))
median(with(subset(b.pres,pais %in% "global" & p.glm<0.05 & sign(PRD) <0),PRD))

cat(sprintf("%0.1f%% of the (evaluated) Macrogroups are threatened under subcriterion A2.\n",
            sum(resumen.global$A2a %in% c("CO","CR","EN","VU"))/sum(!resumen.global$A2a %in% c("NE"))*100))

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
