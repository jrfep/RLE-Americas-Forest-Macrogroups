## rate of change per year:
as.numeric(gsub("M","",cdgs[cdgs %in% subset(tipologia, macrogroup_key != "")$macrogroup_key]))



total.cntr <- data.frame()

for (k in 17:20) {
    t1 <- read.table(sprintf("calculos_201712/SAM_Paises_AOO_Anthromes_%s00.tab",k),stringsAsFactors=F)
    t2 <- read.table(sprintf("calculos_201712/NAC_Paises_AOO_Anthromes_%s00.tab",k),stringsAsFactors=F) 
    assign(sprintf("ant.%s00",k),rbind(t1,t2))

    tt <-  with(subset(get(sprintf("ant.%s00",k)),!(V2 %in% "*") & V1 %in% as.numeric(gsub("M","",cdgs[cdgs %in% subset(tipologia, macrogroup_key != "")$macrogroup_key])) & V4 %in% legend.Anthromes$value[grep("woodland",legend.Anthromes$nombre)]),
                tapply(V5,list(factor(V1,levels=unique(ant.1700$V1))),function(x) sum(x/1e6,na.rm=T)))

    
    total.cntr <- rbind(total.cntr,tt)
    colnames(total.cntr) <- names(tt)
}
  
rowSums(total.cntr,na.rm=T)[1]-rowSums(total.cntr,na.rm=T)[4]
rownames(total.cntr) <- sprintf("a%s00",17:20)
ant.rsm <- aggregate(t(total.cntr),list(division=tipologia$Division.Code[match(sprintf("M%03d",as.numeric(colnames(total.cntr))),tipologia$macrogroup_key)]),sum,na.rm=T)
ant.rsm$formation <- substr(ant.rsm$division,1,5)
ant.rsm <- subset(ant.rsm,formation %in% c("1.A.1","1.A.2","1.A.3","1.A.4","1.A.5",
                                "1.B.1","1.B.2","1.B.3"))

ant.rsm$x1 <- c(0,cumsum(ant.rsm[,2])[-nrow(ant.rsm)])
ant.rsm$x2 <- cumsum(ant.rsm[,2])
ant.rsm$y2 <- (ant.rsm[,"a2000"]/ ant.rsm[,"a1700"])
ant.rsm$y3 <- (ant.rsm[,"a1900"]/ ant.rsm[,"a1700"])
ant.rsm$os <-  5e5*(as.numeric(as.factor(ant.rsm$formation)))

layout(1)

plot(0,0,xlim=c(0,max(ant.rsm$x2+ant.rsm$os)),ylim=c(0,1),pch=NA,xaxs="i",axes=F,xlab="",ylab="Area remaining (%)")
axis(2,seq(0,1,by=.25),sprintf("%s",round(seq(0,1,by=.25)*100)))
for (k in unique(ant.rsm$formation)) {
    with(subset(ant.rsm,formation %in% k),
         rect(min(x1+os),1,max(x2+os),
              sum(a2000,na.rm=T)/sum(a1700,na.rm=T),
              col=c("grey37"),border="grey37"))
    with(subset(ant.rsm,formation %in% k),
         rect(min(x1+os),1,max(x2+os),
              sum(a1900,na.rm=T)/sum(a1700,na.rm=T),
              col=c("grey77"),border="grey77"))
    
    with(subset(ant.rsm,formation %in% k),
         rect(min(x1+os),1,max(x2+os),0,
              col=c(NA),border=1))
    with(subset(ant.rsm,formation %in% k),text(min(x1+os)-2e5,0.5,
                                               as.character(head(subset(tipologia,Division.Code %in% k))$formation),srt=90,cex=.75))


   if ((k %in% "1.A.2")) 
    with(subset(ant.rsm,formation %in% k),
         text((max(x1+os)+min(x1+os))/2,0.10,expression(paste("Total area (in million ",km^2,"): ")),cex=.65))

    if (!(k %in% "1.A.5")) 
        with(subset(ant.rsm,formation %in% k),
             text((max(x1+os)+min(x1+os))/2,0.05,
                  sprintf("%0.1f",sum(a1700)/1e6),cex=.65,srt=ifelse(k %in% "1.A.5",90,0)))
}
text(1243824,0.95,"lost < 1900",col="grey37",cex=.5)
text(1243824,0.89,"lost > 1900",col="grey77",cex=.5)


dev.copy(pdf,file=sprintf("~/Provita/etc/MS/%s_Figure1.pdf",hoy),width=8,height=4)
dev.off()


layout(matrix(1:2,ncol=1))
par(mar=c(2,4,1,0),oma=c(0,2,0,1),xpd=NA)
plot(0,0,xlim=c(0,max(ant.rsm$x2+ant.rsm$os)),ylim=c(0,1),pch=NA,xaxs="i",axes=F,xlab="",ylab="By formation")
axis(2,seq(0,1,by=.25),sprintf("%s",round(seq(0,1,by=.25)*100)))
for (k in unique(ant.rsm$formation)) {
    with(subset(ant.rsm,formation %in% k),
         rect(min(x1+os),1,max(x2+os),
              sum(a2000,na.rm=T)/sum(a1700,na.rm=T),
              col=c("grey37"),border="grey37"))
    with(subset(ant.rsm,formation %in% k),
         rect(min(x1+os),1,max(x2+os),
              sum(a1900,na.rm=T)/sum(a1700,na.rm=T),
              col=c("grey77"),border="grey77"))
    
    with(subset(ant.rsm,formation %in% k),
         rect(min(x1+os),1,max(x2+os),0,
              col=c(NA),border=1))
    with(subset(ant.rsm,formation %in% k),text(min(x1+os)-2e5,-0.25,
                                               as.character(head(subset(tipologia,Division.Code %in% k))$formation),srt=90,cex=.75))


   if ((k %in% "1.A.2")) 
    with(subset(ant.rsm,formation %in% k),
         text((max(x1+os)+min(x1+os))/2,0.10,expression(paste("Total area (in million ",km^2,"): ")),cex=.65))

    if (!(k %in% "1.A.5")) 
        with(subset(ant.rsm,formation %in% k),
             text((max(x1+os)+min(x1+os))/2,0.05,
                  sprintf("%0.1f",sum(a1700)/1e6),cex=.65,srt=ifelse(k %in% "1.A.5",90,0)))
}
text(1243824,0.95,"lost < 1900",col="grey37",cex=.5)
text(1243824,0.89,"lost > 1900",col="grey77",cex=.5)

mtext("Area remaining (%)",2,line=0.8,outer=T)
plot(0,0,xlim=c(0,max(ant.rsm$x2+ant.rsm$os)),ylim=c(0,1),pch=NA,xaxs="i",axes=F,xlab="",ylab="By division")
axis(2,seq(0,1,by=.25),sprintf("%s",round(seq(0,1,by=.25)*100)))

with(ant.rsm,rect(x1+os,1,x2+os,y3,col=c("grey77"),border="grey77"))
with(ant.rsm,rect(x1+os,y3,x2+os,y2,col=c("grey37"),border="grey37"))
with(ant.rsm,rect(x1+os,0,x2+os,1,col=NA,border=1))

dev.copy(pdf,file=sprintf("~/Provita/etc/MS/%s_FigureA1.pdf",hoy),width=8,height=6)
dev.off()

ant.rsm[,5]/ant.rsm[,2]
ant.rsm[,4]/ant.rsm[,2]

round(xx$p1950/50,3)/round(xx$p1750/250,3)
 1-(sum(xx$min.bosque.x))/sum(xx$max.bosque.x)
sum(xx$min.bosque.x)/sum(xx$max.bosque.x)
